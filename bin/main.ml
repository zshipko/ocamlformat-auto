let ( // ) = Filename.concat

let all_versions () =
  let inp =
    Unix.open_process_in
      "opam info ocamlformat --field all-versions --color=never"
  in
  let r = In_channel.input_line inp |> Option.value ~default:"" in
  In_channel.close inp;
  List.map String.trim @@ String.split_on_char ' ' r

let latest () = List.rev (all_versions ()) |> List.hd

let cleanup tmp =
  if Sys.file_exists tmp && Sys.is_directory tmp then
    try ignore @@ Unix.system (Printf.sprintf "rm -r '%s'" tmp) with _ -> ()

let handle_status tmp = function
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n ->
      Printf.eprintf "Subprocess exited with code %d\n" n;
      cleanup tmp;
      exit n
  | Unix.WSIGNALED n ->
      Printf.eprintf "Subprocess received signal %d\n" n;
      cleanup tmp;
      exit 1
  | Unix.WSTOPPED n ->
      Printf.eprintf "Subprocess stopped with status %d\n" n;
      cleanup tmp;
      exit 1

let make_link src path =
  let () = try Unix.unlink path with _ -> () in
  Unix.symlink src path

let detect_version () =
  if not (Sys.file_exists ".ocamlformat") then None
  else
    In_channel.with_open_text ".ocamlformat" (fun f ->
        let f = In_channel.input_all f in
        let lines = String.split_on_char '\n' f in
        List.fold_left
          (fun acc line ->
            if Option.is_none acc && String.starts_with ~prefix:"version" line
            then
              let s = String.split_on_char '=' line in
              let v = List.nth s 1 in
              Some v
            else acc)
          None lines)

let install_cmd path version ocaml_version force link =
  let version_s = Option.value ~default:(latest ()) version in
  let timestamp = Unix.time () |> int_of_float in
  let path_file = (path // "ocamlformat-") ^ version_s in
  let link_file = path // "ocamlformat" in
  let () =
    if (not force) && Sys.file_exists path_file then ()
    else
      let tempdir =
        Printf.sprintf "ocamlformat-manager-%s-%s.%d" version_s ocaml_version
          timestamp
      in
      let tmp = Filename.get_temp_dir_name () // tempdir in
      let () = Unix.mkdir tmp 0o766 in
      let () = at_exit (fun () -> cleanup tmp) in
      let () = Unix.chdir tmp in
      let () =
        Unix.system ("opam switch create . " ^ ocaml_version)
        |> handle_status tmp
      in
      let ocamlformat_with_version =
        match version with
        | Some v -> "ocamlformat." ^ v
        | None -> "ocamlformat"
      in
      let install =
        Printf.sprintf "opam install -y '%s'" ocamlformat_with_version
      in
      let _r = Unix.system install |> handle_status tmp in
      let () = Unix.rename "_opam/bin/ocamlformat" path_file in
      let () = cleanup tmp in
      ()
  in
  if link then make_link path_file link_file

let link_cmd path version =
  let version =
    match version with
    | None ->
        Printf.eprintf
          "ERROR: unable to determine correct ocamlformat version\n";
        exit 1
    | Some v -> v
  in
  let bin = Printf.sprintf "ocamlformat-%s" version in
  make_link (path // bin) (path // "ocamlformat")

let clean_cmd path version =
  let files = Sys.readdir path in
  let suffix = Option.value ~default:"" version in
  Array.iter
    (fun filename ->
      if
        String.starts_with ~prefix:"ocamlformat" filename
        && String.ends_with ~suffix filename
      then Sys.remove (path // filename))
    files

let list_cmd path available =
  if available then
    let v = all_versions () in
    List.iter print_endline v
  else
    let files = Sys.readdir path in
    Array.iter
      (fun filename ->
        if String.starts_with ~prefix:"ocamlformat" filename then
          match filename with
          | "ocamlformat" ->
              let f = Unix.readlink (path // filename) in
              Printf.printf "ocamlformat (%s)\n%!" (Filename.basename f)
          | _ -> print_endline filename)
      files

let exec_cmd path version args =
  let prog =
    match version with Some v -> "ocamlformat-" ^ v | None -> "ocamlformat"
  in
  let args = Array.of_list args in
  Unix.execvp (path // prog) (Array.append [| "ocamlformat" |] args)

open Cmdliner

let path =
  let default = Sys.getenv "HOME" // ".local" // "bin" in
  Arg.(
    value
    & opt dir default
      @@ Arg.info [ "path" ] ~doc:"Path to saved ocamlformat executables")

let ocaml_version =
  let major = Sys.ocaml_release.major in
  let minor = Sys.ocaml_release.minor in
  let patch = Sys.ocaml_release.patchlevel in
  let default = Printf.sprintf "%d.%d.%d" major minor patch in
  Arg.(
    value
    & opt string default
      @@ Arg.info [ "ocaml-version" ]
           ~doc:"Select ocaml version for building ocamlformat")

let version =
  Arg.(
    value
    & pos 0 (some string) (detect_version ())
      @@ Arg.info ~doc:"ocamlformat version" ~docv:"VERSION" [])

let version_flag =
  Arg.(
    value
    & opt (some string) (detect_version ())
      @@ Arg.info ~doc:"ocamlformat version" [ "version" ])

let force =
  Arg.(
    value
    & flag
      @@ info ~doc:"Install even if the selected version is alreadyn installed"
           [ "force" ])

let link_flag =
  Arg.(value & (flag @@ info ~doc:"Install and make default" [ "link" ]))

let available =
  Arg.(
    value
    & flag
      @@ info ~doc:"List all available ocamlformat versions" [ "available" ])

let list =
  let info = Cmd.info ~doc:"List installed ocamlformat executables" "list" in
  Cmd.v info Term.(const list_cmd $ path $ available)

let clean =
  let info = Cmd.info ~doc:"Remove installed ocamlformat executables" "clean" in
  Cmd.v info Term.(const clean_cmd $ path $ version)

let exec =
  let info =
    Cmd.info ~doc:"Detect correct version and execute ocamlformat" "exec"
  in
  let rest = Arg.(value & (pos_all string [] @@ info [])) in
  Cmd.v info Term.(const exec_cmd $ path $ version_flag $ rest)

let install =
  let info = Cmd.info ~doc:"Install ocamlformat" "install" in
  Cmd.v info
    Term.(
      const install_cmd $ path $ version $ ocaml_version $ force $ link_flag)

let link =
  let info = Cmd.info ~doc:"Set default ocamlformat version" "link" in
  Cmd.v info Term.(const link_cmd $ path $ version)

let () =
  let info = Cmd.info "ocamlformat-manager" in
  exit @@ Cmd.eval (Cmd.group info [ clean; list; install; link; exec ])
