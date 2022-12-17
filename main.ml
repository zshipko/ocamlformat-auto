open Bos

let ( // ) = Filename.concat

module Ocamlformat_version = struct
  let all () =
    let inp =
      Unix.open_process_in
        "opam info ocamlformat --field all-versions --color=never"
    in
    let r = input_line inp in
    close_in inp;
    List.filter_map (fun s ->
        if String.equal String.empty s then None else Some (String.trim s))
    @@ String.split_on_char ' ' r

  let latest () = List.rev (all ()) |> List.hd

  let detect () =
    let version = Sys.getenv_opt "OCAMLFORMAT_VERSION" in
    match version with
    | Some version -> Some version
    | None ->
        if not (Sys.file_exists ".ocamlformat") then None
        else
          let f = open_in ".ocamlformat" in
          let n = in_channel_length f in
          let data = really_input_string f n in
          let lines = String.split_on_char '\n' data in
          let () = close_in f in
          List.fold_left
            (fun acc line ->
              if Option.is_none acc && String.starts_with ~prefix:"version" line
              then
                let s = String.split_on_char '=' line in
                let v = List.nth s 1 in
                Some (String.trim v)
              else acc)
            None lines
end

module Temp_dir = struct
  let cleanup tmp =
    if Sys.file_exists tmp && Sys.is_directory tmp then
      let () = Printf.printf "Removing %s\n%!" tmp in
      OS.Dir.delete ~recurse:true (Fpath.v tmp) |> Rresult.R.failwith_error_msg
end

let handle_status ?tmp x =
  match Rresult.R.failwith_error_msg x with
  | `Exited 0 -> ()
  | `Exited n ->
      Printf.eprintf "ERROR Subprocess exited with code %d\n" n;
      Option.iter Temp_dir.cleanup tmp;
      exit n
  | `Signaled n ->
      Printf.eprintf "ERROR Subprocess received signal %d\n" n;
      Option.iter Temp_dir.cleanup tmp;
      exit 1

module Shim = struct
  let copy_self dest =
    Printf.printf "Copying %s to %s\n%!" Sys.executable_name dest;
    OS.Cmd.run_status Cmd.(v "cp" % Sys.executable_name % dest) |> handle_status;
    let make_executable = Cmd.(v "chmod" % "u=rwx,g=rx,o=" % dest) in
    OS.Cmd.run_status make_executable |> handle_status

  let init path =
    let exe = path // "ocamlformat" in
    let () = if Sys.file_exists exe then Unix.unlink exe in
    let installed = path // "ocamlformat-auto" in
    let () = copy_self installed in
    let oc = open_out exe in
    Printf.fprintf oc "#!/usr/bin/env sh\n%s exec -- \"$@\"\n%!" installed;
    let () = close_out oc in
    let make_executable = Cmd.(v "chmod" % "+x" % exe) in
    OS.Cmd.run_status make_executable |> handle_status;
    Printf.printf "Created ocamlformat shim in %s\n%!" path
end

let start_spinner () =
  let finish = ref false in
  let thread =
    Thread.create
      (fun () ->
        let a = Progress.Line.spinner () in
        let spinner = Progress.(Multi.line @@ a) in
        let display = Progress.Display.start spinner in
        while not !finish do
          let () = Unix.sleepf 0.25 in
          Progress.Display.tick display
        done;
        Progress.Display.finalise display)
      ()
  in
  (finish, thread)

let stop_spinner (finish, thread) =
  finish := true;
  Thread.join thread

let install_cmd path version ocaml_version force init =
  let version_s =
    Option.value ~default:(Ocamlformat_version.latest ()) version
  in
  let timestamp = Unix.time () |> int_of_float in
  let path_file = (path // "ocamlformat-") ^ version_s in
  let init_file = path // "ocamlformat" in
  let ocamlformat_with_version =
    match version with
    | Some v -> "ocamlformat." ^ v
    | None ->
        Printf.eprintf
          "ERROR Unable to detect ocamlformat version to install\n%!";
        exit 1
  in
  let () =
    if (not force) && Sys.file_exists path_file then
      Printf.printf "%s is already installed, run with --force to reinstall\n%!"
        ocamlformat_with_version
    else
      let tempdir =
        Printf.sprintf "ocamlformat-auto-%s-%s.%d" version_s ocaml_version
          timestamp
      in
      let () = Printf.printf "Installing %s\n%!" ocamlformat_with_version in
      let tmp = Filename.get_temp_dir_name () // tempdir in
      let () = Unix.mkdir tmp 0o766 in
      let () = at_exit (fun () -> Temp_dir.cleanup tmp) in
      let () = Printf.printf "Creating new switch in %s\n%!" tmp in
      let spinner = start_spinner () in
      let () =
        OS.Cmd.run_status ~quiet:true ~err:OS.Cmd.err_stderr
          Cmd.(v "opam" % "switch" % "create" % tmp % ocaml_version)
        |> handle_status ~tmp
      in
      let () = stop_spinner spinner in

      let install =
        Cmd.(
          v "opam" % "install" % "--switch" % tmp % "-y"
          % ocamlformat_with_version)
      in
      let () = Printf.printf "Building %s\n%!" ocamlformat_with_version in
      let spinner = start_spinner () in
      let () =
        OS.Cmd.run_status ~quiet:true ~err:OS.Cmd.err_stderr install
        |> handle_status ~tmp
      in
      let () = stop_spinner spinner in
      let () = Printf.printf "Copying to %s\n%!" path_file in
      let () = Unix.rename (tmp // "_opam/bin/ocamlformat") path_file in
      let () = Temp_dir.cleanup tmp in
      Printf.printf "Installed %s to %s\n%!" ocamlformat_with_version path
  in
  if init then Shim.init init_file

let init_cmd path = Shim.init path

let uninstall_cmd path version =
  let files = Sys.readdir path in
  let suffix = Option.value ~default:"" version in
  Array.iter
    (fun filename ->
      if
        String.starts_with ~prefix:"ocamlformat" filename
        && String.ends_with ~suffix filename
      then
        let () = Sys.remove (path // filename) in
        Printf.printf "Removed %s\n%!" (path // filename))
    files

let list_cmd path available =
  if available then
    let v = Ocamlformat_version.all () in
    List.iter print_endline v
  else
    let files = Sys.readdir path in
    Array.iter
      (fun filename ->
        if String.starts_with ~prefix:"ocamlformat" filename then
          match filename with
          | "ocamlformat" | "ocamlformat-auto" -> ()
          | _ -> print_endline filename)
      files

let exec_cmd path version args =
  let v, prog =
    match version with
    | Some v -> (v, "ocamlformat-" ^ v)
    | None ->
        Printf.eprintf "ERROR Unable to detect ocamlformat version\n";
        exit 1
  in
  let exe = path // prog in
  let () =
    if not (Sys.file_exists exe) then
      let () =
        Printf.eprintf
          "ERROR ocamlformat version not found, try running:\n\
           \tocamlformat-auto install %s\n"
          v
      in
      exit 1
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
    & pos 0 (some string) (Ocamlformat_version.detect ())
      @@ Arg.info ~doc:"ocamlformat version" ~docv:"VERSION" [])

let version_flag =
  Arg.(
    value
    & opt (some string) (Ocamlformat_version.detect ())
      @@ Arg.info ~doc:"ocamlformat version" [ "version" ])

let force =
  Arg.(
    value
    & flag
      @@ info ~doc:"Install even if the selected version is alreadyn installed"
           [ "force" ])

let init_flag =
  Arg.(value & (flag @@ info ~doc:"Install and initialize shim" [ "init" ]))

let available =
  Arg.(
    value
    & flag
      @@ info ~doc:"List all available ocamlformat versions" [ "available" ])

let list =
  let info = Cmd.info ~doc:"List installed ocamlformat executables" "list" in
  Cmd.v info Term.(const list_cmd $ path $ available)

let uninstall =
  let info =
    Cmd.info
      ~doc:
        "Remove installed ocamlformat executables, if no version is provided \
         then ocamlformat-auto and all ocamlformat versions will be removed"
      "uninstall"
  in
  Cmd.v info Term.(const uninstall_cmd $ path $ version)

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
      const install_cmd $ path $ version $ ocaml_version $ force $ init_flag)

let init =
  let info = Cmd.info ~doc:"Initialize shim" "init" in
  Cmd.v info Term.(const init_cmd $ path)

let () =
  let info = Cmd.info "ocamlformat-auto" in
  exit @@ Cmd.eval (Cmd.group info [ init; install; exec; list; uninstall ])
