# ocamlformat-auto

`ocamlformat-auto` is an application inspired by [ocamlformat-vmux](https://github.com/CraigFe/ocamlformat-vmux)
that can be used to manage multiple versions of [ocamlformat](https://github.com/ocaml-ppx/ocamlformat)

It will create an executable script named `ocamlformat` that will select the proper version for each
project.

## Installation

Using [opam](https://github.com/ocaml/opam):

```shell
$ opam pin add git+https://github.com/zshipko/ocamlformat-auto.git
```

Using [dune](https://dune.build):

```shell
$ dune exec ./main.exe init
```

## Typical usage

In the root of an existing project (with an `.ocamlformat` file) you can run the following
to ensure the proper `ocamlformat` version is installed and `ocamlformat` will work as
expected:

```shell
$ ocamlformat-auto install --init
```

## More examples

Initialize the shim:

```shell
$ ocamlformat-auto init
```

Install the detected version (or latest if no .ocamlformat file is present):

```shell
$ ocamlformat-auto install
```

Install a specific version:

```shell
$ ocamlformat-auto install 0.20.0
```

List installed versions:

```shell
$ ocamlformat-auto list
```

Execute ocamlformat on a file (this will try to detect the correct version
by checking the `.ocamlformat` file for your project):

```shell
$ ocamlformat-auto exec -- ./bin/main.ml -i
```

Once the installed (using the `init` command), you can run `ocamlformat`
like normal and the correct version will automatically be selected:

```shell
$ ocamlformat ./bin/main.ml -i
```

For more information run `ocamlformat-auto --help`



