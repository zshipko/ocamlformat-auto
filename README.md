# ocamlformat-manager

`ocamlformat-manager` is an application that can be used to manage multiple
versions of [ocamlformat](https://github.com/ocaml-ppx/ocamlformat)

## Examples

Install the latest ocamlformat:

```shell
$ ocamlformat-manager install
```

Install a specific version:

```shell
$ ocamlformat-manager install 0.20.0
```

List installed versions:

```shell
$ ocamlformat-manager list
```

Set a specific version as default:

```shell
$ ocamlformat-manager link 0.20.0
```

Execute ocamlformat on a file (this will try to detect the correct version
by checking the `.ocamlformat` file for your project):

```shell
$ ocamlformat-manager exec -- ./bin/main.ml -i
```

