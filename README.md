# Build

You need the following OCaml packages:

```
spotlib
compiler-libs.common
ppx_import
ppx_meta_conv
ppx_meta_conv_ocaml
typerep
ppx_typerep_conv
ppx_orakuda
ppx_monadic
levenshtein
opamfind
unmagic
ppx_tools
```

In non Windows, you also need the following to compile the server.

```
cohttp.lwt
tyxml
tyxml.ppx
```

In Windows, the server cannot be compiled since `lwt` is not compiled with Unix support.

The source code of OCamlOScope.2 should be compilable with the latest versions of these packages, but it is not always guaranteed.

# Scraping

```
$ mkdir out
$ ./oco scrape
```

The command scrapes the installed OCamlFind packages and save the result under `out/` directory.  All the packages must be compiled with `-bin-annot` compiler option and the build files must be kept.  Here is an example of such a setting:

```
$ export OCAMLPARAM=_,_,bin-annot=1
$ export OPAMKEEPBUILDDIR=1
```

Compilers and packages should be installed using OPAM for easier scraping.  If the compiler or some packages are installed by hand, not by OPAM, you have to specify the source directory of these softwares explicitly with `--src-dir` option. For example:

```
$ ./oco scrape --src-dir $HOME/build/ocaml-4.03.0 --src-dir $HOME/mysrc/mypackage
```

Scraping may fail if source files or `.cm*` files of scraping modules are not found.

## Files

`oco scrape` creates the following files:

* `<package>.sig` : Extracted type information from `*.cmi` files.
* `<package>.hump`: Program abstraction from `*.cmt` and `*.cmti` files. 
* `<package>.dat`: The final data file of the package, which links `<package>.sig`, `<package>.hump` and `*.hump` files which the package relys on.

You can send `<package>.dat` files to OCamlOScope server maintainer to add the package to the server db.

# Linking

After scraping, you have to manually link these `*.dat` files into one:

```
$ ./oco link
```

The scraped data are linked together into one file: `out/all.all`.

# Interactive search

After linking, you can start a search session:

```
$ ./oco search
...
? <input your query>
```
