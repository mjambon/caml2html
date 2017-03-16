Caml2html
=========

Caml2html is a command-line tool that highlights the syntax of OCaml
source code.

Requirements
------------

Caml2html needs an OCaml compiler (>= 3.00) properly installed.
GNU make is required for the compilation.

Compiling
---------

```bash
$ make      # try "make byte" if make does not work
```

Compiling the library (optional):

```bash
$ make lib  # try "make bytelib" if it does not work
```

Installing the executable
-------------------------

```
$ make install
```

The program is installed in the `BINDIR`
directory specified at the first line of the Makefile (`/usr/bin` by
default), and is named `caml2html` (even for bytecode option).

Uninstalling
------------

```
$ make uninstall
```

How to run it
-------------

Type `caml2html -help`, or have a look at the [html documentation](https://mjambon.github.io/mjambon2016/caml2html.html).

Authors and license
-------------------

Caml2html was originally written by
Sébastien Ailleret, and is now developed by Martin Jambon.
It is distributed for free under a GPL license (see `LICENSE` file).
