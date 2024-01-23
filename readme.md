# War and Peace

This ANSI Common Lisp program categorizes the chapters of the book War and Peace by Leo Tolstoi into war-related and peace-related chapters.

## Environment Setup

This program was written using the [SBCL](https://www.sbcl.org/) Common Lisp implementation. As of this writing, SBCL 2.4.0 can compile and run this code; other Lisp implementations may also be capable of running it.

Installing SBCL on a Debian-based distribution should be as simple as issuing this command at a shell prompt:

```sh
$ sudo apt install sbcl
```

Other distributions may or may not package SBCL; refer to the SBCL project's [getting started](https://www.sbcl.org/getting.html) page for more information.


This program uses the [cl-str](https://github.com/vindarel/cl-str) library for string manipulation. [Quicklisp](https://www.quicklisp.org/beta/) is used for dependency management. Detailed instructions for installing and using Quicklisp may be found on that project's homepage, but here is a quickstart guide:

```sh
$ curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
$ sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
       --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
       --eval '(ql:add-to-init-file)' \
       --quit
```

With Quicklisp installed, your environment should be ready to run this program. 

## Program usage

The program is executed via `run.sh`. The program must be run from within this directory, or it will not find the files it needs. On the first run, Quicklisp will retrieve dependencies before loading the program. The result will be written to the file `categorization`.

Example:

```sh
$ git clone https://github.com/skyler544/war-and-peace
$ cd war-and-peace
$ ./run.sh
```
