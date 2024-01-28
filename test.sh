#!/bin/bash

sbcl --noinform \
     --eval '(ql:quickload :str :silent t)' \
     --eval '(ql:quickload :lparallel :silent t)' \
     --eval '(ql:quickload :fiveam :silent t)' \
     --eval '(load "war-and-peace.lisp")' \
     --eval '(load "tests.lisp")' \
     --eval '(fiveam:run!)' \
     --quit
