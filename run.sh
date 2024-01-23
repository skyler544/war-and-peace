#!/bin/bash

sbcl --noinform \
     --eval '(ql:quickload :str :silent t)' \
     --eval '(ql:quickload :lparallel :silent t)' \
     --load 'war-and-peace.lisp' \
     --eval '(output-categorization)' \
     --quit
