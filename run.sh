#!/bin/bash

sbcl --eval '(ql:quickload :str)' \
     --load 'war-and-peace.lisp' \
     --eval '(categorize-book)' \
     --quit
