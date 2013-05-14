;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(asdf:defsystem :printv
  :description      "printv: a batteries-included tracing and debug-logging macro"
  :author           "Dan Corkill <corkill@GBBopen.org>"  
  :author           "Dan Lentz <danlentz@gmail.com>"
  :maintainer       "Dan Lentz <danlentz@gmail.com>"
  :license          "Apache 2.0"
  :version          "0.1.0"
  :long-description "PRINTV is a 'batteries-included' tracing and debug-logging macro
  based on 'The Handy PRINTV' by Dan Corkill Copyright (C) 2006-2010, Dan Corkill
  <corkill@GBBopen.org>, and open-source licensed under terms of Apache License 2.0.
  printv is available from http://github.com/danlentz/printv or from a current quicklisp
  dist."
  :serial  t
  :components ((:static-file "printv.asd")
                (:static-file "README.md")
                (:file        "package")
                (:file        "lock")
                (:file        "time")
                (:file        "printv")))

  
