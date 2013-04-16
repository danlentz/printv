;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(asdf:defsystem :printv
  :description      "printv: A batteries-included debug-logging macro"
  :author           "Dan Corkill <corkill@GBBopen.org>"  
  :author           "Dan Lentz <danlentz@gmail.com>"
  :maintainer       "Dan Lentz <danlentz@gmail.com>"
  :license          "Apache 2.0"
  :version          "0.1.0"
  :long-description "A batteries-included debug-logging macro based on
  'The Handy PRINTV Macro' by Dan Corkill Copyright (C) 2006-2010, Dan Corkill
  <corkill@GBBopen.org>, and open-source licensed under Apache License 2.0"
  :serial t
  :components ((:static-file "printv.asd")
                (:static-file "README.md")
                (:file        "printv")))

  
