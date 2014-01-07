;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(defpackage :printv
  (:use :cl #+abcl :java)
  (:export
    #:ppmx
    #:printv
    #:ppmx-reader
    #:enable-ppmx-reader
    #:printv-reader
    #:enable-printv-reader
    #:enable-printv-output
    #:disable-printv-output
    #:with-printv-output-to
    #:disable-printv
    #:enable-printv
    #:with-printv-disabled
    #:with-printv-enabled
    #:format-universal-time
    #:format-decoded-time
    #:*figlet-font*
    #:*figlet-executable*
    #:*printv-output*
    #:*default-printv-output*
    #:*printv-macro-char*
    #:*ppmx-macro-char*
    #:*major-separator*
    #:*minor-separator*
    #:*timestamp-designator*))
