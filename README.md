printv
======

>   A batteries-included debug-logging macro based on __"The Handy PRINTV Macro"__
> by *Dan Corkill, Copyright (c) 2006-2010*, and open-source licensed under the terms of
> Apache License version 2.0


### Motivation

There are a variety of sophisticated debugging facilities available to
the common-lisp programmer, and as well a number of very capable
logging utilities that range from very simple tools to fairly complex
application-grade suites.  In spite of this, one utility which I
almost always wind up turning to is the *"Handy PRINTV Macro"* that is
distributed as part of the GBBopen suite.  It is an effective
subistitute for tracing evaluation with ad-hoc PRINT forms that
doesn't require one to compose legions of complete PRINT or (worse yet
FORMAT) calls explicitly enumerating each value one wishes to
incorporate in the trace.  It also provides a standardized format for
your debug-logging output that is both consistent and easy to
interpret via quick "eyeball" inspection. Debug-logging may be easily
incorporated into existing code by simply enclosing a form or forms
(implicit progn) within a PRINTV macro.  __PRINTV always respects
multiple-values__.

As I have wound up copying this utility from project to project,
incorporating various extensions and tweaks along the way, it occurred
to me that it was probably time to spin off my "extended" PRINTV into
a standalone library.  Also included is the Clozure Associates' *PPMX*
macro-expansion macro which is very useful in its own right, and has
proven invaluable during development and debugging of PRINTV.

The extended features implemented (in addition to making PRINTV available
independently of the massive GBBopen project) include:

* Tracing lexical variable assignments in LET and LET* forms
* Tracing conditional evaluations inside COND forms
* Character-macro to support DWIM *'PRINTV the following form'* reader extension
  instead of unsightly, cumbersome, and error-prone nesting of (PRINTV
  ...) s-expression structure that becomes increasingly problematic
  to understand and even more-so to (eventually) remove
* Support for enabling and disabling PRINTV output to user-selected stream
  (initially \*TRACE-OUTPUT\*) effective for global or dynamic extent
* Support for additional typographic functionality that can generate
  output that is both attractive and utilitarian for structuring
  trace output in a manner that is easy to discern by eye and to navigate when
  seeking a particular segment of output.   Included are: 
   * Major Separator (thick horizontal rule)
   * Minor Separator (thin horizontal rule)
   * Banner Text     (FIGLET generated)  
* Macro bindings on keywords (:PRINTV, :PPMX) in addition to standard
  symbols (PRINTV:PRINTV, PRINTV:PPMX) for ease of use globally
* Inclusion of PPMX macro-expander distributed by Clozure Associates with
  their excellent Common-Lisp implementation: *Clozure Common Lisp*

### Usage

A quick example that shows a few features in action:

    CL-USER> (:printv 
               :ff :|banner| :hr 
               ""
               "this is an example composition testing PRINTV functionality"
               "" 
               :hr
               ""
               *print-case* 
               (let ((x 0) (y 1) (z 2))
                 (values x y z)))

prints the following:

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; ======================================================================== ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    #|
     _                                 
    | |__   __ _ _ __  _ __   ___ _ __ 
    | '_ \ / _` | '_ \| '_ \ / _ \ '__|
    | |_) | (_| | | | | | | |  __/ |   
    |_.__/ \__,_|_| |_|_| |_|\___|_|   

    |#
    ;;; ------------------------------------------------------------------------ ;;;
    ;;; 
    ;;; this is an example composition testing PRINTV functionality
    ;;; 
    ;;; ------------------------------------------------------------------------ ;;;
    ;;;
    ;;;
    ;;;   *PRINT-CASE* => :UPCASE
    ;;;   (LET ((X 0) (Y 1) (Z 2))
            (VALUES X Y Z)) =>
               [ [X=0]  [Y=1]  [Z=2] ]
    ;;;   => 0, 1, 2

and returns multiple-values:

    0
    1
    2

#### Configurables

#### Basic Form Evaluation and Tracing 

#### Tracing LET and LET* lexical assignments

#### Tracing Evaluation of COND clauses

#### Extended Typographic Bells and Whistles

#### Macro debugging with PPMX

#### Enablement and Control of Output

### Extension

For the most part, extensions to PRINTV may be incorporated by
implementing additional clauses within the function `EXPANDER`, using
the existing clauses as a template to get started. Typically,
evaluated forms will involve construction of two function-call
s-expressions: a `FORM-PRINTER` to show what is being evaluated, and a
`VALUES-PRINTER` to show the result.

For all expansions except those intended as simple typographic
commands, it is very important that as a result of expansion the
symbol designated by `RESULT-SYM` is set to a multiple-value-list of
the evaluation of the original form transformed by the function
designated by `VALUES-TRANS-FN`, and is supplied as the argument to
the `VALUES-PRINTER` function call.

For example, the `EXPANDER` clause of a simple, evaluated form appears
as follows. Note the first line which, if it evaluates to a non-nil
value, designates the expansion that follows to be the one
applicable for the given form:

    ((or (consp form) (and (symbolp form) (not (keywordp form))))
      `((form-printer   ',form)
        (values-printer (setf ,result-sym (funcall ,values-trans-fn
                                           (multiple-value-list ,form))))))
      




