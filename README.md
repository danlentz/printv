printv
======

>  PRINTV is a 'batteries-included' tracing and debug-logging macro based
> on __"The Handy PRINTV"__ by *Dan Corkill, Copyright (c) 2006-2010*,
> and open-source licensed under the terms of Apache License version 2.


### Motivation

There are a variety of sophisticated debugging facilities available to
the common-lisp programmer, and as well a number of very capable
logging utilities that range from very simple tools to fairly complex
application-grade suites.  In spite of this, one utility which I
almost always wind up turning to is the *"Handy PRINTV Macro"* that is
distributed as part of the GBBopen suite.  It is an effective
subistitute for tracing evaluation with ad-hoc printing forms that
doesn't require one to compose legions of cumbersome logging, PRINT or
FORMAT calls that explicitly enumerate each value one wishes to
incorporate into the trace.  It also provides a standardized format
for your debug-logging output that is both consistent and easy to
interpret via quick "eyeball" inspection. Debug-logging a trace of the
evaluation of program forms may be easily incorporated into existing
code by simply enclosing the form or forms within a PRINTV macro-call
(implicit progn).  __PRINTV always respects multiple-values__.

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

To start, here's a quick example that shows a few features in action:

    CL-USER> (:printv 
               :ff :|banner-text| :hr 
               ""
               "This is an example composition testing various PRINTV functionalities."
               "Self-evaluation of strings may be used within a printv form to effect"
               "written commentary."
               "" 
               :hr
               "" 
               (+ 2 3)              
               *print-case*
               *package*
               'symbol
               (let* ((x 0) (y (1+ x)) (z (1+ y)))
                 (values x y z)))

prints the following:

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; ======================================================================== ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    #|
     _                                      _            _   
    | |__   __ _ _ __  _ __   ___ _ __     | |_ _____  _| |_ 
    | '_ \ / _` | '_ \| '_ \ / _ \ '__|____| __/ _ \ \/ / __|
    | |_) | (_| | | | | | | |  __/ | |_____| ||  __/>  <| |_ 
    |_.__/ \__,_|_| |_|_| |_|\___|_|        \__\___/_/\_\\__|

    |#
    ;;; ------------------------------------------------------------------------ ;;;
    ;;; 
    ;;; This is an example composition testing various PRINTV functionalities.
    ;;; Self-evaluation of strings may be used within a printv form to effect
    ;;; written commentary.
    ;;; 
    ;;; ------------------------------------------------------------------------ ;;;
    ;;;
    ;;;   (+ 2 3) => 5
    ;;;   *PRINT-CASE* => :UPCASE
    ;;;   *PACKAGE* => #<PACKAGE "COMMON-LISP-USER">
    ;;;   'SYMBOL => SYMBOL
    ;;;   (LET* ((X 0) (Y (1+ X)) (Z (1+ Y)))
            (VALUES X Y Z)) =>
               [ [X=0]  [Y=1]  [Z=2] ]
    ;;;   => 0, 1, 2

and returns multiple-values:

    0
    1
    2

#### Configurables

* `*default-printv-output*` [`*trace-output*`]
> Controls the default stream to which PRINTV/PPMX output will be
> directed under the following circumstances: initially on program
> load, subsequent to any evaluation of `(enable-printv-output)`
> with stream argument unspecified, or within the dynamic extent of
> `(with-printv-output-enabled () ...)` macro-call (i.e., *stream*
> argument (second form) evaluates as NIL).


* `*printv-output*` [`*default-printv-output*`]
> The stream to which PRINTV/PPMX is currently directed.  May be
> affected using functions `enable-printv-output` and
> `disable-printv-output` or within the dynamic extent of macro
> calls `with-printv-output-enabled` and `with-printv-output-disabled.`


* `*major-separator*` [`:ff`]
> A special keyword defined to create a *thick* horizontal rule in
> printed output; it is not evaluated.


* `*minor-separator*` [`:hr`]
> A special keyword defined to create a *thin* horizontal rule in
> printed output; it is not evaluated.

* `*printv-macro-char*` [`#\^`]
> Character to use as macro-character to implement reader extension
> that will behave as if the next form that follows is enclosed within
> a PRINTV macro-call.  Return values and program operation will be
> unffected. 

* `*ppmx-macro-char*` [`#\$`]
> Character to use as macro-character to implement reader extension
> that will behave as if the next form that follows is enclosed within
> a PPMX macro-call.  Note that this expansion will NOT be evaluated, and
> so program operation may necessarily be affected if indescriminantly
> introduced into live code.

* `*figlet-executable*` [`"figlet"`]
>

* `*figlet-font*` [`"standard"`]
>

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
                                             
### Desiredata

1. Cleaner formatted output from LET, LET*, and COND. 
2. Consistent ";;;" prefixing for every line of output for LET, LET*,
   COND, and other multi-line evaluated forms.
3. Dynamic tracking of indent according to structure.
4. Forms with implicit progn occurring within printv should have that
   progn recursively PRINTVed, with incorporation of formatting
   described by items 1, 2, and 3, above.
5. Test Suite



