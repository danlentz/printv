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
* Support for fully disabling printv with global or dynamic extent in order to 
  allow printv instrumented code to operate at full performance that should be
  nearly identical to that of equivalent to code without such
  instrumentation.
* Support enablement and disablement of PRINTV output to user-selected stream
  (initially \*TRACE-OUTPUT\*) effective for global or dynamic extent.
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
> unffected, and so may be used as-needed without fear of corrupting
> live code.

* `*ppmx-macro-char*` [`#\$`]
> Character to use as macro-character to implement reader extension
> that will behave as if the next form that follows is enclosed within
> a PPMX macro-call.  Note that this expansion will NOT be evaluated, and
> so program operation may necessarily be affected if indescriminantly
> introduced into live code.

* `*figlet-executable*` [`"figlet"`]
> Specifies the path where the desired FIGLET executable is found on
> this system. If provided as simple command name without
> specification of absolute directory (the default), this command will
> be invoked if valid executable with this name can be found in the
> default shell search path of the user who owns the current common-lisp
> process. 

* `*figlet-font*` [`"standard"`]
> The name of the (ASCII) 'figlet font' to be used in FIGLET output.

#### Basic Form Evaluation and Tracing 

The fundamental purpose of PRINTV is to log a trace of the forms with
its scope and the result of their evaluation to a specified output
stream.  This allows the user to closely monitor the execution of
program code in order to better understand its operation and to
quickly identify problems that may occur due to undexpected results.

A simple example that illustrates this basic functionality is shown
below.  Say one wishes to trace the execution of the following
calculation:

    (defvar *y*) 
    (defparameter *x* 2)
    (setf *y* (sqrt *x*))
    (setf *y* (/ 1 *y*))
    
Evaluation of this block of code results in the value `0.70710677.` To
understand more clearly (or simply to track), it's operation, one
might simply enclose the code within PRINTV. The result is the ability
to see a trace, form by form, of its evaluation:

    (:printv
      (defvar *y*) 
      (defparameter *x* 2)
      (setf *y* (sqrt *x*))
      (setf *y* (/ 1 *y*)))
      
This produces the following text to PRINTV's output stream, but still
results in the same value:  `0.70710677.`

    ;;;   (DEFVAR *Y*) => *Y*
    ;;;   (DEFPARAMETER *X* 2) => *X*
    ;;;   (SETF *Y* (SQRT *X*)) => 1.4142135
    ;;;   (SETF *Y* (/ 1 *Y*)) => 0.70710677

To take a step further, one might augment the block of code as
follows:

    (:printv
      (defvar *y*) 
      (defparameter *x* 2)
      (setf *y* (sqrt *x*))
      *x*
      *y*
      (setf *y* (/ 1 *y*)))

prints:

    ;;;   (DEFVAR *Y*) => *Y*
    ;;;   (DEFPARAMETER *X* 2.0) => *X*
    ;;;   (SETF *Y* (SQRT *X*)) => 1.4142135
    ;;;   *X* => 2.0
    ;;;   *Y* => 1.4142135
    ;;;   (SETF *Y* (/ 1 *Y*)) => 0.70710677

and correctly returns the value of the final form:

    0.70710677

The semantics (apart from tracing the evaluation of enclosed forms)
are exactly as in PROGN -- the idea being that PRINTV may be used
liberally just about anywhere, without changing the meaning or
operation of the forms enclosed.  Notice that bound symbols are also
'forms'.  Inclusion of `*x*` and `*y*` forms within the implicit-progn
of a PRINTV expression (in any but the last position) has no effect on
the overall semantics of the given of code, but a record of the
symbol-name and its value at the current point of execution is
incorporated into the text sent to PRINTV's output stream.  This is
the essence of logging with PRINTV.

Self-evaluating forms, such as literal strings, are also useful within
the context of PRINTV.  Since they evaluate to themselves, there is
little point in showing the result of this evaluation. Instead,
self-evaluating forms are handled specially in order to provide
adiitional capabilities that may be useful within PRINTV's logging
output.  In the case of literal strings, the text of the string is
presented directly within the logging output, providing a quick means
of augmenting your logs with descriptive annotations or other helpful
exposition:

    (:printv
      "This was my homework assignment over spring break"
      ""
      (defvar *y*) 
      (defparameter *x* 2)
      (setf *y* (sqrt *x*))
      (setf *y* (/ 1 *y*)))

This produces a slightly more informative output to the PRINTV
stream. Notice that the empty string `""` was used to insert a 'blank'
line within the logged output:

    ;;; This was my homework assignment over spring break
    ;;; 
    ;;;   (DEFVAR *Y*) => *Y*
    ;;;   (DEFPARAMETER *X* 2) => *X*
    ;;;   (SETF *Y* (SQRT *X*)) => 1.4142135
    ;;;   (SETF *Y* (/ 1 *Y*)) => 0.70710677
    
One might make use of other simple self-evaluating forms, such as
numbers and 'ordinary' keywords (more on these later) to further
annotate the output:

    (:printv :FINAL-ANSWER  "" 0.70710677)

Prints:

    ;;;   :FINAL-ANSWER
    ;;; 
    ;;;   0.70710677

Finally, you may rely on PRINTV to always respect multiple-values,
which are denoted in logging output as a series of comma-separated
forms. Multiple-values are always correctly returned when produced by
evaluation of any form, both within PRINTV's implicit progn and when
produced as the result of the final form:

    (:printv
       (values 1 2 3)
       (values 'a 'b 'c))

Logs the following:           

    ;;;   (VALUES 1 2 3) => 1, 2, 3
    ;;;   (VALUES 'A 'B 'C) => A, B, C

And returns as multiple-values:

    A
    B
    C
    
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



