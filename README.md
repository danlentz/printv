printv
======

> PRINTV is a 'batteries-included' tracing and debug-logging macro based
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

* Thread Safety
* Tracing lexical variable assignments in LET and LET* forms
* Tracing conditional evaluations inside COND forms
* Support a variety of output destinations, including to files and to arbitrary
  user-defined streams.
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
   * Human Readable Timestamp
   * Banner Text     (FIGLET generated)  
* Macro bindings on keywords (:PRINTV, :PPMX) in addition to standard
  symbols (PRINTV:PRINTV, PRINTV:PPMX) for ease of use globally
* Inclusion of PPMX macro-expander distributed by Clozure Associates with
  their excellent Common-Lisp implementation: *Clozure Common Lisp*

### Usage

To start, here's a quick example that shows a few features in action:

    CL-USER> (:printv                
               :|.. printv-output ..|
               ""
               :hr
               :ts
               :hr
               ""
               "This is an example composition testing various PRINTV functionalities."
               "Self-evaluation of strings may be used within a PRINTV form to augment"
               "the log output with annotations or other commentary."
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

    #||
                      _       _                          _               _         
           _ __  _ __(_)_ __ | |___   __      ___  _   _| |_ _ __  _   _| |_       
          | '_ \| '__| | '_ \| __\ \ / /____ / _ \| | | | __| '_ \| | | | __|      
     _ _  | |_) | |  | | | | | |_ \ V /_____| (_) | |_| | |_| |_) | |_| | |_   _ _ 
    (_|_) | .__/|_|  |_|_| |_|\__| \_/       \___/ \__,_|\__| .__/ \__,_|\__| (_|_)
          |_|                                               |_|                    
    ||#
    ;;;
    ;;; ------------------------------------------------------------------------ ;;;
    ;;; .............. Wednesday, April 17, 2013 06:39:03 PM EDT ............... ;;;
    ;;; ------------------------------------------------------------------------ ;;;
    ;;; 
    ;;; This is an example composition testing various PRINTV functionalities.
    ;;; Self-evaluation of strings may be used within a printv form to augment
    ;;; the log output with annotations or other commentary.
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

#### Basic Form Evaluation and Tracing 

The fundamental purpose of PRINTV is to log a trace of the forms
within its lexical scope and to log the result of their evaluation to
a specified output destination.  This allows the user to closely monitor
the execution of program code, to better understand its
operation, and to quickly identify problems that may occur due to
undexpected results.

Here is a simple example that illustrates this basic functionality.
Say one has written the following code to perform some desired
calculation:

    (defvar *y*) 
    (defparameter *x* 2)
    (setf *y* (sqrt *x*))
    (setf *y* (/ 1 *y*))
    
Evaluation of this block of code results in the value `0.70710677.`
To understand more clearly (or simply to track), it's operation, one
can simply enclose the code within PRINTV. The principlal benefit of
instrumenting your code with PRINTV derives from the capability to produce
a trace, form by form as they are evaluated, and possibly to archive this
detailed record of its execution to persistent storage media.

    (:printv
      (defvar *y*) 
      (defparameter *x* 2)
      (setf *y* (sqrt *x*))
      (setf *y* (/ 1 *y*)))
      
This produces the following text to PRINTV's output stream, and still
results in the same returned value:  `0.70710677.`

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
    
#### LET, LET*, and COND: Binding Forms and Conditional Clauses

For certain, more structurally complex forms, simply printing the
value to which it evaluates does not necessarily provide much insight
as to how that value may have been arrived at.  Lexical binding forms,
LET and LET*, are such a case.  These forms often perform significant
calculations as part of their initial binding lists that are
subsequently referenced by the following series of form(s) within
their implicit progn. For example, the following code should return
a humanly readable string that denotes the time exactly 24 hours ago:

    (let* ((a (get-universal-time))
           (b (- a 86400)))
       (format-universal-time nil b))

Might return `"Tuesday, April 16, 2013 11:45:50 AM EDT"`, but the
significant calculation that is performed to arrive at this result is
not apparent from just the result of this evaluation. To get a better
understanding what is happening, one needs to see the values that are
lexically bound to the symbols `a` and `b`. PRINTV supports this kind
of introspection by providing special handling of LET and LET* binding
forms:

    (printv
      (let* ((a (get-universal-time))
             (b (- a 86400)))
         (format-universal-time nil b)))

Logs the following:

    ;;;   (LET* ((A (GET-UNIVERSAL-TIME)) (B (- A 86400)))
            (FORMAT-UNIVERSAL-TIME NIL B)) =>
               [ [A=3575203206]  [B=3575116806] ]
    ;;;   => "Tuesday, April 16, 2013 12:00:06 PM EDT"

Notice the values bound to `a` and `b` are logged with a printed
representation denoted by square brackets.

Similarly, within COND forms, evaluation of the clause heads is of
interest not only to track what they evaluate *to*, but also to see
*which* ones are evaluated *at all* (COND clauses provide
short-circuiting semantics).  PRINTV provides special handling for
these as well:

    (printv
      (cond
        ((null     :x) (values "no"  1))
        ((stringp  :x) (values "no"  2))
        ((symbolp  :x) (values "no"  3))
        ((keywordp :x) (values "yes" 4))
        (t             (values "no"  5))))

Logs the following:
          
    ;;;   (COND ((NULL :X) (VALUES "no" 1)) ((STRINGP :X) (VALUES "no" 2))
                ((SYMBOLP :X) (VALUES "no" 3)) ((KEYWORDP :X) (VALUES "yes" 4))
                (T (VALUES "no" 5))) =>
           [(NULL :X) -> NIL]
           [(STRINGP :X) -> NIL]
           [(SYMBOLP :X) -> T]          
    ;;;   => "no", 3

And returns multiple-values:

    "no"
    3

Notice that each cond clause evaluated, depicted by a bracketed
*[clause -> result]* representation, is displayed below the COND form
and before the final result in the printv output text.  By examining
this output, it is evident that the successful clause head was
`(symbolp :x)` which, of course, is true, but that the more specific
clause head `(keywordp :x)` that, most likely, was the author's
intent, was never evaluated. This is a common error which can be fixed
by reordering the two clauses, but one that can be sometimes be
elusive in actual code.  With PRINTV, it was immediately obvious what
the problem was and how to resolve it:

    (printv
      (cond
        ((null     :x) (values "no"  1))
        ((stringp  :x) (values "no"  2))
        ((keywordp :x) (values "yes" 3))        
        ((symbolp  :x) (values "no"  4))
        (t             (values "no"  5))))

Now logs:
          
    ;;;   (COND ((NULL :X) (VALUES "no" 1)) ((STRINGP :X) (VALUES "no" 2))
                ((KEYWORDP :X) (VALUES "yes" 3)) ((SYMBOLP :X) (VALUES "no" 4))
                (T (VALUES "no" 5))) =>
           [(NULL :X) -> NIL]
           [(STRINGP :X) -> NIL]
           [(KEYWORDP :X) -> T]          
    ;;;   => "yes", 3

And, as was the programmer's intent, now correctly returns:

    "yes"
    3

This type of programmer error does not cause a warning that can be
reported by the compiler, or flag an error to alert the user at runtime. In
fact, the original version of the code is entirely *valid*, but it does not
correctly represent the programmer's intent.  This is just one of many
situations in which the 'tracing' functionality of PRINTV is
indespensible for quick identification and resolution of errors in
code semantics.

By helping to quickly identify and locate the faulty
semantics caused by improper order of the COND clauses, PRINTV has
possibly saved you enough time for a coffee-break!
    
#### Extended Typography: Bells, and Whistles

As you begin to spend time crafting and annotating your debug-logging
output, you may start to think of what you include within PRINTV as a
primitive text markup DSL. And so you may begin to wish for a little
more typographic panache.  This, at least, occurs for me now and
again. It is very important, though, that in our pursuit of ever more
beautiful output we do not introduce artifacts that could change the
meaning or operation of code enclosed in a PRINTV form.  This limits
our options when choosing the manner in which we can implement such
features.

You may have noticed from the first example shiown in section USAGE
that the keywords `:hr` and `:ts` were used to insert into the log
a thin horizontal rule and timestamp, respectively.  For these kinds of
simple, self-contained tags, it is reasonable that we can choose a few
keywords such as these and handle them specially when encountered
within the implicit progn of a PRINTV. In doing so, we can simply
specify that they will not be evaluated, effectively returning
`(values)`. Inclusion of these tags will have no effect, even if
they are used in the final (value-returning) position of the
progn. Further, the special keywords that control these typographic
features are user-configurable (see CONFIGURABLES, below).

Be aware, however, that although useful, these typographic extensions
may potentially at odds with the PRINTV philosophy of pure
transparency. In particular, although the formatting tags are
"invisible" within the scope of PRINTV and returned values of
evaluated forms are automatically passed through to be correctly
returned form PRINTV when these tags appear in the tail position of
PRINTV's implicit progn, this will not the case if PRINTV is disabled
(see `disable-printv`) or when the printv is removed.  The safest
approach is to never use these tags in the tail position -- making
sure that the final form is the one returning the desired values.  In
practice I have not found this to be difficult to manage, and,
practically speaking, I have not come up with a *totally* safe
alternative means to specify extended formatting.  In my opinion, the
benefits are worth this small inconvenience; also, one is always free
to ignore the extended formatting features entirely, or even disable
them by configuring them to be uninterned symbols, like so:

    (setf *major-separator*      (gensym))
    (setf *minor-separator*      (gensym))
    (setf *timestamp-designator* (gensym))    

The following are a few examples which illustrate some PRINTV's that
use the timestamp, thin and thick rule features:

    (printv :hr "Section 1." :hr)

Prints:

    ;;; ------------------------------------------------------------------------ ;;;
    ;;; Section 1.
    ;;; ------------------------------------------------------------------------ ;;;
    
Returns:

    "Section 1."

Similarly:

    (printv :ff :hr (* 2 (+ 3 (/ -1 4))) :hr :ff)

Prints:

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; ======================================================================== ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; ------------------------------------------------------------------------ ;;;
    ;;;   (* 2 (+ 3 (/ -1 4))) => 11/2
    ;;; ------------------------------------------------------------------------ ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; ======================================================================== ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Returns:

    11/2

Another special markup tag (by default) is `:ts`, which causes a
human-readable timestamp to be incorporated into the printv log output:

    (printv :hr :ts :hr (machine-version) :hr)

Prints:

    ;;; ------------------------------------------------------------------------ ;;;
    ;;; .............. Wednesday, April 17, 2013 06:39:03 PM EDT ............... ;;;
    ;;; ------------------------------------------------------------------------ ;;;
    ;;;   (MACHINE-VERSION) => "Intel(R) Xeon(R) CPU           E5462  @ 2.80GHz"
    ;;; ------------------------------------------------------------------------ ;;;

Returns:

    "Intel(R) Xeon(R) CPU           E5462  @ 2.80GHz"

  

#### Enablement and Control of Output

A principle design-goal of this PRINTV library is to provide a number
of options of the destinations to where the output may be directed,
and a robust api with which the user may flexibly select one
within some particular dynamic extent of program
execution. Under-the-hood, output control is implemeted using two
special-variables: `*default-printv-output*` and `*printv-output*`
(see CONFIGURABLES).





#### Macro debugging with PPMX

As one last example, using the magic of PPMX we can finally have a
look at the internal operation of PRINTV itself -- this is very
frequently necessary when developing extensions to PRINTV.  We will show
the macroexpansion of a form used in one of our previous examples:

    (ppmx (printv :hr :ts :hr (machine-version) :hr))

Prints:
    
    ;;; Form: (PRINTV :HR :TS :HR (MACHINE-VERSION) :HR)
    ;;;
    ;;; Macro expansion:

    (FLET ((EXP-1 ()
             (LET ((*PRINT-READABLY* NIL) #:G3262)
               (MINOR-SEPARATOR)
               (TIMESTAMP)
               (MINOR-SEPARATOR)
               (FORM-PRINTER '(MACHINE-VERSION))
               (VALUES-PRINTER
                (SETF #:G3262 (FUNCALL # (MULTIPLE-VALUE-LIST (MACHINE-VERSION)))))
               (MINOR-SEPARATOR)
               (VALUES-LIST #:G3262))))
      (ETYPECASE *PRINTV-OUTPUT*
        (NULL (PROGN :HR :TS :HR (MACHINE-VERSION) :HR))
        (PATHNAME
         (BORDEAUX-THREADS:WITH-RECURSIVE-LOCK-HELD (*PRINTV-LOCK*)
           (WITH-OPEN-FILE
               (LOGFILE *PRINTV-OUTPUT* :DIRECTION :OUTPUT :IF-DOES-NOT-EXIST
                :CREATE :IF-EXISTS :APPEND)
             (WITH-PRINTV-OUTPUT-TO (LOGFILE)
               (EXP-1)))))
        (STREAM
         (BORDEAUX-THREADS:WITH-RECURSIVE-LOCK-HELD (*PRINTV-LOCK*)
           (EXP-1)))))
    ;;;
    ;;;     

    
#### Configurables

* `*default-printv-output*` [`*trace-output*`]
> Controls the default stream to which PRINTV/PPMX output will be
> directed under the following circumstances: initially on program
> load, subsequent to any evaluation of `(enable-printv)` or to `(enable-printv-output)`
> with stream argument unspecified, or within the dynamic extent of
> `(with-printv-output-to () ...)` macro-call (i.e., *stream*
> argument (second form) as NIL).

* `*printv-output*` [`*default-printv-output*`]
> The stream to which PRINTV/PPMX is currently directed. Types of
> valid values this may hold include streams (log to stream), pathnames
> (log to file), or null (disable printv). May be
> affected using functions `enable-printv-output` and
> `disable-printv-output` or within the dynamic extent of macro
> call `with-printv-output-to`. See also the more powerful
> 'enablement' controls provided by: `enable-printv`,
> `disable-printv`, `with-printv-enabled`, and `with-printv-disabled.`

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



