printv
======

>   A batteries-included debug-logging macro adapted from __"The Handy PRINTV Macro"__
> by *Dan Corkill, Copyright (c) 2006-2010*, and open-source licensed under the terms of
> Apache License version 2.0


### Motivation

There are a variety of sophisticated debugging facilities available to
the common-lisp programmer, and as well a number of very capable
logging utilities that range from very simple tools to fairly complex
application-grade suites.  In spite of this, one utility which I
almost always wind up turning to is the *"Handy PRINTV Macro"* that is
distributed as part of the GBBopen suite.  It is an effective
subistitute for ad-hoc PRINT forms that doesn't require one to compose
a complete PRINT or (worse yet FORMAT) that explicitly enumerates each
value to include and provides a standardized format of debug-logging
output that is consistent and easy to interpret via quick "eyeball"
inspection. Debug-logging may be easily incorporated into existing
code by simply enclosing a form or forms (implied-progn) by a PRINTV
macro.  __PRINTV always respects multiple values__.

As I have wound up copying this utility from project to project,
incorporating various extensions and tweaks along the way, it occurred
to me that it was probably time to spin off my "extended" printv into
a standalone library.  Also included is the Clozure Associates' *PPMX*
macro-expansion macro which is very useful in its own right, and has
proven invaluable during development and debugging of PRINTV.

The extended features implemented (apart from having printv available
independently of the massive GBBopen project) include:

* Tracing lexical variable assignments in LET and LET* forms

* Tracing conditional evaluations inside COND forms

* Character-macros to support DWIM 'PRINTV the following form' reader
  instead of unsightly, cumbersome, and error-prone nesting of (PRINTV
  ...) s-expression structure that becomes increasingly problematic
  to understand and more-so to (eventually) remove

* Support for enabling and disabling PRINTV output to user-selected stream
  (initially \*TRACE-OUTPUT\*) with effect of either global or dynamic extent

* Support for additional typographic functionality that can generate
  easy-to-read output that is both attractive and utilitarian for structuring
  trace output in a manner that is easy to discern by eye and navigate when
  seeking a particular segment of output.   Included are the following:
   * Major Separator (thick horizontal rule)
   * Minor Separator (thin horizontal rule)
   * Banner Text     (*FIGLET* generated)

   
* Macro bindings on keywords (:PRINTV :PPMX) for ease of access globally

* Inclusion of *PPMX* macro-expander distributed by Clozure Associates with
  their excellent Common-Lisp implementation: *Clozure Common Lisp*.

### Usage

#### Configurables

#### Basic Form Evaluation and Tracing 

#### Tracing LET and LET* lexical assignments

#### Tracing Evaluation of COND clauses

#### Extended Typographic Bells and Whistles

#### Macro debugging with PPMX

#### Enablement and Control of Output

### Extension

