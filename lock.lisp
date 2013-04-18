;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

;;; In order to eliminate any dependecies for the PRINTV system, and because
;;; only such a limited functionality is required (make-recursive-lock
;;; and with-recursive-lock-held) portable implementations have been
;;; incorporated from bordeaux-threads, in accordance with the following
;;; licensing terms:

;;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.
;;;

(in-package :printv)

#+sbcl       (progn 
               (defun make-recursive-lock (&optional name)
                 (sb-thread:make-mutex :name (or name "printv")))
               (defmacro with-recursive-lock-held ((place) &body body)
                 `(sb-thread:with-recursive-lock (,place)
                    ,@body)))

#+clozure    (progn
               (defun make-recursive-lock (&optional name)
                 (ccl:make-lock (or name "printv")))
               (defmacro with-recursive-lock-held ((place) &body body)
                 `(ccl:with-lock-grabbed (,place)
                    ,@body)))

#+allegro    (progn
               (defun make-recursive-lock (&optional name)
                 (mp:make-process-lock :name (or name "printv")))
               (defmacro with-recursive-lock-held ((place &key timeout) &body body)
                 `(mp:with-process-lock (,place :timeout ,timeout)
                    ,@body)))

#+clisp      (progn
               (defun make-recursive-lock (&optional name)
                 (mt:make-mutex :name (or name "printv")
                   :recursive-p t))
               (defmacro with-recursive-lock-held ((place) &body body)
                 `(mt:with-mutex-lock (,place) ,@body)))

#+cmucl      (progn
               (defun make-recursive-lock (&optional name)
                 (mp:make-lock (or name "printv")))
               (defmacro with-recursive-lock-held ((place &key timeout) &body body)
                 `(mp:with-lock-held (,place "Lock Wait" :timeout ,timeout) ,@body)))

#+ecl        (progn
               (defun make-recursive-lock (&optional name)
                 (mp:make-lock :name (or name "printv") :recursive t))
               (defmacro with-recursive-lock-held ((place) &body body)
                 `(mp:with-lock (,place) ,@body)))

#+lispworks  (progn
               (defun make-recursive-lock (&optional name)
                 (mp:make-lock :name (or name "printv")
                   #-(or lispworks4 lispworks5) :recursivep
                   #-(or lispworks4 lispworks5) t))
               (defmacro with-recursive-lock-held ((place) &body body)
                 `(mp:with-lock (,place) ,@body)))

#+scl        (progn
               (defun make-recursive-lock (&optional name)
                 (thread:make-lock (or name "printv")
                   :type :recursive))
               (defmacro with-recursive-lock-held ((place) &body body)
                 `(thread:with-lock-held (,place)
                    ,@body)))

#+abcl       (progn
               (defun make-recursive-lock (&optional name)
                 (make-mutex-recursive
                   :name (or name "printv")
                   :lock (jnew "java.util.concurrent.locks.ReentrantLock")))
               (defun acquire-recursive-lock (lock &optional (wait-p t))
                 (check-type lock mutex-recursive)
                 (cond
                   (wait-p (jcall +lock+ (mutex-recursive-lock lock)) t)
                   (t      (jcall +try-lock+ (mutex-recursive-lock lock)))))
               (defun release-recursive-lock (lock)
                 (check-type lock mutex-recursive)
                 (unless (jcall +is-held-by-current-thread+ (mutex-lock lock))
                   (error "Attempt to release lock not held by calling thread."))
                 (jcall +unlock+ (mutex-lock lock))
                 (values))
               (defmacro with-recursive-lock-held ((place) &body body)
                 `(unwind-protect (progn
                                    (acquire-recursive-lock ,place)
                                    ,@body)
                    (release-recursive-lock ,place))))
