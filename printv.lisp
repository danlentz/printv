;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :printv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configurables - Adjust to Individual Taste
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *default-printv-output*     *trace-output*)
(defvar *printv-output*             *default-printv-output*)
(defvar *printv-lock*               (make-recursive-lock))

(defparameter *figlet-executable*      "figlet")
(defparameter *figlet-font*            "standard")
(defparameter *major-separator*        :ff)
(defparameter *minor-separator*        :hr)
(defparameter *timestamp-designator*   :ts)
(defparameter *printv-macro-char*      #\^)
(defparameter *ppmx-macro-char*        #\$)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output Enablement and Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun enable-printv-output (&optional (stream *default-printv-output*))
  (setf *printv-output* stream))
                                      
(defun disable-printv-output ()
  (setf *printv-output* (make-broadcast-stream)))

(defmacro with-printv-output-to ((&optional (destination *default-printv-output*))
                                  &body body)
  `(let ((*printv-output* ,destination))
     ,@body))

(defun disable-printv ()
  (enable-printv-output nil))

(defun enable-printv ()
  (enable-printv-output))

(defmacro with-printv-disabled (&body body)
  `(with-printv-output-to (nil)
     ,@body))

(defmacro with-printv-enabled (&body body)
  `(with-printv-output-to (*default-printv-output*)
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PPMX - Macroexpansion Pretty-Printer (from CCL)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ppmx (form)
  "Pretty prints the macro expansion of FORM."
  `(flet ((mx () 
            (let* ((exp1 (macroexpand-1 ',form))
                    (exp (macroexpand     exp1))
                    (*print-circle*        nil))
              (format *printv-output* "~%;;; Form: ~W"  (quote ,form))
              (cond
                ((equal exp exp1) (format *printv-output* "~%;;;~%;;; Macro expansion:~%")
                                  (pprint exp *printv-output*))
                (t                (format *printv-output* "~&;;; First step of expansion:~%")
                                  (pprint exp1 *printv-output*)
                                  (format *printv-output* "~%;;;~%;;; Final expansion:~%")
                                  (pprint exp *printv-output*)))
              (format *printv-output* "~%;;;~%;;; ")
              (values))))
     (etypecase *printv-output*
       (null      (values)) 
       (pathname  (with-recursive-lock-held (*printv-lock*)
                    (with-open-file (logfile *printv-output* 
                                      :direction :output
                                      :if-does-not-exist :create
                                      :if-exists :append)
                      (with-printv-output-to (logfile)
                        (mx)))))
       (stream    (with-recursive-lock-held (*printv-lock*)
                    (mx))))))

(setf (macro-function :ppmx) (macro-function 'ppmx))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRINTV - Extended Edition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adapted from the Handy PRINTV Macro Written by Dan Corkill
;; Copyright (C) 2006-2010, Dan Corkill <corkill@GBBopen.org>
;; Licensed under Apache License 2.0 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun minor-separator ()
  (format *printv-output* "~&;;; ~72,,,'-<-~> ;;;~%")
  (force-output *printv-output*))

(defun major-separator ()
  (format *printv-output* "~&;;;~77,,,';<;~>~%")
  (format *printv-output* "~&;;; ~72,,,'=<=~> ;;;~%")
  (format *printv-output* "~&;;;~77,,,';<;~>~%")
  (force-output *printv-output*))

(defun timestamp (&optional (time (get-universal-time)))
  (funcall #'format *printv-output*
  (let* ((ts-str (format-universal-time nil time))
          (ts-len (length ts-str))
          (pad0 (floor (- 80 ts-len 10) 2))
          (pad1 (ceiling (- 80 ts-len 10) 2)))
    (concatenate 'string
      "~&;;; "
      (make-string pad0  :initial-element #\.)
      " " ts-str " "
      (make-string pad1  :initial-element #\.)
      " ;;;~%"))))

(defun form-printer (form)
  (typecase form
    ;; String (label):
    (string (if (and (> (length form) 2) (equal (subseq form 0 2) "#|"))
              (format *printv-output* "~&~a~%" form)
              (format *printv-output* "~&;;; ~a~%" form)))
    ;; Evaluated form:
    ((or cons (and symbol (not keyword)))
      (format *printv-output* "~&;;;   ~w =>" form))
    (vector (format *printv-output* "~&;;   ~s~%" form)) 
    ;; Self-evaluating form:
    (t (format *printv-output* "~&;;;   ~s~%" form)))
  (force-output *printv-output*))

(defun values-printer (values-list)
  (format *printv-output* "~:[ [returned 0 values]~;~:*~{ ~w~^,~}~]~%"  values-list)
  (force-output *printv-output*))

(defmacro vlet* (bind-forms &body body)
  `(progn  (format *printv-output* "~&           [")
     (let* ,(mapcar #'(lambda (form)
                        (if (listp form)
                          `(,(car form) (let ((v ,(cadr form)))
                                          (format *printv-output*
                                            " [~S=~S] " ',(car form) v)
                                          v))
                          form))
              bind-forms)
       (format *printv-output* "]~&;;;   =>")
       ,@body)))

(defmacro vlet (bind-forms &body body)
  `(progn  (format *printv-output* "~&           [")
     (let ,(mapcar #'(lambda (form)
                       (if (listp form)
                         `(,(car form) (let ((v ,(cadr form)))
                                         (format *printv-output*
                                           " [~S=~S] " ',(car form) v)
                                         v))
                         form))
             bind-forms)
       (format *printv-output* "]~&;;;   =>")
       ,@body)))

(defmacro vcond (&body clauses)
  `(progn  (format *printv-output* "~&~%          ")
     (multiple-value-prog1 (cond ,@(mapcar #'(lambda (clause)
                                               `((let ((x ,(car clause)))
                                                   (format *printv-output*
                                                     " [~S -> ~S]~%          "
                                                     ',(car clause) x)
                                                   x)
                                                  ,@(cdr clause)))
                                     clauses))
       (format *printv-output* "~&;;;   =>"))))

(defun expander (forms &optional (values-trans-fn 'identity))
  (let ((result-sym (gensym)))
    `(flet ((exp-1 ()
              (let ((*print-readably* nil) ,result-sym)
                ,@(loop for form in forms nconcing
                    (cond
                      ;; Markup form:
                      ((eq form *major-separator*) (list '(major-separator)))
                      ((eq form *minor-separator*) (list '(minor-separator)))
                      ((eq form *timestamp-designator*) (list '(timestamp)))
                      ;; Binding form:
                      ((and (consp form) (or (eq (car form) 'let) (eq (car form) 'let*)))
                        `((form-printer (append '(,(car form) ,(cadr form)) ',(cddr form)))
                           (values-printer
                             (setf ,result-sym (funcall ',values-trans-fn
                                                 (multiple-value-list
                                                   ,(case (car form)
                                                      (let `(vlet ,@(rest form)))
                                                      (let* `(vlet* ,@(rest form))))))))))
                      ;; COND form:
                      ((and (consp form) (eq (car form) 'cond)) 
                        `((form-printer (append '(,(car form)) ',(rest form)))
                           (values-printer
                             (setf ,result-sym (funcall ',values-trans-fn
                                                 (multiple-value-list (vcond ,@(rest form))))))))
                      ;; FIGLET banner:             
                      ((and (keywordp form) (every (lambda (c)
                                                     (or
                                                       (and (alpha-char-p c) (lower-case-p c))
                                                       (not (alpha-char-p c))))
                                              (symbol-name form)))
                        `((form-printer 
                            ,(with-output-to-string (s)                      
                               (princ "#||" s)
                               (terpri s)
                               (ignore-errors
                                 (asdf/run-program:run-program
                                   (format nil "~A -f ~A -w ~D ~A" *figlet-executable*
                                     *figlet-font* *print-right-margin* (symbol-name form))
                                   :output s))
                               (princ "||#" s)))))
                      ;; Evaluated form:
                      ((or (consp form) (and (symbolp form) (not (keywordp form))))
                        `((form-printer ',form)
                           (values-printer
                             (setf ,result-sym (funcall ',values-trans-fn
                                                 (multiple-value-list ,form))))))
                      ;; Self-evaluating form:
                      (t `((form-printer 
                             (car (setf ,result-sym (list ,form))))))))
                (values-list ,result-sym))))
       (etypecase *printv-output*
         (null      ,(append '(progn) forms))
         (pathname  (with-recursive-lock-held (*printv-lock*)
                      (with-open-file (logfile *printv-output* 
                                        :direction :output
                                        :if-does-not-exist :create
                                        :if-exists :append)
                        (with-printv-output-to (logfile)
                          (exp-1)))))
         (stream    (with-recursive-lock-held (*printv-lock*)
                      (exp-1)))))))

(defmacro printv (&rest forms)
  (expander forms))

(setf (macro-function :printv) (macro-function 'printv))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRINTV/PPMX Readtable Extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+swank
(defun register-readtable (&optional (package *package*))
  (let ((package-name (typecase package
                        (string                package)
                        (symbol   (symbol-name package))
                        (package (package-name package)))))
    (push (cons package-name *readtable*) swank:*readtable-alist*)))

#-swank
(defun register-readtable (&optional (package *package*))
  nil)

(defun ppmx-reader (stream char)
  (declare (ignore char))
    (let ((body (read stream t nil t)))
      `(ppmx ,body)))

(defun enable-ppmx-reader (&optional (char *ppmx-macro-char*))
  (prog1 char
    (setf *readtable* (copy-readtable *readtable*))
    (set-macro-character char 'ppmx-reader)
    (register-readtable)))

(defun printv-reader (stream char)
  (declare (ignore char))
    (let ((body (read stream t nil t)))
      `(printv ,body)))

(defun enable-printv-reader (&optional (char *printv-macro-char*))
  (prog1 char
    (setf *readtable* (copy-readtable *readtable*))
    (set-macro-character char 'printv-reader)
    (register-readtable)))

;; (when (find-package :readtable)
;;   (let ((pubsyms '(ppmx-reader enable-ppmx-reader
;;                     printv-reader enable-printv-reader)))
;;   (import pubsyms :readtable)
;;   (export pubsyms :readtable)))


(enable-ppmx-reader)
(enable-printv-reader)

(register-readtable)
(pushnew :printv *features*)
