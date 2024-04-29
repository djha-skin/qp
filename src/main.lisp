;;;; main.lisp -- Quoted Printable tool.
;;;;
;;;; SPDX-FileCopyrightText: 2024 Daniel Jay Haskin
;;;; SPDX-License-Identifier: MIT
;;;;

(in-package #:cl-user)

(defpackage
  #:com.djhaskin.qp (:use #:cl)
  (:documentation
    "
    Quoted Printable Content Character Encoding Tool.
    ")
  (:import-from #:com.djhaskin.nrdl)
  (:import-from #:com.djhaskin.cl-i)
  (:local-nicknames
    (#:nrdl #:com.djhaskin.nrdl)
    (#:cl-i #:com.djhaskin.cl-i)
    )
  (:export #:main))

(in-package #:com.djhaskin.qp)

(defun from (options)
  (declare (ignore options))
  (unless (gethash :input options)
    (error 'cl-i:exit-error :status :cl-usage-error :'
    (error "No input file specified."))
  (let* ((
  (let ((result (make-hash-table :test #'equal)))
    (setf (gethash :subcommand result) "from")
    result))

(defun none (options)
  (declare (ignore options))
  (let ((result (make-hash-table :test #'equal)))
    (setf (gethash :subcommand result) "none")
    result))

(defun to (options)
  (declare (ignore options))
  (let ((result (make-hash-table :test #'equal)))
    (setf (gethash :subcommand result) "to")
    result))

(defparameter argv uiop:*command-line-arguments*)

(defun main (argv)
  (multiple-value-bind (code results)
      (cl-i:execute-program
        "qp"
        (cl-i:system-environment-variables)
        `((() . ,#'none)
          (("from") . ,#'from)
          (("to") . ,#'to))
        :cli-arguments argv
        :helps
        '((() . "Prints this help message.")
          (("from") . "Convert from quoted printable")
          (("to") . "Convert to quoted printable")))
    code))