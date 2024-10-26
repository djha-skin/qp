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
  (:import-from #:com.djhaskin.cliff)
  (:import-from #:cl-ppcre)
  (:local-nicknames
    (#:nrdl #:com.djhaskin.nrdl)
    (#:cliff #:com.djhaskin.cliff)
    )
  (:export #:main))

(in-package #:com.djhaskin.qp)

(declaim (inline byte-to-hex))
(defun byte-to-hex (b)
  (declare (type (unsigned-byte 8) b))
  ;; If the digit is 0-9
  (cond ((and (>= b #x30)
              (<= b #x39))
         (- b #x30))
        ;; If the digit is A-F
        ((and (>= b #x41)
              (<= b #x46))
         (+ 10 (- b #x41)))
        ;; If the digit is a-f
        ((and (>= b #x61)
              (<= b #x66))
         (+ 10 (- b #x61)))))

(defun extract-byte (strm)
  (let ((b (read-byte strm nil nil)))
    (when b
      (if (= b #x3D) ;; character `=`
          (let ((next (read-byte strm)))
            (if (= next #x0A)
                :continue
                (let ((b2 (byte-to-hex next)))
                  (when b2
                    (let ((b3 (byte-to-hex (read-byte strm))))
                      (when b3
                        (+ (* 16 b2) b3)))))))
          b))))

(defparameter *col* 0)

(defun write-limited (b strm)
  (when (or (>= *col* 75)
            (and (= b #x3D)
                 (> *col* 72)))
    (write-byte #x3D strm)
    (write-byte #x0A strm)
    (setf *col* 0))
  (write-byte b strm)
  (incf *col*))

(defun hex-digit-byte (d)
  (if (and (>= d 0)
           (< d 10))
      (+ d #x30)
      (+ (- d 10) #x41)))


(defun transfer (options stream-a stream-b)
  (let ((rev (gethash :reverse options)))
    (if rev
        (loop for b = (read-byte stream-a nil nil)
              while b do
              (if (and (>= b #x20) (< b #x7f) (/= b #x3d))
                  (write-limited b stream-b)
                  (progn
                    (write-limited #x3D stream-b)
                    (multiple-value-bind (quotient remainder)
                        (truncate b 16)
                      (write-limited (hex-digit-byte quotient) stream-b)
                      (write-limited (hex-digit-byte remainder) stream-b)))))
        (loop for b = (extract-byte stream-a)
              while b do
              (when (not (eql b :continue))
                (write-byte b stream-b))))))

(defun qp (options)
  (let* ((result (make-hash-table :test #'equal))
         (ofile (cliff:ensure-option-exists :file options)))
    (if (equalp ofile "-")
        (transfer options *standard-input* *standard-output*)
        (with-open-file (strm ofile :direction :output
                               :if-exists :supersede
                               :element-type '(unsigned-byte 8))
          (transfer options strm *standard-output*)))
    (setf (gethash :status result) :successful)
    result))

(defparameter argv uiop:*command-line-arguments*)

(defun main (argv)
  (cliff:execute-program
    "qp"
    :default-function #'qp
    :helps
    '((() . "Converts between quoted printable and binary."))
    :cli-arguments argv
    :reference-file #P"./.git/HEAD"
    :cli-aliases
    '(("-h" . "help")
      ("--help" . "help")
      ("-f" . "--set-file")
      ("--file" . "--set-file")
      ("--reverse" . "--enable-reverse")
      ("-r" . "--enable-reverse")
      ("-R" . "--disable-reverse"))
    :defaults
    '((:file . "-"))
    :suppress-final-output t))