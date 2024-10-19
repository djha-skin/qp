;;;; main.lisp -- Quoted Printable tool.
;;;;
;;;; SPDX-FileCopyrightText: 2024 Daniel Jay Haskin
;;;; SPDX-License-Identifier: MIT
;;;;

(in-package #:cl-user)

(defpackage
  #:com.djhaskin.qp (:use #:cl #:arrows)
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
  (cond ((and (>= b 48)
              (<= b 57))
         (- b 48))
        ((and (>= b 65)
              (<= b 70))
         (+ 10 (- b 55)))
        ((and (>= b 97)
              (<= b 102))
         (+ 10 (- b 97)))))


(declaim (inline hex-number))
(defun hex-number (b1 b2)
  (declare (type (unsigned-byte 8) b1 b2))
  (let ((h1 (byte-to-hex b1))
        (h2 (byte-to-hex b2)))
    (when (and h1 h2)
      (+ (* 16 h1) h2))))

(defun extract-byte (strm)
  (let ((b (read-byte strm nil nil)))
    (when b
        (if (char= (code-char b) #\=)
            (let ((b2 (byte-to-hex (read-byte strm))))
              (when b2
                (let ((b3 (byte-to-hex (read-byte strm))))
                  (when b3
                  (+ (* 16 b2) b3)))))
            b))))

(defun transfer (stream-a stream-b)
  (loop for b = (extract-byte stream-a)
        while b do
        (write-byte b stream-b)))

(defun from (options)
  (let* ((result (make-hash-table :test #'equal))
         (ofile (cliff:ensure-option-exists :file options)))
    (if (equalp ofile "-")
        (transfer *standard-input* *standard-output*)
        (with-open-file (strm ofile :direction :output
                               :if-exists :supersede
                               :element-type '(unsigned-byte 8))
          (transfer strm *standard-output*)))
    (setf (gethash :status result) :successful)
    result))

(defparameter argv uiop:*command-line-arguments*)

(defun main (argv)
  (cliff:execute-program
    "qp"
    :default-function #'from
    :helps
    '((() . "Converts between quoted printable and binary."))
    :cli-arguments argv
    :reference-file #P"./.git/HEAD"
    :cli-aliases
    '(("-h" . "help")
      ("--help" . "help")
      ("-f" . "--set-file")
      ("--file" . "--set-file")
      ("--direction" . "--nrdl-direction"))
    :defaults
    '((:file . "-"))
    :suppress-final-output t))