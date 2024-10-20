;;;; main.lisp -- Tests for the RSS syncer, if any. (Likely not.)
;;;;
;;;; SPDX-FileCopyrightText: 2024 Daniel Jay Haskin
;;;; SPDX-License-Identifier: MIT

#+(or)
(progn
  (declaim (optimize (speed 0) (space 0) (debug 3)))
  (progn
    (asdf:load-system "parachute")
    (asdf:load-system "com.djhaskin.qp")
    (asdf:test-system "com.djhaskin.qp")))


(in-package #:cl-user)

(defpackage #:com.djhaskin.qp/tests
  (:use #:cl)
  (:import-from
    #:org.shirakumo.parachute
    #:define-test
    #:true
    #:false
    #:fail
    #:is
    #:isnt
    #:is-values
    #:isnt-values
    #:of-type
    #:finish
    #:test)
  (:import-from
    #:com.djhaskin.qp)
  (:local-nicknames
    (#:parachute #:org.shirakumo.parachute)
    (#:qp #:com.djhaskin.qp)))

(in-package #:com.djhaskin.qp/tests)

(define-test hex-stuff)
(define-test byte-to-hex
  :parent hex-stuff)

(define-test "byte-to-hex: well defined range"
  :parent byte-to-hex
  (loop for a from 0 below 127
        do
        (let ((response (qp::byte-to-hex a)))
          (true (or (null response)
                    (and (>= response 0)
                         (< response 16)))
                "~@{~@?~}"
                "byte-to-hex at ~A was ~A" a response))))

(define-test "byte-to-hex: correct value of digits"
  :parent byte-to-hex
  (is eql (qp::byte-to-hex (char-code #\a)) 10)
  (is eql (qp::byte-to-hex (char-code #\b)) 11)
  (is eql (qp::byte-to-hex (char-code #\c)) 12)
  (is eql (qp::byte-to-hex (char-code #\d)) 13)
  (is eql (qp::byte-to-hex (char-code #\e)) 14)
  (is eql (qp::byte-to-hex (char-code #\f)) 15)
  (is eql (qp::byte-to-hex (char-code #\A)) 10)
  (is eql (qp::byte-to-hex (char-code #\B)) 11)
  (is eql (qp::byte-to-hex (char-code #\C)) 12)
  (is eql (qp::byte-to-hex (char-code #\D)) 13)
  (is eql (qp::byte-to-hex (char-code #\E)) 14)
  (is eql (qp::byte-to-hex (char-code #\F)) 15)
  (is eql (qp::byte-to-hex (char-code #\0)) 0)
  (is eql (qp::byte-to-hex (char-code #\1)) 1)
  (is eql (qp::byte-to-hex (char-code #\2)) 2)
  (is eql (qp::byte-to-hex (char-code #\3)) 3)
  (is eql (qp::byte-to-hex (char-code #\4)) 4)
  (is eql (qp::byte-to-hex (char-code #\5)) 5)
  (is eql (qp::byte-to-hex (char-code #\6)) 6)
  (is eql (qp::byte-to-hex (char-code #\7)) 7)
  (is eql (qp::byte-to-hex (char-code #\8)) 8)
  (is eql (qp::byte-to-hex (char-code #\9)) 9))


(define-test hex-number
  :parent hex-stuff)

(define-test "hex-number: well-defined range"
  :parent hex-number
  (loop for i from 0 below 100
        for a = (random 127)
        for b = (random 127)
        for response = (qp::hex-number a b)
        do
        (true (or (null response)
                  (and (>= response 0)
                       (< response 256)))
                "~@{~@?~}"
                "hex-number at (~A,~A) was ~A" a b response)))

(define-test "hex-number: examples"
  :parent hex-number
  (is eql (qp::hex-number (char-code #\0) (char-code #\a)) 10)
  (is eql (qp::hex-number (char-code #\0) (char-code #\0)) 0)
  (is eql (qp::hex-number (char-code #\F) (char-code #\f)) 255)
  (is eql (qp::hex-number (char-code #\a) (char-code #\0))
      (qp::hex-number (char-code #\A) (char-code #\0)))
  (is eql (qp::hex-number (char-code #\7) (char-code #\f)) 127))

(define-test io)

(define-test extract-char
  :parent io)

(define-test "extract char: typical examples"
  :parent extract-byte
  (with-input-from-string (strstrm  "bc=0a123")
    (is eql (qp::extract-char strstrm) (code-char #\a))
    (is eql (qp::extract-char strstrm) (code-char #\b))
    (is eql (qp::extract-char strstrm) (code-char #\c))
    (is eql (qp::extract-char strstrm) (code-char #\Newline))
    (is eql (qp::extract-char strstrm) (code-char #\1))
    (is eql (qp::extract-char strstrm) (code-char #\2))
    (is eql (qp::extract-char strstrm) (code-char #\3))))


(test *)
