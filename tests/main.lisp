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

(define-test test-1
  (is t t))
