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
  (:import-from #:com.djhaskin.cl-i)
  (:import-from #:cl-ppcre)
  (:local-nicknames
    (#:nrdl #:com.djhaskin.nrdl)
    (#:cl-i #:com.djhaskin.cl-i)
    )
  (:export #:main))

(in-package #:com.djhaskin.qp)


;;; `CODE-CHAR` is not portable, so here we are.
;;; List of an ascii table in the form of an alist,
;;; Where the key is the ascii code and the value is the character.
(defparameter *ascii-table*
  '((32 . " ") (10 . "\n") (9 . "\t") (13 . "\r")
    (33 . "!") (34 . "\"") (35 . "#") (36 . "$")
    (37 . "%") (38 . "&") (39 . "'") (40 . "(")
    (41 . ")") (42 . "*") (43 . "+") (44 . ",")
    (45 . "-") (46 . ".") (47 . "/") (48 . "0")
    (49 . "1") (50 . "2") (51 . "3") (52 . "4")
    (53 . "5") (54 . "6") (55 . "7") (56 . "8")
    (57 . "9") (58 . ":") (59 . ";") (60 . "<")
    (61 . "=") (62 . ">") (63 . "?") (64 . "@")
    (65 . "A") (66 . "B") (67 . "C") (68 . "D")
    (69 . "E") (70 . "F") (71 . "G") (72 . "H")
    (73 . "I") (74 . "J") (75 . "K") (76 . "L")
    (77 . "M") (78 . "N") (79 . "O") (80 . "P")
    (81 . "Q") (82 . "R") (83 . "S") (84 . "T")
    (85 . "U") (86 . "V") (87 . "W") (88 . "X")
    (89 . "Y") (90 . "Z") (91 . "[") (92 . "\\")
    (93 . "]") (94 . "^") (95 . "_") (96 . "`")
    (97 . "a") (98 . "b") (99 . "c") (100 . "d")
    (101 . "e") (102 . "f") (103 . "g") (104 . "h")
    (105 . "i") (106 . "j") (107 . "k") (108 . "l")
    (109 . "m") (110 . "n") (111 . "o") (112 . "p")
    (113 . "q") (114 . "r") (115 . "s") (116 . "t")
    (117 . "u") (118 . "v") (119 . "w") (120 . "x")
    (121 . "y") (122 . "z") (123 . "{") (124 . "|")
    (125 . "}") (126 . "~")))

(declaim (inline replace-hex))
(defun replace-hex (target-string
                     start end
                     match-start match-end
                     reg-starts reg-ends)
  (declare (ignore start end match-start match-end))
  (let ((first-capture-group (subseq target-string
                                     (elt reg-starts 0)
                                     (elt reg-ends 0))))
    (cdr (assoc (parse-integer first-capture-group :radix 16)
                *ascii-table*))
    ))

(defun from (options)
  (let ((result (make-hash-table :test #'equal))
        (encoded (cl-i:ensure-option-exists :encoded options)))

    (cl-ppcre:regex-replace-all
      "=\r?\n" encoded "")
    (cl-ppcre:regex-replace-all
      "=([0-9A-Fa-f][0-9A-Fa-f])"
      encoded
      #'replace-hex)
    (setf (gethash :decoded result)
          encoded)
    (setf (gethash :status result) :successful)
    result))

(defun none (options)
  (declare (ignore options))
  (let ((result (make-hash-table :test #'equal)))
    (setf (gethash :subcommand result) "none")
    result))

(defun to (options)
  (declare (ignore options))
  (let ((result (make-hash-table :test #'equal)))
    :cc
    result))

(defparameter argv uiop:*command-line-arguments*)

(defun main (argv)
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
      (("to") . "Convert to quoted printable"))))