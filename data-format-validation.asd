;;;; -*- lisp -*-
;; System definition
;; Copyright (C) 2009 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>

;;; Copying:

;; This file is part of the DATA-FORMAT-VALIDATION Common Lisp library

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; DATA-FORMAT-VALIDATION is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(in-package :asdf)
(defsystem "data-format-validation"
    :name "Data Format Validation"
    :description "Validation and conversion between user and internal data."
    :author "Dr. John A.R. Williams"
    :version "0.1.1"
    :maintainer "Dr. John A.R. Williams"
    :licence "GPL v3"
    :depends-on (:cl-ppcre)
    :components
    ((:file "defpackage")
     (:file "validation" :depends-on ("defpackage" "parse-number" "parse-time"))
     (:file "parse-time" :depends-on ("defpackage"))
     (:file "parse-number" :depends-on ("defpackage"))))
