;; Package definition
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

(in-package :cl-user)

(defpackage :data-format-validation
  (:documentation "Exports API for validation and conversion of user
  data to internal types and back again.")
  (:nicknames #:dfv)
  (:use :cl :cl-ppcre)
  (:export
   ;; main generic interface and condition
   #:parse-input #:format-output #:invalid-format #:equivalent
   #:invalid-format-type #:invalid-format-value #:invalid-format-reason
   ;; new use data types -  ang and date are also formatter functions
   #:date #:filename #:eng #:time-period #:pathnames #:separated #:roman
   #:headers #:dimensional-parameter #:*timezone* #:percentage #:duration
   ;; functions for doing aggregates of user data and condition
   #:parse-options #:parse-arguments #:ignore-extra-arguments #:use-default
   ;; some other more generally useful helper library functions
   #:split-string #:join-strings))
