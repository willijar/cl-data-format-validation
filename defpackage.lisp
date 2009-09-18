;;;; Package definitions for user data parsing and formating
;;;; Copyright (C) 2004-2009 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: defpackage.lisp,v 1.16 2007/11/06 08:13:28 willijar Exp $

(in-package :cl-user)

(defpackage :user-data
  (:documentation "Generic parsing and formating")
  (:use :cl :split-sequence :cl-ppcre)
  (:export
   ;; main generic interface and condition
   #:invalid-input #:parse-input #:format-output #:use-default
   ;; new use data types -  ang and date are also formatter functions
   #:date #:filename #:eng #:time-period #:pathnames #:separated #:roman
   ;; functions for aggregates of user data and condition
   #:parse-options #:parse-arguments #:unknown-option #:too-many-arguments
   #:ignore-extra-arguments
   ;; some other more generally useful helper functions
   #:split-string #:join-strings))
