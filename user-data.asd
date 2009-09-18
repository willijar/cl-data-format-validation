;;;; -*- lisp -*-
;;;; $Id: jarw.asd,v 1.9 2007/01/22 12:26:08 willijar Exp willijar $

(in-package :asdf)
(defsystem "user-data"
    :name "cl-jarw"
    :description "General Purpose Library"
    :author "Dr. John A.R. Williams"
    :version "0.1"
    :maintainer "Dr. John A.R. Williams"
    :licence "GPL Style"
    :long-description "General Purpose Library of common functions and idioms"
    :depends-on (:cl-ppcre :split-sequence)
    :components
    ((:file "defpackage")
     (:file "validation" :depends-on ("defpackage" "parse-number" "parse-time"))
     (:file "parse-time" :depends-on ("defpackage"))
     (:file "parse-number" :depends-on ("defpackage"))))
