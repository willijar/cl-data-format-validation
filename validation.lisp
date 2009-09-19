;; API definition and implementation for numerous data types.
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
(in-package :data-format-validation)

(define-condition invalid-input(condition)
  ((value :reader value
          :initarg :value
          :documentation "The value input"
          :initform nil)
   (reason :reader reason
           :initarg :reason
           :documentation "Textual description of reason value is invalid."
           :initform nil))
  (:report (lambda (condition stream)
             (format stream "Invalid input: ~S ~@[[Reason: ~A]~]"
                     (value condition)
                     (reason condition)))))

(defmacro invalid-input (value &rest reason)
  "Generate an invalid-input error for given value using reason"
  `(error 'invalid-input
	  :value ,value
	  :reason (format nil ,@reason)))

(defgeneric format-output(specification value &key &allow-other-keys)
  (:documentation "Return a string representation of value formatted
according to a specification. If specification is a list the first
element specifies the actual validation method and the rest of the
list are passed as keyword arguments to the specific method e.g.

  (format-output '(date :fmt :rfc2822) (get-universal-time))

>\"Mon, 10 Jul 2006 15:43:45 +00\"
"))

(defgeneric parse-input(specification input &key &allow-other-keys)
  (:documentation "Validate and parse user input according to
specification, returning the validated object. Throws an invalid-input
condition if input is invalid.  If specification is a list the first
element specifies the actual validation method and the rest of the
list are passed as keyword arguments to the specific method e.g.

   (parse-input '(integer :min 0) input)

will return the integer value from strin if it is >0, or signal and
invalid-input error if not.

   (parse-input '(member :type integer :set (1 5 7)) input)

will return it only if it has a value in the set.

The use-value restart may be used to provide substitute value if the
input is invalid.")
  (:method :around (specification input &rest args)
    (restart-case
      (call-next-method)
    (use-value(result)
      :report (lambda(stream)
                (format stream "Specify an ~A input to use this time"
                        specification))
      :interactive (lambda()
                     (format t "Enter new value and press return: ")
                     (apply #'parse-input `(specification ,(read-line) ,@args)))
      result))))

(defun is-nil-string(string)
  (or (= 0 (length string))
      (string-equal string "NIL")
      (every #'(lambda(c) (eql c #\space)) string)))

(defparameter +ws+ '(#\space #\tab #\newline #\return)
  "Bag of white space delimiter characters")

(defun split-string (string &key count
                     (delimiter +ws+)
                     remove-empty-subseqs)
  "Split `string' along whitespace as defined by the sequence `ws'.
Whitespace which causes a split is elided from the result.  The whole
string will be split, unless `max' is provided, in which case the
string will be split into `max' tokens at most, the last one
containing the whole rest of the given `string', if any."
  (declare (optimize speed) (type string string) (type (or fixnum null) count)
           (type boolean remove-empty-subseqs))
    (nreverse
     (let ((list nil) (start 0) (words 0) end
           (is-ws (etypecase delimiter
                    (character #'(lambda(c) (char= c delimiter)))
                    (list #'(lambda(c) (member c delimiter :test #'char=)))
                    (function delimiter)
                    (simple-array
                     #'(lambda(c) (find c (the (simple-array character (*))
                                            delimiter) :test #'char=))))))
       (declare (type fixnum start words) (type list list))
       (loop
        (when  (and count (>= words (1- count)))
          (return
            (let ((last (subseq string start)))
              (declare (type string last))
              (if (and remove-empty-subseqs (= (length last) 0))
                  list
                  (cons (subseq string start) list)))))
        (setf end (position-if is-ws string :start start))
        (unless (and remove-empty-subseqs  (= start (or end (length string))))
          (push (subseq string start end) list))
        (incf words)
        (unless end (return list))
        (setf start (1+ end))))))

(defun join-strings(strings  &optional (separator #\space))
  "Return a new string by joining together the STRINGS,
separating each string with a SEPARATOR character or string"
  (with-output-to-string(os)
    (let ((firstp t))
      (map 'nil
           #'(lambda(string)
               (if firstp
                   (setf firstp nil)
                   (if (characterp separator)
                       (write-char separator os)
                       (write-sequence separator os)))
               (write-string string os))
           strings))))

(defmethod parse-input((spec list) input &rest rest)
  "Dispatch a list spec to appropriate method"
  (declare (ignore rest))
  (apply #'parse-input (nconc (list (car spec) input) (cdr spec))))

(defmethod parse-input((spec (eql nil)) input &key &allow-other-keys)
  "No validation - just return string"
  input)

(defmethod parse-input((spec function) input
                       &key &allow-other-keys)
  "Function - call it with data"
  (funcall spec input))

(defmethod parse-input((spec (eql 'boolean)) input
                       &key &allow-other-keys)
  (setf input (string-left-trim " " (string-right-trim " " input)))
  (cond
    ((member input '("YES" "Y" "TRUE" "T" "1") :test #'equalp)
     t)
    ((member input '("NO" "N" "FALSE" "NIL" "F" "0") :test #'equalp)
     nil)
    (t (invalid-input input "Boolean Value must be TRUE or FALSE"))))

(defmethod format-output((spec (eql 'boolean)) input
                         &key &allow-other-keys)
  (if input "TRUE" "FALSE"))

(defmethod parse-input((spec (eql 'integer)) (input string)
                       &key min max nil-allowed (radix 10))
  (unless (and nil-allowed (is-nil-string input))
    (handler-case
        (let ((v (parse-integer input :radix radix)))
          (when (and max (> v max))
            (invalid-input input
                           "The integer must be less than or equal to ~D" max))
          (when (and min (< v min))
            (invalid-input input
                           "The integer must be more than or equal to ~D" min))
          v)
      (error ()
        (invalid-input
         input
         "Character sequence must form a valname integer, specified as an
optional sign (+ or -) followed by a a non-empty sequence of digits")))))

(defmethod parse-input((spec (eql 'number)) (input string)
                       &key min max nil-allowed format (radix 10))
  "Real, integer or rational numbers"
  (declare (ignore format))
  (unless (and nil-allowed (is-nil-string input))
    (handler-case
        (let ((v (parse-number input :radix radix)))
          (when (and max (> v max))
            (invalid-input input
                           "The number must be less than or equal to ~D" max))
          (when (and min (< v min))
            (invalid-input input
                           "The number must be more than or equal to ~D" min))
          v)
      (error ()
        (invalid-input
         input "The character sequence must form a valname number")))))

(defmethod parse-input((spec (eql 'string)) s
                       &key
                       (strip-return nil)
                       nil-allowed  word-count max-word-count
                       (min-length 0) max-length)
  (when (not s)
    (if nil-allowed
        (return-from parse-input nil)
        (setf s "")))
  (when strip-return
      (setf s
          (with-output-to-string(os)
            (loop :for c :across s
                  :when (not (eql c #\return))
                  :do (write-char c os)))))
  (let* ((s (string-trim +ws+ s)))
    (unless (and nil-allowed (is-nil-string s))
      (when (< (length s) min-length)
        (invalid-input s "The value must be at least ~D characters long."
                       min-length))
      (when (and max-length (> (length s) max-length))
        (invalid-input s "The value must be at most ~D characters long."
                       max-length))
      (when (or word-count max-word-count)
        (let((no-words (length  (split-string s :remove-empty-subseqs t))))
          (when (and word-count (< no-words word-count))
            (invalid-input s "You must provide a value of at least ~D words."
                           word-count))
          (when (and max-word-count (> no-words max-word-count))
            (invalid-input s "You must provide a value of at most ~D words."
                           word-count))))
      s)))

(defmethod parse-input((spec (eql 'symbol)) (input string)
                       &key nil-allowed (package :keyword) (convert #'identity))
  (unless (and nil-allowed (is-nil-string input))
    (intern (funcall convert input) package)))

(defmethod parse-input((spec (eql 'member))  input
                       &key type set (test #'equal) key)
  (let ((value (parse-input type input)))
    (unless (member value set :test test :key key)
      (invalid-input input "The input must be one of ~A" set))
    value))

(defmethod parse-input ((spec (eql 'pathname)) (input string)
                        &key must-exist wild-allowed nil-allowed)
  (unless (and nil-allowed (is-nil-string input))
  (let ((pathname (pathname input)))
    (if wild-allowed
        pathname
        (if (wild-pathname-p pathname)
            (invalid-input input "Pathname ~S is wild." pathname)
            (if must-exist
                (if (probe-file pathname)
                    pathname
                    (invalid-input input "File at ~S does not exist"
                                   pathname))
                pathname))))))

(defmethod parse-input ((spec (eql 'pathnames)) (input string)
                        &key must-exist wild-allowed nil-allowed)
  (unless (and nil-allowed (is-nil-string input))
    (mapcar
     #'(lambda(p)
         (parse-input 'pathname p
                      :must-exist must-exist
                      :wild-allowed wild-allowed))
     (split-string input :delimiter #\:))))

(defmethod parse-input((spec (eql 'filename)) value
                       &key (if-invalid :error) (replacement "-"))
  "Return a safe filename from a string path value.
May return an error or replace invalid characters."
  (multiple-value-bind(start end reg-start reg-end)
      (ecase if-invalid
        (:error (cl-ppcre:scan "^([^\\/?:*]+)$" value))
        (:replace (cl-ppcre:scan "([^\\/]+)$" value)))
    (declare (ignore end))
    (unless (and start (> (aref reg-end 0) (aref reg-start 0)))
      (invalid-input
       value
       "Contains characters forbidden for a filename."))
    (let ((fname (subseq value (aref reg-start 0) (aref reg-end 0))))
      (if (eql if-invalid :error)
          fname
          (cl-ppcre::regex-replace-all  "[\/?:*]" fname replacement)))))

(defmethod parse-input ((spec (eql 'list)) input
                        &key (separator ", ") type min-length max-length)
  "Validates that a list of given of between min-length and max-length
in length. Each member of list is validated against type"
  (unless (listp input)
    (setf input
          (split-string
                 input
                 :count max-length :delimiter separator :remove-empty-subseqs t)))
  (let ((value
         (if (listp type)
             (mapcar #'(lambda(type v) (parse-input type v))
                     type input)
             (mapcar #'(lambda(v) (parse-input type v)) input ))))
    (cond ((and min-length (< (length value) min-length))
           (invalid-input input
                          "The input must be at least ~D long" min-length))
          ((and max-length (> (length value) max-length))
           (invalid-input input
                          "The input must be less than ~D long" max-length))
          (value))))

(defmethod parse-input((spec (eql 'date)) (input string)
                       &key nil-allowed (zone 0))
  (if (is-nil-string input)
      (unless nil-allowed
        (invalid-input input "The Date/Time field cannot be empty."))
      (handler-case
          (parse-time input :error-on-mismatch t :default-zone zone)
        (error ()
          (invalid-input input "Not a recognized date/time format.
Try the ISO format YYYY/MM/DD HH:MM:SS")))))

(defparameter +month-names+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "The names of the months.")

(defparameter +week-days+
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  "The names of the days of the week.")

(defparameter +time-zones+
  '((5 "EDT" . "EST") (6 "CDT" . "CST") (7 "MDT" . "MST") (8 "PDT" . "PST")
    (0 "BST" . "GMT") (-2 "MET DST" . "MET"))
  "The string representations of the time zones.")

(defun date(os utime &optional colon-p at-p
            (precision 6) (timezone nil))
  "Formatter which formats a universal time for output as a date and time

Modifiers:

- os: an output stream designator
- arg: a universal time
- colon-p: a generalised boolean (default false).
             If true use month and day names in date
- at-p: a generalised boolean (default false) - if true print in yyyy-mm-dd
          (sortable) format rather than dd-mm-yyy
- precision: what precision to print it to. 6 is to the second,
             7 includes timezone, a negative number counts backward.
- timezone: an integer (default nil).
            If nil no timezone used and time is in current timezone
              adjusted for daylight saving time.

Result:

nil

Examples:

 (format nil \"~/date/\" (get-universal-time)) => \"19-03-2009 08:30\""
   (multiple-value-bind (se mi ho da mo ye dw dst tz)
       (decode-universal-time (round utime) timezone)
     (declare (ignore dst))
     (let ((month-name (aref +month-names+ (1- mo)))
           (week-day-name (aref +week-days+ dw)))
       (when (> precision 0)
         (cond
           ((and at-p colon-p)
            (format os "~4d ~a, ~a ~2,'0d" ye week-day-name month-name da))
           (colon-p
            (format os "~A, ~2,'0d ~a ~4d" week-day-name da month-name ye))
           (at-p (format os "~4d-~2,'0d-~2,'0d" ye mo da))
           (t (format os "~2,'0d-~2,'0d-~4d" da mo ye)))
         (when (> precision 3) (write-char #\space os)))
       (when (or (> precision 3) (< precision 0))
         (format os "~2,'0d:~2,'0d" ho mi)
         (when (or (> precision 5) (< precision -2)) (format os ":~2,'0d" se)))
       (when (= precision 7)
         (format os " ~3@d" tz)))))

(defmethod parse-input((spec (eql 'read)) (value string) &key (multiplep nil)
                       (type 't) (package *package*))
  "Parse input using read. If multiple is true will read until
finished, returning a list of values. If type is set, then returned
value(s) must be of this type"
  (let ((*package* (find-package package)))
    (with-input-from-string(is value)
      (flet((do-read()
              (let ((v
                   (handler-case
                       (read is)
                     (end-of-file(e) (error e))
                     (error(e)
                       (invalid-input
                        value
                        (with-output-to-string(os)
                          (write e :stream os :readably nil :escape nil)))))))
              (unless (typep v type)
                (invalid-input value "~S is not of type ~S" v type))
              v)))
      (if multiplep
          (let ((results nil))
            (handler-case
                (loop (push (do-read) results))
              (end-of-file() (nreverse results))))
          (do-read))))))

(defmethod parse-input((spec (eql 'separated)) (value sequence)
                       &key (separator ",") type)
  (let ((results nil))
    (do*((start 0 (+ (length separator) end))
         (end (search separator value) (search separator value :start2 start)))
        ((not end)
         (push (subseq value start) results))
      (push (subseq value start end) results))
    (nreverse (mapcar #'(lambda(r) (parse-input type r)) results))))

(defmethod format-output((spec (eql 'separated)) (value list)
                         &key (separator ", ") type)
  (with-output-to-string(os)
    (dolist(v value)
      (unless (eql v (car value))
        (write-string separator os))
      (write-string (format-output type v) os))))

(defmethod format-output((spec list) output &rest rest)
  "Dispatch a list spec to appropriate method"
  (if spec
      (apply #'format-output
             (append (list (first spec) output)
                     (rest spec) rest) )
      (call-next-method)))

(defmethod format-output(spec output &key &allow-other-keys)
  "No validation - just output value"
  (declare (ignore spec))
  (if output
      (write-to-string output :readably nil :escape nil)
      ""))

(defmethod format-output((spec string) output  &key &allow-other-keys)
  (format nil spec output))

(defmethod format-output((spec function) output   &key &allow-other-keys)
  (funcall spec output))

(defmethod format-output((spec (eql nil)) output &key &allow-other-keys)
  "No validation - just output value"
  (declare (ignore spec))
  (if output
      (write-to-string output :readably nil :escape nil)
      ""))

(defun format-time(out  utime
                   &key (fmt :rfc2822) (timezone nil))
  "Formats a universal time for output.

OUT controls where the result will go.  If OUT is T, then the output
is sent to the standard output stream.  If it is NIL, then the output
is returned in a string as the value of the call.  Otherwise, OUT must
be a stream to which the output will be sent.

UTIME is the universal time to be output (default is current time)
TZ is the time zone default will give local time.

FMT is a keyword symbol specifying which output format is used as follows
:RFC2822    - output as per RFC2822 for internet messages
:SHORT     - output in a shorter format (same as :ISO)
:TIME-ONLY - outputs time as hh:mm:ss
:DATE-ONLY - outputs date as dd-mm-yyyy
:ISO       - output as per ISO 8602"
  (declare (number utime) (symbol fmt))
  (multiple-value-bind (se mi ho da mo ye dw dst tz)
      (decode-universal-time (round utime) timezone)
    (declare (fixnum se mi ho da mo ye dw tz) (ignore dst))
    (let ((month-name (aref +month-names+ (1- mo)))
          (week-day-name (aref +week-days+ dw)))
      (ecase fmt
        (:iso
         (format
          out
          "~4d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d~:[~; ~:[+~;-~]~2,'0d~]"
          ye mo da ho mi se timezone (< 0 0) tz))
        (:short
         (format out "~4d-~2,'0d-~2,'0d ~2,'0d:~2,'0d"
                 ye mo da ho mi))
        (:rfc2822
         (format
          out
          "~A, ~2,'0d ~a ~4d ~2,'0d:~2,'0d:~2,'0d~:[~; ~:[+~;-~]~2,'0d~]"
          week-day-name da month-name ye ho mi se timezone (< tz 0) tz))
        (:http
         (format out "~A, ~2,'0d ~a ~4d ~2,'0d:~2,'0d:~2,'0d GMT"
                 week-day-name da month-name ye ho mi se))
        (:notmz
         (format out
                 "~4d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
                 ye mo da ho mi se))
        (:date-only
         (format out "~2,'0d-~a-~4d" da month-name ye))
        (:time-only
         (format out "~2,'0d:~2,'0d:~2,'0d" ho mi se))))))

(defmethod format-output((spec (eql 'date)) output &key (fmt :iso) (zone nil)
                         (if-nil nil)
                         &allow-other-keys)
  (if output
    (if (numberp output)
        (format-time nil output :fmt fmt :timezone zone)
        output)
    if-nil))

(defmethod format-output((spec (eql 'number)) output &key format
                         &allow-other-keys)
  (if format (format nil format output) (call-next-method)))

(defmethod format-output((spec (eql 'integer)) output &key format
                         &allow-other-keys)
  (if format (format nil format output) (call-next-method)))

(defmethod format-output((spec (eql 'read)) output &key (multiplep nil))
  "Parse input using read. If multiple is true will read until
finsihed, returning a list of values."
  (with-output-to-string(os)
    (if (and multiplep (listp output))
        (dolist(item output)
          (unless (eql item (car output))
            (write-char #\space os))
          (write item :stream os :readably t))
        (write output :stream os :readably t))))

(defmethod format-output((spec (eql 'list)) output &key type (separator ", "))
  (join-strings
   (if (listp type)
       (mapcar #'(lambda(type v) (format-output type v)) type output)
       (mapcar #'(lambda(v) (format-output type v)) output))
   separator))

(defmethod format-output((spec (eql 'member)) output &key type)
  (format-output type output))

(defmethod parse-input((spec (eql 'time-period)) (value string)
                       &key &allow-other-keys)
  "A time period in hours, minutes and (optionally) seconds"
  (let* ((values  (split-string value :delimiter #\: :count 3))
         (hours (parse-input 'integer (first values) :min 0))
         (minutes (parse-input 'integer (second values) :min 0 :max 59))
         (seconds (if (third values)
                      (parse-input 'integer (third values) :min 0 :max 59)
                      0)))
    (+ (* 3600 hours) (* 60 minutes) seconds)))

(defmethod format-output((spec (eql 'time-period)) (value number)
                        &key &allow-other-keys)
  (multiple-value-bind(hours value) (floor value 3600.0)
    (multiple-value-bind(minutes seconds) (floor value 60.0)
      (format nil "~2,'0D:~2,'0D:~2,'0D" hours minutes  (round seconds)))))

(define-condition unknown-option(condition)
  ((option :reader option :initarg :option))
  (:report (lambda (condition stream)
             (format stream "Unknown option ~A" (option condition)))))

(defun parse-options(spec options-list &optional allow-other-options)
  "Parse an option list (alist of names and strings to be parsed)
against a specification. The specification is a list of entries each
of which lists the name, and optionally type and default values. The
output is an alist of variable names and parsed values. Options in
options-list not in spec are not returned and will signal an error
unless allow-other-options is true"
  (unless allow-other-options
    (dolist(option options-list)
          (unless (find (car option) spec
                        :test #'string-equal
                        :key (lambda(s) (if (listp s) (first s) s)))
            (cerror "Ignore Option" 'unknown-option :option option))))
  (mapcar #'(lambda(s)
              (multiple-value-bind(name type default)
                  (if (listp s) (values-list s) s)
                (let ((option
                       (find name options-list
                             :key #'car :test #'string-equal)))
                  (list name
                        (if option
                            (if type
                                (restart-case
                                    (parse-input type (cdr option))
                                  (use-default()
                                    :report
                                    (lambda(s)
                                      (format
                                       s "Use default value of ~S" default))
                                    default)
                                  (use-value(v)
                                    :report "Use default value"
                                    v))
                                (or (cdr option) default))
                            default)))))
          spec))

(define-condition too-many-arguments(invalid-input)())

(defun parse-arguments(spec argument-string &optional allow-spaces)
  "Parse a string of whitespace delimited arguments according to spec.
The specification is a list of entries each
of which lists the name, and optionally type and default values. The
output is an alist of variable names and parsed values.
If allow-spaces is true, last element can contain spaces"
  (let ((arguments (split-string argument-string
                                 :count (length spec)
                                 :remove-empty-subseqs t)))
    (when (not allow-spaces)
      (let* ((a (first (last arguments)))
             (p (position #\space a)))
        (when p
            (restart-case
                (error 'too-many-arguments
                       :value a
                       :reason "Too many arguments in argument list")
              (ignore-extra-arguments()
                :report "Ignore extra arguments"
                (setf (first (last arguments)) (subseq a 0 p)))))))
    (mapcar
     #'(lambda(s)
         (multiple-value-bind(name type default)
             (if (listp s) (values-list s) s)
           (let ((a (or (pop arguments) default)))
             (list
              name
              (restart-case
                  (parse-input type a)
                (use-default()
                  :report
                  (lambda(s) (format s "Use default value of ~S" default))
                  default))
              a))))
     spec)))

(defparameter +engineering-units+ "YZEPTGMk munfazy")
(defun eng(os arg &optional colon-p at-p
                 (d 2) (padchar #\space) (exponentchar #\E))
  "Formatter which outputs its numerical argument `arg` in engineering format
to stream `os`.
It takes arguments d,padchar,exponentchar where
d is the number of decimal places to display after the decimal point
padchar is the character to pad the start of the number
exponentchar is the character to use to display between radix and exponent
It also takes the : modifier which will cause it to output the exponent
as an SI units prefix rather than a number.

Arguments:

- `os`: an output stream designator
- `arg`: a number
- `colon-p`: a generalised boolean (default false)
- `at-p`: a generalised boolean (default false) - ignored
- `d`: an integer (default 2)
- `padchar`: a character (default `space`)
- `exponentchar`: a character (default `e`))

Result:

nil

Examples:

`(format nil \"~/eng/\" 35000) => \"35.00e+3\"`
"
  (declare (ignore at-p))
  (if (numberp arg)
      (let* (
            ;; note use u instead of \mu for 1e-6 so utf-8 not needed
             (order (if (zerop arg) 0 (floor (log arg 10) 3)))
             (scale (* 3 order))
             (radix-format
              (if (or (zerop d) (integerp arg))
                  (format nil "~~,'~CD" padchar)
                  (format nil "~~,~@[~D~],,,'~CF"
                           d  padchar)))
             (radix (/ arg (expt 10 scale))))
        (when (zerop d) (setf radix (round radix)))
        (if (and colon-p (< -1 (- 8 order) (length +engineering-units+)))
            (format os "~@? ~:[~C~;~]"
                    radix-format
                    radix
                    (zerop scale)
                    (char +engineering-units+ (- 8 order)))
            (format os "~@?~:[~C~@D~;~]"
                    radix-format
                    radix
                    (zerop scale)
                    exponentchar
                    scale)))
      (princ arg os)))

(defmethod format-output((spec (eql 'eng)) (value number)
                        &key (units t) (padchar #\space) (decimal-places 2)
                         &allow-other-keys)
  "Output in engineering style with units. If units is a string then
the output will contain that unit and the appropriate suffix. If t
only the suffix is output. If nil no units or suffix is output"
  (with-output-to-string(os)
    (eng os value  units nil decimal-places padchar)
    (when (stringp units) (write-string units os))))

(defmethod parse-input((spec (eql 'eng)) (value string)
                       &key (units t) &allow-other-keys)
  ;; we assume all of suffix non numerical characters make up units
  ;; and value before that is a number.
  (let* ((p (1+ (position-if #'digit-char-p value :from-end t)))
         (num (float (parse-number (subseq value 0 p))))
         (suffix (subseq value p)))
    (flet ((scaled-num(c)
             (let ((p (position c +engineering-units+)))
               (unless p
                    (error 'invalid-input :value value
                           :reason "Invalid enginnering unit"))
               (* num (expt  10 (* 3 (- 8 p)))))))

    (cond
      ((and
        (stringp units)
        (let ((p (search units suffix)))
          (unless p (error 'invalid-input :value value :reason "Invalid units"))
          (when (> p 0)
            (let ((c (char suffix (1- p))))
              (unless (white-space-p c)
                (scaled-num c)))))))
      ((and
        units
        (let ((p (position-if-not #'white-space-p suffix)))
          (when p (scaled-num (char suffix p))))))
      (num)))))

(defparameter +roman-numeral-map+
  '(("M"  . 1000) ("CM" . 900) ("D"  . 500) ("CD" . 400)
    ("C"  . 100) ("XC" . 90) ("L"  . 50) ("XL" . 40)
    ("X"  . 10) ("IX" . 9) ("V"  . 5) ("IV" . 4) ("I"  . 1)))

(defmethod format-output((spec (eql 'roman)) (n integer)
                         &key  &allow-other-keys)
  "convert integer to Roman numeral"
  (unless (< 0 n 4000)
    (error 'invalid-input :value n
           :reason "number out of range (must be 1..3999)"))
  (with-output-to-string(os)
    (dolist(item +roman-numeral-map+)
      (let ((numeral (car item))
            (int (cdr item)))
        (loop
         (when (< n int) (return))
         (write-sequence numeral os)
         (decf n int))))))

(defmethod parse-input((spec (eql 'roman)) (s string)
                       &key &allow-other-keys)
  "Convert roman numeral to integer"
  (let ((result 0)
        (index 0)
        (slen (length s)))
    (dolist(item +roman-numeral-map+)
      (let* ((numeral (car item))
             (int (cdr item))
             (len (length numeral)))
        (loop
         (let ((end (+ index len)))
           (unless (<= end slen) (return))
           (unless (string-equal numeral s :start2 index :end2 end)
             (return))
           (incf result int)
           (incf index len)))))
    (when (< index slen)
      (error 'invalid-input :value s :reason "Invalid Roman numeral" ))
    result))

