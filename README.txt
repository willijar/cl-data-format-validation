.. -*-RST-*-
.. |VERSION| replace:: 0.1.6

========================
DATA FORMAT VALIDATION
========================

:Author:       Dr John A.R. Williams
:Contact:      J.A.R.Williams@jarw.org.uk
:date:         2010/04/08
:status:       Initial Public Release
:version:      |VERSION|
:copyright:    © 2011 J.A.R. Williams
:abstract:     DATA-FORMAT-VALIDATION is a library for Common Lisp providing a
     consistent regular interface for converting (and validating) external data
     (in the form of strings usually) into internal data types and
     for formatting internal data back into external presentable
     strings, all according to a conversion or type specification. 

.. meta::
   :keywords: Common Lisp

.. contents:: Table of Contents

.. |DFV| replace:: DATA-FORMAT-VALIDATION
.. |JARW| replace:: John A.R. Williams

Download and Installation
=========================

|DFV| together with this documentation can be downloaded from the git
repository at
`<git://github.com/willijar/cl-data-format-validation.git>` or from
<http://www.jarw.org.uk/lisp/cl-data-format-validation.tar.gz>. The
current release version is |VERSION|.

|DFV| comes with a system definition for 
`ASDF <http://www.cliki.net/asdf>`_ and is compiled and loaded in the usual
way. It depends upon `CL-PPCRE <http://weitz.de/cl-ppcre/>`_.

|DFV|  is made available under the terms of the GPL v3 license - see
the file ``LICENSE.txt`` for details.

Support
=======

For questions, bug reports, feature requests, improvements, or patches
please email <J.A.R.Williams@jarw.org.uk>.

The API
=======

generic function **parse-input** `specification value &key &allow-other-keys => object`
   Validate and parse user input according to
   specification, returning the validated object. Throws an invalid-input
   condition if input is invalid.  If specification is a list the first
   element specifies the actual validation method and the rest of the
   list are passed as keyword arguments to the specific method::

     (parse-input '(integer :min 0) input)

   will return the integer value from strin if it is >0, or signal and
   invalid-input error if not and::

     (parse-input '(member :type integer :set (1 5 7)) input)

   will return it only if it has a value in the set.

   The `use-value` restart may be used to provide substitute value if the
   input is invalid.

generic function **format-output** `specification value &key &allow-other-keys => string`
   Return a string representation of value formatted
   according to a specification. If specification is a list the first
   element specifies the actual validation method and the rest of the
   list are passed as keyword arguments to the specific method e.g.::

    (format-output '(date :fmt :rfc2822) (get-universal-time))
    >"Mon, 10 Jul 2006 15:43:45 +00"

generic function **equivalent** `specification input reference &key &allow-other-keys => boolean`
  Return true if the input and reference values can be consider
  equivalent according to the specification. The default is to test
  using **equal**.

generic function **parse-options** `spec options-list &optional allow-other-options => options`
  Parse an option list (alist of names and strings to be parsed)
  against a specification. The specification is a list of entries each
  of which lists the name, and optionally the type specification (to
  be used by **parse-input**) and the default value to be used if there
  is no entry in the options-list. The
  output is an alist of names and the parsed or default values. Options in
  `options-list` not in spec are not returned and will signal a correctable
  `unknown-option` error unless `allow-other-options` is true.

generic function **parse-arguments** `spec argument-string &optional allow-spaces => arguments`
  Parse a string of whitespace delimited arguments according to spec.
  The specification is a list of entries each
  of which lists the name, and optionally the type  specification (to
  be used by **parse-input**) and default values. The
  output is an alist of variable names and parsed values.
  If allow-spaces is true, last element can contain spaces
  (i.e. trailing spaces are not trimmed).

formatter function **eng** `os arg &optional colon-p at-p d padchar exponentchar`
  Formatter which outputs its numerical argument `arg` in engineering format
  to stream `os`.
  It takes arguments `d,padchar,exponentchar` where
  `d` is the number of decimal places to display after the decimal point
  `padchar` is the character to pad the start of the number
  `exponentchar` is the character to use to display between radix and exponent
  It also takes the : modifier which will cause it to output the exponent
  as an SI units prefix rather than a number.

  e.g. `(format nil \"~/eng/\" 35000) => \"35.00e+3\"`

formatter function **date** `os utime &optional colon-p at-p precision 6 timezone`
  Formatter which formats a universal time for output as a date and time

  Modifiers:

  - os: an output stream designator
  - arg: a universal time
  - colon-p: a generalised boolean (default false).
             If true use month and day names in date
  - at-p: a generalised boolean (default false) - if true print in yyyy-mm-dd
          (sortable) format rather than dd-mm-yyy
  - precision: what precision to print it to. 6 is to the second,
             7 includes timezone, a negative number counts backward.
  - timezone: an integer (default `*timezone*`).
            If nil no timezone used and time is in current timezone
            adjusted for daylight saving time.
        
  e.g. `(format nil \"~/date/\" (get-universal-time)) => \"19-03-2009 08:30\""`

function **join-strings** `strings &optional (separator #\space) => string`
  Return a new string by joining together the list of  `strings`,
  separating each string with a `separator` character or string

function **split-string** `string &key count delimiter remove-empty-subseqs => list`
  Split `string` along whitespace as defined by the sequence `delimiter`.
  Whitespace which causes a split is elided from the result.  The whole
  string will be split, unless `max` is provided, in which case the
  string will be split into this number of tokens at most, the last one
  containing the whole rest of the given `string`. If
  `remove-empty-subseqs` is true zero length entries are removed. This
  is similar to `split-sequence` however it only takes a string input and
  the delimiter may be a string.

Type Specifications
===================

A type specification is an S-expression composed of a symbol
specifying the particular conversion and a keyword argument list of
qualifiers. Specific methods of **parse-input** and **format-output**
are specialised on the conversion type symbol and take the remainder
of the S-expression as an argument list. Adding your own conversions
is simply a matter of providing appropriately specialised
methods. The intended semantics are that the if the output from
**format-output** is read back in using **parse-input** with thye same
type specifications then an equivalent object should result.

Many conversions take the `nil-allowed` argument which
convert an empty or all whitespace string to nil corresponding to a
null input, otherwise an empty string is considered invalid input.
Methods specialisations are provided for the following types:


**boolean** `&key`
  Converts typical user boolean values (e.g. "TRUE", "Y",  "0") into a
  boolean type. On output "TRUE" and "FALSE" are used.

**bit-vector** `&key`
  Converts between a string of 0 and 1s and a bit vector.

**date** `&key nil-allowed zone fmt` Uses the `parse-time` library of
  Jim Healy and Daniel Barlow to convert to internal universal time in
  specified timezone `zone` which to defaults to special variable
  `*timezone*` for output but to `nil` for parsing input. If `zone` is
  nil the time will be in the current timezone allowing for local
  daylight savings time - otherwise it is in the specified timezone,
  which will be written out.

  `fmt` is a keyword specifying the output format to be used as
  follows.

   A stand alone formatter of the same name is also provided.
  
  :RFC2822   - output as per RFC2822 for internet messages
  :SHORT     - output in a shorter format (same as :ISO)
  :TIME-ONLY - outputs time as hh:mm:ss
  :DATE-ONLY - outputs date as dd-mm-yyyy
  :ISO       - output as per ISO 8602 (default)

**dimensional-parameter** `&key padchar decimal-places tol`
  Converts between a string which includes units and normal scaling
  suffixes and a cons of the numerical value and the base units
  string. `padchar` and `decimal-places` are as per **eng**.

  A dimensional comparator is equivalent if the numerical values and
  the units are equivalent.
 
**eng** `&key units padchar decimal-places`
  Parse a number suffix
  with units. The standard engineering prefixes are assumed for the
  units (but with 'u' instead of 'µ'). The appropriatly scaled
  floating point value is returned and if the `units`. If `units` is a
  string then the input units suffix must match. On output the number
  will be scaled and the appropriate engineering prefix used.
  A general purpose formatter of the same name is also provided.

**filename** `&key if-invalid replacement`
  Return a safe filename from a string path value.
  May return an error or replace invalid characters with the specified
  replacement letter (default '-');

**headers** `&key stream skip-blanks-p field-specifications
  preserve-newlines-p termination-test if-no-specification`
  Parse or format internet message style headers. `parse-input` takes
  either a string or stream as the input value. 

  `field-specifications`
  is either an a-list by field name of giving the parse type
  specification to be applied recursively for that field or a function
  which returns the parse type specification and a `present-p` values
  in the usual way. `if-no-specification` specifies either a type
  specification to be used if the field is not found in
  `field-specifications`, `:error` for this case to be flagged as an
  error or `:ignore` to ignore fields without specifications.
  If defaults to `nil` i.e. value is passed through as a string
  without parsing.

  `skip-blanks-p` will allow the parser to skip leading blank lines on
  the input. `termination-test` is a test function which of one
  argument (a string - a line) which should return true if the
  argument terminates the headers - default tests for a zero length
  line. If `preserve-newlines-p` is true then continuation lines will
  keep their newline characters, otherwise the newlines and first
  continuation character are removed.

  `format-output` will write its output to `stream` if it is given,
  otherwise it will return a string containing the output headers.   

**integer** `&key min max nil-allowed radix format`
  Converts to an integer between `min` and `max` (inclusive, and if
  specified). `radix` specified the base (in the usual way). `format`
  specifies the format control string to be used for output.

**list** `&key separator type min-length max-length`
  Return a list of objects delimited by the given `separator`
  string. Each member is recursively checked the nested type
  (another type specification). If specified `min-length` and
  `max-length` specify the required length bounds. The type
  specification may be a list of type specifications applied to each
  element in turn or a single type specification applied to all
  elements (note there is an ambiguity if you specify a list of one
  symbol - in this it is taken as a conversion for the first element only).

**member** `&key type set test  key`
  Recursively uses `type` to convert string to internal object which
  is then checked for membership of the list `set` using `key` and
  `test`(default is equal allowing for string tests).

**nil** `&key`
  Return string unchanged.

**number** `&key min max nil-allowed format radix tol`
  Converts to a general number between `min` and `max` (inclusive, and if
  specified). `radix`
  specified the base (in the usual way). `format` specifies the format
  control string to be used for output. The `parse-number` library of
  Matthew Danish is used to do the conversion.

  `tol` is the tolerance to be used for **equivalence** testing - it
  can either be a multiplier applied to the reference value or a
  function of two arguments - the input and the reference value.

**pathname** `&key must-exist wild-allowed nil-allowed`
  Convert input to a pathname. If `wild-allowed` is true then the
  pathname is allowed to be wild, otherwise if `must-exist` is true
  then the pathname must correspond to an existing file (checked using
  probe-file.  

**pathnames** `&key must-exist wild-allowed nil-allowed`
  Return a list of pathnames delimited by ':', each checked as for **pathname**

**read** `&key multiplep type package`
  Uses the lisp reader with the current package set to
  `package`. `type` is a Common Lisp type against which the read
  object(s) is checked. If `multiplep` is true then read will be
  continually called until all characters are used up and the results
  are returned as a list. On output, if `multiplep` is true list of
  objects are separated by a space and written readably. 

**roman**
  Convert between roman numerals (up to 4000) and an integer

**string** `&key strip-return nil-allowed min-word-count max-word-count min-length max-length`
  Validates that the string is between `min-length` and `max-length`
  characters long (inclusive, and if specified) and the word count is
  between `min-word-count` and `max-word-count`. 
  Whitespace is trimmed from the returned string, and if
  `strip-return` is specified the RETURN characters are stripped from
  the string (useful when handling input from http forms).

**symbol** `&key nil-allowed package convert`
  Returns a symbol from the string interned into `package` (default
  is the keyword package). `conversion` is a function applied to the
  string before it is interned (default identity) which may for
  example be used to change case or map special characters.

**time-period** `&key`
  A time period in hours, minutes and (optionally) seconds is
  converted into an integer number of seconds. ':' is used as the
  delimiter between fields.

Conditions and Restarts
=======================

**invalid-format** 
  is signalled if the input doesn't meet the type specification. It has
  readers `invalid-format-value` and `invalid-format-reason`.

**use-value** 
  restart may be invoked to specify a result to be used if
  invalid-input is signalled.

**use-default**
  This restart is available for **parse-options** and
  **parse-arguments** and will result in a default specified value
  being used.


Acknowledgements
================

Matthew Danish for the parse-number library used and enclosed with
this.

Daniel Barlow and Jim Healey for the parse-time library.

