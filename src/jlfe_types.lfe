(defmodule jlfe_types
  (export all))

; XXX add functions for checking for types and then dispatching to extract the
; actual value

(defun java-types ()
  '(java.lang.Boolean
    java.lang.Byte
    java.lang.Character
    java.lang.Double
    java.lang.Enum
    java.lang.Float
    java.lang.Integer
    java.lang.Long
    java.lang.Number
    java.lang.Short
    java.lang.String
    java.lang.StringBuffer))

(defun java-type? (obj)
  (lists:member
    (java:call obj 'getClass '() '())
    (java-types)))

(defun java-obj? (obj)
  (lists:member
    '"java"
    (string:tokens
      (atom_to_list (java:call obj 'getClass '() '()))
      '".")))

(defun check (java-object)
  ""
  (let ((type ()))
    (cond
      ('true))
    )
  )
