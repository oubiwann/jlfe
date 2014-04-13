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

(defun java-type? (java-object)
  (lists:member
    (java:call java-object 'getClass '() '())
    (java-types)))

(defun java-obj? (java-object)
  (lists:member
    '"java"
    (string:tokens
      (atom_to_list (call java-object 'getClass))
      '".")))

(defun check (java-object)
  ""
  (let ((type ()))
    (cond
      ('true))
    )
  )

(defun value-of (java-object)
  (case (call java-object 'getClass)
    ('java.lang.Boolean
      (list_to_atom (call java-object 'toString)))
    ('java.lang.Float
      (call java-object 'doubleValue))
    (_ java-object)))
