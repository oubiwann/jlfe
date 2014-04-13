(defmodule jlfe_java
  (export all))

(defun dot-dispatch ()
  "The functionalty presented here is completely derrived from Clojure; the
  text of this docstring is copied and adapted from the Clojure docs here:
    http://clojure.org/java_interop#Java%20Interop-The%20Dot%20special%20form

  The following forms show all the uses of the dot special form:

    * (.instance-expr:member-symbol)

    * (.Classname-symbol:member-symbol)

    * (.instance-expr:method-symbol args)

    * (.Classname-symbol:method-symbol args)

  Examples of each:

    * instance:

    * Classname:

    * member:

    * method:

  The '.' special prefix for forms is the basis for access to Java. It can be
  considered a member-access operator, and/or read as 'in the scope of'.

  If the atom between the '.' and the ':' is a symbol that resolves to a class
  name, the access is considered to be to a static member of the named class.
  Otherwise it is presumed to be an instance member and the first argument is
  evaluated to produce the target object.

  If the atom after the ':' is a symbol and no args are supplied it is taken
  to be a field access - the name of the field is the name of the symbol, and
  the value of the expression is the value of the field, unless there is a no
  argument public method of the same name, in which case it resolves to a call
  to the method.

  If args are supplied, it is taken to be a method call. The first element of
  the list must be a simple symbol, and the name of the method is the name of
  the symbol. The args, if any, are evaluated from left to right, and passed
  to the matching method, which is called, and its value returned. If the
  method has a void return type, the value of the expression will be nil. Note
  that placing the method name in a list with any args is optional in the
  canonic form, but can be useful to gather args in macros built upon the
  form.

  Note that boolean return values will be turned into Booleans, chars will
  become Characters, and numeric primitives will become Numbers unless they
  are immediately consumed by a method taking a primitive.

  The member access forms given at the top of this section are preferred for
  use in all cases other than in macros."

)

(defun get-java-name(name)
  (if
    (lfe-utils:capitalized?
      (atom_to_list name)) (lfe-utils:atom-cat 'java.lang. name)
    name))

(defun call
  (((cons mod (cons func args)))
    (lfe_io:format
      '"Got mod: ~p~nGot func: ~p~nGot args:~p~n"
      (list mod func args))
    (cond
      ;; this condition looks for things like (.String:getName) and converts
      ;; to (.java.lang.String:getName)
      ((lfe-utils:capitalized? (atom_to_list mod))
        (let ((args (++ (list (lfe-utils:atom-cat 'java.lang. mod) func '()) args)))
          (lfe_io:format '"New args: ~p~n" (list args))
          (apply #'java:call/4 args)))
      ('true
        (java:call mod func '() '())))))
