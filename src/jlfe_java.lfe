(defmodule jlfe_java
  (export all))

(defun get-java-name(name)
  (if
    (lfe-utils:capitalized?
      (atom_to_list name)) (lfe-utils:atom-cat 'java.lang. name)
    name))

(defun call
  "Cases to consider:
    * dot forms with abbreviated Classnames
    * full dot forms (both classes and instances)
    * constants (static fields)

  We want to map native Erjang supported calls mapped jlfe calls in the
  following manner:

    * (java.lang.Double:new 42) -> (.Double 42) or (.java.lang.Double 42)
    * (dbl:intValue) -> (.dbl:intValue)
    * (java:get_static 'java.lang.Double 'SIZE) -> (.java.lang.Double:SIZE) or
                                                   (.Double:SIZE)
  Some examples with args:

    * (set hm (java.util.HashMap:new))
    * (call hm 'put 1 'foo) -> (.hm:put 1 'foo)
    * (call hm 'get 1) -> (.hm:get 1)

  These can all be boiled down to two approaches, with 'call' and
  'java:get_static'.

  With 'call':

    * (call class member)
    * (call class member arg-1 ... arg-N)
    * (call instannce member)
    * (call instannce member arg-1 ... arg-N)

  With 'java:get_static':

    * (java:get_static ...)

  As such, this function needs to do the following (in order):

    1. Check to see if the class/instance begins with a capital letter; if so,
       prepend 'java.lang. to it.
    2. Attempt to (call ...) with the passed args inside a try form.
    3. If this fails, try to call (java:get_static ...).
    4. If that fails, re-throw the first error.
  "
  (((cons mod (cons func args)))
    (lfe_io:format
       '"Got mod: ~p~nGot func: ~p~nGot args:~p~n"
       (list mod func args))
    (let ((mod (get-java-name mod)))
      (cond
        ;; this condition looks for things like (.String:getName) and converts
        ;; to (.java.lang.String:getName)
        ((lfe-utils:capitalized? (atom_to_list mod))
          (let ((args (++ (list mod func '()) args)))
            (lfe_io:format '"New args: ~p~n" (list args))
            (eval `(call ,@args))))
        ('true
          (eval `(call ,@args)))))))
