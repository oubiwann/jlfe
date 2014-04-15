(defmodule jlfe-java
  (export all)
  (import
    (from lists
      (map 2)
      (nth 2)
      (nthtail 2))
    (from lfe-utils
      (capitalized? 1)
      (atom-cat 2))
    (from string
      (tokens 2))
    ))

(defun get-java-name(name)
  (if
    (lfe-utils:capitalized?
      (atom_to_list name)) (lfe-utils:atom-cat 'java.lang. name)
    name))

(defun call-old
  (((cons mod (cons func args)))
    (lfe_io:format
       '"Got mod: ~p~nGot func: ~p~nGot args:~p~n"
       (list mod func args))
    (let ((mod (get-java-name mod)))
      (cond
        ;; this condition looks for things like (.String:getName) and converts
        ;; to (.java.lang.String:getName)
        ((capitalized? (atom_to_list mod))
          (let ((args (++ (list mod func '()) args)))
            (lfe_io:format '"New args: ~p~n" (list args))
            (eval `(call ,@args))))
        ('true
          (eval `(call ,@args)))))))

(defun get-names
  "If the passed name doesn't have a ':' in it, there's no method/member
  defined, so assume the 'new' method.

  This function accepts a list of either one or two elements. It is expected
  that the Java mod:fun string will be tokenized on ':' and the result of that
  passed to this function.
  "
  (((cons class (cons method _)))
    (list class method))
  (((cons class _))
    (list class '"new")))

(defun -parse-mod-func (mod-func)
  "Every passed mod-func will have an initial dot. If not, something has gone
  really wrong.

  Once the dot is taken care of, any one of the following could be true:
    * no ':' divider, in which case assume 'new' and add ':new'
    * a capital initial letter, in which case, prepend 'java.lang'
  "
  (let* (((list mod func) (get-names (tokens (atom_to_list mod-func) '":")))
         (name (nthtail 1 mod)))
    (cond
      ((capitalized? name)
        (list (++ '"java.lang" mod) func))
      ('true
        (list name func)))))

(defun parse-mod-func (mod-func)
  "Let another function do all the work. We're just gonna kick back and convert
  the results to atoms."
  (map #'list_to_atom/1 (-parse-mod-func mod-func)))

(defun dispatch
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

    * (call Class member)
    * (call Class member arg-1 ... arg-N)
    * (call instannce member)
    * (call instannce member arg-1 ... arg-N)

  With 'java:get_static':

    * (java:get_static Class member)

  We can unify these with one syntax:

    * (.Class) -> constructor
    * (.Class arg-1 ... arg-N) -> constructor
    * (.Class:new) -> constructor
    * (.Class:new arg-1 ... arg-N) -> constructor
    * (.Class:member) -> use try/catch to distinguish between static method
                         calls and static field variable access
    * (.Class:member arg-1 ... arg-N)
    * (.instance:member)
    * (.instance:member arg-1 ... arg-N)

  As such, this function needs to do the following (in order):

    1. Call a function that:
      a. Checks to see if there is a mod:func; if not, assume mod:new.
      b. Checks to see if the class/instance begins with a capital letter;
         if so, prepend 'java.lang. to it.
    2. Attempt to (call ...) with the passed args inside a try form.
    3. If this fails, try to call (java:get_static ...).
    4. If that fails, re-throw the first error.
    5. Call a method on an instance that's a saved variable.
  "
  (((cons mod-func args))
    (let* (((list mod func) (parse-mod-func mod-func))
           (all (++ (list mod func) args)))
      ; XXX debug
      ; (lfe_io:format '"mod: ~w~nfunc: ~w~nargs: ~w~n" (list mod func args))
      ; (lfe_io:format '"all args: ~p~n" (list all))
      ; (lfe_io:format '"all args, no eval: ~w~n" (list all))
      (java-call all)
      ))
  ((args)
    (tuple 'error
      (list (io_lib:format '"No match; got args: ~p~n" (list args))))))

(defun java-call (args)
  (try
    ;; This will work for classes, but not instances.
    (java-call-multi-arity args)
    (catch
      ((= error (tuple type value _)) (when (== value 'undef))
        ;; This will work for instances.
        ;(java-call-instance-method args error)))))
        (error error)))))

;; XXX This is one ugly hack. Counldn't get a macro to work, though, so I fell
;; back on this. Erlang and LFE do this some thing in a couple places :-/
;; PRs welcome!
(defun java-call-multi-arity (args)
  "Awwww, yeah. Who wants boiled plate for dinner?"
  ; XXX debug
  ; (io:format '"Arg count: ~p~nArgs: ~w~n" (list (length args) args))
  (case (length args)
    (2 (java-call-2-arity args))
    (3 (apply #'call/3 args))
    (4 (apply #'call/4 args))
    (5 (apply #'call/5 args))
    (6 (apply #'call/6 args))
    (7 (apply #'call/7 args))
    (8 (apply #'call/8 args))
    (9 (apply #'call/9 args))
    (10 (apply #'call/10 args))
    (11 (apply #'call/11 args))
    (12 (apply #'call/12 args))
    (13 (apply #'call/13 args))
    (14 (apply #'call/14 args))
    (15 (apply #'call/15 args))))

(defun java-call-2-arity (args)
  "This is the normal 2-arity call, wrapped with tender loving care in a catch
  that will try to make a static method call if this one fails."
  (try
    ;; The only time this is expected to fail is when a user is trying
    ;; to :
    ;;   * make calls such as (.String:getName) when (call ...) won't
    ;;     cut it, or
    ;;   * access a field variable
    (apply #'call/2 args)
    (catch
     ((= error (tuple type value _)) (when (== value 'badfun))
       ; XXX debug
       ; (io:format '"Got error type: ~p~nGot error value: ~p~n"
       ;           (list type value))
       ;; We've gotten the error we expect when a user needs to make the
       ;; other calls; let's try one:
       (java-call-static-method args error)))))

(defun java-call-static-method (args error)
  "The function for static methods. This called when the normal call/2 fails."
  (try
    (apply #'java:call/4 (++ args '(() ())))
    (catch
      (_
        ;; Looks like that didn't work; let's try the other call:
        (java-call-static-field args error)))))

(defun java-call-static-field (args error)
  "The function for static fields (e.g., constants). This is called when the
  call for static methods fails."
  (try
    (apply #'java:get_static/2 args)
    (catch
      ;; We don't actually care about this error, so let's error
      ;; the original one.
      (_ (error error)))))

(defun java-call-instance-method (args error)
  "The function for calling members on Java instances. This is called when the
  multiple arity caller fails."
  (try
    (java-call-multi-arity (++ (list (eval (nth 1 args))) (nthtail 1 args)))
    (catch
      ;; We don't actually care about this error, so let's error
      ;; the original one.
      (_ (error error)))))
