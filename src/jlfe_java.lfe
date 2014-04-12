(defmodule jlfe_java
  (export all))

(defun capitalized? (string)
  "This function checks to see if the first letter of a passed string is
  capitalized.

  Capital letters range from 'A' (ASCII code 65) to 'Z' (ASCII code 90)."
  (lists:member (car string) (lists:seq 65 90)))

(defun call
  (((cons mod (cons func (cons args _))))
    ; (lfe_io:format
    ;   '"Got mod: ~p~nGot func: ~p~nGot args:~p~n"
    ;   (list mod func args))
    (cond
      ((capitalized? (atom_to_list mod))
        (java:call (lfe-utils:atom-cat 'java.lang. mod) func '() '()))
      ('true
        (java:call mod func '() '())))))
