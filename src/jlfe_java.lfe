(defmodule jlfe_java
  (export all))

(defun call
  (((cons mod (cons func (cons args _))))
    (lfe_io:format
      '"Got mod: ~p~nGot func: ~p~nGot args:~p~n"
      (list mod func args))
    (java:call mod func '() '())))
