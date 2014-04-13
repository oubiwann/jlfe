(defmodule unit-jlfe_types-tests
  (export all)
  (import
    (from lfeunit-util
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))))

(include-lib "deps/lfeunit/include/lfeunit-macros.lfe")

(deftest java-types
  (is-equal 12 (length (jlfe_types:java-types))))

(deftest java-type?
  (is 'true (jlfe_types:java-type? (java.lang.Double:new))))
