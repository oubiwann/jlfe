(defmodule unit-jlfe-types-tests
  (export all)
  (import
    (from lfeunit-util
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))))

(include-lib "deps/lfeunit/include/lfeunit-macros.lfe")

(deftest java-types
  (is-equal 12 (length (jlfe-types:java-types))))

(deftest java-type?
  (is 'true (jlfe-types:java-type? (java.lang.Double:new))))
