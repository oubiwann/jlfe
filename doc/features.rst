jlfe Development
================


Features
--------

Below we have classified the features as one of the following:

* **Existing** - features that have been pushed to this repo on github.

* **Under Development** - feature that are currently being worked on;
  these are incomplete and may be broken or may not have been pushed to the
  public repo.

* **Planned** - no work has been done, but we're thinking about these.

As we make progress, we will moving items from one category to another.

Watch for changes!


Existing
,,,,,,,,

*Caveat:* for those coming from Clojure, note that even though the "dot" is
part of the form (and not a separate form unto itself), it follows the ordering
of the `Clojure dot form`_: instance/class, then member/method/field. It dos
*not* follow the ordering for when the "dot" is part of the calling form.

Many of the existing features have demonstrated usage here:
`README - jlfe Usage`_

* ``java.lfe`` - an empty module that alleviates the user from having to
  compile a ``java.beam`` every time they want to call Erjang's
  ``java:get_static/2`` or ``java:call/4`` functions.

* Convenience short-cut for ``java.lang.*``: any passed Classname that begins
  with a capital letter is prepended with ``java.lang.``.

* An LFE REPL wrapper that provides syntax for the following:

  * Constructors

    * ``(.Class)`` (implicit ``:new``)

    * ``(.Class arg-1 ... arg-N)`` (implicit ``:new``)

    * ``(.Class:new)``

    * ``(.Class:new arg-1 ... arg-N)``

  * Static class methods, fields

    * ``(.Class:member)`` (static method)

    * ``(.Class:member arg-1 ... arg-N)`` (static method)

    * ``(.Class:member)`` (static field variable/constants)

  * Nested classes

    * ``(.OuterClass$InnerClass:member)``

    * ``(.OuterClass$InnerClass:member arg-1 ... arg-N)``

* Easier type conversion/coercion than what Erjang provides. (In particular,
  ``jlfe_types:value-of``.)


Under Development
,,,,,,,,,,,,,,,,,

* An LFE REPL wrapper that provides syntax for the following:

  * Instance methods

    * ``(.instance:member)``

    * ``(.instance:member arg-1 ... arg-N)``


Planned
,,,,,,,

* Macros (syntax) for accessing values of nested objects.

* Allowing expressions as instance and class arguments.


.. Links
.. -----

.. _README - jlfe Usage: https://github.com/oubiwann/jlfe/blob/master/README.rst#jlfe-usage
.. _Clojure dot form: http://clojure.org/java_interop#Java%20Interop-The%20Dot%20special%20form
