####
jlfe
####

*An experimental wrapper around bits of LFE*

.. image:: images/logos/DukeOfferingLFE-square-tiny.png


Introduction
============

This project is 1000% experimentation. 

Its sole purpose is to explore the possibility of slightly increasing
programmer convenience when using LFE on Erjang (Erlang on the JVM).

To even get it running required copying chunks of code from LFE (code
that either wasn't exported by modules or wasn't designed to be adapted
for use by third-party libraries).

This isn't a "use at own risk" warning; this is a "don't use" warning :-) If
you wish to make your own experiments on LFE, be sure to use LFE itself.


Features
========


**Existing**

* ``java.lfe`` - an empty module that alleviates the user from having to
  compile a ``java.beam`` every time they want to call Erjang's
  ``java:get_static/2`` or ``java:call/4`` functions.

* Convenience short-cut for ``java.lang.*``: any passed Classname that begins
  with a capital letter is prepended with ``java.lang.``.


**Under development**

*Caveat:* the following are being tested only in the REPL; absolutely no
effort has been made to ensure that they work when used and compiled in ``.lfe``
files.

*Caveat:* for those coming from Clojure, note that even though the "dot" is
part of the form (and not a separate form unto itself), it follows the ordering
of the `Clojure dot form`_: instance/class, then member/method/field. It dos
*not* follow the ordering for when the "dot" is part of the calling form.

* An LFE REPL wrapper that provides syntax for the following:

  * ``(.instance:instanceMember args)``

  * ``(.Classname:instanceMember args)``

  * ``(.Classname:staticMethod args)``

  * ``(.Classname:staticField)``


**Planned**

* Macros (syntax) for accessing values of nested objects.


Dependencies
============

This project assumes that you have `rebar`_ installed somwhere in your
``$PATH``. Simiarly for `lfetool`_ and `kerl`_.

This project depends upon the following, which are installed to the ``deps``
directory of this project when you run ``rebar get-deps``:

* `LFE`_ (Lisp Flavored Erlang; needed only to compile)
* `lfeunit`_ (needed only to run the unit tests)

Dependencies not installed automatically:

* `lfetool`_
* `kerl`_ (see below)
* `Erjang`_ (see below)
* `rlwrap`_ (``readline`` support for the Erjang shell; installable on many
  linux distros; on Mac OS X, install with `Homebrew`_)

If you don't have ``kerl`` and Erlang installed:

.. code:: bash

    $ lfetool install kerl
    $ lfetool install erlang R16B
    $ . /opt/erlang/R16B/activate

Erjang installation is similarly easy:

.. code:: bash

    $ lfetool install erjang


Obtaining and Building jlfe
===========================

Download and compile ``jlfe``:

.. code:: bash

    $ git clone https://github.com/oubiwann/jlfe.git
    $ cd jlfe
    $ rebar get-deps
    $ rebar compile


Running the jlfe REPL
=====================

With everything built, you're now ready to play. To run the jlfe REPL wrapper

.. code:: bash

    $ export RLWRAP="rlwrap \
        --command=jlfe \
        --prompt-colour=YELLOW \
        --histsize=100000 \
        --remember"
    $ PATH=`lfetool info path` \
      ERL_LIBS=`lfetool info erllibs` \
      $RLWRAP jerl -noshell -s jlfe_shell


jlfe Usage
==========

Make sure the old syntax still works:

.. code:: cl

    > (: io format '"hey there~n" '())
    hey there
    ok
    > (io:format '"hey there~n" '())
    hey there
    ok

Now try out some jlfe Java syntax:

.. code:: cl

    > (.java.lang.String:getName)
    java.lang.String

or

.. code:: cl

    > (.String:getName)
    java.lang.String


.. Links
.. -----
.. _rebar: https://github.com/rebar/rebar
.. _LFE: https://github.com/rvirding/lfe
.. _lfeunit: https://github.com/lfe/lfeunit
.. _Erjang: https://github.com/trifork/erjang
.. _lfetool: https://github.com/lfe/lfetool/
.. _kerl: https://github.com/spawngrid/kerl
.. _rlwrap: http://utopia.knoware.nl/~hlub/uck/rlwrap/#rlwrap
.. _Homebrew: http://brew.sh/
.. _Clojure dot form: http://clojure.org/java_interop#Java%20Interop-The%20Dot%20special%20form
