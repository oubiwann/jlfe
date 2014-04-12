####
jlfe
####

*An experimental wrapper around bits of LFE to explore the possibility of
increasing the convenience of using Erjang (Java) from LFE.*


Introduction
============

This project is 1000% experimentation. To even get it running required copying
large chunks of code from LFE (code that either wasn't exported by modules or
wasn't designed to be adapted for use by third-party libraries).

This isn't a "use at own risk" warning; this is a "don't use" warning :-) If
you wish to make your own experiments on LFE, be sure to use LFE itself.


Features
========

Existing:

* ``java.erl`` - an empty module that alleviates the user from having to
  compile a ``java.beam`` every time they want to call Erjang's
  ``java:get_static/2`` or ``java:call/4`` functions.

Under development:

* An LFE REPL wrapper that provides syntax for the following:

  * ``(.instanceMember:instance args)``

  * ``(.instanceMember:Classname args)``

  * ``(.Classname:staticMethod args)``

  * ``(.Classname:staticField)``

Planned:

* Macros for short-cuts for commonly used Java libraries (such as
  ``java.lang``).

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


jlfe Usage
==========

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
