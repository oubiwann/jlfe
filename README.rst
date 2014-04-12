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
--------

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

* Macros (syntax) for accessing values of nested objects.


Dependencies
------------

This project assumes that you have `rebar`_ installed somwhere in your
``$PATH``.

This project depends upon the following, which are installed to the ``deps``
directory of this project when you run ``make deps``:

* `LFE`_ (Lisp Flavored Erlang; needed only to compile)
* `lfeunit`_ (needed only to run the unit tests)
* `Erjang`_ (Erlang on the JVM; easily installable with `lfetool`_)
* `rlwrap`_ (``readline`` support for the Erjang shell; installable on many
  linux distros and on Mac OS X with `Homebrew`_)

Assuming you have Erlang installed with `kerl`_ and that you have `lfetool`_
installed, you can install Erjang like so:

.. code:: bash

    $ . /opt/erlang/erlang-16b/activate
    $ lfetool install erjang


Obtaining and Building
======================

 .. code:: bash

    $ git clone https://github.com/oubiwann/jlfe.git
    $ cd jlfe
    $ rebar compile


Usage
=====

With everything built, you're now ready to play. To run the jlfe REPL wrapper

 .. code:: bash

    $ PATH=`lfetool info path` \
      ERL_LIBS=`lfetool info erllibs` \
      rlwrap --command=jlfe \
             --prompt-colour=YELLOW \
             --histsize=100000 \
             --remember \
      jerl -noshell -s jlfe_shell


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
