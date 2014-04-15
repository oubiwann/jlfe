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

Initially, chunks of LFE code were copied, but the latest version requires
that one manually make a single change to a function (instructions below),
and then does the rest in the jlfe code.


**Development Information**

* `Goals`_

* `Features`_


Dependencies
============

This project assumes that you have `rebar`_ installed somwhere in your
``$PATH``. Simiarly for `lfetool`_ and `kerl`_.

This project depends upon the following, which are automatically installed to
the ``deps`` directory of this project when you run ``rebar get-deps``:

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

    $ lfetool install rebar
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


Hacking LFE
===========

The next step is to update a function in LFE, the LFE in your ``./deps/lfe``
directory.

Open up the file ``./deps/lfe/src/lfe_macro.erl`` and find this function,
somewhere around line 800:

.. code:: erlang

    exp_predef([Fun|As], _, St) when is_atom(Fun) ->
        case string:tokens(atom_to_list(Fun), ":") of
            [M,F] ->
                {yes,[call,?Q(list_to_atom(M)),?Q(list_to_atom(F))|As],St};
            _ -> no                                 %This will also catch a:b:c
        end;

Next you need to change that to the following:

.. code:: erlang

    exp_predef([Fun|As]=Call, _, St) when is_atom(Fun) ->
        FirstChar = lists:nth(1, atom_to_list(Fun)),
        Tokens = string:tokens(atom_to_list(Fun), ":"),
        case [FirstChar,Tokens] of
            [46,_] ->
                {yes,[call,?Q(jlfe_java),?Q(dispatch),?Q(Call)],St};
            [_,[M,F]] ->
                {yes,[call,?Q(list_to_atom(M)),?Q(list_to_atom(F))|As],St};
            [_,_] -> no                             %This will also catch a:b:c
        end;

I *did* say hack ...

Be sure to recompile your deps:

.. code:: bash

    $ rebar compile


Running the jlfe REPL
=====================

With everything built, you're now ready to play. To run the jlfe REPL wrapper

.. code:: bash

    $ lfetool repl jlfe


jlfe Usage
==========


Syntax Additions
----------------


Constructors
,,,,,,,,,,,,


.. code:: cl

    > (.java.util.HashMap)
    ()
    >
    > (.java.lang.Double 42)
    42.0

Or you can use the short-cut for all ``java.lang.*`` classes:

.. code:: cl

    > (.Double 42)
    42.0


Static Methods
,,,,,,,,,,,,,,

.. code:: cl

    > (.java.lang.String:getName)
    java.lang.String

or

.. code:: cl

    > (.String:getName)
    java.lang.String
    >
    > (.Math:sin 0.5)
    0.479425538604203


Static Field Variables
,,,,,,,,,,,,,,,,,,,,,,

e.g., constants:

.. code:: cl

    > (.Math:PI)
    3.141592653589793
    >
    > (.java.math.BigDecimal:ROUND_CEILING)
    2


Nested Classes
,,,,,,,,,,,,,,

.. code:: cl

    > (java.util.AbstractMap$SimpleEntry:new '"a" '"b")
    #B()
    > (.java.util.AbstractMap$SimpleEntry:new "a" "b")
    #B()


Utility Functions
-----------------

Some Java types from Erjang don't render anything useful when evaluated:

.. code:: cl

    > (set bool (.Boolean true))
    #B()
    > (set flt (.Float 42))
    #B()
    > (set bigdec (java.math.BigDecimal:new 42))
    #B()


The ``value-of`` function let's us treat Java objects as distinct values
while still keeping the object around, should we want to call any methods on
it, etc.:

.. code:: cl

    > (jlfe_types:value-of bool)
    true
    > (jlfe_types:value-of flt)
    42.0
    > (jlfe_types:value-of bigdec)
    42.0

Types that don't need special treatment are passed through, as-is:

.. code:: cl

    > (jlfe_types:value-of (.Integer 42))
    42


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
.. _Goals: doc/goals.rst
.. _Features: doc/features.rst
