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
wish to make your own experiments on LFE, be sure to use LFE itself


Dependencies
------------

This project assumes that you have `rebar`_ installed somwhere in your
``$PATH``.

This project depends upon the following, which are installed to the ``deps``
directory of this project when you run ``make deps``:

* `LFE`_ (Lisp Flavored Erlang; needed only to compile)
* `lfeunit`_ (needed only to run the unit tests)
* `Erjang`_ (Erlang on the JVM; installable with `lfetool`_)


Installation
============

Just add it to your ``rebar.config`` deps:

.. code:: erlang

    {deps, [
        ...
        {jlfe, ".*", {git, "git@github.com:YOURNAME/jlfe.git", "master"}}
      ]}.


And then do the usual:

.. code:: bash

    $ rebar get-deps
    $ rebar compile


Usage
=====

Add content to me here!

.. Links
.. -----
.. _rebar: https://github.com/rebar/rebar
.. _LFE: https://github.com/rvirding/lfe
.. _lfeunit: https://github.com/lfe/lfeunit
.. _Erjang: https://github.com/trifork/erjang
.. _lfetool: https://github.com/lfe/lfetool/