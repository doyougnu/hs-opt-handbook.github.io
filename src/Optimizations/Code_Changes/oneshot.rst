.. _OneShot Monad Chapter:

.. |oneshot| replace:: `oneShot <https://hackage.haskell.org/package/base-4.21.0.0/docs/GHC-Base.html#v:oneShot>`__

`OneShotting and The OneShot Monad Trick`
=========================================

What is the OneShot Monad Trick
-------------------------------

The oneshot monad trick is manual code transformation that reduces
:ref:`excessive closure allocation <canonical-closure-alloc>` by using the magic
|oneshot| function in ``GHC.Exts``, thereby improving performance. Is this
chapter, we'll go through the ``oneShot`` monad trick, describe how it works,
and when to emplow it.

The Trick
---------

First we'll begin with the trick itself. The trick uses pattern synonyms
combined with the ``oneShot`` function to inline ``oneShot`` into the definition
of a monad. For example, consider this state monad implementation used
throughout GHC:

.. code-block:: haskell

   -- | A state monad which is strict in the state `s`, but lazy in the value `a`.
   --
   -- See Note [Strict State monad] for the particular notion of strictness and
   -- implementation details.
   newtype State s a = State' { runState' :: s -> (# a, s #) }

   pattern State :: (s -> (# a, s #)) -> State s a

   -- This pattern synonym makes the monad eta-expand,
   -- which as a very beneficial effect on compiler performance
   -- See #18202.
   -- See Note [The one-shot state monad trick] in GHC.Utils.Monad
   -- It also implements the particular notion of strictness of this monad;
   -- see Note [Strict State monad].
   pattern State m <- State' m
     where
       State m = State' (oneShot $ \s -> forceState (m s))

   -- | Forces the state component of the unboxed representation pair of 'State'.
   -- See Note [Strict State monad]. This is The Place doing the forcing!
   forceState :: (# a, s #) -> (# a, s #)
   forceState (# a, !s #) = (# a, s #)

.. todo::
   show the trick for a more complex monad such as RunAsm

.. todo::
   show the trick for the simplest monad, I'll have to adapt the state monad an ghc.util.monad

.. todo::
   show the trick for a real world monad, use ``IOEnv`` in GHC



How it works
------------

When to use it
--------------

References
----------

.. [#] See the `one-shot state monad trick <https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Utils/Monad.hs?ref_type=heads#L259>`__ note.

.. [#] See the `oneShot magic <https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Types/Id/Make.hs?ref_type=heads#L2389>`__ note.

.. [#] See the `oneShot <https://gitlab.haskell.org/ghc/ghc/-/wikis/one-shot>`__ GHC wiki page.

.. [#] See Joachim Breitner's `case study <https://www.joachim-breitner.de/blog/763-Faster_Winter_5__Eta-Expanding_ReaderT>`__ on speeding up a webassembly interpretor. We thank him for his labor.
