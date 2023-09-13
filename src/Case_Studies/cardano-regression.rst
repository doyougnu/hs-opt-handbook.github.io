.. _cardano regression case study:

..
   Local Variables
.. |c-l| replace:: `cardano-ledger <https://github.com/input-output-hk/cardano-ledger/>`__
.. |new| replace:: GHC-9.2.8
.. |old| replace:: GHC-8.10.7
.. |inline|     replace:: ``INLINE``
.. |inlineable| replace:: ``INLINEABLE``
.. |spec|       replace:: ``SPECIALIZE``


`Cardano-Ledger: Performance Regression Updating from GHC-8.10.7 to GHC-9.2.8`
==============================================================================

This chapter is a case study on a performance regression in the |c-l| code base
that IOG observed when upgrading the code base from |old| to |new|. To root
cause the performance regression this case study directly inspects the
:ref:`Core <Reading Core>` and uses the GHC :ref:`Profiler <GHC Flags>`. After
reading this chapter, one should be able to spot inefficient Core, understand
the difference and use cases for |inline|, |inlineable| and |spec| pragmas.

The rest of the chapter is structured as follows. We introduce evidence of the
performance regression. From this information we choose candidates to inspect as
leads in our investigation. TODO :math:`\ldots{}`


Evidence of a Regression
------------------------

The regression was first observed in an integration test performed by the
Cardano Benchmark team which resulted in two GHC Profiles:

One for |old|:

.. image:: /_static/cardano-regression/8107_perf.png
   :width: 800

And one for |new|:

.. image:: /_static/cardano-regression/927_perf.png
   :width: 800

First, notice the difference in ``total alloc`` at the top of the report
summaries. |old| shows total allocations of ~157GB, while |new| shows total
allocations around ~220GB; a 40% increase.

Next, observe that two :term:`CAF`'s have changed position in the summary:
``size`` from ``Cardano.Ledger.UMap`` and ``updateStakeDistribution`` from
``Cardano.Ledger.Shelley.LedgerState.IncrementalStake``. These two functions
will be our guides to understanding the regression. In the spirit of :ref:`Don't
think, look <Don't think, look>`, we'll compare the Core output between |old|
and |new|.

Understanding the Cardano.Ledger.UMap.size regression
-----------------------------------------------------

Here is the Core output on |new|:

.. code-block:: haskell

   -- RHS size: {terms: 22, types: 63, coercions: 0, joins: 0/0}
   size :: forall c k v. UView c k v -> Int
   [GblId,
    Arity=1,
    Str=<1L>,
    Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
            WorkFree=True, Expandable=True,
            Guidance=ALWAYS_IF(arity=1,unsat_ok=True,boring_ok=False)
   ...
   size
     = \ (@c_aviN)
         (@k_aviO)
         (@v_aviP)
         (ds_dAfr :: UView c_aviN k_aviO v_aviP) ->
         case ds_dAfr of wild_Xe {
           __DEFAULT ->
             Cardano.Ledger.UMap.$fFoldableUView_$cfoldl'
               @c_aviN
               @k_aviO
               @Int
               @v_aviP
               (Cardano.Ledger.UMap.size2 @v_aviP)
               Cardano.Ledger.UMap.size1
               wild_Xe;
           PtrUView co_aviQ [Dmd=A] co1_aviR [Dmd=A] ds1_dAiu ->
             case ds1_dAiu of { UMap ds2_sJNa ds3_sJNb ->
             case ds3_sJNb of {
               Data.Map.Internal.Bin dt_iAio ds4_iAip ds5_iAiq ds6_iAir
                                     ds7_iAis ->
                 ghc-prim:GHC.Types.I# dt_iAio;
               Data.Map.Internal.Tip -> Cardano.Ledger.UMap.size1
             }
             }
         }

.. note::

   I've elided the :term:`Unfolding` for ``size`` and only present the
   ``IdInfo`` for the term. Unfoldings are important to inspect and understand,
   but for our purposes the unfoldings are simply copies of the function body.
   See :ref:`Unfoldings <Reading Core>` in the Reading Core chapter. For our
   purposes, unless stated otherwise all Core will be generated with
   ``-ddump-simpl`` and no suppression flags. This is purposefully done to show
   what Core in a real project can look like.
