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

The Process
-----------


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
   but for our purposes the unfoldings will generally be copies of the function
   body. See :ref:`Unfoldings <Reading Core>` in the Reading Core chapter for
   more. Similarly, I will selectively replace the strictness analysis output
   with an ellipses because it is typically too large to format well.

   Unless stated otherwise all Core will be generated with ``-ddump-simpl`` and
   no suppression flags. This is purposefully done to show what Core in a real
   project can look like and to help train your eye to make sense of the noisy
   output.


On |old| the Core is slightly different:


.. code-block:: haskell

    size :: forall c k v. UView c k v -> Int
    [GblId,
    Arity=1,
    Caf=NoCafRefs,
    Str=<S,1*U>,
    Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
            WorkFree=True, Expandable=True, Guidance=IF_ARGS [70] 100 20}]
    size
    = \ (@ c_a7SFB)
        (@ k_a7SFC)
        (@ v_a7SFD)
        (ds_d7UZd :: UView c_a7SFB k_a7SFC v_a7SFD) ->
        case ds_d7UZd of wild_Xfk {
            __DEFAULT ->
            Cardano.Ledger.UMap.size_$cfoldl'
                @ c_a7SFB
                @ k_a7SFC
                @ Int
                @ v_a7SFD
                (Cardano.Ledger.UMap.size2 @ v_a7SFD)
                Cardano.Ledger.UMap.size1
                wild_Xfk;
            PtrUView co_a7SFF [Dmd=<L,A>] co1_a7SFG [Dmd=<L,A>] ds1_d7Vel ->
            case ds1_d7Vel of { UMap ds2_s90fe ds3_s90ff ->
            case ds3_s90ff of {
                Data.Map.Internal.Bin dt_a7UZH ds4_a7UZI ds5_a7UZJ ds6_a7UZK
                                    ds7_a7UZL ->
                ghc-prim-0.6.1:GHC.Types.I# dt_a7UZH;
                Data.Map.Internal.Tip -> Cardano.Ledger.UMap.size1
            }
            }
        }

Notice that on |new| the ``DEFAULT`` case calls
``Cardano.Ledger.UMap.$fFoldableUView_$cfoldl'`` whereas on |old| this call is
``Cardano.Ledger.UMap.size_$cfoldl'``. Let's check these functions:

|new|:

.. code-block:: haskell

   -- RHS size: {terms: 215, types: 375, coercions: 57, joins: 0/4}
   Cardano.Ledger.UMap.$fFoldableUView_$cfoldl'
     :: forall c k b a. (b -> a -> b) -> b -> UView c k a -> b
   [GblId, Arity=3, Str=<LCL(C1(L))><1L><1L>, Unf=OtherCon []]
   Cardano.Ledger.UMap.$fFoldableUView_$cfoldl'
     = \ (@c_a2svV)
         (@k_a2svW)
         (@b_a2szt)
         (@a_a2szu)
         (accum_a2plt :: b_a2szt -> a_a2szu -> b_a2szt)
         (ans0_a2plu :: b_a2szt)
         (ds_d2xJs :: UView c_a2svV k_a2svW a_a2szu) ->
         case ds_d2xJs of {
           RewDepUView co_a2szv [Dmd=A] co1_a2szw ds1_d2xTB ->
             case ds1_d2xTB of { UMap ds2_s2BfK ds3_s2BfL ->
             letrec {
               go15_s2zs0 [Occ=LoopBreaker, Dmd=SCS(C1(L))]
                 :: b_a2szt
                    -> Map (Credential 'Staking c_a2svV) (UMElem c_a2svV) -> b_a2szt
               [LclId, Arity=2, Str=<1L><1L>, Unf=OtherCon []]
               go15_s2zs0
                 = \ (z'_i2wnP :: b_a2szt)
                     (ds4_i2wnQ
                        :: Map (Credential 'Staking c_a2svV) (UMElem c_a2svV)) ->
                     case ds4_i2wnQ of {
                       Data.Map.Internal.Bin ipv_i2wnT ipv1_i2wnU ipv2_i2wnV ipv3_i2wnW
                                             ipv4_i2wnX ->
                         case go15_s2zs0 z'_i2wnP ipv3_i2wnW of z''_i2wnZ { __DEFAULT ->
                         case (umElemRDPair @c_a2svV ipv2_i2wnV)
                              `cast` ((Maybe (Sub (Sym co1_a2szw)))_R
                                      :: Maybe RDPair ~R# Maybe a_a2szu)
                         of {
                           Nothing -> go15_s2zs0 z''_i2wnZ ipv4_i2wnX;
                           Just x1_iBKi ->
                             go15_s2zs0 (accum_a2plt z''_i2wnZ x1_iBKi) ipv4_i2wnX
                             ...

|old|:

.. code-block:: haskell

   -- RHS size: {terms: 272, types: 431, coercions: 77, joins: 0/4}
   Cardano.Ledger.UMap.size_$cfoldl'
     :: forall c k b a. (b -> a -> b) -> b -> UView c k a -> b
   [GblId,
    Arity=3,
    Caf=NoCafRefs,
    Str=<L,C(C1(U))><S,1*U><S,1*U>,
    Unf=OtherCon []]
   Cardano.Ledger.UMap.size_$cfoldl'
     = \ (@ c_a7TVW)
         (@ k_a7TVX)
         (@ b_a7TZI)
         (@ a_a7TZJ)
         (accum_a7RPi :: b_a7TZI -> a_a7TZJ -> b_a7TZI)
         (ans0_a7RPj :: b_a7TZI)
         (ds_d8v9s :: UView c_a7TVW k_a7TVX a_a7TZJ) ->
         case ds_d8v9s of {
           RewDepUView co_a7TZL [Dmd=<L,A>] co1_a7TZM ds1_d8wpq ->
             case ds1_d8wpq of { UMap ds2_s90eY ds3_s90eZ ->
             letrec {
               go15_s8G6Q [Occ=LoopBreaker]
                 :: b_a7TZI
                    -> Map (Credential 'Staking c_a7TVW) (UMElem c_a7TVW) -> b_a7TZI
               [LclId, Arity=2, Str=<S,1*U><S,1*U>, Unf=OtherCon []]
               go15_s8G6Q
                 = \ (z'_a8iQB :: b_a7TZI)
                     (ds4_a8iQC
                        :: Map (Credential 'Staking c_a7TVW) (UMElem c_a7TVW)) ->
                     case ds4_a8iQC of {
                       Data.Map.Internal.Bin ipv_a8iQF ipv1_a8iQG ipv2_a8iQH ipv3_a8iQI
                                             ipv4_a8iQJ ->
                         case go15_s8G6Q z'_a8iQB ipv3_a8iQI of z''_a8iQL { __DEFAULT ->
                         case ipv2_a8iQH of {
                           __DEFAULT -> go15_s8G6Q z''_a8iQL ipv4_a8iQJ;
                           TFEEE dt_d8BOJ dt1_d8BOK -> ...


These functions are again nearly identical. Both define a function which inputs
four type variables , and three term variables, and then defines a local
function called with a recursive let. For example, on |old| we have:
``c_a7TVW``, ``k_a7TVX``, ``b_a7TZI``, and ``a_a7TZJ`` for type variables,
``accum_a7RPi``, ``ans0_a7RPj``, and ``ds_d8v9s`` for term variables, and
``go15_s8G6Q`` for the local recursive function.

From the summary comment above the function signature we can see that
``cfoldl'`` on |old| is larger (272 terms) compared to |new| (215 terms). Now
larger Core *is not always* worse than smaller Core; it depends on
specialization and inlining behavior.

In this case, the larger Core is a better performing program. On |old| we can
see that the local function ``go15_s8G6Q`` begins pattern matching on an
:term:`Algebraic Data Type` bound to ``ds4_a8iQC``, which is a ``Data.Map``.
Once the map is scrutinized ``go15_s8G6Q`` is called again with the argument
``z'_a8iQB``. The result of the first recursive call is scrutinized by a
``__DEFAULT`` branch that in turn scrutinizes ``ipv2_a8iQH``. However, on |new|
the result of the recursive call in a case expression which scrutinizes
``(umElemRDPair @c_a2svV ipv2_i2wnV)`` *and* allocates a ``Maybe``. So it seems
that the culprit is ``unElemRDPair`` is no longer being inlined and because it
is no longer being inlined the case-of-known-constructors optimization is not
firing.

This is the source of the regression for ``size``. The fix is simple, just
inline ``umElemRDPair``. In fact, there are six functions in
``Cardano.Ledger.UMap`` which would benefit from inlining. First here is the
source for ``umElemRDPair``:

.. code-block:: haskell

   -- | Extract the reward-deposit pair if it is present.
   -- We can tell that the reward is present when Txxxx has an F in the first position
   --
   -- This is equivalent to the pattern (ElemP (SJust r) _ _ _) -> Just r
   umElemRDPair :: UMElem c -> Maybe RDPair
   umElemRDPair = \case
     TFEEE r -> Just r
     TFEEF r _ -> Just r
     TFEFE r _ -> Just r
     TFEFF r _ _ -> Just r
     TFFEE r _ -> Just r
     TFFEF r _ _ -> Just r
     TFFFE r _ _ -> Just r
     TFFFF r _ _ _ -> Just r
     _ -> Nothing

We can see that this function is very simple, it takes a ``UMElem c``, pattern
matches and converts the first field to a ``Maybe``. This function benefits from
inlining because its rather large, so GHC might not conclude that it should be
inlined, but also because its simple. It just srutinizes its input with a case
expression and returns a ``Just``. Such simple functions are prime candidates
for inlining because they allow other optimizations to fire. Here is another
such function in the same module:

.. code-block:: haskell

   -- | A n-Tuple view of the `UMElem`.
   -- We can view all of the constructors as an `UMElem`.
   umElemAsTuple ::
     UMElem c ->
     (StrictMaybe RDPair, Set Ptr, StrictMaybe (KeyHash 'StakePool c), StrictMaybe (DRep c))
   umElemAsTuple = \case
     TEEEE -> (SNothing, Set.empty, SNothing, SNothing)
     TEEEF v -> (SNothing, Set.empty, SNothing, SJust v)
     TEEFE s -> (SNothing, Set.empty, SJust s, SNothing)
     TEEFF s v -> (SNothing, Set.empty, SJust s, SJust v)
     TEFEE p -> (SNothing, p, SNothing, SNothing)
     TEFEF p v -> (SNothing, p, SNothing, SJust v)
     TEFFE p s -> (SNothing, p, SJust s, SNothing)
     TEFFF p s v -> (SNothing, p, SJust s, SJust v)
     TFEEE r -> (SJust r, Set.empty, SNothing, SNothing)
     TFEEF r v -> (SJust r, Set.empty, SNothing, SJust v)
     TFEFE r s -> (SJust r, Set.empty, SJust s, SNothing)
     TFEFF r s v -> (SJust r, Set.empty, SJust s, SJust v)
     TFFEE r p -> (SJust r, p, SNothing, SNothing)
     TFFEF r p v -> (SJust r, p, SNothing, SJust v)
     TFFFE r p s -> (SJust r, p, SJust s, SNothing)
     TFFFF r p s v -> (SJust r, p, SJust s, SJust v)

We can see that this function is similar and still just as simple. Its likely
too large for GHC to conclude to inline it itself, but all the function does is
scrutinize the input and return a triple; just like ``umElemRDPair``. Thus this
function is another prime candidate for inlining.

Understanding the Cardano.Ledger.Address.updateStakeDistribution Regression
---------------------------------------------------------------------------

One of the useful features of GHC's profiler is the explicit call stack that is
reported alongside the summary. In the Cardano Ledger code base, this is
especially useful because the ledger is a large Cabal project consisting of 39
packages and hundreds of modules. From the summary output, we know that the
``updateStakeDistribution`` is in the critical path of the regression, so we can
search the remainder of the profile to observe the rest of the critical path:

.. code-block:: bash


   COST CENTRE                          MODULE                                              SRC                                                                            no.       entries      %time %alloc   %time %alloc

          ...
          updateStakeDistribution       Cardano.Ledger.Shelley.LedgerState.IncrementalStake src/Cardano/Ledger/Shelley/LedgerState/IncrementalStake.hs:(92,1)-(95,84)      15633     1            3.8   11.8     6.7   13.6
           runStateT                    Control.Monad.Trans.State.Lazy                      Control/Monad/Trans/State/Lazy.hs:161:33-41                                    15640     104000052    0.0    0.0     0.0    0.0
           addrEitherBabbageTxOutL      Cardano.Ledger.Babbage.TxOut                        src/Cardano/Ledger/Babbage/TxOut.hs:(218,1)-(227,5)                            15634     5000004      0.2    0.7     0.3    0.8
            getEitherAddrBabbageTxOut   Cardano.Ledger.Babbage.TxOut                        src/Cardano/Ledger/Babbage/TxOut.hs:(601,1)-(611,90)                           15635     5000004      0.1    0.1     0.1    0.1
             decodeAddress28            Cardano.Ledger.Alonzo.TxOut                         src/Cardano/Ledger/Alonzo/TxOut.hs:(118,1)-(129,58)                            15636     1000001      0.0    0.1     0.0    0.1
          ...


We see that ``updateStakeDistribution`` is calling ``runStateT``,
``addrEitherBabbageTxOutL``, ``getEitherAddrBabbageTxOut``, and
``decodeAddress28``. Furthermore, GHC is attributing 11.8% of allocations to
``updateStakeDistribution`` even though ``updateStakeDistribution`` is only
entered once (the ``1`` in the ``entries`` column). So it is the entry point to
the regression's critical path. Therefore, we'll inspect and compare the Core of
``addrEitherBabbageTxOutL``, ``getEitherAddrBabbageTxOut``, and
``decodeAddress28`` before developing a hypothesis.

.. note::
   This sequence of calls: ``updateStakeDistribution``, ``runStateT``,
   ``addrEitherBabbageTxOutL``, ``getEitherAddrBabbageTxOut``, and
   ``decodeAddress28`` is repeated four times in the profile, but never differs.
   I'm only showing one such sequence for brevity.

Now we'll check each function's Core to observe a difference between |old| and
|new| beginning with the top of the call stack: ``decodeAddress28``.

|old|:

.. code-block:: haskell

    -- RHS size: {terms: 96, types: 79, coercions: 50, joins: 1/2}
    decodeAddress28 [InlPrag=INLINE (sat-args=2)]
    :: forall c.
        HashAlgorithm (ADDRHASH c) =>
        Credential 'Staking c -> Addr28Extra -> Maybe (Addr c)
    [GblId,
    Arity=3,
    Caf=NoCafRefs,
    Str=<S(SLLL),1*U(1*U,A,A,A)><L,U><S,1*U(U,U,U,U)>,
    Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
            WorkFree=True, Expandable=True,
            Guidance=ALWAYS_IF(arity=3,unsat_ok=False,boring_ok=False)
            Tmpl= \ (@ c_a5pDW)
            ...

|new|:

.. code-block:: haskell

    -- RHS size: {terms: 54, types: 81, coercions: 52, joins: 0/0}
    decodeAddress28 [InlPrag=INLINE (sat-args=2)]
    :: forall c.
        HashAlgorithm (ADDRHASH c) =>
        Credential 'Staking c -> Addr28Extra -> Maybe (Addr c)
    [GblId,
    Arity=3,
    Str=<1P(1L,A,A,A)><L><1P(L,L,L,L)>,
    Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
            WorkFree=True, Expandable=True,
            Guidance=ALWAYS_IF(arity=2,unsat_ok=False,boring_ok=False)
            Tmpl= \ (@c_a2yQq)
            ...

On |new| ``decodeAddress28`` is only 54 terms and has no join points, however,
on |old| ``decodeAddress28`` is 96 terms and has join points. Let's spot check
the optimized body of the function to see the difference first hand. Note that
both versions show ``InlPrag=INLINE`` meaning that the source code has an
``INLINE`` pragma and therefore ``getEitherAddrBabbageTxOut`` will use the
unfolding of ``decodeAddress28`` (i.e., the Core on the right hand side of
``Tmpl``).

|new|:

.. code-block:: haskell

   decodeAddress28
   = \ (@c_a2yQq)
       ($dHashAlgorithm_a2yQr :: HashAlgorithm (ADDRHASH c_a2yQq))
       (eta_X1q :: Credential 'Staking c_a2yQq)
       (eta1_X1r :: Addr28Extra) ->
       case eta1_X1r of
       { Addr28Extra dt_d2DMO dt1_d2DMP dt2_d2DMQ dt3_d2DMR ->
       case GHC.Num.Natural.naturalEq#
               ((Cardano.Crypto.Hash.Class.$p1HashAlgorithm
                   @(ADDRHASH c_a2yQq) $dHashAlgorithm_a2yQr)
               `cast` (GHC.TypeNats.N:KnownNat[0] <SizeHash (ADDRHASH c_a2yQq)>_N
                       ; GHC.TypeNats.N:SNat[0] <SizeHash (ADDRHASH c_a2yQq)>_P
                       :: KnownNat (SizeHash (ADDRHASH c_a2yQq)) ~R# Natural))
               Cardano.Ledger.Alonzo.TxOut.getAlonzoTxOutEitherAddr3
       of {
           __DEFAULT -> GHC.Maybe.Nothing @(Addr c_a2yQq);
           1# ->
           case Unsafe.Coerce.unsafeEqualityProof
                   @(*)
                   @(ghc-prim:GHC.Types.Any :~: ghc-prim:GHC.Types.Any)
                   @(SizeHash (ADDRHASH c_a2yQq) :~: 28)
           of
           { Unsafe.Coerce.UnsafeRefl co_a2HvY ->
           GHC.Maybe.Just
               @(Addr c_a2yQq)
               (Cardano.Ledger.Address.Addr
               @c_a2yQq
               (case ghc-prim:GHC.Prim.and# dt3_d2DMR 2## of {
                   __DEFAULT -> Cardano.Ledger.BaseTypes.Mainnet;
                   0## -> Cardano.Ledger.BaseTypes.Testnet
                   })
               (case ghc-prim:GHC.Prim.and# dt3_d2DMR 1## of {
                   __DEFAULT ->
                       Cardano.Ledger.Credential.KeyHashObj
                       @'Payment
                       @c_a2yQq
                       ((cardano-crypto-class-2.1.2.0-AvwSxmsDYBzI1yIWWm4yyw:Cardano.Crypto.PackedBytes.PackedBytes28
                           @28
                           @~(<28>_N :: 28 ghc-prim:GHC.Prim.~# 28)
                           dt_d2DMO
                           dt1_d2DMP
                           dt2_d2DMQ
                           (ghc-prim:GHC.Prim.wordToWord32#
                               (ghc-prim:GHC.Prim.uncheckedShiftRL# dt3_d2DMR 32#)))
                       `cast` ((PackedBytes
                                   (Nth:2 (Sub co_a2HvY)
                                   ; Nth:1 (Sub (Sym co_a2HvY))))_R
                               ; Sym (Cardano.Crypto.Hash.Class.N:Hash[0]
                                           <ADDRHASH c_a2yQq>_N
                                           <Cardano.Crypto.DSIGN.Class.VerKeyDSIGN
                                               (DSIGN c_a2yQq)>_P)
                               ; Sym (Cardano.Ledger.Keys.N:KeyHash[0] <'Payment>_P <c_a2yQq>_N)
                               :: PackedBytes 28 ~R# KeyHash 'Payment c_a2yQq));
                   0## ->
                       Cardano.Ledger.Credential.ScriptHashObj
                       @'Payment
                       @c_a2yQq
                       ((cardano-crypto-class-2.1.2.0-AvwSxmsDYBzI1yIWWm4yyw:Cardano.Crypto.PackedBytes.PackedBytes28
                           @28
                           @~(<28>_N :: 28 ghc-prim:GHC.Prim.~# 28)
                           dt_d2DMO
                           dt1_d2DMP
                           dt2_d2DMQ
                           (ghc-prim:GHC.Prim.wordToWord32#
                               (ghc-prim:GHC.Prim.uncheckedShiftRL# dt3_d2DMR 32#)))
                       `cast` ((PackedBytes
                                   (Nth:2 (Sub co_a2HvY)
                                   ; Nth:1 (Sub (Sym co_a2HvY))))_R
                               ; Sym (Cardano.Crypto.Hash.Class.N:Hash[0]
                                           <ADDRHASH c_a2yQq>_N <EraIndependentScript>_P)
                               ; Sym (Cardano.Ledger.Hashes.N:ScriptHash[0] <c_a2yQq>_N)
                               :: PackedBytes 28 ~R# ScriptHash c_a2yQq))
                   })
               (Cardano.Ledger.Credential.$WStakeRefBase @c_a2yQq eta_X1q))
           }
       }
       }

Let's orient ourselves before checking |old|. We see that ``decodeAddress28``
takes four arguments: a type variable called ``c_a2yQq``, a type class
dictionary called ``$dHashAlgorithm_a2yQr``, and two eta-expanded variables:
``eta_X1q`` and ``eta1_X1r``. It then immediately scrutinizes ``eta1_X1r`` to
bind ``Addr28Extra dt_d2DMO dt1_d2DMP dt2_d2DMQ dt3_d2DMR``, checks the equality
of the CAF ``Cardano.Ledger.Alonzo.TxOut.getAlonzoTxOutEitherAddr3`` and the
result of the application of the superclass ``$p1HashAlgorithm`` to the type
variable ``c_a2yQq`` and the dictionary ``$dHashAlgorithm_a2yQr``.
Unfortunately, the call to ``cardano-crypto-class`` ``PackedBytes28`` looks
duplicated, although this duplication could be an optimization. If the call was
de-duplicated then it would need to be allocated in a ``let`` which would be
more expensive than inlining and thereby duplicating the expression. The only
exception would be if GHC concluded that the de-duplicated ``let`` could be a
:term:`join point` since join points do not capture a closure, and therefore do
not perform the same allocation as a ``let`` binding does. Otherwise, this Core
looks good. It is concise, and is using unboxed values and primitives such as
``0##``, ``ghc-prim:GHC.Prim.and# dt3_d2DMR 2##``, and
``GHC.Num.Natural.naturalEq#``. Now let's check |old|:


|old|:

.. code-block:: haskell


    decodeAddress28
    = \ (@ c_a5pDW)
        ($dHashAlgorithm_a5pDY :: HashAlgorithm (ADDRHASH c_a5pDW))
        (eta2_X1hM :: Credential 'Staking c_a5pDW)
        (eta3_X2zx :: Addr28Extra) ->
        case eta3_X2zx of
        { Addr28Extra dt_d7b29 dt1_d7b2a dt2_d7b2b dt3_d7b2c ->
        join {
            $w$j_s8s7n [InlPrag=NOUSERINLINE[2], Dmd=<L,1*C1(U)>]
            :: (28 ghc-prim-0.6.1:GHC.Prim.~# SizeHash (ADDRHASH c_a5pDW))
                -> Maybe (Addr c_a5pDW)
            [LclId[JoinId(1)], Arity=1, Str=<L,U>, Unf=OtherCon []]
            $w$j_s8s7n (ww_s8s7l
                        :: 28 ghc-prim-0.6.1:GHC.Prim.~# SizeHash (ADDRHASH c_a5pDW))
            = GHC.Maybe.Just
                @ (Addr c_a5pDW)
                (Cardano.Ledger.Address.Addr
                    @ c_a5pDW
                    (case ghc-prim-0.6.1:GHC.Prim.and# dt3_d7b2c 2## of {
                        __DEFAULT -> Cardano.Ledger.BaseTypes.Mainnet;
                        0## -> Cardano.Ledger.BaseTypes.Testnet
                    })
                    (case ghc-prim-0.6.1:GHC.Prim.and# dt3_d7b2c 1## of {
                        __DEFAULT ->
                        Cardano.Ledger.Credential.KeyHashObj
                            @ 'Payment
                            @ c_a5pDW
                            ((cardano-crypto-class-2.1.2.0:Cardano.Crypto.PackedBytes.PackedBytes28
                                @ 28
                                @~ (<28>_N :: 28 ghc-prim-0.6.1:GHC.Prim.~# 28)
                                dt_d7b29
                                dt1_d7b2a
                                dt2_d7b2b
                                (ghc-prim-0.6.1:GHC.Prim.narrow32Word#
                                (ghc-prim-0.6.1:GHC.Prim.uncheckedShiftRL# dt3_d7b2c 32#)))
                            `cast` ((PackedBytes
                                        ww_s8s7l)_R ; (Sym (Cardano.Crypto.Hash.Class.N:Hash[0]
                                                                <ADDRHASH c_a5pDW>_N
                                                                <Cardano.Crypto.DSIGN.Class.VerKeyDSIGN
                                                                (DSIGN
                                                                    c_a5pDW)>_P) ; Sym (Cardano.Ledger.Keys.N:KeyHash[0]
                                                                                            <'Payment>_P
                                                                                            <c_a5pDW>_N))
                                    :: PackedBytes 28 ~R# KeyHash 'Payment c_a5pDW));
                        0## ->
                        Cardano.Ledger.Credential.ScriptHashObj
                            @ 'Payment
                            @ c_a5pDW
                            ((cardano-crypto-class-2.1.2.0:Cardano.Crypto.PackedBytes.PackedBytes28
                                @ 28
                                @~ (<28>_N :: 28 ghc-prim-0.6.1:GHC.Prim.~# 28)
                                dt_d7b29
                                dt1_d7b2a
                                dt2_d7b2b
                                (ghc-prim-0.6.1:GHC.Prim.narrow32Word#
                                (ghc-prim-0.6.1:GHC.Prim.uncheckedShiftRL# dt3_d7b2c 32#)))
                            `cast` ((PackedBytes
                                        ww_s8s7l)_R ; (Sym (Cardano.Crypto.Hash.Class.N:Hash[0]
                                                                <ADDRHASH c_a5pDW>_N
                                                                <EraIndependentScript>_P) ; Sym (Cardano.Ledger.Hashes.N:ScriptHash[0]
                                                                                                    <c_a5pDW>_N))
                                    :: PackedBytes 28 ~R# ScriptHash c_a5pDW))
                    })
                    (Cardano.Ledger.Credential.$WStakeRefBase
                        @ c_a5pDW eta2_X1hM)) } in
        case (Cardano.Crypto.Hash.Class.$p1HashAlgorithm
                @ (ADDRHASH c_a5pDW) $dHashAlgorithm_a5pDY)
            `cast` (GHC.TypeNats.N:KnownNat[0] <SizeHash
                                                    (ADDRHASH c_a5pDW)>_N ; GHC.TypeNats.N:SNat[0]
                                                                                <SizeHash
                                                                                    (ADDRHASH
                                                                                    c_a5pDW)>_P
                    :: KnownNat (SizeHash (ADDRHASH c_a5pDW)) ~R# GHC.Natural.Natural)
        of {
            GHC.Natural.NatS# a1_a7mr6 ->
            case Cardano.Ledger.Alonzo.TxOut.$fAlonzoEraTxOutAlonzoEra43 of {
                GHC.Natural.NatS# b1_a7mr9 ->
                case ghc-prim-0.6.1:GHC.Prim.eqWord# a1_a7mr6 b1_a7mr9 of {
                    __DEFAULT -> GHC.Maybe.Nothing @ (Addr c_a5pDW);
                    1# ->
                    jump $w$j_s8s7n
                        @~ (UnsafeCo nominal 28 (SizeHash (ADDRHASH c_a5pDW))
                            :: 28 ghc-prim-0.6.1:GHC.Prim.~# SizeHash (ADDRHASH c_a5pDW))
                };
                GHC.Natural.NatJ# ipv_a7ovU -> GHC.Maybe.Nothing @ (Addr c_a5pDW)
            };
            GHC.Natural.NatJ# dt4_a7mre ->
            case Cardano.Ledger.Alonzo.TxOut.$fAlonzoEraTxOutAlonzoEra43 of {
                GHC.Natural.NatS# ipv_a7ovW -> GHC.Maybe.Nothing @ (Addr c_a5pDW);
                GHC.Natural.NatJ# dt5_a7mri ->
                let {
                    nx#_a7mrh :: integer-gmp-1.0.3.0:GHC.Integer.Type.GmpSize#
                    [LclId]
                    nx#_a7mrh
                    = ghc-prim-0.6.1:GHC.Prim.uncheckedIShiftRL#
                        (ghc-prim-0.6.1:GHC.Prim.sizeofByteArray# dt4_a7mre) 3# } in
                case ghc-prim-0.6.1:GHC.Prim.==#
                        nx#_a7mrh
                        (ghc-prim-0.6.1:GHC.Prim.uncheckedIShiftRL#
                            (ghc-prim-0.6.1:GHC.Prim.sizeofByteArray# dt5_a7mri) 3#)
                of {
                    __DEFAULT -> GHC.Maybe.Nothing @ (Addr c_a5pDW);
                    1# ->
                    case {__pkg_ccall integer-gmp-1.0.3.0 ByteArray#
                                    -> ByteArray#
                                    -> Int#
                                    -> State# RealWorld
                                    -> (# State# RealWorld, Int# #)}_a7mrl
                            dt4_a7mre dt5_a7mri nx#_a7mrh ghc-prim-0.6.1:GHC.Prim.realWorld#
                    of
                    { (# ds2_a7mrn, ds3_a7mro #) ->
                    case ds3_a7mro of {
                        __DEFAULT -> GHC.Maybe.Nothing @ (Addr c_a5pDW);
                        0# ->
                        jump $w$j_s8s7n
                            @~ (UnsafeCo nominal 28 (SizeHash (ADDRHASH c_a5pDW))
                                :: 28 ghc-prim-0.6.1:GHC.Prim.~# SizeHash (ADDRHASH c_a5pDW))
                    }
                    }
                }
            }
        }
        }

``decodeAddress28`` is *less* optimized on |old|, it has the same inputs but
defines a :term:`join point`. Using a join point can be slower than inlining
[#]_ because the join point may prevent future optimizations that inlining would
allow, which is the case here. The Core for generated by |old| scrutinizes
``$p1HashAlgorithm``, leading to an extra case statement and unpacking of
``GHC.Natural``. In contrast, the Core generated by |new| casts
``$p1HashAlgorithm`` to a ``GHC.Natural`` which is then consumed by
``GHC.Num.Natural.naturalEq#``; no extra case needed and the cast will occur at
compile time.

We have observed a difference in the optimized versions of ``decodeAddress28``,
now let's check the unfoldings:

|new|:

.. code-block:: haskell

   -- RHS size: {terms: 54, types: 81, coercions: 52, joins: 0/0}
   decodeAddress28 [InlPrag=INLINE (sat-args=2)]
     :: forall c.
        HashAlgorithm (ADDRHASH c) =>
        Credential 'Staking c -> Addr28Extra -> Maybe (Addr c)
   [GblId,
    Arity=3,
    Str=<1P(1L,A,A,A)><L><1P(L,L,L,L)>,
    Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
            WorkFree=True, Expandable=True,
            Guidance=ALWAYS_IF(arity=2,unsat_ok=False,boring_ok=False)
            Tmpl= \ (@c_a2yQq)
                    ($dHashAlgorithm_a2yQr [Occ=Once1]
                       :: HashAlgorithm (ADDRHASH c_a2yQq))
                    (eta_X1q [Occ=Once1] :: Credential 'Staking c_a2yQq)
                    (eta1_X1r [Occ=Once1!] :: Addr28Extra) ->
                    case eta1_X1r of
                    { Addr28Extra dt_d2DMO [Occ=Once2] dt1_d2DMP [Occ=Once2]
                                  dt2_d2DMQ [Occ=Once2] dt3_d2DMR ->
                    case (Cardano.Crypto.Hash.Class.$p1HashAlgorithm
                            @(ADDRHASH c_a2yQq) $dHashAlgorithm_a2yQr)
                         `cast` (GHC.TypeNats.N:KnownNat[0] <SizeHash (ADDRHASH c_a2yQq)>_N
                                 ; GHC.TypeNats.N:SNat[0] <SizeHash (ADDRHASH c_a2yQq)>_P
                                 :: KnownNat (SizeHash (ADDRHASH c_a2yQq)) ~R# Natural)
                    of x1_a2ARA [Occ=Once1]
                    { __DEFAULT ->
                    case GHC.Num.Natural.naturalEq# x1_a2ARA 28 of {
                      __DEFAULT -> GHC.Maybe.Nothing @(Addr c_a2yQq);
                      1# ->
                        case Unsafe.Coerce.unsafeEqualityProof
                               @(*)
                               @(ghc-prim:GHC.Types.Any :~: ghc-prim:GHC.Types.Any)
                               @(SizeHash (ADDRHASH c_a2yQq) :~: 28)
                        of
                        { Unsafe.Coerce.UnsafeRefl co_a2HvY ->
                        GHC.Maybe.Just
                          @(Addr c_a2yQq)
                          (Cardano.Ledger.Address.Addr
                             @c_a2yQq
                             (case GHC.Word.neWord64
                                     (GHC.Word.W64# (ghc-prim:GHC.Prim.and# dt3_d2DMR 2##))
                                     (GHC.Word.W64# 0##)
                              of {
                                False -> Cardano.Ledger.BaseTypes.Testnet;
                                True -> Cardano.Ledger.BaseTypes.Mainnet
                              })
                             (case GHC.Word.neWord64
                                     (GHC.Word.W64# (ghc-prim:GHC.Prim.and# dt3_d2DMR 1##))
                                     (GHC.Word.W64# 0##)
                              of {
                                False ->
                                  Cardano.Ledger.Credential.$WScriptHashObj
                                    @'Payment
                                    @c_a2yQq
                                    ((cardano-crypto-class-2.1.2.0-AvwSxmsDYBzI1yIWWm4yyw:Cardano.Crypto.PackedBytes.$WPackedBytes28
                                        (GHC.Word.W64# dt_d2DMO)
                                        (GHC.Word.W64# dt1_d2DMP)
                                        (GHC.Word.W64# dt2_d2DMQ)
                                        (GHC.Word.W32#
                                           (ghc-prim:GHC.Prim.wordToWord32#
                                              (ghc-prim:GHC.Prim.uncheckedShiftRL# dt3_d2DMR 32#))))
                                     `cast` ((PackedBytes
                                                (Nth:2 (Sub co_a2HvY)
                                                 ; Nth:1 (Sub (Sym co_a2HvY))))_R
                                             ; Sym (Cardano.Crypto.Hash.Class.N:Hash[0]
                                                        <ADDRHASH c_a2yQq>_N <EraIndependentScript>_P)
                                             ; Sym (Cardano.Ledger.Hashes.N:ScriptHash[0] <c_a2yQq>_N)
                                             :: PackedBytes 28 ~R# ScriptHash c_a2yQq));
                                True ->
                                  Cardano.Ledger.Credential.$WKeyHashObj
                                    @'Payment
                                    @c_a2yQq
                                    ((cardano-crypto-class-2.1.2.0-AvwSxmsDYBzI1yIWWm4yyw:Cardano.Crypto.PackedBytes.$WPackedBytes28
                                        (GHC.Word.W64# dt_d2DMO)
                                        (GHC.Word.W64# dt1_d2DMP)
                                        (GHC.Word.W64# dt2_d2DMQ)
                                        (GHC.Word.W32#
                                           (ghc-prim:GHC.Prim.wordToWord32#
                                              (ghc-prim:GHC.Prim.uncheckedShiftRL# dt3_d2DMR 32#))))
                                     `cast` ((PackedBytes
                                                (Nth:2 (Sub co_a2HvY)
                                                 ; Nth:1 (Sub (Sym co_a2HvY))))_R
                                             ; Sym (Cardano.Crypto.Hash.Class.N:Hash[0]
                                                        <ADDRHASH c_a2yQq>_N
                                                        <Cardano.Crypto.DSIGN.Class.VerKeyDSIGN
                                                           (DSIGN c_a2yQq)>_P)
                                             ; Sym (Cardano.Ledger.Keys.N:KeyHash[0]
                                                        <'Payment>_P <c_a2yQq>_N)
                                             :: PackedBytes 28 ~R# KeyHash 'Payment c_a2yQq))
                              })
                             (Cardano.Ledger.Credential.$WStakeRefBase @c_a2yQq eta_X1q))
                        }
                    }
                    }
                    }}]

|old|:

.. code-block:: haskell

   -- RHS size: {terms: 96, types: 79, coercions: 50, joins: 1/2}
   decodeAddress28 [InlPrag=INLINE (sat-args=2)]
     :: forall c.
        HashAlgorithm (ADDRHASH c) =>
        Credential 'Staking c -> Addr28Extra -> Maybe (Addr c)
   [GblId,
    Arity=3,
    Caf=NoCafRefs,
    Str=<S(SLLL),1*U(1*U,A,A,A)><L,U><S,1*U(U,U,U,U)>,
    Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
            WorkFree=True, Expandable=True,
            Guidance=ALWAYS_IF(arity=3,unsat_ok=False,boring_ok=False)
            Tmpl= \ (@ c_a5pDW)
                    ($dHashAlgorithm_a5pDY [Occ=OnceL1]
                       :: HashAlgorithm (ADDRHASH c_a5pDW)) ->
                    let {
                      $dKnownNat_s7bbH [Occ=OnceL1] :: GHC.Natural.Natural
                      [LclId, Unf=OtherCon []]
                      $dKnownNat_s7bbH = 28 } in
                    let {
                      $dKnownNat1_a5pE7 [Occ=OnceL1]
                        :: KnownNat (SizeHash (ADDRHASH c_a5pDW))
                      [LclId]
                      $dKnownNat1_a5pE7
                        = Cardano.Crypto.Hash.Class.$p1HashAlgorithm
                            @ (ADDRHASH c_a5pDW) $dHashAlgorithm_a5pDY } in
                    \ (stakeRef_a5odb [Occ=Once1] :: Credential 'Staking c_a5pDW)
                      (ds_d5JEl [Occ=Once1!] :: Addr28Extra) ->
                      case ds_d5JEl of
                      { Addr28Extra dt_d7b29 [Occ=Once1] dt1_d7b2a [Occ=Once1]
                                    dt2_d7b2b [Occ=Once1] dt3_d7b2c ->
                      case sameNat
                             @ (SizeHash (ADDRHASH c_a5pDW))
                             @ 28
                             $dKnownNat1_a5pE7
                             ($dKnownNat_s7bbH
                              `cast` (Sym (GHC.TypeNats.N:SNat[0]
                                               <28>_P) ; Sym (GHC.TypeNats.N:KnownNat[0]) <28>_N
                                      :: GHC.Natural.Natural ~R# KnownNat 28))
                             (Data.Proxy.Proxy @ Nat @ (SizeHash (ADDRHASH c_a5pDW)))
                             (Data.Proxy.Proxy @ Nat @ 28)
                      of {
                        Nothing -> GHC.Maybe.Nothing @ (Addr c_a5pDW);
                        Just x_a7baN [Occ=Once1!] ->
                          case x_a7baN of { Refl co_a5pEe ->
                          let {
                            addrHash_s7bbM [Occ=Once2] :: PackedBytes 28
                            [LclId]
                            addrHash_s7bbM
                              = cardano-crypto-class-2.1.2.0:Cardano.Crypto.PackedBytes.$WPackedBytes28
                                  (GHC.Word.W64# dt_d7b29)
                                  (GHC.Word.W64# dt1_d7b2a)
                                  (GHC.Word.W64# dt2_d7b2b)
                                  (GHC.Word.W32#
                                     (ghc-prim-0.6.1:GHC.Prim.narrow32Word#
                                        (ghc-prim-0.6.1:GHC.Prim.uncheckedShiftRL#
                                           dt3_d7b2c 32#))) } in
                          GHC.Maybe.Just
                            @ (Addr c_a5pDW)
                            (Cardano.Ledger.Address.Addr
                               @ c_a5pDW
                               (case GHC.Word.neWord64
                                       (GHC.Word.W64# (ghc-prim-0.6.1:GHC.Prim.and# dt3_d7b2c 2##))
                                       (GHC.Word.W64# 0##)
                                of {
                                  False -> Cardano.Ledger.BaseTypes.Testnet;
                                  True -> Cardano.Ledger.BaseTypes.Mainnet
                                })
                               (case GHC.Word.neWord64
                                       (GHC.Word.W64# (ghc-prim-0.6.1:GHC.Prim.and# dt3_d7b2c 1##))
                                       (GHC.Word.W64# 0##)
                                of {
                                  False ->
                                    Cardano.Ledger.Credential.$WScriptHashObj
                                      @ 'Payment
                                      @ c_a5pDW
                                      (addrHash_s7bbM
                                       `cast` ((PackedBytes
                                                  co_a5pEe)_R ; (Sym (Cardano.Crypto.Hash.Class.N:Hash[0]
                                                                          <ADDRHASH c_a5pDW>_N
                                                                          <EraIndependentScript>_P) ; Sym (Cardano.Ledger.Hashes.N:ScriptHash[0]
                                                                                                               <c_a5pDW>_N))
                                               :: PackedBytes 28 ~R# ScriptHash c_a5pDW));
                                  True ->
                                    Cardano.Ledger.Credential.$WKeyHashObj
                                      @ 'Payment
                                      @ c_a5pDW
                                      (addrHash_s7bbM
                                       `cast` ((PackedBytes
                                                  co_a5pEe)_R ; (Sym (Cardano.Crypto.Hash.Class.N:Hash[0]
                                                                          <ADDRHASH c_a5pDW>_N
                                                                          <Cardano.Crypto.DSIGN.Class.VerKeyDSIGN
                                                                             (DSIGN
                                                                                c_a5pDW)>_P) ; Sym (Cardano.Ledger.Keys.N:KeyHash[0]
                                                                                                        <'Payment>_P
                                                                                                        <c_a5pDW>_N))
                                               :: PackedBytes 28 ~R# KeyHash 'Payment c_a5pDW))
                                })
                               (Cardano.Ledger.Credential.$WStakeRefBase
                                  @ c_a5pDW stakeRef_a5odb))
                          }
                      }
                      }}]

The unfoldings slightly only slightly differ, |old| allocates a let-bound
function while |new| eta-expands to four arguments. More importantly, we can now
see where the join point came from. |old| generates a ``let`` binding:
``addrHash_s7bbM`` which is then referenced in the branches of ``case
GHC.Word.neWord64...``. So the join point is an optimization on that ``let``.
Whether or not this will be faster than sinking the ``let``, as is the case on
|new|, is unclear, it very well could be. To check, we can inspect the Core of
``getEitherAddrBabbageTxOut`` to see how these unfoldings are optimized at their
call sites. If we find that the join point is preventing optimizations then we
can conclude that inlining is better for this chain of functions and that |new|
is better optimizing this code. If we find that the call site does not differ
and the only difference between |old| and |new| is ``decodeAddress28``, then we
can conclude that the inlining is not enabling more optimizations and the the
join point is worth it.

Let's check ``getEitherAddrBabbageTxOut``:

|new|:

.. code-block:: haskell

   -- RHS size: {terms: 58, types: 170, coercions: 16, joins: 0/0}
   getEitherAddrBabbageTxOut [InlPrag=INLINABLE]
     :: forall era.
        (HasCallStack, HashAlgorithm (ADDRHASH (EraCrypto era))) =>
        BabbageTxOut era
        -> Either (Addr (EraCrypto era)) (CompactAddr (EraCrypto era))
                      ...

|old|:

.. code-block:: haskell

   -- RHS size: {terms: 216, types: 293, coercions: 134, joins: 2/4}
   getEitherAddrBabbageTxOut [InlPrag=INLINABLE]
     :: forall era.
        (HasCallStack, HashAlgorithm (ADDRHASH (EraCrypto era))) =>
        BabbageTxOut era
        -> Either (Addr (EraCrypto era)) (CompactAddr (EraCrypto era))
                       ...


``getEitherAddrBabbageTxOut`` on |new| is small and simple with only 58 terms.
On |old| it grows to 216 terms *with* two join points.
``getEitherAddrBabbageTxOut`` is marked ``INLINEABLE`` so we'll inspect the body
instead of the unfolding:

|new|:

.. code-block:: haskell

   getEitherAddrBabbageTxOut
  = \ (@era_a1CtR)
      ($dIP14_a1CtS :: HasCallStack)
      ($dHashAlgorithm_a1CtT
         :: HashAlgorithm (ADDRHASH (EraCrypto era_a1CtR)))
      (eta_Xl :: BabbageTxOut era_a1CtR) ->
      case eta_Xl of {
        TxOutCompact' dt1_d1KDh ds_d1GmE ->
          Data.Either.Right
            @(Addr (EraCrypto era_a1CtR))
            @(CompactAddr (EraCrypto era_a1CtR))
            ((Data.ByteString.Short.Internal.SBS dt1_d1KDh)
             `cast` (Sym (Cardano.Ledger.Address.N:CompactAddr[0]
                              <EraCrypto era_a1CtR>_P)
                     :: Data.ByteString.Short.Internal.ShortByteString
                        ~R# CompactAddr (EraCrypto era_a1CtR)));
        TxOutCompactDH' dt1_d1KDi ds_d1GmF ds1_d1GmG ->
          Data.Either.Right
            @(Addr (EraCrypto era_a1CtR))
            @(CompactAddr (EraCrypto era_a1CtR))
            ((Data.ByteString.Short.Internal.SBS dt1_d1KDi)
             `cast` (Sym (Cardano.Ledger.Address.N:CompactAddr[0]
                              <EraCrypto era_a1CtR>_P)
                     :: Data.ByteString.Short.Internal.ShortByteString
                        ~R# CompactAddr (EraCrypto era_a1CtR)));
        TxOutCompactDatum dt1_d1KDj ds_d1GmK dt2_d1KDk ->
          Data.Either.Right
            @(Addr (EraCrypto era_a1CtR))
            @(CompactAddr (EraCrypto era_a1CtR))
            ((Data.ByteString.Short.Internal.SBS dt1_d1KDj)
             `cast` (Sym (Cardano.Ledger.Address.N:CompactAddr[0]
                              <EraCrypto era_a1CtR>_P)
                     :: Data.ByteString.Short.Internal.ShortByteString
                        ~R# CompactAddr (EraCrypto era_a1CtR)));
        TxOutCompactRefScript dt1_d1KDl ds_d1GmH ds1_d1GmI ds2_d1GmJ ->
          Data.Either.Right
            @(Addr (EraCrypto era_a1CtR))
            @(CompactAddr (EraCrypto era_a1CtR))
            ((Data.ByteString.Short.Internal.SBS dt1_d1KDl)
             `cast` (Sym (Cardano.Ledger.Address.N:CompactAddr[0]
                              <EraCrypto era_a1CtR>_P)
                     :: Data.ByteString.Short.Internal.ShortByteString
                        ~R# CompactAddr (EraCrypto era_a1CtR)));
        TxOut_AddrHash28_AdaOnly stakeRef_a1y6l dt1_d1KDm dt2_d1KDn
                                 dt3_d1KDo dt4_d1KDp dt5_d1KDq ->
          case $dHashAlgorithm_a1CtT of
          { Cardano.Crypto.Hash.Class.C:HashAlgorithm ww1_a1Giq ww2_a1Gir
                                                      ww3_a1Gis ww4_a1Giu ->
          case Cardano.Ledger.Alonzo.TxOut.$wdecodeAddress28
                 @(EraCrypto era_a1CtR)
                 ww1_a1Giq
                 stakeRef_a1y6l
                 dt1_d1KDm
                 dt2_d1KDn
                 dt3_d1KDo
                 dt4_d1KDp
          of {
          ...

Notice the last case expression; ``getEitherAddrBabbageTxOut`` explicitly calls
``decodeAddress28``, this is likely why ``getEitherAddrBabbageTxOut`` is smaller
on |new|. Let's check |old|:

|old|:

.. code-block:: haskell

   getEitherAddrBabbageTxOut
   = \ (@ era_a2SDv)
       ($dIP12_a2SDx :: HasCallStack)
       ($dHashAlgorithm_a2SDy
          :: HashAlgorithm (ADDRHASH (EraCrypto era_a2SDv)))
       (eta4_X1hp :: BabbageTxOut era_a2SDv) ->
       case eta4_X1hp of {
         TxOutCompact' dt_d5mAe ds1_d3IkC ->
           Data.Either.Right
             @ (Addr (EraCrypto era_a2SDv))
             @ (CompactAddr (EraCrypto era_a2SDv))
             ((Data.ByteString.Short.Internal.SBS dt_d5mAe)
              `cast` (Sym (Cardano.Ledger.Address.N:CompactAddr[0]
                               <EraCrypto era_a2SDv>_P)
                      :: Data.ByteString.Short.Internal.ShortByteString
                         ~R# CompactAddr (EraCrypto era_a2SDv)));
         TxOutCompactDH' dt_d5mAf ds1_d3IkD ds2_d3IkE ->
           Data.Either.Right
             @ (Addr (EraCrypto era_a2SDv))
             @ (CompactAddr (EraCrypto era_a2SDv))
             ((Data.ByteString.Short.Internal.SBS dt_d5mAf)
              `cast` (Sym (Cardano.Ledger.Address.N:CompactAddr[0]
                               <EraCrypto era_a2SDv>_P)
                      :: Data.ByteString.Short.Internal.ShortByteString
                         ~R# CompactAddr (EraCrypto era_a2SDv)));
         TxOutCompactDatum dt_d5mAg ds1_d3IkI dt1_d5mAh ->
           Data.Either.Right
             @ (Addr (EraCrypto era_a2SDv))
             @ (CompactAddr (EraCrypto era_a2SDv))
             ((Data.ByteString.Short.Internal.SBS dt_d5mAg)
              `cast` (Sym (Cardano.Ledger.Address.N:CompactAddr[0]
                               <EraCrypto era_a2SDv>_P)
                      :: Data.ByteString.Short.Internal.ShortByteString
                         ~R# CompactAddr (EraCrypto era_a2SDv)));
         TxOutCompactRefScript dt_d5mAi ds1_d3IkF ds2_d3IkG ds3_d3IkH ->
           Data.Either.Right
             @ (Addr (EraCrypto era_a2SDv))
             @ (CompactAddr (EraCrypto era_a2SDv))
             ((Data.ByteString.Short.Internal.SBS dt_d5mAi)
              `cast` (Sym (Cardano.Ledger.Address.N:CompactAddr[0]
                               <EraCrypto era_a2SDv>_P)
                      :: Data.ByteString.Short.Internal.ShortByteString
                         ~R# CompactAddr (EraCrypto era_a2SDv)));
         TxOut_AddrHash28_AdaOnly stakeRef_a2PWm dt_d5mAj dt1_d5mAk
                                  dt2_d5mAl dt3_d5mAm dt4_d5mAn ->
           case $dHashAlgorithm_a2SDy of
           { Cardano.Crypto.Hash.Class.C:HashAlgorithm ww1_a3Hw4 ww2_a3Hw5
                                                       ww3_a3Hw6 ww4_a3Hw7 ->
           join {
             $w$j_a5wdt [InlPrag=NOUSERINLINE[2], Dmd=<L,1*C1(U)>]


Sure enough. |old| inlines ``decodeAddress28`` and takes advantage of the join
point. From the Core meta data of ``decodeAddress28`` we can see that |new|
decides ``decodeAddress28`` can be inlined due to the ``InlPrag`` field at the
function header: ``decodeAddress28 [InlPrag=INLINE (sat-args=2)]``. However, it
does not look like ``decodeAddress28`` is inlined across packages (``alonzo`` is
a different package than ``babbage`` in the Cardano code base). Furthermore, we
can check the source code. We should find that ``decodeAddress28`` is in the
export list of its module and that it is *not* marked ``INLINE``, or else it
should have been inlined across the package boundary:

.. code-block:: haskell

   module Cardano.Ledger.Alonzo.TxOut (
     ...
     decodeDataHash32,
     encodeDataHash32,
     encodeAddress28,
     decodeAddress28,
     ...
   )
   where
   ...
   decodeAddress28 ::
     forall c.
     HashAlgorithm (ADDRHASH c) =>
     Credential 'Staking c ->
     Addr28Extra ->
     Maybe (Addr c)
   decodeAddress28 stakeRef (Addr28Extra a b c d) = do
     Refl <- sameNat (Proxy @(SizeHash (ADDRHASH c))) (Proxy @28)
     let network = if d `testBit` 1 then Mainnet else Testnet
         paymentCred =
           if d `testBit` 0
             then KeyHashObj (KeyHash addrHash)
             else ScriptHashObj (ScriptHash addrHash)
         addrHash :: Hash (ADDRHASH c) a
         addrHash =
           hashFromPackedBytes $
             PackedBytes28 a b c (fromIntegral (d `shiftR` 32))
     pure $! Addr network paymentCred (StakeRefBase stakeRef)

TODO start here tomorrow

Going Further
-------------

The regression is directly observable from the Core summary output that GHC
produces at the top of each Core file. Here is the Core summary on |new| for
``Cardano.Ledger.Address`` :

.. code-block:: haskell

   ==================== Tidy Core ====================
   2023-08-09 17:58:04.217192572 UTC

   Result size of Tidy Core
     = {terms: 59,840,
        types: 65,769,
        coercions: 31,464,
        joins: 135/1,454}
   ...

while on |old| we have:

.. code-block:: haskell

   ==================== Tidy Core ====================
   2023-08-08 22:45:09.679031824 UTC

   Result size of Tidy Core
     = {terms: 10,681,
        types: 18,069,
        coercions: 7,591,
        joins: 22/273}
   ...

Notice the 6-fold increase in terms on |new| along with concomitant increases in
types, coercions, and join points. Now let's find where the code bloat is
occurring by inspecting the Core of ``updateStakeDistribution``.

|new|:

.. code-block:: haskell

   updateStakeDistribution
     = \ (@era_a4IOQ)
         ($dEraTxOut_a4IOR :: EraTxOut era_a4IOQ)
         (pp_a4IkS :: PParams era_a4IOQ)
         (incStake0_a4IkT :: IncrementalStake (EraCrypto era_a4IOQ))
         (utxoDel_a4IkU :: UTxO era_a4IOQ)
         (utxoAdd_a4IkV :: UTxO era_a4IOQ) ->
         case incStake0_a4IkT of { IStake ww1_s4KkP ww2_s4KkQ ->
         case Cardano.Ledger.Shelley.LedgerState.IncrementalStake.$wincrementalAggregateUtxoCoinByCredential
                @era_a4IOQ $dEraTxOut_a4IOR pp_a4IkS (id @Coin) utxoAdd_a4IkV ww1_s4KkP ww2_s4KkQ
         of
         { (# ww4_s4KpB, ww5_s4KpC #) ->
         case Cardano.Ledger.Shelley.LedgerState.IncrementalStake.$wincrementalAggregateUtxoCoinByCredential
                @era_a4IOQ $dEraTxOut_a4IOR pp_a4IkS
                (GHC.Num.Integer.integerNegate
                 `cast` (Sym (Cardano.Ledger.Coin.N:Coin[0])
                 % <'Many>_N ->_R Sym (Cardano.Ledger.Coin.N:Coin[0])
                         :: (Integer -> Integer) ~R# (Coin -> Coin)))
                utxoDel_a4IkU ww4_s4KpB ww5_s4KpC
         of
         { (# ww7_X4, ww8_X5 #) ->
         Cardano.Ledger.Shelley.LedgerState.Types.IStake
           @(EraCrypto era_a4IOQ) ww7_X4 ww8_X5
         }}}

while on |old| we have:

.. code-block:: haskell


   updateStakeDistribution
     = \ (@ era_atqca)
         ($dEraTxOut_atqcc :: EraTxOut era_atqca)
         (pp_atppw :: PParams era_atqca)
         (incStake0_atppx :: IncrementalStake (EraCrypto era_atqca))
         (utxoDel_atppy :: UTxO era_atqca)
         (utxoAdd_atppz :: UTxO era_atqca) ->
         case incStake0_atppx of { IStake ww1_sxkut ww2_sxkuu ->
         case Cardano.Ledger.Shelley.LedgerState.IncrementalStake.$wincrementalAggregateUtxoCoinByCredential
                @ era_atqca
                $dEraTxOut_atqcc
                pp_atppw
                (id @ Coin)
                utxoAdd_atppz
                ww1_sxkut
                ww2_sxkuu
         of
         { (# ww4_sxkuY, ww5_sxkuZ #) ->
         case Cardano.Ledger.Shelley.LedgerState.IncrementalStake.$wincrementalAggregateUtxoCoinByCredential
                @ era_atqca $dEraTxOut_atqcc pp_atppw
                (integer-gmp-1.0.3.0:GHC.Integer.Type.negateInteger
                 `cast` (Sym (Cardano.Ledger.Coin.N:Coin[0])
                         ->_R Sym (Cardano.Ledger.Coin.N:Coin[0])
                         :: (Integer -> Integer) ~R# (Coin -> Coin)))
                utxoDel_atppy ww4_sxkuY ww5_sxkuZ
         of
         { (# ww7_XxkBY, ww8_XxkC0 #) ->
         Cardano.Ledger.Shelley.LedgerState.Types.IStake
           @ (EraCrypto era_atqca) ww7_XxkBY ww8_XxkC0
         }}}

The Core is essentially identical between compiler versions;
``GHC.Num.Integer.integerNegate`` and
``integer-gmp-1.0.3.0:GHC.Integer.Type.negateInteger`` differ because of the
``ghc-bignum`` changes between |old| and |new|, but they will compile to the
same primops. Therefore, the regression must occur in a function called by
``updateStakeDistribution``; of which we have only one candidate: the worker for
``incrementalAggregateUtxoCoinByCredential``. First here is the source code for
this function:

.. code-block:: haskell

   incrementalAggregateUtxoCoinByCredential ::
     forall era.
     EraTxOut era =>
     PParams era ->
     (Coin -> Coin) ->
     UTxO era ->
     IncrementalStake (EraCrypto era) ->
     IncrementalStake (EraCrypto era)
   incrementalAggregateUtxoCoinByCredential pp mode (UTxO u) initial =
     Map.foldl' accum initial u
     where
       keepOrDelete new Nothing =
         case mode new of
           Coin 0 -> Nothing
           final -> Just final
       keepOrDelete new (Just old) =
         case mode new <> old of
           Coin 0 -> Nothing
           final -> Just final
       ignorePtrs = HardForks.forgoPointerAddressResolution (pp ^. ppProtocolVersionL)
       accum ans@(IStake stake ptrs) out =
         let c = out ^. coinTxOutL
          in case out ^. addrTxOutL of
               Addr _ _ (StakeRefPtr p) ->
                 if ignorePtrs
                   then ans
                   else IStake stake (Map.alter (keepOrDelete c) p ptrs)
               Addr _ _ (StakeRefBase hk) -> IStake (Map.alter (keepOrDelete c) hk stake) ptrs
               _other -> ans

The Core for this function is too large to show but does not differ between
compiler versions, we can quickly spotcheck for differences by checking the
Core summary for the worker of ``incrementalAggregateUtxoCoinByCredential``:

|new|:

.. code-block:: haskell

   -- RHS size: {terms: 154, types: 238, coercions: 122, joins: 0/7}
   Cardano.Ledger.Shelley.LedgerState.IncrementalStake.$wincrementalAggregateUtxoCoinByCredential [InlPrag=[2]]
     :: forall {era}.
        EraTxOut era =>
        PParams era
        -> (Coin -> Coin)
        -> UTxO era
        -> Map (Credential 'Staking (EraCrypto era)) Coin
        -> Map Cardano.Ledger.Credential.Ptr Coin
        -> (# Map (Credential 'Staking (EraCrypto era)) Coin,
              Map Cardano.Ledger.Credential.Ptr Coin #)
   [GblId,
    Arity=6,
    Str=...,
    Unf=OtherCon []]
   Cardano.Ledger.Shelley.LedgerState.IncrementalStake.$wincrementalAggregateUtxoCoinByCredential

|old|:

.. code-block:: haskell

   -- RHS size: {terms: 154, types: 241, coercions: 126, joins: 0/7}
   Cardano.Ledger.Shelley.LedgerState.IncrementalStake.$wincrementalAggregateUtxoCoinByCredential [InlPrag=NOUSERINLINE[2]]
     :: forall era.
        EraTxOut era =>
        PParams era
        -> (Coin -> Coin)
        -> UTxO era
        -> Map (Credential 'Staking (EraCrypto era)) Coin
        -> Map Cardano.Ledger.Credential.Ptr Coin
        -> (# Map (Credential 'Staking (EraCrypto era)) Coin,
              Map Cardano.Ledger.Credential.Ptr Coin #)
   [GblId,
    Arity=6,
    Str=...,
    Unf=OtherCon []]
   Cardano.Ledger.Shelley.LedgerState.IncrementalStake.$wincrementalAggregateUtxoCoinByCredential

Notice that the terms are identical, there are slightly less types on |new|, and
more coercions on |old|. So again the difference must be in function that
``incrementalAggregateUtxoCoinByCredential`` calls. We have the following candidates:

#. ``coinTxOutL``
#. ``HardForks.forgoPointerAddressResolution``
#. ``ppProtocolVersionL``
#. ``pp``
#. ``Map.alter``
#. ``addrTxOutL``

Of those there are only two functions that were not inlined. An easy way to tell
is simply to search for the function name in the Core of
``incrementalAggregateUtxoCoinByCredential``; if a function was inlined *and*
regressed then we should see a signal in the terms field of the Core summary.
Thus our only candidates are ``Map.alter`` and ``addrTxOutL``.

``Map.alter`` is a function imported from ``Data.Map.Strict``, so as a first
sanity check we'll make sure that the same version of the ``containers`` library
was used. Fortunately, the ``cardano-ledger`` code base uses `nix
<https://nixos.org/>`_ precisely specify dependencies for ``cardano-ledger``.
This means we can simply ask cabal to list the installed packages and observe
any difference in versions.

Here is the environment for |new|:

.. code-block:: console

   $ nix develop .#ghc927
   [nix-shell:~/cardano-ledger]$ cabal list --installed | awk -v RS='' '/* containers/'
   * containers
       Synopsis: Assorted concrete container types
       Default available version: 0.6.7
       Installed versions: 0.6.5.1
       License:  BSD3

while on |old| we have:

.. code-block:: console

   $ nix develop .#ghc8107
   [nix-shell:~/cardano-ledger]$ cabal list --installed | awk -v RS='' '/* containers/'
   * containers
       Synopsis: Assorted concrete container types
       Default available version: 0.6.7
       Installed versions: 0.6.5.1
       License:  BSD3

which leaves only ``addrTxOutL``. Again, using the Core summary as a spot check
we find a significant difference:

|new|:

.. code-block:: haskell

   -- RHS size: {terms: 1,058,
                 types: 1,043,
                 coercions: 541,
                 joins: 15/25}
   Cardano.Ledger.Core.$dmaddrTxOutL [InlPrag=INLINE (sat-args=0)]

|old|:

.. code-block:: haskell

   -- RHS size: {terms: 658,
                 types: 1,028,
                 coercions: 503,
                 joins: 4/17}
   Cardano.Ledger.Core.$dmaddrTxOutL [InlPrag=INLINE (sat-args=0)]

Notice that |new| is almost twice the size of |old|. Note also that
``addrTxOutL`` is prefixed with ``$dm``. :ref:`As you'll recall <Reading Core>`.
``$`` generally means the name comes from a type class dictionary ("generally"
because on can get ``$wfoo`` through the :ref:`Worker/Wrapper <Worker Wrapper
Chapter>` optimization), while ``dm`` means that this function is a *default
method* of a type class.

Let's check the source, ``addrTxOutL`` belongs to a large type class called
``EraTxOut`` located in ``Cardano.Ledger.Core``:

.. code-block:: haskell

   -- | Abstract interface into specific fields of a `TxOut`
   class
     ( Val (Value era)
     , ToJSON (TxOut era)
     , DecCBOR (Value era)
     , DecCBOR (CompactForm (Value era))
     , EncCBOR (Value era)
     , ToCBOR (TxOut era)
     , FromCBOR (TxOut era)
     , EncCBOR (TxOut era)
     , DecCBOR (TxOut era)
     , DecShareCBOR (TxOut era)
     , Share (TxOut era) ~ Interns (Credential 'Staking (EraCrypto era))
     , NoThunks (TxOut era)
     , NFData (TxOut era)
     , Show (TxOut era)
     , Eq (TxOut era)
     , EraPParams era
     ) =>
     EraTxOut era
     where
     -- | The output of a UTxO for a particular era
     type TxOut era = (r :: Type) | r -> era

     ...

    addrTxOutL :: Lens' (TxOut era) (Addr (EraCrypto era))
    addrTxOutL =
      lens
        ( \txOut -> case txOut ^. addrEitherTxOutL of
            Left addr -> addr
            Right cAddr -> decompactAddr cAddr
        )
        (\txOut addr -> txOut & addrEitherTxOutL .~ Left addr)
    {-# INLINE addrTxOutL #-}

The type class is highly polymorphic with 16 type class constraints.
``addrTxOutL`` is a lens which calls ``addrEitherTxOutL`` and ``decompactAddr``.
Let's check those functions.

|new|:

.. code-block:: haskell

   -- RHS size: {terms: 6, types: 129, coercions: 0, joins: 0/0}
   addrEitherTxOutL
     :: forall era.
        EraTxOut era =>
        Lens'
          (TxOut era)
          (Either (Addr (EraCrypto era)) (CompactAddr (EraCrypto era)))
   [GblId[ClassOp],
    Arity=1,
    Caf=NoCafRefs,
    Str=<SP(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,SL,A,A)>,
    RULES: Built in rule for addrEitherTxOutL: "Class op addrEitherTxOutL"]
   addrEitherTxOutL
     = \ (@era_a1TXN) (v_B1 :: EraTxOut era_a1TXN) ->
         case v_B1 of v_B1
         { Cardano.Ledger.Core.C:EraTxOut v_B2 v_B3 v_B4 v_B5 v_B6 v_B7 v_B8
                                          v_B9 v_Ba v_Bb v_Bc v_Bd v_Be v_Bf v_Bg v_Bh v_Bi v_Bj v_Bk
                                          v_Bl v_Bm v_Bn v_Bo v_Bp v_Bq v_Br ->
         v_Bp
         }

|old|:

.. code-block:: haskell

   -- RHS size: {terms: 6, types: 130, coercions: 0, joins: 0/0}
   addrEitherTxOutL
     :: forall era.
        EraTxOut era =>
        Lens'
          (TxOut era)
          (Either (Addr (EraCrypto era)) (CompactAddr (EraCrypto era)))
   [GblId[ClassOp],
    Arity=1,
    Caf=NoCafRefs,
    Str=<S(LLLLLLLLLLLLLLLLLLLLLLLSLL),U(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,U,A,A)>,
    RULES: Built in rule for addrEitherTxOutL: "Class op addrEitherTxOutL"]
   addrEitherTxOutL
     = \ (@ era_a3NLT) (v_B1 :: EraTxOut era_a3NLT) ->
         case v_B1 of v_B1
         { Cardano.Ledger.Core.C:EraTxOut v_B2 v_B3 v_B4 v_B5 v_B6 v_B7 v_B8
                                          v_B9 v_Ba v_Bb v_Bc v_Bd v_Be v_Bf v_Bg v_Bh v_Bi v_Bj v_Bk
                                          v_Bl v_Bm v_Bn v_Bo v_Bp v_Bq v_Br ->
         v_Bp
         }

No difference, also the Core is fairly well formed. Good job lens! This leaves
only ``addrTxOutL``:

|new|:

.. code-block:: haskell


   -- RHS size: {terms: 1,058,
                 types: 1,043,
                 coercions: 541,
                 joins: 15/25}
   Cardano.Ledger.Core.$dmaddrTxOutL [InlPrag=INLINE (sat-args=0)]
     :: forall era.
        EraTxOut era =>
        Lens' (TxOut era) (Addr (EraCrypto era))
   [GblId,
    Arity=1,
    Str=...,
    Unf=Unf{Src=Compulsory, TopLvl=True, Value=True, ConLike=True,
            WorkFree=True, Expandable=True,
            Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
            Tmpl= \ (@era_a1TXN) ($dEraTxOut_a1ZWu :: EraTxOut era_a1TXN) ->
                    let {
   ...
   Cardano.Ledger.Core.$dmaddrTxOutL
     = \ (@era_a1TXN) ($dEraTxOut_a1ZWu :: EraTxOut era_a1TXN) ->
         let {

|old|:

.. code-block:: haskell

   -- RHS size: {terms: 658,
                 types: 1,028,
                 coercions: 503,
                 joins: 4/17}
   Cardano.Ledger.Core.$dmaddrTxOutL [InlPrag=INLINE (sat-args=0)]
     :: forall era.
        EraTxOut era =>
        Lens' (TxOut era) (Addr (EraCrypto era))
   [GblId,
    Arity=2,
    Str=...,
    Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
            WorkFree=True, Expandable=True,
            Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
            Tmpl= \ (@ era_a3NLT) ($dEraTxOut_a3Xnj :: EraTxOut era_a3NLT) ->
                    let {
                      $dEraPParams_a3YKj [Occ=OnceL1] :: EraPParams era_a3NLT
   ...
   Cardano.Ledger.Core.$dmaddrTxOutL
     = \ (@ era_a3NLT) ($dEraTxOut_a3Xnj :: EraTxOut era_a3NLT) (@ (f_a3XoX :: * -> *))
         (eta_B1 :: Functor f_a3XoX) ->
         let {

The Core summary shows a blow up of *exactly* 400 more terms on |new| (1,058)
compared to |old| (658). Note that I am showing a bit of the unfoldings (the
``Unf`` record, and specifically the ``Tmpl=`` field) for reasons that will soon
be apparent. That the increase in terms is so regular is peculiar. It suggests
that we have some name that is 100 terms or so and it is being inlined 4 times.
This is a clue, we should pay special attention to ``INLINE`` pragmas and
unfoldings.

There are several interesting differences in these versions. First, from the
meta information we can see that the ``Arity`` changes. On |new|
``$dmaddrTxOutL`` has an ``Arity`` of 1, while on |old| it has an ``Arity``
of 2. But that is just the unfolding, the optimized version on |old| shows three
arguments: 2 type arguments, ``era_a3NLT`` and ``f_a3XoX :: * -> *``; and one
type class dictionary ``$dEraTxOut_a3Xnj``. However, on |new| the optimized
version still only has two arguments: ``era_a1TXN`` and ``dEraTxOut_a1ZWu``.
Furthermore, notice that the extra argument on |old| is called ``eta_B1``, this
argument comes from the eta expansion optimization. Lastly, the ``Src`` field
has changed, in |old| we have ``Src=InlineStable`` while on |new| its
``Src=Compulsory``. In practice, these should be basically the same thing.
``Compulsory`` is used for GHC generated names and means that the function will
be inlined at every call site [#]_, whereas ``InlineStable`` means the function
is a wrapper or system-generated unfolding.

The rest of the Core is large, so I will just highlight the relevant parts. Our
first tactic is to step through the Core to observe the difference between the
two versions. We'll begin with the unfoldings for each version. This is a sanity
check, unfoldings are very close to the raw right-hand side of a function
definition and so we should expect these to be essentially identical.
Furthermore, because ``addrTxOutL`` is marked as ``INLINE`` the unfolding is the
Core that will be inlined into each call site.

Unfortunately this is not the case for this function. Recall that this function
creates a lens and pattern matches on an ``Either`` with a case expression; here
is that case expression in the unfolding:

|new|:

.. code-block:: haskell

   Cardano.Ledger.Core.$dmaddrTxOutL [InlPrag=INLINABLE]
     :: forall era. EraTxOut era => Lens' (TxOut era) (Addr (EraCrypto era))
   [GblId,
    Arity=1,
    Str=...,
    Unf=Unf{Src=Compulsory, TopLvl=True, Value=True, ConLike=True,
            WorkFree=True, Expandable=True,
            Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
            Tmpl= \ (@era_afw7) ($dEraTxOut_apRS :: EraTxOut era_afw7) ->
   ...
   lens
   @(TxOut era_afw7)
   @(Addr (EraCrypto era_afw7))
   @(Addr (EraCrypto era_afw7))
   @(TxOut era_afw7)
   (\ (txOut_afwj [Occ=Once1] :: TxOut era_afw7) ->
        case (addrEitherTxOutL
                @era_afw7
                $dEraTxOut_apRS
        ...
        of {
            Left addr_afwk [Occ=Once1] -> addr_afwk;
            Right cAddr_afwl ->
            let {
                $dIsString1_isdy [Occ=OnceL1] :: String -> [Char]
                [LclId, Arity=1, Unf=OtherCon []]
                $dIsString1_isdy
                = \ (eta1_isdz [Occ=Once1] :: String) -> eta1_isdz } in
            let {
                $dMonadFail1_isdA
                :: MonadFail
                        (Control.Monad.Trans.Fail.FailT
                        [Char] Data.Functor.Identity.Identity)
                [LclId]
                $dMonadFail1_isdA
                = Control.Monad.Trans.Fail.$fMonadFailFailT
                    @[Char]
                    @Data.Functor.Identity.Identity
                    ($dIsString1_isdy
                        `cast` (Sym (Data.String.N:IsString[0]) <[Char]>_N
                                :: (String -> [Char]) ~R# Data.String.IsString [Char]))
                    Data.Functor.Identity.$fMonadIdentity } in
            let {
                header_ise0 :: GHC.Word.Word8
                [LclId]
                header_ise0
                = cardano-prelude-0.1.0.2-DWhOQlInrHGJKMWDMqUhtQ:Cardano.Prelude.Compat.ByteString.Short.unsafeShortByteStringIndex
                    (cAddr_afwl
                        `cast` (Cardano.Ledger.Address.N:CompactAddr[0]
                                    <EraCrypto era_afw7>_P
                                :: CompactAddr (EraCrypto era_afw7)
                                ~R# Data.ByteString.Short.Internal.ShortByteString))
                    (ghc-prim:GHC.Types.I# 0#) } in
            let {
                s2_ise1 :: Int
                [LclId, Unf=OtherCon []]
                s2_ise1 = ghc-prim:GHC.Types.I# 0# } in
            case cAddr_afwl
                    `cast` (Cardano.Ledger.Address.N:CompactAddr[0]
                                <EraCrypto era_afw7>_P
                            :: CompactAddr (EraCrypto era_afw7)
                            ~R# Data.ByteString.Short.Internal.ShortByteString)
            of wild1_ise2
            { Data.ByteString.Short.Internal.SBS ba#_ise5 ->
            join {

Notice the ``Right`` branch has four ``let`` expressions, two for type class
dictionaries: ``$dIsString1_isdy`` and ``$dMonadFail1_isdA`` and two values: a
``Word8`` value called ``header_ise0`` and an ``Int`` called ``s2_ise1``.

|old|:

.. code-block:: haskell

   Cardano.Ledger.Core.$dmaddrTxOutL [InlPrag=INLINE (sat-args=0)]
   :: forall era. EraTxOut era => Lens' (TxOut era) (Addr (EraCrypto era))
   [GblId,
   Arity=2,
   Str=...,
   Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
           WorkFree=True, Expandable=True,
            Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
            Tmpl= \ (@ era_a3NLT) ($dEraTxOut_a3Xnj :: EraTxOut era_a3NLT) ->
    ...
    lens
        @ (TxOut era_a3NLT)
        @ (Addr (EraCrypto era_a3NLT))
        @ (Addr (EraCrypto era_a3NLT))
        @ (TxOut era_a3NLT)
        (\ (txOut_a3NM5 [Occ=Once1] :: TxOut era_a3NLT) ->
        case (addrEitherTxOutL
                @ era_a3NLT
                $dEraTxOut_a3Xnj
        ...
        of {
            Left addr_a3NM6 [Occ=Once1] -> addr_a3NM6;
            Right cAddr_a3NM7 ->
            let {
                header_sJVD :: GHC.Word.Word8
                [LclId]
                header_sJVD
                = Data.ByteString.Short.Internal.index
                    (cAddr_a3NM7
                        `cast` (Cardano.Ledger.Address.N:CompactAddr[0]
                                    <EraCrypto era_a3NLT>_P
                                :: CompactAddr (EraCrypto era_a3NLT)
                                ~R# Data.ByteString.Short.Internal.ShortByteString))
                    (ghc-prim-0.6.1:GHC.Types.I# 0#) } in
            let {
                s2_aIzM :: Int
                [LclId, Unf=OtherCon []]
                s2_aIzM = ghc-prim-0.6.1:GHC.Types.I# 0# } in
                case cAddr_a3NM7
                        `cast` (Cardano.Ledger.Address.N:CompactAddr[0]
                                    <EraCrypto era_a3NLT>_P
                                :: CompactAddr (EraCrypto era_a3NLT)
                                ~R# Data.ByteString.Short.Internal.ShortByteString)
                of
                { Data.ByteString.Short.Internal.SBS barr#_iFrL ->
                join {...

On |old| the ``Right`` branch is smaller, instead of four let expressions we
only have two, and the only two are values not type class dictionaries. This
means that the function ``Cardano.Ledger.decompactAddr`` has regressed because
this function constitutes the ``Right`` branch.

The extra ``let`` will increase allocations for this branch so they likely
contribute to the regression. The larger problem is that we would expect these
dictionaries to inline and thereby enabling more optimizations, such as floating
the ``let`` out of the branch.

Let's continue diving into the Core. We'll pay special attention to the type
class dictionaries because these ``lets`` are evidence that something is amiss
with the polymorphism of ``decompactAddr``.

Here is a meaningful difference. Note that this Core is firmly in the body of
``decompactAddr`` :

|new|:

.. code-block:: haskell

   ...
   case ghc-prim:GHC.Classes.geInt
       karg2_iseK
       (ghc-prim:GHC.Types.I# 0#)
   of {
   False ->
       jump $j_ise3
       (GHC.Base.build
           @[Char]
           (\ (@a6_isg9)
               (c1_isga [Occ=Once1!,
                       OS=OneShot]
               :: [Char]
                   -> a6_isg9
                   -> a6_isg9)
               (n_isgb [Occ=Once1,
                       OS=OneShot]
               :: a6_isg9) ->
               c1_isga
               (GHC.Base.build
                   @Char
                   (\ (@b_isgc) ->
                       ghc-prim:GHC.CString.unpackFoldrCString#
                       @b_isgc
                       "Impossible: Negative offset"#))
               n_isgb));
   True ->
       case (((Cardano.Ledger.Address.failDecoding
               @(Control.Monad.Trans.State.Lazy.StateT
                   Int
                   (Control.Monad.Trans.Fail.FailT
                       [Char]
                       Data.Functor.Identity.Identity))
               @(Hash.Hash
                   (CC.ADDRHASH
                       (EraCrypto
                           era_afw7))
                   (Cardano.Crypto.DSIGN.Class.VerKeyDSIGN
                       (CC.DSIGN
                           (EraCrypto
                           era_afw7))))
               (Control.Monad.Trans.State.Lazy.$fMonadFailStateT
                   @(Control.Monad.Trans.Fail.FailT
                       [Char]
                       Data.Functor.Identity.Identity)
                   @Int
                   $dMonadFail1_isdA)
                    (GHC.Base.build
                        @Char
                        (\ (@b_isgl) ->
                            ghc-prim:GHC.CString.unpackFoldrCString#
                            @b_isgl
                            "Hash"#))

   ...

We have a case expression comparing two ``Int``: ``ghc-prim:GHC.Classes.geInt
karg2_iseK (ghc-prim:GHC.Types.I# 0#)`` and then matches on the resulting
``Bool``. The ``False`` branch is a :term:`join point`; what is interesting
about this branch is the snippet that is building a ``String``:
``ghc-prim:GHC.CString.unpackFoldrCString# @b_isgc "Impossible: Negative
offset"#``. Clearly this is an error branch, but we should expect calls like
this to become a :term:`CAF` after being floated out to the top level. The
``True`` branch is not better, it calls ``Cardano.Ledger.Address.failDecoding``
and does the type applications for that function, which must use monad
transformer stack. Notably, the ``MonadFail`` dictionary *has not* been inlined,
this is why this application: ``Control.Monad.Trans.State.Lazy.$fMonadFailStateT
<type-args> $dMonadFail1_isdA`` exists. Also recall that ``$dMonadFail1_isdA``
is the name that was allocated in an extra ``let``; from a quick search this
name is referenced 22 times, thus an optimized version is likely to have a big
impact.

Here is the same section of Core on |old|:

.. code-block:: haskell

   ...
   case ghc-prim-0.6.1:GHC.Classes.geInt
           karg2_sYxg
           (ghc-prim-0.6.1:GHC.Types.I#
           0#)
   of {
   False ->
       jump $j_sYwR
       (GHC.Base.build
           @ [Char]
           (\ (@ a6_aJZH)
               (c1_aJZI [Occ=Once1!,
                       OS=OneShot]
                   :: [Char]
                   -> a6_aJZH
                   -> a6_aJZH)
               (n_aJZJ [Occ=Once1,
                       OS=OneShot]
                   :: a6_aJZH) ->
               c1_aJZI
               (GHC.Base.build
                   @ Char
                   (\ (@ b_iHyc) ->
                       ghc-prim-0.6.1:GHC.CString.unpackFoldrCString#
                       @ b_iHyc
                       "Impossible: Negative offset"#))
               n_aJZJ));
   True ->
       case (((Cardano.Ledger.Address.failDecoding
               @ (Control.Monad.Trans.State.Lazy.StateT
                       Int
                       (Control.Monad.Trans.Fail.FailT
                       [Char]
                       Data.Functor.Identity.Identity))
               @ (Hash.Hash
                       (CC.ADDRHASH
                       (EraCrypto
                           era_a3NLT))
                       (Cardano.Crypto.DSIGN.Class.VerKeyDSIGN
                       (CC.DSIGN
                           (EraCrypto
                               era_a3NLT))))
               (Cardano.Ledger.Address.fromCborAddr_$s$fMonadFailStateT
                   @ Int)
               (GHC.Base.build
                   @ Char
                   (\ (@ b_iHyc) ->
                       ghc-prim-0.6.1:GHC.CString.unpackFoldrCString#
                       @ b_iHyc
                       "Hash"#))

The ``False`` branch is essentially identical, but the ``True`` branch is in a
much better form. Notice that we still have the same type applications, but now
instead of a reference to ``dMonadFail1_isdA`` there is a call to a
*specialized* function:
``Cardano.Ledger.Address.fromCborAddr_$s$fMonadFailStateT``. From its name we
can conclude that this function is ``Cardano.Ledger.Address.fromCborAddr`` and
has been specialized to ``MonadFailStateT`` (also note the ``$s`` indicating
that it is a GHC generated specialized version).

We have enough data to craft a hypothesis: The lack of specialization of
``fromCborAddr`` on |new| is a major contributor to Core bloat, which in turn
causes the performance regression.

The functions we need to inspect are ``Cardano.Ledger.Address.decompactAddr``,
``Cardano.Ledger.Address.fromCborAddr``, and
``Cardano.Ledger.Address.failDecoding``. Here is their source:

.. code-block:: haskell

   module Cardano.Ledger.Address (
     -- * Compact Address
     ...
     decompactAddr,
     ...
     fromCborAddr,
     ...)

   decompactAddr :: forall c. (HasCallStack, Crypto c) => CompactAddr c -> Addr c
   decompactAddr (UnsafeCompactAddr sbs) =
     case runFail $ evalStateT (decodeAddrStateAllowLeftoverT True sbs) 0 of
       Right addr -> addr
       Left err ->
         error $
           "Impossible: Malformed CompactAddr was allowed into the system. "
             ++ " Decoder error: "
             ++ err
   {-# INLINE decompactAddr #-}

   fromCborBothAddr :: forall c s. Crypto c => Decoder s (Addr c, CompactAddr c)
   fromCborBothAddr = do
     ifDecoderVersionAtLeast (natVersion @7) decodeAddrRigorous fromCborBackwardsBothAddr
     where
       decodeAddrRigorous = do
         sbs <- decCBOR
         flip evalStateT 0 $ do
           addr <- decodeAddrStateAllowLeftoverT False sbs
           pure (addr, UnsafeCompactAddr sbs)
       {-# INLINE decodeAddrRigorous #-}
   {-# INLINE fromCborBothAddr #-}


   failDecoding :: MonadFail m => String -> String -> m a
   failDecoding name msg = fail $ "Decoding " ++ name ++ ": " ++ msg
   {-# NOINLINE failDecoding #-}

Note that ``decompactAddr`` and ``fromCborAddr`` are in the export list of the
module but ``failDecoding`` is not. Typically GHC will be very good at
optimizing module local functions such as ``failDecoding``, however
``failDecoding`` is marked ``NOINLINE`` which interacts with GHC's optimizer
[#]_. Similarly, ``decompactAddr`` and ``fromCborAddr`` should be marked
``INLINEABLE`` rather than ``INLINE`` because they are in the export list. This
will gives GHC the opportunity to specialize these functions *in other modules*,
rather than marking them as cheap to inline. If we know exactly which types will
be used (which in this case we do), then we can issue a ``SPECIALIZE`` pragma
for the same effect and with less load on GHC [#]_.

From ``decompactAddr`` we can see that ``MonadFailStateT`` is likely
coming from ``decodeAddrStateAllowLeftoverT`` due to ``decompactAddr`` calling
``runFail`` and ``evalStateT``.

First, we'll remove the ``NOINLINE`` and see if this alters the Core of
``addrTxOutL`` to be closer to the Core produce by |old|:

.. code-block:: haskell

   -- no NOINLINE pragma now
   failDecoding :: MonadFail m => String -> String -> m a
   failDecoding name msg = fail $ "Decoding " ++ name ++ ": " ++ msg

which results in this Core, we'll still be looking at the unfolding because
``addrTxOutL`` in marked INLINE:

.. code-block:: haskell

   -- RHS size: {terms: 635, types: 635, coercions: 186, joins: 13/21}
   Cardano.Ledger.Core.$dmaddrTxOutL [InlPrag=INLINE (sat-args=0)]
     :: forall era.
        EraTxOut era =>
        Lens' (TxOut era) (Addr (EraCrypto era))
   [GblId,
    Arity=1,
    Str=<LP(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,MP(MP(MP(A,MP(L,A,A,A),A,A,A,A,A,A,A),A,A,A,A,A,A,A,A,A,A,A),A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A),A,A,A,A,A,A,A,LCL(C1(C1(L))),A,A)>,
    Unf=Unf{Src=Compulsory, TopLvl=True, Value=True, ConLike=True,
            WorkFree=True, Expandable=True,
            Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
            Tmpl= \ (@era_a2bTm) ($dEraTxOut_a2hQj :: EraTxOut era_a2bTm) ->
    ...
    lens
        @(TxOut era_a2bTm)
        @(Addr (EraCrypto era_a2bTm))
        @(Addr (EraCrypto era_a2bTm))
        @(TxOut era_a2bTm)
        (\ (txOut_a2bTy [Occ=Once1] :: TxOut era_a2bTm) ->
        case (addrEitherTxOutL
                @era_a2bTm
                $dEraTxOut_a2hQj
        ...
        of {
            Left addr_a2bTz [Occ=Once1] -> addr_a2bTz;
            Right cAddr_a2bTA ->
            let {
                header_i2jrF :: GHC.Word.Word8
                [LclId]
                header_i2jrF
                = cardano-prelude-0.1.0.2-Ww24WREo4wCDwR8SsfBcg:Cardano.Prelude.Compat.ByteString.Short.unsafeShortByteStringIndex
                    (cAddr_a2bTA
                        `cast` (Cardano.Ledger.Address.N:CompactAddr[0]
                                    <EraCrypto era_a2bTm>_P
                                :: CompactAddr (EraCrypto era_a2bTm)
                                ~R# Data.ByteString.Short.Internal.ShortByteString))
                    (ghc-prim:GHC.Types.I# 0#) } in
            case cAddr_a2bTA
            ...

            case ghc-prim:GHC.Classes.geInt
                    ww_i2jsu (ghc-prim:GHC.Types.I# 0#)
            of {
            False ->
                jump $j_i2jrH
                (GHC.Base.build
                    @[Char]
                    (\ (@a3_i2jsB)
                        (c1_i2jsC [Occ=Once1!,
                                    OS=OneShot]
                            :: [Char]
                            -> a3_i2jsB -> a3_i2jsB)
                        (n_i2jsD [Occ=Once1,
                                OS=OneShot]
                            :: a3_i2jsB) ->
                        c1_i2jsC
                        (GHC.Base.build
                            @Char
                            (\ (@b_i2jsE) ->
                                ghc-prim:GHC.CString.unpackFoldrCString#
                                @b_i2jsE
                                "Impossible: Negative offset"#))
                        n_i2jsD));
            True ->
                jump $j_i2jrH
                (GHC.Base.build
                    @[Char]

Just by removing ``NOINLINE`` we have reduced the Core of ``addrTxOutL`` to 635
terms. The Core is also better optimized: now both the ``True`` and ``False``
branch jump to the join point ``$j_i2jrH``. Let's check the Core summary for
``Cardano.Ledger.Address``:

.. code-block:: haskell

   ==================== Tidy Core ====================
   2023-09-22 14:45:53.829768066 UTC

   Result size of Tidy Core
     = {terms: 50,443, types: 53,109, coercions: 19,256, joins: 92/906}

Just from one change we have shaved off 10,000 terms. We can still go further;
recall that on |old| ``fromCborAddr`` was specialized but was not on ``new``.
Let's revert the ``NOINLINE`` and see how the Core reacts to specializing
``fromCborAddr``.

output on |old| because ``failDecoding`` is still marked ``NOINLINE``. Let's run
a microbenchmark to see if this change is reflected in time.

|old|:

.. code-block:: console

   [nix-shell:~/cardano-ledger]$ cabal bench cardano-ledger-core:addr
   Build profile: -w ghc-8.10.7 -O1
   In order, the following will be built (use -v for more details):
    - cardano-ledger-core-1.7.0.0 (lib) (file src/Cardano/Ledger/Core.hs changed)
    - cardano-ledger-core-1.7.0.0 (lib:testlib) (dependency rebuilt)
    - cardano-ledger-core-1.7.0.0 (bench:addr) (dependency rebuilt)
   Preprocessing library for cardano-ledger-core-1.7.0.0..
   Building library for cardano-leRunning 1 benchmarks...
   Benchmark addr: RUNNING...
   benchmarking decodeAddr (500)
   time                 236.6 μs   (235.2 μs .. 238.5 μs)
                       1.000 R²   (0.999 R² .. 1.000 R²)
   mean                 236.8 μs   (235.4 μs .. 238.4 μs)
   std dev              5.308 μs   (4.187 μs .. 6.549 μs)

   benchmarking decodeAddr (1000)
   time                 419.6 μs   (418.4 μs .. 420.9 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
   mean                 421.1 μs   (420.1 μs .. 422.5 μs)
   std dev              3.992 μs   (2.800 μs .. 6.338 μs)

   Benchmark addr: FINISH

|new| with ``failDecoding`` marked ``NOINLINE``:

|new| without ``failDecoding`` marked ``NOINLINE``:

.. code-block:: console

   [nix-shell:~/cardano-ledger]$ cabal bench cardano-ledger-core:addr
   Build profile: -w ghc-9.2.8 -O1
   Preprocessing benchmark 'addr' for cardano-ledger-core-1.7.0.0..
   Building benchmark 'addr' for cardano-ledger-core-1.7.0.0..
   Running 1 benchmarks...
   Benchmark addr: RUNNING...
   benchmarking decodeAddr (500)
   time                 239.6 μs   (237.3 μs .. 241.8 μs)
                        0.999 R²   (0.999 R² .. 1.000 R²)
   mean                 238.7 μs   (237.6 μs .. 239.7 μs)
   std dev              3.677 μs   (3.146 μs .. 4.308 μs)

   benchmarking decodeAddr (1000)
   time                 474.3 μs   (472.2 μs .. 476.7 μs)
                        1.000 R²   (0.999 R² .. 1.000 R²)
   mean                 484.8 μs   (481.1 μs .. 490.7 μs)
   std dev              14.56 μs   (11.85 μs .. 17.14 μs)

   Benchmark addr: FINISH








..
   Notice that the function is polymorphic in ``EraTxOut``.
   This type class gives
   access to ``addrTxOutL`` which is used in a lens in this line: ``in case out ^.
   addrTxOutL``.

.. [#] As stated by ``Note [INLINE and default methods]`` in GHC source.
       Default methods are a special case that are always ``Compulsory``.
.. [#] See `This issue <https://gitlab.haskell.org/ghc/ghc/-/issues/22629>`_, in
       fact there are many issues edge cases in the intersection of ``NOINLINE``
       and the optimizer, most notably `it prevents
       <https://gitlab.haskell.org/ghc/ghc/-/issues/21458>`_, `specialization
       <https://gitlab.haskell.org/ghc/ghc/-/issues/22629>`_.
.. [#] See :ref:`INLINE vs INLINEABLE vs NOINLINE <Inline Chapter>`. `Mark
       Karpov <https://markkarpov.com/>`_ also has a great `post
       <https://markkarpov.com/tutorial/ghc-optimization-and-fusion.html#specializing>`_
       on this subtle dance.

.. [#] As stated by Sebastian Graf. See his `Zurihac 2023 <https://youtu.be/DiKjWl9xnvw?si=AZcylREakQfq6yzm&t=3218>`_
       .
