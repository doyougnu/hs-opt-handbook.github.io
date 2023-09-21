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

The regression is directly observable from the Core summary output that GHC
produces at the top of each Core file. Here is the Core summary on |new|:

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
because on can get ``$wfoo`` through the :ref:`Worker/Wrapper <Worker/Wrapper>`
optimization), while ``dm`` means that this function is a *default method* of a
type class.

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

The Core summary shows a blow up of exactly 400 more terms on |new| (1,058)
compared to |old| (658). Note that I am showing a bit of the unfoldings (the
``Unf`` record, and specifically the ``Tmpl=`` field) for reasons that will soon
be apparent.

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
only have two, and the only two are values not type class dictionaries.







..
   Notice that the function is polymorphic in ``EraTxOut``.
   This type class gives
   access to ``addrTxOutL`` which is used in a lens in this line: ``in case out ^.
   addrTxOutL``.

.. [#] As stated by ``Note [INLINE and default methods]`` in GHC source.
       Default methods are a special case that are always ``Compulsory``.
