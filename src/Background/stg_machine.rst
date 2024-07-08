.. _stg_machine:

The Spineless Tagless G-machine
===============================

In the previous chapter we argued that in order to debug, one must
:ref:`Understand the System`. In pursuit of this, this chapter presents the
Spineless Tagless G-machine (STG machine), as described by :cite:t:`fastCurry`.
We focus on presenting the abstract machine and how it is mapped to hardware,
that is, its operational semantics. The STG machine is unique in that it also
has a precise denotational semantics defined in the original paper
(:cite:t:`jones1992implementing`). Understanding the STG machine is required
background for all who are serious about writing fast, highly optimized Haskell
since by understanding the machine one can understand how their code is executed
on the underlying hardware. Furtheremore, knowing the operational semantics of
the STG machine will enable one to understand the upcoming chapters on
:ref:`Reading Cmm` and :ref:`Reading STG` and ....

The Abstract Model
------------------

.. todo::
   cite the g-machine paper by Hudak I think?

The spineless tagless G-machine is the abstract machine that GHC Haskell
executes. Each backend that GHC supports encodes an STG machine, be it the
native code generator, the Javascript or WASM backends, or the LLVM backend,
they all encode an Stg machine and use it to execute Haskell programs. We'll
begin by presenting the invariants of the machine which are key to understanding
its semantics.

Machine Invariants
^^^^^^^^^^^^^^^^^^

The Stg machine enforces:

1. All function and constructor arguments are variables references or constants.
   We call these arguments *Atomic*.

2. All constructors and built-in operator applications are :term:`saturated`.

3. Pattern matching *is only* performed by ``case`` expressions, and the
   patterns must be simple, single-level patterns. Note that the
   :term:`scrutinee` of a case expression is not restricted to be an atom and
   can be any expression.

4. All bindings are done with a special binding form called a *lambda form*.
   Operationally this means that each and every ``let`` expression performs heap
   allocation. This is one reason why :ref:`Reading Stg` is attractive when
   analyzing performance of a program.

Abstract Syntax
---------------

Here is the abstract syntax of the Stg machine. This is a denotational
presentation:

.. code-block:: haskell

   type Literals = [Literal]

   -- | literals or constants, we just us an Int for show. In reality, there are
   -- many kinds of literal expressions such as Doubles and Words.
   newtype Literal = MkLiteral { unLiteral :: Int }
     deriving newtype (Eq, Show)

   type Variables      = [Variable]
   type LocalVariables = [Variable] -- ^ convenience synonym to differentiate local
                                    -- variables from free variables
   type FreeVariables  = [Variable] -- ^ same as @LocalVariables@

   -- | Our representation for variables is just a String.
   newtype Variable = MkVariable {unVariable :: String}
     deriving newtype (Eq, Show)

   type Atoms = [Atom]

   -- | an Atom is either a variable or a literal
   data Atom = V Variable | L Literal
     deriving stock (Eq, Show)

   -- | Similar to @Variables@ we use a @String@ to represent constructors.
   newtype Constructor = MkConstructor {unConstructor :: String}
     deriving newtype (Eq, Show)

   -- | Update flags are attached to every @LambdaForm@ and therefore to every heap
   -- object (also called a closure in the literature) allocated by a @Let@ or
   -- @LetRec@.
   data UpdateFlag = Updatable | SingleEntry | ReEntrant
     deriving stock (Eq, Show)

   -- | Primitive operations, we keep this list small just for demonstration. In
   -- reality, GHC has numerous primitive operations
   data PrimOp = PrimAdd | PrimMult | PrimSub | PrimDiv
     deriving stock (Eq, Show)

   -- | Lambda-forms track .the local and free variables in closure (which is the
   -- body of the let, the @Expression@ in this definition) and whether the closure
   -- should be updated if its evaluated.
   data LambdaForm = LocalVariables UpdateFlag FreeVariables Expression
     deriving stock (Eq, Show)

   type Alternatives = [Alternative]

   -- | Alternatives in a case-expression can either be algebraic, matching on data
   -- constructors, or they can match on a primitive operation.
   data Alternative = Alg  [AlgebraicAlt] DefaultAlt
                    | Prim [PrimAlt]      DefaultAlt
     deriving stock (Eq, Show)


   data AlgebraicAlt = AAlt Constructor Variables Expression
     deriving stock (Eq, Show)

   data PrimAlt = PAlt Literal Expression
     deriving stock (Eq, Show)

   data DefaultAlt = DAlt Variable Expression
     deriving stock (Eq, Show)

   data Expression = Literal        Literal
                   | Application    Variable    Atoms
                   | SatConstructor Constructor Variables
                   | BuiltInOp      PrimOp      Atoms
                   | Let            Bindings    Expression
                   | Case           Expression  Alternatives
     deriving stock (Eq, Show)

   -- | unlike an imperative program, where a program is a sequence of statements.
   -- In Stg, a program is a sequence of bindings.
   newtype Program = Program { unProgram :: Bindings }
     deriving newtype (Eq, Show)

   type Bindings = [LambdaForm]


NEXT: reduce the size of the code, and show the operational semantics not the
denotational semantics



Variables
^^^^^^^^^

We'll represent variables as a ``String``:

.. code-block:: haskell

   data Variable = String




What are Values
---------------

What is a Closure
-----------------

Heap Objects
------------

Tagging Heap Objects
--------------------

The Representation of Heap Objects
----------------------------------

What Does it Mean to Enter a Closure
------------------------------------

How The Machine Runs
--------------------

Known and Unknown Functions
---------------------------

Stg Apply Functions
-------------------

Tagging Closures
----------------
