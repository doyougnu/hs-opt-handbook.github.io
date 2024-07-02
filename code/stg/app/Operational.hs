{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.HashMap.Lazy as HM
import Data.Hashable
import Data.List

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- | literals or constants, we just us an Int for show. In reality, there are
-- many kinds of literal expressions such as Doubles and Words.
newtype Literal = MkLiteral { unLiteral :: Int }
  deriving newtype (Eq, Show)

-- | Our representation for variables is just a String.
newtype Variable = MkVariable {unVariable :: String}
  deriving newtype (Eq, Show, Hashable, Semigroup)

-- | an Atom is either a variable or a literal
data Atom' a = V a | L Literal
  deriving stock (Eq, Show, Functor)

type Atom = Atom' Variable

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

-- | Lambda-forms track the local and free variables in closure (which is the
-- body of the let, the @Expression@ in this definition) and whether the closure
-- should be updated if its evaluated.
data LambdaForm = MkLambdaForm
                  [Variable]      -- ^ local variables
                  UpdateFlag      -- ^ update flag
                  [Variable]      -- ^ free variables
                  Expression      -- ^ body
  deriving stock (Eq, Show)

type Arity = Int

data HeapObject = BlackHole                 -- ^ only created and used during an evaluation
                | Thunk Expression          -- ^ a thunk
                | Constructor Constructor [Atom]        -- ^ all constructors are saturated
                | Pap Variable [Atom]       -- ^ partially applied function
                | Function Arity [Variable] Expression  -- ^ A function object
  deriving stock (Eq, Show)

isValue :: HeapObject -> Bool
isValue Function{}    = True
isValue Pap{}         = True
isValue Constructor{} = True
isValue _             = False

isThunk :: HeapObject -> Bool
isThunk Thunk{} = True
isThunk _       = False

isFunction :: HeapObject -> Bool
isFunction Function{} = True
isFunction _          = False

isPap :: HeapObject -> Bool
isPap Pap{} = True
isPap _     = False

isPapOrFun :: HeapObject -> Bool
isPapOrFun x = isPap x || isFunction x

constructorName :: HeapObject -> Constructor
constructorName (Constructor c _) = c
constructorName _                 = error "constructorName called on non-constructor"

constructorAtoms :: HeapObject -> [Atom]
constructorAtoms (Constructor _ a) = a
constructorAtoms _                 = error "constructorName called on non-constructor"

-- | Alternatives in a case-expression can either be algebraic, matching on data
-- constructors, or they can match on a primitive operation.
type Alternatives  = Alternatives' Expression
data Alternatives' a = Alg  [AlgebraicAlt' a] DefaultAlt
  deriving stock (Eq, Show, Functor)

type AlgebraicAlt = AlgebraicAlt' Expression
data AlgebraicAlt' a = AAlt Constructor [Variable] a
  deriving stock (Eq, Show, Functor)

type DefaultAlt = Expression

data Expression = Atom        Atom
                | Application Variable    [Atom]        -- ^ arguments >= 1
                | BuiltInOp   PrimOp      [Atom]
                | Let         Variable HeapObject  Expression
                -- | LetRec      Variable HeapObject  Expression
                | Case        Expression  Alternatives
  deriving stock (Eq, Show)

type Bindings = [LambdaForm]

-- | unlike an imperative program, where a program is a sequence of statements.
-- In Stg, a program is a sequence of bindings.
newtype Program = Program { unProgram :: Bindings }
  deriving newtype (Eq, Show)

altConName :: AlgebraicAlt' a -> Constructor
altConName (AAlt c args e) = c

----------------------------- Utilities ----------------------------------------
substitute :: Variable -> Atom -> Expression -> Expression
substitute old new e@(Atom (V v))
  | v == old  = Atom new
  | otherwise = e
substitute old (V new) e@(Application fun_name arguments)
  | fun_name == old = Application new  arguments
  | otherwise = e
substitute old (L new) e@(Application fun_name arguments)
  = error "substituting a function name with a literal"
substitute old new@V{}  (BuiltInOp op arguments) = BuiltInOp op $ replace arguments
  where replace []       = []
        replace (V a:as) | a == old = new : replace as
        replace (L a:as) = (L a) : replace as
substitute old new (Let binder bindee body) =
  Let binder bindee (substitute old new body)
substitute old new (Case scrut alts) =
  Case (substitute old new scrut) (fmap (substitute old new) alts)

massSubstitute :: [Variable] -> [Atom] -> Expression -> Expression
massSubstitute [] [] acc       = acc
massSubstitute (v:vs) (a:as) e = massSubstitute vs as (substitute v a e)
massSubstitute _  _ _          = error "var list and atom list unequal sizes"

isVariable :: Atom -> Bool
isVariable (V _) = True
isVariable _     = False

isLiteral :: Atom -> Bool
isLiteral = not . isVariable

----------------------------- The Machine --------------------------------------

data StackKont = InScrut Alternatives
               | InUpdThunk Variable -- ^ Variable is thunk name
               | InFunApp [Atom]

type Stack = [StackKont]

push :: StackKont -> Stack -> Stack
push = (:)

type Heap = HM.HashMap Variable HeapObject

allocate :: Variable -> HeapObject -> Heap -> Heap
allocate v o h = HM.insert v o h

dereference :: Variable -> Heap -> HeapObject
dereference v h = (HM.!) h v

data State = State { code  :: Expression
                   , stack :: Stack
                   , heap  :: Heap
                   }

step :: State -> State
-- let
step (State (Let bndr bnde body) stack heap) = State body stack new_heap
  where new_heap = HM.insert bndr bnde heap
-- ret
step (State a@(Atom e) (InScrut alts : stack) heap) =
  case e of
  L _ -> State (Case a alts) stack heap
  -- if e is not a literal than its a variable
  V v ->
    let new_scrut = dereference v heap
    in if isValue new_scrut
       then State (Case (Atom (V v)) alts) stack heap
  -- this case should be impossible
       else error "the machine failed!!"
-- case rules
step (State (Case (Atom (V v)) (Alg cons dflt)) stack heap) =
  let
    obj = dereference v heap
    obj_must_be_con = constructorName obj
    atoms           = constructorAtoms obj
  in case find (\x -> obj_must_be_con == altConName x) cons of
    -- case con rule
    Just (AAlt _con args body) ->
      let body' = massSubstitute args atoms body
      in State body' stack heap
    -- case any rule, no match, choose default
    Nothing   -> State dflt stack heap
-- case
step (State (Case scrut alts) stack heap) = State scrut new_stack heap
  where new_stack = push (InScrut alts) stack
-- Thunk
step (State (Atom (V e)) stack heap)
  | isThunk $ dereference e heap =
    case dereference e heap of
    Thunk entry -> step (State entry (InUpdThunk e : stack) (allocate e BlackHole heap))
    _           -> error "impossible"
-- update
step (State e@(Atom (V y)) (InUpdThunk x : stack) heap)
  | isValue $ dereference y heap =
    let y_value  = dereference y heap
        new_heap = allocate x y_value heap
    in step (State e stack new_heap)
-- knowncall, and perfectly saturated
-- this also is the exact rule when the function is an unknown function
step (State (Application f atoms) stack heap)
  | isFunction $ dereference f heap =
    let (Function arity args old_body) = dereference f heap
        body = massSubstitute args atoms old_body
    in step (State body stack heap)
-- primop
step (State (BuiltInOp op atoms) stack heap) = error "todo primops"
  -- case op of
  --   PrimAdd  ->
  --   PrimMult ->
  --   PrimSub  ->
  --   PrimDiv  ->
-- oversaturated, callk
step (State (Application f atoms) stack heap)
  | isFunction $ dereference f heap =
    let Function arity args old_body = dereference f heap
        num_args                 = length atoms
    in if num_args > arity
       then -- we are oversaturated, rule callk
         let body      = massSubstitute args (take arity atoms) old_body
             leftovers = drop arity atoms
         in step (State body (InFunApp leftovers : stack) heap)
       else -- we are undersaturated, rule pap2
         let pap      = Pap f atoms
             p        = f <> MkVariable "_pap"
             new_heap = allocate p pap heap
         in step (State (Atom (V p)) stack new_heap)
-- thunk calls, rule tcall
step (State (Application f atoms) stack heap)
  | isThunk $ dereference f heap = step (State (Atom (V f)) (InFunApp atoms: stack) heap)
-- pap calls, rule pcall
step (State (Application f atoms) stack heap)
  | isPap $ dereference f heap =
    let Pap g pap_atoms = dereference f heap
    in step (State (Application g (pap_atoms <> atoms)) stack heap)
-- retfun
step (State (Atom (V f)) (InFunApp atoms : stack) heap)
  | isPapOrFun $ dereference f heap = step (State (Application f atoms) stack heap)
