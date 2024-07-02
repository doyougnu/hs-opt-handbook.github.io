{-# LANGUAGE DerivingStrategies #-}
module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- | literals or constants, we just us an Int for show. In reality, there are
-- many kinds of literal expressions such as Doubles and Words.
newtype Literal = MkLiteral { unLiteral :: Int }
  deriving newtype (Eq, Show)

-- | Our representation for variables is just a String.
newtype Variable = MkVariable {unVariable :: String}
  deriving newtype (Eq, Show)

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

-- | Lambda-forms track the local and free variables in closure (which is the
-- body of the let, the @Expression@ in this definition) and whether the closure
-- should be updated if its evaluated.
data LambdaForm = MkLambdaForm
                  [Variable]      -- ^ local variables
                  UpdateFlag      -- ^ update flag
                  [Variable]      -- ^ free variables
                  Expression      -- ^ body
  deriving stock (Eq, Show)

-- | Alternatives in a case-expression can either be algebraic, matching on data
-- constructors, or they can match on a primitive operation.
data Alternative = Alg  [AlgebraicAlt] DefaultAlt
                 | Prim [PrimAlt]      DefaultAlt
  deriving stock (Eq, Show)

data AlgebraicAlt = AAlt Constructor [Variable] Expression
  deriving stock (Eq, Show)

data PrimAlt = PAlt Literal Expression
  deriving stock (Eq, Show)

data DefaultAlt = DAlt Variable Expression
  deriving stock (Eq, Show)

data Expression = Literal        Literal
                | Application    Variable    [Atom]
                | SatConstructor Constructor [Variable]
                | BuiltInOp      PrimOp      [Atom]
                | Let            Bindings    Expression
                | LetRec         Bindings    Expression
                | Case           Expression  [Alternative]
  deriving stock (Eq, Show)

type Bindings = [LambdaForm]

-- | unlike an imperative program, where a program is a sequence of statements.
-- In Stg, a program is a sequence of bindings.
newtype Program = Program { unProgram :: Bindings }
  deriving newtype (Eq, Show)
