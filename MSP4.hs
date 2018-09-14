module MSP4 where

{-
This is like MSP2 and is based on §3.7 for “Controlled Inlining”.

If you load this into ghci and execute $$(run fact), it will stall.
Read the paper and fix this.
-}

import Language.Haskell.TH hiding (Exp) -- Metaprogramming support for Haskell
import Language.Haskell.TH.Syntax hiding (Exp)
import Prelude hiding (repeat)

type Code a = Q (TExp a)

data Exp  = Int Int
          | Var String
          | App String Exp
          | Add Exp Exp
          | Sub Exp Exp
          | Mul Exp Exp
          | Div Exp Exp
          | Ifz Exp Exp Exp
  deriving (Show)

-- “Declaration f x b” denotes “f = (λ x → b)”; i.e., top level items
data Declaration = Declaration { name :: String , parameter :: String , body :: Exp}
  deriving (Show)                 

-- The main method is the expression to be evaluated, given auxiliary definitions.
data Program = Program {supercombinators :: [Declaration] , mainMethod :: Exp}
  deriving (Show)

fact  :: Program
fact  = let 𝓍 = Var "x" in Program
  {supercombinators = [ Declaration { name      = "fact"
                                    , parameter = "x"
                                    , body      = Ifz        𝓍
                                                  {- then -} (Int 1)  
                                                  {- else -} (𝓍 `Mul`(App "fact" (𝓍 `Sub` Int 1)))
                                    }
                      ]
  , mainMethod      = App "fact" (Int 10)
  }

-- A simple interpreter

type VariableTable = String -> Code Int
type FunctionTable = String -> (Code Int -> Code Int) -- Unlike MSP2, "Code" between the arrow.

-- Function patching: f[x ≔ v] behaves like f but now goes to v at position x.
patch :: Eq a => (a -> b) -> a -> b -> (a -> b)
patch f x v = \y -> if x == y then v else f y

-- Main difference at case "App", from MSP2.
eval :: Exp -> VariableTable -> FunctionTable -> Code Int
eval (Int i) env fenv = [|| i ||]
eval (Var v) env fenv = env v
eval (App name  exp) env fenv = (fenv name) (eval exp env fenv)  -- Only difference from MSP2.
eval (Add e1 e2) env fenv     = [|| $$(eval e1 env fenv)  + $$(eval e2 env fenv) ||]
eval (Sub e1 e2) env fenv   = [|| $$(eval e1 env fenv)  - $$(eval e2 env fenv) ||]
eval (Mul e1 e2) env fenv   = [|| $$(eval e1 env fenv)  * $$(eval e2 env fenv) ||]
eval (Div e1 e2) env fenv   = [|| $$(eval e1 env fenv)  `div` $$(eval e2 env fenv) ||]
eval (Ifz c e1 e2) env fenv = [|| if   $$(eval c  env fenv) == 0
                                  then $$(eval e1 env fenv)
                                  else $$(eval e2 env fenv)
                               ||]

-- repeat n f = f^n = f.f.f...f  (n-many times)
repeat :: Int -> (a -> a) -> (a -> a)
repeat 0 f = id
repeat n f = f . repeat (n-1) f

-- Lift a declaration into a function
deval :: Declaration -> VariableTable -> FunctionTable -> (Code Int -> Code Int) -- Again, typing changed from MSP2
deval (Declaration name var body) env fenv =
  let
    ff        :: (Code Int -> Code Int) -> Code Int -> Code Int
    ff that x = eval body (patch env var x) (patch fenv name that)
    -- Evaluate “body[var, name ≔ x, that]”;
    -- i.e., replace strings with actual values and functions then simply “body”.
    
    this      :: Code Int -> Code Int
    this x    = ((repeat 0 ff) this) x
    {- i.e.,
        λ x → “body[var, name ≔ x, λ y → body[var, name ≔ y, this]]”;
        i.e., we evaluate “body” where references to “name” go to the function
        that again evaluates “body” and its references to “name” invoke a recursive call.

        That is, every occurrence of “name” in “body” is unfolded once, rather than a direct call.
        Hence, if “name = λvar → body” is interpreted as “name = λ var → body[name ≔ λ y → body[x ≔ y]]”.

        Therefore, only one function call is needed for every two iterations.
    -}
  in
    this

peval :: Program -> VariableTable -> FunctionTable -> Code Int
peval (Program []     exp) env fenv = eval exp env fenv
peval (Program (d:ds) exp) env fenv = peval (Program ds exp) env fenv'
  where df    = deval d env fenv
        fenv' = patch fenv (name d) df

-- Usage: $$(run p)
run :: Program -> Code Int
run p = peval p emptyEnv emptyEnv
  where emptyEnv = \x -> error ("Yikes! The name " ++ x ++ " is not defined")

fact_direct :: Int
fact_direct = let f n = if n == 0 then 1 else n * f (n - 1) in f 10
