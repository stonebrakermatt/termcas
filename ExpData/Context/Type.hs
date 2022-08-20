
{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - File defining a context type to store
 - assigned variables and functions while the
 - program runs -}
module ExpData.Context.Type where
import qualified Data.Map as Map
import qualified ExpData.Expression.Type as Exp





{- Context types and the empty context -}
type ContextEntry = Either (Exp.Expression, Exp.Expression) (Exp.Set, Exp.Set)
type Context = Map.Map Dependency ContextEntry

{- Empty context -}
empty_context :: Context
empty_context = Map.empty

{- Simple helper functions -}
context_elems :: Context -> [ContextEntry]
context_elems context = Map.elems context
show_context_entry :: ContextEntry -> [Char]
show_context_entry entry = case entry of 
    Left exp -> show (fst exp) ++ " = " ++ show (snd exp)
    Right set -> show (fst set) ++ " = " ++ show (snd set)



{- Dependency types -}
data DependencyTree 
    = Empty
    | Leaf Dependency
    | Branch Dependency [DependencyTree]
    deriving (Show, Read, Eq)
data DependencyType
    = V
    | F Int
    | S
    deriving (Read, Eq)
type Dependency = ([Char], DependencyType)

{- Implementing show and an ordering function -}
instance Show DependencyType where
    show V = "variable"
    show (F 1) = "function of 1 argument"
    show (F n) = "function of " ++ show n ++ " arguments"
    show S = "set"
instance Ord DependencyType where
    V <= _ = True
    F n <= F m = n <= m
    F n <= V = False
    _ <= S = True