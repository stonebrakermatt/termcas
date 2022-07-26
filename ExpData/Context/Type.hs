{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - File defining a context type to store
 - assigned variables and functions while the
 - program runs -}
module ExpData.Context.Type where
import Data.Map
import qualified ExpData.Dependency.Type as D
import qualified ExpData.Expression.Type as E





{- Context types and the empty context -}
type ContextEntry = (E.Expression, E.Expression)
type Context = Map D.ExpressionDependency ContextEntry
empty_context :: Context
empty_context = empty

{- Simple helper functions -}
context_elems :: Context -> [ContextEntry]
context_elems context = elems context
show_context_entry :: ContextEntry -> [Char]
show_context_entry entry = show (fst entry) ++ " = " ++ show (snd entry)
