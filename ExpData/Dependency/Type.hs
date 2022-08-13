{- TermCAS
 - v0.1.0
 - (c) 2022 Matt Stonebraker
 -
 - File defining an ordered dependency type. Since
 - they are ordered, we use them as keys in the
 - context map -}
module ExpData.Dependency.Type where





{- Dependency types -}
data DependencyType
    = V
    | F Int
    | S
    deriving (Read, Eq)
type Dependency = ([Char], DependencyType)

{- Implementing show and an ordering function -}
instance Show DependencyType where
    show V = "Variable"
    show (F 1) = "Function of 1 argument"
    show (F n) = "Function of " ++ show n ++ " arguments"
    show S = "Set"
instance Ord DependencyType where
    V <= _ = True
    F n <= F m = n <= m
    S >= _ = True