module Resolve where
import Parser
import Data.List (nub, sort)
import Data.Maybe (fromJust)
{-data Formula
  = Atom Char |
    Not Formula |
    And Formula Formula |
    Or Formula Formula |
    If Formula Formula |
    Iff Formula Formula
    deriving (Eq, Ord)

instance Show Formula where
  show (Atom f)       = show f
  show (Not f)        = "(" ++ " ~" ++ show f ++")"
  show (And f f')     = "(" ++ (show f) ++ " && "  ++ show f' ++ ")"
  show (Or f f')      = "(" ++ (show f) ++ " || "  ++ show f' ++ ")"
  show (If f f')      = "(" ++ (show f) ++ " -> "  ++ show f' ++ ")"
  show (Iff f f')     = "(" ++  (show f) ++ " <-> " ++ show f' ++ ")"
-}
--TODO: Clauses should be sets not lists.
type Clause  = [Formula]
type Clauses = [Clause]

apply :: Formula -> Formula
--applys an equivalence to a formula which brings it nearer CNF form
apply (Iff f1 f2)
  = And (Or (Not f) f') (Or f (Not f'))
  where
    f  = apply f1
    f' = apply f2
apply (If f1 f2)
  = Or (Not (apply f1)) (apply f2)
apply (Not (Or f1 f2))
  = And (Not (apply f1)) (Not (apply f2))
apply (Not (And f1 f2))
  = Or (Not (apply f1)) (Not (apply f2))
apply (Not (Not f))
  = apply f
apply (And f0 (Or f1 f2))
  = Or (And f f') (And f f'')
  where
    f   = apply f0
    f'  = apply f1
    f'' = apply f2
apply (Or (And f1 f2) f0)
  = And (Or f' f) (Or f'' f)
  where
    f   = apply f0
    f'  = apply f1
    f'' = apply f2
apply (And f1 f2)
  | f1 == f2  = apply f1
  | otherwise = And (apply f1) (apply f2)
apply (Or f1 f2)
  | f1 == f2  = apply f1
  | otherwise = Or (apply f1) (apply f2)
apply (Not f)
  = Not (apply f)
apply f
  = f

cnfForm :: Formula -> Formula
--Repeatedly apply equivalences until formula is in CNF form
cnfForm f
  | f == f'   = f'
  | otherwise = cnfForm f'
  where
    f' = apply f

extractClauses :: Formula -> Clauses
--Pre: Formula is in CNF form
extractClauses (And f f')
  = nub ((extractClauses f) ++ (extractClauses f'))
extractClauses (Or f f')
  = [concat ((extractClauses f) ++ (extractClauses f'))]
extractClauses f
  = [[f]]



resolve :: Clause -> Clause -> Clause
--Pre: Clause only contains literals.
resolve c c'
  = removeContradictions (nub (c ++ c'))
  where
    removeContradictions [] = []
    removeContradictions (f:fs)
      | filtered == fs = f : removeContradictions fs
      | otherwise      = filtered
      where
        filtered = case f of
          (Not p) -> filter (/= p) fs
          p       -> filter (/= (Not p)) fs

valid :: [Formula] -> Formula -> Bool
--Determines whether an argument is valid using a naive implementation of the resolution algorithm
valid axioms (Conclusion c) = resolution clauses
  where
    clauses = nub (concatMap extractClauses (cnfForm (Not c) : (map cnfForm axioms)))
    resolution clauses'
      | elem [] resolved              = True
      | sort resolved == sort clauses' = False
      | otherwise                     = resolution (nub (resolved ++ clauses'))
      where
        resolved = nub [resolve f f' | f <- clauses, f' <- (filter (/= f) clauses')]
