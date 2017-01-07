# LogicReasoning
Haskell module for parsing logical arguments and validating theorems with natural deduction proofs.

The Parser module using monadic parser combinators to parse a string into a data representation. The following conventions are used: 

And -'&&' 

Or - '||'

Not - '~'

If - '->'

Iff (if and only if) - '<->'

Atoms are must be single characters.

Usage:
```Haskell
    formula = "p&&q"
    parseSingleFormula(formula)
    Output:
    And (Atom 'p') (Atom 'q')
    
