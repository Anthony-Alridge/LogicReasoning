-- '&&' and
-- '||' or
-- '~' not
-- '->' impplication
-- '<->' double implication
-- '|-' single turnstile
import Data.Char

data Truth a
  = Known a | Assumed a | Unknown

data Formula
  = Atom Char |
    Not Formula |
    And Formula Formula |
    Or Formula Formula |
    If Formula Formula |
    Iff Formula Formula |
    Conclusion Formula

  deriving (Eq, Ord)

instance Show Formula where
  show (Atom f)       = show f
  show (Not f)        = "(" ++ " ~" ++ show f ++")"
  show (Conclusion f) = "(" ++ " |- " ++ show f ++")"
  show (And f f')     = "(" ++ (show f) ++ " && "  ++ show f' ++ ")"
  show (Or f f')      = "(" ++ (show f) ++ " || "  ++ show f' ++ ")"
  show (If f f')      = "(" ++ (show f) ++ " -> "  ++ show f' ++ ")"
  show (Iff f f')     = "(" ++  (show f) ++ " <-> " ++ show f' ++ ")"

--Takes a string, returns a formula if it succeeds, and the rest of the
--string ready to parsed.
newtype Parser a = Parser {parse :: String -> [(a, String)]}

instance Monad Parser where
  return v =
    Parser $ \inp -> [(v, inp)]
  (>>=) p f =
    Parser $
    \inp -> [(r, s) | (v, inp') <- parse p inp, (r,s) <- parse (f v) inp']


removeWhitespace :: String -> String
removeWhitespace inp = filter (not . isSpace) inp

----BASIC COMBINATORS TO PROVIDE BUILDING BLOCKS----

zero :: Parser a
zero = Parser $ \_ -> []

item :: Parser Char
item = Parser item'
    where item' [] = []
          item' (a : x) = [(a, x)]

sat :: (Char -> Bool) -> Parser Char
sat p = do
          x <- item
          if p x then return x else zero

plus :: Parser a -> Parser a -> Parser a
m `plus` n = Parser $ \s -> parse m s ++ parse n s

bchoice :: Parser a -> Parser a -> Parser a
m `bchoice` n = Parser $ \s -> if not (null (parse m s)) then parse m s else parse n s

sepby :: Parser a -> Parser b -> Parser [a]
sepby p sep = do
  x <- p
  xs <- reiterate $
    do
      sep
      y <- p
      return y
  return $ x:xs
lower :: Parser Char
lower = sat $ \c -> 'a' <= c && c <= 'z'

upper :: Parser Char
upper = sat $ \c -> 'A' <= c && c <= 'Z'

letter :: Parser Char
letter = lower `plus` upper

lit :: Char -> Parser Char
lit c = sat $ \x -> x == c

stringLit :: String -> Parser String
stringLit s = mapM lit s

reiterate :: Parser a -> Parser [a]
reiterate m = multiple `bchoice` return []
    where multiple = do
            t <- m
            ts <- reiterate m
            return $ t : ts


chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  res <- rest x
  return res
  where
    rest item = (do
      f <- op
      y <- p
      rest (f item y)) `bchoice` (return item)

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do
  open
  x <- p
  close
  return x

-----ACTUALLY PARSING ------
atom :: Parser Formula
atom = do
  t <- letter
  return (Atom t)

not' :: Parser Formula
not' = do
  lit '~'
  a <- atom
  return (Not a)

conclusion :: Parser Formula
conclusion = do
  stringLit "|-"
  a <- formula
  return (Conclusion a)

and' :: Parser (Formula -> Formula -> Formula)
and' = do
  stringLit "&&"
  return And

or' :: Parser (Formula -> Formula -> Formula)
or' = do
  stringLit "||"
  return Or

if' :: Parser (Formula -> Formula -> Formula)
if' = do
  stringLit "->"
  return If
iff :: Parser (Formula -> Formula -> Formula)
iff = do
  stringLit "<->"
  return Iff

formula' :: Parser Formula
formula' = bchoice (bchoice atom not') (bracket (lit '(') formula (lit ')') )

formula :: Parser Formula
formula = foldr (flip chainl1) formula' [iff, if', or', and']

parseSingleFormula :: String -> Formula
parseSingleFormula inp = fst $ head (parse formula (removeWhitespace inp))

argument :: Parser [Formula]
argument = sepby formula (lit ',')

parseargument :: String -> [Formula]
parseargument inp = fst $ head (parse argument (removeWhitespace inp))
