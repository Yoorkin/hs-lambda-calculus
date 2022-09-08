module Parser(
    Stmt(..),
    Expression(..),
    parseLambda
) where

import Text.Parsec
import Text.Parsec.Token

data Stmt = Dsp Expression
          | Let String Expression
          | Eval Expression
          deriving(Show)

data Expression = Abs String Expression
                | Term String
                | App Expression Expression
                deriving(Eq,Ord)

instance Show Expression where
    show (Abs v e) = "(λ" ++ v ++ "." ++ show e ++ ")"
    show (Term v) = v
    show (App xs x) = "(" ++ show xs ++ " " ++ show x ++ ")"

stmtParser, letParser, displayParser :: Parsec String st Stmt
stmtParser = letParser <|> displayParser <|> (Eval <$> expParser)
displayParser = Dsp <$> (string "dsp" *> expParser)
letParser = string "let" *> (Let <$> (spaces *> idParser <* spaces) <*> (char '=' *> expParser))


expParser, absParser, eleParser, appParser, parensParser, varParser :: Parsec String st Expression
expParser = try appParser <|> eleParser
eleParser = spaces *> (absParser <|> varParser <|> parensParser) <* spaces
absParser = (char '/' <|> char 'λ') *> (Abs <$> idParser <*> (char '.' *> expParser))
varParser = Term <$> idParser
idParser = many1 (noneOf " \\.()")
appParser = warp <$> ((:) <$> eleParser <*> many1 eleParser)

warp :: [Expression] -> Expression
warp [x] = x
warp xs = App (warp $ init xs) (last xs)

parensParser = between (char '(') (char ')') expParser 

parseLambda :: String -> Either ParseError Stmt
parseLambda = parse stmtParser ""
