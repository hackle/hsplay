import Text.Parsec
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex
import Data.Functor.Identity

type Parser = Parsec String ()

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = ""
  , Tok.commentEnd      = ""
  , Tok.commentLine     = ""
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = letter
  , Tok.opStart         = oneOf "!#$%&*+-/<=>@^_.,;"
  , Tok.opLetter        = oneOf "!#$%&*+-/<=>@^_.,;"
  , Tok.reservedNames   = []
  , Tok.reservedOpNames = []
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

data Token = Token String | Brackets [Token] deriving (Show, Eq)

tokenz :: Parser Token
tokenz = idOrOp' <|> parens' where
    idOrOp' = id' <|> op' 
    id' = Token <$> Tok.identifier lexer
    op' = Token <$> Tok.operator lexer
    parens' = Brackets <$> (Tok.parens lexer (many tokenz))

comb :: Parser [ Token ]
comb = do
    p <- many tokenz
    eof
    return p

tokenize :: String -> Maybe [Token]
tokenize s = either (\_ -> Nothing) Just $ parse comb "" s
-- tokenize "A + B * C" = Just [Token "A", Token "+", Token "B", Token "*", Token "C"]

-- tokenize "function a(arg, arg)" = Just [Token "function", Token "a", Brackets [Token "arg", Token "arg"]]


-- tokenize "add(a, b) = a + b" == Just [Token "add",Brackets [Token "a",Token ",",Token "b"],Token "=",Token "a",Token "+",Token "b"]
-- [Brackets [Token "XeSfA",Token "--/@",Brackets [Token "e"]]] == tokenize (XeSfA--/@(e))