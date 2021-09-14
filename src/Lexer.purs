module Lexer where

import Control.Alt ((<|>))
import Text.Parsing.Parser.String (oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), LanguageDef, TokenParser, alphaNum, letter, makeTokenParser)

languageDef :: LanguageDef
languageDef = LanguageDef
  { commentStart: "(*"
  , commentEnd: "*)"
  , commentLine: ""
  , nestedComments: false
  , identStart: letter
  , identLetter: alphaNum <|> oneOf ['_', '\'']
  , opStart: oneOf ['#']
  , opLetter: oneOf ['#']
  , reservedNames: [ "if"
                    , "else"
                    , "while"
                    , "read"
                    , "write"
                    , "int" 
                    , "fst"
                    , "snd"
                    , "not"
                    , "true"
                    , "false"
                    ]
  , reservedOpNames: ["#"
                    , "+"
                    , "-"
                    , "*"
                    , "/"
                    , "%"
                    , "<"
                    , ">"
                    , "<="
                    , ">="
                    , "=="
                    , "!="
                    , "&"
                    , "|"
                    , ":="
                    , "("
                    , ")"
                    , "{"
                    , "}"
                    , ";"]
  , caseSensitive: true
}

token :: TokenParser
token = makeTokenParser languageDef