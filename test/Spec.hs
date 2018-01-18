import           Control.Monad.State.Lazy
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

import           Language.SMEIL.Parser.Impl
import           Language.SMEIL.Parser.Lexer
import           Language.SMEIL.Parser.Monad
import qualified Language.SMEIL.Syntax       as S

--p a = parse a "(test)"
--p :: Parser a -> String -> String -> Either (ParseError String) a
p x c =
  case runParser (runStateT x (initialPos "(test)")) "(test)" c of
    Right (r, _) -> Right r
    Left l       -> Left l

pos :: SourcePos
pos = initialPos "(test)"

mkSrcSpan :: SrcSpan
mkSrcSpan = SrcSpan pos pos

main :: IO ()
main = hspec $
  describe "Lexer" $ do
    -- TODO: Tests disabled until we figure out how to work around mismatching SrcSpans
    -- it "parses direction" $ do
    --   p direction "in" `shouldParse` S.In mkSrcSpan
    --   p direction "out" `shouldParse` S.Out mkSrcSpan
    --   p direction "const" `shouldParse` S.Const mkSrcSpan
    -- it "parses literals" $ do
    --    p literal "true" `shouldParse` S.LitTrue mkSrcSpan
    --    p literal "false" `shouldParse` S.LitFalse mkSrcSpan
    --    p literal "20" `shouldParse` S.LitInt 20 mkSrcSpan
    --    p literal "3.4" `shouldParse` S.LitFloat 3.4 mkSrcSpan
    --    p literal "\"foobar\"" `shouldParse` S.LitString "foobar" mkSrcSpan
    --    p literal "\"foo\\\"bar\"" `shouldParse` S.LitString "foo\"bar" mkSrcSpan
    it "parses identifiers" $ do
       p ident "_" `shouldParse` "_"
       p ident `shouldSucceedOn` "foo"
       p ident `shouldSucceedOn` "foo_bar"
       p ident  `shouldFailOn` "_bar"
       p ident `shouldSucceedOn` "foo"
    it "parses numbers" $ do
      p integer "10" `shouldParse` 10
      p integer "0o10" `shouldParse` 8
      p integer "0x10" `shouldParse` 16
