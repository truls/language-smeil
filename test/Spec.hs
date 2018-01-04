import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

import           Language.SMEIL.Parser.Impl
import           Language.SMEIL.Parser.Lexer
import qualified Language.SMEIL.Syntax       as S

p a = parse a "(test)"

main :: IO ()
main = hspec $
  describe "Lexer" $ do
    it "parses direction" $ do
      p direction "in" `shouldParse` S.In
      p direction "out" `shouldParse` S.Out
      p direction "const" `shouldParse` S.Const
    it "parses literals" $ do
       p literal "true" `shouldParse` S.LitTrue
       p literal "false" `shouldParse` S.LitFalse
       p literal "20" `shouldParse` S.LitInt 20
       p literal "3.4" `shouldParse` S.LitFloat 3.4
       p literal "\"foobar\"" `shouldParse` S.LitString "foobar"
       p literal "\"foo\\\"bar\"" `shouldParse` S.LitString "foo\"bar"
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
