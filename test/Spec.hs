import           Combinators
import           Def
import qualified LL1.Combinators as LL1
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "LL(k) parser tests with type hints" $ do
    it "0 arguments" $ do
      parse "def f() =" `shouldBe` Def (IW "f") [] Nothing
      parse "def f   (                )=" `shouldBe` Def (IW "f") [] Nothing
      parse "def _1() -> int =" `shouldBe` Def (IW "_1") [] (Just (THW "int"))
    it "1 argument" $ do
      parse "def f(_HOWDY : float) =" `shouldBe` Def (IW "f") [(IW "_HOWDY", Just (THW "float"))] Nothing
      parse "def f   (      jeez          )=" `shouldBe` Def (IW "f") [(IW "jeez", Nothing)] Nothing
      parse "def _1(__1) -> List[str] =" `shouldBe` Def (IW "_1") [(IW "__1", Nothing)] (Just (THW "List[str]"))
    it "2 arguments & more" $ do
      parse "def _(_1, _2:str, _3) =" `shouldBe` Def (IW "_") [ (IW "_1", Nothing)
                                                              , (IW "_2", Just (THW "str"))
                                                              , (IW "_3", Nothing)
                                                              ] Nothing
      parse "def f   (      jeez : HMM  ,louise:thassatype, ya_make_me_sick          )=" `shouldBe` 
        Def (IW "f") [ (IW "jeez", Just (THW "HMM"))
                     , (IW "louise", Just (THW "thassatype"))
                     , (IW "ya_make_me_sick", Nothing)
                     ] Nothing
      parse "def _1(a,b:type1,c,d,r:type2,e:type3,q) -> NoneType =" `shouldBe` 
        Def (IW "_1") [ (IW "a", Nothing)
                      , (IW "b", Just (THW "type1"))
                      , (IW "c", Nothing)
                      , (IW "d", Nothing)
                      , (IW "r", Just (THW "type2"))
                      , (IW "e", Just (THW "type3"))
                      , (IW "q", Nothing)
                      ] (Just (THW "NoneType"))
  describe "LL(1) parser tests with type hints" $ do
    it "0 arguments" $ do
      LL1.parse "def f() =" `shouldBe` Def (IW "f") [] Nothing
      LL1.parse "def f   (                )=" `shouldBe` Def (IW "f") [] Nothing
      LL1.parse "def _1() -> int =" `shouldBe` Def (IW "_1") [] (Just (THW "int"))
    it "1 argument" $ do
      LL1.parse "def f(_HOWDY : float) =" `shouldBe` Def (IW "f") [(IW "_HOWDY", Just (THW "float"))] Nothing
      LL1.parse "def f   (      jeez          )=" `shouldBe` Def (IW "f") [(IW "jeez", Nothing)] Nothing
      LL1.parse "def _1(__1) -> List[str] =" `shouldBe` Def (IW "_1") [(IW "__1", Nothing)] (Just (THW "List[str]"))
    it "2 arguments & more" $ do
      LL1.parse "def _(_1, _2:str, _3) =" `shouldBe` Def (IW "_") [ (IW "_1", Nothing)
                                                                  , (IW "_2", Just (THW "str"))
                                                                  , (IW "_3", Nothing)
                                                                  ] Nothing
      LL1.parse "def f   (      jeez : HMM  ,louise:thassatype, ya_make_me_sick          )=" `shouldBe` 
        Def (IW "f") [ (IW "jeez", Just (THW "HMM"))
                     , (IW "louise", Just (THW "thassatype"))
                     , (IW "ya_make_me_sick", Nothing)
                     ] Nothing
      LL1.parse "def _1(a,b:type1,c,d,r:type2,e:type3,q) -> NoneType =" `shouldBe` 
        Def (IW "_1") [ (IW "a", Nothing)
                      , (IW "b", Just (THW "type1"))
                      , (IW "c", Nothing)
                      , (IW "d", Nothing)
                      , (IW "r", Just (THW "type2"))
                      , (IW "e", Just (THW "type3"))
                      , (IW "q", Nothing)
                      ] (Just (THW "NoneType"))