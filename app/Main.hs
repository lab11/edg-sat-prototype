module Main where

import EDG

testModule :: Module ()
testModule = do
  setIdent "test"
  setSignature "seedMod"
  setType [
      "field1" <:= IntV 12
    , "field2" <:= IntC $ lessThan 15
    ]

  addPort "port1" $ do
    setIdent "testPort"
    setKind "a"
    setType [
        "b" <:= StringV "Foo"
      ]

    return ()

  constrain $ typeVal "field1" :< typeVal "field2"
  constrain $ port "port1" connected

  return ()

testLink :: Link ()
testLink = do
  setIdent "link"
  setSignature "linktest"
  setType []

  addPort "porta" $ do
    setIdent "portfoo"
    setKind "a"
    setType []

    return ()

  return ()


testLibrary :: EDGLibrary
testLibrary = EDGLibrary{
    modules = []
  , links   = [
        ("testLink",2,testLink)
      ]
  }

main :: IO ()
main = synthesize testLibrary "Seed" testModule
