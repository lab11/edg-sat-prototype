module NewEncoding.SwAdapters where

import Control.Monad

import EDG
import NewEncoding.CommonPorts
import NewEncoding.CommsPorts
import NewEncoding.Util


fat32 :: Module ()
fat32 = do
  setIdent "FAT32 Filesystem"
  setSignature "FAT32 Filesystem"
  setType []

  fat32Api <- addPort "fat32" $ do
    apiProducer
    setType [
      "apiType" <:= StringV "fat32",
      -- TODO: more temp sensor properties, scale resolution accuracy
      "apiData" <:= Record [
        "size" <:= IntC unknown,
        "tech" <:= StringC unknown,
        "form" <:= StringC unknown
        ]
      ]
    return ()

  nvmApi <- addPort "nvmemory" $ do
    apiConsumer
    setType [
      "apiType" <:= StringV "nvmemory",
      "apiData" <:= Record [
        -- minimums found from https://stackoverflow.com/questions/8412792/smallest-fat32-partition
        "size" <:= IntC (greaterThan 33548800),
        "tech" <:= StringC unknown,
        "form" <:= StringC unknown
        ]
      ]
    return ()

  ensureConnected [fat32Api, nvmApi]

  setFieldsEq False [fat32Api, nvmApi] ["controlUid", "controlName", "deviceData", "apiData.size", "apiData.tech", "apiData.form"]

  return ()

litButton :: Module ()
litButton = do
  setIdent "Lit Button"
  setSignature "Lit Button"
  setType []

  litButtonApi <- addPort "litButton" $ do
    apiProducer
    setType [
      "apiType" <:= StringV "litButton",
      -- TODO: more temp sensor properties, scale resolution accuracy
      "deviceData" <:= Record [
        "led" <:= StringC unknown,
        "button" <:= StringC unknown
        ]
      ]
    return ()

  buttonApi <- addPort "buttonApi" $ do
    apiConsumer
    setType [
      "apiType" <:= StringV "button"
      ]
    return ()

  ledApi <- addPort "led" $ do
    apiConsumer
    setType [
      "apiType" <:= StringV "led"
      ]
    return ()

  ensureConnected [litButtonApi, buttonApi, ledApi]

  setFieldsEq False [litButtonApi, buttonApi, ledApi] ["controlUid", "controlName"]
  constrain $ port litButtonApi (typeVal "deviceData.button") :== port buttonApi (typeVal "deviceData.device")
  constrain $ port litButtonApi (typeVal "deviceData.led") :== port ledApi (typeVal "deviceData.device")

  return ()
