
module Experiments where


import EDG
import NewEncoding.Util
import NewEncoding.CommonPorts
import NewEncoding.CommonLinks

import NewEncoding.CommsPorts
import NewEncoding.CommsLinks

import NewEncoding.CommonModules
import NewEncoding.ChipModules
import NewEncoding.RedundantModules

import NewEncoding.SwAdapters

import Control.Monad

import Text.Printf

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import qualified NewEncoding.Blinky as Blinky
import qualified NewEncoding.Simon as Simon
import qualified NewEncoding.Datalogger as Datalogger
import qualified NewEncoding.FeedbackFan as FeedbackFan
import qualified NewEncoding.Robot as Robot
import qualified NewEncoding.SimonTrinket as SimonTrinket
import qualified NewEncoding.AlternativeSimonTrinket as AlternativeSimonTrinket

import Data.Time
import System.Environment
import System.Random

fullLibrary :: EDGLibrary
fullLibrary = EDGLibrary{
  modules = [
    ("i2cPower", 1, i2cPower),
    ("button", 4, button),
    ("led", 4, led),
    ("tmp102", 1, tmp102),
    ("lcd3v3", 1, serialLcd16x2_3v3),
    ("lcd5v", 1, serialLcd16x2_5v),
    ("domeButton", 4, domeButton),
    ("openLog", 1, openLog),
    ("sdcard", 1, sdcard),
    ("qre1113Analog", 2, qre1113Analog),
    ("pwmControlFan", 1, powerControlFan),
    ("powerControlFan", 1, pwmControlFan),
    ("pcf8575", 1, pcf8575),
    ("tb6612fng", 1, tb6612fng),
    ("digitalAmplifier", 1, digitalAmplifier),
    ("litButton", 4, litButton),
    ("fat32", 1, fat32),
    ("apm3v3", 1, apm3v3),
    ("trinket3v3", 1, arduinoTrinket3v3)
    ],
  links = [
    ("apiLink", 12, apiLink),
    ("powerLink", 3, powerLink 10),
    ("usbLink", 1, usbLink),
    ("digitalLink", 2, digitalLink),
    ("digitalBidirLink", 0, digitalBidirLink),
    ("digitalBidirSinkLink", 6, digitalBidirSinkLink),
    ("digitalBidirSourceLink", 6, digitalBidirSourceLink),
    ("motorLink", 2, motorLink),
    ("analogLink", 2, analogLink),
    ("spiLink", 1, spiLink 2),
    ("uartLink", 1, uartLink),
    ("i2cLink", 1, i2cLink 2)
    ]
  }

emptyLibrary :: EDGLibrary
emptyLibrary = EDGLibrary{
  modules = [],
  links = []
  }

baseLib :: EDGLibrary
baseLib = EDGLibrary {
  modules = [
    ("apm3v3", 1, apm3v3),
    ("trinket3v3", 1, arduinoTrinket3v3)
    ],
  links = [
    ("apiLink", 1, apiLink),
    ("powerLink", 1, powerLink 10),
    ("usbLink", 1, usbLink),
    ("digitalLink", 2, digitalLink),
    ("digitalBidirLink", 0, digitalBidirLink),
    ("digitalBidirSinkLink", 2, digitalBidirSinkLink),
    ("digitalBidirSourceLink", 2, digitalBidirSourceLink),
    ("spiLink", 1, spiLink 2),
    ("uartLink", 1, uartLink),
    ("i2cLink", 1, i2cLink 4)
    ]
  }

buttonMin :: EDGLibrary
buttonMin = EDGLibrary {
  modules = [
    -- Base links
    ("i2cPower", 1, i2cPower),
    -- Basic devices
    ("button", 1, button),
    ("led", 1, led),
    -- Interfaces
    ("domeButton", 1, domeButton),
    ("pcf8575", 1, pcf8575),
    ("litButton", 1, litButton)
    ],
  links = [
    ("apiLink", 3, apiLink),
    ("powerLink", 2, powerLink 10),
    ("usbLink", 1, usbLink),
    ("digitalBidirSinkLink", 2, digitalBidirSinkLink),
    ("digitalBidirSourceLink", 2, digitalBidirSourceLink),
    ("i2cLink", 1, i2cLink 4)
    ]
  }


ledMin :: EDGLibrary
ledMin = EDGLibrary {
  modules = [
    ("led", 1, led)
    ],
  links = [
    ("apiLink", 2, apiLink),
    ("powerLink", 1, powerLink 6),
    ("digitalBidirSinkLink", 1, digitalBidirSinkLink),
    ("digitalBidirSourceLink", 1, digitalBidirSourceLink)
    ]
  }

fanMin :: EDGLibrary
fanMin = EDGLibrary{
  modules = [
    ("pwmControlFan", 1, powerControlFan),
    ("powerControlFan", 1, pwmControlFan),
    ("pcf8575", 1, pcf8575),
    ("tb6612fng", 1, tb6612fng),
    ("digitalAmplifier", 1, digitalAmplifier)
    ],
  links = [
    ("apiLink", 2, apiLink),
    ("powerLink", 2, powerLink 10),
    ("digitalLink", 2, digitalLink),
    ("digitalBidirLink", 0, digitalBidirLink),
    ("digitalBidirSinkLink", 2, digitalBidirSinkLink),
    ("digitalBidirSourceLink", 2, digitalBidirSourceLink),
    ("analogLink", 1, analogLink),
    ("spiLink", 1, spiLink 2),
    ("uartLink", 1, uartLink),
    ("i2cLink", 1, i2cLink 2)
    ]
  }

sensorMin :: EDGLibrary
sensorMin = EDGLibrary{
  modules = [
    ("i2cPower", 1, i2cPower),
    ("tmp102", 1, tmp102),
    ("qre1113Analog", 2, qre1113Analog),
    ("pcf8575", 1, pcf8575),
    ("tb6612fng", 1, tb6612fng),
    ("digitalAmplifier", 1, digitalAmplifier)
    ],
  links = [
    ("apiLink", 2, apiLink),
    ("powerLink", 1, powerLink 10),
    ("digitalLink", 2, digitalLink),
    ("digitalBidirSinkLink", 2, digitalBidirSinkLink),
    ("digitalBidirSourceLink", 2, digitalBidirSourceLink),
    ("analogLink", 2, analogLink),
    ("spiLink", 1, spiLink 2),
    ("uartLink", 1, uartLink),
    ("i2cLink", 1, i2cLink 2)
    ]
  }

storageMin :: EDGLibrary
storageMin = EDGLibrary{
  modules = [
    ("i2cPower", 1, i2cPower),
    ("domeButton", 4, domeButton),
    ("openLog", 1, openLog),
    ("sdcard", 1, sdcard),
    ("qre1113Analog", 2, qre1113Analog),
    ("digitalAmplifier", 1, digitalAmplifier),
    ("fat32", 1, fat32)
    ],
  links = [
    ("apiLink", 2, apiLink),
    ("powerLink", 1, powerLink 10),
    ("digitalLink", 2, digitalLink),
    ("digitalBidirSinkLink", 2, digitalBidirSinkLink),
    ("digitalBidirSourceLink", 2, digitalBidirSourceLink),
    ("analogLink", 2, analogLink),
    ("spiLink", 1, spiLink 2),
    ("uartLink", 1, uartLink),
    ("i2cLink", 1, i2cLink 2)
    ]
  }

displayMin :: EDGLibrary
displayMin = EDGLibrary{
  modules = [
    ("pcf8575", 1, pcf8575),
    ("tb6612fng", 1, tb6612fng)
    ],
  links = [
    ("apiLink", 2, apiLink),
    ("powerLink", 1, powerLink 10),
    ("digitalLink", 2, digitalLink),
    ("digitalBidirLink", 0, digitalBidirLink),
    ("digitalBidirSinkLink", 2, digitalBidirSinkLink),
    ("digitalBidirSourceLink", 2, digitalBidirSourceLink),
    ("motorLink", 1, motorLink),
    ("analogLink", 1, analogLink),
    ("spiLink", 1, spiLink 2),
    ("uartLink", 1, uartLink),
    ("i2cLink", 1, i2cLink 2)
    ]
  }

addLibrary :: EDGLibrary -> EDGLibrary -> EDGLibrary
addLibrary (EDGLibrary mods1 lnks1) (EDGLibrary mods2 lnks2)
  = EDGLibrary modsO lnksO
  where
    mods1m = Map.fromList . map (\ (a,b,c) -> (a,(b,c))) $ mods1
    mods2m = Map.fromList . map (\ (a,b,c) -> (a,(b,c))) $ mods2
    lnks1m = Map.fromList . map (\ (a,b,c) -> (a,(b,c))) $ lnks1
    lnks2m = Map.fromList . map (\ (a,b,c) -> (a,(b,c))) $ lnks2
    modsOm = Map.unionWith (\ (c,m) (c',_) -> (c+c',m)) mods1m mods2m
    lnksOm = Map.unionWith (\ (c,m) (c',_) -> (c+c',m)) lnks1m lnks2m
    modsO = map (\ (a,(b,c)) -> (a,b,c)) . Map.assocs $ modsOm
    lnksO = map (\ (a,(b,c)) -> (a,b,c)) . Map.assocs $ lnksOm

maxLibrary :: EDGLibrary -> EDGLibrary -> EDGLibrary
maxLibrary (EDGLibrary mods1 lnks1) (EDGLibrary mods2 lnks2)
  = EDGLibrary modsO lnksO
  where
    mods1m = Map.fromList . map (\ (a,b,c) -> (a,(b,c))) $ mods1
    mods2m = Map.fromList . map (\ (a,b,c) -> (a,(b,c))) $ mods2
    lnks1m = Map.fromList . map (\ (a,b,c) -> (a,(b,c))) $ lnks1
    lnks2m = Map.fromList . map (\ (a,b,c) -> (a,(b,c))) $ lnks2
    modsOm = Map.unionWith (\ (c,m) (c',_) -> (max c c',m)) mods1m mods2m
    lnksOm = Map.unionWith (\ (c,m) (c',_) -> (max c c',m)) lnks1m lnks2m
    modsO = map (\ (a,(b,c)) -> (a,b,c)) . Map.assocs $ modsOm
    lnksO = map (\ (a,(b,c)) -> (a,b,c)) . Map.assocs $ lnksOm

printLibMods :: EDGLibrary -> String
printLibMods = show . map (\ (a,b,_) -> (a,b)) . modules

printLibLnks :: EDGLibrary -> String
printLibLnks = show . map (\ (a,b,_) -> (a,b)) . modules

multLib :: Int -> EDGLibrary -> EDGLibrary
multLib c l = foldr addLibrary emptyLibrary (replicate c l)

randLib :: Int -> EDGLibrary -> IO EDGLibrary
randLib i EDGLibrary{..} = do
  newLinks <- forM links $ \ (a,_,b) -> do
    c <- randomRIO(0,i)
    return (a,c,b)
  newMods <- forM modules $ \ (a,_,b) -> do
    c <- randomRIO(0,i)
    return (a,c,b)
  return $ EDGLibrary newMods newLinks

-- | gets a list of length n that sums to t, the stupid way.
getRandList :: Int -> Int -> IO [Int]
getRandList n t =
  sub1s . map length . group . sort . add1s <$>
    mapM (\ _ -> randomRIO (1,n)) [1..t]
  where
    add1s = (++) [1..n+1]
    sub1s = map (\ n -> n - 1)

-- the input is the total number of modules we should allocate. n is
-- the amount of noise we should add to the library.
universalSeed :: Int -> Int -> IO (EDGLibrary,[Int], Module ())
universalSeed i n = do
  sl <- getRandList 6 i
  let (le:se:st:di:fa:bu:_) = sl
  rlib <- randLib n fullLibrary
  let lib = foldr addLibrary baseLib $ concat @[] [
                replicate le ledMin
              , replicate se sensorMin
              , replicate st storageMin
              , replicate di displayMin
              , replicate fa fanMin
              , replicate bu buttonMin
              , [rlib]
              ]
  return (lib,sl, seed le se st di fa bu)
  where
    seed :: Int -> Int -> Int -> Int -> Int -> Int -> Module ()
    seed le se st di fa bu = do
      setIdent "Control Logic"
      setSignature "controlLogic"

      setType [
        "controlUid" <:= UID
        ]

      let makePorts i s m = forM @[] [1..i]
            (\ id -> let name = s ++ (show id) in addPort name $ m name)

      -- Power providing elements.

      usbHost <- addPort "usbHost" $ do
        usbHost
        setType[
          "voltage" <:= range (FloatV 4.5) (FloatV 5.5),
          "limitCurrent" <:= range (FloatV 0) (FloatV 0.5)
          ]
        return ()

      pwr12v <- addPort "pwr12v" $ do
        powerSource
        setType[
          "voltage" <:= range (FloatV 11.8) (FloatV 12.2),  -- really good supply!
          "limitCurrent" <:= range (FloatV 0) (FloatV 3)  -- really beefy supply!
          ]
        return ()

      leds <- makePorts le "led" $ \ name -> do
        apiConsumer
        setType [
          "controlName" <:= StringV name,
          "apiType" <:= StringV "led"
          ]
        constrain $ typeVal "apiData.bandwidth" :>= Lit (FloatV 10)
        return ()

      sensors <- makePorts se "temp-sensor" $ \name -> do
        apiConsumer
        setType [
          "controlName" <:= StringV name,
          "apiType" <:= StringV "temperatureSensor"
          ]
        constrain $ typeVal "apiData.tempRange.min" :<= Lit (FloatV 0)
        constrain $ typeVal "apiData.tempRange.max" :>= Lit (FloatV 75)
        constrain $ typeVal "apiData.tempResolution" :<= Lit (FloatV 0.5)
        return ()

      storages <- makePorts st "storage" $ \ name -> do
        apiConsumer
        setType [
          "controlName" <:= StringV name,
          "apiType" <:= StringV "fat32"
          ]
        constrain $ typeVal "apiData.size" :>= Lit (IntV 1073741824)  -- 1 GiB
        return ()

      displays <- makePorts di "display" $ \name -> do
        apiConsumer
        setType [
          "controlName" <:= StringV name,
          "apiType" <:= StringV "characterLcd"
          ]
        return ()

      fans <- makePorts fa "fan" $ \name -> do
        apiConsumer
        setType [
          "controlName" <:= StringV name,
          "apiType" <:= StringV "controlledFan"
          ]
        return ()

      buttons <- makePorts bu "button" $ \ name -> do
        apiConsumer
        setType [
          "controlName" <:= StringV name,
          "apiType" <:= StringV "litButton"
          ]
        -- constrain $ typeVal "apiData.bandwidth" :>= Lit (FloatV 10)
        return ()

      let allPorts = concat @[] [leds, sensors, storages ,
                       displays, fans, buttons]

      ensureConnected allPorts
      setFieldsEq True allPorts ["controlUid"]

      return ()

run :: EDGSettings -> IO ()
run e@EDGSettings{..} = do
  numReqs <- randomRIO (1,6)
  noise <- randomRIO (0,20)
  (outLib,seedList,randSeed) <- universalSeed numReqs noise
  sequence_ $ fmap (printLib numReqs noise outLib seedList) timingData
  makeSynthFunc outLib [("controlLogic",randSeed)] e
  where
    printLib :: Int -> Int -> EDGLibrary -> [Int] -> FilePath -> IO ()
    printLib reqs noise lib sl fp = do
      progName <- getProgName
      args <- getArgs
      let cmd = concat . intersperse " " $ progName : args
      time <- getCurrentTime
      appendFile fp $
        printf ("{\"cmd\":%s,\"time\":%s,"
          ++"\"reqs\":%i,\"noise\":%i,"
          ++"\"mods\":%s,\"libs\":%s,\"seed\":%s}\n")
          (show cmd :: String)
          (show $ formatTime defaultTimeLocale
              rfc822DateFormat time :: String)
          (reqs :: Int)
          (noise :: Int)
          (show $ printLibMods lib :: String)
          (show $ printLibLnks lib :: String)
          (show sl :: String)


