module NewEncoding.Design where

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

fullLibrary :: EDGLibrary
fullLibrary = EDGLibrary{
  modules = [
    -- Base links
    ("i2cPower", 1, i2cPower),

    -- Basic devices
    ("button", 4, button),
    ("led", 4, led),

    -- More devices
    ("tmp102", 1, tmp102),
    ("lcd3v3", 1, serialLcd16x2_3v3),
    ("lcd5v", 1, serialLcd16x2_5v),
    ("domeButton", 4, domeButton),
    ("openLog", 1, openLog),
    ("sdcard", 1, sdcard),
    ("qre1113Analog", 2, qre1113Analog),

    ("pwmControlFan", 1, powerControlFan),
    ("powerControlFan", 1, pwmControlFan),

    -- Interfaces
    ("pcf8575", 1, pcf8575),
    ("tb6612fng", 1, tb6612fng),
    ("digitalAmplifier", 4, digitalAmplifier),
    ("litButton", 4, litButton),

    ("l7805", 1, l7805),

    ("fat32", 1, fat32),

    -- Microcontrollers
    ("apm3v3", 1, apm3v3),
    ("trinket3v3", 1, arduinoTrinket3v3)
    ],
  links = [
    ("apiLink", 12, apiLink),

    ("powerLink", 3, powerLink 6),
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
