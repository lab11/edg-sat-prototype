module NewEncoding.Design where

import EDG
import NewEncoding.Util
import NewEncoding.CommonPorts
import NewEncoding.CommonLinks
import NewEncoding.CommonModules
import NewEncoding.CommsPorts
import NewEncoding.CommsLinks
import NewEncoding.ChipModules

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
    ("sdcard", 1, sdcard),

    ("pwmControlFan", 1, powerControlFan),
    ("powerControlFan", 1, pwmControlFan),

    -- Interfaces
    ("pcf8575", 1, pcf8575),
    ("digitalAmplifier", 1, digitalAmplifier),

    -- Microcontrollers
    ("apm3v3", 1, apm3v3)
    ],
  links = [
    ("apiLink", 8, apiLink),

    ("powerLink", 3, powerLink 6),
    ("usbLink", 1, usbLink),

    ("digitalLink", 2, digitalLink),
    ("digitalBidirLink", 0, digitalBidirLink),
    ("digitalBidirSinkLink", 4, digitalBidirSinkLink),
    ("digitalBidirSourceLink", 4, digitalBidirSourceLink),

    ("spiLink", 1, spiLink 2),
    ("uartLink", 1, uartLink),
    ("i2cLink", 1, i2cLink 2)
    ]
  }
