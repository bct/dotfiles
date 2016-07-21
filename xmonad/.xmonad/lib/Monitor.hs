module Monitor where

import Data.List
import Data.Char

data BatteryStatus = BatteryStatus
    { 
      chargingStatus :: Maybe ChargingStatus
    , designCapacity :: Maybe Int
    , presentRate :: Maybe Int
    , realCapacity :: Maybe Int
    , designCapacityWarning :: Maybe Int
    , designCapacityLow :: Maybe Int
    , remaining :: Maybe Int
    }
    
emptyStatus = BatteryStatus Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    
data ChargingStatus = Charging | Discharging
                    deriving Show

fromString :: String -> Maybe ChargingStatus
fromString s | s == "charging"    = Just Charging
             | s == "discharging" = Just Discharging
             | otherwise         = Nothing

batteryStatus :: IO BatteryStatus
batteryStatus = do
  s1 <- readFile statefile
  s2 <- readFile infofile
  let l = (lines s1) ++ (lines s2)
      status = foldl updateBatStat emptyStatus l
  return status
  where statefile = "/proc/acpi/battery/BAT1/state"
        infofile = "/proc/acpi/battery/BAT1/info"
        lowbat = 25
        lowcol = "#ff4747"
        symbol = "/home/daniel/.dzen/xbm8x8/bat_empty_02.xbm"
        updateBatStat stat line 
          | "charging state:" `isPrefixOf` line          = stat {chargingStatus = fromString $ procValue line}
          | "present rate:" `isPrefixOf` line            = stat {presentRate = Just $ procNumValue line} 
          | "remaining capacity:" `isPrefixOf` line      = stat {remaining = Just $ procNumValue line}
          | "design capacity:" `isPrefixOf` line         = stat {designCapacity = Just $ procNumValue line}
          | "last full capacity:" `isPrefixOf` line      = stat {realCapacity = Just $ procNumValue line}
          | "design capacity warning:" `isPrefixOf` line = stat {designCapacityWarning = Just $ procNumValue line}
          | "design capacity low:" `isPrefixOf` line     = stat {designCapacityLow = Just $ procNumValue line}
          | otherwise                                   = stat
  
procValue :: String -> String
procValue =  (dropWhile (== ' ')) . (drop 1) . (dropWhile (/= ':'))

procNumValue :: String -> Int
procNumValue = strToInt . (takeWhile isNumeric) . procValue

isNumeric :: Char -> Bool
isNumeric = (flip elem) $ map intToDigit [0 .. 10]

strToInt :: String -> Int
strToInt = (foldl comb 0) . (zip [0..]) . reverse . (map digitToInt)
           where 
             comb x (i,d) = x + d*10^i
