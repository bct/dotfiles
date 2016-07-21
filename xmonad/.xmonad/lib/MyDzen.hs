-------------------------------------------------------------------------------
-- Module      :  MyDzen
--
-- Some functions to work with dzen
--
-------------------------------------------------------------------------------

module MyDzen where

import Data.List (intercalate)
import System.IO
import System.Posix.IO
import System.Process (runInteractiveCommand)
import System.Posix.Process (executeFile, forkProcess, createSession)
import XMonad.Hooks.DynamicLog hiding (dzen)
import Graphics.X11.Xlib     (openDisplay)
import Graphics.X11.Xinerama (xineramaQueryScreens, xsi_width)
import Style
import Control.Monad
import Control.Concurrent
import qualified Color as C
import Monitor

-- $usage
--
-- To use, copy the source code for this module into
-- @~\/.xmonad\/lib\/Dzen.hs@ and add the following to your
-- @~\/.xmonad\/xmonad.hs@:
--
-- >
-- > import Dzen
-- > import XMonad.Hooks.DynamicLog hiding (dzen)
-- >
-- > main :: IO ()
-- > main = do
-- >     d <- spawnDzen someDzen
-- >
-- >     xmonad $ defaultConfig
-- >         { ...
-- >         , logHook = myLogHook d
-- >         }
-- >
-- > myLogHook h = dynamicLogWithPP $ defaultPP
-- >     { ...
-- >     , ppOutput = hPutStrLn h
-- >     }
--
-- If you want to feed some other process into a dzen you can use the 
-- following:
--
-- > spawnToDzen "conky" someDzen
--
-- Where someDzen is a 'DzenConf' (see 'defaultDzen' for an example).
--

-- | A data type to fully describe a spawnable dzen bar, take a look at
--   @\/usr\/share\/doc\/dzen2\/README@ to see what input is acceptable. 
--   Options are wrapped in 'Just', so using 'Nothing' will not add that 
--   option to the @dzen2@ executable. @exec@ and @addargs@ can be 
--   empty for the same purpose.
data DzenConf = DzenConf 
    { xPosition :: Maybe DzenWidth -- ^ x position
    , yPosition :: Maybe Int       -- ^ y position
    , screen    :: Maybe ScreenNum -- ^ screen number (0 based, Nothing implies 0)
    , width     :: Maybe DzenWidth -- ^ width
    , height    :: Maybe Int       -- ^ line height
    , alignment :: Maybe TextAlign -- ^ alignment of title window
    , font      :: Maybe String    -- ^ font
    , fgColor   :: Maybe C.Color    -- ^ foreground color
    , bgColor   :: Maybe C.Color    -- ^ background color
    , exec      :: [String]        -- ^ exec flags, ex: [\"onstart=lower\", ...]
    , addargs   :: [String]        -- ^ additional arguments, ex: [\"-p\", \"-tw\", \"5\"]
    }

-- | Xinerama screen number
type ScreenNum = Int

-- | Define a width and x argument as straight pixel, or percentages
data DzenWidth = Pixels Int | Percent Double

-- | A simple data type for the text alignment of the dzen bar
data TextAlign = LeftAlign | RightAlign | Centered

-- | 'show' 'TextAlign' makes it suitable for use as a dzen argument
instance Show TextAlign where
    show LeftAlign  = "l"
    show RightAlign = "r"
    show Centered   = "c"

data GDBar = GDBar
             { barHeight :: Int
             , barWidth  :: Int
             , minVal    :: Int
             , maxVal    :: Int
             , current   :: Int
             , barFGColor   :: C.Color
             , barBGColor   :: C.Color
             }

defaultGDBar :: StyleConfig -> GDBar
defaultGDBar style = GDBar { barHeight = 10
                           , barWidth  = 80
                           , minVal    = 0
                           , maxVal    = 100
                           , current   = 0
                           , barFGColor   = dzenFGColor2 style
                           , barBGColor   = dzenFGColor  style
                           }

parseGDBar :: GDBar -> String
parseGDBar bar = "^fg(" ++ (show $ barFGColor bar) ++ ")"
                 ++ "^r(" ++ len1 ++ "x" ++ h ++ ")"
                 ++ "^fg(" ++ (show $ barBGColor bar) ++ ")"
                 ++ "^r(" ++ len2 ++ "x" ++ h ++ ")"
                 ++ "^fg()"
                 where
                   h = show $ barHeight bar
                   len1 = show $ cpos
                   len2 = show $ (barWidth bar) - cpos
                   cpos = round $ (fromIntegral $ current bar) * scale
                   scale = (fromIntegral $ barWidth bar) / (fromIntegral $ (maxVal bar) - (minVal bar))

data DzenContent = Text String 
                 | Container [DzenContent] 
                 | Icon String 
                 | FG C.Color DzenContent
                 | BG C.Color DzenContent
                 | ProgressBar GDBar

parseDzen :: DzenContent -> String
parseDzen (Text s) = s
parseDzen (Container ss) = concatMap parseDzen ss
parseDzen (Icon s) = "^i(" ++ s ++ ")"
parseDzen (FG c s) = "^fg(" ++ (show c) ++ ")" ++ (parseDzen s) ++ "^fg()"
parseDzen (BG c s) = "^bg(" ++ (show c) ++ ")" ++ (parseDzen s) ++ "^bg()"
parseDzen (ProgressBar bar) = parseGDBar bar


getDzenIcon :: StyleConfig -> String -> DzenContent
getDzenIcon style name = Icon $ (iconDir style) ++ "/" ++ name  ++ ".xbm"

doTimed :: Int -> IO () -> IO ()
doTimed interval f = forever $ f >> threadDelay interval

printTimed :: Int -> IO DzenContent -> Handle -> IO ()
printTimed interval f h = doTimed interval $ f >>= (print . parseDzen)
  where
    print s = hPutStr h (s++"\n") >> hFlush h

-- | Spawn a dzen by configuraion and return its handle, behaves
--   exactly as spawnPipe but takes a 'DzenConf' as argument.
spawnDzen :: DzenConf -> IO Handle
spawnDzen d = do
--  dz <- dzen d
--  (inp,out,err,pid) <- runInteractiveCommand dz
--  return inp
    (rd, wr) <- createPipe
    setFdOption wr CloseOnExec True
    h <- fdToHandle wr
    hSetBuffering h LineBuffering
    _ <- forkProcess $ do
        --_  <- createSession
        _  <- dupTo rd stdInput
        --dz <- dzen d
        dza <- dzenArgsUnquoted d
        --executeFile "/bin/sh" False ["-c", dz] Nothing
        executeFile "dzen2" True dza Nothing
    return h

batteryBar :: StyleConfig -> IO DzenContent
batteryBar style = do s <- batteryStatus
                      let bMax = zeroIfNothing (designCapacity s)
                          bCur = zeroIfNothing (remaining s)
                          batIcon = getDzenIcon style "bat_empty_02"
                          batBar  = (defaultGDBar style) { maxVal  = bMax
                                                         , current = bCur}
                      return $ Container [batIcon, Text "  ", ProgressBar batBar]
                   where
                     zeroIfNothing (Just n) = n
                     zeroIfNothing Nothing  = 0

fooBar :: IO DzenContent
fooBar = return $ Text "foooo"

-- Iterate through some IO items until one matches the criterion
search :: (String -> Bool) -> IO String -> IO String
search crit inp = do s <- inp
                     if (crit s) then (return s) else (search crit inp)

--batteryLevel :: String -> IO String
--batteryLevel ifile = h

-- | Run a function in a seperate thread
runToDzen :: (Handle -> IO ()) -> DzenConf -> IO ()
runToDzen f d = do
    (rd, wr) <- createPipe
    setFdOption rd CloseOnExec True
    setFdOption wr CloseOnExec True
    hin  <- fdToHandle rd
    hout <- fdToHandle wr
    hSetBuffering hin  LineBuffering
    hSetBuffering hout LineBuffering

    -- the dzen
    dz <- dzen d
    (inp,out,err,pid) <- runInteractiveCommand dz

    -- the input process
    forkIO $ f inp
    return ()

-- | Spawn a process sending its stdout to the stdin of the dzen
spawnToDzen :: String -> DzenConf -> IO ()
spawnToDzen x d = do
    (rd, wr) <- createPipe
    setFdOption rd CloseOnExec True
    setFdOption wr CloseOnExec True
    hin  <- fdToHandle rd
    hout <- fdToHandle wr
    hSetBuffering hin  LineBuffering
    hSetBuffering hout LineBuffering

    -- the dzen
    _ <- forkProcess $ do
        _  <- createSession
        _  <- dupTo rd stdInput
        dz <- dzen d
        executeFile "/bin/sh" False ["-c", dz] Nothing

    -- the input process
    _ <- forkProcess $ do
        _ <- createSession
        _ <- dupTo wr stdOutput
        executeFile "/bin/sh" False ["-c", x] Nothing

    return ()

-- | The full computed dzen command for a 'DzenConf'
dzen :: DzenConf -> IO String
dzen d = return . unwords . (:) "dzen2" =<< dzenArgs d

-- | The computed list of arguments for a 'DzenConf'
dzenArgs :: DzenConf -> IO [String]
dzenArgs d = do
    x <- mkWidth (screen d) (xPosition d)
    w <- mkWidth (screen d) (width d)

    let s = fmap (+1) $ screen d -- the -xs arg is 1 index

    return $ addOpt ("-fn", fmap quote          $ font      d)
          ++ addOpt ("-fg", fmap (quote . show) $ fgColor   d)
          ++ addOpt ("-bg", fmap (quote . show) $ bgColor   d)
          ++ addOpt ("-ta", fmap show           $ alignment d)
          ++ addOpt ("-y" , fmap show           $ yPosition d)
          ++ addOpt ("-h" , fmap show           $ height    d)
          ++ addOpt ("-xs", fmap show s             )
          ++ addOpt ("-x" , fmap show x             )
          ++ addOpt ("-w" , fmap show w             )
          ++ addExec (exec d)
          ++ addargs d

    where
        quote = ("'"++) . (++"'")

        addOpt (_  , Nothing ) = []
        addOpt (opt, Just arg) = [opt, arg]

        addExec [] = []
        addExec es = ["-e", quote $ intercalate ";" es]

dzenArgsUnquoted :: DzenConf -> IO [String]
dzenArgsUnquoted d = do
    x <- mkWidth (screen d) (xPosition d)
    w <- mkWidth (screen d) (width d)

    let s = fmap (+1) $ screen d -- the -xs arg is 1 index

    return $ addOpt ("-fn", fmap show $ font      d)
          ++ addOpt ("-fg", fmap show $ fgColor   d)
          ++ addOpt ("-bg", fmap show $ bgColor   d)
          ++ addOpt ("-ta", fmap show $ alignment d)
          ++ addOpt ("-y" , fmap show $ yPosition d)
          ++ addOpt ("-h" , fmap show $ height    d)
          ++ addOpt ("-xs", fmap show s             )
          ++ addOpt ("-x" , fmap show x             )
          ++ addOpt ("-w" , fmap show w             )
          ++ addExec (exec d)
          ++ addargs d

    where
        addOpt (_  , Nothing ) = []
        addOpt (opt, Just arg) = [opt, arg]

        addExec [] = []
        addExec es = ["-e", intercalate ";" es]

-- | Return the width of ScreenNum s (0 index), return 0 if screen  
--   doesn't exist
screenWidth :: ScreenNum -> IO Double
screenWidth s = do
    dsp <- openDisplay ""
    mss <- xineramaQueryScreens dsp
    return $ case mss of
        Nothing -> 0
        Just [] -> 0
        Just ss -> if s >= 0 && s < length ss -- prevent bad index
            then fromIntegral . xsi_width $ ss !! s else 0

-- | Given a 'DzenWidth', give back the Maybe Int that can be used as an 
--   argument for @-w@ or @-x@.
mkWidth :: Maybe ScreenNum -> Maybe DzenWidth -> IO (Maybe Int)
mkWidth Nothing  w                  = mkWidth (Just 0) w
mkWidth _        Nothing            = return Nothing
mkWidth (Just _) (Just (Pixels x))  = return $ Just x
mkWidth (Just s) (Just (Percent c)) = return . go =<< screenWidth s
    where
        go 0  = Nothing
        go sw = Just . round $ (c/100) * sw

-- | A default dzen configuration. Similar colors to default decorations 
--   and prompts in other modules. Added options @-p@ and @-e 
--   \'onstart=lower\'@ are useful for dzen-as-statusbar.
defaultDzen :: DzenConf
defaultDzen = nothingDzen
    { alignment = Just LeftAlign
    , font      = Just "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
    , fgColor   = Just C.white
    , bgColor   = Just C.grey
    , exec      = ["onstart=lower"]
    , addargs   = ["-p"]
    }

-- | Same thing but with an XFT font (Verdana)
defaultDzenXft :: DzenConf
defaultDzenXft = defaultDzen { font = Just "Verdana-8" }

-- | A dzen with all options as 'Nothing' or the empty list.
nothingDzen :: DzenConf
nothingDzen = DzenConf
    { xPosition = Nothing
    , yPosition = Nothing
    , screen    = Nothing
    , width     = Nothing
    , height    = Nothing
    , alignment = Nothing
    , font      = Nothing
    , fgColor   = Nothing
    , bgColor   = Nothing
    , exec      = []
    , addargs   = []
    }

myDzenPP style dzenPipe = defaultPP
    { ppCurrent         = dzenColor (show $ Style.dzenBGColor style) (show $ Style.dzenFGColor style) . pad 
    , ppHidden          = dzenColor (show $ Style.dzenFGColor2 style) "" . pad
    , ppHiddenNoWindows = noText
    , ppLayout          = noText
    , ppUrgent          = dzenColor (show $ Style.dzenUrgentFGColor style) (show $ Style.dzenUrgentBGColor style) . pad . dzenStrip
    , ppTitle           = dzenColor (show $ Style.dzenFGColor style) "" . pad
    , ppWsSep           = ""
    , ppSep             = " "
    , ppOutput          = hPutStrLn dzenPipe
    }
    where noText x = ""
