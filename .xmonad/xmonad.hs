--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Loggers

import XMonad.Layout.PerWorkspace
import XMonad.Layout.Grid
import XMonad.Layout.IM
import Data.Ratio ((%))
import XMonad.Layout.SimpleDecoration

import XMonad.Layout.NoBorders

import XMonad.Layout.Tabbed
import XMonad.Util.Themes

import XMonad.Actions.CycleWS(prevWS, nextWS, toggleWS)

import XMonad.Actions.TopicSpace

import XMonad.Actions.SpawnOn(spawnHere, shellPromptHere, spawnOn)

import Control.Monad(Monad(return, (>>=), (>>)), Functor(..), (=<<), mapM, sequence, (<=<), zipWithM_)
import XMonad.Actions.GridSelect

import XMonad.Actions.PhysicalScreens

import Graphics.X11.ExtraTypes.XF86

-- local lib dir
import Style
import MyDzen

-- The list of all topics/workspaces of your xmonad configuration.
-- The order is important, new topics must be inserted
-- at the end of the list if you want hot-restarting
-- to work.
myTopics :: [Topic]
myTopics =
 [ "stripe" , "web", "mail", "mi-go", "?", "doc", "music", "office" ]

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
    { topicDirs = M.fromList $
        [ ("stripe",   "stripe")
        ]

    , defaultTopicAction = const spawnShell

    , defaultTopic = "stripe"
    , maxTopicHistory = 10

    , topicActions = M.fromList $
        [
          ("?",          spawnShell >>
                         spawn "urxvt -e htop")
        , ("mi-go",      spawn "urxvt -e ssh mi-go.lan")
        , ("web",        spawn "firefox")
        , ("stripe",     spawnShell >*> 5)
        ]
    }

wsgrid = gridselect gsConfig <=< asks $ map (\x -> (x,x)) . workspaces . config

myNavigator :: TwoD a (Maybe a)
--myNavigator = defaultNavigation
-- custom gridselect keybindings (hjkl = htns)
myNavigator = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where navKeyMap = M.fromList [
           ((0,xK_Escape), cancel)
          ,((0,xK_Return), select)
          ,((0,xK_slash) , substringSearch myNavigator)
          ,((0,xK_h)     , move (-1,0)  >> myNavigator)
          ,((0,xK_t)     , move (0,1)   >> myNavigator)
          ,((0,xK_n)     , move (0,-1)  >> myNavigator)
          ,((0,xK_s)     , move (1,0)   >> myNavigator)
          ,((0,xK_space) , setPos (0,0) >> myNavigator)
          ]
        -- The navigation handler ignores unknown key symbols
        navDefaultHandler = const myNavigator

gsConfig = defaultGSConfig { gs_navigate = myNavigator }

promptedGoto  = wsgrid >>= flip whenJust (switchTopic myTopicConfig)
promptedShift = wsgrid >>= \x -> whenJust x $ \y -> windows (W.greedyView y . W.shift y)

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    , ((modMask              , xK_a     ), currentTopicAction myTopicConfig)
    , ((modMask              , xK_t     ), promptedGoto)
    , ((modMask .|. shiftMask, xK_t     ), promptedShift)

    -- launch dmenu
    , ((modMask,               xK_p     ), spawn "dmenu_run")

    -- change keyboard layout
    , ((modMask              , xK_equal ),        spawn "setxkbmap dvorak; xmodmap /home/bct/.xmodmaprc")
    , ((modMask              , xK_bracketright ), spawn "setxkbmap us")

    -- close focused window
    , ((modMask .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)

    -- Media buttons
    , ((0, xF86XK_AudioLowerVolume   ), spawn "amixer -D pulse sset Master 5%-")
    , ((0, xF86XK_AudioRaiseVolume   ), spawn "amixer -D pulse sset Master 5%+")
    , ((0, xF86XK_AudioMute          ), spawn "amixer -D pulse set Master toggle")

    -- Shift+Insert
    , ((mod4Mask             , xK_t     ), spawn "xdotool key --clearmodifiers Shift+Insert")

    -- Lock the screen
    , ((modMask .|. shiftMask, xK_l     ), spawn "slock")

    -- Switch to the previously active workspace
    , ((modMask,               xK_r     ), toggleWS)

    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modMask,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modMask,               xK_g     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modMask              , xK_q     ), restart "xmonad" True)

    , ((0,       xF86XK_MonBrightnessUp ), spawn "xbacklight +19")
    , ((0,       xF86XK_MonBrightnessDown ), spawn "xbacklight -19")

    ]

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
--     [((m .|. modMask, k), windows $ f i)
--        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
--        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    ++
     [ ((modMask, k), switchNthLastFocused myTopicConfig i)
     | (i, k) <- zip [1..] [xK_1 .. xK_9]]

     --
     -- mod-{o,e}, Switch to physical/Xinerama screens 1 or 2
     -- mod-shift-{w,e}, Move client to screen 1 or 2
     --
     --
    ++
     [((modMask .|. mask, key), f sc)
         | (key, sc) <- zip [xK_o, xK_e] [0..]
         , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]



spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = do
  t <- asks (terminal . config)
  spawnHere $ "cd " ++ dir ++ " && " ++ t

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
myLayout = avoidStruts $ smartBorders $ stripeWorkspace $ webWorkspace $ dashboard $ (tiled ||| Mirror tiled ||| tabbed ||| Full)
  where
     dashboard       = onWorkspace "?"      $ simpleDeco shrinkText defaultTheme (Grid ||| tiled)
     webWorkspace    = onWorkspace "web"    tabbed
     stripeWorkspace = onWorkspace "stripe" $ Mirror tiled

     tabbed   = tabbedAlways shrinkText tabTheme
     tabTheme = defaultTheme { fontName = "xft:Droid Sans:pixelsize=14" }

     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "mplayer2"       --> doFloat
    , className =? "mpv"            --> doFloat
    , className =? "feh"            --> doFloat
    , title     =? "QEMU"           --> doFloat
    ]

------------------------------------------------------------------------
-- Status bars and logging
--
myLogHook dzenPipe = dynamicLogWithPP $ myDzenPP Style.defaultStyle dzenPipe

myLogHookBar = defaultDzen
 -- use the default as a base and override width and
 -- colors
 { width   = Just $ Percent 100
 , font    = Just $ Style.defaultFont defaultStyle
 , height  = Just $ 22
 , fgColor = Just $ Style.dzenFGColor defaultStyle
 , bgColor = Just $ Style.dzenBGColor defaultStyle
 , screen  = Just 0
 }

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
--main = xmonad myConfig
main = do
  dzenPipe <- spawnDzen myLogHookBar

  xmonad $ myConfig dzenPipe

myConfig dzenPipe = defaultConfig {
  -- simple stuff
    terminal           = "urxvt",
    focusFollowsMouse  = True,
    borderWidth        = 1,

    normalBorderColor  = "#666666",
    focusedBorderColor = "#66aacc",

    modMask            = mod1Mask,
    workspaces         = myTopics,

  -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

  -- hooks, layouts
    layoutHook         = myLayout,
    manageHook         = myManageHook <+> manageDocks,
    logHook            = myLogHook dzenPipe
}

-- |
-- -- Helper function which provides ToggleStruts keybinding
-- --
toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
