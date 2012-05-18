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

import XMonad.Layout.Tabbed
import XMonad.Util.Themes

import XMonad.Actions.CycleWS(prevWS, nextWS, toggleWS)

import XMonad.Actions.TopicSpace

import XMonad.Actions.SpawnOn(spawnHere, shellPromptHere, spawnOn)

import Control.Monad(Monad(return, (>>=), (>>)), Functor(..), (=<<), mapM, sequence, (<=<), zipWithM_)
import XMonad.Actions.GridSelect

import XMonad.Layout.Reflect

-- The list of all topics/workspaces of your xmonad configuration.
-- The order is important, new topics must be inserted
-- at the end of the list if you want hot-restarting
-- to work.
myTopics :: [Topic]
myTopics =
 [ "?" , "web", "mail", "mi-go", "pdf", "doc", "ebook", "gimp", "photos", "uzbl", "vm", "ciibis", "wememe" ]

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
    { topicDirs = M.fromList $
        [ ("uzbl",   "projects/uzbl")
        , ("pdf",    "pdf")
        , ("ciibis", "irl/ciibis")
        ]

    , defaultTopicAction = const spawnShell

    , defaultTopic = "?"
    , maxTopicHistory = 10

    , topicActions = M.fromList $
        [

          ("?",          spawnShell >>
                         spawn "urxvtc -e htop")
        , ("mail",       spawn "urxvtc -e mutt")
        , ("web",        spawn "uzbl-browser")
        , ("mi-go",      spawn "urxvtc -e ssh necronomicorp.com")
        , ("ciibis",     spawnShell)

        , ("pdf",        spawn "epdfview")
        , ("gajim",      spawnShell >*> 4)
        , ("movie",      spawn "uzbl-browser http://mi-go:5678/")

--        , ("windows",    spawn "urxvtc -e sudo qemu-system-x86_64 -hda xp.img -m 256 -soundhw all")

        , ("ebook",      spawn "calibre")

        , ("gimp",      spawn "gimp")

        , ("photos",     spawn "geeqie")

        --, ("conf",       spawnShell >> spawnShellIn "wd/ertai/private")
        --, ("darcs",      spawnShell >*> 3)
        --, ("haskell",    spawnShell >*> 2 >>
        --                 spawnShellIn "wd/dev-haskell/ghc")
        --, ("documents",  spawnShell >*> 2 >>
        --                 spawnShellIn "Documents" >*> 2)
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
    , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

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

    , ((0,                     0x1008ff26), prevWS) -- XfF86Back
    , ((0,                     0x1008ff27), nextWS) -- XfF86Forward

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
myLayout = avoidStruts $ webWorkspace $ gajimWorkspace $ imWorkspace $ dashboard $ gimp $ (tiled ||| Mirror tiled ||| tabbed ||| Full)
  where
     dashboard       = onWorkspace "?"      Grid
     imWorkspace     = onWorkspace "talk"  $ gridIM (1%7) (Role "roster")
     gajimWorkspace  = onWorkspace "gajim" $ gridIM (1%7) (Role "roster")
     webWorkspace    = onWorkspace "web"    tabbed

     gimp            = onWorkspace "gimp" $ withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") Full

     tabbed   = tabbedAlways (shrinkText) tabTheme
     tabTheme = defaultTheme { fontName = "-*-liberation serif-medium-r-normal--13-0-0-0-p-0-*" }

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
    , className =? "feh"            --> doFloat
    , title     =? "QEMU"           --> doFloat
    ]

------------------------------------------------------------------------
-- Status bars and logging

freeMemory = logCmd "/usr/bin/free -m | awk '/buffers\\/cache/ { print $3 }'"

myLogHook = dynamicLogWithPP $ defaultPP {
                ppCurrent = dzenColor "white" "#6688bb" . pad
              , ppVisible = dzenColor "white" "black"   . pad
              , ppUrgent  = dzenColor "black" "yellow"   . dzenStrip
              , ppTitle = dzenEscape
              , ppExtras = [ loadAvg, freeMemory, date "%m-%d %X" ]
            }

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
main = xmonad $ withUrgencyHook NoUrgencyHook $ myConfig

myConfig = defaultConfig {
  -- simple stuff
    terminal           = "urxvtc",
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
    logHook            = myLogHook
}
