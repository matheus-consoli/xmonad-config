import qualified Data.Map as M
import Data.Monoid
import System.Exit
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spiral
import XMonad.Layout.ShowWName
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: [Char]
myTerminal = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth = 6

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt"). You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask :: KeyMask
myModMask = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces :: [[Char]]
myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]

-- Border colors for unfocused and focused windows, respectively.
--
--- my colors -------------------
-- myNormalBorderColor :: [Char]
-- myNormalBorderColor = "#060611"

-- myFocusedBorderColor :: [Char]
-- myFocusedBorderColor = "#453a62"
----------------------------------

-- catppuccin inspired
-- myNormalBorderColor :: [Char]
-- myNormalBorderColor = "#181926"

-- myFocusedBorderColor :: [Char]
-- myFocusedBorderColor = "#f38ba8"

-- dark academia
myNormalBorderColor :: [Char]
myNormalBorderColor = "#202430"

myFocusedBorderColor :: [Char]
myFocusedBorderColor = "#43473e"


toggleWindowInAllWorkspaces = do
  copies <- wsContainingCopies
  if null copies then windows copyToAll else killAllOtherCopies

-- myStatusBar = "xmobar"
-- myStatusBarPP = xmobarPP { ppCurrent = xmobarColor "#453a62" "" }

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
    [
      -- launch my emacs terminal
      ( (modm .|. shiftMask, xK_Return),
        spawn "emacsclient -c -eval '(progn (switch-to-buffer \"terminal\") (multi-term-next))'"
      ),
      -- launch a new terminal
      ( (modm .|. mod1Mask, xK_Return),
        spawn myTerminal
      ),
      -- launch dmenu
      ( (modm, xK_p),
        spawn "rofi -show run"
      ),
      -- launch window selector
      ( (modm, xK_w),
        spawn "rofi -show-icons -show window"
      ),
      -- launch window selector
      ( (modm, xK_c),
        spawn "rofi -show calc -modi calc -no-show-match -no-sort -no-persistent-history"
      ),
      -- close focused window
      ( (modm .|. shiftMask, xK_c),
        kill
      ),
      -- crop the screen
      ( (modm, xK_i),
        spawn "flameshot gui"
      ),
      -- print the entire screen
      ( (modm .|. shiftMask, xK_i),
        spawn "flameshot full -p ~/Pictures"
      ),
      -- launch emacs
      ( (modm, xK_semicolon),
        spawn "emacsclient -c"
      ),
      -- launch xcolor
      ( (modm .|. controlMask, xK_i),
        spawn "xcolor -s clipboard"
      ),
      -- launch scratchpad with telegram
      ( (modm .|. shiftMask, xK_t),
        namedScratchpadAction myScratchpads "telegram"
      ),
      -- launch scratchpad with spotify
      ( (modm .|. shiftMask, xK_s),
        namedScratchpadAction myScratchpads "spotify"
      ),
      -- launch scratchpad with discord
      ( (modm .|. shiftMask, xK_d),
        namedScratchpadAction myScratchpads "discord"
      ),
      -- launch scratchpad with zulip
      ( (modm .|. shiftMask, xK_z),
        namedScratchpadAction myScratchpads "zulip"
      ),
      -- launch scratchpad with slack
      ( (modm .|. shiftMask, xK_f),
        namedScratchpadAction myScratchpads "slack"
      ),
      -- Rotate through the available layout algorithms
      ( (modm, xK_space),
        sendMessage NextLayout
      ),
      -- Reset the layouts on the current workspace to default
      ( (modm .|. shiftMask, xK_space),
        setLayout $ XMonad.layoutHook conf
      ),
      -- copy the focused window to all workspaces (util to use with floating windows)
      ( (modm, xK_s),
        toggleWindowInAllWorkspaces
      ),
      -- Resize viewed windows to the correct size
      ( (modm, xK_n),
        refresh
      ),
      -- Move focus to the next window
      ( (modm, xK_Tab),
        windows W.focusDown
      ),
      -- Move focus to the next window
      ( (modm, xK_j),
        windows W.focusDown
      ),
      -- Move focus to the previous window
      ( (modm, xK_k),
        windows W.focusUp
      ),
      -- Move focus to the master window
      ( (modm, xK_m),
        windows W.focusMaster
      ),
      -- Swap the focused window and the master window
      ( (modm, xK_Return),
        windows W.swapMaster
      ),
      -- Swap the focused window with the next window
      ( (modm .|. shiftMask, xK_j),
        windows W.swapDown
      ),
      -- Swap the focused window with the previous window
      ( (modm .|. shiftMask, xK_k),
        windows W.swapUp
      ),
      -- Shrink the master area
      ( (modm, xK_h),
        sendMessage Shrink
      ),
      -- Expand the master area
      ( (modm, xK_l),
        sendMessage Expand
      ),
      -- Shrink the master area
      ( (modm .|. shiftMask, xK_h),
        sendMessage MirrorShrink
      ),
      -- Expand the master area vertically
      ( (modm .|. shiftMask, xK_l),
        sendMessage MirrorExpand
      ),
      -- Push window back into tiling
      ( (modm, xK_t),
        withFocused $ windows . W.sink
      ),
      -- Increment the number of windows in the master area
      ( (modm, xK_comma),
        sendMessage (IncMasterN 1)
      ),
      -- Deincrement the number of windows in the master area
      ( (modm, xK_period),
        sendMessage (IncMasterN (-1))
      ),
      -- Quit xmonad
      ( (modm .|. shiftMask, xK_q),
        io (exitWith ExitSuccess)
      ),
      -- Restart xmonad
      ( (modm, xK_q),
        spawn "xmonad --recompile; killall xmobar; xmonad --restart"
      )
    ]
      ++
      --
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      --
      [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]

-- ++
--
-- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2 or 3
-- mod-shift-{w,e,r}, Move client to screen 1, 2 or 3
--
--    [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
--   | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
--   , (f  , m ) <- [(W.view, 0), (W.shift, shiftMask)]
--   ]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (modm, button1),
        (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
      ),
      -- mod-button2, Raise the window to the top of the stack
      ( (modm, button2),
        (\w -> focus w >> windows W.shiftMaster)
      ),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ( (modm, button3),
        (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
      )
      -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts. Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = smartBorders $ smartSpacingWithEdge 3 $ (tiled ||| Mirror tiled ||| threeColumns ||| Grid ||| spiral (6 / 7) ||| noBorders Full)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = ResizableTall nmaster delta ratio []

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio = 1 / 2

    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100

    -- three columns
    threeColumns = ThreeCol 1 (3/100) (1/3)


myShowWNameConf :: SWNConfig
myShowWNameConf =
  def
    { swn_font = "xft:Comfortaa:bold:size=20"
    , swn_fade = 0.5
    , swn_bgcolor = "#1e2030"
    , swn_color = "#5b6078"
    }


------------------------------------------------------------------------
-- My ScratchPads
myScratchpads =
  [ NS "spotify" "spotify" (getClassName "Spotify") defaultSize
  , NS "telegram" "telegram-desktop" (getClassName "TelegramDesktop") defaultSize
  , NS "discord" "discord" (getClassName "discord") defaultSize
  , NS "zulip" "zulip" (getClassName "zulip") defaultSize
  , NS "slack" "slack" (getClassName "Slack") defaultSize
  ]
  where
    getClassName app = className =? app
    defaultSize = customFloating $ W.RationalRect 0.05 0.05 0.90 0.90

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook :: Query (Endo WindowSet)
myManageHook =
  composeAll
    [ className =? "MPlayer" --> doFloat,
      className =? "Gimp" --> doFloat,
      resource =? "desktop_window" --> doIgnore,
      (className =? "firefox" <&&> resource =? "Dialog") --> doFloat,
      title =? "Spotify" --> doFloat
    ]
    <+> namedScratchpadManageHook myScratchpads

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook

--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = handleEventHook def -- <+> ewmhFullscreen

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
-- Run xmonad with the settings you specify. No need to modify this.
--
main :: IO ()
main =
  xmonad $
    ewmhFullscreen $
      ewmh $
        def
          { -- simple stuff
            terminal = myTerminal,
            focusFollowsMouse = myFocusFollowsMouse,
            clickJustFocuses = myClickJustFocuses,
            borderWidth = myBorderWidth,
            modMask = myModMask,
            workspaces = myWorkspaces,
            normalBorderColor = myNormalBorderColor,
            focusedBorderColor = myFocusedBorderColor,
            -- key bindings
            keys = myKeys,
            mouseBindings = myMouseBindings,
            -- hooks, layouts
            layoutHook = showWName' myShowWNameConf myLayout,
            manageHook = myManageHook,
            handleEventHook = myEventHook
          }
