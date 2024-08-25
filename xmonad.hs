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
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: [Char]
myTerminal = "kitty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth = 4

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
myNormalBorderColor = "#2e4f6a" -- "#413c38" -- #202430

myFocusedBorderColor :: [Char]
myFocusedBorderColor = "#ff898f" -- "#d38162" -- "#ada193" -- "#43473e"

toggleWindowInAllWorkspaces = do
  copies <- wsContainingCopies
  if null copies then windows copyToAll else killAllOtherCopies

-- myStatusBar = "xmobar"
-- myStatusBarPP = xmobarPP { ppCurrent = xmobarColor "#453a62" "" }

myTerminalCmd = "emacsclient -c -eval '(progn (switch-to-buffer \"terminal\") (multi-term-next))'"

summonNSP = namedScratchpadAction myScratchpads

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys c@(XConfig {XMonad.modMask = modm}) =
  mkKeymap c $
    [ ("M-S-<Return>", spawn myTerminalCmd),
      ("M-M1-<Return>", spawn myTerminal),
      ("M-p", spawn "rofi -show run"),
      ("M-w", spawn "rofi -show-icons -show window"),
      ("M-c", spawn "rofi -show calc -modi calc -no-show-match -no-sort -no-persistent-history"),
      ("M-S-c", kill),
      ("M-i", spawn "flameshot gui"),
      ("M-S-i", spawn "deepin-screen-recorder"),
      ("M-C-i", spawn "xcolor -s clipboard"),
      ("M-;", spawn "emacsclient -c"),
      ("M-S-t", summonNSP "telegram"),
      ("M-S-s", summonNSP "spotify"),
      ("M-S-s", summonNSP "spotify"),
      ("M-S-f", summonNSP "slack"),
      ("M-S-d", summonNSP "discord"),
      ("M-<Space>", sendMessage NextLayout),
      ("M-S-<Space>", setLayout $ XMonad.layoutHook c),
      ("M-s", toggleWindowInAllWorkspaces),
      ("M-n", refresh),
      ("M-j", windows W.focusDown),
      ("M-k", windows W.focusUp),
      ("M-m", windows W.focusMaster),
      ("M-<Return>", windows W.swapMaster),
      ("M-S-j", windows W.swapDown),
      ("M-S-k", windows W.swapUp),
      ("M-h", sendMessage Shrink),
      ("M-l", sendMessage Expand),
      ("M-S-h", sendMessage MirrorShrink),
      ("M-S-l", sendMessage MirrorExpand),
      ("M-t", withFocused $ windows . W.sink),
      ("M-,", sendMessage (IncMasterN (-1))),
      ("M-.", sendMessage (IncMasterN 1)),
      ("M-q", spawn "xmonad --recompile;")
    ]
      ++ [ (mod ++ show k, windows $ f i)
         | (i, k) <- zip (XMonad.workspaces c) [1 .. 9],
           (mod, f) <- [("M-", W.greedyView), ("M-S-", W.shift)]
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
myLayout = smartBorders $ smartSpacingWithEdge 6 $ (tiled ||| Mirror tiled ||| threeColumns ||| Grid ||| spiral (6 / 7) ||| noBorders Full)
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
    threeColumns = ThreeCol 1 (3 / 100) (1 / 3)

myShowWNameConf :: SWNConfig
myShowWNameConf =
  def
    { swn_font = "xft:Poor Story:bold:size=30",
      swn_fade = 0.3,
      swn_bgcolor = "#373c3a", -- "#c4cbd4" -- "#453736" -- "#1e2030"
      swn_color = "#bfbcaa" -- "#1b2b27" -- "#ada193" -- "#5b6078"
    }

------------------------------------------------------------------------
-- My ScratchPads
myScratchpads =
  [ NS "spotify" "spotify" (getClassName "Spotify") defaultSize,
    NS "telegram" "telegram-desktop" (getClassName "TelegramDesktop") defaultSize,
    NS "discord" "discord" (getClassName "discord") defaultSize,
    NS "zulip" "zulip" (getClassName "zulip") defaultSize,
    NS "slack" "slack" (getClassName "Slack") defaultSize
  ]
  where
    getClassName app = className =? app
    defaultSize = customFloating $ W.RationalRect 0.30 0.20 0.37 0.65

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
      title =? "Spotify" --> doFloat,
      title =? "Jitsi Meet" --> doFloat,
      (className =? "TelegramDesktop" <&&> title =? "Media viewer") --> doFloat,
      className =? "Spacedrive" --> doFloat
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
