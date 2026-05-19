{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
module UI
  ( runApp
  ) where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Edit (Editor, editor, renderEditor, handleEditorEvent, getEditContents, editFocusedAttr)
import Control.Concurrent (forkIO)
import Control.Exception (try, SomeException)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import qualified Data.Set as Set
import Data.Set (Set)
import System.Process (readProcess)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import Languages
import Lens.Micro.Platform
import NixGen
import NixSearch
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)

-- Resource names

data Name
  = LangList
  | PkgList
  | SearchEdit
  | SearchList
  | DirEdit
  deriving (Eq, Ord, Show)

-- Custom events

data CuteEvent
  = SearchResults [NixResult]
  | SearchError String
  | GenerateDone String
  | CacheUpdateStarted Channel
  | CacheUpdateDone Channel [NixResult]
  | CacheUpdateFailed Channel String
  | CacheLoadDone Channel [NixResult]
  | CacheLoadFailed Channel String

-- App screen

data Screen
  = LangScreen    -- ^ pick a language
  | PkgScreen     -- ^ pick packages
  | SearchScreen  -- ^ nix search + custom add
  | DirScreen     -- ^ pick output directory
  | DoneScreen    -- ^ result message
  deriving (Eq, Show)

-- App state

data AppState = AppState
  { _screen         :: Screen
  , _langCursor     :: Int
  , _pkgCursor      :: Int
  , _pkgToggled     :: Set String       -- ^ toggled OFF from defaults
  , _pkgEnabled     :: Set String       -- ^ toggled ON extras
  , _pythonVenv     :: Bool             -- ^ toggle to enter virtualenv on shell hook
  , _searchEdit     :: Editor String Name
  , _searchResults  :: [NixResult]
  , _searchCursor   :: Int
  , _searchLoading  :: Bool
  , _searchError    :: Maybe String
  , _extraAttrs     :: [(String, Channel)] -- ^ user-added from search with their channel
  , _dirEdit        :: Editor String Name
  , _resultMsg      :: String
  , _arch           :: String
  , _activeChannel  :: Channel
  , _unstableCache  :: Maybe [NixResult]
  , _stableCache    :: Maybe [NixResult]
  , _cacheUpdating  :: Bool
  , _stableVersion  :: String
  } deriving (Show)

makeLensesFor [("_searchEdit", "searchEdit"), ("_dirEdit", "dirEdit")] ''AppState

-- UI Layout and Truncation Constants
langColWidth :: Int
langColWidth = 12

pkgColWidth :: Int
pkgColWidth = 30

searchAttrColWidth :: Int
searchAttrColWidth = 28

searchDescMaxLength :: Int
searchDescMaxLength = 55

-- Helpers

currentLang :: AppState -> Language
currentLang s = allLanguages !! (_langCursor s)

currentPkgs :: AppState -> [LangPackage]
currentPkgs s =
  let lang = currentLang s
      base = packagesFor lang
  in base

-- A package is "active" if (default AND not toggled off) OR explicitly enabled
isPkgActive :: AppState -> LangPackage -> Bool
isPkgActive s p
  | pkgDefault p = not (Set.member (pkgName p) (_pkgToggled s))
  | otherwise    = Set.member (pkgName p) (_pkgEnabled s)

selectedPkgNames :: AppState -> [LangPackage]
selectedPkgNames s = filter (isPkgActive s) (currentPkgs s)

targetDir :: AppState -> String
targetDir s = concat (getEditContents (_dirEdit s))

-- Colour theme

pink, lavender, mint, peach, fgDim :: AttrName
pink     = attrName "pink"
lavender = attrName "lavender"
mint     = attrName "mint"
peach    = attrName "peach"
fgDim    = attrName "fgDim"

selectedAttr, activeAttr, inactiveAttr, headerAttr, titleAttr, hintAttr :: AttrName
selectedAttr = attrName "selected"
activeAttr   = attrName "active"
inactiveAttr = attrName "inactive"
headerAttr   = attrName "header"
titleAttr    = attrName "title"
hintAttr     = attrName "hint"

rgb :: Int -> Int -> Int -> V.Color
rgb = V.rgbColor

theMap :: AttrMap
theMap = attrMap (V.white `on` V.black) $
  [ (pink,         fg (rgb 255 105 180))
  , (lavender,     fg (rgb 180 130 255))
  , (mint,         fg (rgb 100 230 180))
  , (peach,        fg (rgb 255 180 100))
  , (fgDim,        fg (rgb 120 120 140))
  , (selectedAttr, rgb 30 30 50 `on` rgb 180 130 255)
  , (activeAttr,   fg (rgb 100 230 180))
  , (inactiveAttr, fg (rgb 90 90 110))
  , (headerAttr,   V.withStyle (fg (rgb 255 105 180)) V.bold)
  , (titleAttr,    V.withStyle (fg (rgb 180 130 255)) V.bold)
  , (hintAttr,     fg (rgb 100 110 130))
  , (editFocusedAttr, rgb 20 20 40 `on` rgb 200 150 255)
  ]


-- Drawing

drawUI :: AppState -> [Widget Name]
drawUI s = case _screen s of
  LangScreen   -> [drawLangScreen s]
  PkgScreen    -> [drawPkgScreen s]
  SearchScreen -> [drawSearchScreen s]
  DirScreen    -> [drawDirScreen s]
  DoneScreen   -> [drawDoneScreen s]

-- Banner / header
drawBanner :: Widget Name
drawBanner = withAttr titleAttr $ vBox
  [ hCenter $ str "  ✿ cutesetup ✿  "
  , hCenter $ withAttr hintAttr (str "nix devshell scaffolding")
  ]

drawFooter :: String -> Widget Name
drawFooter hints = withAttr hintAttr $ hCenter $ str hints

-- Lang screen

drawLangScreen :: AppState -> Widget Name
drawLangScreen s = joinBorders $
  withBorderStyle unicode $
  border $
  vBox
  [ drawBanner
  , hBorder
  , hCenter $ withAttr headerAttr (str "Pick a language")
  , hCenter $ withAttr hintAttr (str "↑↓ navigate   Enter select   q quit")
  , hBorder
  , viewport LangList Vertical $ vBox (zipWith (drawLangRow s) [0..] allLanguages)
  , hBorder
  , drawFooter $ "  " ++ show (length allLanguages) ++ " languages available"
  ]

drawLangRow :: AppState -> Int -> Language -> Widget Name
drawLangRow s i lang =
  let sel = i == _langCursor s
      icon = langIcon lang
      name = show lang
      desc = langDescription lang
      row  = hBox
        [ str (if sel then " ▶ " else "   ")
        , withAttr peach (str icon)
        , str " "
        , withAttr (if sel then selectedAttr else titleAttr) (str (padStringRight langColWidth name))
        , str "  "
        , withAttr fgDim (str desc)
        ]
  in if sel
       then withAttr selectedAttr (padRight Max row)
       else padRight Max row

-- ── Pkg screen

drawPkgScreen :: AppState -> Widget Name
drawPkgScreen s =
  let lang    = currentLang s
      pkgs    = currentPkgs s
      nSel    = length (selectedPkgNames s)
      isPipActive = lang == Python && any (\p -> pkgName p == "python3Packages.pip") (selectedPkgNames s)
      hintStr = if isPipActive
                  then "↑↓ navigate   Space toggle   v toggle venv   s nix-search   Enter next   Esc back"
                  else "↑↓ navigate   Space toggle   s nix-search   Enter next   Esc back"
  in joinBorders $
     withBorderStyle unicode $
     border $
     vBox
     [ drawBanner
     , hBorder
     , hCenter $ withAttr headerAttr
         (str (langIcon lang ++ "  " ++ show lang ++ " packages"))
     , hCenter $ withAttr hintAttr
         (str hintStr)
     , hBorder
     , viewport PkgList Vertical $ vBox (zipWith (drawPkgRow s) [0..] pkgs)
     , drawVenvRecommendation s
     , hBorder
     , hBox
       [ withAttr mint (str ("  ✔  " ++ show nSel ++ " selected"))
       , str "   "
       , withAttr fgDim (str "s → nix search for more")
       ]
     ]

drawPkgRow :: AppState -> Int -> LangPackage -> Widget Name
drawPkgRow s i p =
  let sel     = i == _pkgCursor s
      active  = isPkgActive s p
      check   = if active then withAttr mint (str "[✔]") else withAttr inactiveAttr (str "[ ]")
      row = hBox
        [ str (if sel then " ▶ " else "   ")
        , check
        , str " "
        , withAttr (if active then activeAttr else inactiveAttr)
            (str (padStringRight pkgColWidth (pkgLabel p)))
        , str "  "
        , withAttr fgDim (str (pkgDesc p))
        ]
  in if sel
       then withAttr selectedAttr (padRight Max row)
       else padRight Max row

-- Search screen

drawSearchScreen :: AppState -> Widget Name
drawSearchScreen s =
  let extraCount = length (_extraAttrs s)
      extraNames = map fst (_extraAttrs s)
  in joinBorders $
     withBorderStyle unicode $
     border $
     vBox
     [ drawBanner
     , hBorder
     , hCenter $ withAttr headerAttr (str "❄  nix search")
     , hCenter $ withAttr hintAttr
         (str "Type to search   ↑↓ navigate   Enter add   Ctrl+s switch channel   Ctrl+u update cache   Esc back")
     , hBorder
     , hBox
       [ withAttr lavender (str "  search: ")
       , renderEditor (str . unlines) (_screen s == SearchScreen) (_searchEdit s)
       , fill ' '
       , withAttr titleAttr (str " channel: ")
       , case _activeChannel s of
           Unstable -> withAttr pink (str "[unstable]")
           Stable   -> withAttr mint (str "[stable]")
       , str "  "
       ]
     , hBorder
     , if _searchLoading s
         then hCenter (withAttr peach (str (case _searchError s of
                                              Just err -> err
                                              Nothing  -> "Loading packages cache…")))
         else case _searchError s of
           Just err -> hCenter (withAttr peach (str ("⚠️  " ++ err)))
           Nothing  -> drawSearchResults s
     , hBorder
     , hBox
       [ withAttr mint (str ("  ✔  " ++ show extraCount ++ " added:"))
       , str "  "
       , withAttr fgDim (str (intercalate ", " extraNames))
       ]
     , hBorder
     , drawFooter "  Enter → add to devshell   Esc → back to packages   Tab → continue"
     ]

drawSearchResults :: AppState -> Widget Name
drawSearchResults s =
  if null (_searchResults s)
    then hCenter $ withAttr fgDim (str "no results — try searching above")
    else viewport SearchList Vertical $
         vBox (zipWith (drawResultRow s) [0..] (_searchResults s))

drawResultRow :: AppState -> Int -> NixResult -> Widget Name
drawResultRow s i r =
  let sel     = i == _searchCursor s
      added   = any (\(attr, _) -> attr == nrAttr r) (_extraAttrs s)
      chanStr = case lookup (nrAttr r) (_extraAttrs s) of
        Just Unstable -> " [unstable]"
        Just Stable   -> " [stable]"
        Nothing       -> ""
      check   = if added then withAttr mint (str "[+]") else str "   "
      row = hBox
        [ str (if sel then " ▶ " else "   ")
        , check
        , str " "
        , withAttr lavender (str (padStringRight searchAttrColWidth (nrAttr r)))
        , withAttr hintAttr (str chanStr)
        , str "  "
        , withAttr fgDim (str (take searchDescMaxLength (nrDesc r)))
        ]
  in if sel
       then withAttr selectedAttr (padRight Max row)
       else padRight Max row

-- Dir screen

drawDirScreen :: AppState -> Widget Name
drawDirScreen s =
  joinBorders $
  withBorderStyle unicode $
  border $
  vBox
  [ drawBanner
  , hBorder
  , hCenter $ withAttr headerAttr (str " output directory")
  , hCenter $ withAttr hintAttr (str "Edit path   Enter generate   Esc back")
  , hBorder
  , hBox
    [ withAttr lavender (str "  dir: ")
    , renderEditor (str . unlines) True (_dirEdit s)
    ]
  , hBorder
  , drawSummary s
  , hBorder
  , drawFooter "  Enter → write flake.nix + .envrc"
  ]

drawSummary :: AppState -> Widget Name
drawSummary s =
  let lang  = currentLang s
      pkgs  = selectedPkgNames s
      extra = _extraAttrs s
      formatExtra (attr, chan) = attr ++ case chan of
        Unstable -> " (unstable)"
        Stable   -> " (stable)"
  in vBox
     [ hBox [ withAttr lavender (str "  language: ")
            , withAttr peach    (str (langIcon lang ++ " " ++ show lang))
            ]
     , hBox [ withAttr lavender (str "  packages: ")
            , withAttr mint     (str (intercalate ", " (map pkgLabel pkgs)))
            ]
     , if null extra then emptyWidget
       else hBox [ withAttr lavender (str "  extra:    ")
                 , withAttr mint     (str (intercalate ", " (map formatExtra extra)))
                 ]
     ]

-- Done screen

drawDoneScreen :: AppState -> Widget Name
drawDoneScreen s =
  joinBorders $
  withBorderStyle unicode $
  border $
  vBox
  [ drawBanner
  , hBorder
  , center $ vBox
    [ hCenter $ withAttr mint (str "✿ ✿ ✿  done!  ✿ ✿ ✿")
    , str ""
    , hCenter $ withAttr activeAttr (str (_resultMsg s))
    , str ""
    , hCenter $ withAttr hintAttr (str "Run:  direnv allow")
    , hCenter $ withAttr hintAttr (str "Then: nix develop")
    , str ""
    , hCenter $ withAttr peach (str "Press q or Enter to exit")
    ]
  ]

-- ─── Event handling ────────────────────────────────────────────────────────────

handleEvent :: CuteChan -> BrickEvent Name CuteEvent -> EventM Name AppState ()
handleEvent _ (AppEvent ev) = handleCustomEvent ev
handleEvent chan ev = do
  s <- get
  case _screen s of
    LangScreen   -> handleLangEvent chan ev
    PkgScreen    -> handlePkgEvent chan ev
    SearchScreen -> handleSearchEvent chan ev
    DirScreen    -> handleDirEvent chan ev
    DoneScreen   -> handleDoneEvent chan ev

handleCustomEvent :: CuteEvent -> EventM Name AppState ()
handleCustomEvent (SearchResults rs) = do
  modify $ \s -> s { _searchResults = rs
                   , _searchLoading = False
                   , _searchError   = Nothing
                   , _searchCursor  = 0 }
handleCustomEvent (SearchError err) = do
  modify $ \s -> s { _searchLoading = False
                   , _searchError   = Just err }
handleCustomEvent (GenerateDone msg) = do
  modify $ \s -> s { _screen = DoneScreen, _resultMsg = msg }
handleCustomEvent (CacheUpdateStarted c) = do
  modify $ \s -> s { _cacheUpdating = True, _searchLoading = True, _searchError = Just ("⏳ Downloading " ++ show c ++ " packages list…") }
handleCustomEvent (CacheUpdateDone c pkgs) = do
  modify $ \s ->
    let nextState = case c of
          Unstable -> s { _unstableCache = Just pkgs }
          Stable   -> s { _stableCache = Just pkgs }
    in nextState { _cacheUpdating = False, _searchLoading = False, _searchError = Nothing, _searchCursor = 0 }
handleCustomEvent (CacheUpdateFailed c err) = do
  modify $ \s -> s { _cacheUpdating = False, _searchLoading = False, _searchError = Just ("Failed to download " ++ show c ++ " cache: " ++ err) }
handleCustomEvent (CacheLoadDone c pkgs) = do
  modify $ \s ->
    let nextState = case c of
          Unstable -> s { _unstableCache = Just pkgs }
          Stable   -> s { _stableCache = Just pkgs }
    in nextState { _searchLoading = False, _searchError = Nothing, _searchCursor = 0 }
handleCustomEvent (CacheLoadFailed _ err) = do
  modify $ \s -> s { _searchLoading = False, _searchError = Just err }

-- Lang screen
handleLangEvent :: CuteChan -> BrickEvent Name CuteEvent -> EventM Name AppState ()
handleLangEvent _ (VtyEvent (V.EvKey V.KUp _)) = do
  modify $ \s -> s { _langCursor = max 0 (_langCursor s - 1) }
  vScrollBy (viewportScroll LangList) (-1)
handleLangEvent _ (VtyEvent (V.EvKey V.KDown _)) = do
  modify $ \s -> s { _langCursor = min (length allLanguages - 1) (_langCursor s + 1) }
  vScrollBy (viewportScroll LangList) 1
handleLangEvent _ (VtyEvent (V.EvKey V.KEnter _)) = do
  modify $ \s -> s
    { _screen     = PkgScreen
    , _pkgCursor  = 0
    , _pkgToggled = Set.empty
    , _pkgEnabled = Set.empty
    , _extraAttrs = []
    }
handleLangEvent _ (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt
handleLangEvent _ _ = return ()

-- Pkg screen
handlePkgEvent :: CuteChan -> BrickEvent Name CuteEvent -> EventM Name AppState ()
handlePkgEvent _ (VtyEvent (V.EvKey V.KUp _)) = do
  modify $ \s -> s { _pkgCursor = max 0 (_pkgCursor s - 1) }
  vScrollBy (viewportScroll PkgList) (-1)
handlePkgEvent _ (VtyEvent (V.EvKey V.KDown _)) = do
  modify $ \s -> s { _pkgCursor = min (length (currentPkgs s) - 1) (_pkgCursor s + 1) }
  vScrollBy (viewportScroll PkgList) 1
handlePkgEvent _ (VtyEvent (V.EvKey (V.KChar ' ') _)) = do
  s <- get
  let pkgs = currentPkgs s
      idx  = _pkgCursor s
      p    = pkgs !! idx
      name = pkgName p
  if pkgDefault p
    then modify $ \st ->
           if Set.member name (_pkgToggled st)
             then st { _pkgToggled = Set.delete name (_pkgToggled st) }
             else st { _pkgToggled = Set.insert name (_pkgToggled st) }
    else modify $ \st ->
           if Set.member name (_pkgEnabled st)
             then st { _pkgEnabled = Set.delete name (_pkgEnabled st) }
             else st { _pkgEnabled = Set.insert name (_pkgEnabled st) }
handlePkgEvent _ (VtyEvent (V.EvKey (V.KChar 'v') _)) = do
  s <- get
  let lang = currentLang s
      isPipActive = lang == Python && any (\p -> pkgName p == "python3Packages.pip") (selectedPkgNames s)
  when isPipActive $
    modify $ \st -> st { _pythonVenv = not (_pythonVenv st) }
handlePkgEvent chan (VtyEvent (V.EvKey (V.KChar 's') _)) = do
  modify $ \s -> s { _screen = SearchScreen }
  s2 <- get
  let activePkgs = case _activeChannel s2 of
        Unstable -> _unstableCache s2
        Stable   -> _stableCache s2
  case activePkgs of
    Nothing -> do
      modify $ \st -> st { _searchLoading = True, _searchError = Nothing }
      liftIO $ triggerCacheLoad chan (_stableVersion s2) (_activeChannel s2)
    Just _ -> return ()
handlePkgEvent _ (VtyEvent (V.EvKey V.KEnter _)) =
  modify $ \s -> s { _screen = DirScreen }
handlePkgEvent _ (VtyEvent (V.EvKey V.KEsc _)) =
  modify $ \s -> s { _screen = LangScreen }
handlePkgEvent _ (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt
handlePkgEvent _ _ = return ()

-- Search screen
handleSearchEvent :: CuteChan -> BrickEvent Name CuteEvent -> EventM Name AppState ()
handleSearchEvent _ (VtyEvent (V.EvKey V.KEsc _)) =
  modify $ \s -> s { _screen = PkgScreen }
handleSearchEvent _ (VtyEvent (V.EvKey (V.KChar '\t') [])) =
  modify $ \s -> s { _screen = DirScreen }
handleSearchEvent _ (VtyEvent (V.EvKey V.KUp _)) = do
  modify $ \s -> s { _searchCursor = max 0 (_searchCursor s - 1) }
  vScrollBy (viewportScroll SearchList) (-1)
handleSearchEvent _ (VtyEvent (V.EvKey V.KDown _)) = do
  modify $ \s -> s { _searchCursor = min (length (_searchResults s) - 1) (_searchCursor s + 1) }
  vScrollBy (viewportScroll SearchList) 1
handleSearchEvent _ (VtyEvent (V.EvKey V.KEnter _)) = do
  s <- get
  let results = _searchResults s
  when (not (null results)) $ do
    let r    = results !! _searchCursor s
        attr = nrAttr r
        isAdded = any (\(a, _) -> a == attr) (_extraAttrs s)
    if isAdded
      then modify $ \st -> st { _extraAttrs = filter (\(a, _) -> a /= attr) (_extraAttrs st) }
      else modify $ \st -> st { _extraAttrs = _extraAttrs st ++ [(attr, _activeChannel st)] }
handleSearchEvent chan (VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl])) = do
  s <- get
  when (not (_cacheUpdating s)) $ do
    let nextChan = case _activeChannel s of
          Unstable -> Stable
          Stable   -> Unstable
    modify $ \st -> st { _activeChannel = nextChan, _searchResults = [], _searchCursor = 0 }
    s2 <- get
    let activePkgs = case nextChan of
          Unstable -> _unstableCache s2
          Stable   -> _stableCache s2
    case activePkgs of
      Nothing -> do
        modify $ \st -> st { _searchLoading = True, _searchError = Nothing }
        liftIO $ triggerCacheLoad chan (_stableVersion s2) nextChan
      Just pkgs -> do
        let q = concat (getEditContents (_searchEdit s2))
            results = searchLocal q pkgs
        modify $ \st -> st { _searchResults = results, _searchError = Nothing }
handleSearchEvent chan (VtyEvent (V.EvKey (V.KChar 'u') [V.MCtrl])) = do
  s <- get
  when (not (_cacheUpdating s)) $ do
    liftIO $ triggerCacheUpdate chan (_stableVersion s) (_activeChannel s)
handleSearchEvent _ ev = zoom searchEdit $ handleEditorEvent ev

-- Dir screen
handleDirEvent :: CuteChan -> BrickEvent Name CuteEvent -> EventM Name AppState ()
handleDirEvent _ (VtyEvent (V.EvKey V.KEsc _)) =
  modify $ \s -> s { _screen = PkgScreen }
handleDirEvent _ (VtyEvent (V.EvKey V.KEnter _)) = do
  s <- get
  let dir   = targetDir s
      lang  = currentLang s
      pkgs  = selectedPkgNames s
      extra = _extraAttrs s
      cfg   = GenerateConfig
        { gcLang         = lang
        , gcPackages     = pkgs
        , gcExtraAttrs   = extra
        , gcTargetDir    = if null dir then "." else dir
        , gcSystemArch   = _arch s
        , gcPythonVenv   = _pythonVenv s && (lang == Python) && any (\p -> pkgName p == "python3Packages.pip") pkgs
        , gcNixpkgsStable = _stableVersion s
        }
  result <- liftIO $ generateBoth cfg
  case result of
    Right msg -> modify $ \st -> st { _screen = DoneScreen, _resultMsg = msg }
    Left err  -> modify $ \st -> st { _resultMsg = "✖ " ++ err, _screen = DoneScreen }
handleDirEvent _ ev = zoom dirEdit $ handleEditorEvent ev

-- Done screen
handleDoneEvent :: CuteChan -> BrickEvent Name CuteEvent -> EventM Name AppState ()
handleDoneEvent _ (VtyEvent (V.EvKey V.KEnter _)) = halt
handleDoneEvent _ (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt
handleDoneEvent _ _ = return ()

-- Cache helpers

triggerCacheLoad :: CuteChan -> String -> Channel -> IO ()
triggerCacheLoad chan version c = void $ forkIO $ do
  res <- loadCache version c
  case res of
    Right pkgs -> writeBChan chan (CacheLoadDone c pkgs)
    Left _     -> writeBChan chan (CacheLoadFailed c "Cache not found. Press Ctrl+u to download.")

triggerCacheUpdate :: CuteChan -> String -> Channel -> IO ()
triggerCacheUpdate chan version c = void $ forkIO $ do
  writeBChan chan (CacheUpdateStarted c)
  res <- updateCache version c
  case res of
    Left err -> writeBChan chan (CacheUpdateFailed c err)
    Right _  -> do
      loadRes <- loadCache version c
      case loadRes of
        Right pkgs -> writeBChan chan (CacheUpdateDone c pkgs)
        Left err   -> writeBChan chan (CacheUpdateFailed c ("Failed to load cache after download: " ++ err))

-- Initial state

initialState :: FilePath -> String -> String -> AppState
initialState cwd arch version = AppState
  { _screen        = LangScreen
  , _langCursor    = 0
  , _pkgCursor     = 0
  , _pkgToggled    = Set.empty
  , _pkgEnabled    = Set.empty
  , _pythonVenv    = True
  , _searchEdit    = editor SearchEdit (Just 1) ""
  , _searchResults = []
  , _searchCursor  = 0
  , _searchLoading = False
  , _searchError   = Nothing
  , _extraAttrs    = []
  , _dirEdit       = editor DirEdit (Just 1) cwd
  , _resultMsg     = ""
  , _arch          = arch
  , _activeChannel = Unstable
  , _unstableCache = Nothing
  , _stableCache   = Nothing
  , _cacheUpdating = False
  , _stableVersion = version
  }

-- BChan-aware search trigger

type CuteChan = BChan CuteEvent

appWithSearch :: CuteChan -> App AppState CuteEvent Name
appWithSearch chan = App
  { appDraw         = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEventWithChan chan
  , appStartEvent   = return ()
  , appAttrMap      = const theMap
  }

handleEventWithChan :: CuteChan -> BrickEvent Name CuteEvent -> EventM Name AppState ()
handleEventWithChan chan ev = do
  s <- get
  -- Intercept search-screen typing to do local search
  case (_screen s, ev) of
    (SearchScreen, VtyEvent (V.EvKey k mods))
      | isTypingKey k mods -> do
          zoom searchEdit $ handleEditorEvent ev
          s2 <- get
          let q = concat (getEditContents (_searchEdit s2))
              activePkgs = case _activeChannel s2 of
                Unstable -> _unstableCache s2
                Stable   -> _stableCache s2
          case activePkgs of
            Nothing -> modify $ \st -> st { _searchResults = [], _searchError = Just "Cache not loaded. Press Ctrl+u to download." }
            Just pkgs -> do
              let results = searchLocal q pkgs
              modify $ \st -> st { _searchResults = results, _searchError = Nothing, _searchCursor = 0 }
    _ -> handleEvent chan ev
  where
    isTypingKey (V.KChar _) [] = True
    isTypingKey V.KBS      [] = True
    isTypingKey V.KDel     [] = True
    isTypingKey _          _  = False

-- Entry point

runApp :: IO ()
runApp = do
  cwd  <- getCurrentDirectory
  arch <- detectArch
  chan <- newBChan 10

  envStable <- lookupEnv "NIX_STABLE_VERSION"
  let stableVersion = maybe "nixos-25.11" id envStable

  let buildVty = VCP.mkVty V.defaultConfig
  initialVty <- buildVty

  void $ customMain initialVty buildVty (Just chan)
    (appWithSearch chan)
    (initialState cwd arch stableVersion)

detectArch :: IO String
detectArch = do
  res <- tryReadProcess "uname" ["-m"] ""
  case res of
    Left _  -> return "x86_64-linux"
    Right r ->
      let m = filter (/= '\n') r
      in return $ case m of
           "x86_64"  -> "x86_64-linux"
           "aarch64" -> "aarch64-linux"
           _         -> "x86_64-linux"

tryReadProcess :: String -> [String] -> String -> IO (Either String String)
tryReadProcess cmd args inp = do
  res <- try @SomeException (readProcess cmd args inp)
  return $ case res of
    Left e  -> Left (show e)
    Right r -> Right r

-- Utility

padStringRight :: Int -> String -> String
padStringRight n s
  | length s >= n = take n s
  | otherwise     = s ++ replicate (n - length s) ' '

drawVenvRecommendation :: AppState -> Widget Name
drawVenvRecommendation s =
  let lang = currentLang s
      isPipActive = lang == Python && any (\p -> pkgName p == "python3Packages.pip") (selectedPkgNames s)
  in if isPipActive
       then vBox
            [ hBorder
            , padLeftRight 2 $ withAttr peach $ border $ vBox
              [ hCenter $ withAttr pink (str " Highly Recommended for Pip")
              , hCenter $ str "Using pip inside a Nix environment can cause externally-managed-environment errors."
              , hCenter $ str "We highly recommend auto-creating and activating a virtual environment."
              , str ""
              , hCenter $ hBox
                [ withAttr mint (str (if _pythonVenv s then "[✔] " else "[ ] "))
                , withAttr titleAttr (str "Auto-create & enter virtualenv in flake shellHook")
                , withAttr hintAttr (str " (Press 'v' to toggle)")
                ]
              ]
            ]
       else emptyWidget
