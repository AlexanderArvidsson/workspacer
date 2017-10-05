{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack)
import Data.Maybe
import Data.List (intercalate, find)
import Data.Char (toUpper)
import System.Process
import GHC.Generics
import Options.Applicative
import Data.Monoid ((<>))

data Rectangle =
  Rectangle { x :: Int
            , y :: Int
            , width :: Int
            , height :: Int
            }
  deriving (Generic, Show)

data Workspace =
  Workspace { num :: Int
            , name :: String
            , visible :: Bool
            , focused :: Bool
            , rect :: Rectangle
            , output :: String
            , urgent :: Bool
            }
  deriving (Generic, Show)

instance FromJSON Rectangle
instance FromJSON Workspace

getWorkspaces :: String -> [Workspace]
getWorkspaces json
  | isNothing list = []
  | otherwise = fromJust list
  where list = decode (pack json) :: Maybe [Workspace]

getWorkspaceNames :: [Workspace] -> [String]
getWorkspaceNames = map name

readWorkspaces :: IO String
readWorkspaces = readProcess "i3-msg" ["-t", "get_workspaces"] ""

getCurrentWorkspace :: [Workspace] -> Maybe Workspace
getCurrentWorkspace workspaces = find focused workspaces

trim char = reverse . dropWhile (== char) . reverse

openMenu :: [Workspace] -> String -> Options -> IO String
openMenu workspaces title opts = do
  output <- case optMenu opts of
    "rofi" -> readProcess "rofi"
                  ["-width", "30", "-dmenu", "-p", title]
                  menu

    "dmenu" -> readProcess "dmenu" ["-i"] menu

  return (dropWhile (==':') (trim '\n' output))

  where menu = intercalate "\n" $ getWorkspaceNames workspaces

setWorkspace :: String -> IO ()
setWorkspace workspace = callCommand ("i3-msg workspace " ++ workspace)

swapWorkspace :: String -> String -> IO ()
swapWorkspace from to
  | from /= to = callCommand ("i3-msg '"
                           ++ "rename workspace " ++ from  ++ " to temporary; "
                           ++ "rename workspace " ++ to    ++ " to " ++ from ++ "; "
                           ++ "rename workspace temporary" ++ " to " ++ to ++ "'")
  | otherwise = error "Cannot swap workspace with itself"

moveContainerTo :: String -> IO ()
moveContainerTo workspace
  = callCommand ("i3-msg move container to workspace " ++ workspace)

runWithOptions :: Options -> IO ()
runWithOptions opts = do
  output <- readWorkspaces

  let workspaces = getWorkspaces output

  case optCommand opts of
    WorkspaceSet -> do
      newWorkspace <- openMenu workspaces "Switch to workspace:" opts
      setWorkspace newWorkspace

    WorkspaceSwap -> do
      newWorkspace <- openMenu workspaces "Swap current workspace with:" opts

      case getCurrentWorkspace workspaces of
        (Just current) -> do
          print (name current ++ " , " ++ newWorkspace)
          swapWorkspace (name current) newWorkspace

    WorkspaceMove -> do
      newWorkspace <- openMenu workspaces "Move container to workspace:" opts
      moveContainerTo newWorkspace

data Command = WorkspaceSet | WorkspaceMove | WorkspaceSwap
  deriving (Show)

data Options = Options { optCommand :: Command
                       , optMenu :: String }
  deriving (Show)

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

optsSet :: Parser Command
optsSet = pure WorkspaceSet

optsMove :: Parser Command
optsMove = pure WorkspaceMove

optsSwap :: Parser Command
optsSwap = pure WorkspaceMove

optsCommand :: Parser Command
optsCommand = subparser
  ( command "set"  (withInfo optsSet "Set the current workspace")
 <> command "swap" (withInfo optsSwap "Swap current workspace with another")
 <> command "move" (withInfo optsMove "Move current container to workspace") )

parseOptions :: Parser Options
parseOptions = Options <$> optsCommand
                       <*> strOption
                           ( long "menu"
                          <> short 'm'
                          <> help "What menu to use"
                          <> showDefault
                          <> value "rofi"
                          <> metavar "MENU" )

main :: IO ()
main = 
  runWithOptions =<<
    execParser (parseOptions `withInfo` "Workspace action menus using i3-msg")
