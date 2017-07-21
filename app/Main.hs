module Main where

import UI (playGame)
import Dino (Game(..))

import Control.Monad (when)
import Data.Monoid ((<>))
import System.Exit (exitSuccess)
import Text.Read (readMaybe)

import Options.Applicative
import qualified System.Directory as D
import System.FilePath ((</>))

newtype Opts = Opts { score :: Bool }

opts :: Parser Opts
opts = Opts
  <$> switch (long "high-score" <> short 's' <> help "Print highscore and exit")

dinoHeader :: String
dinoHeader = "Dino - a subpar ripoff of chrome's infinite scroller"

dinoFooter :: String
dinoFooter = "Controls - WS or arrow keys to move. p to pause, r to restart, q to quit"

fullOpts :: ParserInfo Opts
fullOpts = info (helper <*> opts) (fullDesc <> header dinoHeader <> footer dinoFooter)

-- Basically copied from tetris example
main :: IO ()
main = do
  (Opts hs) <- execParser fullOpts
  when hs (getHighScore >>= printM >> exitSuccess) -- show high score and exit
  g <- playGame
  handleEndGame (_highscore g)

-- Copied from tetris example
handleEndGame :: Int -> IO ()
handleEndGame s = do
  mhs <- getHighScore
  case mhs of
    Nothing -> newHighScore
    Just hs -> if s <= hs then justShowScore else newHighScore
  where
    justShowScore = putStrLn $ "Your final score: " ++ show s
    newHighScore = do
      putStrLn $ "Congrats! You got the new highest score: " ++ show s
      setHighScore s

-- High score stuff
-- Copied from tetris example
getHighScore :: IO (Maybe Int)
getHighScore = do
  lb <- getLeaderboardFile
  exists <- D.doesFileExist lb
  if exists
     then readMaybe <$> readFile lb
     else return Nothing

-- Copied from tetris example
setHighScore :: Int -> IO ()
setHighScore s = do
  lb <- getLeaderboardFile
  writeFile lb (show s)

-- Copied from tetris example
getLeaderboardFile :: IO FilePath
getLeaderboardFile = do
  xdg <- D.getXdgDirectory D.XdgData "dino"
  D.createDirectoryIfMissing True xdg
  return (xdg </> "leaderboard")

-- Utilities
-- Copied from tetris example
printM :: Show a => Maybe a -> IO ()
printM Nothing  = putStrLn "None"
printM (Just s) = print s
