-- File: app/Main.hs
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Text.RawString.QQ (r)
import Scaffolder (scaffold)

-- | Parse command-line arguments (in Main) expecting a "--new <directory>" flag pair.
parseArgs :: [String] -> IO FilePath
parseArgs args = case args of
  [] -> usageAndExit "Error: no arguments provided."
  _  -> findNew args
  where
    findNew [] = usageAndExit "Error: '--new' flag not found."
    findNew ("--new":dir:_)
      | take 2 dir == "--" = usageAndExit "Error: no valid directory specified after '--new'"
      | otherwise          = return dir
    findNew (_:rest) = findNew rest

    usageAndExit err = do
      hPutStrLn stderr err
      hPutStrLn stderr "Usage: scaffolds --new <directory>"
      exitFailure

main :: IO ()
main = do
  -- Print ASCII art banner using a raw string literal.
  let asciiArt = [r|

  ░▒▓███████▓▒░░▒▓██████▓▒░ ░▒▓██████▓▒░░▒▓████████▓▒░▒▓████████▓▒░▒▓██████▓▒░░▒▓█▓▒░      ░▒▓███████▓▒░ ░▒▓███████▓▒░
 ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
 ░▒▓█▓▒░      ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
  ░▒▓██████▓▒░░▒▓█▓▒░      ░▒▓████████▓▒░▒▓██████▓▒░ ░▒▓██████▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░░▒▓██████▓▒░
        ░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░      ░▒▓█▓▒░
        ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░      ░▒▓█▓▒░
 ░▒▓███████▓▒░ ░▒▓██████▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░      ░▒▓██████▓▒░░▒▓████████▓▒░▒▓███████▓▒░░▒▓███████▓▒░

         ▗▖   ▄   ▄     ▗▄▄▖ ▄   ▄ ▗▞▀▜▌▄▄▄▄       ▗▄▄▖▗▞▀▚▖ ▄▄▄ ▗▞▀▜▌ ▄▄▄ ▐▌    ▗▖ ▗▖▄ █  ▄▄▄  ▄▄▄  ▄▄▄▄
         ▐▌   █   █     ▐▌ ▐▌█   █ ▝▚▄▟▌█   █     ▐▌   ▐▛▀▀▘█    ▝▚▄▟▌█    ▐▌    ▐▌ ▐▌▄ █ ▀▄▄  █   █ █   █
         ▐▛▀▚▖ ▀▀▀█     ▐▛▀▚▖ ▀▀▀█      █   █     ▐▌▝▜▌▝▚▄▄▖█         █ ▗▞▀▜▌    ▐▌ ▐▌█ █ ▄▄▄▀ ▀▄▄▄▀ █   █
         ▐▙▄▞▘▄   █     ▐▌ ▐▌▄   █                ▝▚▄▞▘                 ▝▚▄▟▌    ▐▙█▟▌█ █
               ▀▀▀            ▀▀▀
  |]

  putStrLn asciiArt

  let version = "2.0.26-1"
  putStrLn $ "Version: " ++ version

  args <- getArgs
  targetPath <- parseArgs args
  scaffold targetPath



