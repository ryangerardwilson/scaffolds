-- File: src/Scaffolder.hs
{-# LANGUAGE CPP #-}
module Scaffolder
  ( scaffold
  , writeFileWithInfo
  , ensureDir
  ) where

import qualified Data.ByteString as BS
import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.FilePath ((</>))
import Control.Monad (when)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

#ifndef mingw32_HOST_OS
import System.Posix.Files ( getFileStatus, setFileMode, fileMode
                          , ownerExecuteMode, groupExecuteMode, otherExecuteMode
                          )
import Data.Bits ((.|.))
#endif

import Templates

-- | Write a file and print an informational message.
writeFileWithInfo :: FilePath -> BS.ByteString -> IO ()
writeFileWithInfo path content = do
  BS.writeFile path content
  putStrLn $ "[INFO] scaffolds - Created or updated file: " ++ path

-- | Ensure that a directory exists.
ensureDir :: FilePath -> IO ()
ensureDir = createDirectoryIfMissing True

-- | The main scaffolding function: creates directories and writes out embedded files.
scaffold :: FilePath -> IO ()
scaffold targetDir = do
  ensureDir targetDir
  let fullPath sub = targetDir </> sub

  writeFileWithInfo (fullPath "main.ml") fileMainExtMl
  writeFileWithInfo (fullPath ".env") extEnv
  writeFileWithInfo (fullPath ".gitignore") extGitignore
  writeFileWithInfo (fullPath ".tailwind_build_input") extTailwindBuildInput
  writeFileWithInfo (fullPath "compiler") fileCompiler

  ensureDir (fullPath $ "dbs" </> "logs")
  writeFileWithInfo (fullPath $ "dbs" </> "logs" </> "schema.sql") dbsLogsSchema

  ensureDir (fullPath $ "dbs" </> "auth")
  writeFileWithInfo (fullPath $ "dbs" </> "auth" </> "schema.sql") dbsAuthSchema

  ensureDir (fullPath "utils")
  writeFileWithInfo (fullPath $ "utils" </> "migrations.ml") utilsMigrations
  writeFileWithInfo (fullPath $ "utils" </> "debugger.ml") utilsDebugger
  writeFileWithInfo (fullPath $ "utils" </> "authentication.ml") utilsAuthentication
  writeFileWithInfo (fullPath $ "utils" </> "database.ml") utilsDatabase
  writeFileWithInfo (fullPath $ "utils" </> "renderer.ml") utilsRenderer

  ensureDir (fullPath "resources")
  writeFileWithInfo (fullPath $ "resources" </> "signup.html") resourcesSignup
  writeFileWithInfo (fullPath $ "resources" </> "debugger.html") resourcesDebugger
  writeFileWithInfo (fullPath $ "resources" </> "about.html") resourcesAbout
  writeFileWithInfo (fullPath $ "resources" </> "login.html") resourcesLogin
  writeFileWithInfo (fullPath $ "resources" </> "landing.html") resourcesLanding
  writeFileWithInfo (fullPath $ "resources" </> "dashboard.html") resourcesDashboard

  ensureDir (fullPath $ "resources" </> "assets")
  writeFileWithInfo (fullPath $ "resources" </> "assets" </> "styles.css") assetsStyles

  ensureDir (fullPath "lib")
  writeFileWithInfo (fullPath $ "lib" </> "login.ml") libLogin
  writeFileWithInfo (fullPath $ "lib" </> "logout.ml") libLogout
  writeFileWithInfo (fullPath $ "lib" </> "signup.ml") libSignup
  writeFileWithInfo (fullPath $ "lib" </> "about.ml") libAbout
  writeFileWithInfo (fullPath $ "lib" </> "assets.ml") libAssets
  writeFileWithInfo (fullPath $ "lib" </> "dashboard.ml") libDashboard
  writeFileWithInfo (fullPath $ "lib" </> "landing.ml") libLanding

  -- Set executable permission on the "compiler" file.
  let compilerPath = fullPath "compiler"
  exists <- doesFileExist compilerPath
  when exists $ do
#ifndef mingw32_HOST_OS
    status <- getFileStatus compilerPath
    let newMode = fileMode status .|. ownerExecuteMode .|. groupExecuteMode .|. otherExecuteMode
    setFileMode compilerPath newMode
    putStrLn "[INFO] scaffolds - Set +x on compiler"
#else
    putStrLn "[INFO] scaffolds - Skipping setting executable permission (Windows)"
#endif

  putStrLn "[INFO] scaffolds - Scaffolding complete. You can now edit your files or compile."

