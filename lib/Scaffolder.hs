{-# LANGUAGE CPP #-}

module Scaffolder
  ( scaffold,
    writeFileWithInfo,
    ensureDir,
  )
where

import Control.Monad (when)
import qualified Data.ByteString as BS
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))

#ifndef mingw32_HOST_OS
import System.Posix.Files ( getFileStatus, setFileMode, fileMode
                          , ownerExecuteMode, groupExecuteMode, otherExecuteMode
                          )
import Data.Bits ((.|.))
#endif

import Templates

templates :: [(FilePath, BS.ByteString)]
templates =
  [ ("main.ml", fileMainExtMl),
    (".env", extEnv),
    (".gitignore", extGitignore),
    (".tailwind_build_input", extTailwindBuildInput),
    ("compiler", fileCompiler),
    ("dbs/logs/schema.sql", dbsLogsSchema),
    ("dbs/auth/schema.sql", dbsAuthSchema),
    ("utils/migrations.ml", utilsMigrations),
    ("utils/debugger.ml", utilsDebugger),
    ("utils/authentication.ml", utilsAuthentication),
    ("utils/database.ml", utilsDatabase),
    ("utils/renderer.ml", utilsRenderer),
    ("resources/signup.html", resourcesSignup),
    ("resources/debugger.html", resourcesDebugger),
    ("resources/about.html", resourcesAbout),
    ("resources/login.html", resourcesLogin),
    ("resources/landing.html", resourcesLanding),
    ("resources/dashboard.html", resourcesDashboard),
    ("resources/assets/styles.css", resourcesAssetsStyles),
    ("lib/login.ml", libLogin),
    ("lib/logout.ml", libLogout),
    ("lib/signup.ml", libSignup),
    ("lib/about.ml", libAbout),
    ("lib/assets.ml", libAssets),
    ("lib/dashboard.ml", libDashboard),
    ("lib/landing.ml", libLanding)
  ]

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
  mapM_
    ( \(rel, fileData) -> do
        let dir = takeDirectory rel
        if not (null dir)
          then ensureDir (fullPath dir)
          else return ()
        writeFileWithInfo (fullPath rel) fileData
    )
    templates

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
