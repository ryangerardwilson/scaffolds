-- File: src/Templates.hs
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Templates
  ( fileMainExtMl
  , extEnv, extGitignore, extTailwindBuildInput, fileCompiler
  , dbsLogsSchema, dbsAuthSchema
  , utilsMigrations, utilsDebugger, utilsAuthentication, utilsDatabase, utilsRenderer
  , resourcesSignup, resourcesDebugger, resourcesAbout, resourcesLogin
  , resourcesLanding, resourcesDashboard
  , assetsStyles
  , libLogin, libLogout, libSignup, libAbout, libAssets, libDashboard, libLanding
  ) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

fileMainExtMl :: ByteString
fileMainExtMl = $(embedFile "ocaml/main.ml")

extEnv :: ByteString
extEnv = $(embedFile "ocaml/.env")

extGitignore :: ByteString
extGitignore = $(embedFile "ocaml/.gitignore")

extTailwindBuildInput :: ByteString
extTailwindBuildInput = $(embedFile "ocaml/.tailwind_build_input")

fileCompiler :: ByteString
fileCompiler = $(embedFile "ocaml/compiler")

dbsLogsSchema :: ByteString
dbsLogsSchema = $(embedFile "ocaml/dbs/logs/schema.sql")

dbsAuthSchema :: ByteString
dbsAuthSchema = $(embedFile "ocaml/dbs/auth/schema.sql")

utilsMigrations :: ByteString
utilsMigrations = $(embedFile "ocaml/utils/migrations.ml")

utilsDebugger :: ByteString
utilsDebugger = $(embedFile "ocaml/utils/debugger.ml")

utilsAuthentication :: ByteString
utilsAuthentication = $(embedFile "ocaml/utils/authentication.ml")

utilsDatabase :: ByteString
utilsDatabase = $(embedFile "ocaml/utils/database.ml")

utilsRenderer :: ByteString
utilsRenderer = $(embedFile "ocaml/utils/renderer.ml")

resourcesSignup :: ByteString
resourcesSignup = $(embedFile "ocaml/resources/signup.html")

resourcesDebugger :: ByteString
resourcesDebugger = $(embedFile "ocaml/resources/debugger.html")

resourcesAbout :: ByteString
resourcesAbout = $(embedFile "ocaml/resources/about.html")

resourcesLogin :: ByteString
resourcesLogin = $(embedFile "ocaml/resources/login.html")

resourcesLanding :: ByteString
resourcesLanding = $(embedFile "ocaml/resources/landing.html")

resourcesDashboard :: ByteString
resourcesDashboard = $(embedFile "ocaml/resources/dashboard.html")

assetsStyles :: ByteString
assetsStyles = $(embedFile "ocaml/resources/assets/styles.css")

libLogin :: ByteString
libLogin = $(embedFile "ocaml/lib/login.ml")

libLogout :: ByteString
libLogout = $(embedFile "ocaml/lib/logout.ml")

libSignup :: ByteString
libSignup = $(embedFile "ocaml/lib/signup.ml")

libAbout :: ByteString
libAbout = $(embedFile "ocaml/lib/about.ml")

libAssets :: ByteString
libAssets = $(embedFile "ocaml/lib/assets.ml")

libDashboard :: ByteString
libDashboard = $(embedFile "ocaml/lib/dashboard.ml")

libLanding :: ByteString
libLanding = $(embedFile "ocaml/lib/landing.ml")

