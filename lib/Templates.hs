{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Templates
  ( fileMainExtMl, extEnv, extGitignore, extTailwindBuildInput, fileCompiler, dbsLogsSchema, dbsAuthSchema, utilsMigrations, utilsDebugger, utilsAuthentication, utilsDatabase, utilsRenderer, resourcesSignup, resourcesDebugger, resourcesAbout, resourcesLogin, resourcesLanding, resourcesDashboard, resourcesAssetsStyles, resourcesComponentsWrappersBaseTemplate, resourcesComponentsEmbedsMainContent, libLogin, libLogout, libSignup, libAbout, libAssets, libDashboard, libLanding
  ) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

fileMainExtMl :: ByteString
fileMainExtMl = $(embedFile "src_ocaml_project/main.ml")  -- fileMainExtMl

extEnv :: ByteString
extEnv = $(embedFile "src_ocaml_project/.env")  -- extEnv

extGitignore :: ByteString
extGitignore = $(embedFile "src_ocaml_project/.gitignore")  -- extGitignore

extTailwindBuildInput :: ByteString
extTailwindBuildInput = $(embedFile "src_ocaml_project/.tailwind_build_input")  -- extTailwindBuildInput

fileCompiler :: ByteString
fileCompiler = $(embedFile "src_ocaml_project/compiler")  -- fileCompiler

dbsLogsSchema :: ByteString
dbsLogsSchema = $(embedFile "src_ocaml_project/dbs/logs/schema.sql")  -- dbsLogsSchema

dbsAuthSchema :: ByteString
dbsAuthSchema = $(embedFile "src_ocaml_project/dbs/auth/schema.sql")  -- dbsAuthSchema

utilsMigrations :: ByteString
utilsMigrations = $(embedFile "src_ocaml_project/utils/migrations.ml")  -- utilsMigrations

utilsDebugger :: ByteString
utilsDebugger = $(embedFile "src_ocaml_project/utils/debugger.ml")  -- utilsDebugger

utilsAuthentication :: ByteString
utilsAuthentication = $(embedFile "src_ocaml_project/utils/authentication.ml")  -- utilsAuthentication

utilsDatabase :: ByteString
utilsDatabase = $(embedFile "src_ocaml_project/utils/database.ml")  -- utilsDatabase

utilsRenderer :: ByteString
utilsRenderer = $(embedFile "src_ocaml_project/utils/renderer.ml")  -- utilsRenderer

resourcesSignup :: ByteString
resourcesSignup = $(embedFile "src_ocaml_project/resources/signup.html")  -- resourcesSignup

resourcesDebugger :: ByteString
resourcesDebugger = $(embedFile "src_ocaml_project/resources/debugger.html")  -- resourcesDebugger

resourcesAbout :: ByteString
resourcesAbout = $(embedFile "src_ocaml_project/resources/about.html")  -- resourcesAbout

resourcesLogin :: ByteString
resourcesLogin = $(embedFile "src_ocaml_project/resources/login.html")  -- resourcesLogin

resourcesLanding :: ByteString
resourcesLanding = $(embedFile "src_ocaml_project/resources/landing.html")  -- resourcesLanding

resourcesDashboard :: ByteString
resourcesDashboard = $(embedFile "src_ocaml_project/resources/dashboard.html")  -- resourcesDashboard

resourcesAssetsStyles :: ByteString
resourcesAssetsStyles = $(embedFile "src_ocaml_project/resources/assets/styles.css")  -- resourcesAssetsStyles

resourcesComponentsWrappersBaseTemplate :: ByteString
resourcesComponentsWrappersBaseTemplate = $(embedFile "src_ocaml_project/resources/components/wrappers/base-template.html")  -- resourcesComponentsWrappersBaseTemplate

resourcesComponentsEmbedsMainContent :: ByteString
resourcesComponentsEmbedsMainContent = $(embedFile "src_ocaml_project/resources/components/embeds/main-content.html")  -- resourcesComponentsEmbedsMainContent

libLogin :: ByteString
libLogin = $(embedFile "src_ocaml_project/lib/login.ml")  -- libLogin

libLogout :: ByteString
libLogout = $(embedFile "src_ocaml_project/lib/logout.ml")  -- libLogout

libSignup :: ByteString
libSignup = $(embedFile "src_ocaml_project/lib/signup.ml")  -- libSignup

libAbout :: ByteString
libAbout = $(embedFile "src_ocaml_project/lib/about.ml")  -- libAbout

libAssets :: ByteString
libAssets = $(embedFile "src_ocaml_project/lib/assets.ml")  -- libAssets

libDashboard :: ByteString
libDashboard = $(embedFile "src_ocaml_project/lib/dashboard.ml")  -- libDashboard

libLanding :: ByteString
libLanding = $(embedFile "src_ocaml_project/lib/landing.ml")  -- libLanding
