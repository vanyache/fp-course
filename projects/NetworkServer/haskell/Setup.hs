{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.List ( nub )
import Distribution.Package ( PackageId, InstalledPackageId, packageVersion, packageName )
import Distribution.PackageDescription ( PackageDescription(), TestSuite(..) )
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.Utils ( rewriteFile, createDirectoryIfMissingVerbose )
import Distribution.Simple.Setup ( BuildFlags(buildVerbosity), fromFlag )
import Distribution.Simple.LocalBuildInfo ( withLibLBI, withTestLBI, LocalBuildInfo(), ComponentLocalBuildInfo(componentPackageDeps) )
import Distribution.Text (display)
import Distribution.Verbosity ( Verbosity )
import System.FilePath ( (</>) )

#if __GLASGOW_HASKELL__ <= 710
-- GHC 7.10 and earlier do not support the MIN_VERSION_Cabal macro.
-- Set it to `1` here to match the Cabal dependency bounds in the cabal file.
#define MIN_VERSION_Cabal(a,b,c) 1
#endif

#if MIN_VERSION_Cabal(2,0,0)
import           Distribution.Types.PackageName (PackageName, unPackageName)
import           Distribution.Simple.BuildPaths (autogenPackageModulesDir)
import           Distribution.Version (Version, versionNumbers)

showVersion :: Version -> String
showVersion = intercalate "." . fmap show . versionNumbers

autogenModulesDirCompat :: LocalBuildInfo -> String
autogenModulesDirCompat = autogenPackageModulesDir

#else
import           Distribution.Simple (PackageName, unPackageName)
import           Distribution.Simple.BuildPaths (autogenModulesDir)
import           Data.Version (showVersion)

autogenModulesDirCompat :: LocalBuildInfo -> String
autogenModulesDirCompat = autogenModulesDir
#endif

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi
     buildHook simpleUserHooks pkg lbi hooks flags
  }

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule verbosity pkg lbi = do
  let dir = autogenModulesDirCompat lbi
  createDirectoryIfMissingVerbose verbosity True dir
  withLibLBI pkg lbi $ \_ libcfg -> do
    withTestLBI pkg lbi $ \suite suitecfg -> do
      rewriteFile (dir </> "Build_" ++ testName suite ++ ".hs") $ unlines
        [ "module Build_" ++ testName suite ++ " where"
        , "deps :: [String]"
        , "deps = " ++ (show $ testDeps libcfg suitecfg)
        ]

testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [String]
testDeps xs ys = nub . fmap (display . fst) $ componentPackageDeps xs ++ componentPackageDeps ys
