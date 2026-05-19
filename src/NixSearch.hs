{-# LANGUAGE OverloadedStrings #-}
module NixSearch
  ( nixSearch
  , NixResult (..)
  , Channel (..)
  , getCachePath
  , cacheExists
  , loadCache
  , updateCache
  , searchLocal
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson (FromJSON(..), decodeFileStrict, withArray)
import Data.Char (toLower)
import Data.List (isInfixOf)
import qualified Data.Vector as V
import System.Directory (createDirectoryIfMissing, doesFileExist, getXdgDirectory, XdgDirectory(XdgCache))
import System.FilePath ((</>))
import System.Process (readCreateProcessWithExitCode, shell)
import System.Exit (ExitCode(..))

-- Channel selection for stable vs unstable package databases
data Channel = Unstable | Stable deriving (Eq, Show, Ord, Enum, Bounded)

data NixResult = NixResult
  { nrAttr :: String
  , nrName :: String
  , nrDesc :: String
  } deriving (Eq, Show)

instance FromJSON NixResult where
  parseJSON = withArray "NixResult" $ \arr -> do
    if V.length arr >= 3
      then do
        attr <- parseJSON (arr V.! 0)
        name <- parseJSON (arr V.! 1)
        desc <- parseJSON (arr V.! 2)
        return $ NixResult attr name desc
      else fail "Expected a list of at least 3 elements"

-- Default fallback version for stable NixOS pinning
defaultStableVersion :: String
defaultStableVersion = "nixos-25.11"

-- Legacy function to keep the interface compileable if needed, or we can deprecate it.
nixSearch :: String -> IO (Either String [NixResult])
nixSearch query = do
  -- Legacy nix search, fallback to local search if cache exists
  exists <- cacheExists defaultStableVersion Unstable
  if exists
    then do
      res <- loadCache defaultStableVersion Unstable
      return $ case res of
        Left err -> Left err
        Right pkgs -> Right (searchLocal query pkgs)
    else return $ Left "Cache not found. Please update/download the packages cache first."

-- Get the URL for the package index of a channel
channelUrl :: String -> Channel -> String
channelUrl _    Unstable = "https://channels.nixos.org/nixpkgs-unstable/packages.json.br"
channelUrl version Stable   = "https://channels.nixos.org/" ++ version ++ "/packages.json.br"

-- Get the cache directory path
getCacheDir :: IO FilePath
getCacheDir = do
  dir <- getXdgDirectory XdgCache "cutesetup"
  createDirectoryIfMissing True dir
  return dir

-- Get the cache file path for a channel
getCachePath :: String -> Channel -> IO FilePath
getCachePath version chan = do
  dir <- getCacheDir
  return $ case chan of
    Unstable -> dir </> "packages-unstable.cache.json"
    Stable   -> dir </> "packages-stable-" ++ version ++ ".cache.json"

-- Check if the cache file exists for a channel
cacheExists :: String -> Channel -> IO Bool
cacheExists version chan = do
  path <- getCachePath version chan
  doesFileExist path

-- Load the cache database for a channel from disk
loadCache :: String -> Channel -> IO (Either String [NixResult])
loadCache version chan = do
  path <- getCachePath version chan
  exists <- doesFileExist path
  if not exists
    then return $ Left "Cache file does not exist"
    else do
      mVal <- decodeFileStrict path
      case mVal of
        Nothing   -> return $ Left "Failed to parse cache JSON"
        Just pkgs -> return $ Right pkgs

-- Update/download the packages list for a channel
-- Downloads the .br file and extracts/compresses it on the fly via a python shell command
updateCache :: String -> Channel -> IO (Either String String)
updateCache version chan = do
  path <- getCachePath version chan
  let url = channelUrl version chan
      {- The pyScript is an optimization tool designed to run in a pipeline.
         It processes the large Nix packages JSON index on the fly:
         1. `json.load(sys.stdin)` parses the raw decompressed channel packages database object.
         2. For each package entry under the "packages" map:
            - `k` represents the Nixpkgs attribute path (e.g. "python3Packages.pip").
            - `v.get('pname', '')` extracts the package name without the attribute path.
            - `v.get('meta', {}).get('description', '')` fetches the package's description text.
         3. It constructs a compact array of 3-element arrays `[attr, name, desc]` instead of full objects.
         4. `json.dump` writes the compact list to stdout.
         This reduces the cached file size from ~60MB to ~5MB, yielding substantial memory and parsing speedups in Haskell. -}
      pyScript = "import json, sys; d=json.load(sys.stdin); out=[[k, v.get('pname',''), v.get('meta',{}).get('description','')] for k,v in d.get('packages',{}).items()]; json.dump(out, sys.stdout)"
      cmd = "curl -L " ++ url ++ " | nix-shell -p brotli python3 --run \"brotli -d | python3 -c \\\"" ++ pyScript ++ "\\\"\" > " ++ show path
  
  res <- try @SomeException $ readCreateProcessWithExitCode (shell cmd) ""
  case res of
    Left err -> return $ Left ("Process execution failed: " ++ show err)
    Right (exitCode, _, stderr) -> case exitCode of
      ExitSuccess -> return $ Right ("Successfully updated " ++ show chan ++ " cache!")
      ExitFailure _ -> return $ Left ("Failed to update cache. Stderr:\n" ++ stderr)

-- Maximum number of search results to return
maxSearchResults :: Int
maxSearchResults = 40

-- Filter packages database locally based on query
searchLocal :: String -> [NixResult] -> [NixResult]
searchLocal query pkgs
  | null query = []
  | otherwise =
      let q = map toLower query
          matches p = q `isInfixOf` map toLower (nrAttr p)
                   || q `isInfixOf` map toLower (nrName p)
                   || q `isInfixOf` map toLower (nrDesc p)
      in take maxSearchResults (filter matches pkgs)
