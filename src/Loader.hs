{- 
Loads and caches resources such as config and data files,
as well as (potentially) textures, models, etc.
We don't precisely need a cache...  Everything stays in memory
anyway, and we don't have levels that don't need large chunks
of the game, so.
We load everything eagerly(ish), then have a bunch of functions
that return it from the store.
Loading is not done on-demand now, so things must all be loaded
in the right order!  For instance, space features depend on having
zones loaded, and regions depend on having space features loaded.
So first:
string lists
zones
space feature
region
-}

module Loader where
import Control.Monad
import qualified Data.Map as Map
import IO
import System.Directory
import System.FilePath
import Text.JSON

import Universe
import Util

dataDir :: FilePath
dataDir = "data"
zoneDir :: FilePath
zoneDir = joinPath [dataDir, "zones"]
regionDir :: FilePath
regionDir = joinPath [dataDir, "regions"]
spacefeatureDir :: FilePath
spacefeatureDir = joinPath [dataDir, "spacefeatures"]

data LoadStore = LoadStore {
  ls_zoneType :: Map.Map String ZoneType,
  ls_spaceFeatureType :: Map.Map String SpaceFeatureType,
  ls_regionType :: Map.Map String RegionType,
  -- String lists are handy for several miscellanious things, like
  -- star names, file lists, etc.
  ls_stringList :: Map.Map String [String]
  }

newLoadStore :: LoadStore
newLoadStore = LoadStore {
  ls_zoneType = Map.empty,
  ls_spaceFeatureType = Map.empty,
  ls_regionType = Map.empty,
  ls_stringList = Map.empty
  }
               
addZoneType :: String -> ZoneType -> LoadStore -> LoadStore
addZoneType name val ls =
  ls { ls_zoneType = Map.insert name val (ls_zoneType ls) }

addSpaceFeatureType :: String -> SpaceFeatureType -> LoadStore -> LoadStore
addSpaceFeatureType name val ls =
  ls { ls_spaceFeatureType = Map.insert name val (ls_spaceFeatureType ls) }

addRegionType :: String -> RegionType -> LoadStore -> LoadStore
addRegionType name val ls =
  ls { ls_regionType = Map.insert name val (ls_regionType ls) }

addStringList :: String -> [String] -> LoadStore -> LoadStore
addStringList name val ls =
  ls { ls_stringList = Map.insert name val (ls_stringList ls) }

getValue :: Monad m => 
            (LoadStore -> Map.Map String b) -> LoadStore -> String -> m b
getValue f ls key = do
  case Map.lookup key (f ls) of
    Nothing -> fail ("getValue: Tried to get nonexistant value: " ++ (show key))
    Just a -> return a

-- Not 100% sure what the practical error handling is here,
-- but it seems to work.
slurpFile :: String -> IO String
slurpFile filename = do
  -- Bracket takes function to do first, function to do last no matter what,
  -- function to produce output
  -- Catch takes a function to do, and a function to do upon error.
  catch
    (do
      h <- openFile filename ReadMode
      putStrLn $ "Reading " ++ filename
      hGetContents h)
    (\err -> fail $ "Error reading file " ++ filename ++ ": " ++ (show err))
    
-- I'm not 100% sure just HOW fail fits into the IO monad here, but...
-- I guess I'll find out once it happens!
-- It appears to work at least, and be able to be caught with
-- catch.  Magic.
loadJSON :: String -> IO JSValue
loadJSON filename = do
  str <- slurpFile filename
  case decodeStrict str of
    Error s -> fail ("JSON parsing error in file " ++ filename ++ ": " ++ s)
    Ok j -> return j    


-- Okay... verification and combinators.
-- We need to: Make sure a json object value exists,
-- and make sure that it has the right type value.
-- Possible values are: Bool, rational, string, array, object.
-- ...you know what?  Fuck combinators.  All we really want
-- is a function that says "Get thing out of object.
-- Make sure thing is of appropriate type.  Raise error
-- of some kind if thing does not exist or type is wrong."
-- And then we can map that across a list of
-- name/type pairs.
-- ...yeah, that's basically a combinator, given how it's used.

ensureObj :: (Monad m) => JSValue -> m (JSObject JSValue)
ensureObj jsobj =
  case jsobj of
    JSObject j -> return j
    _ -> fail "Invalid JSON value: expected object"

ensureArray :: (Monad m) => JSValue -> m [JSValue]
ensureArray jsobj =
  case jsobj of
    JSArray j -> return j
    _ -> fail "Invalid JSON value: expected object"

ensureString :: (Monad m) => JSValue -> m String
ensureString jsobj =
  case jsobj of
    JSString j -> return $ fromJSString j
    _ -> fail "Invalid JSON value: expected object"
    
ensureBool :: (Monad m) => JSValue -> m Bool
ensureBool jsobj =
  case jsobj of
    JSBool j -> return j
    _ -> fail "Invalid JSON value: expected object"
    
ensureRational :: (Monad m) => JSValue -> m Rational
ensureRational jsobj =
  case jsobj of
    JSRational _ j -> return j
    _ -> fail "Invalid JSON value: expected rational"

ensureDouble :: (Monad m) => JSValue -> m Double
ensureDouble jsobj =
  case jsobj of
    JSRational _ j -> return (fromRational j)
    _ -> fail "Invalid JSON value: expected rational"



-- Makes sure an object member exists and return it.
ensureObjMember :: (Monad m) => String -> JSObject JSValue -> m JSValue
ensureObjMember name obj =
  let r = valFromObj name obj in
  case r of
    Ok j -> return j
    Error s -> fail s

ensureMemberIs :: (Monad m) => String -> (JSValue -> m a) -> JSObject JSValue -> m a
ensureMemberIs name getter obj = do
  omember <- ensureObjMember name obj
  getter omember

-- Makes sure all members in the array are of the given function's type,
-- and that there are at least i of them.
ensureArrayIs :: (Monad m) => Integer -> (JSValue -> m a) -> JSValue -> m [a]
ensureArrayIs i getter obj = do
  arr <- ensureArray obj
  if length arr >= (fromIntegral i) then
    mapM getter arr
    else fail $ "Invalid JSON value: Expected array of length at least " ++ (show i)

ensurePair :: (Monad m) => (JSValue -> m a) -> (JSValue -> m b) -> JSValue -> m (a, b)
ensurePair gettera getterb obj = do
  arr <- ensureArray obj
  case arr of
    pa : pb : _ -> do
      a <- (gettera pa) 
      b <- (getterb pb)
      return (a, b)
    _ -> fail $ "Invalid JSON value: Array doesn't even look like a pair: " ++ (show arr)
    
ensureAssocList :: (Monad m) => (JSValue -> m a) -> (JSValue -> m b) -> JSValue -> m [(a, b)]
ensureAssocList gettera getterb obj = do
  arr <- ensureArray obj
  lst <- mapM (ensurePair gettera getterb) arr
  return lst


listToPair :: (Monad m) => [a] -> m (a, a)
listToPair [] = fail "listToPair: List too short"
listToPair (a : []) = fail "listToPair: List too short"
listToPair (a : b : _) = return (a,b)

loadZoneType ::  LoadStore -> String -> IO LoadStore
loadZoneType ls filename = do
  j <- loadJSON $ joinPath [zoneDir, filename]
  o <- ensureObj j
  n <- ensureMemberIs "name" ensureString o
  rm <- ensureMemberIs "resourceModifier" ensureDouble o
  gm <- ensureMemberIs "grainularityModifier" ensureDouble o
  radm <- ensureMemberIs "radiationModifier" ensureDouble o
  let zt = ZoneType {zt_name = n, zt_resourceMod = rm, zt_grainularityMod = gm, zt_radiationMod = radm}
  return $ addZoneType filename zt ls

getZoneType :: Monad m => LoadStore -> String -> m ZoneType
getZoneType = getValue ls_zoneType


loadSpaceFeatureType :: LoadStore -> String -> IO LoadStore
loadSpaceFeatureType ls filename = do
    j <- loadJSON $ joinPath [spacefeatureDir, filename]
    o <- ensureObj j
    n <- ensureMemberIs "name" ensureString o
    nz' <- ensureMemberIs "numZones" (ensureArrayIs 2 ensureDouble) o
    (za, zb) <- listToPair nz'
    let nz = (round za, round zb)
    rm <- ensureMemberIs "resourceModifier" ensureDouble o
    gm <- ensureMemberIs "grainularityModifier" ensureDouble o
    radm <- ensureMemberIs "radiationModifier" ensureDouble o
    ztNames <- ensureMemberIs "zoneTypes" (ensureAssocList ensureString ensureDouble) o
    let (strs, nums) = unzip ztNames
    zts <- mapM (getZoneType ls) strs
    let ztss = zip zts nums
    let sft = SpaceFeatureType {
          st_name = n, 
          st_numZones = nz, 
          st_resourceMod = rm, 
          st_grainularityMod = gm, 
          st_radiationMod = radm, 
          st_zoneTypes = pdf2cdf ztss
          }
    return $ addSpaceFeatureType filename sft ls

getSpaceFeatureType :: Monad m => LoadStore -> String -> m SpaceFeatureType
getSpaceFeatureType = getValue ls_spaceFeatureType


loadRegionType :: LoadStore -> String -> IO LoadStore
loadRegionType ls filename = do
  j <- loadJSON $ joinPath [regionDir, filename]
  o <- ensureObj j
  n <- ensureMemberIs "name" ensureString o
  pm <- ensureMemberIs "particleModifier" ensureDouble o
  hm <- ensureMemberIs "hazardModifier" ensureDouble o
  am <- ensureMemberIs "ageModifier" ensureDouble o
  stNames <- ensureMemberIs "starTypes" (ensureAssocList ensureString ensureDouble) o
  let (strs, nums) = unzip stNames
  sts <- mapM (getSpaceFeatureType ls) strs
  let stss = zip sts nums
  let rt = RegionType {
    rt_name = n,
    rt_particleMod = pm,
    rt_hazardMod = hm,
    rt_ageMod = am,
    rt_starTypes = pdf2cdf stss
    }
  return $ addRegionType filename rt ls

getRegionType :: Monad m => LoadStore -> String -> m RegionType
getRegionType = getValue ls_regionType

loadStringList :: LoadStore -> String -> IO LoadStore
loadStringList ls filename = do
  j <- loadJSON $ joinPath [dataDir, filename]
  a <- ensureArray j
  strs <- mapM ensureString a
  return $ addStringList filename strs ls

getStringList :: Monad m => LoadStore -> String -> m [String]
getStringList = getValue ls_stringList

loadSpaceFeatureTypes :: LoadStore -> IO LoadStore
loadSpaceFeatureTypes ls = do
  filenames <- getFilesInDir spacefeatureDir
  foldM loadSpaceFeatureType ls filenames

  
loadZoneTypes :: LoadStore -> IO LoadStore
loadZoneTypes ls = do
  filenames <- getFilesInDir zoneDir
  foldM loadZoneType ls filenames

loadRegionTypes :: LoadStore -> IO LoadStore
loadRegionTypes ls = do
  filenames <- getFilesInDir regionDir
  foldM loadRegionType ls filenames

-- Here we go!  This loads EVERYTHING!
allValues :: IO LoadStore
allValues = do
  let n1 = newLoadStore
  -- First, important string lists.
  n2 <- foldM loadStringList n1 ["region-names.txt",
                                "spacefeature-names.txt",
                                "constellation-names.txt"
                                ]
  -- Then zone types, spacefeature types, region types.
  n3 <- loadZoneTypes n2
  n4 <- loadSpaceFeatureTypes n3
  n5 <- loadRegionTypes n4
  return n5