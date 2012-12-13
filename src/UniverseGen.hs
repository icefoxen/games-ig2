{- Handles generation of a starmap according to given rules.
-}

module UniverseGen where
import Control.Monad.State
import Data.Ratio
import Random

import Loader
import Universe
import Util

-- XXX: We want to be able to provide a CDF for
-- region distribution...
data UniverseGenSeed = UniverseGenSeed {
  ug_numRegions :: Int,
  ug_starsPerConstellation :: (Int, Int),
  ug_starsPerRegion :: (Int, Int),
  ug_regionTypes :: [(String, Double)],
  ug_radius :: Double,
  -- Must be an Int, not an Integer
  -- Random seed = machine word
  ug_randomSeed :: Int,
  ug_loadStore :: LoadStore,
  ug_constellationNames :: [String],
  ug_regionNames :: [String],
  ug_starNames :: [String]
}

store :: UniverseGenSeed -> LoadStore
store = ug_loadStore

-- Grmbl.
-- The region types should really be loaded from some
-- file...?
-- Hell, the thing as a whole should probably be loaded
-- from some file...
-- With the region types resolved to actual RegionType's, not
-- strings.
standardUniverseSeed :: IO UniverseGenSeed
standardUniverseSeed = do
  ls <- Loader.allValues
  cn <- getStringList ls "constellation-names.txt"
  rn <- getStringList ls "region-names.txt"
  sn <- getStringList ls "spacefeature-names.txt"
  return UniverseGenSeed {
  ug_numRegions = 8,
  ug_starsPerConstellation = (3, 10),
  ug_starsPerRegion = (10,50),
  ug_regionTypes = [("abyss.txt", 0.1), 
                    ("ocean.txt", 0.2),
                    ("plains.txt", 0.4),
                    ("shoals.txt", 0.5),
                    ("wasteland.txt", 0.6),
                    ("desert.txt", 0.7),
                    ("pass.txt", 0.8),
                    ("rapids.txt", 0.9),
                    ("storm.txt", 1.0)
                   ],
  ug_radius = 500,
  ug_randomSeed = 42,
  ug_loadStore = ls,

  ug_constellationNames = cn,
  ug_regionNames = rn,
  ug_starNames = sn
  }

genGalaxy :: UniverseGenSeed -> IO Galaxy
genGalaxy ug = do
  setStdGen $ mkStdGen $ ug_randomSeed ug
  genRegions ug (ug_numRegions ug) (ug_regionNames ug) []

genRegions :: UniverseGenSeed -> Int -> [String] -> 
              [Region] -> IO [Region]
genRegions ug num names accm =
  if num <= 0 then do return accm
    else do
      let name = head names
          ls = ug_loadStore ug
      rn <- (randomRIO (0.0, 1.0) :: IO Double)
      rtname <- selectFromCDF rn (ug_regionTypes ug)
      rt <- getRegionType ls rtname
      r <- genRegion ug rt name
      genRegions ug (num-1) (tail names) (r : accm)

-- XXX: TODO: Figure out location bounds
-- Voronoi diagrams!
-- Figure out bounds of regions, tell the stars where they are...
-- Figure out bounds of constellations.
-- We can do it with more Voronoi diagrams!
genRegion :: UniverseGenSeed -> RegionType -> String -> IO Region
genRegion ug rt name = do
  numStars <- randomRIO $ ug_starsPerRegion ug
  x <- randomRIO (0, (ug_radius ug))
  y <- randomRIO (0, (ug_radius ug))
  s <- genSpaceFeatures ug (x, y) numStars rt (ug_starNames ug) []
  return Region {
    r_name = name,
    r_type = rt,
    r_stars = s
    }

-- XXX: TODO:
-- Apply zone bias
-- Mix edges of zones???
-- Figure zone location
-- Mongle constellations (fill them in later)
genSpaceFeatures :: UniverseGenSeed -> (Double, Double) ->
              Int -> RegionType -> [String] -> [SpaceFeature] 
              -> IO [SpaceFeature]
genSpaceFeatures ug loc num rt names accm =
  if num <= 0 then do return accm
    else do
    let name = head names
        ls = ug_loadStore ug
    sfn <- (randomRIO (0.0, 1.0) :: IO Double)
    sft <- selectFromCDF sfn (rt_starTypes rt)
    s <- genSpaceFeature ug sft loc name
    genSpaceFeatures ug loc (num-1) rt (tail names) (s:accm)

genSpaceFeature :: UniverseGenSeed -> SpaceFeatureType -> (Double,Double) 
                   -> String -> IO SpaceFeature
genSpaceFeature ug sft loc name = do
  nz <- randomRIO (st_numZones sft)
  z <- genZones ug nz (st_zoneTypes sft) []
  let f zone = zone {
        z_resources = (z_resources zone) * (st_resourceMod sft),
        z_radiation = (z_radiation zone) * (st_radiationMod sft),
        z_grainularity = (z_grainularity zone) * (st_grainularityMod sft)
        }
      zs = map f z
  return SpaceFeature {
    s_name = name,
    s_type = sft,
  
    s_constellation = "Bad constellation name; see UniverseGen.hs",
    s_location = loc,
  
    s_owner = "",
    s_zones = zs
  }


genZones :: UniverseGenSeed -> Integer -> [(ZoneType, Double)] ->
            [Zone] -> IO [Zone]
genZones ug num cdf accm =
  if num <= 0 then 
    return accm
    else do
      zn <- (randomRIO (0.0, 1.0) :: IO Double)
      zt <- selectFromCDF zn cdf
      z <- genZone ug zt
      genZones ug (num - 1) cdf (z : accm)

-- XXX: TODO:
-- Decide upon range of resources, etc.
-- Make more normalized values somehow.  Die rolls?
-- Figure out habitability?
-- Gradate values with distance to star
genZone :: UniverseGenSeed -> ZoneType -> IO Zone
genZone ug zt = do
  size <- (randomRIO (1, 10) :: IO Double)
  resources <- (randomRIO (1, 100) :: IO Double)
  grain <- (randomRIO (1, 100) :: IO Double)
  rad <- (randomRIO (1, 100) :: IO Double)
  let rm = resources * (zt_resourceMod zt)
      gm = grain * (zt_grainularityMod zt)
      radm = rad * (zt_radiationMod zt)
                     
  return Zone {
    z_size = size,
    z_type = zt,
    z_resources = rm,
    z_grainularity = gm,
    z_radiation = radm,
    
    z_transport = 0,
    z_defense = 0,
    z_infrastructure = 0,
    z_population = [],
    z_cultures = []
  }
  
