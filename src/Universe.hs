{- Stars and other strange glowy things in space. -}

module Universe where
import Data.List as List
import Text.Printf

--import UniverseGen

data Universe = Universe {
  u_map :: Galaxy
  --u_seed :: UniverseGenSeed
  }


data SpaceFeature = SpaceFeature {
  s_name :: String,
  s_type :: SpaceFeatureType,
  
  s_constellation :: String,
  s_location :: (Double, Double),
  
  -- If owner == "" then unowned and thus uninhabited...
  s_owner :: String,
  s_zones :: [Zone]
  }

instance Show SpaceFeature where
  show sf = printf "Feature %s (%s)\n  Type: %s\n  Location & zone count: %s, %d\n%s\n"
            (s_name sf) (s_constellation sf) (st_name $ s_type sf) 
            (show $ s_location sf) (length $ s_zones sf) (show $ s_zones sf)
                    
-- XXX: How to do age?
-- We basically need to define a lifecycle chain for each
-- space feature type...
-- Okay, there's basically young, middle and old.
-- Young has lots of dust and gas and such and
-- young big bright stars, middle has lots of supernova
-- remnants and mid-size stars, old is mostly thin
-- gas and dust and small stars.
data SpaceFeatureType = SpaceFeatureType {
  st_name :: String,
  st_numZones :: (Integer, Integer),
  st_resourceMod :: Double,
  st_grainularityMod :: Double,
  st_radiationMod :: Double,
  -- Type, and weighting.
  st_zoneTypes :: [(ZoneType, Double)]
  }

instance Show SpaceFeatureType where
  show x = "SpaceFeatureType: " ++ (st_name x)

data Zone = Zone {
  z_size :: Double,
  z_type :: ZoneType,
  z_resources :: Double,
  z_grainularity :: Double,
  z_radiation :: Double,
  
  
  z_transport :: Double,
  z_infrastructure :: Double,
  z_defense :: Double,
  -- For the moment, different species are just separate populations 
  -- with names attached.
  -- Integer == arbitrary-precision.  :D
  z_population :: [(String, Integer)],
  -- Then we have the culture split-up, which for the moment is merely
  -- names and a percentage (1.0 = 100%)
  z_cultures :: [(String, Double)]
  }

instance Show Zone where
  show z = printf "Zone %s\n  Size %s\n  Resources, grainularity, radiation: %s, %s, %s\n"
           (zt_name $ z_type z) (show $ z_size z) (show $ z_resources z) 
           (show $ z_grainularity z) (show $ z_radiation z)
  
data ZoneType = ZoneType {  
  zt_name :: String,
  zt_resourceMod :: Double,
  zt_grainularityMod :: Double,
  zt_radiationMod :: Double
  }

instance Show ZoneType where
  show x = "ZoneType: " ++ (zt_name x)


data Region = Region {
  r_name :: String,
  r_type :: RegionType,
  r_stars :: [SpaceFeature]
}

instance Show Region where
  show r =
    printf "Region %s\n  Type: %s\n  Num stars: %d\n%s\n"
      (r_name r) (rt_name $ r_type r) (length $ r_stars r) (show $ r_stars r)

data RegionType = RegionType {
  rt_name :: String,
  rt_particleMod :: Double,
  rt_hazardMod :: Double,
  rt_ageMod :: Double,
  rt_starTypes :: [(SpaceFeatureType, Double)]
  }

instance Show RegionType where
  show x = "RegionType: " ++ (rt_name x)


type Galaxy = [Region]

getAllSFs :: Galaxy -> [SpaceFeature]
getAllSFs g =
  let ls = map r_stars g in
  concat ls

findSFBy :: (SpaceFeature -> Bool) -> Galaxy -> Maybe SpaceFeature
findSFBy f g = List.find f (getAllSFs g)

findSFByLocation :: Galaxy -> (Double, Double) -> Maybe SpaceFeature
findSFByLocation g l = 
  findSFBy (\s -> (s_location s) == l) g

findSFByName :: Galaxy -> String -> Maybe SpaceFeature
findSFByName g n =
  findSFBy (\s -> (s_name s) == n) g

{-
--findStarByRegion
--findStarByConstellation
--findStarWithin
--findStarNearestTo
-}