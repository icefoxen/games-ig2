namespace hs TurnProtocol

struct ZoneT {
   1: required double zt_size,
   2: required string zt_type,
   3: required double zt_resources,
   4: required double zt_grainularity,
   5: required double zt_radiation,

   6: required double zt_transport,
   7: required double zt_infrastructure,
   8: required double zt_defense,
   9: required double zt_population, // XXX: Wrong
   // XXX: Cultures
   
}


struct SpaceFeatureT {
   1: required string sft_name,
   2: required string sft_constellation,
   3: required double sft_locX,
   4: required double sft_locY,
   5: required string sft_owner,
   6: required string sft_type,
   7: required list<ZoneT> sft_zones
}

typedef list<SpaceFeatureT> galaxy
