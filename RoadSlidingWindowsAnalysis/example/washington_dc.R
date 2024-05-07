# load data
library(sf)
devtools::load_all()


roads <- st_read('.//example//district-of-columbia-latest-free.shp//gis_osm_roads_free_1.shp')

roads_base <- roads %>% dplyr::filter(fclass %in% c(  #'motorway', 'motorway_link',
                                                      'primary', 'primary_link',
                                                      'secondary', 'secondary_link',
                                                      'trunk', 'trunk_link',
                                                      'tertiary', 'tertiary_link'
                                                      #,
                                                      #'residential', 
                                                      #'living_street',
                                                      #'bus_guideway', 
                                                      #'busway',
                                                      #'road',  
                                                      #'unclassified'
                                                      ))

cors <- create_corridors(roads_base)
sw <- create_sliding_windows(cors)

st_write(cors, 'output//corridors.shp')
st_write(sw, 'output//sliding_windows.shp')
