# load data
devtools::load_all()
roads_df <- sf::st_read('data//arkansas-latest-free.shp//gis_osm_roads_free_1.shp')
roads_base <- roads_df[order(roads_df$osm_id),]

roads2 <- roads_base %>% dplyr::filter(fclass %in% c( 'motorway', 'motorway_link',
                                              'primary', 'primary_link',
                                              'secondary', 'secondary_link',
                                              'trunk', 'trunk_link'))

roads21 <- roads_df %>% dplyr::filter(fclass %in% c( 'tertiary', 'tertiary_link',
                                              'residential', 'living_street',
                                              'bus_guideway', 'busway',
                                              'road', 'unclassified'))

cors2 <- create_corridors(roads2)
sw1 <- create_sliding_windows(cors2)

cors21 <- create_corridors(roads21)
sw11 <- create_sliding_windows(cors21)

swf<- rbind(sw1, sw11)
corsf <- rbind(cors2, cors21)
file.remove('output//roads_ar.shp')
file.remove('output//roads_ar.prj')
file.remove('output//roads_ar.dbf')
file.remove('output//roads_ar.shx')
st_write(corsf, 'output//roads_ar.shp')
file.remove('output//sw_results_ar.shp')
file.remove('output//sw_results_ar.prj')
file.remove('output//sw_results_ar.dbf')
file.remove('output//sw_results_ar.shx')
st_write(sw11, 'output//sw_results_ar.shp')