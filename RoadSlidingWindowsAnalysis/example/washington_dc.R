# load data
devtools::load_all()

roads <- sf::st_read('data//district-of-columbia-latest-free.shp//gis_osm_roads_free_1.shp')
roads_base <- roads[order(roads$osm_id),]

roads <- roads_base %>% dplyr::filter(fclass %in% c(  'motorway', 'motorway_link',
                                                      'primary', 'primary_link',
                                                      'secondary', 'secondary_link',
                                                      'trunk', 'trunk_link',
                                                      'tertiary', 'tertiary_link',
                                                      'residential', 'living_street',
                                                      'bus_guideway', 'busway',
                                                      'road',  'unclassified'))


cors <- create_corridors(roads)
sw <- create_sliding_windows(cors)


file.remove('output//roads_dc.shp')
file.remove('output//roads_dc.prj')
file.remove('output//roads_dc.dbf')
file.remove('output//roads_dc.shx')
st_write(cors, 'output//roads_dc.shp')
file.remove('output//sw_results_dc.shp')
file.remove('output//sw_results_dc.prj')
file.remove('output//sw_results_dc.dbf')
file.remove('output//sw_results_dc.shx')
sf::st_write(roads_df, 'output//sw_results_dc.shp')
file.remove('output//sw_results_dc.shp')
file.remove('output//sw_results_dc.prj')
file.remove('output//sw_results_dc.dbf')
file.remove('output//sw_results_dc.shx')
sf::st_write(sw, 'output//sw_results_dc.shp')
