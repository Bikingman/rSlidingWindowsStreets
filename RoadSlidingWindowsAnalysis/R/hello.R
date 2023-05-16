# Hello, world!
#
# This is an example function named 'hello' 
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


library(tidyverse)
library(sf)
library(igraph)
library(stplanr)
# load data
setwd("~/code/RoadsSlidingWindowsAnalysis/data/district-of-columbia-latest-free.shp")
roads <- st_read('gis_osm_roads_free_1.shp')
roads_base <- roads[order(roads$osm_id),]

roads <- roads_base %>% filter(fclass %in% c( 'motorway', 'motorway_link',
                                               'primary', 'primary_link',
                                               'secondary', 'secondary_link',
                                               'trunk', 'trunk_link',
                                               'tertiary', 'tertiary_link',
                                               'residential', 'living_street',
                                               'bus_guideway', 'busway',
                                               'road', 'cycleway',
                                               'track', 'unclassified'))

roads$name[is.na(roads$name)] <- "-- No Name Found --"
roads$name[is.na(roads$fclass)] <- "-- No Functional Class Found --"

combined_roads <-roads %>% 
  group_by(fclass, name) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

combined_roads$new_id <- row_number(combined_roads$geometry)

pb <- txtProgressBar(min = 1, max = nrow(combined_roads), style = 3)
for (i in 1:nrow(combined_roads)) {
  roads_temp <- roads %>% filter(name==combined_roads[i,]$name) %>% filter(fclass==combined_roads[i,]$fclass)
  touching_list = st_touches(roads_temp)
  g = graph.adjlist(touching_list)
  c = components(g)
  roads_temp$groups <- c$membership
  roads_temp_grouped <-roads_temp %>% 
    group_by(groups) %>%
    summarise(geometry = sf::st_union(geometry)) %>%
    ungroup()
  if (i ==1){
    roads_final <-roads_temp_grouped
  } else {
    roads_final <- rbind(roads_final, roads_temp_grouped)
  }
  setTxtProgressBar(pb, i)
}



file.remove('output//points.shp')
file.remove('output//points.prj')
file.remove('output//points.dbf')
file.remove('output//points.shx')
st_write(extended_points, 'output//points.shp')
file.remove('output//roads.shp')
file.remove('output//roads.prj')
file.remove('output//roads.dbf')
file.remove('output//roads.shx')
st_write(roads_final, 'output//roads.shp')