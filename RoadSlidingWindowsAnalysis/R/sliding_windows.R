

.check_for_attribute <- function(roads, name){
  return(name %in% names(roads))
}

.check_pkey <- function(roads, pkey){
  return(length(unique(roads[[pkey]])) == nrow(roads))
}

.order_by_pky <- function(roads, pkey){
  return(roads[order(roads[[pkey]]),])
}

.fill_names <- function(roads, street_name){
  roads[[street_name]][is.na(roads[[street_name]])] <- "-- No Name Found --"
  return(roads)
}

.fill_fclass <- function(roads, fclass){
  roads[[fclass]][is.na(roads[[fclass]])] <- "-- No Functional Class Found --"
  return(roads)
}

.combine_roads <- function(roads, name='name', fclass='fclass'){
  combined_roads <- roads %>%
    dplyr::group_by(!!as.name(fclass), !!as.name(name)) %>%
    dplyr::summarise(geometry = sf::st_union(geometry)) %>%
    dplyr::ungroup()
  combined_roads$new_id <- dplyr::row_number(combined_roads$geometry)
  return(combined_roads)
}

.create_txtProgram_bar <- function(max){
  return(txtProgressBar(min = 0, max = max, style = 3))
}

.get_adj_lst <- function(roads){
  igraph::graph.adjlist(sf::st_touches(roads))
}

.get_seq <- function(n, per, step, length){
  if(n <= 1){
    seq <- seq(0.0, n, step)
  } else {
    seq <- seq(0, 1, per)
  }
  return(seq)
}

.linemerge_line <- function(line){
  l <- sf::st_cast(sf::st_line_merge(sf::st_union(lwgeom::st_snap_to_grid(line, .1))), "LINESTRING")
  l <- sf::st_as_sf(l)
  return(l)
}

.get_percent <- function(length, step){
  return(1/(length/step))
}

.group_by_location <- function(roads, grouped_roads, name='name', fclass='fclass'){
  pb <- .create_txtProgram_bar(nrow(grouped_roads))
  roads_df <- lapply(1:nrow(grouped_roads), function(i){
    setTxtProgressBar(pb, i)
    roads_temp <- roads[ which(roads[[name]]==grouped_roads[i,][[name]]
                                       & roads[[fclass]] == grouped_roads[i,][[fclass]]), ]
    if(nrow(roads_temp) == 1){
      roads_temp_grouped <- grouped_roads[i,] %>%
        dplyr::select(geometry) 
        
    } else {
      com <- igraph::components(.get_adj_lst(roads_temp))
      roads_temp$groups <- com$membership
      roads_temp_grouped <- roads_temp %>%
        dplyr::group_by(groups) %>%
        dplyr::summarise(geometry = sf::st_union(geometry)) %>%
        dplyr::select(-groups)
    }
  })
  return(roads_df %>% dplyr::bind_rows())
}


#' Creates road corridors, grouped by name, functional classification, and name
#'
#' Results are road segments aggregated by name, functional class, and proximity to each other. Can be considered corridors.
#'
#' @param roads roads simple feature object
#' @param name title of the attribute with the street names
#' @param fclass title of the attribute with the fclass values
#' @return a simple features object of corridors
#' @export
create_corridors <- function(roads, fclass='fclass', name='name'){
  print('Combining segments')
  cb <- .combine_roads(roads, fclass, name)
  print('Accounting for proximity ...')
  loc <- NULL
  
  for (name in unique(cb[[fclass]])){
    r_df <- .group_by_location(roads, cb, fclass, name)
    loc <- rbind(r_df, loc)
  }
  return(loc)
}

#' Builds segments for Sliding Windows Analysis
#'
#' Results are a polyline object that can help to identify locations 
#' with a high frequency of crashes, highlighting potential high-injury areas.
#'
#' @param roads Path to the input file
#' @param name title of the attribute with the street names
#' @param fclass title of the attribute with the fclass values
#' @param win max window length in miles
#' @param step distance between each step in miles
#' @return a simple features object of windows from sliding windows analysis
#' @export
create_sliding_windows <- function(roads, name='name', fclass='fclass', win=0.5, step=0.1){
  roads_final <- transform_to_utm(roads)
  roads_final <- .linemerge_line(roads_final)
  roads_final$length_mi <- as.double(sf::st_length(roads_final)/1609.34)
  pb <- .create_txtProgram_bar(nrow(roads_final))
  sw <- lapply(1:nrow(roads_final), function(i){
    per <- .get_percent(roads_final[i,]$length_mi, step)
    seq <- .get_seq(roads_final[i,]$length_mi, per, step)
    theline <- roads_final[i,]
    f <- lapply(seq, function(x){
      if (x + step/win <= 1){
        p <- lwgeom::st_linesubstring(theline, x, ifelse(x + step/win < 1, x + step/win, 1))
        sf::st_as_sf(p, sf_column_name='geometry')
      }
    })
    setTxtProgressBar(pb, i)
    f %>% dplyr::bind_rows()
  })
  final_swa <- sw %>% dplyr::bind_rows()
  final_swa$fid <- dplyr::row_number(final_swa)
  
  return(final_swa)
}

#' Creates road corridors, grouped by name, functional classification, and name
#'
#' Results are road segments aggregated by name, functional class, and proximity to each other. Can be considered corridors.
#'
#' @param crashes sf object of road crashes 
#' @param sliding_windows sf object of sliding windows segments
#' @param crash_sev string, name of vehicle mode attribute within the crash data
#' @param kas combined vector of fatality and severe injury crashes, e.g., c('K', 'A')
#' @param crash_mode string, name of vehicle mode attribute within the crash data
#' @param buffer string, name of vehicle mode attribute within the crash data
#' @param sev_weights dictionary, name of vehicle mode attribute within the crash data
#' @return a simple features object of corridors
#' @export
# counts_crashes <- function(crashes, 
#                             sliding_windows, 
#                             crash_sev_col, 
#                             mode,
#                             bike_crashes,
#                             ped_crashes,
#                             k,
#                             a,
#                             b,
#                             c,
#                             o,
#                             buffer, 
#                             sev_weights=list(k=1, a=2, b=1, c=1, o=1)
#                             ){
#   crashes <- transform_to_utm(crashes)
#   sliding_windows <- transform_to_utm(sliding_windows)
#   sliding_windows <- sliding_windows %>% 
#   bike_crashes <- 
#   ped_crashes <- 
#   kas_crashes <-



#   f <- lapply(1:nrow(sliding_windows), function(x){
      
#     })

#   return(loc)
# }
