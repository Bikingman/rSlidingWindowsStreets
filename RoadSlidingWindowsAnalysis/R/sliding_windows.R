

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

.group_by_location <- function(roads, grouped_roads, name='name', fclass='fclass'){
  pb <- .create_txtProgram_bar(nrow(grouped_roads))
  roads_no_geom <- grouped_roads %>% sf::st_drop_geometry()
  for (i in 1:nrow(roads_no_geom)) {
    roads_temp <- roads[ which(roads[[name]]==roads_no_geom[i,][[name]]
                               & roads[[fclass]] == roads_no_geom[i,][[fclass]]), ]
    com <- igraph::components(.get_adj_lst(roads_temp))
    roads_temp$groups <- com$membership
    roads_temp_grouped <- roads_temp %>%
      dplyr::group_by(groups) %>%
      dplyr::summarise(geometry = sf::st_union(geometry)) %>%
      dplyr::ungroup()
    if (i ==1){
      roads_final <- roads_temp_grouped
    } else {
      roads_final <- rbind(roads_final, roads_temp_grouped)
    }
    setTxtProgressBar(pb, i)
  }
  return(roads_final)
}


.get_seq <- function(n, per, step){
  if(n <= 1){
    seq <- c(0.0, 1, step)
  } else {
    seq <- seq(0, 1, per)
  }
  return(seq)
}

.linemerge_line <- function(line){
  l <- sf::st_cast(sf::st_line_merge(
    st_union(sf::st_cast(line, "MULTILINESTRING"))), "LINESTRING")
  l <- sf::st_as_sf(l)
  return(l)
}

.get_percent <- function(length, step){
  return(1/(length/step))
}


#' Creates 
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
  loc <- .group_by_location(roads, cb, fclass, name)
  return(sf::st_cast(loc, 'MULTILINESTRING'))
}

#' Builds Sliding Windows Analysis
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
  roads_final$length_mi <- as.double(sf::st_length(roads_final)/1609.34)
  pb <- .create_txtProgram_bar(nrow(roads_final))
  for (i in 1:nrow(roads_final)) {
    start_time <- Sys.time()
    if (roads_final[i,]$length_mi > win){
      
      line <- .linemerge_line(roads_final[i,])
      per <- .get_percent(roads_final[i,]$length_mi, step)
      seq <- .get_seq(roads_final[i,]$length_mi, per, step)
      n <- 0
      for (j in seq){
        n <- n + per
        p <- lwgeom::st_linesubstring(line, n, ifelse(n + (step*(win*10)) < 1, n + (step*(win*10)), 1))
        p <- sf::st_as_sf(p, sf_column_name='geometry')
        if (j+per == per && i == 1) {
          final_swa <- p
        } else {
          final_swa <- rbind(final_swa,  p)
        }
      }
    } else {
      if ( i == 1) {
        final_swa <- roads_final[i,]$geometry
      } else {
        final_swa <- rbind(final_swa,  roads_final[i,]$geometry)
      }
    }
    print(Sys.time() - start_time)
    setTxtProgressBar(pb, i)
  }
  return(final_swa)
}


