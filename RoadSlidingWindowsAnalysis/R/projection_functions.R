
get_medroid <- function(spatial_data){
  coors <- as.data.frame(sf::st_coordinates(spatial_data)[,1:2])
  median <- data.frame(x=median(coors$X), y=median(coors$Y))
  return(sf::st_as_sf(median, coords = c('x','y'), remove = FALSE))
}

# returns UTM code from table in WGS84 format 
get_utm_code <- function(lng, lat){
  utm_band = toString((floor((lng + 180 ) / 6 ) %% 60) + 1)
  if (nchar(utm_band) == 1) {
    utm_band <- paste0('0', utm_band)
  } 
  if (lat >= 0 ) { 
    return(paste0('326', utm_band))
  } else {
    return(paste0('327', utm_band))
  }
}

transform_to_utm <- function(spatial_table){
  spatial_table <- sf::st_transform(spatial_table, 4326)
  latlong <- get_medroid(spatial_table)
  crs <- get_utm_code(latlong$x, latlong$y)
  spatial_table <- sf::st_transform(spatial_table, as.integer(crs))
  return(spatial_table)
}