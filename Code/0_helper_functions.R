# Helper functions
# DCM product name
productname_function <- function(aoi_vect){
  is_not_vect <- class(aoi_vect)!= "SpatVector"
  if(is_not_vect[1]){
    aoi_vect <- terra::vect(aoi_vect)
  }
  # The AOI ---
  aoi_ext <- terra::ext(aoi_vect)
  xmin <- aoi_ext[1]
  ymin <- aoi_ext[3]
  
  # The String ---
  str_1 <- "TDM1"
  tttt <- "DCM_"
  nn <- "10"
  B <- ifelse(ymin > 0, "N", "S")  ## North South
  bb <- formatC(abs(floor(ymin)), width = 2, format = "d", flag = "0") # Latitude
  X <- ifelse(xmin > 0, "E", "W") # East West
  xxx <- formatC(abs(floor(xmin)), width = 3, format = "d", flag = "0") # Longitude
  xxx <- formatC(abs(floor(xmin)), width = 3, format = "d", flag = "0") # Longitude
  
  
  location_string <- paste0(B,bb,X,xxx)
  
  name_string <- paste(str_1,tttt,nn,location_string, sep = "_")
  return(name_string)
  
}

### DLR file URL generator

DCM_URL_function <- function(aoi_vect){
  is_not_vect <- class(aoi_vect)!= "SpatVector"
  if(is_not_vect[1]){
    aoi_vect <- terra::vect(aoi_vect)
  }
  # The AOI ---
  aoi_ext <- terra::ext(aoi_vect)
  xmin <- aoi_ext[1]
  ymin <- aoi_ext[3]
  
  # The String ---
  str_1 <- "TDM1"
  tttt <- "DCM_"
  nn <- "10" 
  product_names <- c("FIRST1622_V01_C", "LAST1622_V01_C")
  
  B <- ifelse(ymin > 0, "N", "S")  ## North South
  bb <- formatC(abs(floor(ymin)), width = 2, format = "d", flag = "0") # Latitude
  X <- ifelse(xmin > 0, "E", "W") # East West
  xxx <- formatC(abs(floor(xmin)), width = 3, format = "d", flag = "0") # Longitude
  x10 <- formatC(floor(abs(floor(xmin))/10) * 10, width = 3, format = "d", flag = "0") # Longitude in 10 degree windows
  
  #URL Path
  base_dir <- "https://download.geoservice.dlr.de/TDM30_DCM/files"
  sub_1 <- paste0(str_1, "_", tttt, "_", nn, "_", B, bb)
  sub_2 <- paste0(sub_1,X, x10)
  sub_3 <- paste0(sub_1,X, xxx,"_", product_names)
  
  target_file <- paste0(sub_3, ".zip")
  
  full_path <- paste(base_dir, sub_1, sub_2, sub_3, target_file, sep = "/")
  
  return(full_path)
}




## UTM zone epsg code retriever:

## For getting suitable CRS
get_UTMname <- function (long, lat){
  
  zone <- (floor((long + 180)/6) %% 60) + 1
  NS <- ifelse(lat > 0, "N", "S")
  name_string <- paste0("UTM zone ", zone, NS)
  return(name_string)
  }
  

# error propagation:
rule_quadrature <- function(x,...){
  return(sqrt(sum(x^2,...)))
}



# define modus
Modes <- function(x) {
  x_clean <- na.omit(x)
  ux <- unique(x_clean)
  tab <- tabulate(match(x_clean, ux))
  ux[tab == max(tab)]
}


## Filter pixels based on error layer
pixel_filter <- function(val_lyr, err_lyr){
  ifelse( val_lyr < err_lyr & val_lyr > -1*err_lyr, NA, val_lyr)
}


# From here: https://stackoverflow.com/questions/52297978/decrease-overal-legend-size-elements-and-text
addSmallLegend <- function(myPlot, pointSize = 0.5, textSize = 3, spaceLegend = 0.1) {
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize))) +
    theme(legend.title = element_text(size = textSize), 
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines"))
}


