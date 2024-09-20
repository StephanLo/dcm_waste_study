## Download DCM products for all our sites.

setwd("working_directory")
library(sf)
library(dplyr)
library(RCurl)
source("Code/0_helper_functions.R")

my_sites <- st_read("Data/Landfill_locations/my_sites.gpkg")
DCM_productnames <- st_read("Data/aux_spatial_data/DCM_grid.gpkg")


sink("Processing/download_list.txt")

for( row in 1:nrow(my_sites)){
  aoi <- my_sites[row,]
  intersecting_tiles <- st_filter(DCM_productnames, aoi)
  all_paths <-c(intersecting_tiles$first_DCM_URL, intersecting_tiles$last_DCM_URL)
  
  # print te path to text file:
  for( item in all_paths){
    cat(item)
    cat("\n")
  }
  
}
sink()

