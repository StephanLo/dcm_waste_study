library(terra)
library(sf)
library(dplyr)
## Calculate population and population density for each admin 2 boundary in GADM 3.6
#This script needs:
# I got gridded pop data from GHSL
# Whole world data download: https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_4326_3ss/V1-0/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0.zip
#_______________________________________________________________________________


# GADM data
setwd("working_folder")

## Population density GHSL
world_density <- rast("Data/aux_spatial_data/GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0/GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0.tif") 

## Admin boundaries GADM
admin_2 <- st_read("Data/aux_spatial_data/gadm36_levels_gpkg/gadm36_levels.gpkg", layer = "level2") |> 
  select("NAME_0", "NAME_1", "NAME_2", "ENGTYPE_2")


pop_densitity_level <- extract(world_density, admin_2, fun = "sum")

names(pop_densitity_level) <- c("ID", "POP_GHS_2020")
full_ds <- cbind(admin_boundaries, pop_densitity_level[,2])

st_write(full_ds, "Data/aux_spatial_data/World_lv2_pop.gpkg")

