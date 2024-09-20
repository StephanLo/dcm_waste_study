# Choosing landfill sites!

library(sf)
library(ggplot2)
library(dplyr)
library(openxlsx)
lab_wd <- "C:/Users/stephan/hokudai/PhD"
laptop_wd <- "C:/Users/louwa/Hokudai/PhD"
setwd(laptop_wd)

sf_use_s2(FALSE)


# Step One -------------
 # The data to consider:

 # All landfill sites in OSM database, larger than 100 m x 100m
 osm_landfills <- st_read("Data/Landfill_locations/osm_world_all.gpkg")

 # GHS Global population estimate 2020, aggragated into level 2 administrative boundarie from GADM 3.6
 glob_pop <- st_read("Data/aux_spatial_data/World_lv2_pop.gpkg")


 # subset pop dataset to only districts with landfill sites:
 admins_with_landfills <- st_filter(glob_pop,osm_landfills) |>
   mutate(admin_area = st_area(geom)) |>
   mutate(admin_density = pop_GHS/admin_area)



 # Get landfills.

 ## step 1: Get population for each landfill site:
 landfills_pop <- st_join(osm_landfills, admins_with_landfills, join = st_intersects)

 ## rank landfill sites by their lvl 2 pop density:
landfills_admin2 <- landfills_pop |> 
      group_by(NAME_2) |> 
      arrange(desc(site_area), .by_group = TRUE) |>
      filter(row_number() <= 1) |>
      ungroup() |>
      arrange(desc(admin_density)) |> 
      filter(row_number() <= 300) |> 
      mutate(AOI_ID = 1:n())

 st_write(landfills_admin2, "Data/Landfill_locations/possible_landfills_admin2_300.gpkg")

 openxlsx::write.xlsx(x = st_drop_geometry(landfills_admin2[,c("AOI_ID","name","NAME_EN","NAME_0", "NAME_2", "osm_id")]),
                      file = "Data/Landfill_locations/possible_landfills_admin2_300.xlsx")

 
 
 
 
# # about places with missing level 2 data (only level 1 data available)
 # admin_bounbdaries lvl 1
 admin_1 <- st_read("Data/aux_spatial_data/gadm36_levels_gpkg/gadm36_levels.gpkg", layer = "level1") |>
   select("NAME_0", "NAME_1")

 # what countries dont have level 2 data?
 adm_2_uniq <- unique(admin_2$NAME_0) # countries with admin_2
 missing_adm2 <- admin_1 |> filter(!NAME_0 %in% adm_2_uniq) # filter countries without level 2
 unique(missing_adm2$NAME_0) # 62 countries. yikes!

 pop_densitity_missing <- extract(world_density, missing_adm2, fun = "sum", bind = TRUE) |>
   st_as_sf()


 # work out pop density.
 pop_den_mis <- pop_densitity_missing |>
   mutate(admin_area = st_area(geometry)) |>
   mutate(admin_density = GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0/admin_area)

 osm_landfills <- st_read("Data/Landfill_locations/osm_world_all.gpkg")

 ex_landfills <- st_join(osm_landfills, pop_den_mis, left = FALSE) |> arrange(desc(admin_density))

 landfills_admin1 <- ex_landfills |>
   group_by(NAME_1) |>
   arrange(desc(site_area), .by_group = TRUE) |>  # rank landfills by suface area 
   filter(row_number() <= 1) |>  # Only 1 landfill per admin unit ()
   ungroup() |>
   arrange(desc(admin_density)) |>
   filter(row_number() <= 100) |>
 mutate(adm1AOI_ID = 1:n())

 st_write(landfills_admin1,"Data/Landfill_locations/possible_landfills_admin1_100.gpkg")



###==================================================
# STEP 2
## Load data into QGIS and use earth engine to cross validate potential sites, manually filter out until finally left with desired landfills. 
# save it as  "Data/Landfill_locations/selected_sites.gpkg"
 
#Load the data again thats been updated.
all_sites <- st_read("Data/Landfill_locations/selected_sites.gpkg")


all_sites$ID <- paste0("AOI_", formatC(1:nrow(all_sites), width = 3, format = "d", flag = "0"))

# as points:
all_points <- st_centroid(all_sites)

# with a 50 meter buffer around:
buf_dis <- units::set_units(50, m)

all_buffered <- st_buffer(all_sites, 
                            dist = buf_dis)

# save this data to final.
st_write(all_sites, "Data/Landfill_locations/final_sites.gpkg",layer = "aois",
         append = FALSE)
st_write(all_points, "Data/Landfill_locations/final_sites.gpkg",
         layer = "points", append = F)
st_write(all_buffered, "Data/Landfill_locations/final_sites.gpkg",
         layer = "buffer",  append = F)


# Get summary stats.
#summary_continent <- as.data.frame(all_sites) |> count(CONTINENT)
#summary_country <- as.data.frame(all_sites) |> count(NAME_0)

