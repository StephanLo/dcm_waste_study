## Get world openstreetmap data for landfill sites

library(osmdata)
library(sf)
library(tmap)
library(dplyr)

setwd("working_directory") # update to match yours
sf_use_s2(FALSE)

continents <- st_read("Data/aux_spatial_data/World_Continents.geojson") # see Data notes
countries <- st_read("Data/aux_spatial_data/wb_countries_admin0_10m/WB_countries_Admin0_10m.shp") # See data notes
utm_grid <- st_read("Data/aux_spatial_data/World_UTM_Grid/World_UTM_Grid.shp")
world_data <- data.frame()

min_size_thresh <- units::set_units(1e4, m^2)
################################################ OSM QUERY

for(i in 1:nrow(utm_grid)){
  aoi <- utm_grid[i,]
  print(i)
  bb <- st_bbox(aoi)
  
  x <- opq(bbox = bb) |>
    add_osm_features(list('landuse' = 'landfill',
                          "amenity" = "waste_disposal")) |>
    add_osm_features(list(key = "name")) |>
    osmdata_sf()
  
  ## landfill polygons. # get name  columns
  
  if(!is.null(x$osm_polygons)){
    if("name" %in% colnames(x$osm_polygons)){
      pol_sites <- x$osm_polygons |>
        #filter(!is.null(name)) |>
        #filter(!is.na(name)) |>
        select("osm_id","name") |> 
        st_cast(to = "MULTIPOLYGON") |>
        mutate(site_area = st_area(geometry)) |>
        filter(site_area > min_size_thresh)
    }
  }
  
  if(!is.null(x$osm_multipolygons)){
    if("name" %in% colnames(x$osm_multipolygons)){
      mult_pol <- x$osm_multipolygons |>
        #filter(!is.null(name)) |>
        #filter(!is.na(name)) |>
        select("osm_id","name") |>
        mutate(site_area = st_area(geometry)) |>
        filter(site_area > min_size_thresh)
    }
  }
  
  if(exists("pol_sites") & exists("mult_pol")){
    # combine with continent and country data.
    all_features <- rbind(pol_sites,mult_pol) |> 
      st_join(countries[c("NAME_EN",
                          "CONTINENT",
                          "REGION_UN", 
                          "SUBREGION",
                          "GDP_MD_EST",
                          "ECONOMY",
                          "POP_EST")])
    
    world_data <- rbind(world_data, all_features)
  } else if(exists("pol_sites")){
    # combine with continent and country data.
    all_features <- pol_sites |> 
      st_join(countries[c("NAME_EN",
                          "CONTINENT",
                          "REGION_UN", 
                          "SUBREGION",
                          "GDP_MD_EST",
                          "ECONOMY",
                          "POP_EST")]) 
    
    world_data <- rbind(world_data, all_features)
  } else  if(exists("mult_pol")){
    # combine with continent and country data.
    all_features <- mult_pol |> 
      st_join(countries[c("NAME_EN",
                          "CONTINENT",
                          "REGION_UN", 
                          "SUBREGION",
                          "GDP_MD_EST",
                          "ECONOMY",
                          "POP_EST")])
    
    world_data <- rbind(world_data, all_features)
  }
  
  if(exists("pol_sites")){
    rm(pol_sites)  
  }
  if(exists("mult_pol")){
    rm(mult_pol)  
  }
}


st_write(world_data,paste0("Data/Landfill_locations/", "osm_world_all.gpkg"))


