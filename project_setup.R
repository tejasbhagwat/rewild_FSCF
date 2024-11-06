#===================================================================================================================================
# Load basic packages 
library(sf); library(terra); library(ggplot2); library(dplyr); library(tidyr); library(mapview)
#===================================================================================================================================
# Species of interest
species_list = read.csv("species_list_EDGE.csv")
species_list[] <- lapply(species_list, function(x) {
  if (is.character(x)) {
    gsub("\\s+", " ", trimws(x))  # Replace multiple spaces with a single space
  } else {
    x
  }
})

species_list = c(species_list$SCI_NAME)
#===================================================================================================================================
# IUCN redlist data
iucn_range0 = read_sf("C:/Tejas_Uni_Goe/PhD/12_Beyond_PhD/Science_PolicyForum/IUCN_range_data/Terrestrial_and_freshwater/data_0.shp") |> 
              filter(SCI_NAME %in% species_list)
#iucn_range1 = read_sf("C:/Tejas_Uni_Goe/PhD/12_Beyond_PhD/Science_PolicyForum/IUCN_range_data/Terrestrial_and_freshwater/data_1.shp") |> 
#              filter(SCI_NAME %in% species_list)
#iucn_range2 = read_sf("C:/Tejas_Uni_Goe/PhD/12_Beyond_PhD/Science_PolicyForum/IUCN_range_data/Terrestrial_and_freshwater/data_2.shp") |> 
#              filter(SCI_NAME %in% species_list) 
#iucn_range3 = read_sf("C:/Tejas_Uni_Goe/PhD/12_Beyond_PhD/Science_PolicyForum/IUCN_range_data/Terrestrial_and_freshwater/data_3.shp") |> 
#              filter(SCI_NAME %in% species_list)


write_sf(iucn_range0, "species_ind.gpkg")
