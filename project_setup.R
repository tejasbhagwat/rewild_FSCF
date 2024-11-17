#===================================================================================================================================
# Load basic packages 
#===================================================================================================================================
library(sf); library(terra); library(ggplot2); library(dplyr); library(tidyr); library(tmap); 
library(stringr); library(mapview); library(readr)
#mapviewOptions(basemaps = "Esri.WorldImagery")
#===================================================================================================================================
# Grant EDGE Priority list - Remove white spaces from the .csv file
#===================================================================================================================================
priority_species_list = read.csv("species_list_EDGE.csv")
priority_species_list[] <- lapply(priority_species_list, function(x) {
  if (is.character(x)) {
    gsub("\\s+", " ", trimws(x))  # Replace multiple spaces with a single space
  } else {
    x
  }
})
colnames(priority_species_list)
priority_species_list = priority_species_list %>% dplyr::rename(sci_name = SCI_NAME, common_name = Common_name) #%>% filter(str_detect(Country, "India"))
#===================================================================================================================================
# IUCN Red-list data
#===================================================================================================================================
iucn_terr_mamm = read_sf("C:/Tejas_Uni_Goe/PhD/12_Beyond_PhD/Rewild_fonseca_species_conservation_fund/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
#-----------------------------------------------------------------------------------------------------------------------------------
# Filtering to priority categories and priority taxa
iucn_small_mamm_priority = iucn_terr_mamm %>% dplyr::filter(order_ == "RODENTIA" | order_ == "EULIPOTYPHLA") %>%
                           dplyr::filter(category %in% c("DD","CR","VU","EX","EN")) %>% 
                           st_make_valid() %>% st_transform(3857) %>%
                           group_by(sci_name, order_, category) %>% 
                           summarise(geometry = st_union(geometry))

table(iucn_small_mamm_priority$category)

#===================================================================================================================================
# Filtering to those in the Indian states
#===================================================================================================================================
#-----------------------------------------------------------------------------------------------------------------------------------
# Load India borders
ind_adm = read_sf("C:/Tejas_Uni_Goe/PhD/12_Beyond_PhD/AIGrant_2024/02_PAs/IN_ABh.shp") %>% st_make_valid() %>% 
          st_transform(3857) %>% mutate(STATE = str_replace(STATE, "ANDAMAN AND NICOBAR ISLANDS", "merged"))

ind_states = read_sf("C:/Tejas_Uni_Goe/PhD/04_Data/02_Chapter2_India/01_RS_GIS/03_Administrative_Units/gadm36_IND_1/gadm36_IND_1.shp") %>% 
                  st_make_valid() %>% st_transform(3857) 
#-----------------------------------------------------------------------------------------------------------------------------------
# Load priority states
priority_states = ind_states %>% dplyr::filter(NAME_1 %in% c("West Bengal","Sikkim","Assam","Arunachal Pradesh","Tripura",
                                                             "Nagaland","Meghalaya","Mizoram","Manipur"))
write_sf(priority_states, "priority_states.shp")
#tm_shape(priority_states) + tm_polygons(alpha = 0.4)
#-----------------------------------------------------------------------------------------------------------------------------------
# Filter species by states
iucn_small_mamm_priority_ind = iucn_small_mamm_priority %>% st_intersection(priority_states) %>% 
                               dplyr::select(sci_name, order_, category) %>% group_by(sci_name, order_, category) %>% 
                               summarise(geometry = st_union(geometry))

# write_csv(st_drop_geometry(iucn_small_mamm_priority_ind), "iucn_small_mamm_priority_ind.csv")
#-----------------------------------------------------------------------------------------------------------------------------------
# Load species common names (These are only retrieved for species exported in the previous step)
species_common_names = read_csv("C:/Tejas_Uni_Goe/PhD/12_Beyond_PhD/Rewild_fonseca_species_conservation_fund/MAMMALS_TERRESTRIAL_ONLY/Scientific_to_common_names.txt")
# Remove white space
species_common_names[] <- lapply(species_common_names, function(x) {
  if (is.character(x)) {
    gsub("\\s+", " ", trimws(x))  # Replace multiple spaces with a single space
  } else {
    x
  }
})
print(species_common_names, n = 25)
colnames(species_common_names)
species_common_names = species_common_names %>% dplyr::rename(sci_name = `Scientific Name`, common_name = `Common Name`)

# Add the common names to the data.frame 
iucn_small_mamm_priority_ind = iucn_small_mamm_priority_ind %>% left_join(species_common_names)

#===================================================================================================================================
# Protected areas
#===================================================================================================================================
protected_areas_IN = read_sf("C:/Tejas_Uni_Goe/PhD/12_Beyond_PhD/Science_PolicyForum/Protected_Area_India_Final/Protected_Area_India_Final.shp") %>%
                     st_make_valid() %>% st_transform(3857) %>% st_intersection(priority_states)

#===================================================================================================================================
# Species richness score 
#===================================================================================================================================
spec_richness = read_sf("C:/Tejas_Uni_Goe/PhD/12_Beyond_PhD/Science_PolicyForum/Final_maps/HKB_Final_Score.gpkg") %>% 
                st_transform(3857) %>% st_intersection(priority_states)



# Base map with priority states
ggplot() +
  # Priority states borders (transparent fill)
  geom_sf(data = priority_states, color = "grey", alpha = 0, linewidth = 0.2, linetype = "solid", size = 0.3) +
  
  # Small mammal priority areas (filled by common_name)
  geom_sf(data = iucn_small_mamm_priority_ind, aes(fill = common_name), color = NA, alpha = 0.4) +
  
  # Protected areas with border colors based on Type
  geom_sf(data = protected_areas_IN, 
          aes(color = Type), # Border color mapped to Type
          fill = NA,         # No fill for protected areas
          linewidth = 0.3) + # Border width
  
  # Set manual colors for protected areas
  scale_color_manual(values = c("National Park" = "darkred", 
                                "Wildlife Sanctuary" = "darkblue")) +
  
  # Fill legend for small mammals
  scale_fill_discrete(name = "Priority species") +
  
  # Add legend and theme adjustments
  theme_minimal() +
  theme(legend.position = "right") +
  labs(color = "Protected Area Type", 
       fill = "Small Mammal Priority Areas")
























