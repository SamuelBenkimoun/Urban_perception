# Script handling the descriptive analysis part of the dataset
library(readr)
library(sf)
library(tmap)
library(ggridges)
library(dplyr)
library(ggplot2)
# Importing the formatted dataset
# i.e. the output of the Data_extraction_formatting.R script
htmlpath = "~/Documents/Charbon/VILLE IDEALE PROJET/comments_PLM.csv"
commentaires_plm <- read.csv(htmlpath)
# Importing the spatial administrative layer, to allow mapping
# Can be downloaded from here: https://geoservices.ign.fr/adminexpress
htmlpath = "~/Documents/Charbon/Geo Data/ADMIN-EXPRESS_3-1__SHP_LAMB93_FXX_2023-03-20/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2023-03-20/ADE_3-1_SHP_LAMB93_FXX/ARRONDISSEMENT_MUNICIPAL.shp"
arr <- st_read(htmlpath)

## MAPPING THE NUMBER OF COMMENTS
# Checking that the locality names are a suitable key attribute to join the two datasets
commentaires_plm$loc <- factor(commentaires_plm$loc, levels = sort(unique(commentaires_plm$loc)))
arr$NOM_M <- factor(arr$NOM_M, levels = sort(unique(arr$NOM_M)))
head(unique(commentaires_plm$loc))
head(unique(arr$NOM_M))
# Verifying that no diff exists between the two locality names vectors
setdiff(commentaires_plm$loc, arr$NOM_M)
# Matching the column names with the comments datasets
arr <- arr %>% rename(loc = NOM_M,
                      pop = POPULATION)
# Joining the geo layer to the comments dataset
comments_sf <- merge(arr[c("loc", "pop", "geometry")], commentaires_plm)
# Aggregating the different indicators by arrondissement 
comments_sf_ag <- aggregate(comments_sf, by=list(comments_sf$loc), FUN= mean) %>%
  mutate(loc = Group.1) %>% # Keeping the arrondissement name in the loc column
  select(-Group.1)
# Updating the field to tell in which city each arrondissement is
comments_sf_ag$ville <- comments_sf_ag$loc <- sapply(strsplit(as.character(comments_sf_ag$loc), " "), `[`, 1)
# Mapping the values (general rating average by arrondissement)
tm_shape(comments_sf_ag) +
  tm_fill(col = "average", 
          palette = "viridis", 
          title = "Average general rating /10", 
          style = 'cont', 
          textNA = "") +
  tm_facets(by = "loc") +
  tm_scalebar(position = c("left", "bottom"))+
  tm_layout(legend.outside = TRUE,
            legend.title.size = 1,
            legend.title.fontfamily = "Avenir Next",
            legend.title.fontface = "bold",
            legend.text.size = 0.8,
            legend.position = tm_pos_out(cell.v = "center"),
            credits.text = "ok",
            credits.position = c("left", "bottom")) +
  tm_credits(text = stringr::str_c("Source: Ville Inventive, 2012-2024, IGN, 2023.\n", "Dataset average: ", round(mean(comments_sf_ag$average),2)),
             position = tm_pos_out())

