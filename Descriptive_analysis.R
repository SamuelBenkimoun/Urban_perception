# Script handling the descriptive analysis part of the dataset
library(dplyr)
library(readr)
library(sf)
library(tmap)
library(ggridges)
library(ggplot2)
library(stringr)
# Importing the formatted dataset
# i.e. the output of the Data_extraction_formatting.R script
htmlpath = "~/Documents/Charbon/VILLE IDEALE PROJET/comments_PLM.csv"
commentaires_plm <- read.csv(htmlpath)
# Importing the spatial administrative layer, to allow mapping
# Can be downloaded from here: https://geoservices.ign.fr/adminexpress
htmlpath = "~/Documents/Charbon/Geo Data/ADMIN-EXPRESS_3-1__SHP_LAMB93_FXX_2023-03-20/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2023-03-20/ADE_3-1_SHP_LAMB93_FXX/ARRONDISSEMENT_MUNICIPAL.shp"
arr <- st_read(htmlpath)

## MAPPING THE DIFFERENT INDICATORS BY ARRONDISSEMENTS
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
comments_sf_ag$ville <- sapply(strsplit(as.character(comments_sf_ag$loc), " "), `[`, 1)
# Total of comments by arrondissements
count_comments <- comments_sf$loc %>% 
  table() %>% 
  as_tibble() 
colnames(count_comments) <- c("loc", "nb_comments")
comments_sf_ag <- merge(comments_sf_ag, count_comments)
# Ratio of comments per inhabitants
comments_sf_ag <- comments_sf_ag %>% mutate(comments_ratio = nb_comments/(pop/1000))
# Adding an arrondissement number to produce labels on the maps
#comments_sf_ag <- comments_sf_ag %>% mutate(num_label = as.numeric(str_extract(loc, "\\d+")))

# Mapping function for the different variables and indicators
generate_gradient_map <- function(var, title) {
  # Calculate the average to put it in the map credits
  avg_value <- round(mean(comments_sf_ag[[var]], na.rm = TRUE), 2)
  # Generate the map
  tm_shape(comments_sf_ag) +
    tm_fill(col = var, 
            palette = "viridis", 
            title = title, 
            style = 'cont', 
            textNA = "",
            legend.is.portrait = FALSE) +
    tm_facets(by = "ville") +
    #tm_labels(text = "num_label") +
    tm_scalebar(position = c("left", "bottom"), width = 8) +
    tm_layout(
      legend.outside = TRUE,
      legend.title.size = 1,
      legend.title.fontfamily = "Avenir Next",
      legend.title.fontface = "bold",
      legend.text.size = 0.8,
      legend.orientation = "landscape",
      legend.position = tm_pos_out(cell.v = "bottom", cell.h = "center")
    ) +
    tm_credits(
      text = stringr::str_c("Dataset average: ", avg_value,
      "\nSource: Ville Idéale, 2012-2024, IGN, 2023."),
      position = tm_pos_out(cell.v = "bottom", cell.h = "center"),
      size = 0.8
    )
}

# Mapping the average grading by arrondissements
generate_gradient_map(var = "average", 
                      title = "Average general rating /10")
# Mapping all the other grading categories
categories_to_map <- colnames(comments_sf%>%st_drop_geometry())[6:14]  #can be updated to get a restricted set of categories
categories_to_map # checking the selected categories
for (cat in categories_to_map) {
  category_labels <- c(
    environment = "environment",
    transports = "transportation",
    securite = "safety",
    sante = "health",
    sp_loisirs = "sports and leisure",
    culture = "culture",
    enseignement = "education",
    commerces = "commerce",
    qualite_de_vie = "quality of life"
  )
  cat_label <- category_labels[cat]
  map <- generate_gradient_map(var = cat, 
                        title = paste("Average", cat_label, "rating /10"))
  assign(paste0("map_", cat), map)
}

# Displaying the environment category map for exemple
map_environment

# Mapping the ratio of comments per inhabitants by arrondissements
map_ratio_comments <- generate_gradient_map(var = "comments_ratio", 
                      title = "Number of comments/1000 inhabitants")
map_ratio_comments

# Mapping the total count of comments per arrondissements
tm_shape(comments_sf_ag) +
  tm_polygons(fill = "ville", legend.show = FALSE)+
  tm_symbols(size = "nb_comments",
             title.size = "Number of comments:",
             sizes.legend = c(10, 25, 50, 100, 150),
             sizes.legend.labels = c(10, 25, 50, 100, 150)) +
  tm_facets(by = "ville") +
  tm_layout(legend.outside = TRUE,
            legend.title.size = 1,
            legend.title.fontfamily = "Avenir Next",
            legend.title.fontface = "bold",
            legend.text.size = 0.8,
            legend.orientation = "landscape",
            legend.position = tm_pos_out(cell.v = "bottom", cell.h = "center")) +
  tm_scalebar(position = c("left", "bottom"), width = 8) +
  tm_credits(text = stringr::str_c("Source: Ville Inventive, 2012-2024, IGN, 2023.\n", "Dataset average: ", round(mean(comments_sf_ag$average),2)),
             position = tm_pos_out(cell.v = "bottom", cell.h = "center"),
             size = 0.8
  )

## PLOTTING THE RATING DISTRIBUTIONS

# crop the name to just keep the arrondissement number
commentaires_plm$arr_label <- str_extract(commentaires_plm$loc, "\\d{1,2}(ER|E) ARRONDISSEMENT")
# convert it to a factor, ordered based on the number value so that the labels will appear in the right order when plotting
commentaires_plm <- commentaires_plm %>%
  mutate(
    # Extract the number only (e.g. "1" from "1ER ARRONDISSEMENT")
    arr_num = as.numeric(str_extract(arr_label, "\\d+")),
    # Reorder arr_label factor levels based on [invert order of] arr_num
    arr_label = forcats::fct_reorder(arr_label, -arr_num)
  )

# If needed for the $ville attribute
commentaires_plm <- commentaires_plm %>%
  mutate(ville = sapply(strsplit(as.character(loc), " "), function(x) x[1]))

# Plotting the distribution by arrondissement in each city
ggplot(data=commentaires_plm, aes(x=average, y=arr_label, fill=ville)) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 2, quantile_lines = TRUE
  ) +
  theme_minimal()+
  scale_fill_viridis_d(alpha=.4, direction = -1, option = "H") +
  xlab("Average (general) rating /10")+
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size=18),
        axis.title.y = element_blank(),
  ) +
  labs(title = "Distribution of ratings by arrondissements") + 
  facet_wrap(~ville, scales = "free_y")

## TRYING TO CLASSIFY THE DISTRIBUTIONS BASED ON THEIR SHAPE

quantiles <- seq(0, 1, by = 0.05)
# Compute the quantiles by 'loc'
comm_quantiles <- commentaires_plm %>%
  group_by(loc) %>% # To get the quantiles per arrondissements
  summarise(
    quantiles = list(quantile(average, probs = quantiles, na.rm = TRUE)) # Getting it for the whole specific list of quantiles
  ) %>%
  tidyr::unnest_wider(quantiles, names_sep = "_q") # Creates a new column for each of the quantiles
head(comm_quantiles)
# Standardising (?) the values, to classify based on the shape and not on the values
comm_quantiles.sd <- comm_quantiles[, -1] %>% 
  t() %>% #Transposing to get the scale function operating vertically
  scale(center = TRUE, scale = TRUE) %>%  #Standardising
  t() %>% #Transposing it back to the original format
  as_tibble() #t() function had turnt it into a matrix
comm_quantiles.sd <- cbind(comm_quantiles[,1], comm_quantiles.sd) #Reattaching the arrondissements names
comm_quantiles.sd
