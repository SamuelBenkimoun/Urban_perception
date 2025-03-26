# Script ensuring the data extraction and formatting part.

library(xml2)
library(rvest)
library(stringr)
library(readr)
library(dplyr)

## Parsing the html pages into a single dataframe format
parse <- function (page) {
  # Getting the city name
  loc <- page %>% html_elements(xpath = "//h1") %>%
    html_text() %>%
    gsub(pattern=" \\(.*", replacement = "")
  # Getting the code
  code <- page %>% html_elements(xpath = "//h1") %>%
    html_text() %>%
    gsub(pattern=".*\\(", replacement = "") %>%
    gsub(pattern = "\\)", replacement ="")
  # Extracting the user name
  user <- page %>% paste( collapse = "\n") %>%
    str_match_all("Par <strong>(.*?)</strong>") %>% 
    as.data.frame() %>% 
    select(2) %>%
    rename(user = X2)
  # Getting the ratings
  notes <- page %>% html_nodes(".comm") %>%
    html_nodes("table") %>%
    html_nodes("td") %>%
    html_text()
  # Getting the title of the rating categories
  categories <- page %>% html_elements(xpath = "//th") %>%
    html_text()
  categories <- categories[1:9] 
  # Getting the dates of the reviews
  dates <- page %>% html_nodes(".comm") %>%
    html_text() %>%
    str_extract("([0-9]+(-[0-9]+)+)")
  # Getting the positive part of the review
  comm_positif <- page %>% html_nodes(".comm") %>%
    html_text() %>%
    gsub(pattern = ".*points positifs : ", replacement =  "") %>%
    gsub(pattern = "Les points négatifs : .*", replacement = "")
  # Getting the negative part of the review
  comm_negatif <- page %>% html_nodes(".comm") %>%
    html_text() %>%
    gsub(pattern = ".*points négatifs : ", replacement =  "") %>%
    gsub(pattern = "Signaler ce commentaire.*", replacement = "")
  # Getting the feedback on the review
  feedback <- page %>% html_elements(xpath = "//strong") %>%
    html_text() %>%
    as.numeric() %>%
    na.omit()
  
  # Building the dataframe from the information
  data <- tibble(
    code = rep(code, length(comm_positif)),
    loc = rep(loc, length(comm_positif)),
    dates,
    user,
    environment = notes[seq(1,length(notes),9)],
    transports = notes[seq(2,length(notes),9)],
    securite = notes[seq(3,length(notes),9)],
    sante = notes[seq(4,length(notes),9)],
    sp_loisirs = notes[seq(5,length(notes),9)],
    culture = notes[seq(6,length(notes),9)],
    enseignement = notes[seq(7,length(notes),9)],
    commerces = notes[seq(8,length(notes),9)],
    qualite_de_vie = notes[seq(9,length(notes),9)],
    comm_positif,
    comm_negatif,
    agree = feedback[seq(2,length(feedback),3)],
    disagree = feedback[seq(3,length(feedback),3)]
  )
  return(data)
}

# TO BE ADAPTED DEPENDING ON WHERE THE PAGES ARE STORED
htmlpath = "path/to/html/files"
# Setting the working directory
setwd(htmlpath)
# Listing all the files from the folder
list_pages <- list.files()
# Storing all the files into a list
pages <- lapply(list_pages, read_html, header=TRUE, sep=",")
head(pages)
# Parsing all the html pages and binding it into a single dataframe
commentaires_plm <- purrr::map(pages,parse) %>%
  dplyr::bind_rows()
head(commentaires_plm)

## DATA FORMATTING
# Removing the potential duplicates
commentaires_plm <- commentaires_plm %>% distinct(loc, dates, user, .keep_all = T)
# Formatting the dates
commentaires_plm <- commentaires_plm %>% mutate(dates = lubridate::dmy(commentaires_plm$dates)) 
# Creating a month-year date format to improve readability
commentaires_plm <- commentaires_plm %>% mutate(month = format(as.Date(dates, format = "%d-%m-%Y"), "%Y-%m"))
# Formatting the numeric values
commentaires_plm[5:13] <- lapply(commentaires_plm[5:13], function(x) as.numeric(x))
# Averaging the different categories to get the overall grade
commentaires_plm$average <- commentaires_plm[5:13] %>% rowMeans() %>% round(1)
# Showing the structure of the dataframe
tibble::glimpse(commentaires_plm[, !names(commentaires_plm) %in% "user"])
# Creating a city column
commentaires_plm <- commentaires_plm %>% mutate(ville = str_to_title(str_remove(loc, " ?\\d*(ER|ÈME|E)? ARRONDISSEMENT.*")))


# Writing the csv output file
write_csv(commentaires_plm, "../comments_PLM.csv")
