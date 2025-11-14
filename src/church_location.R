#########################################################################

# This file is used to locate all chruches with in Bavaria

##########################################################################

# Load libraries
library(httr)
library(jsonlite)
library(dplyr)
library(readxl)

# Load List of Municipalities with Coordinates
setwd('/set/your/directory')
municipalities <- read.csv("bavaria_municipalities_coordinates.csv")
# Your Google API Key
api_key <- "Your API key"
# Initialize Empty Data Frame
all_places <- data.frame()

# Loop through each municipality
for (i in 1:nrow(municipalities)) {
  lat <- municipalities$latitude[i]
  lng <- municipalities$longitude[i]
  mun_name <- municipalities$Municipality[i]  # Municipality name
  
  cat("\U0001F4CD Searching churches near", mun_name, "...\n")
  
  # Base URL for Nearby Search API
  base_url <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json"
  
  # First API request
  url <- paste0(base_url, "?location=", lat, ",", lng, "&radius=10000&keyword=church&key=", api_key)
  next_page_token <- NULL
  page_count <- 0
  
  repeat {
    # Call API
    response <- GET(url)
    data_json <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(data_json)
    
    if (!"results" %in% names(data)) {
      print("⚠ No results or API error.")
      break
    }
    
    # Extract Place Data
    places <- data$results %>%
      filter(!is.null(geometry)) %>%  # Remove entries without geometry
      transmute(
        Municipality = mun_name,
        Name = ifelse("name" %in% names(.), name, NA),
        Address = ifelse("vicinity" %in% names(.), vicinity, NA),
        Latitude = ifelse(!is.null(geometry$location$lat), geometry$location$lat, NA),
        Longitude = ifelse(!is.null(geometry$location$lng), geometry$location$lng, NA)
      )
    
    # Append to Main Data Frame
    all_places <- bind_rows(all_places, places)
    
    # Check if there is another page of results
    if (!"next_page_token" %in% names(data) || page_count >= 2) {
      break  # Google allows a max of 3 pages per request
    }
    
    next_page_token <- data$next_page_token
    page_count <- page_count + 1
    
    if (!is.null(next_page_token)) {
      Sys.sleep(3)  # Allow time for token activation
      url <- paste0(base_url, "?pagetoken=", next_page_token, "&key=", api_key)
    } else {
      break
    }
  }
}

# Remove Duplicates
all_places <- distinct(all_places, Name, Latitude, Longitude, .keep_all = TRUE)

# Save Data to CSV
write.csv(all_places, "churches_bavaria_full_with_cleaned_municipalities3.csv", row.names = FALSE)

print("✅ Data saved as churches_bavaria_full.csv")