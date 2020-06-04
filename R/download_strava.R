library(yaml)
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)


# Obtain strava API credentials by creating a Strava API app
credentials <- yaml.load_file('R/strava_creds.yaml')

# Create a Strava API Token
app <- oauth_app("strava", credentials$client_id, credentials$secret)
endpoint <- oauth_endpoint(
  request = NULL,
  authorize = "https://www.strava.com/oauth/authorize",
  access = "https://www.strava.com/oauth/token"
)

token <- oauth2.0_token(endpoint
                        , app
                        , as_header = FALSE
                        , scope = "activity:read_all")



STRAVA_API_DEFAULT_PAGE_SIZE <- 200


# Function to call the strava v3/athlete/activities API
# This API is paginated, so you need to pass a page_number and page_size to retrieve > 1 
# page of results (default 200 results per page)
strava_athlete_activities_api <- function(oauth_token, page_number, page_size = STRAVA_API_DEFAULT_PAGE_SIZE) {
  print(paste0("Querying Strava V1 API v3/athlete/activities [page ", page_number, "]"))
  resp <- GET(
    url = "https://www.strava.com/api/v3/athlete/activities",
    config = oauth_token,
    query = list(per_page = page_size, page = page_number))
  
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
    break
  }

  # Strava API returns data in JSON format, so parse it and return as a data.frame
  jsonlite::fromJSON(content(resp, "text"), flatten = FALSE)
}


# A list to store the returned data into
data_pages <- list()

# Start with page #1
page_number <- 1

repeat {
  
# Query API
  data_pages[[page_number]] <- strava_athlete_activities_api(token, page_number)
  
  # Count number of records returned
  records_returrned <- data_pages[[page_number]] %>% nrow()
  print(paste0('Retrieved ', records_returrned, ' records'))
  
  # Continue to the next page if the number of returned records matches the page size
  if (records_returrned < STRAVA_API_DEFAULT_PAGE_SIZE) {
    print("All records returned, exiting")
    break
  } else {
    print("checking for more records ..")
    page_number <- page_number + 1
  }
}

# Combine the list of data frames into a single dataframe of all collected pages
data <- rbind_pages(data_pages)
print(paste0('returned a total of ', data %>% nrow(), ' records'))


# Every day in the challenge
base_data = tibble(start_date = seq(as.Date("2020-05-25"),as.Date("2020-09-06"),"days"), distance = NA)

plot_data <- data %>% 
  filter(start_date > '2020-05-25', type == "Run") %>%
  mutate(start_date = as.Date(start_date)) %>%
  select(start_date, distance) %>%
  rbind(base_data) %>%
  group_by(start_date) %>%
  summarise(distance = sum(distance, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(weekday = wday(start_date, label = T, week_start = 7), # can put week_start = 1 to start week on Monday
         month = month(start_date, label = T),
         date = yday(start_date),
         week = epiweek(start_date)) %>%
  mutate(distance = round(distance / 1000, 1))

plot_data %>%
write_csv('../walmcconnell.com_site/content/post/subway_challenge_2020/data/wal_strava.csv')

  