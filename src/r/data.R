library(dplyr)
library(purrr)
library(jsonlite)

# List datasets ####
tables <- c(
  "MA01T01",
  "MYE01T015"
)

# API Key ####
api_key <- "801aaca4bcf0030599c019f4efa8b89032e5e6aa1de4a629a7f7e9a86db7fb8c"

# Fetch metadata function ####

fetch_metadata <- function(matrix,
                           api_key,
                           max_attempts = Inf,
                           wait_seconds = 2) {
  attempt <- 1
  repeat {
    result <- tryCatch(
      {
        url <- paste0(
          "https://ws-data.nisra.gov.uk/public/api.restful/",
          "PxStat.Data.Cube_API.ReadDataset/",
          matrix,
          "/JSON-stat/2.0/en?apiKey=",
          api_key
        )

        json_data <- fromJSON(txt = url)

        # Check if API itself returned "error" field
        if ("error" %in% names(json_data)) {
          stop("API returned error field")
        }

        return(json_data)  # ✅ success, return immediately
      },
      error = function(e) {
        message(sprintf("Error fetching metadata for %s (attempt %d): %s",
                        matrix,
                        attempt, e$message))
        return(NULL)
      }
    )

    if (!is.null(result)) {
      return(result)  # break loop if successful
    }

    attempt <- attempt + 1
    if (attempt > max_attempts) {
      stop("Max attempts reached without success.")
    }

    Sys.sleep(wait_seconds)  # backoff before retry
  }
}

# Read CSV data function ####

read_csv_data <- function(matrix,
                          max_attempts = Inf,
                          wait_seconds = 2) {
  attempt <- 1
  repeat {
    result <- tryCatch(
      {
        url <- paste0(
          "https://ws-data.nisra.gov.uk/public/api.restful/",
          "PxStat.Data.Cube_API.ReadDataset/",
          matrix,
          "/CSV/1.0/"
        )

        df <- read.csv(url)
        return(df)  # ✅ success, return immediately
      },
      error = function(e) {
        message(sprintf("Error reading CSV for %s (attempt %d): %s",
                        matrix,
                        attempt, e$message))
        return(NULL)
      }
    )

    if (!is.null(result)) {
      return(result)  # break loop if successful
    }

    attempt <- attempt + 1
    if (attempt > max_attempts) {
      stop("Max attempts reached without success.")
    }

    Sys.sleep(wait_seconds)  # backoff before retry
  }
}

# Fetch data ####
metadata <- list()

for (matrix in tables) {

  # Fetch metadata from API
  metadata_json <- fetch_metadata(matrix, api_key)

  # Read CSV data
  df <- read_csv_data(matrix)

  # Assign dataframe to global environment with table name
  assign(paste0(matrix, "_df"), df)

  # Store metadata
  metadata[[matrix]] <- list(
    label = metadata_json$label,
    updated = as.Date(metadata_json$updated)
  )
}

# filter MYE01T015 for Northern Ireland only
df_ni_mye01t015_all <- MYE01T015_df %>% filter(AA2024 == "N92000002") %>% filter(Sex == "All")

# latest and earliest years
latest_year <- max(as.numeric(df_ni_mye01t015_all$Year))
earliest_year <- min(as.numeric(df_ni_mye01t015_all$Year))

# population figures
latest_pop_total <- df_ni_mye01t015_all %>% filter(Year == latest_year & broadage4 == "All") %>% pull(VALUE)
latest_pop_round_million <- round(latest_pop_total/1000000, digits = 1)
 
if (!dir.exists("public")) dir.create("public")
if (!dir.exists("public/data")) dir.create("public/data")

# Create a list of variables to export
output_data <- list(
  latest_year = latest_year,
  earliest_year = earliest_year,
  latest_pop_round_million = latest_pop_round_million,
  latest_pop_total = latest_pop_total
)

# Write as JSON
jsonlite::write_json(output_data, "public/data/data.json", pretty = TRUE)

message("✓ Data fetched and cached successfully!")
