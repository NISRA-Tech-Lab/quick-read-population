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

# Fetch dataset function ####

fetch_dataset <- function(matrix,
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
        message(sprintf("Error fetching %s (attempt %d): %s",
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

  raw_data <- fetch_dataset(matrix, api_key)

  dimensions <- names(raw_data$dimension)
  size <- raw_data$size

  df <- data.frame(value = as.numeric(raw_data$value))

  for (i in seq_along(dimensions)) {
    dimension_cats <- unlist(raw_data$dimension[[dimensions[[i]]]]$category$label)
    dimension_col <- c()

    for (cat in dimension_cats) {
      if (i == length(dimensions)) {
        dimension_col <- c(dimension_col, cat)
      } else {
        dimension_col <- c(dimension_col, rep(cat, prod(size[(i + 1):length(size)])))
      }
    }

    df[[dimensions[i]]] <- dimension_col
  }

  df <- df %>%
    relocate(value, .after = dimensions[[length(dimensions)]])

  # Assign dataframe to global environment with table name
  assign(paste0(matrix, "_df"), df)

  # Store metadata
  metadata[[matrix]] <- list(
    label = raw_data$label,
    updated = as.Date(raw_data$updated)
  )
}

# Rename TLIST(A1) to year in all dataframes ####
for (matrix in tables) {
  df_name <- paste0(matrix, "_df")
  df <- get(df_name)
  if ("TLIST(A1)" %in% names(df)) {
    df <- df %>% rename(year = `TLIST(A1)`)
    assign(df_name, df)
  }
}

# filter NYE01T015 for Northern Ireland only
df_ni_mye01t015_all <- MYE01T015_df %>% filter(AA2024 == "Northern Ireland") %>% filter(Sex == "All persons")

# latest and earliest years
latest_year <- max(as.numeric(df_ni_mye01t015$year))
earliest_year <- min(as.numeric(df_ni_mye01t015$year))

# population figures
latest_pop_total <- df_ni_mye01t015_all %>% filter(year == latest_year & broadage4 == "All") %>% pull(value)
latest_pop_round_million <- round(latest_pop_total/1000000, digits = 1)
 
if (!dir.exists("public")) dir.create("public")
if (!dir.exists("public/data")) dir.create("public/data")

# Export dataframes to CSV for reference
for (matrix in tables) {
  df_name <- paste0(matrix, "_df")
  df <- get(df_name)
  write.csv(df, file.path("public/data", paste0(matrix, ".csv")), row.names = FALSE)
}

# Create a list of variables to export
output_data <- list(
  latest_year = latest_year,
  latest_pop_round_million = latest_pop_round_million,
  latest_pop_total = latest_pop_total
)

# Write as JSON
jsonlite::write_json(output_data, "public/data/data.json", pretty = TRUE)

message("✓ Data fetched and cached successfully!")
