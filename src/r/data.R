library(dplyr)
library(purrr)
library(jsonlite)

# List datasets ####
tables <- c(
  "MA01T01",
  "MYE01T04",
  "PPMY02T02",
  "MYE01T08"
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

# filter MYE01T04 for Northern Ireland only
df_mye01t04_ni <- MYE01T04_df %>% subset(LGD2014 == "N92000002")

# filter for all sexes
df_ni_mye01t04_all <- df_mye01t04_ni %>% filter(Sex == "All")

# latest and earliest years
latest_year <- max(as.numeric(df_ni_mye01t04_all$Year))
earliest_year <- min(as.numeric(df_ni_mye01t04_all$Year))

# latest population figures
latest_pop_total <- df_ni_mye01t04_all %>% filter(Year == latest_year & broadage4 == "All") %>% pull(VALUE)
latest_pop_round_million <- round(latest_pop_total/1000000, digits = 1)
latest_age_band_totals <- df_ni_mye01t04_all %>% filter(Year == latest_year & broadage4 != "All") %>% select(Broad.age.band..4.cat., VALUE) %>% rename("age_band_labels" = Broad.age.band..4.cat., "value" = VALUE)

pct_under_16 <- df_ni_mye01t04_all %>% filter(Year == latest_year & broadage4 == 1) %>% pull(VALUE) / latest_pop_total*100
pct_under16_round <- round(pct_under_16, digits = 0)
tot_16_39 <- df_ni_mye01t04_all %>% filter(Year == latest_year & broadage4 == 2) %>% pull(VALUE)
tot_40_64 <- df_ni_mye01t04_all %>% filter(Year == latest_year & broadage4 == 3) %>% pull(VALUE)
pct_16_64 <- (tot_16_39 + tot_40_64) / latest_pop_total * 100
pct_16_64_round <- round(pct_16_64, digits = 0)
pct_65_plus <- df_ni_mye01t04_all %>% filter(Year == latest_year & broadage4 == 4) %>% pull(VALUE) / latest_pop_total * 100
pct_65_plus_round <- round(pct_65_plus, digits = 0)

pct_female <-df_mye01t04_ni %>% subset(Year == latest_year) %>% filter(broadage4 == "All" & Sex == 1) %>% pull(VALUE) / latest_pop_total *100
pct_female_round <- round(pct_female, digits = 0)

# earliest year population figures
earliest_pop_total <- df_ni_mye01t04_all %>% filter(Year == earliest_year & broadage4 == "All") %>% pull(VALUE)
earliest_pop_round_million <- round(earliest_pop_total/1000000, digits = 1)

# pop change
pop_change <- latest_pop_total - earliest_pop_total
pop_change_round <- round(pop_change, digits = -3)

# median age
median_1994 <- MA01T01_df %>% filter(Year == 1994 & Sex == "All")%>% pull(VALUE)
median_latest <- MA01T01_df %>% filter(Year == latest_year & Sex == "All") %>% pull(VALUE)
median_latest_round <- round(median_latest, digits = 0)

# historic mid-year projections
df_mid_year_ests <- MYE01T08_df %>% subset(Age == "All" & Sex == "All") %>% select(Year, VALUE) %>% rename("year" = Year, "population" = VALUE)

# projections
df_projections <- PPMY02T02_df %>% subset(Sex == "All") %>% rename(year = Year)
df_projections_yr_tots <- df_projections %>% group_by(year) %>% summarise(total = sum(VALUE))
pop_peak <- max(as.numeric(df_projections_yr_tots$total))
pop_peak_mill <- round(pop_peak/1000000, digits = 2)
pop_peak_year <- df_projections_yr_tots %>% filter(total == pop_peak) %>% pull(year)

# create dataframe for chart with estimates and projections
df_pop_projections_chart <- merge(df_mid_year_ests, df_projections_yr_tots, by= "year", all = TRUE) %>% rename(estimate = "population", projection = "total")

if (!dir.exists("public")) dir.create("public")
if (!dir.exists("public/data")) dir.create("public/data")

# Create a list of variables to export
output_data <- list(
  summary = list(
    latest_year = latest_year,
    earliest_year = earliest_year,
    latest_pop_round_million = latest_pop_round_million,
    latest_pop_total = latest_pop_total,
    pct_under_16 = pct_under16_round,
    pct_16_64 = pct_16_64_round,
    pct_65_plus = pct_65_plus_round,
    pct_female = pct_female_round,
    earliest_pop_total = earliest_pop_total,
    earliest_pop_round_million = earliest_pop_round_million,
    pop_change_round = pop_change_round,
    median_1994 = median_1994,
    median_latest = median_latest_round,
    pop_peak_mill = pop_peak_mill,
    pop_peak_year = pop_peak_year),
  charts = list(
    popAgeChart = list(
      type = 'bar',
      title = paste0("Population by age group ", latest_year),
      labels = latest_age_band_totals$age_band_labels,
      datasets = list(
        list(
          label = "Population",
          data = latest_age_band_totals$value,
          backgroundColor = c("#00205b", "#00205b", "#00205b", "#00205b")
        )
      )
      ),
      projections_chart = list(
        type = 'line',
        title = "Population projections",
        labels = df_pop_projections_chart$year,
        datasets = list(
          list(
            label = "Population estimate",
            data = unname(df_pop_projections_chart$estimate),
            borderColor = "#00205b",
            fill = FALSE,
            spanGaps = FALSE
          ),
          list(
            label = "Population projection",
            data = unname(df_pop_projections_chart$projection),
            borderColor = "#68a41e",
            fill = FALSE,
            spanGaps = TRUE
        )
       )
     )
  )
)

 

# If any of the values are single-length vectors, simplify them to the single value for easier use in the HTML.
output_data$summary <- lapply(output_data$summary, function(x) {
  if (length(x) == 1) x[[1]] else x
})

# Write as JSON
jsonlite::write_json(output_data, "public/data/data.json", pretty = TRUE, auto_unbox = TRUE )

message("✓ Data fetched and cached successfully!")
