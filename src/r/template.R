setwd(dirname(rstudioapi::getSourceEditorContext()$path) %>% dirname() %>% dirname())

library(dplyr)

# Load cached data ####
load("data/cached_data.RData")

# Read HTML template ####
html_content <- readLines("index.html") %>% paste(collapse = "\n")

# Replace placeholders with variables ####
html_content <- gsub("{{latest_year}}", latest_year, html_content, fixed = TRUE)
html_content <- gsub("{{earliest_year}}", earliest_year, html_content, fixed = TRUE)
html_content <- gsub("{{latest_pop_total}}", latest_pop_total, html_content, fixed = TRUE)
html_content <- gsub("{{latest_pop_round_million}}", latest_pop_round_million, html_content, fixed = TRUE)

# Add more placeholders here as needed
# html_content <- gsub("{{variable_name}}", variable_name, html_content, fixed = TRUE)

# Write the final HTML ####
if (!dir.exists("public")) dir.create("public")
writeLines(html_content, "public/index.html")

message("✓ HTML template generated successfully!")
