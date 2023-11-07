# This data is an extract from ESR dashboard
# Three parts will be pulled for the report
# Gender pay gap (%) based on male hourly pay
# Quantiles by gender
# Join with staff list to get AFC band information

# Load required libraries
library(readxl)
library(dplyr)
library(purrr)
library(stringr)

# List all excel and csv files in the directory
files <- list.files(path = "./data_temp", pattern = "\\.xlsx$|\\.csv$", full.names = TRUE)

# Function to process each file
process_file <- function(filepath) {
  # Determine the financial year from the filename
  fy_pattern <- "FY(\\d{2})(\\d{2})"
  fy_matches <- regmatches(filepath, regexec(fy_pattern, filepath))[[1]]
  # reporting period
  financial_year <- paste0("20", fy_matches[2], "/", fy_matches[3])

  # Create three data frames and add the financial year (reporting period)
  if (stringr::str_detect(filepath, "\\.xlsx$")) {
    list(
      afc = read_excel(filepath, skip = 8, col_names = TRUE) |>
        select(2:7) |>
        janitor::clean_names() |>
        mutate(period = financial_year)
    )
    # staff list information as csv
  } else if (stringr::str_detect(filepath, "\\.csv$")) {
    list(
      staff = read.csv(filepath, header = TRUE) |>
        janitor::clean_names() |>
        filter(primary == "Y") |>
        select(
          employee_number,
          org_l3,
          pay_scale
        ) |>
        mutate(
          period = financial_year,
          employee_number = as.character(employee_number)
        )
    )
  }
}

# Apply the function to each file
dfs <- map(files, process_file)

afc <- map(dfs, "afc") |>
  bind_rows() |>
  select(period, everything())

staff <- map(dfs, "staff") |>
  bind_rows() |>
  select(period, everything())

# AFC and staff information join based on employee number
# After that, add lookup
lookup <- read.csv("./data-raw/afc_band_lookup.csv", header = TRUE)

afc_staff <- afc |>
  left_join(staff,
    by = c("period", "employee_number")
  ) |>
  left_join(lookup,
    by = "pay_scale"
  ) |>
  select(-employee_number) |>
  # Data quality error July 2013 Archive employee org
  # is wrong, manually edited
  mutate(
    org_l3 = ifelse(org_l3 == "July 2013 Archive",
      "914 BSA Finance, Commercial and Estates L3", org_l3
    ),
    directorate = stringr::str_replace_all(org_l3, c("^914 BSA " = "", " L3" = "")),
    directorate = stringr::str_trim(directorate),
    headcount = 1,
    gender = ifelse(gender == "Female", "Women", "Men")
  ) |>
  select(period, gender, headcount, hourly_rate, quartile, afc_band, directorate)

# create gpg_class
gpg_class <- gpg_data(afc_staff)

# Keep three main data frame and it will be used to create S3 class
usethis::use_data(gpg_class, overwrite = TRUE)

# delete all the files in data_temp as they only stay in azure storage

# Specify the folder path
folder_path <- "./data_temp"

# List all files in the directory
files_to_delete <- list.files(path = folder_path, full.names = TRUE)

# Remove all files
result <- file.remove(files_to_delete)

# Check if all files were deleted successfully
if (all(result)) {
  cat("All files deleted successfully.\n")
} else {
  cat("Some files could not be deleted.\n")
}

rm(dfs, afc, staff)
