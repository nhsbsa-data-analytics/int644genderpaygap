# Adhoc analysis

# Load necessary libraries
library(ggplot2)
library(dplyr)
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
  # Snapshot as of
  reporting_year <- paste0("31 March 20", fy_matches[3])

  # Create three data frames and add the financial year (reporting period)
  if (stringr::str_detect(filepath, "\\.xlsx$")) {
    list(
      afc = read_excel(filepath, skip = 8, col_names = TRUE) |>
        select(2:7) |>
        janitor::clean_names() |>
        mutate(period = reporting_year)
    )
    # staff list information as csv
  } else if (stringr::str_detect(filepath, "\\.csv$")) {
    list(
      staff = read.csv(filepath, header = TRUE) |>
        janitor::clean_names() |>
        filter(primary == "Y") |> #required, few employee with two entry, keep primary only
        select(
          employee_number,
          org_l3,
          org_l5,
          pay_scale
        ) |>
        mutate(
          period = reporting_year,
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


############## GPG mean gap analysis

male_2023 <- afc |> filter(period == "31 March 2023", gender == "Male") |> pull(hourly_rate)
female_2023 <- afc |> filter(period == "31 March 2023", gender == "Female") |> pull(hourly_rate)

male_2024 <- afc |> filter(period == "31 March 2024", gender == "Male")|> pull(hourly_rate)
female_2024 <- afc |> filter(period == "31 March 2024", gender == "Female")|> pull(hourly_rate)

# Function to calculate Mean GPG
calculate_mean_gpg <- function(male_mean, female_mean) {
  return(((male_mean - female_mean) / male_mean) * 100)
}

# Calculate mean GPG for 2023 and 2024
mean_gpg_2023 <- calculate_mean_gpg(mean(male_2023), mean(female_2023))
mean_gpg_2024 <- calculate_mean_gpg(mean(male_2024), mean(female_2024))

cat("Mean GPG 2023 (%):", mean_gpg_2023, "\n")
cat("Mean GPG 2024 (%):", mean_gpg_2024, "\n")

# Permutation test for Mean GPG
set.seed(42)
observed_diff <- mean_gpg_2024 - mean_gpg_2023

# Combine hourly rates
combined_gpg <- c(male_2023, female_2023, male_2024, female_2024)
n_2023 <- length(c(male_2023, female_2023))

# Permute
n_permutations <- 10000
perm_diffs <- replicate(n_permutations, {
  permuted <- sample(combined_gpg)
  mean(permuted[1:n_2023]) - mean(permuted[(n_2023 + 1):length(combined_gpg)])
})

# Calculate p-value
p_value <- mean(abs(perm_diffs) >= abs(observed_diff))

cat("Observed Mean GPG Change (%):", observed_diff, "\n")
cat("Permutation Test p-value for Mean GPG:", p_value, "\n") # 0.6817

### Result: The change in mean GPG is not statistically significant,
### meaning the observed shift is negligible and consistent with random variability.

####################### GPG median gap analysis

bootstrap_median_gpg_diff <- function(group1_2023, group2_2023, 
                                          group1_2024,
                                          group2_2024, n = 10000) {
  set.seed(42)
  diffs <- replicate(n, {
    male_median_2023 <- median(sample(group1_2023, length(group1_2023), replace = TRUE))
    female_median_2023 <- median(sample(group2_2023, length(group2_2023), replace = TRUE))
    male_median_2024 <- median(sample(group1_2024, length(group1_2024), replace = TRUE))
    female_median_2024 <- median(sample(group2_2024, length(group2_2024), replace = TRUE))
    
    gpg_2023 <- ((male_median_2023 - female_median_2023) / male_median_2023) * 100
    gpg_2024 <- ((male_median_2024 - female_median_2024) / male_median_2024) * 100
    
    return(gpg_2024 - gpg_2023)
  })
  return(quantile(diffs, c(0.025, 0.975)))
}

# Calculate Median GPG
median_gpg_2023 <- ((median(male_2023) - median(female_2023)) / median(male_2023)) * 100
median_gpg_2024 <- ((median(male_2024) - median(female_2024)) / median(male_2024)) * 100

cat("Median GPG 2023 (%):", median_gpg_2023, "\n")
cat("Median GPG 2024 (%):", median_gpg_2024, "\n")

# Bootstrap CI for Median GPG difference
median_gpg_ci <- bootstrap_median_gpg_diff(male_2023, female_2023, male_2024, female_2024)

cat("Bootstrap CI for Change in Median GPG (%):", median_gpg_ci, "\n")
