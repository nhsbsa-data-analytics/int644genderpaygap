###################### Adhoc Analysis###################
# Run 2.gpg_data_process.R to create afc_staff dataframe

df <- afc_staff |> 
  filter(period == '31 March 2024') |> 
  filter(afc_band == 'Non-AfC') 


male_hourly_rate <- df |> filter(gender == "Men") |> select(hourly_rate) |> pull()
female_hourly_rate <- df |> filter(gender == "Women") |> select(hourly_rate) |> pull()

t_test_result <- t.test(male_hourly_rate, female_hourly_rate, var.equal = FALSE)

# Mann-Whitney U Test (Wilcoxon Rank-Sum test in R)
mann_whitney_result <- wilcox.test(male_hourly_rate, female_hourly_rate, exact = FALSE)

# Confidence Intervals for each group
male_ci <- t.test(male_hourly_rate)$conf.int
female_ci <- t.test(female_hourly_rate)$conf.int

# Print results
cat("Welch's t-test results:\n")
print(t_test_result)

cat("\nMann-Whitney U Test (Wilcoxon Rank-Sum) results:\n")
print(mann_whitney_result)

cat("\nConfidence Intervals:\n")
cat("Male 95% CI:", male_ci, "\n")
cat("Female 95% CI:", female_ci, "\n")


################## Check with Median

library(RVAideMemoire)  # If not installed, run install.packages("RVAideMemoire")
hourly_data <- data.frame(
  Hourly_Rate = c(male_hourly_rate, female_hourly_rate),
  Gender = rep(c("Male", "Female"), c(length(male_hourly_rate), length(female_hourly_rate)))
)

mood.medtest(Hourly_Rate ~ Gender, data = hourly_data)
