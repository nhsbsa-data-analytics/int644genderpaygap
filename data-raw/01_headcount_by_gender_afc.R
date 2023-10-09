# This is dummy tidy dataset but it will include 
# mean and median pay per AFC band to create one file

# This dummy data includes maternity leave, sick leave etc
# Therefore, headcounts are slightly higher then reported
# figure

# Library

library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from cleaned employee table in DALP
data_db <- con |> 
  tbl(from = in_schema("DALL_REF", "EMPLOYEE_DASHBOARD_COMBINED_EMPLOYMENT_DATA"))

# Summary headcount table of Financial Year, Gender, AFC band

headcount <- data_db |> 
  filter(substr(ESR_MONTH, 1, 6) == '01-MAR', 
         as.numeric(substr(ESR_MONTH, 8, 9)) %in% c(18, 19, 20, 21, 22, 23)) |> 
  mutate(
    FINANCIAL_YEAR = case_when(
      as.numeric(substr(ESR_MONTH, 8, 9)) == 18 ~ '2017/18',
      as.numeric(substr(ESR_MONTH, 8, 9)) == 19 ~ '2018/19',
      as.numeric(substr(ESR_MONTH, 8, 9)) == 20 ~ '2019/20',
      as.numeric(substr(ESR_MONTH, 8, 9)) == 21 ~ '2020/21',
      as.numeric(substr(ESR_MONTH, 8, 9)) == 22 ~ '2021/22',
      as.numeric(substr(ESR_MONTH, 8, 9)) == 23 ~ '2022/23',
      TRUE ~ 'unknown'
    )
  ) |> 
  group_by(FINANCIAL_YEAR, GENDER, PAY_GRADE_NAME , FTE_GROUP) |> 
  summarise(HEADCOUNT = sum(HEADCOUNT, na.rm = TRUE)) |> 
  ungroup() |> 
  arrange(FINANCIAL_YEAR, GENDER, PAY_GRADE_NAME, FTE_GROUP) |> 
  collect() |> 
  # In case we want to report by year (keep it as factor) 
  mutate(FINANCIAL_YEAR = factor(FINANCIAL_YEAR, 
                                 levels = unique(FINANCIAL_YEAR)),
         PAY_GRADE_NAME = factor(PAY_GRADE_NAME))


# Add to data 
usethis::use_data(headcount, overwrite = TRUE)

DBI::dbDisconnect(con) 
rm(list = ls())
gc()

