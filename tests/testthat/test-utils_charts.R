library(tidyr)
library(dplyr)
library(testthat)

x <- data.frame(
  period = c("2018/19", "2019/20", "2020/21", "2021/22", "2022/23"),
  Women = c(1700, 1800, 1900, 2000, 2300),
  Men = c(1100, 1300, 1300, 1400, 1500)
)

y <- data.frame(
  period = c("2018/19", "2019/20", "2020/21", "2021/22", "2022/23"),
  mean_hr_gpg = c(11, 11, 12, 14, 12),
  median_hr_gpg = c(0.80, 0.5, 2.3, 12.5, 8.88)
)

z <- data.frame(
  period = rep("2021/22", 20),
  gender = c(rep("Female", 10), rep("Male", 10)),
  afc_band = c(
    "Band 2", "Band 3", "Band 4", "Band 5", "Band 6",
    "Band 2", "Band 3", "Band 4", "Band 5", "Band 6"
  ),
  headcount = c(-460, -645, -280, -218, -118, 156, 80, 41, 13, 7),
  perc = c(65.8, 66.5, 62.9, 57.8, 45.2, 47.0, 48.8, 48.2, 38.2, 31.8)
)

quartile_test <- data.frame(
  period = c(rep("2018/19", 8)),
  quartile = c(rep(1, 2), rep(2, 2), rep(3, 2), rep(4, 2)),
  gender = c("women", "men", "women", "men", "women", "men", "women", "men"),
  count = c(425, 282, 438, 261, 461, 269, 380, 344),
  percent = c(60.1, 39.9, 62.7, 37.3, 63.2, 36.8, 52.5, 47.5)
)


paygap_afc <- data.frame(
  period = rep("2021/22", 12),
  afc_band = c(
    "Band 2", "Band 3", "Band 4", "Band 5", "Band 6", "Band 7",
    "Band 8A", "Band 8B", "Band 8C", "Band 8D", "Band 9", "Non-AfC"
  ),
  mean_paygap = c(
    0.945, 0.0887, -0.0223, -2.35, 1.34, -0.720, 0.565,
    4.73, 4.91, -12.6, 16.6, 38.2
  ),
  median_paygap = c(0, 0, 0, -5.61, -2.64, -0.284, 0, 0, 4.21, -7.04, 28.6, 36.0)
)


paygap_dir <- data.frame(
  period = c(
    "2021/22", "2021/22", "2021/22", "2021/22",
    "2021/22", "2021/22", "2021/22", "2021/22",
    "2021/22", "2021/22"
  ),
  directorate = c(
    "Chief Executive Officer",
    "Digital, Data & Technology",
    "Finance, Commercial and Estates",
    "Operations",
    "People & Corporate Services",
    "Portfolio Management", "Primary Care Services", "Projects",
    "Strategy, Perf, Bus Dev & Growth", "Workforce Transformation"
  ),
  mean_paygap = c(80, 1.5, 19.3, 7.8, 7.0, 9, 11, NA, -35, 9.6),
  median_paygap = c(83.1, 12.4, 20.3, 0, -14.2, 4.7, -0.0, NA, -11, -0.00),
  mean_label = c(80, 1.5, 19.3, 7.8, 7.0, 9, 11, NA, 35, 9.6),
  median_label = c(83.1, 12.4, 20.3, 0, 14.2, 4.7, 0.0, NA, 11, 0.00)
)

paygap_all <- data.frame(
  period = c("2018/19", "2019/20", "2020/21", "2021/22", "2022/23"),
  mean_paygap = c(11.5, 11.3, 12.6, 14.0, 12.6),
  median_paygap = c(0.800, 0.581, 2.34, 12.5, 8.88)
)

testthat::test_that("gpg_trend function runs without errors", {
  expect_silent(gpg_trend(x,
    xvar = "period",
    yvars = c("Men", "Women"),
    series_names = c("Men", "Women"),
    yaxis_title = "Men and Women employee headcount",
    yaxis_label = "number",
    colpalette = "gender",
    export_filename = "gpg_trend"
  ))
})


testthat::test_that("gpg_trend outputs a highchart, htmlwidget class", {
  expect_equal(class(
    gpg_trend(x,
      xvar = "period",
      yvars = c("Men", "Women"),
      series_names = c("Men", "Women"),
      yaxis_title = "Men and Women employee headcount",
      yaxis_label = "number",
      colpalette = c("DarkBlue", "Green"),
      export_filename = "gpg_trend"
    )
  ), c("highchart", "htmlwidget"))
})

testthat::test_that("gpg_trend takes list as an input", {
  expect_equal(class(list(x)), "list")
})

testthat::test_that("gpg_trend input data frame must contain Women,
                    Men column", {
                      expect_equal(length(grep("Women|Men", names(x))), 2)
                    })


testthat::test_that("gpg_trend input data frame must contain period column", {
  expect_equal(length(grep("period", names(x))), 1)
})


testthat::test_that("gpg_trend function runs with paygap dataframe", {
  expect_silent(gpg_trend(y,
    xvar = "period",
    yvars = c("mean_hr_gpg", "median_hr_gpg"),
    series_names = c("Mean gender pay gap", "Median gender pay gap"),
    yaxis_title = "Gender pay gap in hourly pay",
    yaxis_label = "percentage",
    colpalette = c("Purple", "WarmYellow"),
    export_filename = "gpg_trend"
  ))
})


testthat::test_that("gpg_pyramid function runs without error", {
  expect_silent(gpg_pyramid(z,
    xvar = "afc_band", yvar = "headcount",
    yaxis_title = "Headcount",
    export_filename = "gpg_pyramid"
  ))
})

testthat::test_that("gpg_stack function runs without error", {
  expect_silent(gpg_stack(quartile_test,
    xvar = "quartile", yvar = "percent",
    groupvar = "gender", yaxis_title = "Men and women in pay quartile",
    export_filename = "gpg_stack"
  ))
})


testthat::test_that("gpg_stack function runs with error", {
  expect_error(gpg_stack(quartile_test,
    xvar = "quartile", yvar = "percent",
    groupvar = NA, yaxis_title = "Men and women in pay quartile",
    export_filename = "gpg_stack"
  ))
})


testthat::test_that("gpg_bar function runs without error with AfC paygap", {
  expect_silent(gpg_bar(paygap_afc,
    xvar = "afc_band", yvar = "mean_paygap",
    yaxis_title = "Mean pay gap",
    export_filename = "gpg_bar"
  ))
})

testthat::test_that("gpg_bar function runs without error with directorate", {
  expect_silent(gpg_bar(paygap_dir,
    xvar = "directorate", yvar = "mean_paygap",
    yaxis_title = "Mean pay gap",
    export_filename = "gpg_bar"
  ))
})


testthat::test_that("gpg_column function runs without error", {
  expect_silent(gpg_column(paygap_all,
    yvar = "mean_paygap",
    yaxis_title = "Mean pay gap",
    export_filename = "gpg_column"
  ))
})
