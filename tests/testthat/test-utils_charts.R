library(tidyr)
library(dplyr)

x <- data.frame(
  period = c("2018/19", "2019/20", "2020/21", "2021/22", "2022/23"),
  Female = c(1700, 1800, 1900, 2000, 2300),
  Male = c(1100, 1300, 1300, 1400, 1500)
)

y <- data.frame(
  period = c("2018/19", "2019/20", "2020/21", "2021/22", "2022/23"),
  mean_hr_gpg = c(11, 11, 12, 14, 12),
  median_hr_gpg = c(0.80, 0.5, 2.3, 12.5, 8.88)
)

z <- data.frame(
  period = rep("2021/22", 20),
  gender = c(rep("Female", 10), rep("Male", 10)),
  afc_band = c("Band 2", "Band 3", "Band 4", "Band 5", "Band 6",
               "Band 2", "Band 3", "Band 4", "Band 5", "Band 6") ,
  headcount = c(-460, -645, -280, -218, -118, 156, 80, 41, 13, 7),
  perc = c(65.8, 66.5, 62.9, 57.8, 45.2, 47.0, 48.8, 48.2, 38.2, 31.8)
)


testthat::test_that("gpg_trend function runs without errors", {
  expect_silent(gpg_trend(x,
    xvar = "period",
    yvars = c("Male", "Female"),
    series_names = c("Male", "Female"),
    yaxis_title = "Male and Female employee headcount",
    yaxis_label = "number",
    colpalette = "gender"
  ))
})


testthat::test_that("gpg_trend outputs a highchart, htmlwidget class", {
  expect_equal(class(
    gpg_trend(x,
      xvar = "period",
      yvars = c("Male", "Female"),
      series_names = c("Male", "Female"),
      yaxis_title = "Male and Female employee headcount",
      yaxis_label = "number",
      colpalette = c("DarkBlue", "Green")
    )
  ), c("highchart", "htmlwidget"))
})

testthat::test_that("gpg_trend takes list as an input", {
  expect_equal(class(list(x)), "list")
})


testthat::test_that("gpg_trend input data frame must contain Female,
                    Male column", {
                      expect_equal(length(grep("Female|Male", names(x))), 2)
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
    colpalette = c("Purple", "WarmYellow")
  ))
})


testthat::test_that("gpg_pyramid function runs without error", {
  expect_silent(gpg_pyramid(z, xvar = "afc_band", yvar = "headcount",
    yaxis_title = "Headcount"
  ))
})

testthat::test_that("gpg_stack function runs without error", {
  expect_silent(gpg_stack(quartile |> filter(period == "2021/22"),
    xvar = "quartile", yvar = "percent", groupvar = "gender",
    yaxis_title = "Males and females in pay quartile"
  ))
})
