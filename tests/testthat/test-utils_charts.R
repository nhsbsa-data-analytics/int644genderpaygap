library(tidyr)
library(dplyr)

df <- gpg_data(nhsbsaGPG::afc_staff)
x <- df$df_hdcnt_gender |>
  tidyr::pivot_wider(
    names_from = gender,
    values_from = headcount
  ) |>
  dplyr::ungroup()

y <- nhsbsaGPG::paygap

z <- df$df_hdcnt_afc |> 
  filter(period == '2021/22') |> 
  mutate(headcount = headcount * ifelse(gender == "Male", 1, -1))

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
  expect_silent(gpg_pyramid(z ,
                          xvar = "afc_band",
                          yvar = "headcount",
                          yaxis_title = "Headcount"
  ))
})

testthat::test_that("gpg_stack function runs without error", {
  expect_silent(gpg_stack(quartile |> filter(period == "2021/22") ,
                            xvar = "quartile",
                            yvar = "percent",
                            groupvar = "gender",
                            yaxis_title = "Males and females in pay quartile"
  ))
})




