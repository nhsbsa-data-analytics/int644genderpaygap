df <- headcount_data(headcount)
x <- df$overview_gender

testthat::test_that("gender_profile runs without errors", {
  expect_silent(gender_profile(x,
    xvar = "FINANCIAL_YEAR",
    yvars = c("Male", "Female"),
    series_names = c("Male", "Female"),
    yaxis_title = "Male and Female employee headcount",
    yaxis_label = "number"
  ))
})


testthat::test_that("gender_profile outputs a highchart, htmlwidget class", {
  expect_equal(class(
    gender_profile(x,
      xvar = "FINANCIAL_YEAR",
      yvars = c("Male", "Female"),
      series_names = c("Male", "Female"),
      yaxis_title = "Male and Female employee headcount",
      yaxis_label = ""
    ),
    yaxis_label = "number"
  ), c("highchart", "htmlwidget"))
})

testthat::test_that("gender_profile takes list as an input", {
  expect_equal(class(list(x)), "list")
})


testthat::test_that("gender_profile input data frame must contain Female,
                    Male column", {
                      expect_equal(length(grep("Female|Male", names(x))), 2)
                    })
