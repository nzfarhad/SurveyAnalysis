# Test file for surveyAnalysis package

library(testthat)
library(surveyAnalysis)

# Test data
test_data <- data.frame(
  gender = c("Male", "Female", "Male", "Female"),
  age = c(25, 30, 35, 40),
  region = c("North", "South", "North", "South"),
  services = c("Health; Education", "Health", "Education", "Health; Transport")
)

# Test analysis plan with all required columns
test_ap <- data.frame(
  variable = c("gender", "age"),
  kobo_type = c("select_one", "integer"),
  aggregation_method = c("proportion", "mean"),
  disaggregation = c("region", "all"),
  label = c("Gender", "Age"),
  disagg_label = c("Region", NA),
  sheet = c("data", "data"),
  Remarks = c("", ""),
  repeat_for = c("", ""),
  stringsAsFactors = FALSE
)

test_that("analysis_func returns correct structure", {
  result <- analysis_func(df = test_data, ap = test_ap)
  
  expect_true(is.data.frame(result))
  expect_true("Question" %in% names(result))
  expect_true("Result" %in% names(result))
  expect_true("Count" %in% names(result))
})

test_that("single_select works correctly", {
  result <- single_select(test_data, "gender", "region", "North")
  
  expect_true(is.data.frame(result))
  expect_equal(result$aggregation_method[1], "perc")
  expect_equal(result$variable[1], "gender")
})

test_that("stat_mean works correctly", {
  result <- stat_mean(test_data, "age", "all", "all")
  
  expect_true(is.data.frame(result))
  expect_equal(result$aggregation_method, "mean")
  expect_equal(result$variable, "age")
  expect_true(is.numeric(result$Freq))
})
