library(fbAdsInsightsR)

context("find-family")

test_that("findFields", {
  expect_equal(length(findFields("getAny")), 73)
})