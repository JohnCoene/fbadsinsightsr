library(fbAdsInsightsR)

context("grab-family")

TK <- get(load("token.RData"))
id <- get(load("id.RData"))

test_that("grabAds", {
  ads <- grabAds(id, TK)
  expect_is(ads, "data.frame")
  ads <- grabAds(id, TK, fields = c("adlabels", "bid_type"), "date_start")
  expect_equal(length(names(ads)[grep("insights_date_start", names(ads))]), 1)
})

test_that("grabAdsets", {
  adsets <- grabAdsets(id, TK)
  expect_is(adsets, "data.frame")
  adsets <- grabAds(id, TK, fields = c("adlabels", "bid_type"), "date_start")
  expect_equal(length(names(adsets)[grep("insights_date_start", 
                                         names(adsets))]), 1)
})

test_that("grabCampaigns", {
  camps <- grabCampaigns(id, TK)
  expect_is(camps, "data.frame")
  camps <- grabAds(id, TK, fields = c("adlabels", "bid_type"), "date_start")
  expect_equal(length(names(camps)[grep("insights_date_start", 
                                        names(camps))]), 1)
})