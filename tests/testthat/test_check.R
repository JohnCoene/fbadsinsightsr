library(fbAdsInsightsR)

TK <- get(load("token.RData"))
id <- get(load("id.RData"))

test_that("checkStatus", {
  camps <- grabCampaigns(id, TK)
  status <- checkStatus(camps$id[1], TK)
  expect_equal(ncol(status), 2)
})

test_that("checkTarget", {
  ads <- grabAds(id, TK)
  target <- checkTargetSentence(ads$id[1], TK)
  expect_equal(nrow(target), 1)
  target <- checkTargetTree(id, TK)
  expect_equal(ncol(target), 8)
})

test_that("checkUsers", {
  users <- checkUsers(id, TK)
  expect_equal(ncol(users), 4)
})