library(fbAdsInsightsR)

context("get-family")

TK <- get(load("token.RData"))
id <- get(load("id.RData"))

test_that("getAny", {
  accs <- getAny(id, TK, action.attribution.windows = "1d_view")
  Sys.sleep(0.1)
  accs <- getAny(id, TK, action.breakdowns = "action_device")
  Sys.sleep(0.1)
  accs <- getAny(id, TK, action.report.time = "conversion")
  Sys.sleep(0.1)
  accs <- getAny(id, TK, breakdowns = c("age", "gender"))
  expect_equal(length(names(accs)[grep("age|gender", names(accs))]), 2)
  Sys.sleep(0.1)
  accs <- getAny(id, TK, breakdowns = "country")
  expect_true(names(accs)[grep("country", names(accs))] == "country")
  Sys.sleep(0.1)
  accs <- getAny(id, TK, date.preset = "last_7_days", time.increment = 1)
  expect_gt(nrow(accs), 1)
  Sys.sleep(0.1)
  accs <- getAny(id, TK, level = "adset")
  expect_true(names(accs)[grep("adset_id", names(accs))] == "adset_id")
  Sys.sleep(0.1)
  accs <- getAny(id, TK, time.range = c(since = "2016-07-10", 
                                        until = as.character(Sys.Date())))
  expect_gt(nrow(accs), 0)
  Sys.sleep(0.1)
  accs <- getAny(id, TK, summary = T, limit = 1000)
  expect_is(accs, "list")
  Sys.sleep(0.1)
  accs <- getAny(id, TK, fields = c("impressions", "clicks", "ctr"))
  expect_equal(ncol(accs), 5)
})

test_that("getActivity", {
  act <- getActivity(TK, id)
  expect_gt(nrow(act), 1)
  act <- getActivity(TK, id, since = "2016-07-10", n = 120, limit = 2000,
                     until = as.character(Sys.Date()), 
                     fields = findFields("getActivity"))
  expect_gt(nrow(act), 1)
})

test_that("getCreative", {
  crea <- listCreatives(id, TK, fields = "object_story_id")
  crea <- getCreative(crea$object_story_id[1], TK)
  expect_is(crea, "data.frame")
})