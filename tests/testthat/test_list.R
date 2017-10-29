library(fbAdsInsightsR)

context("grab-family")

TK <- get(load("token.RData"))
id <- get(load("id.RData"))

test_that("listBusinesses", {
  bus <- listBusinesses(TK)
  expect_equal(nrow(bus), 3)
})

test_that("listAccounts", {
  accs <- listAccounts("me", TK)
  expect_equal(ncol(accs), 2)
  accs <- listAccounts("me", TK, fields = c("account_status", "owner"))
  expect_equal(ncol(accs), 3)
})

test_that("listAdApps", {
  apps <- listAdApps(id, TK)
  expect_equal(ncol(apps), 4)
})

test_that("listApps", {
  expect_warning(listApps(id, TK))
})

test_that("listBroadTargeting", {
  cat <- listBroadTargeting(id, TK)
  expect_equal(ncol(cat), 2)
})

test_that("listCreatives", {
  crea <- listCreatives(id, TK)
  expect_equal(ncol(crea), 2)
  crea <- listCreatives(id, TK, fields = findFields("listCreatives"))
  expect_equal(ncol(crea), 12)
})

test_that("listVideos", {
  vid <- listVideos(id, TK)
  expect_equal(ncol(vid), 2)
})

test_that("listImages", {
  vid <- listImages(id, TK)
  expect_equal(ncol(vid), 2)
  vid <- listImages(id, TK, fields = c("created_time", "hash"))
  expect_equal(ncol(vid), 3)
})