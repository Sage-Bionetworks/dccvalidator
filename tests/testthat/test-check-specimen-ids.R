context("test-check-specimen-ids.R")

library("synapser")
if (on_travis()) syn_travis_login() else synLogin()

test_that("get_annotation finds the value of an annotation", {
  specimenID <- get_annotation("syn18083968", "specimenID")
  expect_equal(specimenID, list("a"))
  expect_null(get_annotation("syn18083968", "assay"))
})

test_that("get_annotation errors if no key provided", {
  expect_error(get_annotation("syn18083968"))
})

test_that("get_specimenID_data returns specimen ID for one or multiple files", {
  synIDs <- c("syn18083968", "syn18083969", "syn18083972", "syn18084124")
  one <- get_specimenID_data(synIDs[1])
  multiple <- get_specimenID_data(synIDs)

  expect_equal(one, c(syn18083968 = "a"))
  expect_equal(
    multiple,
    c(syn18083968 = "a", syn18083969 = "b", syn18083972 = "c")
  )
})

test_that("get_specimenID_metadata errors if no specimenID column", {
  expect_error(get_specimenID_metadata("syn18085721"))
})

test_that("get_specimenID_metadata returns specimen IDs", {
  dat <- get_specimenID_metadata("syn18084015")
  expect_equal(dat, c("a", "b", "c"))
})

test_that("find_specimen_mismatches finds mismatched IDs in annotations and metadata files", {
  synIDs <- c("syn18083968", "syn18083969", "syn18083972", "syn18084124")
  mis1 <- find_specimen_mismatches(synIDs, "syn18084015")
  mis2 <- find_specimen_mismatches(synIDs, "syn18084016")

  expect_equal(
    mis1,
    list(
      missing_from_metadata = structure(character(0), .Names = character(0)),
      missing_from_data = character(0)
    )
  )
  expect_equal(
    mis2,
    list(missing_from_metadata = c(syn18083972 = "c"), missing_from_data = "d")
  )
})
