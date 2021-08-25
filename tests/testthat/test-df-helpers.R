context("df-helpers.R")

Sys.setenv(R_CONFIG_ACTIVE = "testing")

dat1 <- tibble::tribble(
  ~metadataType,
  "manifest",
  "individual",
  "biospecimen",
  "assay"
)

dat2 <- tibble::tribble(
  ~metadataType,
  "manifest",
  "assay"
)

test_that("get_metadataType_indices returns named list for metadataTypes present", { # nolint
  res1 <- get_metadataType_indices(dat1, "manifest")
  res2 <- get_metadataType_indices(dat1, "assay")
  res3 <- get_metadataType_indices(dat1, "biospecimen")
  res4 <- get_metadataType_indices(dat1, "individual")
  res5 <- get_metadataType_indices(dat1, c("manifest", "biospecimen"))
  res6 <- get_metadataType_indices(dat1, c("individual", "assay"))
  res7 <- get_metadataType_indices(
    dat1,
    c("manifest", "biospecimen", "assay", "individual")
  )
  expect_equal(res1, list(manifest = 1))
  expect_equal(res2, list(assay = 4))
  expect_equal(res3, list(biospecimen = 3))
  expect_equal(res4, list(individual = 2))
  expect_equal(res5, list(manifest = 1, biospecimen = 3))
  expect_equal(res6, list(individual = 2, assay = 4))
  expect_equal(res7, list(
    manifest = 1,
    biospecimen = 3,
    assay = 4,
    individual = 2)
  )
})

test_that("get_metadataType_indices doesn't return indices for missing metadataTypes", { # nolint
  res1 <- get_metadataType_indices(dat2, "biospecimen")
  res2 <- get_metadataType_indices(dat2, "individual")
  res3 <- get_metadataType_indices(dat2, c("individual", "biospecimen"))
  res4 <- get_metadataType_indices(dat2, c("manifest", "biospecimen"))
  res5 <- get_metadataType_indices(dat2, c("individual", "assay"))
  res6 <- get_metadataType_indices(
    dat2,
    c("manifest", "biospecimen", "assay", "individual")
  )
  res7 <- get_metadataType_indices(
    tibble::tribble(
      ~metadataType,
      "foo",
      "bar"
    ),
    c("baz", "shazam")
  )
  expect_null(res1)
  expect_null(res2)
  expect_null(res3)
  expect_equal(res4, list(manifest = 1))
  expect_equal(res5, list(assay = 2))
  expect_equal(res6, list(manifest = 1, assay = 2))
  expect_null(res7)
})

test_that("get_metadataType_indices handles duplicate types", {
  res1 <- get_metadataType_indices(
    tibble::tribble(
      ~metadataType,
      "foo",
      "bar",
      "bar"
    ),
    c("foo", "bar")
  )
  res2 <- get_metadataType_indices(
    tibble::tribble(
      ~metadataType,
      "bar",
      "bar"
    ),
    "bar"
  )
  res3 <- get_metadataType_indices(
    tibble::tribble(
      ~metadataType,
      "bar",
      "bar"
    ),
    "foo"
  )
  expect_equal(res1, list(foo = 1, bar = c(2, 3)))
  expect_equal(res2, list(bar = c(1, 2)))
  expect_null(res3)
})

test_that("get_metadataType_indices throws error if missing metadataType col", {
  dat <- tibble::tribble(
    ~assay, ~species,
    NA, "human",
    NA, "human",
    NA, "human",
    "rnaSeq", "human"
  )
  expect_error(get_metadataType_indices(dat, c("foo", "bar")))
})

test_that("gather_template_ids gets the ids from the config", {
  res1 <- gather_template_ids(type = "manifest")
  res2 <- gather_template_ids(type = "biospecimen", species = "human")
  res3 <- gather_template_ids(type = "assay", assay = "rnaSeq")
  res4 <- gather_template_ids(type = "individual", species = "human")
  res5 <- gather_template_ids(
    type = "biospecimen",
    species = "human",
    biospecimen_type = NA
  )
  res6 <- gather_template_ids(
    type = "biospecimen",
    species = "human",
    biospecimen_type = ""
  )
  expect_equal(res1, "syn20820080")
  expect_equal(res2, "syn12973252")
  expect_equal(res3, "syn12973256")
  expect_equal(res4, "syn12973254")
  expect_equal(res5, "syn12973252")
  expect_equal(res6, "syn12973252")
})

test_that("gather_template_ids returns NA if missing species for bio/ind", {
  expect_equal(gather_template_ids(type = "biospecimen"), NA)
  expect_equal(gather_template_ids(type = "individual"), NA)
})

test_that("gather_template_ids returns NA if missing assay for assay", {
  expect_equal(gather_template_ids(type = "assay"), NA)
})

test_that("gather_template_ids gets correct biospecimen type", {
  res1 <- gather_template_ids(
    type = "biospecimen",
    species = "mouse or other animal model",
    biospecimen_type = "in vitro"
  )
  res2 <- gather_template_ids(
    type = "biospecimen",
    species = "mouse or other animal model",
    biospecimen_type = "other"
  )

  expect_equal(res1, "syn25955510")
  expect_equal(res2, "syn12973252")
})

test_that("gather_template_ids returns NA if missing information", {
  expect_true(is.na(gather_template_ids(type = "assay")))
  expect_true(is.na(gather_template_ids(type = "individual")))
  expect_true(is.na(gather_template_ids(type = "biospecimen")))
})
