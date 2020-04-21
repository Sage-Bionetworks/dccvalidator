context("test-check-all.R")

library("tibble")
syn <- attempt_instantiate()
attempt_login(syn)
annots <- tribble(
  ~key, ~value, ~columnType,
  "assay", "rnaSeq", "STRING",
  "fileFormat", "fastq", "STRING",
  "fileFormat", "txt", "STRING",
  "fileFormat", "csv", "STRING",
  "species", "Human", "STRING"
)

test_that("check_all() returns a list of check conditions", {
  skip_if_not(logged_in(syn = syn))
  data <- tibble::tibble(
    metadataType = c(
      "manifest",
      "individual",
      "biospecimen",
      "assay"
    ),
    name = c("file1", "file2", "file3", "file4"),
    species = "human",
    assay = "rnaSeq",
    file_data = c(
      list(data.frame(a = c(TRUE, FALSE), b = c(1, 3))),
      list(data.frame(a = c(TRUE, FALSE), b = c(1, 3))),
      list(data.frame(a = c(TRUE, FALSE), b = c(1, 3))),
      list(data.frame(a = c(TRUE, FALSE), b = c(1, 3)))
    )
  )
  res <- check_all(data, annots, syn)
  expect_equal(class(res), "list")
  expect_true(all(unlist(
    purrr::map(
      res,
      function(x) {
        inherits(x, "check_fail") | inherits(x, "check_pass") | inherits(x, "check_warn") # nolint
      }
    )
  )))
})

test_that("check_all() returns NULL for checks with missing data", {
  skip_if_not(logged_in(syn = syn))
  data1 <- tibble::tibble(
    metadataType = c(
      "manifest",
      "individual",
      "biospecimen",
      "assay"
    ),
    name = c(NA, NA, NA, NA),
    species = "human",
    assay = "rnaSeq",
    file_data = c(
      list(NULL),
      list(NULL),
      list(NULL),
      list(NULL)
    )
  )
  data2 <- tibble::tibble(
    metadataType = c(
      "manifest",
      "individual",
      "biospecimen",
      "assay"
    ),
    name = c("file1", NA, NA, NA),
    species = "human",
    assay = "rnaSeq",
    file_data = c(
      list(data.frame(a = c(TRUE, FALSE), b = c(1, 3))),
      list(NULL),
      list(NULL),
      list(NULL)
    )
  )
  data3 <- tibble::tibble(
    metadataType = c(
      "manifest",
      "individual",
      "biospecimen",
      "assay"
    ),
    name = c(NA, "file2", NA, "file4"),
    species = "human",
    assay = "rnaSeq",
    file_data = c(
      list(NULL),
      list(data.frame(a = c(TRUE, FALSE), b = c(1, 3))),
      list(NULL),
      list(data.frame(a = c(TRUE, FALSE), b = c(1, 3)))
    )
  )
  data4 <- tibble::tibble(
    metadataType = c(
      "manifest",
      "individual",
      "biospecimen",
      "assay"
    ),
    name = c("file1", "file2", "file3", NA),
    species = "human",
    assay = "rnaSeq",
    file_data = c(
      list(data.frame(a = c(TRUE, FALSE), b = c(1, 3))),
      list(data.frame(a = c(TRUE, FALSE), b = c(1, 3))),
      list(data.frame(a = c(TRUE, FALSE), b = c(1, 3))),
      list(NULL)
    )
  )
  res1 <- check_all(data1, annots, syn)
  res2 <- check_all(data2, annots, syn)
  res3 <- check_all(data3, annots, syn)
  res4 <- check_all(data4, annots, syn)

  # Some checks should be NULL based on which data is missing
  # Since all of these have missing data, the # of checks done
  # should be less than the total # of checks possible
  expect_true(all(purrr::map_lgl(res1, ~ is.null(.x))))
  expect_true(sum(purrr::map_lgl(res2, ~ !is.null(.x))) < length(res2))
  expect_true(sum(purrr::map_lgl(res3, ~ !is.null(.x))) < length(res3))
  expect_true(sum(purrr::map_lgl(res4, ~ !is.null(.x))) < length(res4))
})

test_that("check_all() returns expected conditions", {
  skip_if_not(logged_in(syn = syn))
  data <- tibble::tibble(
    metadataType = c(
      "manifest",
      "individual",
      "biospecimen",
      "assay"
    ),
    name = c("file1", "file2", "file3", "file4"),
    species = "human",
    assay = "rnaSeq",
    file_data = c(
      list(data.frame(
        path = c("file1", "file2", "file3", "file4", NA, NA, NA),
        individualID = c(NA, NA, NA, NA, "a", "b", "c"),
        specimenID = c(NA, NA, NA, NA, NA, "1", "3"),
        stringsAsFactors = FALSE
      )),
      list(data.frame(
        individualID = c("a", "b"),
        age = c(27, 32),
        stringsAsFactors = FALSE
      )),
      list(data.frame(
        individualID = c("a", "b"),
        specimenID = c("1", "3"),
        fileFormat = c("xlsx", "tex"),
        stringsAsFactors = FALSE
      )),
      list(data.frame(
        specimenID = c("1", "3"),
        assay = c("rnaSeq", "rnaSeq"),
        stringsAsFactors = FALSE
      ))
    )
  )
  res <- check_all(data, annots, syn)
  # All metadata filenames in manifest passes
  expect_true(inherits(res$meta_files_in_manifest, "check_pass"))
  # Missing individualID "c" from individual metadata
  expect_equal(
    res$individual_ids_indiv_manifest$data$`Missing from individual`[1],
    "c"
  )
  # Invalid tissue annotation values
  expect_equal(res$annotation_values_biosp$data$fileFormat, c("xlsx", "tex"))
})

test_that("check_all() throws error if not exactly 1 metadata type each", {
  skip_if_not(logged_in(syn = syn))
  # Missing biospecimen
  data1 <- tibble::tibble(
    metadataType = c(
      "manifest",
      "individual",
      "assay"
    )
  )
  # Duplicate assay
  data2 <- tibble::tibble(
    metadataType = c(
      "manifest",
      "individual",
      "assay",
      "assay"
    )
  )
  expect_error(check_all(data1, annots, syn))
  expect_error(check_all(data2, annots, syn))
})

test_that("check_all runs check_ages_over_90 for human data", {
  skip_if_not(logged_in(syn = syn))
  data_human <- tibble::tibble(
    metadataType = c(
      "manifest",
      "individual",
      "biospecimen",
      "assay"
    ),
    name = c("file1", "file2", "file3", "file4"),
    species = "human",
    assay = "rnaSeq",
    file_data = c(
      list(data.frame(a = 1)),
      list(data.frame(ageDeath = 95)),
      list(data.frame(a = 1)),
      list(data.frame(a = 1))
    )
  )
  data_animal <- data_human
  data_animal$species <- "mouse or other animal model"
  data_has_na <- data_human
  data_has_na$species <- c(NA, "human", "human", NA)
  res1 <- check_all(data_human, annots, syn)
  res2 <- check_all(data_animal, annots, syn)
  res3 <- check_all(data_has_na, annots, syn)
  expect_true(inherits(res1$ages_over_90, "check_warn"))
  expect_null(res2$ages_over_90)
  expect_true(inherits(res3$ages_over_90, "check_warn"))
})

test_that("check_all catches duplicate file paths in manifest", {
  skip_if_not(logged_in(syn = syn))
  data <- tibble::tibble(
    metadataType = c(
      "manifest",
      "individual",
      "biospecimen",
      "assay"
    ),
    name = c("file1", "file2", "file3", "file4"),
    species = "human",
    assay = "rnaSeq",
    file_data = c(
      list(data.frame(path = c("/file.txt", "/file.txt"))),
      list(data.frame(a = 1)),
      list(data.frame(a = 1)),
      list(data.frame(a = 1))
    )
  )

  res1 <- check_all(data, annots, syn)
  expect_true(inherits(res1$duplicate_file_paths, "check_fail"))
})
