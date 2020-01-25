context("test-check-all.R")

library("tibble")
syn <- attempt_instantiate()
attempt_login(syn)
annots <- get_synapse_annotations(syn = syn)

test_that("check_all() returns a list of check conditions", {
  data <- tibble::tibble(
    metadata_type = c(
      "manifest",
      "individual",
      "biospecimen",
      "assay"
    ),
    file_name = c("file1", "file2", "file3", "file4"),
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
  data1 <- tibble::tibble(
    metadata_type = c(
      "manifest",
      "individual",
      "biospecimen",
      "assay"
    ),
    file_name = c(NA, NA, NA, NA),
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
    metadata_type = c(
      "manifest",
      "individual",
      "biospecimen",
      "assay"
    ),
    file_name = c("file1", NA, NA, NA),
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
    metadata_type = c(
      "manifest",
      "individual",
      "biospecimen",
      "assay"
    ),
    file_name = c(NA, "file2", NA, "file4"),
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
    metadata_type = c(
      "manifest",
      "individual",
      "biospecimen",
      "assay"
    ),
    file_name = c("file1", "file2", "file3", NA),
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

  expect_true(all(unlist(
    purrr::map(res1, function(x) {
      is.null(x)
    })
  )))
  expect_equal(
    sum(unlist(
      purrr::map(res2, function(x) {
        !is.null(x)
      })
    )),
    6
  )
  expect_equal(
    sum(unlist(
      purrr::map(res3, function(x) {
        !is.null(x)
      })
    )),
    9
  )
  expect_equal(
    sum(unlist(
      purrr::map(res4, function(x) {
        !is.null(x)
      })
    )),
    19
  )
})

test_that("check_all() returns expected conditions", {
  data <- tibble::tibble(
    metadata_type = c(
      "manifest",
      "individual",
      "biospecimen",
      "assay"
    ),
    file_name = c("file1", "file2", "file3", "file4"),
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
        tissue = c("kleenex", "puffs"),
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
  expect_true(inherits(res[[24]], "check_pass"))
  # Missing individualID "c" from individual metadata
  expect_equal(res[[6]]$data$`Missing from individual`[1], "c")
  # Invalid tissue annotation values
  expect_equal(res[[12]]$data$tissue, c("kleenex", "puffs"))
})

test_that("check_all() throws error if not exactly 1 of each metadata type present", {
  # Missing biospecimen
  data1 <- tibble::tibble(
    metadata_type = c(
      "manifest",
      "individual",
      "assay"
    )
  )
  # Duplicate assay
  data2 <- tibble::tibble(
    metadata_type = c(
      "manifest",
      "individual",
      "assay",
      "assay"
    )
  )
  expect_error(check_all(data1, annots, syn))
  expect_error(check_all(data2, annots, syn))
})
