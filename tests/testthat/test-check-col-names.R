context("test-check-col-names.R")

syn <- attempt_instantiate()
tryCatch(
  attempt_login(syn),
  error = function(e) {
    print(glue::glue("Did not log into Synapse: {e$message}"))
  }
)
Sys.setenv(R_CONFIG_ACTIVE = "testing")

test_that("check_col_names returns condition object when check passes", {
  template <- data.frame(x = 1, y = 1)
  dat <- data.frame(x = 5:10, y = 5:10)
  result <- check_col_names(dat, names(template))
  expect_true(inherits(result, "check_pass"))
})

test_that("check_col_names returns condition object when check fails", {
  template <- data.frame(x = 1, y = 1, z = 1)
  dat <- data.frame(x = 5:10, y = 5:10)
  result <- check_col_names(dat, names(template))
  expect_true(inherits(result, "check_fail"))
})

test_that("check_col_names returns missing columns in the data", {
  template <- data.frame(x = 1, y = 1, z = 1)
  dat <- data.frame(x = 5:10, y = 5:10)
  result <- check_col_names(dat, names(template))
  expect_equal(result$data, "z")
})

test_that("check_cols_individual works for individual columns", {
  skip_if_not(logged_in(syn = syn))

  cols <- get_template(synID = "syn12973254", syn = syn, version = 1)
  full_col <- data.frame(matrix(ncol = length(cols)))
  colnames(full_col) <- cols
  incomplete_col <- full_col[, !names(full_col) %in% "yearsEducation"]

  expect_true(
    inherits(
      check_cols_individual(
        full_col,
        id = "syn12973254",
        syn = syn,
        version = 1
      ),
      "check_pass"
    )
  )
  expect_true(
    inherits(
      check_cols_individual(
        incomplete_col,
        id = "syn12973254",
        syn = syn,
        version = 1
      ),
      "check_fail"
    )
  )
})

test_that("check_cols_individual returns invalid columns in condition object", {
  skip_if_not(logged_in(syn = syn))

  cols <- get_template(synID = "syn12973254", syn = syn, version = 1)
  full_col <- data.frame(matrix(ncol = length(cols)))
  colnames(full_col) <- cols
  incomplete_col <- full_col[, !names(full_col) %in% "yearsEducation"]

  expect_equal(
    check_cols_individual(
      incomplete_col,
      id = "syn12973254",
      syn = syn,
      version = 1
    )$data,
    "yearsEducation"
  )
})

test_that("check_cols_biospecimen works for biospecimen columns", {
  skip_if_not(logged_in(syn = syn))

  biosp_names <- get_template(synID = "syn12973252", syn = syn, version = 4)

  full_col_biosp <- data.frame(matrix(ncol = length(biosp_names)))
  colnames(full_col_biosp) <- biosp_names
  incomplete_col_biosp <- full_col_biosp[, !names(full_col_biosp) == "organ"]

  expect_true(
    inherits(
      check_cols_biospecimen(
        full_col_biosp,
        id = "syn12973252",
        syn = syn,
        version = 4
      ),
      "check_pass"
    )
  )
  expect_true(
    inherits(
      check_cols_biospecimen(
        incomplete_col_biosp,
        id = "syn12973252",
        syn = syn,
        version = 4
      ),
      "check_fail"
    )
  )
})

test_that("check_cols_biospecimen returns invalid columns in condition obj.", {
  skip_if_not(logged_in(syn = syn))

  biosp_names <- get_template(synID = "syn12973252", syn = syn, version = 4)

  full_col_biosp <- data.frame(matrix(ncol = length(biosp_names)))
  colnames(full_col_biosp) <- biosp_names
  incomplete_col_biosp <- full_col_biosp[, !names(full_col_biosp) == "organ"]

  expect_equal(
    check_cols_biospecimen(
      incomplete_col_biosp,
      id = "syn12973252",
      syn = syn,
      version = 4
    )$data,
    "organ"
  )
})

test_that("check_cols_biospecimen can get drosophila template", {
  skip_if_not(logged_in(syn = syn))

  drosophila_names <- get_template(
    synID = "syn20673251",
    syn = syn,
    version = 1
  )
  drosophila_data <- data.frame(matrix(ncol = length(drosophila_names)))
  colnames(drosophila_data) <- drosophila_names

  expect_true(
    inherits(
      check_cols_biospecimen(
        drosophila_data,
        id = "syn20673251",
        syn = syn,
        version = 1
      ),
      "check_pass"
    )
  )
  expect_true(
    inherits(
      check_cols_biospecimen(
        drosophila_data,
        id = "syn12973252",
        syn = syn,
        version = 4
      ),
      "check_fail"
    )
  )
})

test_that("check_cols_assay works for assay columns", {
  skip_if_not(logged_in(syn = syn))

  rnaseq_names <- get_template(synID = "syn12973256", syn = syn, version = 2)

  full_col_assay <- data.frame(matrix(ncol = length(rnaseq_names)))
  colnames(full_col_assay) <- rnaseq_names
  incomplete_col_assay <- full_col_assay[, !names(full_col_assay) == "RIN"]

  expect_true(
    inherits(
      check_cols_assay(
        full_col_assay,
        id = "syn12973256",
        syn = syn,
        version = 2
      ),
      "check_pass"
    )
  )
  expect_true(
    inherits(
      check_cols_assay(
        incomplete_col_assay,
        id = "syn12973256",
        syn = syn,
        version = 2
      ),
      "check_fail"
    )
  )
})

test_that("check_cols_assay returns invalid columns within condition object", {
  skip_if_not(logged_in(syn = syn))

  rnaseq_names <- get_template(synID = "syn12973256", syn = syn, version = 2)

  full_col_assay <- data.frame(matrix(ncol = length(rnaseq_names)))
  colnames(full_col_assay) <- rnaseq_names
  incomplete_col_assay <- full_col_assay[, !names(full_col_assay) == "RIN"]

  expect_equal(
    check_cols_assay(
      incomplete_col_assay,
      id = "syn12973256",
      syn = syn,
      version = 2
    )$data,
    "RIN"
  )
})

test_that("check_cols_manifest works for manifest columns", {
  skip_if_not(logged_in(syn = syn))

  cols <- get_template(synID = "syn20820080", syn = syn, version = 3)
  dat <- data.frame(matrix(ncol = length(cols)))
  names(dat) <- cols
  incomplete <- dat[, !names(dat) %in% "parent"]

  expect_true(
    inherits(
      check_cols_manifest(dat, id = "syn20820080", version = 3, syn = syn),
      "check_pass"
    )
  )
  expect_equal(
    check_cols_manifest(
      incomplete,
      id = "syn20820080",
      syn = syn,
      version = 3
    )$data,
    "parent"
  )
})

test_that("wrapper functions for specific template gets the correct version", {
  skip_if_not(logged_in(syn = syn))

  dat <- data.frame(
    individualID = 1,
    specimenID = 1,
    organ = 1,
    tissue = 1,
    BrodmannArea = 1,
    tissueWeight = 1,
    nucleicAcidSource = 1,
    cellType = 1
  )
  expect_true(
    inherits(
      check_cols_biospecimen(dat, id = "syn12973252", syn = syn, version = 2),
      "check_pass"
    )
  )
  expect_equal(
    check_cols_biospecimen(
      dat,
      id = "syn12973252",
      syn = syn,
      version = 3
    )$data,
    c("samplingDate", "sampleStatus", "tissueVolume", "fastingState")
  )
})

test_that("check_cols functions handle NULL/NA input", {
  expect_null(check_col_names(NULL, NA))
  expect_null(check_cols_manifest(NULL, NA))
  expect_null(check_cols_individual(NULL, NA))
  expect_null(check_cols_biospecimen(NULL, NA))
  expect_null(check_cols_assay(NULL, NA))
})
