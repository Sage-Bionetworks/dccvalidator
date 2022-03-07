context("test-get-template.R")

syn <- attempt_instantiate()
tryCatch(
  attempt_login(syn),
  error = function(e) {
    print(glue::glue("Did not log into Synapse: {e$message}"))
  }
)

test_that("get_template fails when not logged in to Synapse", {
  skip_if(is.null(syn))

  syn$logout()
  reticulate::py_capture_output(
    expect_error(
      get_template(synID = "syn12973252", syn)
    ),
    type = "stderr"
  )
})

tryCatch(
  attempt_login(syn),
  error = function(e) {
    print(glue::glue("Did not log into Synapse: {e$message}"))
  }
)

test_that("get_template errors for files from synID that are not xlsx or csv", {
  skip_if_not(logged_in(syn = syn))

  reticulate::py_capture_output(
    expect_error(
      get_template("syn17039045", syn = syn)
    ),
    type = "stderr"
  )
})

test_that("get_template can read in excel and csv templates", {
  skip_if_not(logged_in(syn = syn))

  csv <- get_template(synID = "syn18384877", syn = syn, version = 1)
  xlsx <- get_template(synID = "syn18384878", syn = syn, version = 1)
  expect_equal(csv, c("a", "b", "c"))
  expect_equal(xlsx, c("a", "b", "c"))
})

test_that("get_template can get different version of a template", {
  skip_if_not(logged_in(syn = syn))

  xlsx1 <- get_template(synID = "syn18384878", syn = syn, version = 1)
  xlsx2 <- get_template(synID = "syn18384878", syn = syn, version = 2)
  expect_equal(xlsx1, c("a", "b", "c"))
  expect_equal(xlsx2, c("a", "b", "c", "d"))
})

test_that("get_template accepts synID with id param", {
  skip_if_not(logged_in(syn = syn))

  xlsx1 <- get_template(id = "syn18384878", syn = syn, version = 1)
  xlsx2 <- get_template(id = "syn18384878", syn = syn, version = 2)
  expect_equal(xlsx1, c("a", "b", "c"))
  expect_equal(xlsx2, c("a", "b", "c", "d"))
})

test_that("get_template can correctly handles URLs and filepaths", {
  gt <- get_template(id = "https://raw.githubusercontent.com/afwillia/sysbioDCCjsonschemas/schematic_workflow/schematic_schemas/json/amp.ad.data.Assay16SrRNAseqMetadataTemplate.schema.json",
               syn = syn)
  gu <- get_file_schema(file="https://raw.githubusercontent.com/afwillia/sysbioDCCjsonschemas/schematic_workflow/schematic_schemas/json/amp.ad.data.Assay16SrRNAseqMetadataTemplate.schema.json")
  expect_identical(gt, names(gu$properties))
  
  gt <- get_template(id = system.file("testdata",
                      "amp.ad.data.Assay16SrRNAseqMetadataTemplate.schema.json",
                      package="dccvalidator"))
  expect_identical(gt, names(gu$properties))
})

# test_that("get_template can get keys from a (simple) registered schema", {
#   skip_if_not(logged_in(syn = syn))
# 
#   ## Expect schema to have easily obtained distinct properties
#   res1 <- get_template(id = "nkauer-dccvalidator.simpleTest-0.1.0", syn = syn)
#   res2 <- get_template(id = "nkauer-dccvalidator.simpleTest-0.2.0", syn = syn)
#   expect_equal(res1, c("a", "b", "c"))
#   expect_equal(res2, c("a", "b", "c", "d"))
# })

test_that("get_template can get keys from a schema file", {
  skip_if_not(logged_in(syn = syn))
  
  ## Expect schema to have easily obtained distinct properties
  schema <- 
    system.file("testdata",
    "amp.ad.data.Assay16SrRNAseqMetadataTemplate.schema.json",
    package="dccvalidator")
  expect_equal(get_template_keys_schema(file=schema),
    c("dnaExtractionMethod", "runType", "libraryPrep", "sequencingBatch",
    "specimenID", "platform", "assay", "readLength", "dnaBatch", "Component", 
    "assayTarget", "libraryBatch"))
})

test_that("get_template can get a schema file", {
  skip_if_not(logged_in(syn = syn))
  
  schema <- 
    system.file("testdata",
                "amp.ad.data.Assay16SrRNAseqMetadataTemplate.schema.json",
                package="dccvalidator")
  
  schema <- get_file_schema(file = schema)
  expect_equal(names(schema$properties),
    c("dnaExtractionMethod", "runType", "libraryPrep", "sequencingBatch",
      "specimenID", "platform", "assay", "readLength", "dnaBatch", "Component", 
      "assayTarget", "libraryBatch"))
  
  schema_url <- get_file_schema(file="https://raw.githubusercontent.com/afwillia/sysbioDCCjsonschemas/schematic_workflow/schematic_schemas/json/amp.ad.data.Assay16SrRNAseqMetadataTemplate.schema.json")
  expect_equal(names(schema_url$properties),
     c("dnaExtractionMethod", "runType", "libraryPrep", "sequencingBatch",
       "specimenID", "platform", "assay", "readLength", "dnaBatch", "Component", 
       "assayTarget", "libraryBatch"))
})

test_that("get_synapse_schema returns error if schema not returned", {
  skip_if_not(logged_in(syn = syn))

  expect_error(get_synapse_schema(syn, "fake-template"))
})

test_that("get_template returns error if schema not returned", {
  skip_if_not(logged_in(syn = syn))

  expect_error(get_template(syn = syn, id = "fake-template"))
})
