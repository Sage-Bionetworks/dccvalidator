############################
####  Set up test data  ####
############################

library("fs")

dir_create(
  c(
    "testdata-valid",
    "testdata-valid/raw",
    "testdata-valid/metadata",
    "testdata-invalid",
    "testdata-invalid/raw",
    "testdata-invalid/metadata"
  )
)

set.seed(8572)

indIDs    <- c("ABC", "DEF", "GHI", "JKL", "MNO")
sampleIDs <- sprintf("%03d", seq_len(length(indIDs) * 2))

######################
####  Valid data  ####
######################

valid_individual <- data.frame(
  individualID = indIDs,
  age = sample(18:50, size = 5),
  stringsAsFactors = FALSE
)

valid_assay <- data.frame(
  individualID = indIDs,
  sampleID = sampleIDs,
  filename = paste0(LETTERS[seq_along(sampleIDs)], ".bam"),
  stringsAsFactors = FALSE
)

write.csv(valid_individual, "testdata-valid/valid_individual.csv", row.names = FALSE)
write.csv(valid_assay, "testdata-valid/valid_assay.csv", row.names = FALSE)
file_create(paste0("testdata-valid/metadata/", valid_assay$filename))

########################
####  Invalid data  ####
########################

## Missing one row, i.e. one individual ID
invalid_individual <- valid_individual[-1, ]

## Missing one row, i.e. one sample ID (all individual IDs are present because
## they repeat 2x and only one row is missing)
invalid_assay <- valid_assay[-10, ]

write.csv(invalid_individual, "testdata-invalid/invalid_individual.csv", row.names = FALSE)
write.csv(invalid_assay, "testdata-invalid/invalid_assay.csv", row.names = FALSE)
file_create(paste0("testdata-invalid/metadata/", invalid_assay$filename))

#########################################################
####  Data with IDs as factors instead of character  ####
#########################################################

factor_individual <- valid_individual
factor_individual$individualID <- as.factor(factor_individual$individualID)

factor_assay <- valid_assay
factor_assay$individualID <- as.factor(factor_assay$individualID)
