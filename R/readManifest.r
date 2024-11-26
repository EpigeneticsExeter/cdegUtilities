#' Read and Standardise a Manifest File
#'
#' Reads in a manifest file that has a very specific file name depending on
#' the given array type. Important columns are standardised by name and contents
#'
#' @param arrayType This is a string with a value from c("450K", "V1", "V2")
#' @param referenceDirectory This is a string pointing to the directory in
#'  which the manifest files exist in. It is expected to contain:
#'  "450K_reference/AllProbeIlluminaAnno.Rdata"
#'  "EPICArray/EPIC.anno.CRCh38.tsv"
#'  "EPICArray/EPIC-8v2-0_A1.csv"
#' @param probeMatchingIndex This is a vector of probe IDs that are present
#'  in your dataset. This could be obtained via extracting the rownames of your
#'  raw beta matrix, or from a gds file that contains probe IDs.
#'
#' @return A data frame with varying columns. Certain columns are standardised:
#'  designType - The infinium design type
#'  CHR - The chromosome number in the form 'chrxyz'
readManifest <- function(arrayType,
                         referenceDirectory,
                         probeMatchingIndex) {
  if (arrayType == "450K") {
    # Loads probeAnnot (needs generalising in the future)
    load(file.path(
      referenceDirectory,
      "450K_reference",
      "AllProbeIlluminaAnno.Rdata"
    ))
    manifest <- probeAnnot[match(probeMatchingIndex, probeAnnot$TargetID), ]
    colnames(manifest) <- gsub(
      "INFINIUM_DESIGN_TYPE",
      "designType",
      colnames(manifest)
    )
    manifest[["CHR"]] <- paste0("chr", manifest[["CHR"]])
  }

  if (arrayType == "V1") {
    epicV1Manifest <- file.path(
      referenceDirectory,
      "EPICArray",
      "EPIC.anno.GRCh38.tsv"
    )
    manifest <- data.table::fread(
      epicV1Manifest,
      fill = TRUE,
      header = TRUE,
      sep = "\t",
      stringsAsFactors = FALSE,
      data.table = FALSE
    )
    manifest <- manifest[match(probeMatchingIndex, manifest$probeID), ]
  }

  if (arrayType == "V2") {
    epicV2Manifest <- file.path(
      referenceDirectory,
      "EPICArray",
      "EPIC-8v2-0_A1.csv"
    )
    manifest <- data.table::fread(
      epicV2Manifest,
      skip = 7,
      fill = TRUE,
      header = TRUE,
      sep = ",",
      stringsAsFactors = FALSE,
      data.table = FALSE
    )
    manifest <- manifest[match(probeMatchingIndex, manifest$IlmnID), ]
    colnames(manifest) <- gsub(
      "Infinium_Design_Type",
      "designType",
      colnames(manifest)
    )
  }
  return(manifest)
}
