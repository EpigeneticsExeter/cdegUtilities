#' Read and Standardise a Manifest File
#'
#' Reads in a manifest file that has a very specific file name depending on
#' the given array type. Important columns are standardised by name and contents
#'
#' @param referenceDirectory This is a string pointing to the directory in
#'  which the manifest files exist in. It is expected to contain:
#'  "450K_reference/AllProbeIlluminaAnno.Rdata"
#'  "EPICArray/EPIC.anno.CRCh38.tsv"
#'  "EPICArray/EPIC-8v2-0_A1.csv"
#' @param probeMatchingIndex This is a vector of probe IDs that are present
#'  in your dataset. This could be obtained via extracting the rownames of your
#'  raw beta matrix, or from a gds file that contains probe IDs.
#' @param arrayType This is a string with a value from c("450K", "V1", "V2")
#'
#' @return A data frame with varying columns. Certain columns are standardised:
#'  designType - The infinium design type
#'  CHR - The chromosome number in the form 'chrxyz'
#'
#' @examples manifest <- readManifest(
#'   "path/to/references",
#'   rownames(beta_matrix),
#'   "V1"
#' )
#' manifest <- readManifest(
#'   "path/to/references",
#'   Biobase::fData(gds_file)[["Probe_ID"]],
#'   "450K"
#' )
readManifest <- function(referenceDirectory,
                         probeMatchingIndex,
                         arrayType = c("450K", "V1", "V2")) {
  if (!file.exists(referenceDirectory)) {
    stop(
      "Reference directory: ",
      referenceDirectory,
      "Does not exist, please check this."
    )
  }
  arrayType <- match.arg(arrayType)
  if (arrayType == "450K") {
    hm450Kmanifest <- file.path(
      referenceDirectory,
      "450K_reference",
      "AllProbeIlluminaAnno.Rdata"
    )
    if (!file.exists(hm450Kmanifest)) {
      stop(
        "Manifest: ",
        hm450Kmanifest,
        "Does not exist, please check this."
      )
    }
    # Loads probeAnnot (needs generalising in the future)
    load(hm450Kmanifest)
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
    if (!file.exists(epicV1Manifest)) {
      stop(
        "Manifest: ",
        epicV1Manifest,
        "Does not exist, please check this."
      )
    }
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
    if (!file.exists(epicV2Manifest)) {
      stop(
        "Manifest: ",
        epicV2Manifest,
        "Does not exist, please check this."
      )
    }
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
