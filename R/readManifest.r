#' Read and Standardise a Manifest File
#'
#' Reads in a manifest file that has a very specific file name depending on
#' the given array type. Important columns are standardised by name and contents
#'
#' @param manifestFilePath This is a string pointing to the manifest file, expected
#'  to be a csv file.
#' @param probeMatchingIndex This is a vector of probe IDs that are present
#'  in your dataset. This could be obtained via extracting the rownames of your
#'  raw beta matrix, or from a gds file that contains probe IDs.
#'
#' @return A data frame generated from the chosen Illumina manifest file.
#'  Probes/Rows retained will match the probeMatchingIndex provided.
#'
#' @examples manifest <- readManifest(
#'   "path/to/manifest_file.csv",
#'   rownames(beta_matrix),
#' )
#' manifest <- readManifest(
#'   "path/to/manifest_file.csv",
#'   Biobase::fData(gds_file)[["Probe_ID"]],
#' )
#' @export
readManifest <- function(manifestFilePath,
                         probeMatchingIndex) {
  if (!file.exists(manifestFilePath)) {
    stop(
      "Manifest file: ",
      manifestFilePath,
      "Does not exist, please check this."
    )
  }
  # Some manifest files contain a header, if so we ignore it
  firstLine <- readLines(file(manifestFilePath, "r"), n = 1)
  headerPresent <- !grepl("IlmnID", firstLine)
  if (headerPresent) {
    manifest <- data.table::fread(
      manifestFilePath,
      skip = 7,
      fill = TRUE,
      header = TRUE,
      sep = ",",
      stringsAsFactors = FALSE,
      data.table = FALSE
    )
  } else {
    manifest <- data.table::fread(
      manifestFilePath,
      fill = TRUE,
      header = TRUE,
      sep = ",",
      stringsAsFactors = FALSE,
      data.table = FALSE
    )
  }
  manifest <- manifest[match(probeMatchingIndex, manifest$IlmnID), ]
  colnames(manifest) <- gsub(
    "Infinium_Design_Type",
    "designType",
    colnames(manifest)
  )
  return(manifest)
}
