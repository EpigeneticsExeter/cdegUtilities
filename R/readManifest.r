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
