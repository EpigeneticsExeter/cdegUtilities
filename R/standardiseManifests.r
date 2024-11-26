standardiseManifests <- function(arrayType,
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
    epic1Manifest <- file.path(
      referenceDirectory,
      "EPICArray",
      "EPIC.anno.GRCh38.tsv"
    )
    manifest <- data.table::fread(
      epic1Manifest,
      fill = TRUE,
      header = TRUE,
      sep = "\t",
      stringsAsFactors = FALSE,
      data.table = FALSE
    )
    manifest <- manifest[match(probeMatchingIndex, manifest$probeID), ]
  }

  if (arrayType == "V2") {
    epic2Manifest <- file.path(
      referenceDirectory,
      "EPICArray",
      "EPIC-8v2-0_A1.csv"
    )
    manifest <- data.table::fread(
      epic2Manifest,
      skip = 7,
      fill = TRUE,
      header = TRUE,
      sep = ",",
      stringsAsFactors = FALSE,
      data.table = FALSE
    )
    manifest <- manifest[
      match(probeMatchingIndex, manifest$IlmnID),
      c("CHR", "Infinium_Design_Type")
    ]
    colnames(manifest) <- gsub(
      "Infinium_Design_Type",
      "designType",
      colnames(manifest)
    )
    print("loaded EpicV2 manifest")
  }
  return(manifest)
}
