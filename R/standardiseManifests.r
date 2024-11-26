standardiseManifests <- function(arrayType,
                                 referenceDirectory,
                                 probeMatchingIndex) {
  if (arrayType == "450K") {
    load(file.path(
      referenceDirectory,
      "450K_reference",
      "AllProbeIlluminaAnno.Rdata"
    ))
    probeAnnot <- probeAnnot[match(probeMatchingIndex, probeAnnot$TargetID), ]
    colnames(probeAnnot) <- gsub(
      "INFINIUM_DESIGN_TYPE",
      "designType",
      colnames(probeAnnot)
    )
    print("loaded 450k manifest")
    return(probeAnnot)
  }

  if (arrayType == "V1") {
    epic1Manifest <- file.path(
      referenceDirectory,
      "EPICArray",
      "EPIC.anno.GRCh38.tsv"
    )
    probeAnnot <- data.table::fread(
      epic1Manifest,
      fill = TRUE,
      header = TRUE,
      sep = "\t",
      stringsAsFactors = FALSE,
      data.table = FALSE
    )
    probeAnnot <- probeAnnot[match(probeMatchingIndex, probeAnnot$probeID), ]
    return(probeAnnot)
  }

  if (arrayType == "V2") {
    epic2Manifest <- file.path(
      referenceDirectory,
      "EPICArray",
      "EPIC-8v2-0_A1.csv"
    )
    probeAnnot <- data.table::fread(
      epic2Manifest,
      skip = 7,
      fill = TRUE,
      header = TRUE,
      sep = ",",
      stringsAsFactors = FALSE,
      data.table = FALSE
    )
    probeAnnot <- probeAnnot[
      match(probeMatchingIndex, probeAnnot$IlmnID),
      c("CHR", "Infinium_Design_Type")
    ]
    colnames(probeAnnot) <- gsub(
      "Infinium_Design_Type",
      "designType",
      colnames(probeAnnot)
    )
    print("loaded EpicV2 manifest")
    return(probeAnnot)
  }
}
