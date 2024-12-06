% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readManifest.r
\name{readManifest}
\alias{readManifest}
\title{Read and Standardise a Manifest File}
\usage{
readManifest(
  referenceDirectory,
  probeMatchingIndex,
  arrayType = c("450K", "V1", "V2")
)
}
\arguments{
\item{referenceDirectory}{This is a string pointing to the directory in
which the manifest files exist in. It is expected to contain:
"450K_reference/AllProbeIlluminaAnno.Rdata"
"EPICArray/EPIC.anno.CRCh38.tsv"
"EPICArray/EPIC-8v2-0_A1.csv"}

\item{probeMatchingIndex}{This is a vector of probe IDs that are present
in your dataset. This could be obtained via extracting the rownames of your
raw beta matrix, or from a gds file that contains probe IDs.}

\item{arrayType}{This is a string with a value from c("450K", "V1", "V2")}
}
\value{
A data frame with varying columns. Certain columns are standardised:
designType - The infinium design type
CHR - The chromosome number in the form 'chrxyz'
}
\description{
Reads in a manifest file that has a very specific file name depending on
the given array type. Important columns are standardised by name and contents
}
\examples{
manifest <- readManifest(
  "path/to/references",
  rownames(beta_matrix),
  "V1"
)
manifest <- readManifest(
  "path/to/references",
  Biobase::fData(gds_file)[["Probe_ID"]],
  "450K"
)
}
