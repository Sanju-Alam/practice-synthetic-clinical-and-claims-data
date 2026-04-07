# R/01_download_raw_data.R

library(here)

dir.create(here("data"), showWarnings = FALSE)
dir.create(here("data", "raw"), showWarnings = FALSE, recursive = TRUE)

sample_id <- 1

# CMS Sample 1 page exposes these filenames/links.
# The 2010 beneficiary link for sample 1 appears unstable on the current CMS page,
# so this script treats it as optional.

## Sanju: code is not working. downloaded manually
files_to_download <- tibble::tribble(
  ~name,         ~url, ~required,
  "bene_2008",   sprintf("https://www.cms.gov/research-statistics-data-and-systems/downloadable-public-use-files/synpufs/downloads/de1_0_2008_beneficiary_summary_file_sample_%s.zip", sample_id), TRUE,
  "bene_2009",   sprintf("https://www.cms.gov/research-statistics-data-and-systems/downloadable-public-use-files/synpufs/downloads/de1_0_2009_beneficiary_summary_file_sample_%s.zip", sample_id), TRUE,
  "bene_2010",   sprintf("https://www.cms.gov/sites/default/files/2020-09/DE1_0_2010_Beneficiary_Summary_File_Sample_%s.zip", sample_id), FALSE,
  "inpatient",   sprintf("https://www.cms.gov/research-statistics-data-and-systems/downloadable-public-use-files/synpufs/downloads/de1_0_2008_to_2010_inpatient_claims_sample_%s.zip", sample_id), TRUE,
  "outpatient",  sprintf("https://www.cms.gov/research-statistics-data-and-systems/downloadable-public-use-files/synpufs/downloads/de1_0_2008_to_2010_outpatient_claims_sample_%s.zip", sample_id), TRUE,
  "pde",         sprintf("https://downloads.cms.gov/files/DE1_0_2008_to_2010_Prescription_Drug_Events_Sample_%s.zip", sample_id), TRUE
)

download_one <- function(name, url, required = TRUE) {
  out_zip <- here("data", "raw", paste0(name, ".zip"))
  out_dir <- here("data", "raw", name)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  message("Downloading: ", name)
  ok <- tryCatch({
    utils::download.file(url, destfile = out_zip, mode = "wb", quiet = FALSE)
    TRUE
  }, error = function(e) {
    message("Download failed for ", name, ": ", e$message)
    FALSE
  })
  
  if (!ok) {
    if (required) stop("Required file failed: ", name)
    return(invisible(FALSE))
  }
  
  message("Unzipping: ", name)
  tryCatch({
    utils::unzip(out_zip, exdir = out_dir)
    TRUE
  }, error = function(e) {
    if (required) stop("Unzip failed for ", name, ": ", e$message)
    message("Unzip failed for optional file ", name, ": ", e$message)
    FALSE
  })
}

purrr::pwalk(files_to_download, download_one)

message("Finished downloading CMS synthetic sample data.")