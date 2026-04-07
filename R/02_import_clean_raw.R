# R/02_import_clean_raw.R

library(tidyverse)
library(janitor)
library(here)

dir.create(here("data", "interim"), showWarnings = FALSE, recursive = TRUE)

find_first_csv <- function(folder_name) {
  files <- list.files(
    here("data", "raw", folder_name),
    pattern = "\\.csv$|\\.txt$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  if (length(files) == 0) stop("No CSV/TXT found in folder: ", folder_name)
  files[1]
}

read_clean_csv <- function(path) {
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE) |>
    janitor::clean_names()
}

# Read files
bene_2008_path <- find_first_csv("bene_2008")
bene_2009_path <- find_first_csv("bene_2009")
inpatient_path <- find_first_csv("inpatient")
outpatient_path <- find_first_csv("outpatient")
pde_path <- find_first_csv("pde")

bene_2008 <- read_clean_csv(bene_2008_path) |> mutate(file_year = 2008)
bene_2009 <- read_clean_csv(bene_2009_path) |> mutate(file_year = 2009)

bene_all <- bind_rows(bene_2008, bene_2009)

ip_raw <- read_clean_csv(inpatient_path)
op_raw <- read_clean_csv(outpatient_path)
pde_raw <- read_clean_csv(pde_path)

# Inspect names once if needed:
names(bene_all)
names(ip_raw)
names(op_raw)
names(pde_raw)

# Keep a practical subset of variables if present
keep_if_present <- function(df, cols) {
  df |> select(any_of(cols))
}

bene <- bene_all |>
  keep_if_present(c(
    "desynpuf_id",
    "file_year",
    "bene_birth_dt",
    "bene_death_dt",
    "bene_sex_ident_cd",
    "bene_race_cd",
    "state_code",
    "county_cd",
    "sp_state_code",
    "sp_county_code",
    "bene_hi_cvrage_tot_mons",
    "bene_smi_cvrage_tot_mons",
    "bene_hmo_cvrage_tot_mons",
    "plan_cvrg_mos_num",
    "medreimb_ip",
    "benefit_ip",
    "pta_trmtn_cd",
    "ptb_trmtn_cd",
    "sp_alzhdmta",
    "sp_chf",
    "sp_chrnkidn",
    "sp_cncr",
    "sp_copd",
    "sp_depressn",
    "sp_diabetes",
    "sp_ischmcht",
    "sp_osteoprs",
    "sp_ra_oa",
    "sp_strketia"
  ))

ip <- ip_raw |>
  keep_if_present(c(
    "desynpuf_id",
    "clm_id",
    "clm_from_dt",
    "clm_thru_dt",
    "clm_pmt_amt",
    "prvdr_num",
    "clm_admsn_dt",
    "nch_bene_ip_ddctbl_amt",
    "icd9_dgns_cd_1",
    "icd9_dgns_cd_2",
    "icd9_dgns_cd_3"
  )) |>
  mutate(claim_type = "Inpatient")

op <- op_raw |>
  keep_if_present(c(
    "desynpuf_id",
    "clm_id",
    "clm_from_dt",
    "clm_thru_dt",
    "clm_pmt_amt",
    "prvdr_num",
    "nch_bene_ptb_ddctbl_amt",
    "icd9_dgns_cd_1",
    "icd9_dgns_cd_2",
    "icd9_dgns_cd_3"
  )) |>
  mutate(claim_type = "Outpatient")

pde <- pde_raw |>
  keep_if_present(c(
    "desynpuf_id",
    "pde_id",
    "srvc_dt",
    "prod_srvc_id",
    "qty_dspnsd_num",
    "days_suply_num",
    "ptnt_pay_amt",
    "tot_rx_cst_amt"
  )) |>
  mutate(claim_type = "PDE")

# Save local interim files
readr::write_csv(bene, here("data", "interim", "bene_clean.csv"))
readr::write_csv(ip, here("data", "interim", "ip_clean.csv"))
readr::write_csv(op, here("data", "interim", "op_clean.csv"))
readr::write_csv(pde, here("data", "interim", "pde_clean.csv"))

message("Saved interim cleaned files.")
