# R/04_create_app_datasets.R

library(tidyverse)
library(here)

dir.create(here("data", "appdata"), showWarnings = FALSE, recursive = TRUE)

bene_analytic <- readr::read_csv(here("data", "processed", "beneficiary_analytic.csv"), show_col_types = FALSE)
ip <- readr::read_csv(here("data", "interim", "ip_clean.csv"), show_col_types = FALSE)
op <- readr::read_csv(here("data", "interim", "op_clean.csv"), show_col_types = FALSE)
pde <- readr::read_csv(here("data", "interim", "pde_clean.csv"), show_col_types = FALSE)

kpi_overview <- tibble::tibble(
  metric = c(
    "Beneficiaries",
    "Inpatient claims",
    "Outpatient claims",
    "Prescription drug events",
    "Years in beneficiary files"
  ),
  value = c(
    n_distinct(bene_analytic$desynpuf_id),
    nrow(ip),
    nrow(op),
    nrow(pde),
    paste(sort(unique(bene_analytic$file_year)), collapse = ", ")
  )
)

cohort_summary <- bene_analytic |>
  group_by(file_year, age_group, sex, race) |>
  summarise(
    beneficiaries = n_distinct(desynpuf_id),
    total_claims = sum(total_claims, na.rm = TRUE),
    total_payment = sum(total_payment, na.rm = TRUE),
    mean_payment = mean(total_payment, na.rm = TRUE),
    diabetes_rate = mean(diabetes_flag, na.rm = TRUE),
    .groups = "drop"
  )

utilization_by_type <- bind_rows(
  bene_analytic |>
    transmute(file_year, age_group, sex, race, claim_type = "Inpatient", count = ip_claims),
  bene_analytic |>
    transmute(file_year, age_group, sex, race, claim_type = "Outpatient", count = op_claims),
  bene_analytic |>
    transmute(file_year, age_group, sex, race, claim_type = "PDE", count = pde_events)
) |>
  group_by(file_year, age_group, sex, race, claim_type) |>
  summarise(
    total_events = sum(count, na.rm = TRUE),
    mean_events = mean(count, na.rm = TRUE),
    .groups = "drop"
  )

payment_by_type <- bind_rows(
  bene_analytic |>
    transmute(file_year, age_group, sex, race, claim_type = "Inpatient", payment = ip_payment),
  bene_analytic |>
    transmute(file_year, age_group, sex, race, claim_type = "Outpatient", payment = op_payment),
  bene_analytic |>
    transmute(file_year, age_group, sex, race, claim_type = "PDE", payment = pde_total_rx_cost)
) |>
  group_by(file_year, age_group, sex, race, claim_type) |>
  summarise(
    total_payment = sum(payment, na.rm = TRUE),
    mean_payment = mean(payment, na.rm = TRUE),
    .groups = "drop"
  )

top_dx <- bind_rows(
  ip |>
    transmute(claim_type = "Inpatient", dx_code = icd9_dgns_cd_1),
  op |>
    transmute(claim_type = "Outpatient", dx_code = icd9_dgns_cd_1)
) |>
  filter(!is.na(dx_code), dx_code != "") |>
  count(claim_type, dx_code, sort = TRUE) |>
  group_by(claim_type) |>
  slice_head(n = 20) |>
  ungroup()

# Keep patient drilldown small for GitHub/app
set.seed(123)
sample_ids <- bene_analytic |>
  distinct(desynpuf_id) |>
  {\(df) dplyr::slice_sample(df, n = min(200, nrow(df)))}() |>
  pull(desynpuf_id)

patient_drilldown_sample <- bind_rows(
  ip |>
    filter(desynpuf_id %in% sample_ids) |>
    transmute(
      desynpuf_id,
      event_date = clm_from_dt,
      claim_type = "Inpatient",
      payment_amt = clm_pmt_amt,
      dx_code = icd9_dgns_cd_1
    ),
  op |>
    filter(desynpuf_id %in% sample_ids) |>
    transmute(
      desynpuf_id,
      event_date = clm_from_dt,
      claim_type = "Outpatient",
      payment_amt = clm_pmt_amt,
      dx_code = icd9_dgns_cd_1
    ),
  pde |>
    filter(desynpuf_id %in% sample_ids) |>
    transmute(
      desynpuf_id,
      event_date = srvc_dt,
      claim_type = "PDE",
      payment_amt = tot_rx_cst_amt,
      dx_code = prod_srvc_id
    )
)

readr::write_csv(kpi_overview, here("data", "appdata", "kpi_overview.csv"))
readr::write_csv(cohort_summary, here("data", "appdata", "cohort_summary.csv"))
readr::write_csv(utilization_by_type, here("data", "appdata", "utilization_by_type.csv"))
readr::write_csv(payment_by_type, here("data", "appdata", "payment_by_type.csv"))
readr::write_csv(top_dx, here("data", "appdata", "top_dx.csv"))
readr::write_csv(patient_drilldown_sample, here("data", "appdata", "patient_drilldown_sample.csv"))

message("Saved app-ready CSVs in data/appdata/")