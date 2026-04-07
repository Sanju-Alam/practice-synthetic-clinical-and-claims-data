# R/03_create_beneficiary_base.R

library(tidyverse)
library(here)

dir.create(here("data", "processed"), showWarnings = FALSE, recursive = TRUE)

bene <- readr::read_csv(here("data", "interim", "bene_clean.csv"), show_col_types = FALSE)
ip   <- readr::read_csv(here("data", "interim", "ip_clean.csv"), show_col_types = FALSE)
op   <- readr::read_csv(here("data", "interim", "op_clean.csv"), show_col_types = FALSE)
pde  <- readr::read_csv(here("data", "interim", "pde_clean.csv"), show_col_types = FALSE)

# Create simple age approximation if birth year available in date field
extract_year_num <- function(x) {
  suppressWarnings(as.numeric(stringr::str_sub(as.character(x), 1, 4)))
}

bene_base <- bene |>
  mutate(
    birth_year = extract_year_num(bene_birth_dt),
    age_approx = if_else(!is.na(birth_year), file_year - birth_year, NA_real_),
    age_group = case_when(
      is.na(age_approx) ~ "Unknown",
      age_approx < 65 ~ "<65",
      age_approx <= 74 ~ "65-74",
      age_approx <= 84 ~ "75-84",
      TRUE ~ "85+"
    ),
    sex = case_when(
      bene_sex_ident_cd == 1 ~ "Male",
      bene_sex_ident_cd == 2 ~ "Female",
      TRUE ~ "Unknown"
    ),
    race = case_when(
      bene_race_cd == 1 ~ "White",
      bene_race_cd == 2 ~ "Black",
      bene_race_cd == 3 ~ "Other",
      bene_race_cd == 5 ~ "Hispanic",
      TRUE ~ "Unknown"
    ),
    diabetes_flag = case_when(
      sp_diabetes == "Y" ~ 1L,
      sp_diabetes == 1 ~ 1L,
      TRUE ~ 0L
    ),
    chf_flag = case_when(
      sp_chf == "Y" ~ 1L,
      sp_chf == 1 ~ 1L,
      TRUE ~ 0L
    )
  ) |>
  distinct(desynpuf_id, file_year, .keep_all = TRUE)

ip_sum <- ip |>
  group_by(desynpuf_id) |>
  summarise(
    ip_claims = n(),
    ip_payment = sum(clm_pmt_amt, na.rm = TRUE),
    .groups = "drop"
  )

op_sum <- op |>
  group_by(desynpuf_id) |>
  summarise(
    op_claims = n(),
    op_payment = sum(clm_pmt_amt, na.rm = TRUE),
    .groups = "drop"
  )

pde_sum <- pde |>
  group_by(desynpuf_id) |>
  summarise(
    pde_events = n(),
    pde_total_rx_cost = sum(tot_rx_cst_amt, na.rm = TRUE),
    pde_patient_pay = sum(ptnt_pay_amt, na.rm = TRUE),
    .groups = "drop"
  )

bene_analytic <- bene_base |>
  left_join(ip_sum, by = "desynpuf_id") |>
  left_join(op_sum, by = "desynpuf_id") |>
  left_join(pde_sum, by = "desynpuf_id") |>
  mutate(
    across(c(ip_claims, op_claims, pde_events), ~replace_na(., 0)),
    across(c(ip_payment, op_payment, pde_total_rx_cost, pde_patient_pay), ~replace_na(., 0)),
    total_claims = ip_claims + op_claims,
    total_payment = ip_payment + op_payment
  )

readr::write_csv(bene_analytic, here("data", "processed", "beneficiary_analytic.csv"))

message("Saved beneficiary_analytic.csv")
