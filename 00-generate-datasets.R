library(random.cdisc.data)

adsl <- radsl(
  N = 400,
  study_duration = 2,
  seed = 1234,
  with_trt02 = TRUE,
  na_percentage = 0,
  na_vars = list(AGE = NA, SEX = NA, RACE = NA, STRATA1 = NA, STRATA2 = NA, BMRKR1 =
    c(seed = 1234, percentage = 0.1), BMRKR2 = c(1234, 0.1), BEP01FL = NA),
  ae_withdrawal_prob = 0.05,
  cached = FALSE
)

adae <- radae(
  adsl,
  max_n_aes = 10L,
  lookup = NULL,
  lookup_aag = NULL,
  seed = NULL,
  na_percentage = 0,
  na_vars = list(AEBODSYS = c(NA, 0.1), AEDECOD = c(1234, 0.1), AETOXGR = c(1234, 0.1)),
  cached = FALSE
)

advs <- radvs(
  adsl,
  param = c("Diastolic Blood Pressure", "Pulse Rate", "Respiratory Rate",
    "Systolic Blood Pressure", "Temperature", "Weight"),
  paramcd = c("DIABP", "PULSE", "RESP", "SYSBP", "TEMP", "WEIGHT"),
  paramu = c("Pa", "beats/min", "breaths/min", "Pa", "C", "Kg"),
  visit_format = "WEEK",
  n_assessments = 5L,
  n_days = 5L,
  seed = NULL,
  na_percentage = 0,
  na_vars = list(CHG2 = c(1235, 0.1), PCHG2 = c(1235, 0.1), CHG = c(1234, 0.1), PCHG =
    c(1234, 0.1), AVAL = c(123, 0.1), AVALU = c(123, 0.1)),
  cached = FALSE
)

readr::write_rds(adae, "adae.Rds")