# =============================================================================
# R/cache_subgroup_strat.R
# Treasury deep dive: stratified per-head waste by subject (Physics vs MFL)
# and visa status (overseas vs domestic), across the three policies.
#
# Output cache: cache/strat_subgroup_R400_v4.csv
# Output object: strat_summary (long, with mean/mcse/CI per cell per metric)
# Runtime estimate: ~30s on 4 cores at R=400
# =============================================================================

stopifnot(exists("apply_policy_and_score_fast"),
          exists("add_training_and_offers_fast"),
          exists("assign_training_incentives_fast"),
          exists("generate_potential_overseas"),
          exists("policy_A"), exists("policy_B"), exists("policy_C"),
          exists("params"))

R_STRAT       <- 400L
N0_STRAT      <- 2000L
SUBJECTS_KEEP <- c("Physics", "Modern Languages")
CACHE_DIR     <- "cache"
CACHE_PATH    <- file.path(CACHE_DIR, "strat_subgroup_R400_v4.csv")
if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)

# ---- Stratified row-level scorer (mirrors apply_policy_and_score_fast) ------
score_rows <- function(df_pipeline, policy,
                       u_integration, u_sponsor, u_retention,
                       relocation_subjects, params) {
  needs_visa  <- df_pipeline$needs_visa
  offered     <- df_pipeline$offered
  qualified   <- df_pipeline$qualified
  subject     <- df_pipeline$subject
  induction   <- df_pipeline$induction_quality
  incentive_v <- df_pipeline$incentive_value
  
  sponsored      <- offered & needs_visa & (u_sponsor < policy$prob_sponsor)
  recruited      <- offered & (!needs_visa | sponsored)
  recruited_visa <- recruited & needs_visa
  
  visa_fee_sw  <- if (policy$visa_years_upfront <= 3) policy$visa_fee_sw_leq3 else policy$visa_fee_sw_gt3
  upfront_unit <- visa_fee_sw + policy$ihs_per_year * policy$visa_years_upfront
  upfront_cost_gross <- upfront_unit * recruited_visa
  
  relocation_eligible <- recruited & qualified & needs_visa &
    (subject %in% relocation_subjects)
  relocation_paid     <- policy$relocation_support * relocation_eligible
  upfront_cost_net    <- pmax(0, upfront_cost_gross - relocation_paid)
  
  integration_course       <- recruited_visa & (u_integration < policy$integration_coverage)
  school_integration_spend <- params$integration_course_cost_per_trainee * integration_course
  sponsor_unit_cost        <- calc_school_sponsorship_cost(policy, params)
  school_sponsorship_spend <- sponsor_unit_cost * recruited_visa
  
  base_retention <- pmin(1,
                         ifelse(needs_visa,
                                policy$retention_baseline_visa    + 0.35 * induction,
                                policy$retention_baseline_no_visa + 0.20 * induction))
  retention_prob  <- pmin(1, base_retention + policy$integration_boost_visa * integration_course)
  retained_year_1 <- u_retention < retention_prob
  left_year_1     <- recruited & !retained_year_1
  
  dfe_spend     <- incentive_v + relocation_paid
  school_spend  <- school_sponsorship_spend + school_integration_spend
  teacher_spend <- upfront_cost_net
  
  dfe_waste     <- incentive_v * (incentive_v > 0 & (!recruited | left_year_1)) +
    relocation_paid * left_year_1
  school_waste  <- sponsor_unit_cost * (left_year_1 & recruited_visa) +
    school_integration_spend * left_year_1
  teacher_waste <- upfront_cost_net * left_year_1
  
  data.frame(
    subject     = subject,
    needs_visa  = needs_visa,
    recruited   = recruited,
    retained    = recruited & retained_year_1,
    dfe_spend, dfe_waste, school_spend, school_waste,
    teacher_spend, teacher_waste,
    stringsAsFactors = FALSE
  )
}

# ---- Per-rep stratified collector -------------------------------------------
one_rep_strat <- function(rep_id, N0, params) {
  potential <- generate_potential_overseas(
    n = N0,
    probs_overseas_group = probs_overseas_group,
    probs_subject_overseas = probs_subject_overseas,
    probs_degree_overseas_all = probs_degree_overseas_all,
    p_eu_has_euss = params$p_eu_has_euss,
    prob_exceptional = params$prob_exceptional
  )
  pipeline <- assign_training_incentives_fast(
    add_training_and_offers_fast(
      df_potential = potential,
      capacity_posts = params$capacity_posts,
      u_qual = runif(nrow(potential)),
      seed_offer = 777 + rep_id
    ),
    params = params
  )
  n_pipe <- nrow(pipeline)
  ui <- runif(n_pipe); us <- runif(n_pipe); ur <- runif(n_pipe)
  
  pols <- list(policy_A, policy_B, policy_C)
  out_list <- vector("list", 3)
  for (k in seq_along(pols)) {
    rows <- score_rows(pipeline, pols[[k]], ui, us, ur,
                       params$relocation_subjects, params)
    rows$policy <- pols[[k]]$name
    rows$rep    <- rep_id
    out_list[[k]] <- rows
  }
  do.call(rbind, out_list)
}

# ---- Run sweep --------------------------------------------------------------
cat(sprintf("Running stratified sweep: R=%d, 3 policies, %d potentials/rep...\n",
            R_STRAT, N0_STRAT))
t0 <- Sys.time()

if (requireNamespace("furrr", quietly = TRUE) &&
    requireNamespace("future", quietly = TRUE)) {
  workers <- max(1L, future::availableCores() - 1L)
  old_plan <- future::plan(future::multisession, workers = workers)
  on.exit(future::plan(old_plan), add = TRUE)
  rep_data <- furrr::future_map_dfr(
    seq_len(R_STRAT),
    function(i) one_rep_strat(i, N0 = N0_STRAT, params = params),
    .options = furrr::furrr_options(seed = TRUE),
    .progress = TRUE
  )
} else {
  warning("furrr unavailable, running sequentially")
  rep_data <- do.call(rbind, lapply(seq_len(R_STRAT),
                                    function(i) one_rep_strat(i, N0 = N0_STRAT, params = params)))
}
cat(sprintf("Rep collection done in %.1fs.\n",
            as.numeric(Sys.time() - t0, units = "secs")))

# ---- Aggregate to (rep, policy, subject, visa_status) -----------------------
rep_data$visa_status <- ifelse(rep_data$needs_visa, "Overseas", "Domestic")
rep_data$subject_grp <- ifelse(rep_data$subject %in% SUBJECTS_KEEP,
                               rep_data$subject, "Other")

cell_per_rep <- rep_data |>
  dplyr::filter(subject_grp %in% SUBJECTS_KEEP) |>
  dplyr::group_by(rep, policy, subject_grp, visa_status) |>
  dplyr::summarise(
    n_recruited   = sum(recruited),
    n_retained    = sum(retained),
    dfe_spend     = sum(dfe_spend),
    dfe_waste     = sum(dfe_waste),
    school_spend  = sum(school_spend),
    school_waste  = sum(school_waste),
    teacher_spend = sum(teacher_spend),
    teacher_waste = sum(teacher_waste),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    system_spend       = dfe_spend + school_spend + teacher_spend,
    system_waste       = dfe_waste + school_waste + teacher_waste,
    waste_per_retained = ifelse(n_retained > 0, system_waste / n_retained, NA_real_),
    dfe_per_retained   = ifelse(n_retained > 0, dfe_waste    / n_retained, NA_real_)
  )

mc_se_local <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))

strat_summary <- cell_per_rep |>
  dplyr::group_by(policy, subject_grp, visa_status) |>
  dplyr::summarise(
    R = dplyr::n(),
    dplyr::across(
      c(n_recruited, n_retained, dfe_waste, system_waste,
        waste_per_retained, dfe_per_retained),
      list(mean = ~mean(.x, na.rm = TRUE),
           mcse = ~mc_se_local(.x)),
      .names = "{.col}__{.fn}"
    ),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(-c(policy, subject_grp, visa_status, R),
                      names_to = c("metric", "stat"),
                      names_sep = "__",
                      values_to = "value") |>
  tidyr::pivot_wider(names_from = stat, values_from = value) |>
  dplyr::mutate(
    ci_low_99    = mean - 2.576 * mcse,
    ci_high_99   = mean + 2.576 * mcse,
    rel_mcse_pct = 100 * mcse / abs(mean)
  )

write.csv(strat_summary, CACHE_PATH, row.names = FALSE)
cat(sprintf("Wrote %s (%d rows).\n", CACHE_PATH, nrow(strat_summary)))

# ---- Sanity print -----------------------------------------------------------
cat("\n--- waste_per_retained by cell ---\n")
strat_summary |>
  dplyr::filter(metric == "waste_per_retained") |>
  dplyr::transmute(policy, subject_grp, visa_status, R,
                   mean = round(mean), mcse = round(mcse, 1),
                   rel_mcse_pct = round(rel_mcse_pct, 2)) |>
  dplyr::arrange(subject_grp, visa_status, policy) |>
  print(n = Inf)

cat("\n--- dfe_waste by cell (the Treasury number) ---\n")
strat_summary |>
  dplyr::filter(metric == "dfe_waste") |>
  dplyr::transmute(policy, subject_grp, visa_status,
                   mean_pounds = round(mean),
                   rel_mcse_pct = round(rel_mcse_pct, 2)) |>
  dplyr::arrange(subject_grp, visa_status, policy) |>
  print(n = Inf)