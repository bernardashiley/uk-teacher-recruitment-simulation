# =============================================================================
# R/fast_functions.R
#
# Performance-optimised drop-ins for the inner loop of the UK teacher
# recruitment simulation. Source from simulation_project.Rmd AFTER the
# block3-functions chunk so that helper globals are defined.
#
# Validated against original implementations across 50 seeds x 3 policies;
# all 25 summary columns match to within 1e-12 tolerance.
#
# Public functions
# ----------------
#   apply_policy_and_score_fast()      Drop-in for apply_policy_and_score()
#   add_training_and_offers_fast()     Drop-in for add_training_and_offers()
#   assign_training_incentives_fast()  Drop-in for assign_training_incentives()
#   run_one_replication_fast()         Drop-in for run_one_replication()
#   run_simulation_sequential()        Full R-rep run, no parallelism
#   run_simulation_parallel()          Full R-rep run via furrr::future_map_dfr
#
# Required globals (must be in scope when these functions are CALLED)
# -------------------------------------------------------------------
#   inv_logit, sample_cat, assert_prob_vec, calc_school_sponsorship_cost,
#   generate_potential_overseas, probs_overseas_group, probs_subject_overseas,
#   probs_degree_overseas_all, policy_A, policy_B, policy_C, policy_map, params
# =============================================================================


# ---- 1. apply_policy_and_score_fast -----------------------------------------

apply_policy_and_score_fast <- function(
  df_pipeline,
  policy,
  cost_school_sponsorship = NULL,            # legacy, unused
  relocation_subjects = c("Physics", "Modern Languages"),
  u_integration = NULL, u_sponsor = NULL, u_retention = NULL,
  params = params,
  return_df = FALSE                          # TRUE only when caller needs $df
) {
  if (is.null(params)) stop("apply_policy_and_score_fast(): params must be provided.")

  n <- nrow(df_pipeline)
  if (is.null(u_integration)) u_integration <- runif(n)
  if (is.null(u_sponsor))     u_sponsor     <- runif(n)
  if (is.null(u_retention))   u_retention   <- runif(n)
  if (length(u_integration) != n) stop("u_integration length mismatch.")
  if (length(u_sponsor)     != n) stop("u_sponsor length mismatch.")
  if (length(u_retention)   != n) stop("u_retention length mismatch.")

  # Pull columns once (avoid repeated $ dispatch inside the hot path)
  needs_visa  <- df_pipeline$needs_visa
  offered     <- df_pipeline$offered
  qualified   <- df_pipeline$qualified
  subject     <- df_pipeline$subject
  induction   <- df_pipeline$induction_quality
  incentive_v <- df_pipeline$incentive_value

  # Recruitment masks
  sponsored      <- offered & needs_visa & (u_sponsor < policy$prob_sponsor)
  recruited      <- offered & (!needs_visa | sponsored)
  recruited_visa <- recruited & needs_visa

  # Visa upfront cost (multiplication by logical = 0/1, faster than if_else)
  visa_fee_sw  <- if (policy$visa_years_upfront <= 3) policy$visa_fee_sw_leq3 else policy$visa_fee_sw_gt3
  upfront_unit <- visa_fee_sw + policy$ihs_per_year * policy$visa_years_upfront
  upfront_cost_gross <- upfront_unit * recruited_visa

  # Relocation
  if (is.null(relocation_subjects)) {
    relocation_eligible <- recruited & qualified & needs_visa
  } else {
    relocation_eligible <- recruited & qualified & needs_visa &
                           (subject %in% relocation_subjects)
  }
  relocation_paid          <- policy$relocation_support * relocation_eligible
  upfront_cost_net_teacher <- pmax(0, upfront_cost_gross - relocation_paid)

  # Integration course
  integration_course        <- recruited_visa & (u_integration < policy$integration_coverage)
  school_integration_spend  <- params$integration_course_cost_per_trainee * integration_course

  # School sponsorship
  sponsor_unit_cost        <- calc_school_sponsorship_cost(policy, params)
  school_sponsorship_spend <- sponsor_unit_cost * recruited_visa

  # Retention. Values for non-recruited rows are computed but never read
  # downstream (subset by `recruited` in the summary), matching the original.
  base_retention <- pmin(1,
    ifelse(needs_visa,
           policy$retention_baseline_visa    + 0.35 * induction,
           policy$retention_baseline_no_visa + 0.20 * induction))
  integ_boost_vec <- policy$integration_boost_visa * integration_course
  retention_prob  <- pmin(1, base_retention + integ_boost_vec)
  retained_year_1 <- u_retention < retention_prob
  left_year_1     <- recruited & !retained_year_1

  # Spend
  dfe_spend     <- incentive_v + relocation_paid
  school_spend  <- school_sponsorship_spend + school_integration_spend
  teacher_spend <- upfront_cost_net_teacher

  # Waste
  dfe_incentive_waste      <- incentive_v * (incentive_v > 0 & (!recruited | left_year_1))
  dfe_relocation_waste     <- relocation_paid * left_year_1
  school_sponsorship_waste <- sponsor_unit_cost * (left_year_1 & recruited_visa)
  school_integration_waste <- school_integration_spend * left_year_1
  teacher_money_wasted     <- upfront_cost_net_teacher * left_year_1

  dfe_waste     <- dfe_incentive_waste + dfe_relocation_waste
  school_waste  <- school_sponsorship_waste + school_integration_waste
  teacher_waste <- teacher_money_wasted

  # Aggregate without dplyr::summarise
  n_recruited      <- sum(recruited)
  n_recruited_visa <- sum(recruited_visa)
  n_qualified      <- sum(qualified)

  total_dfe_spend     <- sum(dfe_spend)
  total_dfe_waste     <- sum(dfe_waste)
  total_school_spend  <- sum(school_spend)
  total_school_waste  <- sum(school_waste)
  total_teacher_spend <- sum(teacher_spend)
  total_teacher_waste <- sum(teacher_waste)

  summary <- tibble::tibble(
    policy = policy$name,
    n_potential = n,
    n_qualified = n_qualified,
    n_offered   = sum(offered),
    n_recruited = n_recruited,
    n_sponsored = sum(sponsored),
    recruitment_rate = mean(recruited),
    attrition_rate_year1 =
      if (n_recruited > 0) mean(!retained_year_1[recruited]) else NA_real_,
    share_needs_visa_among_recruited =
      if (n_recruited > 0) mean(needs_visa[recruited]) else NA_real_,
    share_integration_among_recruited_needs_visa =
      if (n_recruited_visa > 0) mean(integration_course[recruited_visa]) else NA_real_,
    total_dfe_spend = total_dfe_spend,
    total_dfe_waste = total_dfe_waste,
    total_dfe_waste_visa_only = sum(dfe_waste[needs_visa]),
    total_dfe_waste_nonvisa   = sum(dfe_waste[!needs_visa]),
    total_school_spend = total_school_spend,
    total_school_waste = total_school_waste,
    total_teacher_spend = total_teacher_spend,
    total_teacher_waste = total_teacher_waste,
    total_relocation_spend = sum(relocation_paid),
    relocation_spend_wasted = sum(relocation_paid * left_year_1),
    dfe_waste_rate     = if (total_dfe_spend > 0)     total_dfe_waste / total_dfe_spend     else NA_real_,
    school_waste_rate  = if (total_school_spend > 0)  total_school_waste / total_school_spend  else NA_real_,
    teacher_waste_rate = if (total_teacher_spend > 0) total_teacher_waste / total_teacher_spend else NA_real_,
    recruited_over_capacity  = n_recruited / params$capacity_posts,
    recruited_over_qualified = if (n_qualified > 0) n_recruited / n_qualified else NA_real_
  )

  if (return_df) {
    df_out <- df_pipeline
    df_out$sponsored                <- sponsored
    df_out$recruited                <- recruited
    df_out$retention_prob           <- ifelse(recruited, retention_prob, NA_real_)
    df_out$retained_year_1          <- ifelse(recruited, retained_year_1, NA)
    df_out$left_year_1              <- left_year_1
    df_out$relocation_eligible      <- relocation_eligible
    df_out$relocation_paid          <- relocation_paid
    df_out$upfront_cost_gross       <- upfront_cost_gross
    df_out$upfront_cost_net_teacher <- upfront_cost_net_teacher
    df_out$integration_course       <- integration_course
    df_out$school_integration_spend <- school_integration_spend
    df_out$school_sponsorship_spend <- school_sponsorship_spend
    df_out$dfe_spend                <- dfe_spend
    df_out$school_spend             <- school_spend
    df_out$teacher_spend            <- teacher_spend
    df_out$dfe_waste                <- dfe_waste
    df_out$school_waste             <- school_waste
    df_out$teacher_waste            <- teacher_waste
    list(df = df_out, summary = summary)
  } else {
    list(df = NULL, summary = summary)
  }
}


# ---- 2. add_training_and_offers_fast ----------------------------------------

add_training_and_offers_fast <- function(
  df_potential,
  capacity_posts,
  shortage_subjects = c("Physics", "Modern Languages"),
  u_qual = NULL,
  seed_offer = NULL
) {
  n <- nrow(df_potential)
  if (is.null(u_qual)) u_qual <- runif(n)
  if (length(u_qual) != n) stop("u_qual must have length nrow(df_potential).")

  degree_class <- df_potential$degree_class
  induction    <- df_potential$induction_quality
  has_exc      <- df_potential$has_exceptional_qual
  subject      <- df_potential$subject

  degree_simple <- ifelse(
    degree_class %in% c("First", "Upper Second", "Lower Second"),
    degree_class, "Other"
  )

  beta_degree <- c("First" = 0.55, "Upper Second" = 0.30, "Lower Second" = 0.10, "Other" = 0.00)
  offer_bonus <- c("First" = 0.35, "Upper Second" = 0.20, "Lower Second" = 0.05, "Other" = 0.00)
  deg_eff   <- unname(beta_degree[degree_simple]); deg_eff[is.na(deg_eff)]   <- 0
  deg_bonus <- unname(offer_bonus[degree_simple]); deg_bonus[is.na(deg_bonus)] <- 0

  p_qualified <- inv_logit(-0.25 + 1.10 * induction + 0.50 * as.numeric(has_exc) + deg_eff)
  qualified   <- u_qual < p_qualified

  offer_weight <- 1.0 + ifelse(subject %in% shortage_subjects, 0.8, 0) + deg_bonus

  offer <- logical(n)
  idx   <- which(qualified)
  K     <- min(capacity_posts, length(idx))

  # Preserve original semantics: seed-dance happens whenever seed_offer is not
  # NULL, regardless of whether sample() runs. on.exit guarantees restoration
  # even if sample() errors.
  if (!is.null(seed_offer)) {
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      restore_seed <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", restore_seed, envir = .GlobalEnv), add = TRUE)
    } else {
      on.exit({
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
          rm(".Random.seed", envir = .GlobalEnv)
      }, add = TRUE)
    }
    set.seed(seed_offer)
  }

  if (K > 0) {
    winners <- sample(idx, size = K, replace = FALSE,
                      prob = pmax(offer_weight[idx], 1e-6))
    offer[winners] <- TRUE
  }

  out <- df_potential
  out$degree_simple <- degree_simple
  out$p_qualified   <- p_qualified
  out$qualified     <- qualified
  out$offer_weight  <- offer_weight
  out$offered       <- offer
  out
}


# ---- 3. assign_training_incentives_fast -------------------------------------

assign_training_incentives_fast <- function(df_in, params = params) {
  if (is.null(params)) stop("assign_training_incentives_fast(): params must be provided.")

  subject       <- df_in$subject
  degree_simple <- df_in$degree_simple
  has_exc       <- df_in$has_exceptional_qual
  n             <- length(subject)

  is_target <- subject %in% c("Physics", "Modern Languages")
  is_first  <- degree_simple == "First"
  is_upper  <- degree_simple == "Upper Second"
  is_lower  <- degree_simple == "Lower Second"
  is_top3   <- is_first | is_upper | is_lower
  is_top2   <- is_first | is_upper

  bursary_eligible     <- is_target & is_top3
  scholarship_eligible <- is_target & (is_top2 | (is_lower & has_exc))

  scholarship_weight <- numeric(n)
  scholarship_weight[is_first] <- 3
  scholarship_weight[is_upper] <- 2
  scholarship_weight[is_lower] <- 1
  scholarship_weight <- scholarship_weight + 0.001

  award_idx <- function(subj, p_award) {
    idx <- which(subject == subj & scholarship_eligible)
    if (length(idx) == 0L) return(integer(0))
    K <- rbinom(1L, size = length(idx), prob = p_award)
    if (K <= 0L) return(integer(0))
    sample(idx, size = min(K, length(idx)), replace = FALSE,
           prob = scholarship_weight[idx])
  }
  win_phys <- award_idx("Physics",          params$prob_scholarship_physics)
  win_mfl  <- award_idx("Modern Languages", params$prob_scholarship_languages)

  sch_award <- logical(n)
  sch_award[win_phys] <- TRUE
  sch_award[win_mfl]  <- TRUE

  # Vectorised priority assignment: Scholarship > Bursary > None.
  # (scholarship_eligible is a strict subset of bursary_eligible by construction.)
  incentive_type <- rep("None", n)
  incentive_type[bursary_eligible & !sch_award] <- "Bursary"
  incentive_type[sch_award]                     <- "Scholarship"

  incentive_value <- numeric(n)
  is_phys <- subject == "Physics"
  is_mfl  <- subject == "Modern Languages"
  is_sch  <- incentive_type == "Scholarship"
  is_bur  <- incentive_type == "Bursary"
  incentive_value[is_phys & is_sch] <- params$scholarship_physics
  incentive_value[is_phys & is_bur] <- params$bursary_physics
  incentive_value[is_mfl  & is_sch] <- params$scholarship_languages
  incentive_value[is_mfl  & is_bur] <- params$bursary_languages

  out <- df_in
  out$bursary_eligible     <- bursary_eligible
  out$scholarship_eligible <- scholarship_eligible
  out$scholarship_weight   <- scholarship_weight
  out$scholarship_awarded  <- sch_award
  out$incentive_type       <- incentive_type
  out$incentive_value      <- incentive_value
  out
}


# ---- 4. run_one_replication_fast --------------------------------------------
# Drop-in for the original run_one_replication(). Returns a 3-row tibble
# (one per policy) with rep, derived columns, and policy_map join applied.

run_one_replication_fast <- function(rep_id, N0 = 2000, params = params) {
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
      df_potential   = potential,
      capacity_posts = params$capacity_posts,
      u_qual         = runif(nrow(potential)),
      seed_offer     = 777 + rep_id
    ),
    params = params
  )

  n_pipe            <- nrow(pipeline)
  u_sponsor_all     <- runif(n_pipe)
  u_retention_all   <- runif(n_pipe)
  u_integration_all <- runif(n_pipe)

  res_A <- apply_policy_and_score_fast(
    pipeline, policy_A,
    relocation_subjects = params$relocation_subjects,
    u_integration = u_integration_all, u_sponsor = u_sponsor_all,
    u_retention = u_retention_all, params = params
  )$summary
  res_B <- apply_policy_and_score_fast(
    pipeline, policy_B,
    relocation_subjects = params$relocation_subjects,
    u_integration = u_integration_all, u_sponsor = u_sponsor_all,
    u_retention = u_retention_all, params = params
  )$summary
  res_C <- apply_policy_and_score_fast(
    pipeline, policy_C,
    relocation_subjects = params$relocation_subjects,
    u_integration = u_integration_all, u_sponsor = u_sponsor_all,
    u_retention = u_retention_all, params = params
  )$summary

  out <- dplyr::bind_rows(res_A, res_B, res_C)
  out$rep <- rep_id
  out$total_system_waste <- out$total_dfe_waste + out$total_school_waste + out$total_teacher_waste
  out$retained_year1     <- out$n_recruited * (1 - out$attrition_rate_year1)
  out$waste_per_retained <- out$total_system_waste / pmax(out$retained_year1, 1)
  out <- dplyr::left_join(out, policy_map, by = "policy")
  dplyr::select(out, rep, policy_id, policy, policy_short, dplyr::everything())
}


# ---- 5a. Internal: minimal per-rep body -------------------------------------
# Used by run_simulation_*() so that the per-rep mutate + left_join happens
# ONCE on the full 3R-row result rather than R times.

.run_one_replication_minimal <- function(rep_id, N0, params) {
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
      df_potential   = potential,
      capacity_posts = params$capacity_posts,
      u_qual         = runif(nrow(potential)),
      seed_offer     = 777 + rep_id
    ),
    params = params
  )
  n_pipe <- nrow(pipeline)
  u_sponsor_all     <- runif(n_pipe)
  u_retention_all   <- runif(n_pipe)
  u_integration_all <- runif(n_pipe)

  res_A <- apply_policy_and_score_fast(pipeline, policy_A,
              relocation_subjects = params$relocation_subjects,
              u_integration = u_integration_all, u_sponsor = u_sponsor_all,
              u_retention = u_retention_all, params = params)$summary
  res_B <- apply_policy_and_score_fast(pipeline, policy_B,
              relocation_subjects = params$relocation_subjects,
              u_integration = u_integration_all, u_sponsor = u_sponsor_all,
              u_retention = u_retention_all, params = params)$summary
  res_C <- apply_policy_and_score_fast(pipeline, policy_C,
              relocation_subjects = params$relocation_subjects,
              u_integration = u_integration_all, u_sponsor = u_sponsor_all,
              u_retention = u_retention_all, params = params)$summary

  out <- dplyr::bind_rows(res_A, res_B, res_C)
  out$rep <- rep_id
  out
}


.finalise_run <- function(df) {
  df$total_system_waste <- df$total_dfe_waste + df$total_school_waste + df$total_teacher_waste
  df$retained_year1     <- df$n_recruited * (1 - df$attrition_rate_year1)
  df$waste_per_retained <- df$total_system_waste / pmax(df$retained_year1, 1)
  df <- dplyr::left_join(df, policy_map, by = "policy")
  dplyr::select(df, rep, policy_id, policy, policy_short, dplyr::everything())
}


# ---- 5b. run_simulation_sequential ------------------------------------------

run_simulation_sequential <- function(R, N0 = 2000, params = params,
                                      progress = TRUE) {
  if (progress && requireNamespace("progress", quietly = TRUE)) {
    pb <- progress::progress_bar$new(
      format = "  reps [:bar] :current/:total :percent eta: :eta",
      total = R, clear = FALSE, width = 60
    )
    out <- vector("list", R)
    for (i in seq_len(R)) {
      out[[i]] <- .run_one_replication_minimal(i, N0 = N0, params = params)
      pb$tick()
    }
  } else {
    out <- lapply(seq_len(R), .run_one_replication_minimal, N0 = N0, params = params)
  }
  .finalise_run(dplyr::bind_rows(out))
}


# ---- 5c. run_simulation_parallel --------------------------------------------

run_simulation_parallel <- function(R, N0 = 2000, params = params,
                                    workers = NULL,
                                    plan_strategy = "multisession",
                                    progress = FALSE) {
  if (!requireNamespace("furrr", quietly = TRUE) ||
      !requireNamespace("future", quietly = TRUE)) {
    warning("furrr/future not installed; falling back to sequential.")
    return(run_simulation_sequential(R = R, N0 = N0, params = params,
                                     progress = progress))
  }
  if (is.null(workers)) workers <- max(1L, future::availableCores() - 1L)

  old_plan <- future::plan(plan_strategy, workers = workers)
  on.exit(future::plan(old_plan), add = TRUE)

  furrr_opts <- furrr::furrr_options(seed = TRUE)

  out <- furrr::future_map_dfr(
    seq_len(R),
    function(i) .run_one_replication_minimal(i, N0 = N0, params = params),
    .options = furrr_opts,
    .progress = isTRUE(progress)
  )

  .finalise_run(out)
}
