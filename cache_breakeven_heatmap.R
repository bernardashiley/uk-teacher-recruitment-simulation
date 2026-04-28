# =============================================================================
# R/cache_breakeven_heatmap.R
# 2D sensitivity heatmap for the C - A waste-per-retained contrast over a
# (retention_baseline_visa, integration_boost_visa) grid.
#
# Grid: 19 retention levels x 13 integration boost levels = 247 cells
# Per cell: R_CELL = 500 paired CRN reps (A and C share random numbers)
# Total reps: 247 x 500 x 2 policies = 247,000 inner replications
# Runtime estimate: ~6-10 minutes on 4 cores
#
# Output cache: cache/breakeven_heatmap_v4.csv
# Output object: heat_summary
# =============================================================================

stopifnot(exists("apply_policy_and_score_fast"),
          exists("add_training_and_offers_fast"),
          exists("assign_training_incentives_fast"),
          exists("generate_potential_overseas"),
          exists("policy_A"), exists("policy_C"),
          exists("params"))

R_CELL    <- 500L
N0_CELL   <- 2000L
RV_GRID   <- seq(0.50, 0.95, by = 0.025)        # retention_baseline_visa
IB_GRID   <- seq(0.00, 0.15, by = 0.0125)       # integration_boost_visa
CACHE_DIR <- "cache"
CACHE_PATH <- file.path(CACHE_DIR, "breakeven_heatmap_v4.csv")
if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)

# ---- Cell scorer: paired CRN run for A and C at one (rv, ib) point ----------
score_cell <- function(rv, ib, R_cell, N0, params0,
                       policy_A0, policy_C0) {
  pA <- policy_A0; pA$retention_baseline_visa <- rv
  pC <- policy_C0; pC$retention_baseline_visa <- rv
  pC$integration_boost_visa <- ib
  
  rel_subj <- params0$relocation_subjects
  diffs_wpr <- numeric(R_cell)
  diffs_sys <- numeric(R_cell)
  c_wpr     <- numeric(R_cell)
  a_wpr     <- numeric(R_cell)
  
  for (i in seq_len(R_cell)) {
    potential <- generate_potential_overseas(
      n = N0,
      probs_overseas_group = probs_overseas_group,
      probs_subject_overseas = probs_subject_overseas,
      probs_degree_overseas_all = probs_degree_overseas_all,
      p_eu_has_euss = params0$p_eu_has_euss,
      prob_exceptional = params0$prob_exceptional
    )
    pipeline <- assign_training_incentives_fast(
      add_training_and_offers_fast(
        df_potential = potential,
        capacity_posts = params0$capacity_posts,
        u_qual = runif(nrow(potential)),
        seed_offer = 777 + i
      ),
      params = params0
    )
    n_pipe <- nrow(pipeline)
    ui <- runif(n_pipe); us <- runif(n_pipe); ur <- runif(n_pipe)
    
    sA <- apply_policy_and_score_fast(pipeline, pA,
                                      relocation_subjects = rel_subj,
                                      u_integration = ui, u_sponsor = us, u_retention = ur,
                                      params = params0)$summary
    sC <- apply_policy_and_score_fast(pipeline, pC,
                                      relocation_subjects = rel_subj,
                                      u_integration = ui, u_sponsor = us, u_retention = ur,
                                      params = params0)$summary
    
    sysA <- sA$total_dfe_waste + sA$total_school_waste + sA$total_teacher_waste
    sysC <- sC$total_dfe_waste + sC$total_school_waste + sC$total_teacher_waste
    retA <- sA$n_recruited * (1 - sA$attrition_rate_year1)
    retC <- sC$n_recruited * (1 - sC$attrition_rate_year1)
    
    a_wpr[i] <- sysA / max(retA, 1)
    c_wpr[i] <- sysC / max(retC, 1)
    diffs_wpr[i] <- c_wpr[i] - a_wpr[i]
    diffs_sys[i] <- sysC - sysA
  }
  
  data.frame(
    rv = rv, ib = ib, R = R_cell,
    mean_wpr_A     = mean(a_wpr),
    mean_wpr_C     = mean(c_wpr),
    mean_diff_wpr  = mean(diffs_wpr),
    mcse_diff_wpr  = sd(diffs_wpr) / sqrt(R_cell),
    mean_diff_sys  = mean(diffs_sys),
    mcse_diff_sys  = sd(diffs_sys) / sqrt(R_cell)
  )
}

# ---- Build grid and run -----------------------------------------------------
grid <- expand.grid(rv = RV_GRID, ib = IB_GRID,
                    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
cat(sprintf("Heatmap sweep: %d cells x %d reps each = %d total inner reps.\n",
            nrow(grid), R_CELL, nrow(grid) * R_CELL))
t0 <- Sys.time()

if (requireNamespace("furrr", quietly = TRUE) &&
    requireNamespace("future", quietly = TRUE)) {
  workers <- max(1L, future::availableCores() - 1L)
  old_plan <- future::plan(future::multisession, workers = workers)
  on.exit(future::plan(old_plan), add = TRUE)
  heat_summary <- furrr::future_pmap_dfr(
    grid,
    function(rv, ib) score_cell(rv, ib, R_CELL, N0_CELL, params,
                                policy_A, policy_C),
    .options = furrr::furrr_options(seed = TRUE),
    .progress = TRUE
  )
} else {
  warning("furrr unavailable, running sequentially")
  heat_summary <- do.call(rbind, Map(function(rv, ib) {
    score_cell(rv, ib, R_CELL, N0_CELL, params, policy_A, policy_C)
  }, grid$rv, grid$ib))
}

heat_summary <- dplyr::mutate(heat_summary,
                              ci_low_99_wpr  = mean_diff_wpr - 2.576 * mcse_diff_wpr,
                              ci_high_99_wpr = mean_diff_wpr + 2.576 * mcse_diff_wpr,
                              ci_low_99_sys  = mean_diff_sys - 2.576 * mcse_diff_sys,
                              ci_high_99_sys = mean_diff_sys + 2.576 * mcse_diff_sys,
                              c_better_99    = ci_high_99_wpr < 0,
                              c_worse_99     = ci_low_99_wpr  > 0,
                              uncertain_99   = !c_better_99 & !c_worse_99
)

write.csv(heat_summary, CACHE_PATH, row.names = FALSE)

cat(sprintf("\nDone in %.1f min. Wrote %s (%d cells).\n",
            as.numeric(Sys.time() - t0, units = "mins"),
            CACHE_PATH, nrow(heat_summary)))

cat("\n--- Heatmap summary ---\n")
cat(sprintf("Cells where C is reliably BETTER than A (99%%):  %d / %d\n",
            sum(heat_summary$c_better_99), nrow(heat_summary)))
cat(sprintf("Cells where C is reliably WORSE than A (99%%):   %d / %d\n",
            sum(heat_summary$c_worse_99), nrow(heat_summary)))
cat(sprintf("Cells where 99%% CI crosses zero (uncertain):    %d / %d\n",
            sum(heat_summary$uncertain_99), nrow(heat_summary)))

cat("\n--- Worst-case retention (rv = min) row ---\n")
heat_summary |>
  dplyr::filter(rv == min(RV_GRID)) |>
  dplyr::transmute(rv, ib,
                   diff_wpr = round(mean_diff_wpr),
                   ci_low = round(ci_low_99_wpr),
                   ci_high = round(ci_high_99_wpr),
                   verdict = ifelse(c_better_99, "C better",
                                    ifelse(c_worse_99, "C worse", "uncertain"))) |>
  print(n = Inf)