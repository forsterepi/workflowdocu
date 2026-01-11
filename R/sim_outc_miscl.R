# Legend ------------------------------------------------------------------
# Simulation experiment of adjustment for outcome misclassification with single
# binary predictor


# Packages ----------------------------------------------------------------

library(simcausal) # Simulation module
library(simChef) # Conducting simulation experiment


# Load simulation module --------------------------------------------------

sim_mod <- DAG.empty() %>%
  add.nodes(
    node("x",
      distr = "rbern",
      prob = pX
    )
  ) %>%
  add.nodes(
    node("pTrue",
      distr = "rconst",
      const = plogis(
        qlogis(pAlpha) +
          log(ORx) * x
      )
    )
  ) %>%
  add.nodes(
    node("yTrue",
      distr = "rbern",
      prob = pTrue
    )
  ) %>%
  add.nodes(
    node("yObs",
      distr = "rbern",
      prob = ifelse(yTrue == 1,
        se,
        1 - sp
      )
    )
  )

# Prepare experiment ------------------------------------------------------

## Create functions ----

# Data-generating process (dgp)
sim_dgp <- function(n, pX, pAlpha, ORx) {
  # Draw a single value for sensitivity and specificity
  se <- rbeta(1, 221.6, 85.61) %>% round(3)
  sp <- rbeta(1, 1037, 182.1) %>% round(3)

  dags <- list(sim_mod)
  args <- list(
    pX = pX,
    pAlpha = pAlpha,
    ORx = ORx,
    se = se,
    sp = sp
  )

  df <- run_sim(
    dags = dags,
    args = args,
    n = n
  )

  out <- list(
    df = df
  )

  return(out)
}

# Analysis functions, i.e., method functions
sim_method_corrected <- function(df) {
  # Load Stan model that corrects and has weakly informative priors for a and b
  model <- cmdstan_model("Stan/outc_miscl.stan")

  dat <- list(
    N = nrow(df),
    y_obs = df$yObs,
    x = df$x
  )

  fit <- model$sample(data = dat, refresh = 0, show_messages = FALSE)
  fit$sampler_diagnostics()

  out <- fit$summary(
    variables = c("a", "b", "oddsratio"), median, quantile,
    .args = list(probs = c(0.025, .975), names = FALSE)
  )

  out <- list(
    point_a = out$median[out$variable == "a"],
    point_b = out$median[out$variable == "b"],
    point_or = out$median[out$variable == "oddsratio"],
    lower_a = out$quantile.1[out$variable == "a"],
    lower_b = out$quantile.1[out$variable == "b"],
    lower_or = out$quantile.1[out$variable == "oddsratio"],
    upper_a = out$quantile.2[out$variable == "a"],
    upper_b = out$quantile.2[out$variable == "b"],
    upper_or = out$quantile.2[out$variable == "oddsratio"]
  )

  return(out)
}

sim_method_corrected_priors <- function(df) {
  # Load Stan model that corrects and has informative priors for a and b
  model <- cmdstan_model("Stan/outc_miscl_2.stan")

  dat <- list(
    N = nrow(df),
    y_obs = df$yObs,
    x = df$x
  )

  fit <- model$sample(data = dat, refresh = 0, show_messages = FALSE)
  fit$sampler_diagnostics()

  out <- fit$summary(
    variables = c("a", "b", "oddsratio"), median, quantile,
    .args = list(probs = c(0.025, .975), names = FALSE)
  )

  out <- list(
    point_a = out$median[out$variable == "a"],
    point_b = out$median[out$variable == "b"],
    point_or = out$median[out$variable == "oddsratio"],
    lower_a = out$quantile.1[out$variable == "a"],
    lower_b = out$quantile.1[out$variable == "b"],
    lower_or = out$quantile.1[out$variable == "oddsratio"],
    upper_a = out$quantile.2[out$variable == "a"],
    upper_b = out$quantile.2[out$variable == "b"],
    upper_or = out$quantile.2[out$variable == "oddsratio"]
  )

  return(out)
}

sim_method_naive <- function(df) {
  # Load Stan model that does not correct
  model <- cmdstan_model("Stan/outc_miscl_3.stan")

  dat <- list(
    N = nrow(df),
    y = df$yObs,
    x = df$x
  )

  fit <- model$sample(data = dat, refresh = 0, show_messages = FALSE)
  fit$sampler_diagnostics()

  out <- fit$summary(
    variables = c("a", "b", "oddsratio"), median, quantile,
    .args = list(probs = c(0.025, .975), names = FALSE)
  )

  out <- list(
    point_a = out$median[out$variable == "a"],
    point_b = out$median[out$variable == "b"],
    point_or = out$median[out$variable == "oddsratio"],
    lower_a = out$quantile.1[out$variable == "a"],
    lower_b = out$quantile.1[out$variable == "b"],
    lower_or = out$quantile.1[out$variable == "oddsratio"],
    upper_a = out$quantile.2[out$variable == "a"],
    upper_b = out$quantile.2[out$variable == "b"],
    upper_or = out$quantile.2[out$variable == "oddsratio"]
  )

  return(out)
}

sim_method_true <- function(df) {
  # Load Stan model that does not correct
  model <- cmdstan_model("Stan/outc_miscl_3.stan")

  dat <- list(
    N = nrow(df),
    y = df$yTrue,
    x = df$x
  )

  fit <- model$sample(data = dat, refresh = 0, show_messages = FALSE)
  fit$sampler_diagnostics()

  out <- fit$summary(
    variables = c("a", "b", "oddsratio"), median, quantile,
    .args = list(probs = c(0.025, .975), names = FALSE)
  )

  out <- list(
    point_a = out$median[out$variable == "a"],
    point_b = out$median[out$variable == "b"],
    point_or = out$median[out$variable == "oddsratio"],
    lower_a = out$quantile.1[out$variable == "a"],
    lower_b = out$quantile.1[out$variable == "b"],
    lower_or = out$quantile.1[out$variable == "oddsratio"],
    upper_a = out$quantile.2[out$variable == "a"],
    upper_b = out$quantile.2[out$variable == "b"],
    upper_or = out$quantile.2[out$variable == "oddsratio"]
  )

  return(out)
}

sim_method_mle <- function(df) {
  nll <- function(pars, data, se, sp) {
    # Parameters
    a <- pars[1]
    b <- pars[2]

    # Calculate p
    p <- (1 - sp) + (se + sp - 1) * plogis(a + b * data$x)

    # Calculate likelihood
    ll <- data$yObs * log(p) + (1 - data$yObs) * log(1 - p)

    # Return
    -sum(ll)
  }

  # We need to provide the point estimates for se and sp
  mle <- optim(
    par = c(a = -3, b = 0), fn = nll,
    data = df, se = 0.72, sp = 0.85,
    hessian = TRUE
  )
  std_error <- rlang::try_fetch( 
    # Use try in case that standard errors cannot be calculated
    {
      mle$hessian %>%
        solve() %>%
        diag() %>%
        sqrt()
    },
    error = function(cnd) {
      NA
    }
  )

  if (std_error %>% is.na() %>% all()) {
    out <- list(
      point_a = NA,
      point_b = NA,
      point_or = NA,
      lower_a = NA,
      lower_b = NA,
      lower_or = NA,
      upper_a = NA,
      upper_b = NA,
      upper_or = NA
    )
  } else {
    out <- list(
      point_a = mle$par["a"],
      point_b = mle$par["b"],
      point_or = exp(mle$par["b"]),
      lower_a = mle$par["a"] - 1.96 * std_error["a"],
      lower_b = mle$par["b"] - 1.96 * std_error["b"],
      lower_or = exp(mle$par["b"] - 1.96 * std_error["b"]),
      upper_a = mle$par["a"] + 1.96 * std_error["a"],
      upper_b = mle$par["b"] + 1.96 * std_error["b"],
      upper_or = exp(mle$par["b"] + 1.96 * std_error["b"])
    )
  }

  return(out)
}

# Extract interesting measures that vary across simulation iterations
sim_method_real <- function(df) {
  out <- list(
    p_yObs_x1 = mean(df$yObs[df$x == 1]),
    p_yObs_x0 = mean(df$yObs[df$x == 0]),
    p_yTrue_x1 = mean(df$yTrue[df$x == 1]),
    p_yTrue_x0 = mean(df$yTrue[df$x == 0]),
    a_true = qlogis(mean(df$yTrue[df$x == 0])),
    b_true = qlogis(mean(df$yTrue[df$x == 1])) - qlogis(mean(df$yTrue[df$x == 0])),
    or_true = exp(qlogis(mean(df$yTrue[df$x == 1])) - qlogis(mean(df$yTrue[df$x == 0]))),
    or_obs = (sum(df$yObs[df$x == 1] == 1) / sum(df$yObs[df$x == 1] == 0)) /
      (sum(df$yObs[df$x == 0] == 1) / sum(df$yObs[df$x == 0] == 0)),
    se_true = mean(df$yObs[df$yTrue == 1] == 1),
    sp_true = mean(df$yObs[df$yTrue == 0] == 0)
  )

  return(out)
}

# Evaluation function
sim_eval <- function(fit_results) {
  nrep <- fit_results$.rep %>%
    as.numeric() %>%
    max()

  ors <- c(0.8, 1.0, 1.2)
  real_vars <- c(
    "p_yObs_x1", "p_yObs_x0", "p_yTrue_x1", "p_yTrue_x0", "a_true", "b_true",
    "or_true", "or_obs", "se_true", "sp_true"
  )
  non_real_meth <- c("corr", "corr_priors", "mle", "naive", "true")

  for (i in 1:nrep) {
    for (j in seq_along(ors)) {
      for (k in seq_along(real_vars)) {
        fit_results[
          fit_results$.rep == as.character(i) &
            fit_results$ORx == ors[j] &
            fit_results$.method_name %in% non_real_meth,
          real_vars[k]
        ] <- fit_results[
          fit_results$.rep == as.character(i) &
            fit_results$ORx == ors[j] &
            fit_results$.method_name == "real",
          real_vars[k]
        ]
      }
    }
  }

  fit_results %<>% filter(.method_name != "real")

  fit_results %<>%
    mutate(
      contains_a_true = case_when(
        lower_a <= a_true & upper_a >= a_true ~ TRUE,
        lower_a > a_true ~ FALSE,
        upper_a < a_true ~ FALSE
      ),
      contains_b_true = case_when(
        lower_b <= b_true & upper_b >= b_true ~ TRUE,
        lower_b > b_true ~ FALSE,
        upper_b < b_true ~ FALSE
      ),
      contains_or_true = case_when(
        lower_or <= or_true & upper_or >= or_true ~ TRUE,
        lower_or > or_true ~ FALSE,
        upper_or < or_true ~ FALSE
      ),
      contains_or_obs = case_when(
        lower_or <= or_obs & upper_or >= or_obs ~ TRUE,
        lower_or > or_obs ~ FALSE,
        upper_or < or_obs ~ FALSE
      ),
      diff_a = point_a - a_true,
      diff_b = point_b - b_true,
      diff_or = point_or - or_true
    )

  return(fit_results)
}


## Create experiment ----

dgp <- create_dgp(
  .dgp_fun = sim_dgp,
  .name = "dgp_fixed"
)

meth1 <- create_method(
  .method_fun = sim_method_corrected,
  .name = "corr"
)

meth2 <- create_method(
  .method_fun = sim_method_corrected_priors,
  .name = "corr_priors"
)

meth3 <- create_method(
  .method_fun = sim_method_naive,
  .name = "naive"
)

meth4 <- create_method(
  .method_fun = sim_method_true,
  .name = "true"
)

meth5 <- create_method(
  .method_fun = sim_method_mle,
  .name = "mle"
)

meth6 <- create_method(
  .method_fun = sim_method_real,
  .name = "real"
)

ev <- create_evaluator(
  .eval_fun = sim_eval,
  .name = "eval"
)

# Define future plan in order to parallelise
sim_exp <- create_experiment(
  name = "Outcome misclassification",
  future.packages = c("supfuns", "cmdstanr", "rlang"),
  future.globals = "sim_mod"
) %>%
  add_dgp(dgp) %>%
  add_method(meth1) %>%
  add_method(meth2) %>%
  add_method(meth3) %>%
  add_method(meth4) %>%
  add_method(meth5) %>%
  add_method(meth6) %>%
  add_evaluator(ev) %>%
  add_vary_across(
    .dgp = dgp,
    n = list(1000),
    pX = list(0.5),
    pAlpha = list(0.05),
    ORx = list(0.8, 1, 1.2)
  )


## Clean
rm(
  sim_dgp, sim_method_corrected, sim_method_corrected_priors,
  sim_method_naive, sim_method_true, sim_method_mle, sim_method_real,
  sim_eval, dgp, meth1, meth2, meth3, meth4, meth5, meth6, ev
)


# Run experiment ----------------------------------------------------------

if (cache$exists("sim_result")) {
  sim_result <- cache$get("sim_result")
} else {
  sim_result <- sim_exp %>%
    run_experiment(n_reps = 50, save = FALSE)
  cache$set("sim_result", sim_result)
}
rm(sim_exp)


# Visualise ---------------------------------------------------------------

## Prepare data for plotting
df_sim_results <- sim_result$eval_results$eval %>%
  rowwise() %>%
  mutate(
    lower_or_adj = max(0, lower_or),
    upper_or_adj = min(4, upper_or),
    lower_or_star = lower_or < lower_or_adj,
    upper_or_star = upper_or > upper_or_adj,
    meth = factor(.method_name,
      levels = c("true", "naive", "corr", "corr_priors", "mle"),
      labels = c("True", "Naive", "Corrected", "Corrected (priors)", "MLE with optim()")
    ),
    meth_letter = case_when(
      .method_name == "true" ~ "T",
      .method_name == "naive" ~ "N",
      .method_name == "corr" ~ "C",
      .method_name == "corr_priors" ~ "P",
      .method_name == "mle" ~ "M"
    ),
    precision_or = upper_or - lower_or,
    diff_or_obs = or_obs - or_true,
    or_true_sign = case_when(
      or_true > 1 ~ ">1",
      or_true == 1 ~ "=1",
      or_true < 1 ~ "<1"
    ),
    diff_good = case_when(
      abs(diff_or_obs) > abs(diff_or) ~ "Better",
      abs(diff_or_obs) <= abs(diff_or) ~ "Worse"
    )
  )

## point estimates ----

sim_point_est_diff <- df_sim_results %>%
  ggplot(aes(x = diff_or)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0)) +
  xlim(-4, 4) +
  theme_bw() +
  labs(x = "Difference of point estimate to true OR", y = "Density") +
  facet_wrap(~meth, scales = "free")

sim_point_est_var <- df_sim_results %>%
  ggplot(aes(x = or_true, y = point_or, color = meth, label = meth_letter)) +
  geom_abline() +
  geom_point(size = 1) +
  theme_bw() +
  theme(legend.position = "bottom") +
  ylim(0, 8) +
  labs(x = "True OR", y = "Estimated OR", color = "Method") +
  facet_wrap(~meth)

## precision ----

sim_precision <- df_sim_results %>%
  ggplot(aes(x = or_true, y = p_yTrue_x0, color = precision_or)) +
  geom_point() +
  scale_color_distiller(
    palette = "Spectral",
    transform = "reciprocal", breaks = c(0.5, 1, 2, 99)
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap(~meth) +
  labs(x = "True OR", y = "p(yTrue) for X = 0", color = "Range of CI")

## coverage (x = true OR) ----

sim_coverage <- df_sim_results %>%
  ggplot() +
  geom_abline(slope = 1, intercept = 0) +
  geom_linerange(aes(
    x = or_true,
    ymin = lower_or_adj,
    ymax = upper_or_adj,
    color = contains_or_true
  ), linewidth = 0.2) +
  geom_point(aes(
    x = or_true,
    y = point_or,
    color = contains_or_true
  ), size = 1) +
  geom_rug(aes(x = or_true), linewidth = 0.2) +
  geom_point(
    data = filter(df_sim_results, upper_or_star == TRUE),
    mapping = aes(
      x = or_true,
      y = upper_or_adj,
      fill = contains_or_true,
      color = contains_or_true
    ), shape = "triangle filled", size = 1
  ) +
  geom_point(
    data = filter(df_sim_results, lower_or_star == TRUE),
    mapping = aes(
      x = or_true,
      y = lower_or_adj,
      fill = contains_or_true,
      color = contains_or_true
    ), shape = "triangle down filled", size = 1
  ) +
  scale_color_manual(values = c("FALSE" = "#d73027", "TRUE" = "#91cf60")) +
  scale_fill_manual(values = c("FALSE" = "#d73027", "TRUE" = "#91cf60")) +
  guides(fill = "none") +
  theme_bw() +
  ylim(0, 4) +
  facet_wrap(~meth) +
  theme(legend.position = "bottom") +
  labs(x = "True OR", y = "Estimated OR", color = "95%-CI contains true value?")

## coverage (x = replication) ----

### true
df_sim_results_cov_true <- df_sim_results %>%
  filter(.method_name == "true") %>%
  arrange(or_true) %>%
  rownames_to_column("x") %>%
  mutate(x = as.numeric(x))

sim_cov_true <- df_sim_results_cov_true %>%
  ggplot() +
  geom_linerange(aes(
    x = x, ymin = lower_or_adj, ymax = upper_or_adj,
    color = contains_or_true
  ), linewidth = 0.2) +
  geom_point(aes(x = x, y = point_or, color = contains_or_true), size = 1) +
  geom_point(
    data = filter(df_sim_results_cov_true, upper_or_star == TRUE),
    mapping = aes(
      x = x,
      y = upper_or_adj,
      fill = contains_or_true,
      color = contains_or_true
    ), shape = "triangle filled", size = 1, show.legend = FALSE
  ) +
  geom_point(
    data = filter(df_sim_results_cov_true, lower_or_star == TRUE),
    mapping = aes(
      x = x,
      y = lower_or_adj,
      fill = contains_or_true,
      color = contains_or_true
    ), shape = "triangle down filled", size = 1, show.legend = FALSE
  ) +
  scale_color_manual(values = c("FALSE" = "#d73027", "TRUE" = "#91cf60")) +
  geom_point(aes(x = x, y = or_true), shape = 4, size = 1) +
  geom_point(aes(x = x, y = or_obs), shape = 5, size = 1, color = "#d73027") +
  guides(color = "none") +
  theme_bw() +
  labs(
    x = "Replication",
    y = "OR",
    color = "95%-CI contains true value?",
    title = "True"
  ) +
  ylim(0, 4)

rm(df_sim_results_cov_true)

### naive
df_sim_results_cov_naive <- df_sim_results %>%
  filter(.method_name == "naive") %>%
  arrange(or_true) %>%
  rownames_to_column("x") %>%
  mutate(x = as.numeric(x))

sim_cov_naive <- df_sim_results_cov_naive %>%
  ggplot() +
  geom_linerange(aes(
    x = x, ymin = lower_or_adj, ymax = upper_or_adj,
    color = contains_or_true
  ), linewidth = 0.2) +
  geom_point(aes(x = x, y = point_or, color = contains_or_true), size = 1) +
  geom_point(
    data = filter(df_sim_results_cov_naive, upper_or_star == TRUE),
    mapping = aes(
      x = x,
      y = upper_or_adj,
      fill = contains_or_true,
      color = contains_or_true
    ), shape = "triangle filled", size = 1, show.legend = FALSE
  ) +
  geom_point(
    data = filter(df_sim_results_cov_naive, lower_or_star == TRUE),
    mapping = aes(
      x = x,
      y = lower_or_adj,
      fill = contains_or_true,
      color = contains_or_true
    ), shape = "triangle down filled", size = 1, show.legend = FALSE
  ) +
  scale_color_manual(values = c("FALSE" = "#d73027", "TRUE" = "#91cf60")) +
  geom_point(aes(x = x, y = or_true), shape = 4, size = 1) +
  geom_point(aes(x = x, y = or_obs), shape = 5, size = 1, color = "#d73027") +
  theme_bw() +
  labs(
    x = "Replication",
    y = "OR",
    color = "95%-CI contains true value?",
    title = "Naive"
  ) +
  ylim(0, 4)

rm(df_sim_results_cov_naive)

### corr
df_sim_results_cov_corr <- df_sim_results %>%
  filter(.method_name == "corr") %>%
  arrange(or_true) %>%
  rownames_to_column("x") %>%
  mutate(x = as.numeric(x))

sim_cov_corr <- df_sim_results_cov_corr %>%
  ggplot() +
  geom_linerange(aes(
    x = x, ymin = lower_or_adj, ymax = upper_or_adj,
    color = contains_or_true
  ), linewidth = 0.2) +
  geom_point(aes(x = x, y = point_or, color = contains_or_true), size = 1) +
  geom_point(
    data = filter(df_sim_results_cov_corr, upper_or_star == TRUE),
    mapping = aes(
      x = x,
      y = upper_or_adj,
      fill = contains_or_true,
      color = contains_or_true
    ), shape = "triangle filled", size = 1, show.legend = FALSE
  ) +
  geom_point(
    data = filter(df_sim_results_cov_corr, lower_or_star == TRUE),
    mapping = aes(
      x = x,
      y = lower_or_adj,
      fill = contains_or_true,
      color = contains_or_true
    ), shape = "triangle down filled", size = 1, show.legend = FALSE
  ) +
  scale_color_manual(values = c("FALSE" = "#d73027", "TRUE" = "#91cf60")) +
  geom_point(aes(x = x, y = or_true), shape = 4, size = 1) +
  geom_point(aes(x = x, y = or_obs), shape = 5, size = 1, color = "#d73027") +
  guides(color = "none") +
  theme_bw() +
  labs(
    x = "Replication",
    y = "OR",
    color = "95%-CI contains true value?",
    title = "Corrected"
  ) +
  ylim(0, 4)

rm(df_sim_results_cov_corr)

### corr_priors
df_sim_results_cov_corr_priors <- df_sim_results %>%
  filter(.method_name == "corr_priors") %>%
  arrange(or_true) %>%
  rownames_to_column("x") %>%
  mutate(x = as.numeric(x))

sim_cov_corr_priors <- df_sim_results_cov_corr_priors %>%
  ggplot() +
  geom_linerange(aes(
    x = x, ymin = lower_or_adj, ymax = upper_or_adj,
    color = contains_or_true
  ), linewidth = 0.2) +
  geom_point(aes(x = x, y = point_or, color = contains_or_true), size = 1) +
  geom_point(
    data = filter(df_sim_results_cov_corr_priors, upper_or_star == TRUE),
    mapping = aes(
      x = x,
      y = upper_or_adj,
      fill = contains_or_true,
      color = contains_or_true
    ), shape = "triangle filled", size = 1, show.legend = FALSE
  ) +
  geom_point(
    data = filter(df_sim_results_cov_corr_priors, lower_or_star == TRUE),
    mapping = aes(
      x = x,
      y = lower_or_adj,
      fill = contains_or_true,
      color = contains_or_true
    ), shape = "triangle down filled", size = 1, show.legend = FALSE
  ) +
  scale_color_manual(values = c("FALSE" = "#d73027", "TRUE" = "#91cf60")) +
  geom_point(aes(x = x, y = or_true), shape = 4, size = 1) +
  geom_point(aes(x = x, y = or_obs), shape = 5, size = 1, color = "#d73027") +
  guides(color = "none") +
  theme_bw() +
  labs(
    x = "Replication",
    y = "OR",
    color = "95%-CI contains true value?",
    title = "Corrected (priors)"
  ) +
  ylim(0, 4)

rm(df_sim_results_cov_corr_priors)

### mle
df_sim_results_cov_mle <- df_sim_results %>%
  filter(.method_name == "mle") %>%
  arrange(or_true) %>%
  rownames_to_column("x") %>%
  mutate(x = as.numeric(x))

sim_cov_mle <- df_sim_results_cov_mle %>%
  ggplot() +
  geom_linerange(aes(
    x = x, ymin = lower_or_adj, ymax = upper_or_adj,
    color = contains_or_true
  ), linewidth = 0.2) +
  geom_point(aes(x = x, y = point_or, color = contains_or_true), size = 1) +
  geom_point(
    data = filter(df_sim_results_cov_mle, upper_or_star == TRUE),
    mapping = aes(
      x = x,
      y = upper_or_adj,
      fill = contains_or_true,
      color = contains_or_true
    ), shape = "triangle filled", size = 1, show.legend = FALSE
  ) +
  geom_point(
    data = filter(df_sim_results_cov_mle, lower_or_star == TRUE),
    mapping = aes(
      x = x,
      y = lower_or_adj,
      fill = contains_or_true,
      color = contains_or_true
    ), shape = "triangle down filled", size = 1, show.legend = FALSE
  ) +
  scale_color_manual(values = c("FALSE" = "#d73027", "TRUE" = "#91cf60")) +
  geom_point(aes(x = x, y = or_true), shape = 4, size = 1) +
  geom_point(aes(x = x, y = or_obs), shape = 5, size = 1, color = "#d73027") +
  guides(color = "none") +
  theme_bw() +
  labs(
    x = "Replication",
    y = "OR",
    color = "95%-CI contains true value?",
    title = "MLE with optim()"
  ) +
  ylim(0, 4)

rm(df_sim_results_cov_mle)

### combine
sim_coverage_sep <- wrap_plots(
  sim_cov_true,
  sim_cov_naive,
  sim_cov_corr,
  sim_cov_corr_priors,
  sim_cov_mle,
  nrow = 5, ncol = 1,
  guides = "collect",
  axes = "collect"
) &
  theme(legend.position = "bottom")

rm(
  sim_cov_true, sim_cov_naive, sim_cov_corr, sim_cov_corr_priors,
  sim_cov_mle
)

## est better than obs ----

sim_est_better_obs <- df_sim_results %>%
  ggplot(aes(x = diff_or, y = diff_or_obs, color = diff_good)) +
  geom_abline() +
  geom_abline(slope = -1) +
  geom_point(size = 1, shape = 1) +
  scale_color_manual(values = c(Worse = "#d73027", Better = "#91cf60")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlim(-2, 4) +
  facet_wrap(~meth) +
  labs(
    x = "Difference of estimated OR to true OR",
    y = "Difference of observed OR to true OR",
    color = "Is point estimate better than observed OR?"
  )

sim_est_better_obs_table <- df_sim_results %>%
  select(meth, diff_good) %>%
  rename(Method = "meth") %>%
  group_by(Method) %>%
  summarise(`Better? (in %)` = mean(diff_good == "Better", na.rm = T) * 100) %>%
  flextable::as_flextable() %>%
  delete_rows(part = "footer") %>%
  delete_rows(i = 2, part = "header") %>%
  hline_bottom(part = "header")
