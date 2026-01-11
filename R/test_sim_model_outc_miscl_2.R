# Title -------------------------------------------------------------------
# Project Title: greenstress
# Script Title: pre_2_2_test_outc_miscl_2

# Script Description: Testing simulation module "Outcome misclassification
# with predictor"


# Prepare experiment ------------------------------------------------------

## Create functions ----

test_2_dgp <- function(n, pX, pAlpha, ORx, se, sp) {
  dags <- list(sim_mod_2)
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
    df = df,
    se = se,
    sp = sp
  )

  return(out)
}

test_2_method <- function(df, se, sp) {
  out <- list(
    p_x = mean(df$x),
    p_yTrue_x0 = mean(df$yTrue[df$x == 0]),
    or_true = exp(qlogis(mean(df$yTrue[df$x == 1])) - qlogis(mean(df$yTrue[df$x == 0]))),
    se_true = mean(df$yObs[df$yTrue == 1] == 1),
    sp_true = mean(df$yObs[df$yTrue == 0] == 0)
  )

  return(out)
}

## Create experiment ----

dgp2 <- create_dgp(
  .dgp_fun = test_2_dgp,
  .name = "Test Module 2: DGP",
  n = 1000
)

meth2 <- create_method(
  .method_fun = test_2_method,
  .name = "Test Module 2: Method"
)

test_2_exp <- create_experiment(
  name = "Test Module 2"
) %>%
  add_dgp(dgp2) %>%
  add_method(meth2) %>%
  add_vary_across(
    .dgp = dgp2,
    pX = list(0.5),
    pAlpha = list(0.005, 0.05, 0.5),
    ORx = list(1, 1.2, 2),
    se = list(0.6),
    sp = list(0.8)
  )

## Clean
rm(test_2_dgp, test_2_method, dgp2, meth2)


# Run experiment ----------------------------------------------------------

if (cache$exists("test_2_result")) {
  test_2_result <- cache$get("test_2_result")
} else {
  test_2_result <- test_2_exp %>%
    run_experiment(n_reps = 100, save = FALSE)
  cache$set("test_2_result", test_2_result)
}
rm(test_2_exp)


# Visualise ---------------------------------------------------------------

df_s_no <- test_2_result$fit_results %>%
  filter(pAlpha == 0.005, ORx == 1)

df_s_low <- test_2_result$fit_results %>%
  filter(pAlpha == 0.005, ORx == 1.2)

df_s_high <- test_2_result$fit_results %>%
  filter(pAlpha == 0.005, ORx == 2)

df_m_no <- test_2_result$fit_results %>%
  filter(pAlpha == 0.05, ORx == 1)

df_m_low <- test_2_result$fit_results %>%
  filter(pAlpha == 0.05, ORx == 1.2)

df_m_high <- test_2_result$fit_results %>%
  filter(pAlpha == 0.05, ORx == 2)

df_l_no <- test_2_result$fit_results %>%
  filter(pAlpha == 0.5, ORx == 1)

df_l_low <- test_2_result$fit_results %>%
  filter(pAlpha == 0.5, ORx == 1.2)

df_l_high <- test_2_result$fit_results %>%
  filter(pAlpha == 0.5, ORx == 2)

## 0.005, 1 ----
# 0.005, 1, pX
p_pX_s_no <- df_s_no %>%
  ggplot(aes(x = p_x)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.5)) +
  theme_bw() +
  labs(x = "p(X)", y = "Density", title = "OR = 1") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_pX_s_no)$y$range$range[2]

p_pX_s_no <- p_pX_s_no +
  annotate("label",
    x = 0.5,
    y = y_max / 2,
    label = "pX = 0.5"
  )

# 0.005, 1, pAlpha
p_pAlpha_s_no <- df_s_no %>%
  ggplot(aes(x = p_yTrue_x0)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.005)) +
  theme_bw() +
  labs(x = "p(yTrue, X=0)", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_pAlpha_s_no)$y$range$range[2]

p_pAlpha_s_no <- p_pAlpha_s_no +
  annotate("label",
    x = 0.005,
    y = y_max / 2,
    label = "pAlpha = 0.005",
    hjust = "left"
  )

# 0.005, 1, ORx
p_ORx_s_no <- df_s_no %>%
  ggplot(aes(x = or_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 1)) +
  theme_bw() +
  labs(x = "OR", y = "Density") +
  xlim(0, 4)

y_max <- ggplot2::layer_scales(p_ORx_s_no)$y$range$range[2]

p_ORx_s_no <- p_ORx_s_no +
  annotate("label",
    x = 1,
    y = y_max / 2,
    label = "ORx = 1"
  )

# 0.005, 1, se
p_se_s_no <- df_s_no %>%
  ggplot(aes(x = se_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.6)) +
  theme_bw() +
  labs(x = "Sensitivity", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_se_s_no)$y$range$range[2]

p_se_s_no <- p_se_s_no +
  annotate("label",
    x = 0.6,
    y = y_max / 2,
    label = "se = 0.6"
  )

# 0.005, 1, sp
p_sp_s_no <- df_s_no %>%
  ggplot(aes(x = sp_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.8)) +
  theme_bw() +
  labs(x = "Specificity", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_sp_s_no)$y$range$range[2]

p_sp_s_no <- p_sp_s_no +
  annotate("label",
    x = 0.8,
    y = y_max / 2,
    label = "sp = 0.8",
    hjust = "right"
  )

## 0.005, 1.2 ----
# 0.005, 1.2, pX
p_pX_s_low <- df_s_low %>%
  ggplot(aes(x = p_x)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.5)) +
  theme_bw() +
  labs(x = "p(X)", y = "Density", title = "OR = 1.2") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_pX_s_low)$y$range$range[2]

p_pX_s_low <- p_pX_s_low +
  annotate("label",
    x = 0.5,
    y = y_max / 2,
    label = "pX = 0.5"
  )

# 0.005, 1.2, pAlpha
p_pAlpha_s_low <- df_s_low %>%
  ggplot(aes(x = p_yTrue_x0)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.005)) +
  theme_bw() +
  labs(x = "p(yTrue, X=0)", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_pAlpha_s_low)$y$range$range[2]

p_pAlpha_s_low <- p_pAlpha_s_low +
  annotate("label",
    x = 0.005,
    y = y_max / 2,
    label = "pAlpha = 0.005",
    hjust = "left"
  )

# 0.005, 1.2, ORx
p_ORx_s_low <- df_s_low %>%
  ggplot(aes(x = or_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 1.2)) +
  theme_bw() +
  labs(x = "OR", y = "Density") +
  xlim(0, 4)

y_max <- ggplot2::layer_scales(p_ORx_s_low)$y$range$range[2]

p_ORx_s_low <- p_ORx_s_low +
  annotate("label",
    x = 1.2,
    y = y_max / 2,
    label = "ORx = 1.2"
  )

# 0.005, 1.2, se
p_se_s_low <- df_s_low %>%
  ggplot(aes(x = se_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.6)) +
  theme_bw() +
  labs(x = "Sensitivity", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_se_s_low)$y$range$range[2]

p_se_s_low <- p_se_s_low +
  annotate("label",
    x = 0.6,
    y = y_max / 2,
    label = "se = 0.6"
  )

# 0.005, 1.2, sp
p_sp_s_low <- df_s_low %>%
  ggplot(aes(x = sp_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.8)) +
  theme_bw() +
  labs(x = "Specificity", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_sp_s_low)$y$range$range[2]

p_sp_s_low <- p_sp_s_low +
  annotate("label",
    x = 0.8,
    y = y_max / 2,
    label = "sp = 0.8",
    hjust = "right"
  )

## 0.005, 2 ----
# 0.005, 2, pX
p_pX_s_high <- df_s_high %>%
  ggplot(aes(x = p_x)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.5)) +
  theme_bw() +
  labs(x = "p(X)", y = "Density", title = "OR = 2") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_pX_s_high)$y$range$range[2]

p_pX_s_high <- p_pX_s_high +
  annotate("label",
    x = 0.5,
    y = y_max / 2,
    label = "pX = 0.5"
  )

# 0.005, 2, pAlpha
p_pAlpha_s_high <- df_s_high %>%
  ggplot(aes(x = p_yTrue_x0)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.005)) +
  theme_bw() +
  labs(x = "p(yTrue, X=0)", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_pAlpha_s_high)$y$range$range[2]

p_pAlpha_s_high <- p_pAlpha_s_high +
  annotate("label",
    x = 0.005,
    y = y_max / 2,
    label = "pAlpha = 0.005",
    hjust = "left"
  )

# 0.005, 2, ORx
p_ORx_s_high <- df_s_high %>%
  ggplot(aes(x = or_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 2)) +
  theme_bw() +
  labs(x = "OR", y = "Density") +
  xlim(0, 4)

y_max <- ggplot2::layer_scales(p_ORx_s_high)$y$range$range[2]

p_ORx_s_high <- p_ORx_s_high +
  annotate("label",
    x = 2,
    y = y_max / 2,
    label = "ORx = 2"
  )

# 0.005, 2, se
p_se_s_high <- df_s_high %>%
  ggplot(aes(x = se_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.6)) +
  theme_bw() +
  labs(x = "Sensitivity", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_se_s_high)$y$range$range[2]

p_se_s_high <- p_se_s_high +
  annotate("label",
    x = 0.6,
    y = y_max / 2,
    label = "se = 0.6"
  )

# 0.005, 2, sp
p_sp_s_high <- df_s_high %>%
  ggplot(aes(x = sp_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.8)) +
  theme_bw() +
  labs(x = "Specificity", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_sp_s_high)$y$range$range[2]

p_sp_s_high <- p_sp_s_high +
  annotate("label",
    x = 0.8,
    y = y_max / 2,
    label = "sp = 0.8",
    hjust = "right"
  )

p_s <- wrap_plots(
  p_pX_s_no, p_pX_s_low, p_pX_s_high,
  p_pAlpha_s_no, p_pAlpha_s_low, p_pAlpha_s_high,
  p_ORx_s_no, p_ORx_s_low, p_ORx_s_high,
  p_se_s_no, p_se_s_low, p_se_s_high,
  p_sp_s_no, p_sp_s_low, p_sp_s_high,
  nrow = 5, ncol = 3,
  axes = "collect"
) +
  plot_annotation(tag_levels = "A")

rm(
  p_pX_s_no, p_pX_s_low, p_pX_s_high,
  p_pAlpha_s_no, p_pAlpha_s_low, p_pAlpha_s_high,
  p_ORx_s_no, p_ORx_s_low, p_ORx_s_high,
  p_se_s_no, p_se_s_low, p_se_s_high,
  p_sp_s_no, p_sp_s_low, p_sp_s_high
)


## 0.05, 1 ----
# 0.05, 1, pX
p_pX_m_no <- df_m_no %>%
  ggplot(aes(x = p_x)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.5)) +
  theme_bw() +
  labs(x = "p(X)", y = "Density", title = "OR = 1") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_pX_m_no)$y$range$range[2]

p_pX_m_no <- p_pX_m_no +
  annotate("label",
    x = 0.5,
    y = y_max / 2,
    label = "pX = 0.5"
  )

# 0.05, 1, pAlpha
p_pAlpha_m_no <- df_m_no %>%
  ggplot(aes(x = p_yTrue_x0)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.05)) +
  theme_bw() +
  labs(x = "p(yTrue, X=0)", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_pAlpha_m_no)$y$range$range[2]

p_pAlpha_m_no <- p_pAlpha_m_no +
  annotate("label",
    x = 0.05,
    y = y_max / 2,
    label = "pAlpha = 0.05",
    hjust = "left"
  )

# 0.05, 1, ORx
p_ORx_m_no <- df_m_no %>%
  ggplot(aes(x = or_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 1)) +
  theme_bw() +
  labs(x = "OR", y = "Density") +
  xlim(0, 4)

y_max <- ggplot2::layer_scales(p_ORx_m_no)$y$range$range[2]

p_ORx_m_no <- p_ORx_m_no +
  annotate("label",
    x = 1,
    y = y_max / 2,
    label = "ORx = 1"
  )

# 0.05, 1, se
p_se_m_no <- df_m_no %>%
  ggplot(aes(x = se_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.6)) +
  theme_bw() +
  labs(x = "Sensitivity", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_se_m_no)$y$range$range[2]

p_se_m_no <- p_se_m_no +
  annotate("label",
    x = 0.6,
    y = y_max / 2,
    label = "se = 0.6"
  )

# 0.05, 1, sp
p_sp_m_no <- df_m_no %>%
  ggplot(aes(x = sp_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.8)) +
  theme_bw() +
  labs(x = "Specificity", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_sp_m_no)$y$range$range[2]

p_sp_m_no <- p_sp_m_no +
  annotate("label",
    x = 0.8,
    y = y_max / 2,
    label = "sp = 0.8",
    hjust = "right"
  )

## 0.05, 1.2 ----
# 0.05, 1.2, pX
p_pX_m_low <- df_m_low %>%
  ggplot(aes(x = p_x)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.5)) +
  theme_bw() +
  labs(x = "p(X)", y = "Density", title = "OR = 1.2") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_pX_m_low)$y$range$range[2]

p_pX_m_low <- p_pX_m_low +
  annotate("label",
    x = 0.5,
    y = y_max / 2,
    label = "pX = 0.5"
  )

# 0.05, 1.2, pAlpha
p_pAlpha_m_low <- df_m_low %>%
  ggplot(aes(x = p_yTrue_x0)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.05)) +
  theme_bw() +
  labs(x = "p(yTrue, X=0)", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_pAlpha_m_low)$y$range$range[2]

p_pAlpha_m_low <- p_pAlpha_m_low +
  annotate("label",
    x = 0.05,
    y = y_max / 2,
    label = "pAlpha = 0.05",
    hjust = "left"
  )

# 0.05, 1.2, ORx
p_ORx_m_low <- df_m_low %>%
  ggplot(aes(x = or_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 1.2)) +
  theme_bw() +
  labs(x = "OR", y = "Density") +
  xlim(0, 4)

y_max <- ggplot2::layer_scales(p_ORx_m_low)$y$range$range[2]

p_ORx_m_low <- p_ORx_m_low +
  annotate("label",
    x = 1.2,
    y = y_max / 2,
    label = "ORx = 1.2"
  )

# 0.05, 1.2, se
p_se_m_low <- df_m_low %>%
  ggplot(aes(x = se_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.6)) +
  theme_bw() +
  labs(x = "Sensitivity", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_se_m_low)$y$range$range[2]

p_se_m_low <- p_se_m_low +
  annotate("label",
    x = 0.6,
    y = y_max / 2,
    label = "se = 0.6"
  )

# 0.05, 1.2, sp
p_sp_m_low <- df_m_low %>%
  ggplot(aes(x = sp_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.8)) +
  theme_bw() +
  labs(x = "Specificity", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_sp_m_low)$y$range$range[2]

p_sp_m_low <- p_sp_m_low +
  annotate("label",
    x = 0.8,
    y = y_max / 2,
    label = "sp = 0.8",
    hjust = "right"
  )

## 0.05, 2 ----
# 0.05, 2, pX
p_pX_m_high <- df_m_high %>%
  ggplot(aes(x = p_x)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.5)) +
  theme_bw() +
  labs(x = "p(X)", y = "Density", title = "OR = 2") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_pX_m_high)$y$range$range[2]

p_pX_m_high <- p_pX_m_high +
  annotate("label",
    x = 0.5,
    y = y_max / 2,
    label = "pX = 0.5"
  )

# 0.05, 2, pAlpha
p_pAlpha_m_high <- df_m_high %>%
  ggplot(aes(x = p_yTrue_x0)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.05)) +
  theme_bw() +
  labs(x = "p(yTrue, X=0)", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_pAlpha_m_high)$y$range$range[2]

p_pAlpha_m_high <- p_pAlpha_m_high +
  annotate("label",
    x = 0.05,
    y = y_max / 2,
    label = "pAlpha = 0.05",
    hjust = "left"
  )

# 0.05, 2, ORx
p_ORx_m_high <- df_m_high %>%
  ggplot(aes(x = or_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 2)) +
  theme_bw() +
  labs(x = "OR", y = "Density") +
  xlim(0, 4)

y_max <- ggplot2::layer_scales(p_ORx_m_high)$y$range$range[2]

p_ORx_m_high <- p_ORx_m_high +
  annotate("label",
    x = 2,
    y = y_max / 2,
    label = "ORx = 2"
  )

# 0.05, 2, se
p_se_m_high <- df_m_high %>%
  ggplot(aes(x = se_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.6)) +
  theme_bw() +
  labs(x = "Sensitivity", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_se_m_high)$y$range$range[2]

p_se_m_high <- p_se_m_high +
  annotate("label",
    x = 0.6,
    y = y_max / 2,
    label = "se = 0.6"
  )

# 0.05, 2, sp
p_sp_m_high <- df_m_high %>%
  ggplot(aes(x = sp_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.8)) +
  theme_bw() +
  labs(x = "Specificity", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_sp_m_high)$y$range$range[2]

p_sp_m_high <- p_sp_m_high +
  annotate("label",
    x = 0.8,
    y = y_max / 2,
    label = "sp = 0.8",
    hjust = "right"
  )

p_m <- wrap_plots(
  p_pX_m_no, p_pX_m_low, p_pX_m_high,
  p_pAlpha_m_no, p_pAlpha_m_low, p_pAlpha_m_high,
  p_ORx_m_no, p_ORx_m_low, p_ORx_m_high,
  p_se_m_no, p_se_m_low, p_se_m_high,
  p_sp_m_no, p_sp_m_low, p_sp_m_high,
  nrow = 5, ncol = 3,
  axes = "collect"
) +
  plot_annotation(tag_levels = "A")

rm(
  p_pX_m_no, p_pX_m_low, p_pX_m_high,
  p_pAlpha_m_no, p_pAlpha_m_low, p_pAlpha_m_high,
  p_ORx_m_no, p_ORx_m_low, p_ORx_m_high,
  p_se_m_no, p_se_m_low, p_se_m_high,
  p_sp_m_no, p_sp_m_low, p_sp_m_high
)


## 0.5, 1 ----
# 0.5, 1, pX
p_pX_l_no <- df_l_no %>%
  ggplot(aes(x = p_x)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.5)) +
  theme_bw() +
  labs(x = "p(X)", y = "Density", title = "OR = 1") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_pX_l_no)$y$range$range[2]

p_pX_l_no <- p_pX_l_no +
  annotate("label",
    x = 0.5,
    y = y_max / 2,
    label = "pX = 0.5"
  )

# 0.5, 1, pAlpha
p_pAlpha_l_no <- df_l_no %>%
  ggplot(aes(x = p_yTrue_x0)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.5)) +
  theme_bw() +
  labs(x = "p(yTrue, X=0)", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_pAlpha_l_no)$y$range$range[2]

p_pAlpha_l_no <- p_pAlpha_l_no +
  annotate("label",
    x = 0.5,
    y = y_max / 2,
    label = "pAlpha = 0.5"
  )

# 0.5, 1, ORx
p_ORx_l_no <- df_l_no %>%
  ggplot(aes(x = or_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 1)) +
  theme_bw() +
  labs(x = "OR", y = "Density") +
  xlim(0, 4)

y_max <- ggplot2::layer_scales(p_ORx_l_no)$y$range$range[2]

p_ORx_l_no <- p_ORx_l_no +
  annotate("label",
    x = 1,
    y = y_max / 2,
    label = "ORx = 1"
  )

# 0.5, 1, se
p_se_l_no <- df_l_no %>%
  ggplot(aes(x = se_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.6)) +
  theme_bw() +
  labs(x = "Sensitivity", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_se_l_no)$y$range$range[2]

p_se_l_no <- p_se_l_no +
  annotate("label",
    x = 0.6,
    y = y_max / 2,
    label = "se = 0.6"
  )

# 0.5, 1, sp
p_sp_l_no <- df_l_no %>%
  ggplot(aes(x = sp_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.8)) +
  theme_bw() +
  labs(x = "Specificity", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_sp_l_no)$y$range$range[2]

p_sp_l_no <- p_sp_l_no +
  annotate("label",
    x = 0.8,
    y = y_max / 2,
    label = "sp = 0.8",
    hjust = "right"
  )

## 0.5, 1.2 ----
# 0.5, 1.2, pX
p_pX_l_low <- df_l_low %>%
  ggplot(aes(x = p_x)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.5)) +
  theme_bw() +
  labs(x = "p(X)", y = "Density", title = "OR = 1.2") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_pX_l_low)$y$range$range[2]

p_pX_l_low <- p_pX_l_low +
  annotate("label",
    x = 0.5,
    y = y_max / 2,
    label = "pX = 0.5"
  )

# 0.5, 1.2, pAlpha
p_pAlpha_l_low <- df_l_low %>%
  ggplot(aes(x = p_yTrue_x0)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.5)) +
  theme_bw() +
  labs(x = "p(yTrue, X=0)", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_pAlpha_l_low)$y$range$range[2]

p_pAlpha_l_low <- p_pAlpha_l_low +
  annotate("label",
    x = 0.5,
    y = y_max / 2,
    label = "pAlpha = 0.5"
  )

# 0.5, 1.2, ORx
p_ORx_l_low <- df_l_low %>%
  ggplot(aes(x = or_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 1.2)) +
  theme_bw() +
  labs(x = "OR", y = "Density") +
  xlim(0, 4)

y_max <- ggplot2::layer_scales(p_ORx_l_low)$y$range$range[2]

p_ORx_l_low <- p_ORx_l_low +
  annotate("label",
    x = 1.2,
    y = y_max / 2,
    label = "ORx = 1.2"
  )

# 0.5, 1.2, se
p_se_l_low <- df_l_low %>%
  ggplot(aes(x = se_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.6)) +
  theme_bw() +
  labs(x = "Sensitivity", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_se_l_low)$y$range$range[2]

p_se_l_low <- p_se_l_low +
  annotate("label",
    x = 0.6,
    y = y_max / 2,
    label = "se = 0.6"
  )

# 0.5, 1.2, sp
p_sp_l_low <- df_l_low %>%
  ggplot(aes(x = sp_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.8)) +
  theme_bw() +
  labs(x = "Specificity", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_sp_l_low)$y$range$range[2]

p_sp_l_low <- p_sp_l_low +
  annotate("label",
    x = 0.8,
    y = y_max / 2,
    label = "sp = 0.8",
    hjust = "right"
  )

## 0.5, 2 ----
# 0.5, 2, pX
p_pX_l_high <- df_l_high %>%
  ggplot(aes(x = p_x)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.5)) +
  theme_bw() +
  labs(x = "p(X)", y = "Density", title = "OR = 2") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_pX_l_high)$y$range$range[2]

p_pX_l_high <- p_pX_l_high +
  annotate("label",
    x = 0.5,
    y = y_max / 2,
    label = "pX = 0.5"
  )

# 0.5, 2, pAlpha
p_pAlpha_l_high <- df_l_high %>%
  ggplot(aes(x = p_yTrue_x0)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.5)) +
  theme_bw() +
  labs(x = "p(yTrue, X=0)", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_pAlpha_l_high)$y$range$range[2]

p_pAlpha_l_high <- p_pAlpha_l_high +
  annotate("label",
    x = 0.5,
    y = y_max / 2,
    label = "pAlpha = 0.5"
  )

# 0.5, 2, ORx
p_ORx_l_high <- df_l_high %>%
  ggplot(aes(x = or_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 2)) +
  theme_bw() +
  labs(x = "OR", y = "Density") +
  xlim(0, 4)

y_max <- ggplot2::layer_scales(p_ORx_l_high)$y$range$range[2]

p_ORx_l_high <- p_ORx_l_high +
  annotate("label",
    x = 2,
    y = y_max / 2,
    label = "ORx = 2"
  )

# 0.5, 2, se
p_se_l_high <- df_l_high %>%
  ggplot(aes(x = se_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.6)) +
  theme_bw() +
  labs(x = "Sensitivity", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_se_l_high)$y$range$range[2]

p_se_l_high <- p_se_l_high +
  annotate("label",
    x = 0.6,
    y = y_max / 2,
    label = "se = 0.6"
  )

# 0.5, 2, sp
p_sp_l_high <- df_l_high %>%
  ggplot(aes(x = sp_true)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = 0.8)) +
  theme_bw() +
  labs(x = "Specificity", y = "Density") +
  xlim(0, 1)

y_max <- ggplot2::layer_scales(p_sp_l_high)$y$range$range[2]

p_sp_l_high <- p_sp_l_high +
  annotate("label",
    x = 0.8,
    y = y_max / 2,
    label = "sp = 0.8",
    hjust = "right"
  )

p_l <- wrap_plots(
  p_pX_l_no, p_pX_l_low, p_pX_l_high,
  p_pAlpha_l_no, p_pAlpha_l_low, p_pAlpha_l_high,
  p_ORx_l_no, p_ORx_l_low, p_ORx_l_high,
  p_se_l_no, p_se_l_low, p_se_l_high,
  p_sp_l_no, p_sp_l_low, p_sp_l_high,
  nrow = 5, ncol = 3,
  axes = "collect"
) +
  plot_annotation(tag_levels = "A")

rm(
  p_pX_l_no, p_pX_l_low, p_pX_l_high,
  p_pAlpha_l_no, p_pAlpha_l_low, p_pAlpha_l_high,
  p_ORx_l_no, p_ORx_l_low, p_ORx_l_high,
  p_se_l_no, p_se_l_low, p_se_l_high,
  p_sp_l_no, p_sp_l_low, p_sp_l_high
)

rm(
  df_l_high, df_l_low, df_l_no, df_m_high, df_m_low, df_m_no, df_s_high,
  df_s_low, df_s_no, y_max, test_2_result
)
