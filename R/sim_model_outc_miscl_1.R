# Legend ------------------------------------------------------------------
# Simulation module "Outcome misclassification without predictor"

## Variables:
# yTrue (true values of y)
# yObs (observed values of y)

## Parameters:
# p (Probability of yTrue)
# se (sensitivity)
# sp (specificity)


# Module ------------------------------------------------------------------

sim_mod_1 <- DAG.empty() %>%
  add.nodes(
    node("yTrue",
      distr = "rbern",
      prob = p
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
