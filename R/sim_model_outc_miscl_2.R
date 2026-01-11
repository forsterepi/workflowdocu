# Legend ------------------------------------------------------------------
# Simulation module "Outcome misclassification with predictor"

## Variables:
# x (binary predictor)
# pTrue (probability of yTrue)
# yTrue (true values of y)
# yObs (observed values of y)

## Parameters:
# pX (probability of x)
# pAlpha (intercept of logistic regression on probability scale)
# ORx (Odds Ratio of predictor X)
# se (sensitivity)
# sp (specificity)


# Module ------------------------------------------------------------------

sim_mod_2 <- DAG.empty() %>%
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
