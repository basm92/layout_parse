# Regression_settings:

# Modelsummary settings
gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "adj.r.squared","Adj. $R^2$", 2)

glance_custom.fixest <- function(x, ...) { # don't forget ...
  treatment_test <- marginaleffects::hypotheses(
    model=x,
    hypothesis="b1=b6",
    vcov=vcov_conley)
  f <- treatment_test$statistic
  p <- treatment_test$p.value
  
  tibble(
    "F Stat. Treated x I Veneto" = round(f, 2),
    "p value" = round(p, 3))
}

stars=c("*" = 0.1, "**"=0.05, "***"=0.01)
