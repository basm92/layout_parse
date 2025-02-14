# Regression_settings:

# Modelsummary settings
gm_did <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "adj.r.squared","Adj. R. sq.", 2
  )

stars=c("*" = 0.1, "**"=0.05, "***"=0.01)

cm_did <- c(
  "year1867" = "Year (1867)",
  "post" = "Post",
  "group::Veneto" = "Veneto",
  "group::Lombardia" = "Lombardia (Treated)",
  "year1867:groupVeneto" = "Year (1867) x Veneto",
  "year1867:groupLombardo" = "Year (1867) x Lombardo",
  "post:group::Lombardia" = "Post x Treated",
  "group::Lombardia:ename::-3" = "Treated x -3",   
  "group::Lombardia:ename::-2" = "Treated x -2",
  "group::Lombardia:ename::-1" = "Treated x -1",   
  "group::Lombardia:ename::1" = "Treated x 1",  
  "group::Lombardia:ename::2" = "Treated x 2",  
  "group::Lombardia:ename::3" = "Treated x 3"
)


# Regression discontinuity settings and table-making tools
rdd <- function(data, 
                control_variables=NULL,
                fixed_effects=NULL,
                running_var='distance',
                dep_var='number_of_innovations',
                cluster='NAME_LATN',
                ...){
  

  # Control variables filter
  if (!is.null(control_variables)) {
    data <- data |>
      filter(if_all(all_of(control_variables), ~ !is.na(.x)))
  }
  
  # Fixed Effects filter
  if(!is.null(fixed_effects)){
    data <- data |>
      filter(if_all(all_of(fixed_effects), ~ !is.na(.x)))
  }
  
  # Implement dependent variable
  analysis <- data |> 
    mutate(dv = eval(parse(text = dep_var)), running_var = eval(parse(text = running_var))) |> 
    filter(!is.na(dv), !is.na(running_var))
  
  # Implement control variables
  formula_base <- "dv ~ 1"
  
  if (!is.null(control_variables)) {
    control_formula <- paste(control_variables, collapse = " + ")
    model_formula <- as.formula(paste(formula_base, " + ", control_formula))
  } else {
    model_formula <- as.formula(formula_base)
  }
  
  # Implement fixed effects in the same way
  if (!is.null(fixed_effects)){
    fixed_effects_part <- paste(" | ", paste(fixed_effects, collapse = " + "))
    model_formula <- as.formula(paste(paste(deparse(model_formula), collapse = ""), fixed_effects_part))
  } 
  
  model <- feols(model_formula, data = analysis)
  
  # Filter the data such that only the non-NA observations remain
  if(length(model$obs_selection$obsRemoved) > 0) {
    analysis <- analysis[model$obs_selection$obsRemoved,]
  }
  
  # Analyse the residuals from the fixed effects and the controls
  dependent_var <- model$residuals
  
  # Implement cluster
  if (!is.null(cluster)) {
    cluster_var <- paste0("analysis$", cluster)
    cluster_formula <- as.formula(paste("~", cluster_var))
    cluster_data <- eval(parse(text = cluster_var))
  } else {
    cluster_formula <- NULL
    cluster_data <- NULL
  }
  
  # Return the model estimates object and the used dataset
  model_out <- rdrobust(y=dependent_var, x=analysis$running_var, cluster=cluster_data, ...)
  data_out <- analysis |> select(dv, running_var, all_of(control_variables), all_of(fixed_effects))
  
  return(list(data_out, model_out))
  
}

sround <- function(number, decimals) { format(base::round(number, decimals), nsmall = decimals) }
ihs <- function(x) { log(x + sqrt(x^2+1))}
# Next: create a thing that puts that output to table column
make_table_column <- function(rdd_object, margin_dv = 5e4, extra_rows = NULL) {
  
  estimates <- rdd_object[[2]]
  dataset <- rdd_object[[1]] |> st_drop_geometry()
  
  iv <- as.character(estimates$call$x) |> pluck(3)
  # Extract Treatment Effect Estimate
  est <- estimates$Estimate[1] |> sround(3)
  # Extract SE
  se <- estimates$Estimate[3] |> sround(3)
  # P-value and stars
  robust_p_value <- estimates$pv[3] |> sround(3)
  stars = case_when(robust_p_value < 0.01 ~ "***", 
                    robust_p_value < 0.05 ~ "**",
                    robust_p_value < 0.10 ~ "*",
                    TRUE ~ "")
  # Mean DV Treated and Control
  mean_dv_treated <- dataset |>
    filter(between(eval(parse(text=iv)), 0, margin_dv)) |>
    summarise(mean_dv = mean(dv, na.rm=TRUE)) |>
    pull() |>
    sround(3)
  
  mean_dv_control <- dataset |>
    filter(between(eval(parse(text=iv)), -margin_dv, 0)) |>
    summarise(mean_dv = mean(dv, na.rm=TRUE)) |>
    pull() |>
    sround(3)
  
  # No. of observations
  n_control <- estimates$N[1]; n_treat <- estimates$N[2]
  
  # Bandwidth
  bw <- estimates$bws[1,1] |> sround(3)
  
  data.frame(out = c(
    paste0(est, stars),
    paste0("(",se,")"),
    mean_dv_treated,
    mean_dv_control,
    n_treat,
    n_control,
    bw, 
    extra_rows
  ))
}

make_table <- function(list_of_columns, 
                       row_names = NULL,
                       row_names_add = NULL,
                       ...){
  # Get the first row with information
  if(is.null(row_names)){
    row_names <- c("Estimate",
                   "SE (BC)",
                   "Mean DV Treated 50km",
                   "Mean DV Control 50km",
                   "N (Treated)",
                   "N (Control)",
                   "Bandwidth")
    if(!is.null(row_names_add)){
      row_names <- c(row_names, row_names_add)
    }
  }
  
  # Construct Model Names
  names <- map_chr(1:length(list_of_columns), ~ paste0("(", .x, ")", collapse=''))
  df <- list_of_columns |> 
    map(make_table_column) |>
    reduce(bind_cols) 
  names(df) <- names
  
  # Together with the row names
  out <- bind_cols(" "=row_names, df)
  datasummary_df(out, ...)
}


# Potentially Compute optimal RDD bandwidth
compute_optimal_bw <- function(dv, 
                               iv, 
                               control_eq, # of the form "dv ~ x | fe"
                               dataset,
                               bwselect='msetwo'){
  dataset <- dataset |>
    mutate(dv = eval(parse(text = dv)),
           iv = eval(parse(text = iv))) |>
    filter(!is.na(dv), !is.na(iv))
  
  if(!is.null(control_eq)){
    result <- feols(as.formula(control_eq), data = dataset)
    dataset <- modelr::add_residuals(dataset, result, var = "resid")
    dataset <- dataset |>
      select(-dv) |>
      rename(dv = resid)
  }
  
  out <- rdrobust::rdbwselect(y=dataset$dv, x=dataset$iv, c=0, bwselect=bwselect)
  left_bw <- out$bws[1]
  right_bw <- out$bws[2]
  fr <- c(left_bw, right_bw)
  return(fr)
}


coef_map_lt <- c("as.factor(year)::1822:allegiance_1861::Lombardia" = "Lombardia x 1822",
                 "as.factor(year)::1833:allegiance_1861::Lombardia" = "Lombardia x 1833",
                 "as.factor(year)::1844:allegiance_1861::Lombardia" = "Lombardia x 1844",
                 "as.factor(year)::1867:allegiance_1861::Lombardia" = "Lombardia x 1867",
                 "as.factor(year)::1878:allegiance_1861::Lombardia" = "Lombardia x 1878",
                 "as.factor(year)::1889:allegiance_1861::Lombardia" = "Lombardia x 1889",
                 "as.factor(year)::1900:allegiance_1861::Lombardia" = "Lombardia x 1900",
                 "as.factor(year)::1902:allegiance_1861::Lombardia" = "Lombardia x 1902",
                 "as.factor(year)::1911:allegiance_1861::Lombardia" = "Lombardia x 1911", 
                 "as.factor(year_group)::1822:allegiance_1861::Lombardia" = "Lombardia x 1822",
                 "as.factor(year_group)::1833:allegiance_1861::Lombardia" = "Lombardia x 1833",
                 "as.factor(year_group)::1844:allegiance_1861::Lombardia" = "Lombardia x 1844",
                 "as.factor(year_group)::1867:allegiance_1861::Lombardia" = "Lombardia x 1867",
                 "as.factor(year_group)::1878:allegiance_1861::Lombardia" = "Lombardia x 1878",
                 "as.factor(year_group)::1889:allegiance_1861::Lombardia" = "Lombardia x 1889",
                 "as.factor(year_group)::1902:allegiance_1861::Lombardia" = "Lombardia x 1902",
                 "as.factor(year_group)::1911:allegiance_1861::Lombardia" = "Lombardia x 1911")

