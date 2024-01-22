
# Placebo test
innovations <- dataset |> filter(year==1855)
rdrobust(y=innovations$number_of_innovations, x=innovations$distance,
         covs=cbind(innovations$AREA_KM2,
                    innovations$angle_to_line,
                    innovations$mean_elevation)) |> summary()

# Fixed effects
feols(n ~ n_1855 + group + n_1855*group | NUTS_ID, data=innovations)
model <- feols(dn ~ NUTS_ID + prov_code, data = innovations) 
innovations <- modelr::add_residuals(data = innovations, model = model) |>
  rename(depres = resid)

rdrobust::rdrobust(y=innovations$depres, x=innovations$distance, covs=cbind(innovations$AREA_KM2,
                                                                            innovations$angle_to_line,
                                                                            innovations$mean_elevation)) |> summary()

rdplot(y=innovations$depres, x=innovations$distance)


