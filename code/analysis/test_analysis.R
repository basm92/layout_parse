dataset <- st_read('./data/final_datasets/italy.geojson') |>
  mutate(year = as.factor(year))

# Placebo test
data_1855 <- dataset |> filter(year==1855)
rdrobust(y=data_1855$number_of_innovations, x=data_1855$distance,
         covs=cbind(data_1855$AREA_KM2,
                    data_1855$angle_to_line,
                    data_1855$mean_elevation,
                    data_1855$longitude,
                    data_1855$latitude)) |> summary()

# Fixed effects
model <- feols(dn ~ NUTS_ID + prov_code, data = innovations) 
innovations <- modelr::add_residuals(data = innovations, model = model) |>
  rename(depres = resid)

rdrobust::rdrobust(y=innovations$depres, x=innovations$distance, covs=cbind(innovations$AREA_KM2,
                                                                            innovations$angle_to_line,
                                                                            innovations$mean_elevation)) |> summary()

rdplot(y=innovations$depres, x=innovations$distance)


# Difference-in-difference
feols(number_of_innovations ~ i(year, group) + AREA_KM2 + mean_elevation + longitude + latitude, data=dataset)
# Coefficient test?