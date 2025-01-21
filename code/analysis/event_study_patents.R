# patents_event_study
# Patents 1855-1866 
library(fixest); library(tidyverse); library(modelsummary); library(tinytable)
source("./code/data_wrangling/data_wrangling_final_ds.R")

# Set the global bandwidth
bw <- 150000

# Mutate
#final <- 
final2 <- final |>
  group_by(DEN_CIRC, year, allegiance_1861) |>
  summarize(ptvi = sum(patents_together_verz_italy, na.rm=T),
            ip = sum(interpolated_population, na.rm=T)) |>
  mutate(ppc = ptvi/ip) |>
  ungroup()

model1 <- feols(ppc*1e7 ~ i(as.factor(year), allegiance_1861, ref="1859", ref2 = "Veneto") + ip | DEN_CIRC + year, 
                data = final2 |> filter(is.element(year, 1855:1866)))


final |> 
  mutate(patents_together_verz_italy = if_else(is.na(patents_together_verz_italy), 0, patents_together_verz_italy),
         patents_together_verz_italy_pc = if_else(is.na(patents_together_verz_italy_pc), 0, patents_together_verz_italy_pc))

model1 <- feols(patents_together_verz_italy_pc*1e7 ~ i(as.factor(year), allegiance_1861, ref="1859", ref2 = "Veneto") + interpolated_population + abs_distance_to_border | as.factor(COD_PROV)+ year, 
                data = final |> 
                  filter(abs(running) < bw,
                    is.element(year, 1855:1866)),
                vcov=~PRO_COM)


test <- final |>
  filter(is.element(year, 1855:1866)) |> 
  group_by(allegiance_1861, year) |> 
  summarize(total_patents_per_capita = sum(patents_together_verz_italy, na.rm=T)/ sum(interpolated_population, na.rm=T)*1e6)

test |> 
  ggplot(aes(x=year, y=total_patents_per_capita, color=allegiance_1861, group=allegiance_1861)) + geom_line()

summary(model1)




tt1 <- modelsummary(panel_a,
                    coef_map=coef_map,
                    stars=c("*"=0.1, "**"=0.05, "***"=0.01),
                    gof_map = tibble(raw=c("adj.r.squared", "nobs"), 
                                     clean=c("Adj. $R^2$", "N"),
                                     fmt=c(3, 0)),
                    title="Estimates of Unification on Patenting Activity\\label{tab:patent}",
                    estimate = "{estimate}{stars}",
                    #notes = n, 
                    output = "tinytable",
                    width=c(0.3, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1), 
                    add_rows = as_tibble_row(c("Year FE", rep("No", 4), rep("Yes", 2)), .name_repair = "unique") |>
                      bind_rows(as_tibble_row(c("Controls", rep("No", 2), rep("Yes", 4)), .name_repair = "unique"))
)

tt2 <-  modelsummary(panel_b,
                     coef_map=coef_map,
                     stars=c("*"=0.1, "**"=0.05, "***"=0.01),
                     gof_map = tibble(raw=c("adj.r.squared", "nobs"), 
                                      clean=c("Adj. $R^2$", "N"),
                                      fmt=c(3, 0)),
                     title="Testimates of Unification on Patenting Activity\\label{tab:patent}",
                     estimate = "{estimate}{stars}",
                     notes = n, 
                     output = "tinytable",
                     width=c(0.3, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1), 
                     add_rows = as_tibble_row(c("Year FE", rep("No", 4), rep("Yes", 2)), .name_repair = "unique") |>
                       bind_rows(as_tibble_row(c("Controls", rep("No", 2), rep("Yes", 4)), .name_repair = "unique"))
) 

rbind2(tt1, tt2) |>
  group_tt(i=list("Panel A: Pre-Unification"=1, "Panel B: Post-Lombardy, Pre-Veneto Unification"=7)) |>
  style_tt(
    i=c(1, 8, 9), bold=T) |>
  style_tt(i = 3, line = "b") |>
  style_tt(i = 7, line = "b") |>
  style_tt(i = 11, line = "b") |>
  style_tt(i = 15, line = "b") |>
  save_tt("./tables/patents_short_term.tex", overwrite = T)
