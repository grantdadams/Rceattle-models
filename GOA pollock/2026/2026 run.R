library(Rceattle)
library(dplyr)
library(ggplot2)

thisyear = 2026
datfinal <- read_data("2026/pk26_10_2026_final.xlsx")
m26a <- fit_goa_pk(datfinal, random_rec = TRUE)
m26a_retros <- retrospective(Rceattle = m26a, peels = 10, phase=TRUE)

dat2017 <- datfinal
dat2017$endyr <- 2017
m2017 <- fit_goa_pk(dat2017, random_rec = TRUE)

dat2018 <- datfinal
dat2018$endyr <- 2018
m2018 <- fit_goa_pk(dat2018, random_rec = TRUE)


m26b_retros <- retrospective(Rceattle = m26b, peels = 10, phase=TRUE, cores=10, nyrs_forecast = 2, forecast_rec = "model")


retros <- m26a_retros
mns <- names(retros$Rceattle_list)
retros2 <- m26a_retros
xx <- plot_recruitment(retros$Rceattle_list, model_names = mns, add_ci = TRUE, incl_proj = TRUE )@data |>
  mutate(endyr=as.numeric(gsub('Year_', '', Model))) |> mutate(model='26b', cohort=Year-1) |>
  mutate(terminal_year=as.numeric(gsub('Year_', '', as.character(Model))))
yy <- plot_recruitment(retros2$Rceattle_list, model_names = mns, add_ci = TRUE, incl_proj = TRUE)@data |>
  mutate(endyr=as.numeric(gsub('Year_', '', Model))) |> mutate(model='26a', cohort=Year-1) |>
  mutate(terminal_year=as.numeric(gsub('Year_', '', as.character(Model))))
retro_recs <- rbind(xx,yy) |>
  mutate(years_data=terminal_year-cohort) |>
  mutate(cohortf=paste(cohort,'cohort'))
g <-
  filter(retro_recs, cohort>2015 & cohort <= thisyear & years_data < 0) |>
  ggplot(aes(years_data, color=model, y=value))+#, ymin=lower95, ymax=upper95)) +
  geom_line() + geom_point() + #geom_linerange() +
  facet_wrap('cohort', ncol=3, scale='free_y') + #scale_y_log10() +
  labs(x='Years of data', y='Age-1 recruits (millions)')
g
