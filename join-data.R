# Title: Summative oxford - applied stats
# Date: December 2022
# ===========================================================


# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(tidyverse, here)


# Read data and prepare  --------------------------------------------------------------
covid_muns <- read_csv(here("data/input/mexico-municipios-final.csv")) 
covid_mexico <- read_csv(here("data/input/mexico-covid-final.csv")) %>%
  rename(inegi = id_mun) %>% 
  group_by(inegi, id_ent) %>% 
  summarise(tot_covid_deaths = sum(tot_covid_deaths, na.rm = T),
            excess_mortality_ssa = sum(excess_mortality_ssa, na.rm = T)) %>% 
  ungroup()

covid_final <- left_join(covid_mexico, covid_muns, by = c("inegi"))
write_rds(covid_final, "data/output/covid_final.rds")

# Done. 