# Title: Summative oxford - applied stats
# Date: December 2022
# ===========================================================
# covid-summative-stats 


# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(tidyverse, here, ClustOfVar, cluster)



# Data  -------------------------------------------------------------------
covid_data <- read_rds(here("data", "output", "covid_final.rds")) %>% 
  mutate(log_deaths = log(tot_covid_deaths),
         log_deaths = na_if(log_deaths, "-Inf"),
         excess_mortality_log = log(excess_mortality_ssa),
         excess_mortality_log = na_if(excess_mortality_log, "-Inf"),
         gdp_quant = ntile(gdp_percapita_municipal_2020, 4),
         gdp_quant = as.factor(gdp_quant),
         stateCap_quant = ntile(state_capacity, 4),
         stateCap_quant = as.factor(stateCap_quant),
         party_mis_fed_mun_mx = as.factor(party_mis_fed_mun_mx),
         party_mis_state_mun_mx = as.factor(party_mis_state_mun_mx),
         party_exec_right_mun_mx = as.factor(party_exec_right_mun_mx),
         women_exec_mun_mx = as.factor(women_exec_mun_mx)) 


xquant <- covid_data %>% 
    dplyr::select_if(is.numeric) %>% 
  dplyr::select(-tot_covid_deaths, -excess_mortality_ssa, 
         -excess_mortality_log, -log_deaths) %>% 
  as.matrix()

xqual <- covid_data %>%
  dplyr::select_if(is.factor) %>% 
  as.matrix()


# Clusters  ---------------------------------------------------------------

### create dendogran 
tree <- hclustvar(xquant, xqual)
plot(tree)


### estimate optimal clusters 
stab <- stability(tree, B=40)


### partions after optimal 
part_hier <- cutreevar(tree,9)
part_hier$var$cluster1

print(part_hier)
summary(part_hier)


# Done