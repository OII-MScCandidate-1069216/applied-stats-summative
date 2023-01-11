# Title: Summative oxford - applied stats
# Date: December 2022
# ===========================================================
# covid-summative-stats 


# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(tidyverse, here, equatiomatic, dotwhisker,
               MASS, jtools, stargazer, AICcmodavg, DHARMa)

# Read data and prepare  --------------------------------------------------------------
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


# Linear regression  ------------------------------------------------------
covid_cumulative <- covid_data %>% 
  dplyr::select(log_deaths, social_backwardness_mx, pct_healthins_mx, 
                pct_healthins_informal_mx, gdp_percapita_municipal_2020,
                pct_over65_mx, pop_density_mx, 
                pct_obesity_lq, women_leg_mun_mx,
                pct_catholic_mx, party_mis_state_mun_mx, party_mis_fed_mun_mx, 
                women_exec_mun_mx,
                leader_experience_mun_mx) %>% 
  na.omit()



### stepwise regression 
null <- lm(log_deaths ~ 1, data = covid_cumulative)
full <- lm(log_deaths ~ ., data = covid_cumulative)


step_reg <- stepAIC(null, scope = list(lower = null, upper = full),
                    data = covid_cumulative, 
                    direction = "forward")

summary(step_reg)
extract_eq(step_reg)

par(mfrow = c(2, 2))
plot(step_reg)

rm(covid_cumulative)



# Poisson reg -------------------------------------------------------------
covid_binom <- covid_data %>% 
  dplyr::select(tot_covid_deaths, social_backwardness_mx, pct_healthins_mx, 
                pct_healthins_informal_mx, gdp_percapita_municipal_2020,
                pct_over65_mx, pop_density_mx, 
                pct_obesity_lq, women_leg_mun_mx, women_leg_mun_mx, 
                pct_catholic_mx, party_mis_state_mun_mx, party_mis_fed_mun_mx, 
                women_exec_mun_mx,
                leader_experience_mun_mx) %>% 
  na.omit()

poisson_reg <- glm(tot_covid_deaths ~ pct_healthins_mx + pop_density_mx + 
                         pct_over65_mx + party_mis_fed_mun_mx + social_backwardness_mx + 
                         pct_obesity_lq + pct_catholic_mx + gdp_percapita_municipal_2020 + 
                         women_exec_mun_mx + women_leg_mun_mx,
                      family = poisson(),
                       data = covid_binom)



plot(poisson_reg)
summary(poisson_reg)
extract_eq(poisson_reg)


resp <- simulateResiduals(poisson_reg, refit = T)
testDispersion(resp, plot = F)
plot(resp)


# Negative binomial  ------------------------------------------------------
negative_reg <- glm.nb(tot_covid_deaths ~ pct_healthins_mx + pop_density_mx + 
                         pct_over65_mx + party_mis_fed_mun_mx + social_backwardness_mx + 
                         pct_obesity_lq + pct_catholic_mx + gdp_percapita_municipal_2020 + 
                         women_exec_mun_mx + women_leg_mun_mx,
                       data = covid_binom)

plot(negative_reg)
summary(negative_reg)
extract_eq(negative_reg)
dwplot(negative_reg) + theme_minimal()


resnb <- simulateResiduals(negative_reg, refit = T)
plot(resnb)
testDispersion(resnb, plot=F)


# AIC COMPARISON ----------------------------------------------------------
poisson_reg$aic 
negative_reg$aic


# Stargazer ---------------------------------------------------------------
stargazer(step_reg, poisson_reg, negative_reg, type = 'text', star.cutoffs = c(.05, .01, .001), 
          no.space = T, digits = 2)


# DONE. 