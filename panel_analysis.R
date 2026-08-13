library(dplyr)
library(plm)
library(corrplot)
library(ggplot2)
library(car)
library(tidyr)
library(modelsummary)
library(lme4)
library(purrr)
library(broom.mixed)
library(lmerTest)


### read data

df_reg <- read.csv("data_census_regions.csv")
data_var <- read.csv("data_socioeconomic_variables.csv")
data_div <- read.csv("data_diversity_levels.csv")
data_div_ca <- read.csv("data_diversity_scales.csv")

data_var$median_age <- as.numeric(as.character(data_var$median_age))
data <- data_var %>%
  inner_join(data_div, by = c("GEO_ID", "CENSUS_YEAR"))



data_norm <- data %>%
  mutate(across(
    .cols = where(is.numeric) & matches("^[a-z]", ignore.case = FALSE),
    .fns = ~ as.numeric(scale(.)),
    .names = "{.col}"  
  ))

pdata <- pdata.frame(data, index = c("GEO_ID", "CENSUS_YEAR"))

pdata_norm <- pdata.frame(data_norm, index = c("GEO_ID", "CENSUS_YEAR"))

### get summary
summary(data_var)

data_var %>%
  summarise(across(where(is.numeric),
                   list(
                     mean = ~mean(.x, na.rm = TRUE),
                     sd = ~sd(.x, na.rm = TRUE),
                     min = ~min(.x, na.rm = TRUE),
                     max = ~max(.x, na.rm = TRUE)
                   )))

names(data_var)


### summary per region
summary_region <- data_var %>%
  group_by(GEO_ID, GEO_NAME) %>%
  summarise(
    across(
      where(is.numeric) & !any_of(c("GEO_CODE", "GEO_ID", "CENSUS_YEAR")),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE),
        min = ~min(.x, na.rm = TRUE),
        max = ~max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

summary_region

### correlations all data
cor_matrix <- cor(data %>% select(where(is.numeric)), 
                  use = "complete.obs")
cor_matrix
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)

### correlations per region
get_corr_long <- function(df) {
  df_num <- df %>% select(where(is.numeric))
  df_num <- df_num[, sapply(df_num, function(x) sd(x, na.rm = TRUE) > 0), drop = FALSE]
  cmat <- cor(df_num, use = "pairwise.complete.obs")
  as.data.frame(as.table(cmat))
}

corr_long <- data %>%
  group_by(GEO_CODE, GEO_NAME) %>%
  group_modify(~{
    get_corr_long(.x)
  }) %>%
  ungroup()
ggplot(corr_long,
       aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  facet_wrap(~ GEO_NAME) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

### multicollinearity

ols_model <- lm(
  hill_q1 ~
    population_density +
    land_area +
    population_total+
    av_household_size +
    median_age +
    unemployment_rate +
    income_average +
    income_median +
    income_employment_average +
    income_employment_median +
    university_degree_percentage +
    non_movers_percentage +
    migrants_interprovincial_percentage +
    migrants_extern_percentage +
    migrants_intraprovincial_percentage +
    demographic_weight,
  data = data
)

vif(ols_model)

ols_model <- lm(
  hill_q1 ~
    population_density +
    av_household_size +
    unemployment_rate +
    income_median +
    migrants_interprovincial_percentage +
    migrants_extern_percentage +
    demographic_weight,
  data = data
)
vif(ols_model)
plot(ols_model)

X <- data %>%
  dplyr::select(
    population_density,
    av_household_size,
    unemployment_rate,
    income_median,
    migrants_interprovincial_percentage,
    migrants_extern_percentage,
    demographic_weight
  )

cor_mat <- cor(X, use = "pairwise.complete.obs")

corrplot(cor_mat,
         method = "color",
         type = "upper",
         addCoef.col = "black")

#### variance decomposition

variance_decomposition <- function(data, entity_var, vars_to_check) {
  data %>%
    select(all_of(c(entity_var, vars_to_check))) %>%
    pivot_longer(cols = all_of(vars_to_check), names_to = "variable", values_to = "value") %>%
    group_by(variable, .data[[entity_var]]) %>%
    summarise(entity_mean = mean(value, na.rm = TRUE),
              within_var = var(value, na.rm = TRUE),
              .groups = "drop") %>%
    group_by(variable) %>%
    summarise(
      mean_within_var = mean(within_var, na.rm = TRUE),
      between_var = var(entity_mean, na.rm = TRUE),
      total_var = mean_within_var + between_var,
      pct_within = round(100 * mean_within_var / total_var, 1),
      pct_between = round(100 * between_var / total_var, 1)
    )
}

vars_to_check <- c("richness", "hill_q1", "hill_q2",
                   "population_density", "av_household_size", "demographic_weight",
                   "unemployment_rate", "income_median",
                   "migrants_interprovincial_percentage",
                   "migrants_extern_percentage")

decomp_results <- variance_decomposition(data_norm, "GEO_NAME", vars_to_check)
print(decomp_results, n = Inf)

decomp_results %>%
  select(variable, pct_within, pct_between) %>%
  pivot_longer(cols = c(pct_within, pct_between), names_to = "component", values_to = "pct") %>%
  ggplot(aes(x = reorder(variable, pct), y = pct, fill = component)) +
  geom_col() +
  coord_flip() +
  labs(title = "Within vs. Between Variance Share", x = NULL, y = "% of total variance") +
  theme_minimal()

### panel regression

### pooled ols for q0, q1, q2

pool_model1 <- plm(
  hill_q1 ~
    population_density +
    av_household_size +
    unemployment_rate +
    income_median +
    migrants_interprovincial_percentage +
    migrants_extern_percentage +
    demographic_weight,
  data = pdata_norm,
  model = "pooling"
)

summary(pool_model1)

pool_model2 <- plm(
  hill_q2 ~
    population_density +
    av_household_size +
    unemployment_rate +
    income_median +
    migrants_interprovincial_percentage +
    migrants_extern_percentage +
    demographic_weight,
  data = pdata_norm,
  model = "pooling"
)

summary(pool_model2)

pool_model0 <- plm(
  richness ~
    population_density +
    av_household_size +
    unemployment_rate +
    income_median +
    migrants_interprovincial_percentage +
    migrants_extern_percentage +
    demographic_weight,
  data = pdata_norm,
  model = "pooling"
)

summary(pool_model0)

pool_models <- list(
  "Hill q=0" = pool_model0,
  "Hill q=1" = pool_model1,
  "Hill q=2" = pool_model2
)

modelplot(pool_models)
modelsummary(pool_models,
             statistic = "p.value",
             stars = TRUE)

### one or two ways, do we have regional/ time effects?

model_fixed0_reg <- plm(richness ~ 
                      population_density +
                      av_household_size +
                      unemployment_rate +
                      income_median +
                      migrants_interprovincial_percentage +
                      migrants_extern_percentage +
                      demographic_weight,
                    data = pdata_norm, model = "within", effect = "individual")
summary(model_fixed0_reg)

model_fixed1_reg <- plm(hill_q1 ~ 
                      population_density +
                      av_household_size +
                      unemployment_rate +
                      income_median +
                      migrants_interprovincial_percentage +
                      migrants_extern_percentage +
                      demographic_weight,
                    data = pdata_norm, model = "within", effect = "individual")
summary(model_fixed1_reg)

model_fixed2_reg <- plm(hill_q2 ~ 
                      population_density +
                      av_household_size +
                      unemployment_rate +
                      income_median +
                      migrants_interprovincial_percentage +
                      migrants_extern_percentage +
                      demographic_weight,
                    data = pdata_norm, model = "within", effect = "individual")
summary(model_fixed2_reg)

pFtest(model_fixed0_reg, pool_model0)
pFtest(model_fixed1_reg, pool_model1)
pFtest(model_fixed2_reg, pool_model2)

model_fixed0_time <- plm(richness ~ 
                          population_density +
                          av_household_size +
                          unemployment_rate +
                          income_median +
                          migrants_interprovincial_percentage +
                          migrants_extern_percentage +
                          demographic_weight,
                        data = pdata_norm, model = "within", effect = "time")
summary(model_fixed0_time)

model_fixed1_time <- plm(hill_q1 ~ 
                          population_density +
                          av_household_size +
                          unemployment_rate +
                          income_median +
                          migrants_interprovincial_percentage +
                          migrants_extern_percentage +
                          demographic_weight,
                        data = pdata_norm, model = "within", effect = "time")
summary(model_fixed1_time)

model_fixed2_time <- plm(hill_q2 ~ 
                          population_density +
                          av_household_size +
                          unemployment_rate +
                          income_median +
                          migrants_interprovincial_percentage +
                          migrants_extern_percentage +
                          demographic_weight,
                        data = pdata_norm, model = "within", effect = "time")
summary(model_fixed2_time)

pFtest(model_fixed0_time, pool_model0)
pFtest(model_fixed1_time, pool_model1)
pFtest(model_fixed2_time, pool_model2)

model_fixed0_two <- plm(richness ~ 
                          population_density +
                          av_household_size +
                          unemployment_rate +
                          income_median +
                          migrants_interprovincial_percentage +
                          migrants_extern_percentage +
                          demographic_weight,
                        data = pdata_norm, model = "within", effect = "twoway")
summary(model_fixed0_two)

model_fixed1_two <- plm(hill_q1 ~ 
                          population_density +
                          av_household_size +
                          unemployment_rate +
                          income_median +
                          migrants_interprovincial_percentage +
                          migrants_extern_percentage +
                          demographic_weight,
                        data = pdata_norm, model = "within", effect = "twoway")
summary(model_fixed1_two)

model_fixed2_two <- plm(hill_q2 ~ 
                          population_density +
                          av_household_size +
                          unemployment_rate +
                          income_median +
                          migrants_interprovincial_percentage +
                          migrants_extern_percentage +
                          demographic_weight,
                        data = pdata_norm, model = "within", effect = "twoway")
summary(model_fixed2_two)

pFtest(model_fixed0_two, model_fixed0_reg)
pFtest(model_fixed1_two, model_fixed1_reg)
pFtest(model_fixed2_two, model_fixed2_reg)

model_geo_fixed0 <- plm(richness ~ 
                         population_density +
                         av_household_size +
                         unemployment_rate +
                         income_median +
                         migrants_interprovincial_percentage +
                         migrants_extern_percentage +
                         demographic_weight +
                         factor(CENSUS_YEAR),
                       data = pdata_norm, model = "within", effect = "individual")
summary(model_geo_fixed0)
model_geo_random0 <- plm(richness ~ 
                          population_density +
                          av_household_size +
                          unemployment_rate +
                          income_median +
                          migrants_interprovincial_percentage +
                          migrants_extern_percentage +
                          demographic_weight +
                          factor(CENSUS_YEAR),
                        data = pdata_norm, model = "random", effect = "individual",
                        random.method = "walhus")  # needed given your N=13 constraint
summary(model_geo_random0)
phtest(model_geo_fixed0, model_geo_random0)

model_time_fixed0 <- plm(richness ~ 
                          population_density +
                          av_household_size +
                          unemployment_rate +
                          income_median +
                          migrants_interprovincial_percentage +
                          migrants_extern_percentage +
                          demographic_weight +
                          factor(GEO_NAME),
                        data = pdata_norm, model = "within", effect = "time")
summary(model_time_fixed0)
model_time_random0 <- plm(richness ~ 
                           population_density +
                           av_household_size +
                           unemployment_rate +
                           income_median +
                           migrants_interprovincial_percentage +
                           migrants_extern_percentage +
                           demographic_weight +
                           factor(GEO_NAME),
                         data = pdata_norm, model = "random", effect = "time",
                         random.method = "walhus")
summary(model_time_random0)
phtest(model_time_fixed0, model_time_random0)

model_geo_fixed1 <- plm(hill_q1 ~ 
                         population_density +
                         av_household_size +
                         unemployment_rate +
                         income_median +
                         migrants_interprovincial_percentage +
                         migrants_extern_percentage +
                         demographic_weight +
                         factor(CENSUS_YEAR),
                       data = pdata_norm, model = "within", effect = "individual")
summary(model_geo_fixed1)
model_geo_random1 <- plm(hill_q1 ~ 
                          population_density +
                          av_household_size +
                          unemployment_rate +
                          income_median +
                          migrants_interprovincial_percentage +
                          migrants_extern_percentage +
                          demographic_weight +
                          factor(CENSUS_YEAR),
                        data = pdata_norm, model = "random", effect = "individual",
                        random.method = "walhus")  # needed given your N=13 constraint
summary(model_geo_random1)
phtest(model_geo_fixed1, model_geo_random1)

model_time_fixed1 <- plm(hill_q1 ~ 
                          population_density +
                          av_household_size +
                          unemployment_rate +
                          income_median +
                          migrants_interprovincial_percentage +
                          migrants_extern_percentage +
                          demographic_weight +
                          factor(GEO_NAME),
                        data = pdata_norm, model = "within", effect = "time")
summary(model_time_fixed1)
model_time_random1 <- plm(hill_q1 ~ 
                           population_density +
                           av_household_size +
                           unemployment_rate +
                           income_median +
                           migrants_interprovincial_percentage +
                           migrants_extern_percentage +
                           demographic_weight +
                           factor(GEO_NAME),
                         data = pdata_norm, model = "random", effect = "time",
                         random.method = "walhus")
summary(model_time_random1)
phtest(model_time_fixed1, model_time_random1)

model_geo_fixed2 <- plm(hill_q2 ~ 
                         population_density +
                         av_household_size +
                         unemployment_rate +
                         income_median +
                         migrants_interprovincial_percentage +
                         migrants_extern_percentage +
                         demographic_weight +
                         factor(CENSUS_YEAR),
                       data = pdata_norm, model = "within", effect = "individual")
summary(model_geo_fixed2)
model_geo_random2 <- plm(hill_q2 ~ 
                          population_density +
                          av_household_size +
                          unemployment_rate +
                          income_median +
                          migrants_interprovincial_percentage +
                          migrants_extern_percentage +
                          demographic_weight +
                          factor(CENSUS_YEAR),
                        data = pdata_norm, model = "random", effect = "individual",
                        random.method = "walhus")  # needed given your N=13 constraint
summary(model_geo_random2)
phtest(model_geo_fixed2, model_geo_random2)

model_time_fixed2 <- plm(hill_q2 ~ 
                          population_density +
                          av_household_size +
                          unemployment_rate +
                          income_median +
                          migrants_interprovincial_percentage +
                          migrants_extern_percentage +
                          demographic_weight +
                          factor(GEO_NAME),
                        data = pdata_norm, model = "within", effect = "time")
summary(model_time_fixed2)
model_time_random2 <- plm(hill_q2 ~ 
                           population_density +
                           av_household_size +
                           unemployment_rate +
                           income_median +
                           migrants_interprovincial_percentage +
                           migrants_extern_percentage +
                           demographic_weight +
                           factor(GEO_NAME),
                         data = pdata_norm, model = "random", effect = "time",
                         random.method = "walhus")
summary(model_time_random2)
phtest(model_time_fixed2, model_time_random2)

run_panel_tests <- function(outcome, pooled, fixed_reg, fixed_time, fixed_two, 
                            model_geo_fixed, model_geo_random, model_time_fixed, model_time_random, pdata) {
  results <- tibble(
    outcome = outcome,
    test = c("pFtest: region FE vs pooled",
             "pFtest: twoway FE vs time FE",
             "pFtest: time FE vs pooled",
             "pFtest: twoway FE vs region FE",
             "pFtest: twoway FE vs pooled",
             "phtest: region (time dummies)",
             "phtest: time (region dummies)"),
    p_value = c(
      pFtest(fixed_reg, pooled)$p.value,
      pFtest(fixed_two, fixed_time)$p.value,
      pFtest(fixed_time, pooled)$p.value,
      pFtest(fixed_two, fixed_reg)$p.value,
      pFtest(fixed_two, pooled)$p.value,
      phtest(model_geo_fixed, model_geo_random)$p.value,
      phtest(model_time_fixed, model_time_random)$p.value
      )
  )

  return(results)
}

results_q0 <- run_panel_tests("richness", pool_model0, model_fixed0_reg, model_fixed0_time, model_fixed0_two,model_geo_fixed0, model_geo_random0, model_time_fixed0, model_time_random0, pdata)
results_q1 <- run_panel_tests("hill_q1", pool_model1, model_fixed1_reg, model_fixed1_time, model_fixed1_two, model_geo_fixed1, model_geo_random1, model_time_fixed1, model_time_random1, pdata)
results_q2 <- run_panel_tests("hill_q2", pool_model2, model_fixed2_reg, model_fixed2_time, model_fixed2_two, model_geo_fixed2, model_geo_random2, model_time_fixed2, model_time_random2, pdata)

all_results <- bind_rows(results_q0, results_q1, results_q2)
summary_table <- all_results %>%
  mutate(p_value = round(p_value, 4),
         sig = case_when(
           p_value < 0.001 ~ "***",
           p_value < 0.01  ~ "**",
           p_value < 0.05  ~ "*",
           p_value < 0.1   ~ "+",
           TRUE ~ ""
         ),
         display = paste0(p_value, sig)) %>%
  select(outcome, test, display) %>%
  pivot_wider(names_from = outcome, values_from = display)

print(summary_table)

## interpretation: region and time effects for all ling div levels (two-ways) 
##                 q0 random two ways, q1 mixed (region random, time fixed), q2 fixed two ways


model_fixed2_two <- plm(hill_q2 ~ 
                          population_density +
                          av_household_size +
                          unemployment_rate +
                          income_median +
                          migrants_interprovincial_percentage +
                          migrants_extern_percentage +
                          demographic_weight,
                        data = pdata_norm, model = "within", effect = "twoway")
summary(model_fixed2_two)

model_random0 <- plm(richness ~ 
                            population_density +
                            av_household_size +
                            unemployment_rate +
                            income_median +
                            migrants_interprovincial_percentage +
                            migrants_extern_percentage +
                            demographic_weight,
                          data = pdata_norm, model = "random", effect = "twoway",
                          random.method = "walhus")
summary(model_random0)

mixed_mod1 <- lmer(hill_q1 ~
                     population_density +
                     av_household_size +
                     unemployment_rate +
                     income_median +
                     migrants_interprovincial_percentage +
                     migrants_extern_percentage +
                     demographic_weight + factor(CENSUS_YEAR) + (1 | GEO_ID), data = pdata_norm)
summary(mixed_mod1)


### try hybrid model (Mundlak)

vars_to_decompose <- c("population_density", "av_household_size", "unemployment_rate",
                       "income_median", "migrants_interprovincial_percentage",
                       "migrants_extern_percentage", "demographic_weight")

df_mundlak <- data_norm %>%
  group_by(GEO_ID) %>%
  mutate(across(all_of(vars_to_decompose),
                list(between = ~mean(., na.rm = TRUE),
                     within  = ~. - mean(., na.rm = TRUE)),
                .names = "{.col}_{.fn}")) %>%
  ungroup()
mundlak_formula <- as.formula(paste(
  "hill_q1 ~",
  paste0(vars_to_decompose, "_within", collapse = " + "), "+",
  paste0(vars_to_decompose, "_between", collapse = " + "),
  "+ factor(CENSUS_YEAR) + (1 | GEO_ID)"
))

mundlak_q1 <- lmer(as.formula(paste(
  "hill_q1 ~",
  paste0(vars_to_decompose, "_within", collapse = " + "), "+",
  paste0(vars_to_decompose, "_between", collapse = " + "),
  "+ factor(CENSUS_YEAR) + (1 | GEO_ID)"
)), data = df_mundlak)
summary(mundlak_q1)

mundlak_q0 <- lmer(as.formula(paste(
  "richness ~",
  paste0(vars_to_decompose, "_within", collapse = " + "), "+",
  paste0(vars_to_decompose, "_between", collapse = " + "),
  "+ factor(CENSUS_YEAR) + (1 | GEO_ID)"
)), data = df_mundlak)
summary(mundlak_q0)

mundlak_q2 <- lmer(as.formula(paste(
  "hill_q2 ~",
  paste0(vars_to_decompose, "_within", collapse = " + "), "+",
  paste0(vars_to_decompose, "_between", collapse = " + "),
  "+ factor(CENSUS_YEAR) + (1 | GEO_ID)"
)), data = df_mundlak)
summary(mundlak_q2) #just to check, even if no random effects were suggested by Hausman test


### summarize results (table + plot)

final_models <- list(
  "Linguistic diversity q=0" = list ("RE two-ways" = model_random0),
  "Linguistic diversity q=1" =list("Mixed region random + time fixed" = mixed_mod1),
  "Linguistic diversity q=2" =list("FE two-ways" = model_fixed2_two)
)

modelsummary(final_models,
             statistic = NULL,
             stars = TRUE,
             shape = "cbind")

coef_df <- imap_dfr(final_models, function(model_list, outcome) {
  imap_dfr(model_list, function(mod, model_name) {
    tidy(mod, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        Outcome = outcome,
        Model = model_name
      )
  })
})

coef_order <- unique(coef_df$term)

coef_labels <- c(
  "population_density" = "Population density",
  "av_household_size" = "Average household size",
  "unemployment_rate" = "Unemployment rate",
  "income_median" = "Median income",
  "migrants_interprovincial_percentage" = "Interprovincial migrants (%)",
  "migrants_extern_percentage" = "External migrants (%)",
  "demographic_weight" =  "Demographic weight",
  "factor(CENSUS_YEAR)1996"  = "1996",
  "factor(CENSUS_YEAR)2001"  = "2001",
  "factor(CENSUS_YEAR)2006"  = "2006",
  "factor(CENSUS_YEAR)2011"  = "2011",
  "factor(CENSUS_YEAR)2016"  = "2016",
  "factor(CENSUS_YEAR)2021"  = "2021"
)

model_order <- c(
  "FE two-ways",
  "Mixed region random + time fixed",
  "RE two-ways"
)


coef_df <- imap_dfr(final_models, function(model_list, outcome) {
  imap_dfr(model_list, function(mod, model_name) {
    tidy(mod, conf.int = TRUE) %>%
      filter(term != "(Intercept)",
             !grepl("^sd__", term)) %>%
      mutate(
        term_label = dplyr::recode(term, !!!coef_labels),
        term_label = factor(
          term_label,
          levels = rev(dplyr::recode(coef_order, !!!coef_labels))
        ),
        Outcome = outcome,
        Model = factor(model_name, levels = model_order)
      )
  })
})

coef_plot <- ggplot(coef_df,
                    aes(x = estimate,
                        y = term_label,
                        color = Model)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.6),
    height = 0
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ Outcome, scales = "free_x") +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )+
  labs(
    x = "Coefficient estimate",
    y = NULL,
    color = "Model"
  )

coef_plot
ggsave("coefficient_plot_final.png", plot = coef_plot, width = 8,
       height = 4,
       dpi = 600)

models0 <- list("Linguistic diversity q=0" = list (
  "pooled OLS" = pool_model0,
  "FE two-ways" = model_fixed0_two,
  "RE two-ways" = model_random0,
  "Mundlak" = mundlak_q0
))

modelsummary(models0,
             statistic = NULL,
             stars = TRUE,
             shape = "cbind") 


models1 <- list("Linguistic diversity q=1" =list(
  "pooled OLS (q1)" = pool_model1,
  "FE (q1)" = model_fixed1_two,
  "Mixed region random + time fixed (q1)" = mixed_mod1,
  "mundlak (q1)" = mundlak_q1
))

modelsummary(models1,
             statistic = NULL,
             stars = TRUE,
             shape = "cbind") 

models2 <- list("Linguistic diversity q=2" =list(
  "pooled OLS (q2)" = pool_model2,
  "FE (q2)" = model_fixed2_two
))

modelsummary(models2,
             statistic = NULL,
             stars = TRUE,
             shape = "cbind") 
