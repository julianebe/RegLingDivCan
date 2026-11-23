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
library(pheatmap)



### read data

data_var <- read.csv("data_socioeconomic_variables.csv")

data_div_reg <- read.csv("div_levels_reg.csv")


data_var$median_age <- as.numeric(as.character(data_var$median_age))
data_norm_reg <- data_var %>%
  inner_join(data_div_reg, by = c("GEO_ID", "CENSUS_YEAR"))%>%
  mutate(across(
    .cols = where(is.numeric) & matches("^[a-z]", ignore.case = FALSE),
    .fns = ~ as.numeric(scale(.)),
    .names = "{.col}"  
  ))

pdata_reg <- pdata.frame(data_norm_reg, index = c("GEO_ID", "CENSUS_YEAR"))

data <- data_var %>%
  inner_join(data_div_reg, by = c("GEO_ID", "CENSUS_YEAR"))


### correlations predictors

cor_matrix_var <- cor(
  data_var %>% select(where(is.numeric), -GEO_CODE, -GEO_ID, -CENSUS_YEAR),
  use = "complete.obs"
)

corrplot(cor_matrix_var, method = "color", type = "upper",
         tl.cex = 0.7) 

pheatmap(
  cor_matrix_var,
  clustering_method = "complete",
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  display_numbers = TRUE,
  number_format = "%.2f",
  color = colorRampPalette(
    c("blue", "white", "red")
  )(100),
  main = "Clustered Correlation Matrix",
  #filename = "figures/plot_cor_var.png",
  #width = 12,
  #height = 10
)


### check for multicollinearity

ols_model <- lm(
  q1 ~
    population_total+
    land_area +
    population_density +
    demographic_weight+
    median_age +
    av_household_size +
    university_degree_percentage +
    income_average +
    income_median +
    income_employment_average +
    income_employment_median +
    unemployment_rate +
    non_movers_percentage  +
    migrants_intraprovincial_percentage +
    migrants_interprovincial_percentage +
    migrants_extern_percentage,
  data = data
)

vif(ols_model)
plot(ols_model)

ols_model <- lm(
  q1 ~
    population_density +
    demographic_weight+
    av_household_size +
    income_median +
    unemployment_rate +
    migrants_interprovincial_percentage +
    migrants_extern_percentage,
  data = data
)
vif(ols_model)
plot(ols_model)

par(mfrow = c(2, 2))
plot(ols_model)
par(mfrow = c(1, 1))



## correlation with variable selection

X <- data %>%
  dplyr::select(
    population_density,
    demographic_weight,
    av_household_size,
    income_median,
    unemployment_rate,
    migrants_interprovincial_percentage,
    migrants_extern_percentage
  )

cor_mat <- cor(X, use = "complete.obs")

corrplot(cor_mat,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         number.cex = 0.7,
         tl.cex = 0.7)

pheatmap(
  cor_mat,
  clustering_method = "complete",
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  display_numbers = TRUE,
  number_format = "%.2f",
  color = colorRampPalette(
    c("blue", "white", "red")
  )(100),
  main = "Clustered Correlation Matrix",
  #filename = "figures/plot_cor_final_var.png",
  #width = 12,
  #height = 10
)
graphics.off()


#### variance decomposition

variance_decomposition <- function(data, entity_var) {
  vars_to_check <- c("q0", "q1", "q2", "entropy", "ldi",
                     "population_density", "av_household_size", "demographic_weight",
                     "unemployment_rate", "income_median",
                     "migrants_interprovincial_percentage",
                     "migrants_extern_percentage")
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
    )%>%
    select(variable, pct_within, pct_between) %>%
    pivot_longer(cols = c(pct_within, pct_between), names_to = "component", values_to = "pct") %>%
    ggplot(aes(x = reorder(variable, pct), y = pct, fill = component)) +
    geom_col() +
    coord_flip() +
    labs(title = "Within vs. Between Variance Share", x = NULL, y = "% of total variance") +
    theme_minimal()
}

variance_decomposition(data_norm_reg, "GEO_ID")

### panel data analysis

### pooled ols for q0, q1, q2

create_pooled_models<- function(pdata){
  pool_model1 <- plm(
    q1 ~
      population_density +
      demographic_weight+
      av_household_size +
      income_median +
      unemployment_rate +
      migrants_interprovincial_percentage +
      migrants_extern_percentage,
    data = pdata,
    model = "pooling"
  )
  
  pool_model2 <- plm(
    q2 ~
      population_density +
      demographic_weight+
      av_household_size +
      income_median +
      unemployment_rate +
      migrants_interprovincial_percentage +
      migrants_extern_percentage,
    data = pdata,
    model = "pooling"
  )
  
  pool_model0 <- tryCatch(
    plm(
      q0 ~
        population_density +
        demographic_weight+
        av_household_size +
        income_median +
        unemployment_rate +
        migrants_interprovincial_percentage +
        migrants_extern_percentage,
      data = pdata,
      model = "pooling"
    ),
    error = function(e) {
      message("Model q0 failed: ", e$message)
      NULL
    }
  )
  list( q0 = pool_model0, q1 = pool_model1, q2 = pool_model2 )
  
}

pooled_reg <- create_pooled_models(pdata_reg)

### models with geo/time fixed/random/ mixed effects for q0, q1, q2

create_effect_models <- function(pdata, outcomes = c("q0", "q1", "q2")){
  x <- paste(
    "population_density",
    "demographic_weight",    
    "av_household_size",
    "income_median",    
    "unemployment_rate",
    "migrants_interprovincial_percentage",
    "migrants_extern_percentage",
    sep = " + "
  )
  models <- list()
  for (y in outcomes) {
    models[[paste0("fixed_individual_", y)]] <- plm(
      as.formula(paste(y, "~", x)),
      data = pdata,
      model = "within",
      effect = "individual"
    )
    models[[paste0("fixed_time_", y)]] <- plm(
      as.formula(paste(y, "~", x)),
      data = pdata,
      model = "within",
      effect = "time"
    )
    models[[paste0("fixed_twoway_", y)]] <- plm(
      as.formula(paste(y, "~", x)),
      data = pdata,
      model = "within",
      effect = "twoway"
    )
    models[[paste0("geo_fixed_", y)]] <- plm(
      as.formula(
        paste(y, "~", x, "+ factor(CENSUS_YEAR)")
      ),
      data = pdata,
      model = "within",
      effect = "individual"
    )
    models[[paste0("geo_random_", y)]] <- plm(
      as.formula(
        paste(y, "~", x, "+ factor(CENSUS_YEAR)")
      ),
      data = pdata,
      model = "random",
      effect = "individual",
      random.method = "walhus"
    )
    models[[paste0("time_fixed_", y)]] <- plm(
      as.formula(
        paste(y, "~", x, "+ factor(GEO_ID)")
      ),
      data = pdata,
      model = "within",
      effect = "time"
    )
    models[[paste0("time_random_", y)]] <- plm(
      as.formula(
        paste(y, "~", x, "+ factor(GEO_ID)")
      ),
      data = pdata,
      model = "random",
      effect = "time",
      random.method = "walhus"
    )
    models[[paste0("random_twoway_", y)]] <- plm(
      as.formula(paste(y, "~", x)),
      data = pdata,
      model = "random",
      effect = "twoway",
      random.method = "walhus")
  }
  models
}

effect_models_reg <- create_effect_models(pdata_reg)

### test for effects -> which model is best? for q0, q1, q2

run_panel_tests <- function(pooled_models, effect_models, outcomes = c("q0", "q1", "q2")) {
  
  results <- lapply(outcomes, function(y) {
    fixed_individual <- effect_models[[paste0("fixed_individual_", y)]] 
    fixed_time <- effect_models[[paste0("fixed_time_", y)]] 
    fixed_twoway <- effect_models[[paste0("fixed_twoway_", y)]] 
    geo_fixed <- effect_models[[paste0("geo_fixed_", y)]] 
    geo_random <- effect_models[[paste0("geo_random_", y)]] 
    time_fixed <- effect_models[[paste0("time_fixed_", y)]] 
    time_random <- effect_models[[paste0("time_random_", y)]] 
    random_twoway <-effect_models[[paste0("random_twoway_", y)]]
    pooled_model <- pooled_models[[y]]
    
    tibble(
      outcome = y,
      test = c("pFtest: region FE vs pooled",
               "pFtest: twoway FE vs time FE",
               "pFtest: time FE vs pooled",
               "pFtest: twoway FE vs region FE",
               "pFtest: twoway FE vs pooled",
               "phtest: region (time dummies)",
               "phtest: time (region dummies)",
               "phtest: twoway FE vs twoway RE"),
      p_value = c(
        pFtest(fixed_individual, pooled_model)$p.value,
        pFtest(fixed_twoway, fixed_time)$p.value,
        pFtest(fixed_time, pooled_model)$p.value,
        pFtest(fixed_twoway, fixed_individual)$p.value,
        pFtest(fixed_twoway, pooled_model)$p.value,
        phtest(geo_fixed, geo_random)$p.value,
        phtest(time_fixed, time_random)$p.value,
        phtest(fixed_twoway, random_twoway)$p.value
      )
    )
  })
  results
}

display_summary_table <- function(results){
  all_results <- bind_rows(results)
  summary_table <- all_results %>% 
    mutate( 
      p_value = round(p_value, 5), 
      sig = case_when( 
        p_value < 0.001 ~ "***", 
        p_value < 0.01 ~ "**", 
        p_value < 0.05 ~ "*", 
        p_value < 0.1 ~ "+", 
        TRUE ~ "" 
      ), 
      display = paste0(p_value, sig) 
    ) %>% 
    select(outcome, test, display) %>% 
    pivot_wider( 
      names_from = outcome, 
      values_from = display 
    )
  summary_table
}

tests_reg <- run_panel_tests(pooled_reg, effect_models_reg)

tests_reg

display_summary_table(tests_reg)

### create mixed effect model for q1 (region random + time fixed)

create_mixed_model_region_random <- function(pdata, outcome){
  formula <- as.formula(
    paste(
      outcome,
      "~ population_density +
         demographic_weight +
         av_household_size +
         income_median +
         unemployment_rate +
         migrants_interprovincial_percentage +
         migrants_extern_percentage +
         factor(CENSUS_YEAR) +
         (1 | GEO_ID)"
    )
  )
  
  lmer(formula, data = pdata)
}

effect_models_reg$mixed_region_random_q1 <- create_mixed_model_region_random(pdata_reg, "q1")

### create hybrid (Mundlak) models to differentiate between and within effects

create_hybrid_models <- function (data_norm, outcomes = c("q0", "q1", "q2")){
  vars_to_decompose <- c("population_density",
                         "demographic_weight",    
                         "av_household_size",
                         "income_median",    
                         "unemployment_rate",
                         "migrants_interprovincial_percentage",
                         "migrants_extern_percentage")
  
  df_mundlak <- data_norm %>%
    group_by(GEO_ID) %>%
    mutate(across(all_of(vars_to_decompose),
                  list(between = ~mean(., na.rm = TRUE),
                       within  = ~. - mean(., na.rm = TRUE)),
                  .names = "{.col}_{.fn}")) %>%
    ungroup()
  
  models <- list()
  for (y in outcomes) {
    models[[paste0("hybrid_", y)]] <- lmer(as.formula(paste(
      y, "~",
      paste0(vars_to_decompose, "_within", collapse = " + "), "+",
      paste0(vars_to_decompose, "_between", collapse = " + "),
      "+ factor(CENSUS_YEAR) + (1 | GEO_ID)"
    )), data = df_mundlak)
  }
  models
}

effect_models_reg <- c(effect_models_reg, create_hybrid_models(data_norm_reg))

modelsummary(effect_models_reg,
             statistic = NULL,
             stars = TRUE) 

### create regression tables 

list_models_reg <- list(
  "Linguistic diversity q=0" = list ("RE two-ways" = effect_models_reg$random_twoway_q0, 
                                     "FE two-ways" = effect_models_reg$fixed_twoway_q0,
                                     "Hybrid" = effect_models_reg$hybrid_q0,
                                     "Pooled OLS" = pooled_reg$q0),
  "Linguistic diversity q=1" =list("Mixed region random + time fixed" = effect_models_reg$mixed_region_random_q1,
                                   "FE two-ways" = effect_models_reg$fixed_twoway_q1,
                                   "Hybrid" = effect_models_reg$hybrid_q1,
                                   "Pooled OLS" = pooled_reg$q1),
  "Linguistic diversity q=2" =list("FE two-ways" = effect_models_reg$fixed_twoway_q2,
                                   "Hybrid" = effect_models_reg$hybrid_q2,
                                   "Pooled OLS" = pooled_reg$q2)
)

modelsummary(list_models_reg,
             statistic = NULL,
             stars = TRUE,
             shape = "cbind")

list_models_reg_final <- list(
  "Linguistic diversity q=0" = list ("RE two-ways" = effect_models_reg$random_twoway_q0),
  "Linguistic diversity q=1" =list("Mixed region random + time fixed" = effect_models_reg$mixed_region_random_q1),
  "Linguistic diversity q=2" =list("FE two-ways" = effect_models_reg$fixed_twoway_q2)
)

modelsummary(list_models_reg_final,
             statistic = NULL,
             stars = TRUE,
             shape = "cbind")

models0 <- list("Linguistic diversity q=0" = list (
  "pooled OLS" = pooled_reg$q0,
  "FE two-ways" = effect_models_reg$fixed_twoway_q0,
  "RE two-ways" = effect_models_reg$random_twoway_q0,
  "Hybrid" = effect_models_reg$hybrid_q0
))

modelsummary(models0,
             statistic = NULL,
             stars = TRUE,
             shape = "cbind",
             #output = "latex"
             )

models1 <- list("Linguistic diversity q=1" =list(
  "pooled OLS" = pooled_reg$q1,
  "FE two-ways" = effect_models_reg$fixed_twoway_q1,
  "Mixed region random + time fixed (q1)" = effect_models_reg$mixed_region_random_q1,
  "Hybrid" = effect_models_reg$hybrid_q1
))


modelsummary(models1,
             statistic = NULL,
             stars = TRUE,
             shape = "cbind",
             #output = "latex"
             )

models2 <- list("Linguistic diversity q=2" =list(
  "pooled OLS" = pooled_reg$q2,
  "FE two-ways" = effect_models_reg$fixed_twoway_q2
))

modelsummary(models2,
             statistic = NULL,
             stars = TRUE,
             shape = "cbind",
             #output = "latex"
             ) 

### plot outcome (coefficents)

coef_df <- imap_dfr(list_models_reg_final, function(model_list, outcome) {
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
  "RE two-ways",
  "FE two-ways",
  "Mixed region random + time fixed"
)

coef_df <- imap_dfr(list_models_reg_final, function(model_list, outcome) {
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
  #guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
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

ggsave("figures/coefficient_plot_revised.png", plot = coef_plot, width = 8,
       height = 4,
       dpi = 600)
