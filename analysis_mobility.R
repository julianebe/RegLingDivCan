library(dplyr)


data_var <- read.csv("data_socioeconomic_variables.csv")
df_mob1 <- data_var[, c("GEO_CODE", "GEO_NAME", "GEO_ID", "GEO_LEVEL", "CENSUS_YEAR", "non_movers_percentage", "migrants_intraprovincial_percentage", "migrants_interprovincial_percentage", "migrants_extern_percentage")]

df_mob5 <- read.csv("data_mobility_5years.csv")


df_mob <- df_mob1 %>%
  inner_join(df_mob5,
    by = c("GEO_ID", "CENSUS_YEAR", "GEO_NAME", "GEO_CODE", "GEO_LEVEL"),
    suffix = c("_mob1", "_mob5")
  )

cor(df_mob$migrants_extern_percentage_mob1, df_mob$migrants_extern_percentage_mob5, use = "complete.obs")
cor(df_mob$migrants_interprovincial_percentage_mob1, df_mob$migrants_interprovincial_percentage_mob5, use = "complete.obs")
