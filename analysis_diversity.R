library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(rnaturalearth)
library(sf)

df_reg <- read.csv("data_census_regions.csv")

df_reg <- df_reg %>%
  mutate(proportion = as.numeric(gsub(",", ".", proportion)))

df_reg <- df_reg %>%
  mutate(demographic_weight = as.numeric(gsub(",", ".", demographic_weight)))


### calculate levels of linguistic diversity
calculate_div_levels <- function(df) {
  df %>%
    filter(proportion > 0) %>%
    group_by(CENSUS_YEAR, GEO_ID) %>%
    summarise(
      demographic_weight = first(demographic_weight), 
      q0 = n_distinct(L_ID),
      entropy = -sum(proportion * log(proportion)),
      q1 = exp(entropy),
      ldi = 1 - sum(proportion^2),
      simpson = sum(proportion^2),
      q2 = 1 / (sum(proportion^2)),
      .groups = "drop"
    )
}

div_levels_reg <- calculate_div_levels(df_reg)
div_levels_reg


### calculate diversity scales
calculate_div_scales <- function(df) { 
  regional <- df %>% 
    filter(proportion > 0) %>% 
    group_by(CENSUS_YEAR, GEO_ID) %>% 
    summarise( 
      demographic_weight = first(demographic_weight), 
      richness = n_distinct(L_ID),
      entropy = -sum(proportion * log(proportion)),
      hill_q1 = exp(entropy),
      ldi = 1 - sum(proportion^2),
      simpson = sum(proportion^2),
      hill_q2 = 1 / (1 - ldi),
      .groups = "drop" 
    ) 
  alpha <- regional %>% 
    group_by(CENSUS_YEAR) %>% 
    summarise( 
      alpha_q0 = mean(richness),
      alpha_entropy = sum(
        demographic_weight * entropy
      ),
      alpha_q1 = exp(alpha_entropy),
      alpha_ldi = sum(demographic_weight^2*(ldi)),
      alpha_q2 = sum(demographic_weight^2) / sum(demographic_weight^2*(simpson)),
      .groups = "drop" 
    ) 
  gamma <- df %>% 
    filter(COUNT_TOTAL > 0) %>%
    group_by(CENSUS_YEAR, L_ID) %>%
    summarise(
      count_total = sum(COUNT_TOTAL),
      .groups = "drop"
    ) %>%
    group_by(CENSUS_YEAR) %>%
    mutate(
      pooled_proportion = count_total / sum(count_total)
    ) %>%
    summarise(
      gamma_q0 = n_distinct(L_ID),
      gamma_entropy = -sum(
        pooled_proportion * log(pooled_proportion)
      ),
      gamma_q1 = exp(gamma_entropy),
      gamma_ldi = 1 - sum(pooled_proportion^2), 
      gamma_q2 = 1 / (1 - gamma_ldi),
      .groups = "drop"
    ) 
  results <- alpha %>% 
    left_join(gamma, by = "CENSUS_YEAR") %>% 
    mutate( 
      beta_q0 = gamma_q0 / alpha_q0,
      beta_q1 = gamma_q1 / alpha_q1,
      beta_entropy = gamma_entropy - alpha_entropy,
      beta_q2 = gamma_q2 / alpha_q2, 
      beta_ldi = (gamma_ldi - alpha_ldi)/(1-alpha_ldi)
    ) 
}

div_scales_reg <- calculate_div_scales(df_reg)
div_scales_reg

write.csv(div_levels_reg, "div_levels_reg.csv", row.names = FALSE)
write.csv(div_scales_reg, "div_scales_reg.csv", row.names = FALSE)


### visualize line plots

colors <- c(
  AB = rgb(0.12156863, 0.46666667, 0.70588235),
  BC = rgb(0.68235294, 0.78039216, 0.90980392),
  MB = rgb(0.17254902, 0.62745098, 0.17254902),
  NB = rgb(0.83921569, 0.15294118, 0.15686275),
  NL = rgb(1.00000000, 0.59607843, 0.58823529),
  NS = rgb(0.77254902, 0.69019608, 0.83529412),
  NT = rgb(0.54901961, 0.33725490, 0.29411765),
  NU = rgb(0.89019608, 0.46666667, 0.76078431),
  ON = rgb(0.96862745, 0.71372549, 0.82352941),
  PE = rgb(0.78039216, 0.78039216, 0.78039216),
  QC = rgb(0.73725490, 0.74117647, 0.13333333),
  SK = rgb(0.09019608, 0.74509804, 0.81176471),
  YT = rgb(0.61960784, 0.85490196, 0.89803922)
)


div_plot <- function(df_levels, df_scales, level, title){
  valid_levels <- c("q0", "q1", "q2", "entropy", "ldi")
  if (!level %in% valid_levels) { 
    stop( "level must be one of: ", 
          paste(valid_levels, collapse = ", ") 
    ) 
  }
  alpha_var <- paste0("alpha_", level) 
  beta_var <- paste0("beta_", level) 
  gamma_var <- paste0("gamma_", level) 
  required_levels <- c("CENSUS_YEAR", "GEO_ID", level) 
  required_scales <- c( "CENSUS_YEAR", 
                        alpha_var, 
                        beta_var, 
                        gamma_var 
  )
  region_labels_start <- df_levels %>% 
    group_by(GEO_ID) %>% 
    filter(CENSUS_YEAR == min(CENSUS_YEAR)) %>% 
    ungroup() 
  region_labels_end <- df_levels %>% 
    group_by(GEO_ID) %>% 
    filter(CENSUS_YEAR == max(CENSUS_YEAR)) %>% 
    ungroup()
  y_label <- switch(
    level,
    "q0" = "Effective number (Language units or Regions)",
    "q1" = "Effective number (Language units or Regions)",
    "q2" = "Effective number (Language units or Regions)",
    "entropy" = "Linguistic entropy",
    "ldi" = "Linguistic Diversity Index"
  )
  ggplot() +
    geom_line( 
      data = df_levels, 
      aes( x = CENSUS_YEAR, 
           y = .data[[level]], 
           color = GEO_ID, 
           group = GEO_ID 
      ), 
      linewidth = 0.8, 
      alpha = 0.7, 
      show.legend = FALSE 
    ) +
    scale_color_manual(values = colors) +
    geom_line(
      data = df_scales,
      aes(x = CENSUS_YEAR,
          y = .data[[alpha_var]]),
      color = "black",
      linewidth = 1.2
    ) +
    geom_text_repel(
      data = tail(df_scales, 1),
      aes(x = CENSUS_YEAR,
          y = .data[[alpha_var]],
          label = "α"),
      nudge_x = 1,
      color = "black",
      size = 5
    ) +
    geom_line(
      data = df_scales,
      aes(x = CENSUS_YEAR,
          y = .data[[beta_var]]),
      color = "red",
      linewidth = 1.2
    ) +
    geom_text_repel(
      data = tail(df_scales, 1),
      aes(x = CENSUS_YEAR,
          y = .data[[beta_var]],
          label = "β"),
      nudge_x = 1,
      color = "red",
      size = 5
    ) +
    geom_line(
      data = df_scales,
      aes(x = CENSUS_YEAR,
          y = .data[[gamma_var]]),
      color = "blue",
      linewidth = 1.2
    ) +
    geom_text_repel(
      data = tail(df_scales, 1),
      aes(x = CENSUS_YEAR,
          y = .data[[gamma_var]],
          label = "γ"),
      nudge_x = 1,
      color = "blue",
      size = 5
    )+
    coord_cartesian(clip = "off") +
    scale_x_continuous(
      expand = expansion(mult = c(0.08, 0.15))
    ) +
    labs(
      x = "Year",
      y = y_label,
      title = title
    ) +
    theme_minimal()+
    theme(
      legend.position = "none",
      plot.margin = margin(5.5, 40, 5.5, 5.5)
    )
}

p_reg_q0 <- div_plot(div_levels_reg, div_scales_reg, "q0", "Linguistic diversity, Richness/ q=0")
p_reg_q1 <- div_plot(div_levels_reg, div_scales_reg, "q1", "Linguistic diversity, q=1")
p_reg_q2 <- div_plot(div_levels_reg, div_scales_reg, "q2", "Linguistic diversity, q=2")

p_reg_q0
ggsave(
  filename = "figures/plot_line_q0.png",
  width = 12,
  height = 8,
  dpi = 600
)

p_reg_q1
ggsave(
  filename = "figures/plot_line_q1.png",
  width = 12,
  height = 8,
  dpi = 600
)

p_reg_q2
ggsave(
  filename = "figures/plot_line_q2.png",
  width = 12,
  height = 8,
  dpi = 600
)

### create map for color identification
canada_sf <- ne_states(country = "Canada", returnclass = "sf")

labels <- setNames(canada_sf$name, canada_sf$postal)

canada_sf <- canada_sf %>%
  mutate(fill_col = colors[postal])

plot_map <- ggplot(canada_sf) +
  geom_sf(aes(fill = postal), color = "black", linewidth = 0.2) +
  scale_fill_manual(values = colors, labels = labels) +
  theme_void() +
  labs(fill = "Region")

plot_map
ggsave(
  filename = "map_canada_regions.png",
  width = 12,
  height = 8,
  dpi = 600
)

### create combined plots
p4 <- plot_map +
  coord_sf(expand = FALSE)

wrap_plots(
  p_reg_q0, p4,
  p_reg_q1, p_reg_q2,
  ncol = 2
) +
  plot_layout(
    widths = c(1, 1),
    guides = "collect") &
  theme(
    legend.position = "bottom")

ggsave(
  filename = "figures/plot_combined_diversity.png",
  width = 12,
  height = 8,
  dpi = 600
)

##### Robustness Check #####

#### check diversity for units that are reported for the whole time period in any region (constant data for all census waves)

n_years <- n_distinct(df_reg$CENSUS_YEAR)

ids_all_years <- df_reg %>%
  filter(COUNT_TOTAL > 0) %>% 
  group_by(L_ID) %>%
  summarise(n_years_present = n_distinct(CENSUS_YEAR), .groups = "drop") %>%
  filter(n_years_present == n_years) %>%
  pull(L_ID)

df_robust <- df_reg %>% 
  filter(L_ID %in% ids_all_years)%>% 
  filter(COUNT_TOTAL > 0) %>% 
  group_by(CENSUS_YEAR, GEO_ID) %>%
  mutate(
    region_year_total = sum(COUNT_TOTAL),
    proportion = COUNT_TOTAL / region_year_total
  ) %>%
  ungroup()

div_levels_rob <- calculate_div_levels(df_robust)
div_levels_rob

div_scales_rob <- calculate_div_scales(df_robust)
div_scales_rob

p_rob_q0 <- div_plot(div_levels_rob, div_scales_rob, "q0", "Linguistic diversity, Richness/ q=0 (Robust data)" )
p_rob_q1 <- div_plot(div_levels_rob, div_scales_rob, "q1", "Linguistic diversity, q=1 (Robust data)")
p_rob_q2 <- div_plot(div_levels_rob, div_scales_rob, "q2", "Linguistic diversity, q=2 (Robust data)")

p_rob_q0
ggsave(
  filename = "figures/plot_line_q0_rob.png",
  width = 6,
  height = 4,
  dpi = 600
) 

p_rob_q1
ggsave(
  filename = "figures/plot_line_q1_rob.png",
  width = 6,
  height = 4,
  dpi = 600
) 

p_rob_q2
ggsave(
  filename = "figures/plot_line_q2_rob.png",
  width = 6,
  height = 4,
  dpi = 600
)

### create combined plot
wrap_plots(
  p_rob_q0, p4,
  p_rob_q1, p_rob_q2,
  ncol = 2
) +
  plot_layout(
    widths = c(1, 1),
    guides = "collect") &
  theme(
    legend.position = "bottom")

ggsave(
  filename = "figures/plot_combined_diversity_rob.png",
  width = 12,
  height = 8,
  dpi = 600
)


#### check diversity for units that are reported for the whole time period within each region (constant set of language units for the region)

df_robust_region <- df_reg %>%
  filter(COUNT_TOTAL > 0) %>%
  group_by(GEO_CODE, L_ID) %>%
  filter(
    n_distinct(CENSUS_YEAR) == 7 |
      (GEO_ID == "NU" & n_distinct(CENSUS_YEAR) == 5) # with Nunavut
  ) %>%
  ungroup() %>%
  group_by(CENSUS_YEAR, GEO_ID) %>%
  mutate(
    region_year_total = sum(COUNT_TOTAL),
    proportion = COUNT_TOTAL / region_year_total
  ) %>%
  ungroup()

div_levels_rob_reg <- calculate_div_levels(df_robust_region)
div_levels_rob_reg

div_scales_rob_reg <- calculate_div_scales_w(df_robust_region)
div_scales_rob_reg

p_rob_reg_q0 <- div_plot(div_levels_rob_reg, div_scales_rob_reg, "q0", "Linguistic diversity, Richness/ q=0 (Robust region data)" )
p_rob_reg_q1 <- div_plot(div_levels_rob_reg, div_scales_rob_reg, "q1", "Linguistic diversity, q=1 (Robust region data)")
p_rob_reg_q2 <- div_plot(div_levels_rob_reg, div_scales_rob_reg, "q2", "Linguistic diversity, q=2 (Robust region data)")

p_rob_reg_q0
ggsave(
  filename = "figures/plot_line_q0_rob_reg.png",
  width = 6,
  height = 4,
  dpi = 600
) 

p_rob_reg_q1
ggsave(
  filename = "figures/plot_line_q1_rob_reg.png",
  width = 6,
  height = 4,
  dpi = 600
) 

p_rob_reg_q2
ggsave(
  filename = "figures/plot_line_q2_rob_reg.png",
  width = 6,
  height = 4,
  dpi = 600
)

### create map for color identification
canada_sf <- ne_states(country = "Canada", returnclass = "sf")

labels <- setNames(canada_sf$name, canada_sf$postal)

canada_sf <- canada_sf %>%
  mutate(fill_col = colors[postal])

plot_map <- ggplot(canada_sf) +
  geom_sf(aes(fill = postal), color = "black", linewidth = 0.2) +
  scale_fill_manual(values = colors, labels = labels) +
  theme_void() +
  labs(fill = "Region")

plot_map


### create combined plots
wrap_plots(
  p_rob_reg_q0, p4,
  p_rob_reg_q1, p_rob_reg_q2,
  ncol = 2
) +
  plot_layout(
    widths = c(1, 1),
    guides = "collect") &
  theme(
    legend.position = "bottom")

ggsave(
  filename = "figures/plot_combined_diversity_rob_reg.png",
  width = 12,
  height = 8,
  dpi = 600
)
