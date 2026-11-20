library(dplyr)
library(ggplot2)
library(tidyr)

### explore languages and speaker coverage

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
  YT = rgb(0.61960784, 0.85490196, 0.89803922),
  CA = "black"
)

df_ca <- read.csv("data_census_country.csv")
df_reg <- read.csv("data_census_regions.csv")

data_var2 <- read.csv("data_socioeconomic_variables.csv")

speaker_ca <- df_ca %>%
  distinct(CENSUS_YEAR, GEO_ID, region_year_total)

speaker_reg <- df_reg %>%
  distinct(CENSUS_YEAR, GEO_ID, region_year_total)

population <- data_var %>%
  distinct(CENSUS_YEAR, GEO_ID, GEO_NAME, population_total)


speaker_coverage <- population %>%
  left_join(speaker_ca, by = c("CENSUS_YEAR", "GEO_ID")) %>%
  left_join(speaker_reg, by = c("CENSUS_YEAR", "GEO_ID"))%>%
    mutate(
      region_year_total = coalesce(region_year_total.x, region_year_total.y)
    ) %>%
    select(-region_year_total.x, -region_year_total.y)%>%
  mutate(
    coverage = region_year_total / population_total * 100
  )
  
ggplot(speaker_coverage, aes(
  x = CENSUS_YEAR,
  y = coverage,
  group = GEO_ID,
  color = GEO_ID
)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_color_manual(values = colors, labels = setNames(
    speaker_coverage$GEO_NAME[match(names(colors), speaker_coverage$GEO_ID)],
    names(colors)
  )) +
  theme_minimal() +
  labs(
    title = "Speaker Data Coverage by Region",
    x = "Year",
    y = "Coverage (%)",
    color = "Region"
  )

ggsave(
  filename = "figures/plot_speaker_coverage.png",
  width = 6,
  height = 4,
  dpi = 600
)
