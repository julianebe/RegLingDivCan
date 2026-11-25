# Regional Linguistic Diversity in Canada
## 📄 Measuring linguistic diversity in Canada, 1991-2021 - A panel data analysis on provinces and territories

This repository contains the data and the code used in and for the analysis described in the paper **“Measuring linguistic diversity in Canada, 1991-2021 - A panel data analysis on provinces and territories”** presented at **Computational Humanities Research 2027** in Manchester.

### 🔎 Abstract

Since 1988, Canada has officially pursued a policy of multiculturalism and is committed to preserving
and promoting its diverse cultural heritage. Linguistic diversity can be seen as an indicator of cultural
diversity. To draw a nuanced picture of Canadian linguistic diversity between 1991 and 2021, this study
differentiates, measures, and compares three diversity levels using the Hill-numbers framework and three
diversity scales (alpha, beta, gamma diversity) based on L1 speaker numbers. Subsequently, a panel
data analysis is conducted using variables relevant from a sociolinguistic perspective. Overall, Canadian
linguistic diversity increased over the three decades examined. Ontario and British Columbia are the most
diverse regions, while the lowest levels of linguistic diversity are found in northern Canada or along the
east coast, depending on which level linguistic diversity is measured. Average household size, median
income, and unemployment rate, all of which link to the mobility and contact patterns of speakers, were
significantly associated with abundance-sensitive measures of linguistic diversity; additional associations
varied across Hill-number orders.

## 📁 Repository Structure

### 🛢️ `Data`
* `data_census_country.csv`
  contains preprocessed language data collected from Statistics Canada
  - CENSUS_YEAR: The year the data was collected
  - GEO_LEVEL: Level of the geographic area -> here Country
  - GEO_NAME: Name of the geographical area (adapted from 2021) -> here Canada
  - GEO_ID: Two-letters identifier following ISO 3166-1 alpha-2 code -> here CA
  - GEO-CODE: numerical identifier given by Statistics Canada (adapted from 2021) -> here 1
  - LANGUAGE_NAME: Name of language unit given by Statistics Canada within the correspondend census wave (not standardized)
  - LANGUAGE_ID: ISO 639 language code of language unit if existant, else "x"
  - GLOTTOCODE: Glottocode of language unit if existant, else "xxxx"
  - L_ID: Identifier of language unit as combination of "LANGUAGE-ID"-"GLOTTOCODE"
  - LANGUAGE_GROUP: Indicates whether language unit corresponds to official, Indigenous or immigrant languages (offi/indi/immi)
  - COUNT_TOTAL: total count of L1 speakers of the language unit
  - region_year_total: sum of all L1 speakers based in the corresponding geographic area within each census waves represented in the data set

* `data_census_regions.csv`
  contains preprocessed language data collected from Statistics Canada
  - CENSUS_YEAR: The year the data was collected
  - GEO_LEVEL: Level of the geographic area -> here Province/Territory
  - GEO_NAME: Name of the geographical area (adapted from 2021)
  - GEO_ID: Two-letters identifier following ISO 3166-2:CA (adapted from 2021)
  - GEO-CODE: numerical identifier given by Statistics Canada (adapted from 2021)
  - LANGUAGE_NAME: Name of language unit given by Statistics Canada within the correspondend census wave (not standardized)
  - LANGUAGE_ID: ISO 639 language code (3 letters) of language unit if existant, else "x"
  - GLOTTOCODE: Glottocode of language unit if existant, else "xxxx"
  - L_ID: Identifier of language unit as combination of "LANGUAGE-ID"-"GLOTTOCODE"
  - LANGUAGE_GROUP: Indicates whether language unit corresponds to official, Indigenous or immigrant languages (offi/indi/immi)
  - COUNT_TOTAL: total count of L1 speakers of the language unit
  - region_year_total: sum of all L1 speakers based in the corresponding geographic area within each census waves represented in the data set
  - proportion: proportion of L1 speakers of language unit in a region
  - demographic_weight: proportion of population living in a region compared to the total population of Canada

* `data_socioeconomic_variables.csv`
  contains preprocessed data on socio-economic variables collected from Statistics Canada
  - CENSUS_YEAR: The year the data was collected
  - GEO_LEVEL: Level of the geographic area
  - GEO_NAME: Name of the geographical area (adapted from 2021)
  - GEO_ID: Two-letters identifier following ISO 3166 (ISO 3166-1 alpha-2 and ISO 3166-2:CA; adapted from 2021)
  - GEO-CODE: numerical identifier given by Statistics Canada (adapted from 2021)
  - LANGUAGE_NAME: Name of language unit given by Statistics Canada within the correspondend census wave (not standardized)
  - population_total: total population per geographic area
  - land_area: size of geographic area
  - population_density: population density of geographic area
  - median_age: median age of population of geographic area
  - av_household_size: average household size of geographic area
  - income_average: average income in geographic area
  - income_median: median income in geographic area
  - income_employment_average: average employment income in geographic area
  - income_employment_median: median employment income in geographic area
  - unemployment_rate: unemployment rate in geographic area
  - university_degree_percentage: percentage of population holding a university degree in geographic area
  - non_movers_percentage: percentage of population that did not move within the year before the census per geographic area
  - migrants_intraprovincial_percentage: percentage of population that moved within the province/ territory within the year before the census per geographic area
  - migrants_interprovincial_percentage: percentage of population that moved within Canada across provinces/ territories within the year before the census per geographic area
  - migrants_extern_percentage: percentage of population that migrated from abroad to Canada within the year before the census per geographic area

* `data_mobility_5years.csv`
  contains preprocessed data on mobility status in comparison to five years before the census_wave collected from Statistics Canada (mobility status changes compared to the last census)
  - CENSUS_YEAR: The year the data was collected
  - GEO_LEVEL: Level of the geographic area
  - GEO_NAME: Name of the geographical area (adapted from 2021)
  - GEO_ID: Two-letters identifier following ISO 3166 (ISO 3166-1 alpha-2 and ISO 3166-2:CA; adapted from 2021)
  - GEO-CODE: numerical identifier given by Statistics Canada (adapted from 2021)
  - LANGUAGE_NAME: Name of language unit given by Statistics Canada within the correspondend census wave (not standardized)
  - non_movers_percentage: percentage of population that did not move within five years before the census per geographic area
  - migrants_intraprovincial_percentage: percentage of population that moved within the province/ territory within five years before the census per geographic area
  - migrants_interprovincial_percentage: percentage of population that moved within Canada across provinces/ territories within five years before the census per geographic area
  - migrants_extern_percentage: percentage of population that migrated from abroad to Canada within five years before the census per geographic area

* `div_levels_reg.csv`
  Output of regional diversity analysis: Levels of diversity
  - CENSUS_YEAR: year of the corresponded diversity
  - GEO_ID: Two-letters identifier following ISO 3166 (ISO 3166-1 alpha-2 and ISO 3166-2:CA; adapted from 2021)
  - demographic_weight (from socioeconomic variables)
  - q0: diversity following the concept of linguistic richness/ Hill numbers framework q=0 following Hill (1973)
  - entropy: diversity calculated from Shannon entropy following Shannon (1948)
  - q1: diversity following the Hill numbers framework q=1 ( = exponent shannon entropy) following Hill (1973)
  - ldi: Linguistic diversity index following Greenberg (1956)
  - simpson: simpson diversity index following Simpson (1949)
  - q2: diversity following the Hill numbers framework q=2 (= inverse simpson) following Hill (1973)


* `div_scales_reg.csv`
  Output of regional diversity analysis: Scales of diversity (alpha, beta, gamma)
  - CENSUS_YEAR: year of the corresponded diversity
  - alpha_q0
  - alpha_q1
  - alpha_q2
  - beta_q0
  - beta_q1
  - beta_q2
  - gamma_q0
  - gamma_q1
  - gamma_q0


### 💻 `Analysis`

* `analysis_diversity.R`
  - contains the script to analyze regional linguistic diversity in Canada based on various diversity indices
  - contains the script to analyze linguistic diversity in Canada on different scales based diversity components
  - contains the script for the robustness check of Canadian linguistic diversity on a consistent subset  

* `analysis_speaker_coverage.R`
  - contains the script to analyze the speaker coverage of the language units in the dataset

* `analysis_panel.R`
  - contains the correlation analysis of the socioeconomic variables
  - contains the script for the panel data analysis 

* `analysis_mobility.R`
  - contains the correlation analysis of mobility status the year before vs. five years before the census

## ⚠️ Disclosure of Delegation to Generative AI (GAIDeT)

The author declares the use of generative AI in the research process. According to the GAIDeT taxonomy (Suchikova et al. 2026), code generation and optimization were delegated to GAI tools under full human supervision. The GAI tool used were ChatGPT and Claude.ai. Responsibility for the final manuscript lies entirely with the author. GAI tools are not listed as authors and do not bear responsibility for the final outcomes.

## 📜 License

This project is licensed under the **Creative Commons Attribution 4.0 International (CC BY 4.0)** license.   
The project uses data compiled from data collected by Statistics Canada within the Census of Population programms 1991 to 2021. Statistics Canada has an open licence on their data.   
The datasets containing the information on the language units was created and (pre)processed by Juliane Benson based on data by Statistics Canada.  
The datasets containing socioeconomic variables was merged and preprocessed by Juliane Benson based on data by Statistics Canada.   
The datasets containing results of the analysis of regional linguistic diversity in Canada. 

## 🎓 Acknowledgments
This research was funded by WWTF (grant number ICT23-012). It is a part of the  [DIGILINGDIV project](https://digiling.univie.ac.at/digilingdiv/).

## 📬 Contact
If you have any questions about the code or analysis, feel free to contact: **[juliane.rabea.benson@univie.ac.at]**
