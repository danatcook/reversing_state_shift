## Ranking herbivore preference for focal macroalgae
# For multiple-choice palatability assays using Turbinaria, Sargassum and Amansia, we performed a Friedman Test to compare differences in consumption among taxa and Wilcoxon signed-rank tests with Bonferroni correction was computed for post hoc analysis
# For single-species palatability assays using Turbinaria, Sargassum and Amansia, we performed a Kruskall-Wallis test to compare differences in consumption among taxa and Dunn’s test with Benjamini-Hochberg adjustments 


# Packages -----
library(readr)
library(tidyverse)
library(janitor)
library(ggplot2)
library(PMCMRplus) # for post-hoc testing for Friedman test
library(ggpubr)
library(rstatix)
library(FSA) # Dunn's test

# Data ---------------------------------------------
palDat <- read_csv("data/palatabilityDat.csv")

palDat$replicate <- as.factor(palDat$replicate)
palDat$taxa <- as.factor(palDat$taxa)
palDat$treatment <- as.factor(palDat$treatment)
palDat$assay_style <- as.factor(palDat$assay_style)

# cafeteria-style data
cafDat <- palDat %>% 
  filter(assay_style == "Cafeteria") %>% 
  filter(treatment == "Exposed")

# single-species data
sinDat <- palDat %>% 
  filter(assay_style == "Single species") %>% 
  filter(treatment == "Exposed")

# Data wrangling: calculate raw and percent consumption for cafeteria and single species ------

# Calculate raw amount consumed (initial weight - final weight) and percent consumed [(initial-final) / initial x 100%] 
cafDat <- cafDat %>% 
  select(replicate, taxa, assay_style, treatment, initial_weight_g, final_weight_g) %>% 
  mutate(rawConsump = initial_weight_g - final_weight_g) %>% 
  mutate(percConsump = abs((initial_weight_g - final_weight_g) / initial_weight_g * 100))

# Calculate raw amount consumed (initial weight - final weight) and percent consumed [(initial-final) / initial x 100%] 
sinDat <- sinDat %>% 
  select(replicate, taxa, assay_style, treatment, initial_weight_g, final_weight_g) %>% 
  mutate(rawConsump = initial_weight_g - final_weight_g) %>% 
  mutate(percConsump = abs((initial_weight_g - final_weight_g) / initial_weight_g * 100))


# ## Format data into wide format (currently in long)
# cafDat_wide <- cafDat %>% 
#   select(replicate, taxa, percConsump) %>% 
#   dplyr::mutate(taxa = str_replace_all(taxa, " ", "_")) %>%   # Replaces spaces with underscores
#   pivot_wider(names_from = taxa, values_from = percConsump)

# Get rid of unnecessary columns
cafDat_simple <- cafDat %>% 
  select(replicate, taxa, percConsump)

# Get rid of unnecessary columns
sinDat_simple <- sinDat %>% 
  select(replicate, taxa, percConsump)

# Currently replicate has 30 levels, but should be 20 levels. could be screwing up Friedman test. Export dataframe and re-import it, so it replicate has 20 levels. Workaround to ensure 20 levels for 'replicate' to run Friedman test, which checks for correct number of replicates.
# Export dataframe
write.csv(cafDat_simple, "data/caf_data_friedman.csv", row.names = FALSE)


# Import dataframe
caf_data_friedman <- read_csv("data/caf_data_friedman.csv")
caf_data_friedman$replicate <- as.factor(caf_data_friedman$replicate)
caf_data_friedman$taxa <- as.factor(caf_data_friedman$taxa)

# Ensure each replicate has one observation per algae species
# Count number of observations per replicate and algae species
caf_data_friedman %>%
  group_by(replicate, taxa) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  count(n)

## Friedman test: multiple species (non-transformed data) -----
# Run Friedman test
friedman_test(caf_data_friedman, percConsump ~ taxa | replicate)
# Poc-hoc pairwise comparisons
caf_data_friedman %>%
  wilcox_test(percConsump ~ taxa, paired = TRUE, p.adjust.method = "bonferroni")

# A Friedman test was conducted to compare differences in consumption across the three taxa. The results showed a significant difference between groups (χ²(2) = 26.6, p = 0.000002).
# 	Post-hoc Wilcoxon signed-rank tests with Bonferroni correction revealed significant differences between Amansia and Sargassum (W = 179, p = 0.002) and between Amansia and Turbinaria (W = 189, p = 0.0005). There was a marginal difference in consumption between Sargassum and Turbinaria, though not statistically significant (W = 60, p = 0.055).


# Kruskal-Wallis rank sum test: single species (non-transformed data) -----
# Kruskal-Wallis rank sum test (non-parametric alternative to one-way ANOVA when assumptions are not met)
kruskal.test(percConsump ~ taxa, data = sinDat)

# Post-hoc testing using Dunn test (controls for multiple hypothesis testing)
sin.DT <- dunnTest(percConsump ~ taxa, data = sinDat,
                   method = "bh") # Benjamini-Hochberg method
sin.DT


# For single species assays, consumption significantly differed between all taxa (Kruskal-Wallis test, p-value = 2.535e-09).
# Amansia-Sargassum (Dunn's test, p = 0.002) 
# Amansia-Turbinaria (Dunn's test, p = 0.0000000009)
# Sargassum-Turbinaria (Dunn's test, p = 0.002)
