## Ranking herbivore preference for focal macroalgae
# Multiple-choice palatability assays using Turbinaria, Sargassum and Amansia
# We performed a Friedman Test to compare differences in consumption among taxa for multiple-choice assays, and a Kruskall-Wallis test for single-species assays


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


# Data transformation: cafeteria assays -----
# Transform percent consumption using arcsin square root transformation (since values are in percentages and bounded between 0% and 100%)
cafDat <- cafDat %>% 
  dplyr::mutate(percConsump.arcsin = asin(sqrt(percConsump / 100))) # arcsin can only be performed on values from 0 to 1, so must divide percentages by 100

## Check transformation
plot(cafDat$percConsump, cafDat$percConsump.arcsin)

## Check if data are normally distributed
plot(cafDat$percConsump)
hist(cafDat$percConsump, col="lightblue", border="black")
qqnorm(cafDat$percConsump)  
qqline(cafDat$percConsump, col="red", lwd=2)
shapiro.test(cafDat$percConsump)
# Data are not normally distributed!

## Check if transformed data are normally distributed
hist(cafDat$percConsump.arcsin, col="lightblue", border="black")
qqnorm(cafDat$percConsump.arcsin)  
qqline(cafDat$percConsump.arcsin, col="red", lwd=2)
shapiro.test(cafDat$percConsump.arcsin)
# Transformed data are not normally distributed!

sinDat <- sinDat %>% 
  dplyr::mutate(percConsump.arcsin = asin(sqrt(percConsump / 100))) # arcsin can only be performed on values from 0 to 1, so must divide percentages by 100

## Check transformation
plot(sinDat$percConsump, sinDat$percConsump.arcsin)

## Check if data are normally distributed
plot(sinDat$percConsump)
hist(sinDat$percConsump, col="lightblue", border="black")
qqnorm(sinDat$percConsump)  
qqline(sinDat$percConsump, col="red", lwd=2)
shapiro.test(sinDat$percConsump)
# Data are not normally distributed!

## Check if transformed data are normally distributed
hist(sinDat$percConsump.arcsin, col="lightblue", border="black")
qqnorm(sinDat$percConsump.arcsin)  
qqline(sinDat$percConsump.arcsin, col="red", lwd=2)
shapiro.test(sinDat$percConsump.arcsin)
# Transformed data are not normally distributed!



## Format data into wide format (currently in long)
cafDat_wide <- cafDat %>% 
  select(replicate, taxa, percConsump.arcsin) %>% 
  dplyr::mutate(taxa = str_replace_all(taxa, " ", "_")) %>%   # Replaces spaces with underscores
  pivot_wider(names_from = taxa, values_from = percConsump.arcsin)

# Get rid of unnecessary columns
cafDat_simple <- cafDat %>% 
  select(replicate, taxa, percConsump, percConsump.arcsin)

# Currently replicate has 30 levels, but should be 20 levels. could be screwing up Friedman test. Export dataframe and re-import it, so it replicate has 20 levels.
# Export dataframe
write.csv(cafDat_simple, "data/caf_data_friedman.csv", row.names = FALSE)

# Import dataframe
caf_data_friedman <- read_csv("data/caf_data_friedman.csv")
caf_data_friedman$replicate <- as.factor(caf_data_friedman$replicate)
caf_data_friedman$taxa <- as.factor(caf_data_friedman$taxa)

# Ensure each replicate has one observation per algae species
# Count number of observations per replicate and algae species
cafDat_simple %>%
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



## Friedman test: multiple species (arcsin square root transformed data) -----
# Run Friedman test 
# Helps with percentage data (bounded between 0-100%) and skewed data with many 0's
friedman_test(caf_data_friedman, percConsump.arcsin ~ taxa | replicate)
# Poc-hoc pairwise comparisons
caf_data_friedman %>%
  wilcox_test(percConsump.arcsin ~ taxa, paired = TRUE, p.adjust.method = "bonferroni")

# A Friedman test was conducted to compare differences in consumption across the three taxa. The results showed a significant difference between groups (χ²(2) = 26.6, p = 0.000002).
# 	Post-hoc Wilcoxon signed-rank tests with Bonferroni correction revealed significant differences between Amansia and Sargassum (W = 179, p = 0.002) and between Amansia and Turbinaria (W = 188, p = 0.0006). There was a marginal difference in consumption between Sargassum and Turbinaria, though not statistically significant (W = 58, p = 0.088).



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

# Kruskal-Wallis rank sum test: single species (arcsin square root transformed data) -----
# Kruskal-Wallis rank sum test (non-parametric alternative to one-way ANOVA when assumptions are not met)
kruskal.test(percConsump.arcsin ~ taxa, data = sinDat)

# Post-hoc testing using Dunn test (controls for multiple hypothesis testing)
sin.DT <- dunnTest(percConsump.arcsin ~ taxa, data = sinDat,
                   method = "bh") # Benjamini-Hochberg method
sin.DT


# For single species assays, consumption significantly differed between all taxa (Kruskal-Wallis test, p-value = 2.535e-09).
# Amansia-Sargassum (Dunn's test, p = 0.002) 
# Amansia-Turbinaria (Dunn's test, p = 0.0000000009)
# Sargassum-Turbinaria (Dunn's test, p = 0.002)