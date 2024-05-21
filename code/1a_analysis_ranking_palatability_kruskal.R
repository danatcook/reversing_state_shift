## Ranking herbivore preference for focal macroalgae 
# Palatability assays using Turbinaria, Sargassum and Amansia
# We performed a Kruskal-Wallis to compare differences in consumption among taxa for each assay; Dunn’s test with Benjamini-Hochberg adjustments was computed for post hoc analysis 

# One-way ANOVA to test for differences in consumption among the three macroalgae in the exposed treatment for each assay (single, mixed).

# Packages -----
library(readr)
library(tidyverse)
library(janitor)
library(car) # Levene's test
#library(rstatix) # Dunn's test
library(FSA) # Dunn's test

# Functions -----
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

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

sinDat <- palDat %>% 
  filter(assay_style == "Single species") %>% 
  filter(treatment == "Exposed")


# Data wrangling: calculate raw and percent consumption for cafeteria and single species ------

# Calculate raw amount consumed (initial weight - final weight) and percent consumed [(initial-final) / initial x 100%] 
cafDat <- cafDat %>% 
  select(replicate, taxa, assay_style, treatment, initial_weight_g, final_weight_g) %>% 
  mutate(rawConsump = initial_weight_g - final_weight_g) %>% 
  mutate(percConsump = abs((initial_weight_g - final_weight_g) / initial_weight_g * 100))

# Calculate mean, count, SD, SE, 95% CI for percent change in weight grouped by taxa and treatment
cafDat_percConsump <- summarySE(cafDat, measurevar="percConsump", groupvars=c("taxa","treatment"))


# Calculate raw amount consumed (initial weight - final weight) and percent consumed [(initial-final) / initial x 100%] 
sinDat <- sinDat %>% 
  select(replicate, taxa, assay_style, treatment, initial_weight_g, final_weight_g) %>% 
  mutate(rawConsump = initial_weight_g - final_weight_g) %>% 
  mutate(percConsump = abs((initial_weight_g - final_weight_g) / initial_weight_g * 100))

# Calculate mean, count, SD, SE, 95% CI for raw consumption (initial - final) grouped by taxa and treatment and replicate (to average duplicates and return summary stats for each replicate)
sinDat_rawConsumpDup <- summarySE(sinDat, measurevar="rawConsump", groupvars=c("taxa","treatment", "replicate"))


# Data transformation: cafeteria and single species -----
# Transform percent consumption using arcsin square root transformation (since values are in percentages)
cafDat <- cafDat %>% 
  dplyr::mutate(percConsump.arcsin = asin(sqrt(percConsump / 100))) # arcsin can only be performed on values from 0 to 1, so must divide percentages by 100

## Check transformation
plot(cafDat$percConsump, cafDat$percConsump.arcsin)

sinDat <- sinDat %>% 
  dplyr::mutate(percConsump.arcsin = asin(sqrt(percConsump / 100))) # arcsin can only be performed on values from 0 to 1, so must divide percentages by 100

## Check transformation
plot(sinDat$percConsump, sinDat$percConsump.arcsin)


# Kruskal-Wallis rank sum test: cafeteria -----
# Kruskal-Wallis rank sum test (non-parametric alternative to one-way ANOVA when assumptions are not met)
kruskal.test(percConsump.arcsin ~ taxa, data = cafDat)

# Post-hoc testing using Dunn test (controls for multiple hypothesis testing)
caf.DT <- dunnTest(percConsump.arcsin ~ taxa, data = cafDat,
                   method = "bh") # Benjamini-Hochberg method
caf.DT

# For cafeteria-style assays, consumption significantly differed between all taxa (Kruskal-Wallis test, p-value = 1.723e-07).
# Amansia-Sargassum (Dunn's post-hoc test, p = 0.00007) 
# Amansia-Turbinaria (Dunn's post-hoc test, p = 0.0000002)
# Sargassum-Turbinaria (Dunn's post-hoc test, p = 0.01)

# Consumption significantly differed between taxa (Kruskal-Wallis test, p < 0.001). Pairwise comparisons using Dunn’s test with Benjamini-Hochberg adjustments indicated that consumption significantly differed between Amansia and Sargassum (p < 0.001), Amansia and Turbinaria (p < 0.001), and Sargassum and Turbinaria (p = 0.01).


# Kruskal-Wallis rank sum test: single species -----
# Kruskal-Wallis rank sum test (non-parametric alternative to one-way ANOVA when assumptions are not met)
kruskal.test(percConsump.arcsin ~ taxa, data = sinDat)

# Post-hoc testing using Dunn test (controls for multiple hypothesis testing)
sin.DT <- dunnTest(percConsump.arcsin ~ taxa, data = sinDat,
                   method = "bh") # Benjamini-Hochberg method
sin.DT


# For single species assays, consumption significantly differed between all taxa (Kruskal-Wallis test, p-value = 2.535e-09).
# Amansia-Sargassum (Dunn's test, p = 0.002) 
# Amansia-Turbinaria (Dunn's test, p = 0.0000000009)
# Sargassum-Turbinaria (Dunn's test, p = 0.0002)

