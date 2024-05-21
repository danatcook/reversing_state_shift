## The effect of the trade-off on the provision of coral-invasible space 
# Script to use two-way ANOVA to explore differences in the change in cover of bare space (turf/CCA) with ambient browsing level, initial community state, and their interaction as factors.
# Factor A: ambient browsing level; levels: high browsing (N=2 sites), low browsing (N=3 sites).
# Factor B: initial community state (N=30 replicates per state); levels: palatable mac-dominated, unpalatable mac-dominated

# Packages -----
library(readr)
library(ggplot2) 
library(car) # to use Levene's test and run type III anova (for unbalanced design)
library(ggpubr)
library(rstatix)
library(multcompView) # for tukey post-hoc comparison (creates compact letter display)
library(plotrix)
library(tidyverse)
#library(wesanderson)
library(Manu)

# Functions -----
# Save ggplot
#source("C:/Users/danat/OneDrive/Documents/R Resources/functions/save_plot_function.R")
# source("/Users/danacook/R Functions/save_plot_function.R")

# Set assigned colors -----
# Community states
Udom <- '#51806a' # Unpalatable-dominated state = green
Pdom <- '#C582B2' # Palatable-dominated state = lilac
Tdom <- '#7d9fc2' # Palatable-dominated state = pale blue

# Data -----
# Initial, final, and change in the percent cover of Amansia, Sargassum, and Turbinaria after ~10 days in 2021 herbivore response plots.
cover <- read_csv("data/HR_algae_percent_cover.csv")
cover$site <- sub("_", " ", cover$site) # remove 1st '_' from site names
cover$site <- sub("_", " ", cover$site) # remove 2nd '_' from site names
cover$treatment <- sub("-", " ", cover$treatment) # remove 2nd '_' from site names

# Ambient browsing levels at each site (high or low)
ambient.browsing <- read_csv("data/site_ambient_browsing.csv")


# Data wrangling: Two-way Anova for increase in turf/CCA cover -----

# Subset Turf/CCA data (i.e., drop macroalgae) and then
# Calculate increase in turf cover (Initial - Final), 
# rather than change in turf cover (Final - Initial)
turf <- cover %>% 
  dplyr::filter(spaceholder == "Turf/CCA") %>% 
  dplyr::select(!group) %>% 
  mutate(increase.in.cover.spaceholder = final.cover.spaceholder - initial.cover.spaceholder) %>% 
  dplyr::select(!change.in.cover.spaceholder)
  

# Add ambient browsing level to turf df
turf <- merge(turf, ambient.browsing, by = "site")

turf$site <- as.factor(turf$site)
turf$treatment <- as.factor(turf$treatment)
turf$plot <- as.factor(turf$plot)
turf$ambient.browsing <- as.factor(turf$ambient.browsing)


# Data transformation: Two-way Anova for increase in turf/CCA cover -----
# Transform change in turf/CCA cover using arcsin square root transformation (since values are in percentages)
turf <- turf %>% 
  dplyr::mutate(increase.in.cover.spaceholder.arcsin = asin(sqrt(increase.in.cover.spaceholder / 100))) # arcsin can only be performed on values from 0 to 1, so must divide percentages by 100

## Check transformation
plot(turf$increase.in.cover.spaceholder, turf$increase.in.cover.spaceholder.arcsin)


# Data visualization: Two-way Anova for increase in turf/CCA cover -----
## Generate frequency tables to check for balanced design
table(turf$ambient.browsing, turf$treatment)
# We have a 2x2 unbalanced design.

# Set level order for initial community states (i.e., 'treatment')
# turf$treatment <- factor(turf$treatment, levels = c('Unpalatable dominated', 'Palatable dominated'))


# Check Anova assumptions: Two-way Anova for increase in turf/CCA cover -----

## Check normality assumption
# Build the linear model
model.increase  <- lm(increase.in.cover.spaceholder.arcsin ~ ambient.browsing*treatment, data = turf)
# Create a QQ plot of residuals
ggqqplot(residuals(model.increase))
## Points do not fall along the reference line, so data doesn't meet normality assumption. 
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model.increase))
# The p-value = 0.36, so can't reject the null (which is that data are normally distributed).
# Meets assumption that data are normally distributed.


## Check assumption of equal variances
var.test.increase <- leveneTest(increase.in.cover.spaceholder.arcsin ~ ambient.browsing*treatment, data = turf)
var.test.increase
# p = 0.008, so can reject the null (which is the variances are equal, ratio = 1)
# Doesn't meet assumption that variances are equal.
plot(increase.anova, 1) # Run code below that runs 'increase.anova' model
# Maybe a slight relationship between residuals and fitted values (the mean of each group), but variances seem equal enough to meet assumption. 



# Run Anova: Two-way Anova for increase in turf/CCA cover -----
# Question: Does increase in turf cover differ significantly between ambient browsing treatments and initial community states? Does the effect of browsing depend on initial state (i.e., interaction)?

## Run two-way ANOVA for unbalanced design
increase.anova <- aov(increase.in.cover.spaceholder.arcsin ~ ambient.browsing*treatment, data = turf)
Anova(increase.anova, type = "III")

# The increase in turf cover differed significantly between ambient browsing treatments (p = 0.001) and initial community states (p<0.0001). The effect of ambient browsing on the increase in turf cover depends on initial community state (p<0001), i.e., there is an interaction between ambient browsing level and initial community state.


# Post-hoc analysis: Two-way Anova for increase in turf/CCA cover -----
#TukeyHSD(increase.anova, which = "ambient.browsing:treatment")

# Table with factors, means and standard deviation
data_summary <- turf %>% 
  group_by(ambient.browsing, treatment) %>%
  dplyr::summarise(n = n(),
            mean=mean(increase.in.cover.spaceholder), 
            sd=sd(increase.in.cover.spaceholder),
            se=std.error(increase.in.cover.spaceholder)) %>%   
  arrange(desc(mean))

# Tukey's test
tukey <- TukeyHSD(increase.anova)
print(tukey)

# Creating the compact letter display (letters that indicate sig. differences in pairwise comparisons)
tukey.cld <- multcompLetters4(increase.anova, tukey)
print(tukey.cld)

# Adding the compact letter display to the table with means, sd, se
cld <- as.data.frame.list(tukey.cld$`ambient.browsing:treatment`)
data_summary$Tukey <- cld$Letters
print(data_summary)

# Export dataframe of means, sd, compact letter display (Tukey results) for ambient.browsing and treatment
#write_csv(data_summary, "data/processed/increase.in.turf.cover.anova.summary.csv")


# Post-hoc analysis visualization: Two-way Anova for final turf/CCA cover -----
# coloured barplot
ggplot(data_summary, aes(x = factor(ambient.browsing), y = mean, fill = treatment)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.7)  +
  scale_fill_manual(values = c("Palatable dominated" = Pdom,
                               "Unpalatable dominated" = Udom), 
                    name = "Initial Community State", 
                    labels = c("Palatable-dominated", "Unpalatable-dominated")) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge(0.7), width = 0.25) +
  labs(x="", y="Increase in percent cover (mean +/- SE)") +
  #ggtitle("Turf/CCA cover") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text=element_text(size=25), axis.text.x = element_text(face = "bold")) +
  coord_cartesian(ylim=c(0, 101)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_text(aes(label=Tukey), position = position_dodge(0.7), size = 9, 
            vjust=-0.5, hjust=-1.4, colour = "gray25")

#save_plot("anova.increase.in.turf.cover.png", 14, 9, 500, "output/change in algal cover")
