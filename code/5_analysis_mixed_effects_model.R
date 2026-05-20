# Script to use a mixed effects model to test if communities that start out palatable vs. unpalatable respond differently depending on whether they are in a high-browsing or low-browsing site.


# Packages -----
library(readr)
library(tidyverse)
library(lme4)
library(ggplot2)
library(cowplot) # for save_plot()
library(Manu) # for color palette

# Data -----
# Initial, final, and change in the percent cover of Amansia, Sargassum, and Turbinaria after ~10 days in 2021 herbivore response plots.
removal.data <- read_csv("data/HR_algae_percent_cover.csv") %>% 
  # keep only columns we care about
  select(site, treatment, spaceholder, change.in.cover.spaceholder) %>% 
  # keep only the Turf/CCA spaceholder rows
  filter(spaceholder == "Turf/CCA") %>% 
  # rename to nicer variable names
  rename(increase.in.exposed = change.in.cover.spaceholder) %>% 
  rename(initial.community = treatment)

ambient.browsing <- read_csv("data/site_ambient_browsing.csv")

# Add column with ambient browing level for each site
removal.data <- removal.data %>% 
  left_join(ambient.browsing, by = "site")

# Make sure categorical variables are factors
removal.data <- removal.data %>% 
  mutate(
    site             = as.factor(site),
    initial.community = as.factor(initial.community),
    ambient.browsing  = as.factor(ambient.browsing)
  )

# Quick sanity check
glimpse(removal.data)

# Fit the Mixed Effects Model -----
# Fixed effects: initial.community, ambient.browsing, and interaction between them
# Random effects: site
model_full <- lmer(
  increase.in.exposed ~ initial.community * ambient.browsing + (1 | site),
  data  = removal.data,
  REML  = FALSE
)

# Check that model converged
summary(model_full)
isSingular(model_full, tol = 1e-4)
# Model converged

# Test interaction using likelihood ratio test -----
# To test the interaction, compare the full model (interaction included) with a reduced model (same fixed effects but no interaction)

# Reduced model
model_noint <- lmer(
  increase.in.exposed ~ initial.community + ambient.browsing + (1 | site),
  data  = removal.data,
  REML  = FALSE
)

# Check model convergence
summary(model_noint)
isSingular(model_noint)

# Compare the two models
anova(model_noint, model_full)
# The model with the interaction (model_full) fit significantly better than the model without the interaction (model_noint). The interaction stays!

# Refit the full model using REML -----
model_final <- lmer(
  increase.in.exposed ~ initial.community * ambient.browsing + (1 | site),
  data = removal.data,
  REML = TRUE
)

summary(model_final)

# Quick assumption checks -----
# Residuals vs fitted
plot(model_final)

# Normal Q-Q plot for residuals
qqnorm(residuals(model_final)) # No obvious pattern detected, looks random
qqline(residuals(model_final)) # Residuals fall roughly on the Q-Q line

summary(model_final)
anova(model_noint, model_final)

# Interpretation of results -----
# Question: How much exposed bare space do herbivores create over ~10 days, and does that depend on (a) whether the algal community started out palatable-dominated vs unpalatable-dominated and (b) whether the site has high vs low ambient browsing?

# Answer: Yes, our results show a strong, statistically significant interaction between initial community state and ambient browsing pressure. This means that herbivores create different amounts of exposed cover depending on the starting community — and this difference is larger at low-browsing sites than at high-browsing sites. At low-browsing sites, herbivores were able to create much more open space in palatable-dominated communities compared to unpalatable. At high-browsing sites, differences in the amount of open spaced created by herbivores were smaller between palatable and unpalatable communities.


# Key results interpreted:

# 1. Reference level: Palatable-dominated, high-browsing (intercept):
# Intercept = 90.8% --> ~91%; Herbivores created 91% cover of open space in palatable-dominated communities at high-browsing sites.

# 2. Effect of switching to Unpalatable (i.e., Unpalatable-dominated, high-browsing):
# Coefficient = ~26.6% --> ~27%; Predicted exposed cover created = 64% (91-27=64)
# At high-browsing sites, herbivores created 27% on average less open space in communities dominated by unpalatable algae than palatable algae.

# 3. Effect of switching to low browsing (i.e., Palatable-dominated, low-browsing)
# Coefficient = -19.5 --> ~-20%; Predicted exposed cover created = 71% (91-20=71)

# 4. Interaction (extra change when both conditions occur together)
# Interaction term = -40.8; Predicted exposed cover created = 4% [91-(26.6+19.5+40.8)=3.9]
# In unpalatable-dominated communities in low browsing sites, herbivores created almost no exposed substrate -- only ~4% on average.


# Figure ----
## 1. Get model-predicted means for each treatment combination ----
newdat <- expand_grid(
  initial.community = levels(removal.data$initial.community),
  ambient.browsing  = levels(removal.data$ambient.browsing)
)

# Predicted exposed cover created (fixed effects only: re.form = NA)
newdat$fit <- predict(model_final, newdata = newdat, re.form = NA)

## 2. Plot raw data + model predictions ----
# X-axis: Ambient browing level
p <- ggplot(removal.data,
            aes(x = ambient.browsing,
                y = increase.in.exposed,
                colour = initial.community)) +
  # raw data (jittered)
  geom_point(position = position_jitterdodge(
    dodge.width = 0.4,
    jitter.width = 0.3
  ),
  alpha = 0.3) +
  # model predictions: points
  geom_point(data = newdat,
             aes(y = fit),
             position = position_dodge(width = 0.4),
             size = 3) +
  # model predictions: lines
  geom_line(data = newdat,
            aes(y = fit, group = initial.community),
            position = position_dodge(width = 0.4),
            linewidth = 1) +
  # **Manu colors**
  scale_color_manual(
    values = c(
      "Unpalatable-dominated" = "#51806a",
      "Palatable-dominated"   = "#C582B2"
    )
  ) +
  labs(
    x = "Ambient browsing level",
    y = "Percent cover of open space created by herbivores",
    colour = "Initial algal community state"
  ) +
  theme_classic()

# Save
save_plot(
  filename = "output/herbivore.response.mixed.effects.model.png",
  plot = p,
  base_width = 8,
  base_height = 6,
  dpi = 500
)






# GRAVEYARD -----
# Plot version # 2 where X-axis = Initial algal community state
q <- ggplot(removal.data,
       aes(x = initial.community,
           y = increase.in.exposed,
           colour = ambient.browsing)) +
  # raw data (jittered so points don’t sit on top of each other)
  geom_point(position = position_jitterdodge(
    dodge.width = 0.4, jitter.width = 0.1
  ),
  alpha = 0.3) +
  # model-predicted means
  geom_point(data = newdat,
             aes(y = fit),
             position = position_dodge(width = 0.4),
             size = 3) +
  geom_line(data = newdat,
            aes(y = fit, group = ambient.browsing),
            position = position_dodge(width = 0.4),
            linewidth = 1) +
  labs(x = "Initial algal community state",
       y = "Percent cover of open space created by herbivores (%)",
       colour = "Ambient browsing level") +
  theme_classic()


# Post-hoc analysis: planned contrasts of estimated marginal means -----
## ------------------------------------------------------------
## Post-hoc (planned) contrasts for the LMM interaction
## Scientific Reports-friendly workflow using estimated marginal means (EMMs)
##
## Assumes you already fit:
##   model_final <- lmer(increase.in.exposed ~ initial.community*ambient.browsing + (1|site),
##                       data = removal.data, REML = TRUE)
##
## Note: Post-hoc tests should be based on the fitted model (incl. random site effect),
## not Tukey HSD on raw data.
## ------------------------------------------------------------

# Load package for estimated marginal means + contrasts
library(emmeans)

## 1) Estimated marginal means for each treatment combination ----
## This gives the model-based mean response for each combo of:
##   initial.community (Palatable vs Unpalatable)
##   ambient.browsing  (High vs Low)
emm <- emmeans(model_final, ~ initial.community * ambient.browsing)

# Inspect EMMs (estimates, SEs, CIs)
emm


## 2) Planned contrast #1: effect of browsing intensity WITHIN each community state ----
## This addresses whether Turbinaria cover changed at high browsing sites but not at low browsing sites.
##
## It computes High vs Low browsing *separately* for Palatable-dominated and Unpalatable-dominated.
contr_browsing_within_comm <- contrast(
  emm,
  method = "pairwise",          # pairwise differences of ambient browsing levels
  by = "initial.community",     # do the comparison within each initial.community level
  adjust = "none"               # planned contrasts -> typically no multiple-comparison adjustment
)

# View results (estimate = difference, SE, df, t-ratio, p-value)
contr_browsing_within_comm
# RESULT: p=0.0039 --> p<0.01


## 3) Planned contrast #2: cross-factor comparison
## This addresses whether change in Turbinaria cover at HIGH browsing was statistically indistinguishable from palatable at LOW browsing."
##
## IMPORTANT: The contrast weights depend on the order of rows in `emm`.
## Print `emm` first and confirm the row order. A common order is:
##   1) Palatable-dominated    High browsing
##   2) Palatable-dominated    Low browsing
##   3) Unpalatable-dominated  High browsing
##   4) Unpalatable-dominated  Low browsing
##
## The vector c(0, -1, 1, 0) implements:
##   (Unpalatable High) - (Palatable Low)
##
## If your order differs, adjust the weights accordingly.
contr_unpal_high_vs_pal_low <- contrast(
  emm,
  list(
    "Unpalatable High vs Palatable Low" = c(0, -1, 1, 0)
  ),
  adjust = "none"               # planned contrast
)

contr_unpal_high_vs_pal_low
# RESULTS: p=0.53


## 4) Optional: export clean tables for writing / supplement
## Convert to data frames you can print nicely or write to CSV.
# emm_df <- as.data.frame(emm)
# contr1_df <- as.data.frame(contr_browsing_within_comm)
# contr2_df <- as.data.frame(contr_unpal_high_vs_pal_low)
# 
# emm_df
# contr1_df
# contr2_df

# Optional: write to CSV (uncomment and set a path if you want)
# write.csv(emm_df,   "emm_table.csv", row.names = FALSE)
# write.csv(contr1_df,"planned_contrast_browsing_within_community.csv", row.names = FALSE)
# write.csv(contr2_df,"planned_contrast_unpal_high_vs_pal_low.csv", row.names = FALSE)


## 5) (Optional) If you prefer a conservative p-value adjustment ----
## Not required for a small set of planned contrasts, but you can use Holm.
# contr_browsing_within_comm_holm <- contrast(
#   emm,
#   method = "pairwise",
#   by = "initial.community",
#   adjust = "holm"
# )
# contr_browsing_within_comm_holm


