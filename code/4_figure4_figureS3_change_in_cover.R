# Script to produce visualization of a) percent change in total algal cover and b) percent change in focal taxa as a function of initial community palatability (palatable-dominated and unpalatable-dominated).

# Packages -----
library(readr)
library(tidyverse)
library(ggplot2)
library(Manu)


# Functions -----
# Calculates standard error
# source("/Users/danacook/R Functions/se_function.R")
st.err <- function(x, na.rm=FALSE) {
  if(na.rm==TRUE) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

# Save ggplot
# source("/Users/danacook/R Functions/save_plot_function.R")

# Set assigned colors -----
# Community states
Udom <- '#214d65' # Unpalatable-dominated state = dark blue
Pdom <- '#287DAB' # Palatable-dominated state = royal blue
Tdom <- '#E5BF86' # Palatable-dominated state = beige

# Benthic spaceholders
Umac <- "gray30" # Unpalatable macroalgae (Turbinaria) = dark grey
Pmac <- "gray60" # Palatable macroalgae (Amansia + Sargassum) = light grey
TCCA <- "gray80" # Turf/CCA = white

# Data -----

# Initial, final, and change in percent cover of Amansia, Sargassum, and Turbinaria after ~10 days in 2021 herbivore response plots.
cover <- read_csv("data/HR_algae_percent_cover.csv")
cover$site <- sub("_", " ", cover$site) # remove 1st '_' from site names
cover$site <- sub("_", " ", cover$site) # remove 2nd '_' from site names

# Ambient browsing level at 5 sites (high or low)
browsing <- read_csv("data/site_ambient_browsing.csv")

# Site names based on browsing level
site.names <- read_csv("data/raw/site_browsing_names.csv")

# Add browsing site names to cover data
cover <- merge(cover, site.names, by = "site")

# Data wrangling: initial percent cover & composition by treatment and site -----
# Calculates initial cover of palatable algae in each plot and then
# calculates mean (n, sd, se) cover of each group at initial timepoint
initial.assemblage.site <- cover %>% 
  dplyr::group_by(site.names, site, treatment, plot, group) %>% 
  dplyr::summarise(
    initial.cover.group = sum(initial.cover.spaceholder)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(site.names, site, treatment, group) %>%
  dplyr::summarise(
    n = n(),
    mean = mean(initial.cover.group),
    sd = sd(initial.cover.group),
    se = se_function(initial.cover.group)
  )

# Set order of benthic spaceholders
initial.assemblage.site$group <- factor(initial.assemblage.site$group, levels = c('Unpalatable algae', 'Palatable algae', 'Turf/CCA'))

# Set order of sites
initial.assemblage.site$site.names <- factor(initial.assemblage.site$site.names, levels = c('Low Browsing 1', 'Low Browsing 2', 'Low Browsing 3', 'High Browsing 1', 'High Browsing 2'))

# Visualizations: initial percent cover & composition by treatment and site -----
ggplot(initial.assemblage.site, aes(fill=group, 
                                    y=mean, 
                                    x=site.names)) + 
  geom_bar(position="stack", stat="identity", aes(fill = factor(group))) +
  scale_fill_manual(values = c("Unpalatable algae" = Umac,
                               "Palatable algae" = Pmac,
                               "Turf/CCA"= TCCA)) +
  xlab("Site") +
  ylab("Percent cover") +
  ggtitle("Initial percent cover") +
  facet_wrap(~treatment) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, face = "bold")) +
  theme(text = element_text(size = 20), legend.title = element_blank()) +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0))

#save_plot("initial.cover.and.assemblages.png", 10, 6, 500, "output/change in algal cover")



# Data wrangling: final percent cover & composition by treatment and site -----
# Calculates final cover of palatable algae in each plot and then
# calculates mean (n, sd, se) cover of each group at final timepoint
final.assemblage.site <- cover %>% 
  group_by(site.names, site, treatment, plot, group) %>% 
  summarise(
    final.cover.group = sum(final.cover.spaceholder)
  ) %>% 
  ungroup() %>% 
  group_by(site.names, site, treatment, group) %>%
  summarise(
    n = n(),
    mean = mean(final.cover.group),
    sd = sd(final.cover.group),
    se = se_function(final.cover.group)
  )

# Set order of benthic spaceholders
final.assemblage.site$group <- factor(final.assemblage.site$group, levels = c('Unpalatable algae', 'Palatable algae', 'Turf/CCA'))

# Set order of sites
final.assemblage.site$site.names <- factor(initial.assemblage.site$site.names, levels = c('Low Browsing 1', 'Low Browsing 2', 'Low Browsing 3', 'High Browsing 1', 'High Browsing 2'))

# Visualizations: final percent cover & composition by treatment and site -----
ggplot(final.assemblage.site, aes(fill=group, 
                                    y=mean, 
                                    x=site.names)) + 
  geom_bar(position="stack", stat="identity", aes(fill = factor(group))) +
  scale_fill_manual(values = c("Unpalatable algae" = Umac,
                               "Palatable algae" = Pmac,
                               "Turf/CCA"= TCCA)) +
  xlab("\n Site") +
  ylab("Percent cover") +
  ggtitle("Final percent cover") +
  facet_wrap(~treatment) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, face = "bold")) +
  theme(text = element_text(size = 20), legend.title = element_blank()) +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0))

#save_plot("final.cover.and.assemblages.png", 10, 6, 500, "output/change in algal cover")





# Data wrangling: initial percent cover & composition by treatment and ambient browsing (high/low) -----
# Assign browsing levels to each site
initial.assemblage.browsing <- merge(initial.assemblage.site, browsing, by = "site")
# Calculate mean cover of each group by treatment 
initial.assemblage.browsing <- initial.assemblage.browsing %>% 
  group_by(ambient.browsing, treatment, group) %>% 
  summarise(
    n = n(),
    sd = sd(mean),
    se = se_function(mean),
    max = max(mean),
    min = min(mean),
    mean = mean(mean)
  )


# Visualization: initial percent cover & composition by treatment and ambient browsing (high/low) -----
ggplot(initial.assemblage.browsing, aes(fill=group, 
                                  y=mean, 
                                  x=ambient.browsing)) + 
  geom_bar(position="dodge", stat="identity", aes(fill = factor(group))) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9)) +
  scale_fill_manual(values = c("Unpalatable algae" = Umac,
                               "Palatable algae" = Pmac,
                               "Turf/CCA"= TCCA)) +
  xlab("") +
  ylab("Percent cover") +
  ggtitle("Initial percent cover & assemblages") +
  facet_wrap(~treatment) +
  theme_bw() +
  theme(axis.ticks.x = element_blank(), legend.title = element_blank()) +
  theme(text = element_text(size = 18)) +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0))

#save_plot("initial.cover.and.assemblages.ambient.browsing.png", 14, 7, 500, "output/change in algal cover")


# Data wrangling: final percent cover & composition by treatment and ambient browsing (high/low) -----
# Assign browsing levels to each site
final.assemblage.browsing <- merge(final.assemblage.site, browsing, by = "site")
# Calculate mean cover of each group by treatment 
final.assemblage.browsing <- final.assemblage.browsing %>% 
  group_by(ambient.browsing, treatment, group) %>% 
  summarise(
    n = n(),
    sd = sd(mean),
    se = se_function(mean),
    max = max(mean),
    min = min(mean),
    mean = mean(mean)
  )


# Visualization: final percent cover & composition by treatment and ambient browsing (high/low) -----
ggplot(final.assemblage.browsing, aes(fill=group, 
                                        y=mean, 
                                        x=ambient.browsing)) + 
  geom_bar(position="dodge", stat="identity", aes(fill = factor(group))) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9)) +
  scale_fill_manual(values = c("Unpalatable algae" = Udom,
                               "Palatable algae" = Pdom,
                               "Turf/CCA" = Tdom)) +
  xlab("") +
  ylab("Percent cover") +
  ggtitle("Final percent cover & assemblages") +
  facet_wrap(~treatment) +
  theme_bw() +
  theme(axis.ticks.x = element_blank(), legend.title = element_blank()) +
  theme(text = element_text(size = 18)) +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0))

#save_plot("final.cover.and.assemblages.ambient.browsing.png", 14, 7, 500, "output/change in algal cover")


# Data wrangling: shifts in community assemblage by herbivory treatment and initial community treatment -----
assemblage.shifts.browsing <- initial.assemblage.browsing %>% 
  mutate(timepoint = "Day 0")

# Create temporary dataframe to mess with final assemblage data
temp <- final.assemblage.browsing %>% 
  mutate(timepoint = "Day 10")

# Merge intial and final data into one df
assemblage.shifts.browsing <- rbind(assemblage.shifts.browsing, temp)
assemblage.shifts.browsing$ambient.browsing <- as.factor(assemblage.shifts.browsing$ambient.browsing)
assemblage.shifts.browsing$treatment <- as.factor(assemblage.shifts.browsing$treatment)
assemblage.shifts.browsing$timepoint <- as.factor(assemblage.shifts.browsing$timepoint)

# Visualization: shifts in community assemblage by herbivory treatment and initial community treatment -----

ggplot(data = assemblage.shifts.browsing) +
  geom_bar(aes(x = timepoint,
               y = mean, 
               fill = factor(group)), 
           stat = "identity", 
           width = 0.5,
           position=position_dodge(0.5)) + 
  scale_fill_manual(values = c("Unpalatable algae" = Umac,
                               "Palatable algae" = Pmac,
                               "Turf/CCA"= TCCA)) +
  facet_grid(treatment ~ ambient.browsing) + 
  geom_errorbar(aes(x = timepoint, 
                    ymin=mean-se, 
                    ymax=mean+se, 
                    fill = factor(group)), 
                width=0.25, 
                position=position_dodge(0.5)) + 
  facet_grid(treatment ~ ambient.browsing) + 
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  labs(fill = "Benthic spaceholder") + # rename legend title
  xlab("") +
  ylab("Percent cover") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(panel.spacing = unit(1, "lines"))

#save_plot("community.assemblage.shifts.browsing.png", 14, 7, 500, "output/change in algal cover")


# Visualization: shifts in community assemblage by herbivory treatment for palatable-dominated ttt -----
ggplot(data = assemblage.shifts.browsing %>% 
         filter(treatment == "Palatable-dominated")) +
  geom_bar(aes(x = timepoint,
               y = mean, 
               fill = factor(group)), 
           stat = "identity", 
           width = 0.6,
           position=position_dodge(0.6)) + 
  scale_fill_manual(values = c("Unpalatable algae" = Umac,
                               "Palatable algae" = Pmac,
                               "Turf/CCA"= TCCA)) +
  facet_wrap(~ ambient.browsing) + 
  geom_errorbar(aes(x = timepoint, 
                    ymin=mean-se, 
                    ymax=mean+se, 
                    fill = factor(group)), 
                width=0.25, 
                position=position_dodge(0.6)) + 
  facet_wrap(~ ambient.browsing) + 
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  labs(fill = "Benthic spaceholder") + # rename legend title
  xlab("\n Elapsed Time") +
  ylab("Percent cover") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 25)) +
  theme(panel.spacing = unit(1, "lines"))

#save_plot("community.assemblage.shifts.browsing.palatable.png", 14, 6, 500, "output/change in algal cover") 


# Visualization: shifts in community assemblage by herbivory treatment for unpalatable-dominated ttt -----
ggplot(data = assemblage.shifts.browsing %>% 
         filter(treatment == "Unpalatable-dominated")) +
  geom_bar(aes(x = timepoint,
               y = mean, 
               fill = factor(group)), 
           stat = "identity", 
           width = 0.6,
           position=position_dodge(0.6)) + 
  scale_fill_manual(values = c("Unpalatable algae" = Umac,
                               "Palatable algae" = Pmac,
                               "Turf/CCA"= TCCA)) +
  facet_wrap(~ ambient.browsing) + 
  geom_errorbar(aes(x = timepoint, 
                    ymin=mean-se, 
                    ymax=mean+se, 
                    fill = factor(group)), 
                width=0.25, 
                position=position_dodge(0.6)) + 
  facet_wrap(~ ambient.browsing) + 
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  labs(fill = "Benthic spaceholder") + # rename legend title
  xlab("\n Elapsed Time") +
  ylab("Percent cover") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 25)) +
  theme(panel.spacing = unit(1, "lines"))

#save_plot("community.assemblage.shifts.browsing.unpalatable.png", 14, 6, 500, "output/change in algal cover") 
