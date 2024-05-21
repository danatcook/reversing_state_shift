## Effects of trade-off on reef state
# Script to generate figure 2 of plots that persisted or shifted to a new community state.

# Packages -----
library(tidyverse)
library(readr)
library(ggplot2)
#library(wesanderson) # Wes Anderson color palettes
library(Manu) # NZ birds color palettes

# Functions -----
# Save plot using ggsave
#source("Users/danacook/R Functions/save_plot_function.R")

# Final community states of each plot
final.state <- read_csv("data/final_community_states.csv") %>% 
  filter(treatment == "caged" | treatment == "exposed") %>% 
  filter(final_community_state != "Treatment compromised")
final.state$replicate <- as.factor(final.state$replicate)


# Set assigned colors -----
get_pal("Kereru")
kereru <- get_pal("Kereru")
print_pal(kereru)
# Community states
Udom <- '#51806a' # Unpalatable-dominated state = green
Pdom <- '#C582B2' # Palatable-dominated state = lilac
Tdom <- '#7d9fc2' # Palatable-dominated state = pale blue


# Data wrangling: proportion of reps in final community states -----
final.state.counts <- final.state %>% 
  group_by(treatment, final_community_state) %>% 
  dplyr::summarise(count = n()) %>% 
  ungroup() %>% 
  add_row(treatment = "caged", final_community_state = "Turf/CCA-dominated", count = 0) %>% 
  add_row(treatment = "caged", final_community_state = "Unpalatable-dominated", count = 0) %>% 
  add_row(treatment = "exposed", final_community_state = "Palatable-dominated", count = 0) %>% 
  group_by(treatment) %>% 
  mutate(total.count = sum(count)) %>% 
  mutate(prop.count = count/total.count)

final.state.counts$treatment <- as.factor(final.state.counts$treatment)
final.state.counts$final_community_state <- as.factor(final.state.counts$final_community_state)

# Specify order of variables for community states
final.state.counts$final_community_state <- factor(final.state.counts$final_community_state, levels = c("Unpalatable-dominated", "Palatable-dominated", "Turf/CCA-dominated"))

# Create new labels for treatment
treatment.labs <- c("No herbivory", "Ambient herbivory")
names(treatment.labs) <- c("caged", "exposed")


# Data wrangling: proportion of reps in initial community states -----
initial.state.counts <- data.frame(treatment = c("caged", "caged", "caged", "exposed", "exposed", "exposed"),
                                   initial_community_state = c("Palatable-dominated", "Turf/CCA-dominated","Unpalatable-dominated", "Palatable-dominated", "Turf/CCA-dominated","Unpalatable-dominated"),
                                   count = c(0, 0, 18, 0, 0, 20),
                                   total.count = c(18, 18, 18, 20, 20, 20),
                                   prop.count = c(0, 0, 1, 0, 0, 1)
)

# Specify order of variables for community states
initial.state.counts$initial_community_state <- factor(initial.state.counts$initial_community_state, levels = c("Unpalatable-dominated", "Palatable-dominated", "Turf/CCA-dominated"))


# Data wrangling: state shifts between intial and final timepoints -----
# Create new df with prop.count for both treatments and for initial and final timepoints
state.shift.counts <- initial.state.counts %>% 
  dplyr::select(!c(count, total.count)) %>% 
  dplyr::mutate(timepoint = "Time 0 years") %>% 
  dplyr::rename(community_state = initial_community_state)

# Create temporary df to wrangle final.state.counts
temp <- final.state.counts %>% 
  dplyr::select(!c(count, total.count)) %>% 
  dplyr::mutate(timepoint = "Time 2 years") %>% 
  dplyr::rename(community_state = final_community_state)

# Merge initial and final data
state.shift.counts <- rbind(state.shift.counts, temp)


# Visualization: OPTION 1: state shifts between intial and final timepoints -----
# Create community state labels
state_labels <- c("Unpalatable", "Palatable", "Turf/CCA")

# Create herbivory treatment labels to rename faceted labels
herbivory_labels <- c("No herbivory", "Ambient herbivory")
names(herbivory_labels) <- c("caged", "exposed")





# Visualization: state shifts between initial and final timepoints for caged ttt -----
ggplot(data = state.shift.counts %>% 
         filter(treatment == "caged"), 
       aes(x = community_state,
           y = prop.count)) +
  geom_bar(stat = "identity", aes(fill = factor(community_state))) +
  scale_fill_manual(values = c("Unpalatable-dominated" = Udom,
                               "Palatable-dominated" = Pdom,
                               "Turf/CCA-dominated"= Tdom)) +
  facet_wrap(vars(timepoint), strip.position = "top", scales = "free_x") +
  scale_y_continuous(limits = c(0,1.05), expand = c(0,0)) +
  scale_x_discrete(labels = state_labels) +
  xlab("\n Community State") +
  ylab("Proportion of replicates") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + # remove background gridlines
  labs(fill = "Community state") + # rename legend title
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, face = "bold")) +
  theme(text = element_text(size = 23)) +
  theme(panel.spacing.x = unit(0, "lines")) # removes horizontal space between panels

#save_plot("switching.community.state.probability.caged.png", 12, 7, 500, "output/Final Communities")  




# Visualization: state shifts between initial and final timepoints for exposed ttt -----

ggplot(data = state.shift.counts %>% 
         filter(treatment == "exposed"), 
       aes(x = community_state,
           y = prop.count)) +
  geom_bar(stat = "identity", aes(fill = factor(community_state))) +
  scale_fill_manual(values = c("Unpalatable-dominated" = Udom,
                               "Palatable-dominated" = Pdom,
                               "Turf/CCA-dominated" = Tdom)) +
  facet_wrap(vars(timepoint), strip.position = "top", scales = "free_x") +
  scale_y_continuous(limits = c(0,1.05), expand = c(0,0)) +
  scale_x_discrete(labels = state_labels) +
  xlab("\n Community State") +
  ylab("Proportion of replicates") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + # remove background gridlines
  labs(fill = "\n Community state") + # rename legend title
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, face = "bold")) +
  theme(text = element_text(size = 23)) +
  theme(panel.spacing.x = unit(0, "lines")) # removes horizontal space between panels

#save_plot("switching.community.state.probability.exposed.png", 12, 7, 500, "output/Final Communities") 

