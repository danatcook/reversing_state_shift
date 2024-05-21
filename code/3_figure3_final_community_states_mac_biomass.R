# Script to produce figure 3 that shows algal biomass (palatable and unpalatable) in each of the final community states (unpalatable-dominated, palatable-dominated and turf/cca-dominated) from the 2 year competition-palatability trade-off experiment.

# Packages -----
library(tidyverse)
library(readr)
library(ggplot2)
library(wesanderson) # wes anderson color palette 
library(Manu) # NZ bird color palettes

# Functions -----
# Save plot using ggsave
# source("C:/Users/danat/OneDrive/Documents/R Resources/functions/save_plot_function.R")

# Calculate standard error
# source("C:/Users/danat/OneDrive/Documents/R Resources/functions/se_function.R")

st.err <- function(x, na.rm=FALSE) {
  if(na.rm==TRUE) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

# Set assigned colors -----
# Community states - Kotare
Udom <- '#51806a' # Unpalatable-dominated state = green
Pdom <- '#C582B2' # Palatable-dominated state = lilac
Tdom <- '#7d9fc2' # Palatable-dominated state = pale blue

# Benthic spaceholders
Umac <- "gray30" # Unpalatable macroalgae (Turbinaria) = dark grey
Pmac <- "gray60" # Palatable macroalgae (Amansia + Sargassum) = light grey
TCCA <- "gray80" # Turf/CCA = white


# Data -----

# Final weights (g) and proportional biomass of algal taxa in caged, exposed, and partial cage plots
biomass.data <- read_csv("data/biomass.csv")
biomass.data$replicate <- as.factor(biomass.data$replicate)
biomass.data$taxa <- as.factor(biomass.data$taxa)
biomass.data$treatment <- as.factor(biomass.data$treatment)

# Final community state and palatability of each plot
final.state <- read_csv("data/final_community_states.csv")
# Append final community state to biomass data
biomass.data <- merge(biomass.data, final.state, by = c("replicate", "treatment"))
biomass.data$final_community_assemblage <- as.factor(biomass.data$final_community_assemblage)
biomass.data$final_community_state <- as.factor(biomass.data$final_community_state)

# New taxa names (assigns all taxa other than the 3 focal spp as 'other') 
taxa.names <- read_csv("data/list_taxa_renamed.csv")
# Append new taxa names 
biomass.data <- merge(biomass.data, taxa.names, by = "taxa")

# New replicate names
replicate.names <- read_csv("data/list_replicates_renamed.csv")
# Append new replicate names
biomass.data <- merge(biomass.data, replicate.names, by = c("replicate", "treatment"))


# Data wrangling: Biomass of all taxa by replicate -----

# Subset biomass data for caged treatment (n = 18 caged plots)
biomass.caged.data <- biomass.data %>% 
  filter(treatment == "caged") 
# drops rep. 78 and 81 (cages were destroyed, so treatment was compromised)

# Subset biomass data for exposed treatment (n = 20 exposed plots)
biomass.exposed.data <- biomass.data %>% 
  filter(treatment == "exposed")


# Visualizations: Biomass of all taxa by replicate -----

# Specify order of variables for taxa_2
biomass.caged.data$taxa_2 <- factor(biomass.caged.data$taxa_2, levels = c("Amansia rhodantha", "Sargassum pacificum", "Turbinaria ornata", "Other"))
biomass.exposed.data$taxa_2 <- factor(biomass.exposed.data$taxa_2, levels = c("Amansia rhodantha", "Sargassum pacificum", "Turbinaria ornata", "Other"))

# Specify order of replicates
biomass.caged.data$replicate.new.name <- factor(biomass.caged.data$replicate.new.name, levels = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20"))
biomass.exposed.data$replicate.new.name <- factor(biomass.exposed.data$replicate.new.name, levels = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", "E11", "E12", "E13", "E14", "E15", "E16", "E17", "E18", "E19", "E20"))

# Figure 2S -----
# Figure 2b: Caged plots
# Stacked barplot of final raw biomass of algal taxa by replicate
ggplot(biomass.caged.data, aes(fill=taxa_2, y=weight_g, x=replicate.new.name)) + 
  geom_bar(position="stack", stat="identity") +
  scale_y_continuous(limits = c(0,350), expand = c(0, 0)) +
  scale_fill_discrete(name = "Taxa") +
  xlab("Replicate") +
  ylab("Final damp weight (g)") +
  theme_classic()+
  theme(text = element_text(size = 30), 
        axis.text.x = element_text(angle = 45, hjust=1))
# save_plot("final.caged.rawbiomass.by.replicate.tiff", 15, 10, 600, "output/Final Communities")

# Figure 2a: Exposed plots
# Stacked barplot of final raw biomass of algal taxa by replicate
ggplot(biomass.exposed.data, aes(fill=taxa_2, y=weight_g, x=replicate.new.name)) + 
  geom_bar(position="stack", stat="identity") +
  scale_y_continuous(limits = c(0,128), expand = c(0, 0)) +
  xlab("Replicate") +
  ylab("Final damp weight (g)") +
  scale_fill_discrete(name = "Taxa") +
  theme_classic()+
  theme(text = element_text(size = 30), 
        axis.text.x = element_text(angle = 45, hjust=1))
# save_plot("final.exposed.rawbiomass.by.replicate.tiff", 15, 10, 600, "output/Final Communities")




# Data wrangling: Final Turbinaria biomass in community assemblages and states -----

# Summary statistics of Turbinaria biomass in final community assemblages
biomass.turb.assemblage.summary <- biomass.data %>% 
  dplyr::filter(taxa == "Turbinaria ornata") %>%
  dplyr::filter(treatment == "caged" | treatment == "exposed") %>% 
  dplyr::group_by(treatment, final_community_assemblage) %>% 
  dplyr::summarise(
    n = n(),
    mean = mean(weight_g),
    sd = sd(weight_g),
    se = st.err(weight_g)
  ) 

# Summary statistics of Turbinaria biomass in final community states
biomass.turb.state.summary <- biomass.data %>% 
  filter(taxa == "Turbinaria ornata") %>%
  filter(treatment == "caged" | treatment == "exposed") %>% 
  group_by(treatment, final_community_state) %>% 
  summarise(
    n = n(),
    mean = mean(weight_g),
    sd = sd(weight_g),
    se = st.err(weight_g)
  )



# Figure 2: Algal biomass in each of the final community states ------

#Figure 2a
# Barplot of unpalatable and palatable algal biomass in final community states
ggplot(biomass.turb.state.summary, 
       aes(x=factor(final_community_state, level = c('Unpalatable-dominated', 'Palatable-dominated', 'Turf/CCA-dominated')), 
           y=mean)) + 
  geom_bar(stat="identity", 
           color="black", 
           position=position_dodge(), 
           fill = Umac,
           width = 0.7) +
  geom_errorbar(aes(ymin=mean-se, 
                    ymax=mean+se), 
                width=.2,
                position=position_dodge(.9)) +
  xlab("\n Final Community State") +
  ylab("Mean biomass (+/- SE)") +
  #ggtitle("Final biomass of unpalatable macroalgae") +
  scale_y_continuous(limits = c(0,102), expand = c(0, 0)) +
  theme_classic() +
  theme(text = element_text(size = 23)) +
  theme(axis.text.x = element_text(colour = c(Udom, Pdom, Tdom), face = "bold"))

#save_plot("final.turb.biomass.by.community.state.png", 10, 7, 500, "output/Final Communities")



# Data wrangling: Final biomass of palatable mac in community assemblages and states -----

# Summary statistics of palatable mac biomass in final community assemblages
biomass.palatable.assemblage.summary <- biomass.data %>% 
  filter(taxa == "Sargassum pacificum" | taxa == "Amansia rhodantha") %>%
  filter(treatment == "caged" | treatment == "exposed") %>% 
  group_by(treatment, final_community_assemblage) %>% 
  summarise(
    n = n(),
    mean = mean(weight_g),
    sd = sd(weight_g),
    se = st.err(weight_g)
  ) 

# Summary statistics of Turbinaria biomass in final community states
biomass.palatable.state.summary <- biomass.data %>% 
  filter(taxa == "Sargassum pacificum" | taxa == "Amansia rhodantha") %>%
  filter(treatment == "caged" | treatment == "exposed") %>%
  group_by(replicate, treatment, final_community_state) %>% 
  summarise(weight_g = sum(weight_g)) %>% 
  ungroup() %>% 
  group_by(treatment, final_community_state) %>% 
  summarise(
    n = n(),
    mean = mean(weight_g),
    sd = sd(weight_g),
    se = st.err(weight_g)
  )


# Figure 2b
# Barplot of palatable and unpalatable algal biomass in final community states
ggplot(biomass.palatable.state.summary, 
       aes(x=factor(final_community_state, level = c('Unpalatable-dominated', 'Palatable-dominated', 'Turf/CCA-dominated')), 
           y=mean)) + 
  geom_bar(stat="identity", 
           color="black", 
           position=position_dodge(), 
           fill = Pmac,
           width = 0.7) +
  geom_errorbar(aes(ymin=mean-se, 
                    ymax=mean+se), 
                width=.2,
                position=position_dodge(.9)) +
  xlab("\n Final Community State") +
  ylab("Mean biomass (+/- SE)") +
 # ggtitle("Final biomass of palatable macroalgae") +
  scale_y_continuous(limits = c(0,255), expand = c(0, 0)) +
  theme_classic() +
  theme(text = element_text(size = 23)) +
  theme(axis.text.x = element_text(colour = c(Udom, Pdom, Tdom), face = "bold"))

#save_plot("final.palatable.biomass.by.community.state.png", 10, 7, 500, "output/Final Communities")
