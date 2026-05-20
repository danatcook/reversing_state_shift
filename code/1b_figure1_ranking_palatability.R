# Code to produce Figure 1
# Scatterplot showing percent consumption of 3 focal taxa using palatability assay data 
# Data collected: Aug 30 - Sep 1, 2020


# LOAD PACKAGES & FUNCTIONS----------------------------------
library(readr)
library(tidyverse)
library(janitor)

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

# DATA ------
palDat <- read_csv("data/palatabilityDat.csv")

palDat$replicate <- as.factor(palDat$replicate)
palDat$taxa <- as.factor(palDat$taxa)
palDat$treatment <- as.factor(palDat$treatment)
palDat$assay_style <- as.factor(palDat$assay_style)

# cafeteria-style data
cafDat <- palDat %>% 
  filter(assay_style == "Cafeteria")

sinDat <- palDat %>% 
  filter(assay_style == "Single species")


# DATA WRANGLING: Raw consumption and percent consumed -----

# Calculate raw amount consumed (initial weight - final weight) and percent consumed [(initial-final) / initial x 100%] 
cafDat <- cafDat %>% 
  select(replicate, taxa, assay_style, treatment, initial_weight_g, final_weight_g) %>% 
  mutate(rawConsump = initial_weight_g - final_weight_g) %>% 
  mutate(percConsump = abs((initial_weight_g - final_weight_g) / initial_weight_g * 100))

# Calculate mean, count, SD, SE, 95% CI for raw consumption (initial - final) grouped by taxa and treatment
cafDat_rawConsump <- summarySE(cafDat, measurevar="rawConsump", groupvars=c("taxa","treatment"))

# Calculate mean, count, SD, SE, 95% CI for percent change in weight grouped by taxa and treatment
cafDat_percConsump <- summarySE(cafDat, measurevar="percConsump", groupvars=c("taxa","treatment"))


# Calculate raw amount consumed (initial weight - final weight) and percent consumed [(initial-final) / initial x 100%] 
sinDat <- sinDat %>% 
  select(replicate, taxa, assay_style, treatment, initial_weight_g, final_weight_g) %>% 
  mutate(rawConsump = initial_weight_g - final_weight_g) %>% 
  mutate(percConsump = abs((initial_weight_g - final_weight_g) / initial_weight_g * 100))

# Calculate mean, count, SD, SE, 95% CI for raw consumption (initial - final) grouped by taxa and treatment and replicate (to average duplicates and return summary stats for each replicate)
sinDat_rawConsumpDup <- summarySE(sinDat, measurevar="rawConsump", groupvars=c("taxa","treatment", "replicate"))

# Then calculate mean, count, SD, SE, 95% CI for percent change in weight grouped by taxa and treatment (average replicates and return summary stats for each taxa and treatment)
sinDat_rawConsump <- summarySE(sinDat_rawConsumpDup, measurevar="rawConsump", groupvars=c("taxa","treatment"))

# Calculate mean, count, SD, SE, 95% CI for percent change in weight grouped by taxa and treatment and replicate (to average duplicates and return summary stats for each replicate)
sinDat_percConsumpDup <- summarySE(sinDat, measurevar="percConsump", groupvars=c("taxa","treatment", "replicate"))

# Then calculate mean, count, SD, SE, 95% CI for percent change in weight grouped by taxa and treatment
sinDat_percConsump <- summarySE(sinDat_percConsumpDup, measurevar = "percConsump", groupvars = c("taxa", "treatment"))



# GGPLOTS: percent consumed for cafeteria assays --------------------------------

# Scatter plot of means +/-SE of percent consumed for multiple spp (cafeteria) assays
ggplot()+
  geom_point(data = cafDat_percConsump %>% 
               filter(treatment == "Exposed"), 
             aes(x = taxa, 
                 y = percConsump
                 #color=treatment),
             ),
             size = 4)+
  geom_errorbar(data = cafDat_percConsump %>% 
                  filter(treatment == "Exposed"), 
                aes(x = taxa,
                    ymin = percConsump - se, 
                    ymax = percConsump + se), 
                width = 0) +
  # geom_jitter(data = cafDat, 
  #             aes(x = taxa, 
  #                 y = percConsump,
  #                 color=treatment),
  #             height = 0,
  #             width = 0.1,
  #             alpha = 0.4,
  #             size = 3) +
  scale_y_continuous(
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  labs(x = "",
       y = "Percent consumed in 48 hr",
       colour = "Treatment") +
  ggtitle("Cafeteria-style Assays") +
  scale_x_discrete(
    labels = c(
      "Amansia rhodantha" = "Amansia",
      "Sargassum pacificum" = "Sargassum",
      "Turbinaria ornata" = "Turbinaria"
    )) +
  theme_classic() +
  theme(text = element_text(size = 20))  

ggsave("mixAssays_percConsumed_means.tiff", # name of the plot to save, must end with '.png' or other file type
       width = 7,
       height = 5,
       units = "in",
       dpi = 300, # high resolution
       path = "output")

# Scatter plot of means +/-SE of percent consumed for singe sp assays
ggplot()+
  geom_point(data = sinDat_percConsump %>% 
               filter(treatment == "Exposed"), 
             aes(x = taxa, 
                 y = percConsump),
                # color=treatment),
             size = 4)+
  geom_errorbar(data = sinDat_percConsump %>% 
                  filter(treatment == "Exposed"), 
                aes(x = taxa,
                    ymin = percConsump - se, 
                    ymax = percConsump + se), 
                width = 0) +
  # geom_jitter(data = cafDat, 
  #             aes(x = taxa, 
  #                 y = percConsump,
  #                 color=treatment),
  #             height = 0,
  #             width = 0.1,
  #             alpha = 0.4,
  #             size = 3) +
  scale_y_continuous(
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  labs(x = "",
       y = "Percent consumed in 48 hr",
       colour = "Treatment") +
  ggtitle("Single Species Assays") +
  scale_x_discrete(
    labels = c(
      "Amansia rhodantha" = "Amansia",
      "Sargassum pacificum" = "Sargassum",
      "Turbinaria ornata" = "Turbinaria"
    )) +
  theme_classic() +
  theme(text = element_text(size = 20))

ggsave("sinAssays_percConsumed_means.tiff", # name of the plot to save, must end with '.png' or other file type
       width = 7,
       height = 5,
       units = "in",
       dpi = 300, # high resolution
       path = "output")








# GRAVEYARD -----



# Paired line plot of cafeteria assay data, exposed treatment
# Define ggplot's default blue color
ggplot_blue <- "#619CFF"
# Plot of percent consumed [abs((initial - final) / initial x 100%)] for cafeteria assay data
ggplot(data = cafDat %>% 
         filter(treatment == "Exposed"), 
       aes(x = taxa, 
           y = percConsump,
           group = replicate)) +
         geom_point(color = ggplot_blue, size = 3) +  # Scatter points
         geom_line(color = ggplot_blue, alpha = 1) +  # Lines linking replicates
  labs(x = "",
       y = "Percent consumed in 48 hr",
       colour = "Treatment") +
  ggtitle("Cafeteria-style Assays") +
  theme_classic() +
  theme(text = element_text(size = 20)) 

# ggsave("cafAssays_percConsumed_paired_line_exposed.tiff", # name of the plot to save, must end with '.png' or other file type
#        width = 11,
#        height = 6,
#        units = "in",
#        dpi = 300, # high resolution
#        path = "output")

# Paired line plot of cafeteria assay data, cage control treatment
# Define ggplot's default red color
ggplot_red <- "#F8766D"
# Plot of percent consumed [abs((initial - final) / initial x 100%)] for cafeteria assay data
ggplot(data = cafDat %>% 
         filter(treatment == "Caged control"), 
       aes(x = taxa, 
           y = percConsump,
           group = replicate)) +
  geom_point(color = ggplot_red, size = 3) +  # Scatter points
  geom_line(color = ggplot_red, alpha = 0.5) +  # Lines linking replicates
  ylim(0, 100) +
  labs(x = "",
       y = "Percent consumed in 48 hr",
       colour = "Treatment") +
  ggtitle("Cafeteria-style Assays") +
  theme_classic() +
  theme(text = element_text(size = 20)) 

# ggsave("cafAssays_percConsumed_paired_line_caged.tiff", # name of the plot to save, must end with '.png' or other file type
#        width = 11,
#        height = 6,
#        units = "in",
#        dpi = 300, # high resolution
#        path = "output")


ggplot()+
  geom_jitter(data = cafDat, 
              aes(x = taxa, 
                  y = percConsump,
                  color=treatment),
              height = 0.25, # vertical jitter spread
              width = 0.25, # horizontal jitter spread
              alpha = 0.6,
              size = 3) +
  labs(x = "",
       y = "Percent consumed in 48 hr",
       colour = "Treatment") +
  ggtitle("Cafeteria-style Assays") +
  theme_classic() +
  theme(text = element_text(size = 20))   

# ggsave("cafAssays_percConsumed_scatter.tiff", # name of the plot to save, must end with '.png' or other file type
#        width = 11,
#        height = 6,
#        units = "in",
#        dpi = 300, # high resolution
#        path = "output")

# Boxplot of percent consumption data for mixed species assays
ggplot(cafDat, aes(x = taxa, 
                   y = percConsump, 
                   fill = treatment)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Mixed-species Assay", x = "Taxa", y = "Percent consumed in 48 hr")

# ggsave("cafAssays_percConsumed_boxplot.tiff", # name of the plot to save, must end with '.png' or other file type
#        width = 11,
#        height = 6,
#        units = "in",
#        dpi = 300, # high resolution
#        path = "output")


# GGPLOTS: percent consumed for single species assays --------------------------------
# Plot of percent consumed [abs((initial - final) / initial x 100%)] for single species
ggplot() +
geom_jitter(data = sinDat, 
            aes(x = taxa, 
                y = percConsump,
                color=treatment),
            height = 0.25, # vertical jitter spread
            width = 0.25, # horizontal jitter spread
            alpha = 0.7,
            size = 3) +
  scale_color_manual(values = c("Caged control" = ggplot_red, "Exposed" = ggplot_blue)) +  # Set custom colors
  labs(x = "",
       y = "Percent consumed in 48 hr",
       colour = "Treatment") +
  ggtitle("Single Species Assays") +
  theme_classic() +
  theme(text = element_text(size = 20))

# ggsave("sinAssays_percConsumed_scatter.tiff", # name of the plot to save, must end with '.png' or other file type
#        width = 11,
#        height = 6,
#        units = "in",
#        dpi = 300, # high resolution
#        path = "output")

