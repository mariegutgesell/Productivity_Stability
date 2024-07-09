##Pollen Analysis -- Sensitivity Analysis

##Author(s): Marie Gutgesell
##Version: 2024-06-20


#Pkgs
library(tidyverse)
library(ggpubr)
library(latticeExtra)
library(RColorBrewer)
library(RcppRoll)
library(zoo)

#load data
df_pollen <- read.csv("Data/pollen_record.csv", header = T)

#set seed
set.seed(093)

##### Code #####

###### Set up data frame #####
#Re-arrange year
df_pollen_year <- separate(df_pollen, 1, into = c("Year"), ' ', extra = 'drop')

#ensure year is numeric values
df_pollen_year$Year <- as.numeric(df_pollen_year$Year)

df_pollen_pre <- df_pollen_year %>% filter(Year < -1) %>%
  mutate(ag = 'Pre-Agriculture')

df_pollen_post <- df_pollen_year %>% filter(Year > -1) %>%
  mutate(ag = 'Post-Agriculture')

df_pollen_ag <- rbind(df_pollen_pre, df_pollen_post)

##number of pollen records in each 100-year interval
pollen_rec <- df_pollen_ag %>%
  group_by(ag, Year) %>%
  count()

df_pollen_trees <- df_pollen_ag %>% select(Year, trees, ag) %>%
  mutate(proportion = trees,
         pollen = "trees") %>%
  select(- trees)


df_pollen_crops <- df_pollen_ag %>% select(Year, cerealia, crops, ag) %>% ##updated to add cerealia + crops 
  mutate(proportion = (cerealia + crops),
         pollen = "crops") %>%
  select(- crops, - cerealia)


df_pollen_tc <- rbind(df_pollen_trees, df_pollen_crops)


#Sensitivity Analysis
##Want to randomly draw 12 replicates per 100 year period, and then calculate mean, and rolling SD over 5 time period window, and then repeat this process 1000 times
#using a monte carlo simulation
#that randomly pulls the minimum number of records within a given year (12)


##function to randomly select 12 records, calculate mean and then rolling sd of mean across 5 year period
sd_func <- function(x) {
  min_rows <- min(sapply(split(x, x$Year), function(x) nrow(x)))
  sample_rows <- lapply(split(x, x$Year), function(x) x[sample(nrow(x), min_rows), ])
  df_prop <- do.call(rbind, sample_rows)
  
  df_avg <- df_prop %>%
    group_by(Year) %>%
    summarize(mean_prop = mean(proportion),
             sd_prop = mean(proportion)) %>%
    mutate(order = 1:60)
  
  df_year <- x %>%
    select(Year, ag) %>%
    distinct() %>%
    mutate(order = 1:60)
  
  df_sd <- df_avg %>% select(order, mean_prop)
  sd <- rollapply(df_sd, width = 5, FUN = sd, by = 1, align = 'center') 
  df_sd<- as.data.frame(sd) %>%
    rename(sd_5yr = "mean_prop") %>%
    mutate(order = 5:60) %>%
    left_join(df_year %>% select(Year, ag, order), by = "order")
  
}

##run function for 1000 iterations
trees <- replicate(1000, sd_func(df_pollen_trees), simplify = FALSE)
trees_iteration <- bind_rows(trees) %>%
  mutate(Species = "Trees")

crops <- replicate(1000, sd_func(df_pollen_crops), simplify = FALSE)
crops_iteration <- bind_rows(crops) %>%
  mutate(Species = "Crops")

sd_iterations <- rbind(trees_iteration, crops_iteration) 

##calculate mean and sd, se for iterations 
sd_iterations_avg <- sd_iterations %>%
  group_by(ag, Species) %>%
  summarise( mean_sd_pollen = mean(sd_5yr),
             sd_sd_mean = sd(sd_5yr),
             count = n(),
             se_sd = (sd_sd_mean/(sqrt(count))))

#Mean among year variance ggplot
sd_iterations_avg$Species <- factor(sd_iterations_avg$Species, levels = c("Trees", "Crops"))
sd_iterations_avg$ag <- factor(sd_iterations_avg$ag, levels = c("Pre-Agriculture", "Post-Agriculture"))

sd_iterations$Species <- factor(sd_iterations$Species, levels = c("Trees", "Crops"))
sd_iterations$ag <- factor(sd_iterations$ag, levels = c("Pre-Agriculture", "Post-Agriculture"))
##ggplot 
gg_var <- ggplot(data = sd_iterations_avg, aes(x = Species, y = mean_sd_pollen, group = ag, fill = ag)) +
  geom_bar(stat = 'identity',
           color = 'black',
           width = 0.3,
           position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_sd_pollen - se_sd, ymax = mean_sd_pollen + se_sd), width = 0.3, position = 'dodge') +
  ylab("Mean Temporal Variance (SD)") +
  xlab("Trees vs Crops") +
  theme_classic() +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y=element_text(size = 12), 
        axis.title.x = element_blank(), 
        text=element_text(family = "Helvetica")) +
  scale_fill_brewer(palette = 'Palette')
gg_var

##boxplot
gg_var_2 <- ggplot(sd_iterations, aes(x = Species, y = sd_5yr, fill = ag)) +
  geom_boxplot(alpha = 0.6, position = position_dodge(width = 1), outliers = F) +
  # geom_jitter(aes(x = Species, y = sd_5yr, fill = ag), position = position_dodge(width = 1))+
  ylab("Mean Temporal Variance (SD)") +
  xlab("Trees vs Crops") +
  labs(fill = "Time Period")+
  theme_classic() +
  theme(legend.position = "right", 
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_text(size = 12), 
        axis.title.x = element_blank(), 
        text=element_text(family = "Helvetica")) +
  scale_fill_manual(values = c("darkolivegreen3", "darkgoldenrod1")) #+
# scale_fill_brewer(palette = 'Palette')


gg_var_2


##Two-way ANOVA comparing SD pre- and post- ag
df_sd_window_5_fig <- gather(df_sd_window_5, plant_type, sd_5yr, sd_trees_5yr:sd_crops_5yr)

sd_aov <- aov(sd_5yr ~ ag*Species, sd_iterations)
summary(sd_aov)

tree_ttest <- t.test(sd_5yr ~ ag, trees_iteration)
tree_ttest

crop_ttest <- t.test(sd_5yr ~ ag, crops_iteration)
crop_ttest

##Save figure S4
ggsave("Figures/Figure_S4.pdf", gg_var_2)

