##Pollen Data Analysis -- Mean and variance trends of other plant groups
##Supplemental Figure # 


#Author(s):Marie Gutgesell & Reilly O'Connor
#Version: 2024-05-08

#Pkgs
library(tidyverse)
library(ggpubr)
library(latticeExtra)
library(RColorBrewer)
library(RcppRoll)
library(zoo)
library(reshape2)

#load data
df_pollen <- read.csv("Data/pollen_record.csv", header = T)

##### Code #####

###### Set up data frame #####
#Re-arrange year
df_pollen_year <- separate(df_pollen, 1, into = c("Year"), ' ', extra = 'drop')

#ensure year is numeric values
df_pollen_year$Year <- as.numeric(df_pollen_year$Year)


#calculate averages across sites for given 100 year segments
df_pollen_ave <- df_pollen_year %>% 
  mutate(crops_all = cerealia + crops) %>% ##added cerealia + crops to get total proportion of cultivated plant pollen
  group_by(Year) %>%
  summarize(mean_trees = mean(trees),
            mean_crops = mean(crops_all),
            sd_trees = sd(trees),
            sd_crops = sd(crops_all),
            mean_cyper =  mean(cyperaceae),
            sd_cyper = sd(cyperaceae),
            mean_poec = mean(poeceae),
            sd_poec = sd(poeceae),
            mean_herb = mean(herbaceous_plants),
            sd_herb = sd(herbaceous_plants),
)

##### Add pre post agriculture column ##### 
df_pollen_pre <- df_pollen_ave %>% filter(Year < -1) %>%
  mutate(ag = 'Pre-Agriculture')

df_pollen_post <- df_pollen_ave %>% filter(Year > -1) %>%
  mutate(ag = 'Post-Agriculture')


df_pollen_ag <- rbind(df_pollen_pre, df_pollen_post)


##### Calculate ST dev of 5 time point moving windows ##### 
##Here, we are taking the standard deviation of the means within 5 century moving windows (each time point is average of 100 years, moving window across 5 time points)
##Then, we are taking the mean of the standard deviation of the means pre and post agriculture for both crops and trees

df_pollen_sd <- df_pollen_ag %>% select(Year, mean_trees, mean_crops, mean_cyper, mean_poec, mean_herb, ag) %>%
  mutate(order = 1:60)

##trees
df_tree_sd <- df_pollen_sd %>% select(order, mean_trees)
sd_trees <- rollapply(df_tree_sd, width = 5, FUN = sd, by = 1, align = 'center') ##i think we want sd of the means across the 5 years.. not mean of the sd 
df_sd_trees <- as.data.frame(sd_trees) %>%
  rename(sd_trees_5yr = "mean_trees") ##the column stays named mean trees but this is actually the sd of the mean proportion across 5 years

##crops
df_crops_sd <- df_pollen_sd %>% select(order, mean_crops)
sd_crops <- rollapply(df_crops_sd, width = 5, FUN = sd, by = 1, align = 'center')
df_sd_crops <- as.data.frame(sd_crops) %>%
  rename(sd_crops_5yr = "mean_crops")

##cyperaceae
df_cyper_sd <- df_pollen_sd %>% select(order, mean_cyper)
sd_cyper <- rollapply(df_cyper_sd, width = 5, FUN = sd, by = 1, align = 'center')
df_sd_cyper <- as.data.frame(sd_cyper) %>%
  rename(sd_cyper_5yr = "mean_cyper")

##poaceae
df_poec_sd <- df_pollen_sd %>% select(order, mean_poec)
sd_poec <- rollapply(df_poec_sd, width = 5, FUN = sd, by = 1, align = 'center')
df_sd_poec <- as.data.frame(sd_poec) %>%
  rename(sd_poec_5yr = "mean_poec")

##other herbaceous plants
df_herb_sd <- df_pollen_sd %>% select(order, mean_herb)
sd_herb <- rollapply(df_herb_sd, width = 5, FUN = sd, by = 1, align = 'center')
df_sd_herb <- as.data.frame(sd_herb) %>%
  rename(sd_herb_5yr = "mean_herb")

##join all together
df_sd_trees_ord <- df_sd_trees %>% mutate(order = 5:60)
df_sd_crops_ord <- df_sd_crops %>% mutate(order = 5:60)

df_sd_cyper_ord <- df_sd_cyper %>% mutate(order = 5:60)
df_sd_poec_ord <- df_sd_poec %>% mutate(order = 5:60)
df_sd_herb_ord <- df_sd_herb %>% mutate(order = 5:60)


df_sd_windowed <- merge(df_sd_trees_ord, df_sd_crops_ord, by = c("order")) %>%
  merge(df_sd_cyper_ord, by = c("order")) %>%
  merge(df_sd_poec_ord, by = c("order")) %>% 
  merge(df_sd_herb_ord, by = c("order"))


df_years <- df_pollen_sd %>% select(Year, order, ag)

df_sd_window_5 <- merge(df_years, df_sd_windowed, by = c("order"))

df_sd_window_5 <- df_sd_window_5 %>% select(- order)


##Summarize mean sd for pre- and post- agriculture
df_tree_pollen <- df_sd_window_5 %>% select(Year, sd_trees_5yr, ag) %>%
  group_by(ag) %>% 
  summarize(Species = 'Trees',
            mean_sd_pollen = mean(sd_trees_5yr),
            sd_sd_mean = sd(sd_trees_5yr),
            count = n(),
            se_sd = (sd_sd_mean/(sqrt(count)))) 

df_crop_pollen <- df_sd_window_5 %>% select(Year, sd_crops_5yr, ag) %>%
  group_by(ag) %>%
  summarize(Species = 'Crops',
            mean_sd_pollen = mean(sd_crops_5yr),
            sd_sd_mean = sd(sd_crops_5yr),
            count = n(),
            se_sd = (sd_sd_mean/(sqrt(count))))

df_cyper_pollen <- df_sd_window_5 %>% select(Year, sd_cyper_5yr, ag) %>%
  group_by(ag) %>%
  summarize(Species = 'Cyperaceae',
            mean_sd_pollen = mean(sd_cyper_5yr),
            sd_sd_mean = sd(sd_cyper_5yr),
            count = n(),
            se_sd = (sd_sd_mean/(sqrt(count))))

df_poec_pollen <- df_sd_window_5 %>% select(Year, sd_poec_5yr, ag) %>%
  group_by(ag) %>%
  summarize(Species = 'Poaceae',
            mean_sd_pollen = mean(sd_poec_5yr),
            sd_sd_mean = sd(sd_poec_5yr),
            count = n(),
            se_sd = (sd_sd_mean/(sqrt(count))))

df_herb_pollen <- df_sd_window_5 %>% select(Year, sd_herb_5yr, ag) %>%
  group_by(ag) %>%
  summarize(Species = 'Herbaceous Plants',
            mean_sd_pollen = mean(sd_herb_5yr),
            sd_sd_mean = sd(sd_herb_5yr),
            count = n(),
            se_sd = (sd_sd_mean/(sqrt(count))))


df_pollen_ct <- rbind(df_tree_pollen, df_crop_pollen, df_cyper_pollen, df_poec_pollen, df_herb_pollen) %>%
  filter(Species %in% c("Cyperaceae", "Poaceae", "Herbaceous Plants"))

##Figure S3.A) Mean temporal pollen proportions over time 
df_pollen_mean_long <- df_pollen_ag %>%
  select(Year, ag, mean_trees, mean_crops, mean_cyper, mean_poec, mean_herb) %>%
  melt(id.vars = c("Year", "ag", "mean_trees"), variable.name = "group")

df_pollen_mean_long$group <- as.character(df_pollen_mean_long$group) 

df_pollen_mean_long <- df_pollen_mean_long %>%
  mutate(Species = case_when(
    startsWith(group, "mean_cyper") ~ "Cyperaceae",
    startsWith(group, "mean_poec") ~ "Poaceae",
    startsWith(group, "mean_herb") ~ "Herbaceous Plants",
    startsWith(group, "mean_crop") ~ "Crops",
  ))
str(df_pollen_mean_long)

pollen_fig <- df_pollen_mean_long %>%
  filter(Species %in% c("Cyperaceae", "Poaceae", "Herbaceous Plants")) %>%
  ggplot(aes(x = Year, y = value, group = Species, colour = Species)) +
  geom_line() +
  scale_color_manual(values = c("black", "darkgrey", "lightgrey")) +
  labs(x = "Years Since Onset of Agricultural Development", y = "Mean Proportion Pollen") + 
  theme_classic() +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y=element_text(size = 10), 
        axis.title.x = element_text(size = 10), 
        text=element_text(family = "Helvetica"))

pollen_fig

####Figure S3_B) ---- Mean among year variance ggplot
df_pollen_ct$Species <- factor(df_pollen_ct$Species, levels = c("Cyperaceae", "Poaceae", "Herbaceous Plants"))
df_pollen_ct$ag <- factor(df_pollen_ct$ag, levels = c("Pre-Agriculture", "Post-Agriculture"))

df_sd_window_5_2 <- df_sd_window_5 %>%
  rename(Trees = "sd_trees_5yr", Crops = "sd_crops_5yr", Cyperaceae = "sd_cyper_5yr", Poaceae = "sd_poec_5yr", `Herbaceous Plants` = "sd_herb_5yr") %>%
  gather(key = "Species", value = "sd_5yr", -Year, -ag) %>%
  filter(Species %in% c("Cyperaceae", "Poaceae", "Herbaceous Plants"))

df_sd_window_5_2$ag <- factor(df_sd_window_5_2$ag, levels = c("Pre-Agriculture", "Post-Agriculture"))
df_sd_window_5_2$Species <- factor(df_sd_window_5_2$Species, levels = c("Cyperaceae", "Poaceae", "Herbaceous Plants"))


##ggplot 
gg_var <- df_pollen_ct %>%
  filter(Species %in% c("Cyperaceae", "Poaceae", "Herbaceous Plants")) %>%
  ggplot() +
  geom_bar(data = df_pollen_ct, aes(x = Species, y = mean_sd_pollen, group = ag, fill = ag), stat = 'identity',
           color = 'black',
           width = 0.3,
           position = position_dodge(width = 0.3), alpha = 0.4) +
  geom_errorbar(data = df_pollen_ct, aes(x = Species, ymin = mean_sd_pollen - se_sd, ymax = mean_sd_pollen + se_sd, group = ag), width = 0.3, position = 'dodge') +
  geom_point(data = df_sd_window_5_2, aes(x = Species, y = sd_5yr, group = ag, color = ag), position = position_dodge(width = 0.3)) +
  ylab("Mean Temporal Variance (SD)") +
  xlab("Trees vs Crops") +
  theme_classic() +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y=element_text(size = 10), 
        axis.title.x = element_blank(), 
        text=element_text(family = "Helvetica")) +
  scale_fill_manual(values = c("darkolivegreen3", "darkgoldenrod1")) +
  scale_color_manual(values = c("darkolivegreen4", "darkgoldenrod3"))

gg_var



##Combine Figure
supp_pollen_fig <- ggarrange(pollen_fig, gg_var, labels = c("a)", "b)"),
                             ncol = 2, nrow = 1, font.label = list(colour = "black", size = 12, family = "Helvetica") )

supp_pollen_fig


##save figure

ggsave(path = "Figures", filename="Figure_S3.pdf", supp_pollen_fig, 
       height=3600, width=6000,
       units="px", dpi = 800)



#####Statistical Analysis ##############
##Two-way ANOVA comparing SD pre- and post- ag
df_sd_window_5_fig <- gather(df_sd_window_5, plant_type, sd_5yr, sd_trees_5yr:sd_crops_5yr)
sd_aov <- aov(sd_5yr ~ ag*plant_type, df_sd_window_5_fig)
summary(sd_aov)

tree_ttest <- t.test(sd_trees_5yr ~ ag, df_sd_window_5)
tree_ttest

crop_ttest <- t.test(sd_crops_5yr ~ ag, df_sd_window_5)
crop_ttest


cyp_ttest <- t.test(sd_cyper_5yr ~ ag, df_sd_window_5)
cyp_ttest

poac_ttest <- t.test(sd_poec_5yr ~ ag, df_sd_window_5)
poac_ttest

herb_ttest <- t.test(sd_herb_5yr ~ ag, df_sd_window_5)
herb_ttest

##calculating magnitude of change since onset of ag for crops and trees

df <- df_pollen_ag %>%
  filter(ag == "Post-Agriculture") %>%
  filter(Year %in% c("0", "2901")) %>%
  select(Year, mean_trees, mean_crops, mean_cyper, mean_poec, mean_herb) %>%
  gather(plant_type, mean_prop, mean_trees:mean_herb)

mag_diff_trees <- (0.38114136/0.60640240)*100
mag_diff_crops <- (0.03849481/0.02048613)*100

0.60640240*0.6285
0.02048613*1.879


mag_diff_cyper <- (0.10802310/0.06546567)*100
mag_diff_poac <- (0.14226816/0.07866852)*100
mag_diff_herb <- (0.09923793/0.03816122)*100

0.06546567*1.65

