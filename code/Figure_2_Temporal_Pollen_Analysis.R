##Pollen Data Analysis
##Figure 2 and statistical tests for main manuscript 

#Author(s): Reilly O'Connor
#Version: 2023-05-08

#Pkgs
library(tidyverse)
library(ggpubr)
library(latticeExtra)
library(RColorBrewer)
library(RcppRoll)
library(zoo)

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
            cv_trees = sd_trees/mean_trees,
            cv_crops = sd_crops/mean_crops)

##### Add pre post agriculture column ##### 
df_pollen_pre <- df_pollen_ave %>% filter(Year < -1) %>%
  mutate(ag = 'Pre-Agriculture')

df_pollen_post <- df_pollen_ave %>% filter(Year > -1) %>%
  mutate(ag = 'Post-Agriculture')

df_pollen_ag <- rbind(df_pollen_pre, df_pollen_post)


##### Calculate ST dev of 5 time point moving windows ##### 
##Here, we are taking the standard deviation of the means within 5 century moving windows (each time point is average of 100 years, moving window across 5 time points)
##Then, we are taking the mean of the standard deviation of the means pre and post agriculture for both crops and trees

df_pollen_sd <- df_pollen_ag %>% select(Year, mean_trees, mean_crops, ag) %>%
  mutate(order = 1:60)

df_tree_sd <- df_pollen_sd %>% select(order, mean_trees)
sd_trees <- rollapply(df_tree_sd, width = 5, FUN = sd, by = 1, align = 'center') 
df_sd_trees <- as.data.frame(sd_trees) %>%
  rename(sd_trees_5yr = "mean_trees") ##the column stays named mean trees but this is actually the sd of the mean proportion across 5 years

df_crops_sd <- df_pollen_sd %>% select(order, mean_crops)
sd_crops <- rollapply(df_crops_sd, width = 5, FUN = sd, by = 1, align = 'center')
df_sd_crops <- as.data.frame(sd_crops) %>%
  rename(sd_crops_5yr = "mean_crops")

df_sd_trees_ord <- df_sd_trees %>% mutate(order = 5:60)
df_sd_crops_ord <- df_sd_crops %>% mutate(order = 5:60)

df_sd_windowed <- merge(df_sd_trees_ord, df_sd_crops_ord, by = c("order"))

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

df_pollen_ct <- rbind(df_tree_pollen, df_crop_pollen)

##### Figure 2 - Pollen Figures ##### 

##Figure 2A) Time series of mean proportion tree/crop pollen
# --> Make the plot with second y axis:
# --> construct separate plots for each series
mean_tree <- xyplot(mean_trees ~ Year, df_pollen_ag, type = "l" , lwd=2, lty = 1, col="black", ylab = "", xlab = "")
mean_crop <- xyplot(mean_crops ~ Year, df_pollen_ag, type = "l", lwd=2, lty = 2, col="black", ylab = "", xlab = "")

par(font.lab = 2, cex.lab = 1.5, family = "helvetica") # Set font and size for axis labels

mean_proportion_plot <- doubleYScale(mean_tree, mean_crop, add.ylab2 = FALSE, use.style=FALSE)
mean_proportion_plot




##Figure 2B) Mean Temporal Variance 
##Set up df and proper ordered factor for figure formatting
df_pollen_ct$Species <- factor(df_pollen_ct$Species, levels = c("Trees", "Crops"))
df_pollen_ct$ag <- factor(df_pollen_ct$ag, levels = c("Pre-Agriculture", "Post-Agriculture"))

df_sd_window_5_2 <- df_sd_window_5 %>%
  rename(Trees = "sd_trees_5yr", Crops = "sd_crops_5yr") %>%
  gather(key = "Species", value = "sd_5yr", -Year, -ag)

df_sd_window_5_2$ag <- factor(df_sd_window_5_2$ag, levels = c("Pre-Agriculture", "Post-Agriculture"))
df_sd_window_5_2$Species <- factor(df_sd_window_5_2$Species, levels = c("Trees", "Crops"))

#Plot figure
gg_var <- ggplot() +
  geom_bar(data = df_pollen_ct, aes(x = Species, y = mean_sd_pollen, group = ag, fill = ag), stat = 'identity',
           color = 'black',
           width = 0.9,
           position = position_dodge(width = 0.9), alpha = 0.4) +
  geom_errorbar(data = df_pollen_ct, aes(x = Species, ymin = mean_sd_pollen - se_sd, ymax = mean_sd_pollen + se_sd, group = ag), width = 0.3, position = position_dodge(width = 0.9)) +
  geom_point(data = df_sd_window_5_2, aes(x = Species, y = sd_5yr, group = ag, color = ag), position = position_dodge(width = 0.9)) +
  ylab("Mean Temporal Variance (SD)") +
  xlab("Trees vs Crops") +
  theme_classic() +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y=element_text(size = 12), 
        axis.title.x = element_blank(), 
        text=element_text(family = "Helvetica")) +
  scale_fill_manual(values = c("darkolivegreen3", "darkgoldenrod1")) +
  scale_color_manual(values = c("darkolivegreen4", "darkgoldenrod3"))
gg_var



##Save figures 
#Manually save mean_proportion plot as .TIFF file
tiff("figures/Figure_2A.Tiff", height=3.6, width=6, units="in", res=800, compression="lzw")
mean_proportion_plot
dev.off()


ggsave(path = "figures", filename="Figure_2B.Tiff", gg_var, 
       height=3600, width=6000,
       units="px", dpi = 800, compression="lzw")


###Statistical Analysis ########## ---------------------
##Two-Anova 
df_sd_window_5_fig <- gather(df_sd_window_5, plant_type, sd_5yr, sd_trees_5yr:sd_crops_5yr)
sd_aov <- aov(sd_5yr ~ ag*plant_type, df_sd_window_5_fig)
summary(sd_aov)


#Two-sample T-Test comparing SD pre- and post- ag
library(car)
df_sd_window_5 <- df_sd_window_5 %>%
  mutate(sd_trees_5yr_log = log(sd_trees_5yr))

hist(df_sd_window_5$sd_trees_5yr)
hist(df_sd_window_5$sd_trees_5yr_log)
qqnorm(df_sd_window_5$sd_trees_5yr)
qqnorm(df_sd_window_5$sd_trees_5yr_log)
shapiro.test(df_sd_window_5$sd_trees_5yr)
shapiro.test(df_sd_window_5$sd_trees_5yr_log)

tree_ttest <- t.test(sd_trees_5yr ~ ag, df_sd_window_5)
leveneTest(sd_trees_5yr ~ ag, df_sd_window_5)
tree_ttest

tree_ttest_2 <- t.test(sd_trees_5yr_log ~ ag, df_sd_window_5)
leveneTest(sd_trees_5yr_log ~ ag, df_sd_window_5)
tree_ttest_2


df_sd_window_5 <- df_sd_window_5 %>%
  mutate(sd_crops_5yr_log = log(sd_crops_5yr))

hist(df_sd_window_5$sd_crops_5yr)
hist(df_sd_window_5$sd_crops_5yr_log)
qqPlot(df_sd_window_5$sd_crops_5yr_log)
shapiro.test(df_sd_window_5$sd_crops_5yr_log)
qqPlot(df_sd_window_5$sd_crops_5yr)
shapiro.test(df_sd_window_5$sd_crops_5yr)

crop_ttest <- t.test(sd_crops_5yr ~ ag, df_sd_window_5)
leveneTest(sd_crops_5yr ~ ag, df_sd_window_5)
crop_ttest

crop_ttest2 <- t.test(sd_crops_5yr_log ~ ag, df_sd_window_5)
leveneTest(sd_crops_5yr_log ~ ag, df_sd_window_5)
crop_ttest2

##calculating magnitude of change since onset of ag for crops and trees

df <- df_pollen_ag %>%
  filter(ag == "Post-Agriculture") %>%
  filter(Year %in% c("0", "2901")) %>%
  select(Year, mean_trees, mean_crops) %>%
  gather(plant_type, mean_prop, mean_trees:mean_crops)

mag_diff_trees <- (0.38114136/0.60640240)*100
mag_diff_crops <- (0.03849481/0.02048613)*100

0.60640240*0.6285
0.02048613*1.879
