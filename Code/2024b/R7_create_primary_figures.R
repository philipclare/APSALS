##############################################################################
##   
## APSALS Wave 11 Initiation Trajectories Paper
## Create figures from Stata analysis output for primary analysis
## Author: Philip J Clare
## Date: 22 June 2023
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## Analysis pre-registered at DOI:10.17605/OSF.IO/BRDUV
##
##############################################################################
# 1. Setup Environment
#-----------------------------------------------------------------------------

# 1.1. Specify working directory
workdir <- "D:/UNSW/APSALS - Documents/Papers/PIP46. Age of initiation and the trajectory of alcohol use and harm/"

# 1.2 Check and load packages
libs <- c("cowplot","ggplot2")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 2. Load the pooled results and clean for figure creation
#-------------------------------------------------------------------------------------

# 2.1. Alcohol consumption results
alcfq_res <- readRDS(paste0(workdir,"Results/Raw/Primary alcfq_res 20240501.rds"))
alcfq_res$age2 <- ifelse(alcfq_res$age-alcfq_res$init<=3,alcfq_res$age-alcfq_res$init,NA)
alcfq_res$age <- factor(alcfq_res$age,
                        labels=c("11","12","13","14","15","16","17","18","19","20","21","22","23"))
alcfq_res$age2 <- factor(alcfq_res$age2,
                         labels=c("0","1","2","3"))
alcfq_res$init <- factor(alcfq_res$init,
                         labels=c("11","12","13","14","15","16","17","18","19","20"))
alcfq_res$outcome <- "Consumption"

# 2.2. Monthly heavy episodic drinking results
hed_res <- readRDS(paste0(workdir,"Results/Raw/Primary hed_res 20240501.rds"))
hed_res$age2 <- ifelse(hed_res$age-hed_res$init<=3,hed_res$age-hed_res$init,NA)
hed_res$age <- factor(hed_res$age,
                      labels=c("11","12","13","14","15","16","17","18","19","20","21","22","23"))
hed_res$age2 <- factor(hed_res$age2,
                       labels=c("0","1","2","3"))
hed_res$init <- factor(hed_res$init,
                       labels=c("11","12","13","14","15","16","17","18","19","20"))
hed_res$outcome <- "HED"

# 2.3. Any heavy episodic drinking results
anyhed_res <- readRDS(paste0(workdir,"Results/Raw/Primary anyhed_res 20240501.rds"))
anyhed_res$age2 <- ifelse(anyhed_res$age-anyhed_res$init<=3,anyhed_res$age-anyhed_res$init,NA)
anyhed_res$age <- factor(anyhed_res$age,
                         labels=c("11","12","13","14","15","16","17","18","19","20","21","22","23"))
anyhed_res$age2 <- factor(anyhed_res$age2,
                          labels=c("0","1","2","3"))
anyhed_res$init <- factor(anyhed_res$init,
                          labels=c("11","12","13","14","15","16","17","18","19","20"))
anyhed_res$outcome <- "HED"

# 2.4. Alcohol-related harms results
harms_res <- readRDS(paste0(workdir,"Results/Raw/Primary harms_res 20240501.rds"))
harms_res$age2 <- ifelse(harms_res$age-harms_res$init<=3,harms_res$age-harms_res$init,NA)
harms_res$age <- factor(harms_res$age,
                        labels=c("11","12","13","14","15","16","17","18","19","20","21","22","23"))
harms_res$age2 <- factor(harms_res$age2,
                         labels=c("0","1","2","3"))
harms_res$init <- factor(harms_res$init,
                         labels=c("11","12","13","14","15","16","17","18","19","20"))
harms_res$outcome <- "Harms"

# 2.5. Alcohol-related harms results
anyharms_res <- readRDS(paste0(workdir,"Results/Raw/Primary anyharms_res 20240501.rds"))
anyharms_res$age2 <- ifelse(anyharms_res$age-anyharms_res$init<=3,anyharms_res$age-anyharms_res$init,NA)
anyharms_res$age <- factor(anyharms_res$age,
                           labels=c("11","12","13","14","15","16","17","18","19","20","21","22","23"))
anyharms_res$age2 <- factor(anyharms_res$age2,
                            labels=c("0","1","2","3"))
anyharms_res$init <- factor(anyharms_res$init,
                            labels=c("11","12","13","14","15","16","17","18","19","20"))
anyharms_res$outcome <- "Harms"

# 2.6. DSM-IV Dependence results
depend_res <- readRDS(paste0(workdir,"Results/Raw/Primary depend_res 20240501.rds"))
depend_res$age2 <- ifelse(depend_res$age-depend_res$init<=3,depend_res$age-depend_res$init,NA)
depend_res$age <- factor(depend_res$age,
                         labels=c("14","15","16","17","18","19","20","21","22","23"))
depend_res$age2 <- factor(depend_res$age2,
                          labels=c("0","1","2","3"))
depend_res$init <- factor(depend_res$init,
                          labels=c("11","12","13","14","15","16","17","18","19","20"))
depend_res$outcome <- "Dependence"

# 2.7. DSM-IV Abuse results
abuse_res <- readRDS(paste0(workdir,"Results/Raw/Primary abuse_res 20240501.rds"))
abuse_res$age2 <- ifelse(abuse_res$age-abuse_res$init<=3,abuse_res$age-abuse_res$init,NA)
abuse_res$age <- factor(abuse_res$age,
                        labels=c("14","15","16","17","18","19","20","21","22","23"))
abuse_res$age2 <- factor(abuse_res$age2,
                         labels=c("0","1","2","3"))
abuse_res$init <- factor(abuse_res$init,
                         labels=c("11","12","13","14","15","16","17","18","19","20"))
abuse_res$outcome <- "Abuse"

# 2.8. DSM-5 AUD results
aud_res <- readRDS(paste0(workdir,"Results/Raw/Primary aud_res 20240501.rds"))
aud_res$age2 <- ifelse(aud_res$age-aud_res$init<=3,aud_res$age-aud_res$init,NA)
aud_res$age <- factor(aud_res$age,
                      labels=c("14","15","16","17","18","19","20","21","22","23"))
aud_res$age2 <- factor(aud_res$age2,
                       labels=c("0","1","2","3"))
aud_res$init <- factor(aud_res$init,
                       labels=c("11","12","13","14","15","16","17","18","19","20"))
aud_res$outcome <- "AUD"

######################################################################################
# 3. Define theme and common properties
#-------------------------------------------------------------------------------------

fig_dat_1 <- rbind(alcfq_res,hed_res,harms_res)
fig_dat_1$outcome <- factor(fig_dat_1$outcome, levels=c("Consumption","HED","Harms"))

fig_dat_2 <- rbind(depend_res,abuse_res,aud_res)
fig_dat_2$outcome <- factor(fig_dat_2$outcome, levels=c("Dependence","Abuse","AUD"))


######################################################################################
# 3. Define theme and common properties
#-------------------------------------------------------------------------------------

figure_theme <- theme_classic() +
  theme(panel.grid.major.y = element_line(color = "grey80", linewidth = 0.3),
        text = element_text(size = 8),
        axis.line = element_line(colour = 'grey80', linewidth = 0.3),
        axis.ticks = element_line(colour = "grey80", linewidth = 0.3),
        axis.title = element_text(size = 7),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(hjust = -0.01),
        legend.position = "bottom",
        legend.title = element_text(size = 7))

######################################################################################
# 4. Draw primary analysis figures
#-------------------------------------------------------------------------------------

fig1a <- ggplot(fig_dat_1[which(fig_dat_1$outcome=="Consumption" & (fig_dat_1$init=="12" | fig_dat_1$init=="14" | fig_dat_1$init=="16" | fig_dat_1$init=="18" | fig_dat_1$init=="20")),],
                aes(x=age, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age, fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Age") +
  ylab("Mean number of drinks") + 
  scale_y_continuous(limits=c(0,800), breaks=seq(0, 800, by = 200), expand = c(0, 0)) +
  figure_theme +
  labs(color = "Age of initiation",
       fill = "Age of initiation")

fig1b <- ggplot(fig_dat_1[which(fig_dat_1$outcome=="HED" & (fig_dat_1$init=="12" | fig_dat_1$init=="14" | fig_dat_1$init=="16" | fig_dat_1$init=="18" | fig_dat_1$init=="20")),],
                aes(x=age, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age, fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Age") +
  ylab("Proportion reported at least monthly HED") + 
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by = 0.2), expand = c(0, 0)) +
  figure_theme +
  labs(color = "Age of initiation",
       fill = "Age of initiation")

fig1c <- ggplot(fig_dat_1[which(fig_dat_1$outcome=="Harms" & (fig_dat_1$init=="12" | fig_dat_1$init=="14" | fig_dat_1$init=="16" | fig_dat_1$init=="18" | fig_dat_1$init=="20")),],
                aes(x=age, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age, fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Age") +
  ylab("Mean number of alcohol-specific harms") + 
  scale_y_continuous(limits=c(0,6), breaks=seq(0, 6, by = 2), expand = c(0, 0)) +
  figure_theme +
  labs(color = "Age of initiation",
       fill = "Age of initiation")

fig1 <- plot_grid(fig1a + theme(axis.text.x = element_blank(),
                                 axis.title.x = element_blank(),
                                 legend.position = "none"), 
                   fig1b + theme(axis.text.x = element_blank(),
                                 axis.title.x = element_blank(),
                                 legend.position = "none"), 
                   fig1c,
                   nrow = 3,
                   align = "v",
                   axis = "lr",
                   labels = list("(a) Number of drinks consumed in year",
                                 "(b) Monthly heavy episodic drinking",
                                 "(c) Number of alcohol-related Harms"),
                   label_size = 8,
                   vjust = c(2.25,2.25,2.25),
                   hjust = c(-0.21,-0.23,-0.23),
                   rel_heights = c(1,1,1.4))

fig2a <- ggplot(fig_dat_2[which(fig_dat_2$outcome=="Dependence" & (fig_dat_2$init=="14" | fig_dat_2$init=="16" | fig_dat_2$init=="18" | fig_dat_2$init=="20")),],
                aes(x=age, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age, fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Age") +
  ylab("Proportion reported dependence") + 
  scale_y_continuous(limits=c(0,0.4), breaks=seq(0, 0.4, by = 0.1), expand = c(0, 0)) +
  figure_theme +
  labs(color = "Age of initiation",
       fill = "Age of initiation")

fig2b <- ggplot(fig_dat_2[which(fig_dat_2$outcome=="Abuse" & (fig_dat_2$init=="14" | fig_dat_2$init=="16" | fig_dat_2$init=="18" | fig_dat_2$init=="20")),],
                aes(x=age, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age, fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Age") +
  ylab("Proportion reported abuse") + 
  scale_y_continuous(limits=c(0,0.4), breaks=seq(0, 0.4, by = 0.1), expand = c(0, 0)) +
  figure_theme +
  labs(color = "Age of initiation",
       fill = "Age of initiation")

fig2c <- ggplot(fig_dat_2[which(fig_dat_2$outcome=="AUD" & (fig_dat_2$init=="14" | fig_dat_2$init=="16" | fig_dat_2$init=="18" | fig_dat_2$init=="20")),],
                aes(x=age, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age, fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Age") +
  ylab("Proportion reported AUD") + 
  scale_y_continuous(limits=c(0,0.6), breaks=seq(0, 0.6, by = 0.2), expand = c(0, 0)) +
  figure_theme +
  labs(color = "Age of initiation",
       fill = "Age of initiation")

fig2 <- plot_grid(fig2a + theme(axis.text.x = element_blank(),
                                axis.title.x = element_blank(),
                                legend.position = "none"), 
                  fig2b + theme(axis.text.x = element_blank(),
                                axis.title.x = element_blank(),
                                legend.position = "none"), 
                  fig2c,
                  nrow = 3,
                  align = "v",
                  axis = "lr",
                  labels = list("(a) DSM-IV Alcohol Dependence",
                                "(b) DSM-IV Alcohol Abuse",
                                "(c) DSM-5 Alcohol Use Disorder"),
                  label_size = 8,
                  vjust = c(2.25,2.25,2.25),
                  hjust = c(-0.25,-0.3,-0.25),
                  rel_heights = c(1,1,1.4))

ggsave(plot = fig1,
       filename = paste0(workdir,"Results/F1 Primary analysis 20240501.tiff"),
       width = 16,
       height = 20,
       units = "cm")
ggsave(plot = fig2,
       filename = paste0(workdir,"Results/F2 Primary analysis 20240501.tiff"),
       width = 16,
       height = 20,
       units = "cm")

# 4.2. Alcohol consumption figures
alcfq_fig1 <- ggplot(alcfq_res[which((alcfq_res$init=="12" | alcfq_res$init=="14" | alcfq_res$init=="16" | alcfq_res$init=="18" | alcfq_res$init=="20") & !is.na(alcfq_res$age2)),],
                     aes(x=age2, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age2, fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Years since initiation") +
  ylab("Mean number of drinks") + 
  scale_y_continuous(limits=c(0,600), breaks=seq(0, 600, by = 200), expand = c(0, 0)) +
  figure_theme +
  labs(color="Age of initiation",fill="Age of initiation")

alcfq_fig2 <- ggplot(alcfq_res,
                     aes(x=age, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age, fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Age") +
  ylab("Mean number of drinks") + 
  scale_y_continuous(limits=c(0,1000), breaks=seq(0, 1000, by = 200), expand = c(0, 0)) +
  figure_theme +
  labs(color="Age of initiation",fill="Age of initiation")

ggsave(plot = alcfq_fig1,
       filename = paste0(workdir,"Results/SD1 alcfq1 20240501.tiff"),
       width = 16,
       height = 10,
       units = "cm")
ggsave(plot = alcfq_fig2,
       filename = paste0(workdir,"Results/SD2 alcfq2 20240501.tiff"),
       width = 16,
       height = 10,
       units = "cm")

# 4.3. Heavy episodic drinking figures
hed_fig1 <- ggplot(hed_res[which((hed_res$init=="12" | hed_res$init=="14" | hed_res$init=="16" | hed_res$init=="18" | hed_res$init=="20") & !is.na(hed_res$age2)),],
                   aes(x=age2, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age2, fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Years since initiation") +
  ylab("Proportion reported at least monthly HED") + 
  scale_y_continuous(limits=c(0,0.8), breaks=seq(0, 0.8, by = 0.2), expand = c(0, 0)) +
  figure_theme +
  labs(color="Age of initiation",fill="Age of initiation")

hed_fig2 <- ggplot(hed_res,
                   aes(x=age, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age, fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Age") +
  ylab("Proportion reported at least monthly HED") + 
  scale_y_continuous(limits=c(0,0.8), breaks=seq(0, 0.8, by = 0.2), expand = c(0, 0)) +
  figure_theme +
  labs(color="Age of initiation",fill="Age of initiation")

ggsave(plot = hed_fig1,
       filename = paste0(workdir,"Results/SD3 hed1 20240501.tiff"),
       width = 16,
       height = 10,
       units = "cm")
ggsave(plot = hed_fig2,
       filename = paste0(workdir,"Results/SD4 hed2 20240501.tiff"),
       width = 16,
       height = 10,
       units = "cm")

# 4.4. Alcohol-related harms figures
harms_fig1 <- ggplot(harms_res[which((harms_res$init=="12" | harms_res$init=="14" | harms_res$init=="16" | harms_res$init=="18" | harms_res$init=="20") & !is.na(harms_res$age2)),],
                     aes(x=age2, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age2,fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Years since initiation") +
  ylab("Mean number of alcohol-related harms") + 
  scale_y_continuous(limits=c(0,6), breaks=seq(0, 6, by = 1), expand = c(0, 0)) +
  figure_theme +
  labs(color="Age of initiation",fill="Age of initiation")

harms_fig2 <- ggplot(harms_res,
                     aes(x=age, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age,fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Age") +
  ylab("Mean number of alcohol-related harms") + 
  scale_y_continuous(limits=c(0,6), breaks=seq(0, 6, by = 1), expand = c(0, 0)) +
  figure_theme +
  labs(color="Age of initiation",fill="Age of initiation")

ggsave(plot = harms_fig1,
       filename = paste0(workdir,"Results/SD5 harms1 20240501.tiff"),
       width = 16,
       height = 10,
       units = "cm")
ggsave(plot = harms_fig2,
       filename = paste0(workdir,"Results/SD6 harms2 20240501.tiff"),
       width = 16,
       height = 10,
       units = "cm")

# 4.5. DSM-IV Dependence figures
depend_fig1 <- ggplot(depend_res[which((depend_res$init=="12" | depend_res$init=="14" | depend_res$init=="16" | depend_res$init=="18" | depend_res$init=="20") & !is.na(depend_res$age2)),],
                      aes(x=age2, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age2,fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Years since initiation") +
  ylab("Proportion reported DSM-IV Alcohol Dependence") + 
  scale_y_continuous(limits=c(0,0.4), breaks=seq(0, 0.4, by = 0.1), expand = c(0, 0)) +
  figure_theme +
  labs(color="Age of initiation",fill="Age of initiation")

depend_fig2 <- ggplot(depend_res,
                      aes(x=age, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age,fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Age") +
  ylab("Proportion reported DSM-IV Alcohol Dependence") + 
  scale_y_continuous(limits=c(0,0.4), breaks=seq(0, 0.4, by = 0.1), expand = c(0, 0)) +
  figure_theme +
  labs(color="Age of initiation",fill="Age of initiation")

ggsave(plot = depend_fig1,
       filename = paste0(workdir,"Results/SD7 depend1 20240501.tiff"),
       width = 16,
       height = 10,
       units = "cm")
ggsave(plot = depend_fig2,
       filename = paste0(workdir,"Results/SD8 depend2 20240501.tiff"),
       width = 16,
       height = 10,
       units = "cm")

# 4.6. DSM-IV Abuse figures
abuse_fig1 <- ggplot(abuse_res[which((abuse_res$init=="12" | abuse_res$init=="14" | abuse_res$init=="16" | abuse_res$init=="18" | abuse_res$init=="20") & !is.na(abuse_res$age2)),],
                     aes(x=age2, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age2,fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Years since initiation") +
  ylab("Proportion reported DSM-IV Alcohol Abuse") + 
  scale_y_continuous(limits=c(0,0.4), breaks=seq(0, 0.4, by = 0.1), expand = c(0, 0)) +
  figure_theme +
  labs(color="Age of initiation",fill="Age of initiation")

abuse_fig2 <- ggplot(abuse_res,
                     aes(x=age, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age,fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Age") +
  ylab("Proportion reported DSM-IV Alcohol Abuse") + 
  scale_y_continuous(limits=c(0,0.4), breaks=seq(0, 0.4, by = 0.1), expand = c(0, 0)) +
  figure_theme +
  labs(color="Age of initiation",fill="Age of initiation")

ggsave(plot = abuse_fig1,
       filename = paste0(workdir,"Results/SD9 abuse1 20240501.tiff"),
       width = 16,
       height = 10,
       units = "cm")
ggsave(plot = abuse_fig2,
       filename = paste0(workdir,"Results/SD10 abuse2 20240501.tiff"),
       width = 16,
       height = 10,
       units = "cm")

# 4.7. DSM-5 AUD figures
aud_fig1 <- ggplot(aud_res[which((aud_res$init=="12" | aud_res$init=="14" | aud_res$init=="16" | aud_res$init=="18" | aud_res$init=="20") & !is.na(aud_res$age2)),],
                   aes(x=age2, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age2,fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Years since initiation") +
  ylab("Proportion reported DSM-5 AUD") + 
  scale_y_continuous(limits=c(0,0.8), breaks=seq(0, 0.8, by = 0.2), expand = c(0, 0)) +
  figure_theme +
  labs(color="Age of initiation",fill="Age of initiation")

aud_fig2 <- ggplot(aud_res,
                   aes(x=age, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age,fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Age") +
  ylab("Proportion reported DSM-5 AUD") + 
  scale_y_continuous(limits=c(0,0.6), breaks=seq(0, 0.6, by = 0.2), expand = c(0, 0)) +
  figure_theme +
  labs(color="Age of initiation",fill="Age of initiation")

ggsave(plot = aud_fig1,
       filename = paste0(workdir,"Results/SD11 aud1 20240501.tiff"),
       width = 16,
       height = 10,
       units = "cm")
ggsave(plot = aud_fig2,
       filename = paste0(workdir,"Results/SD12 aud2 20240501.tiff"),
       width = 16,
       height = 10,
       units = "cm")

######################################################################################
# 5. Draw sensitivity analysis figures
#-------------------------------------------------------------------------------------

# 5.1. Heavy episodic drinking figures
anyhed_fig1 <- ggplot(anyhed_res[which((anyhed_res$init=="12" | anyhed_res$init=="14" | anyhed_res$init=="16" | anyhed_res$init=="18" | anyhed_res$init=="20") & !is.na(anyhed_res$age2)),],
                      aes(x=age2, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age2, fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Years since initiation") +
  ylab("Proportion reported any HED") + 
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by = 0.2), expand = c(0, 0)) +
  figure_theme +
  labs(color="Age of initiation",fill="Age of initiation")

anyhed_fig2 <- ggplot(anyhed_res,
                      aes(x=age, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age, fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Age") +
  ylab("Proportion reported any HED") + 
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by = 0.2), expand = c(0, 0)) +
  figure_theme +
  labs(color="Age of initiation",fill="Age of initiation")

ggsave(plot = anyhed_fig1,
       filename = paste0(workdir,"Results/SF1 anyhed1 20240501.tiff"),
       width = 16,
       height = 10,
       units = "cm")
ggsave(plot = anyhed_fig2,
       filename = paste0(workdir,"Results/SF2 anyhed2 20240501.tiff"),
       width = 16,
       height = 10,
       units = "cm")

# 4.2. Alcohol-related harms figures
anyharms_fig1 <- ggplot(anyharms_res[which((anyharms_res$init=="12" | anyharms_res$init=="14" | anyharms_res$init=="16" | anyharms_res$init=="18" | anyharms_res$init=="20") & !is.na(anyharms_res$age2)),],
                        aes(x=age2, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age2,fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Years since initiation") +
  ylab("Proportion reported any alcohol-related harms") + 
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by = 0.2), expand = c(0, 0)) +
  figure_theme +
  labs(color="Age of initiation",fill="Age of initiation")

anyharms_fig2 <- ggplot(anyharms_res,
                        aes(x=age, y=estimate, group=init, color=init)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=age,fill=init), alpha=0.2, linetype=0) +
  geom_line() +
  xlab("Age") +
  ylab("Proportion reported any alcohol-related harms") + 
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, by = 0.2), expand = c(0, 0)) +
  figure_theme +
  labs(color="Age of initiation",fill="Age of initiation")

ggsave(plot = anyharms_fig1,
       filename = paste0(workdir,"Results/SF3 anyharms1 20240501.tiff"),
       width = 16,
       height = 10,
       units = "cm")
ggsave(plot = anyharms_fig2,
       filename = paste0(workdir,"Results/SF4 anyharms2 20240501.tiff"),
       width = 16,
       height = 10,
       units = "cm")