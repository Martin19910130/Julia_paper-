###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###             IPM - Climate change and land management 
###                   interactively influence the 
###                   popilation dynamics of 
###                   Bromus erectus (Poaceae)
###                   
###                   Models + Plots (no bootstrap)
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
gc()

### prepare pallets for plots
cbPalette <- c("#0072B2", "#D55E00", "#56B4E9", "#E69F00")
rbPalette <- c("#0072B2", "#D55E00")

library(ggplot2)

### Read Data
setwd("C://Users/ma22buky/Documents/Julia_Paper/Done/")
demo_dat <- read.csv("Bro_Demography.csv")

### subset into the 4 different treatments
mow_amb <- subset(demo_dat, climate == "ambient" & management == "mowing")
gra_amb <- subset(demo_dat, climate == "ambient" & management == "grazing") 
mow_fut <- subset(demo_dat, climate == "future" & management == "mowing")
gra_fut <- subset(demo_dat, climate == "future" & management == "grazing")

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###               Survival
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### survival ~ logsizet0
##  Survival - mowing ambient
mow_amb_sur_mod <- glm(mow_amb$survival ~ mow_amb$logsizet0, family = "binomial")
mow_amb_slope <- coef(mow_amb_sur_mod)[2]
mow_amb_inter <- coef(mow_amb_sur_mod)[1]

## Survival - grazing ambient
gra_amb_sur_mod <- glm(gra_amb$survival ~ gra_amb$logsizet0, family = "binomial")
gra_amb_slope <- coef(gra_amb_sur_mod)[2]
gra_amp_inter <- coef(gra_amb_sur_mod)[1]

## Survival - mowing future
mow_fut_sur_mod <- glm(mow_fut$survival ~ mow_fut$logsizet0, family = "binomial")
mow_fut_slope <- coef(mow_fut_sur_mod)[2]
mow_fut_inter <- coef(mow_fut_sur_mod)[1]

## Survival - grazing future
gra_fut_sur_mod <- glm(gra_fut$survival ~ gra_fut$logsizet0, family = "binomial")
gra_fut_slope <- coef(gra_fut_sur_mod)[2]
gra_fut_inter <- coef(gra_fut_sur_mod)[1]

## Plot Survival
ggplot(demo_dat, aes(x = logsizet0, y = jitter(survival), color = climate, shape = management)) + 
  geom_point() + theme_classic() + scale_colour_manual(values=rbPalette) + 
  scale_shape_manual(values=c(16, 17)) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F, 
              mapping = aes(x = logsizet0, y = survival, linetype = treatment)) +
  ylab("Plant survival") + xlab("") + theme(legend.position = "none") + 
  ggtitle("a) Plant survival") + scale_y_continuous(breaks=c(0, 1)) +
  scale_x_continuous(breaks = c(-1.38629 ,0 , 1.6094, 2.70805, 3.9120, 5.0106), 
                     labels = c(0.25, 1, 5, 15, 50, 150))

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###           Growth
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### logsizet1 ~ logsizet0
## Growth - mowing ambient
mow_amb_gro_mod <- lm(mow_amb$logsizet1 ~ mow_amb$logsizet0)

## Growth - grazing ambient
gra_amb_gro_mod <- lm(gra_amb$logsizet1 ~ gra_amb$logsizet0)

## Growth - mowing future
mow_fut_gro_mod <- lm(mow_fut$logsizet1 ~ mow_fut$logsizet0)

## Growth - grazing future
gra_fut_gro_mod <- lm(gra_fut$logsizet1 ~ gra_fut$logsizet0)

## Plot Growth
ggplot(demo_dat, aes(x = logsizet0, y = logsizet1, color = climate, shape = management)) + geom_point() +
  geom_smooth(method = "lm", se = F, mapping = aes(x = log))

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###           Flower probability
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Flower probability - mowing ambient
mow_amb_fl_pr_mod <- glm(mow_amb$flower ~ mow_amb$logsizet0, family = "binomial")

## Flower probability - grazing ambient
gra_amb_fl_pr_mod <- glm(gra_amb$flower ~ gra_amb$logsizet0, family = "binomial")

## Flower probability - mowing future
mow_fut_fl_pr_mod <- glm(mow_fut$flower ~ mow_fut$logsizet0, family = "binomial")

## Flower probability - grazing future
gra_fut_fl_pr_mod <- glm(mow_fut$flower ~ mow_fut$logsizet0, family = "binomial")

## Plot Flower probability
ggplot(demo_dat, aes(x = logsizet0, y = jitter(flower, .5), color = climate, shape = management)) +
  geom_point() + geom_smooth(method = "glm" , method.args = list(family = "binomial"), aes(y = flower), se = F)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###           Seeds per flowering plant.
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mow_amb_seed_plant_mod <- lm(mow_amb$seeds_per_ind ~ mow_amb$logsizet0)