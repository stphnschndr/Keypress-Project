#Keypress S18 Analysis 

#MAC
setwd("/Users/stephenschneider/Dropbox/1. Projects/Keypress/Keypress PaID/Keypress-Project/")

#OH5
# setwd("C:\\Users\\sschneider8\\Dropbox\\1. Projects\\Keypress\\Keypress Data Analysis S18\\Keypress S18 Script\\")
source("Keypress Clean for MPSA data.R")

library(stargazer)
library(dplyr)
library(visreg)
library(car)
library(MASS)
attach(data)

# Correlation Matrix --------------------------------------
# Corr_mat1 <- (data.frame(Avg_rep, Avg_dem, rep_pleasant_avg, dem_pleasant_avg,
#                          ideology_sr, partyid, age, female, white, income, relig_att))
# attach(Corr_mat1)
# 
# Corr_mat2 <- (data.frame(rep_know_avg, dem_know_avg, rep_know_pleasant , dem_know_pleasant,
#                          ideology_sr, partyid, age, female, white, income, relig_att))
# rcorr(as.matrix(Corr_mat2))
# 
# Corr_mat3 <- data.frame(App_avg, App_pleasant_avg, App_arousal_avg, Threat_avg, Threat_pleasant_avg, Threat_arousal_avg,
#                         Disgust_avg, Disgust_pleasant_avg, Disgust_arousal_avg, ideology_sr, partyid, age, female, white, income, relig_att)
# 
# Corr_mat4 <- data.frame(rep_know_avg, dem_know_avg, rep_know_pleasant , dem_know_pleasant,
#                         App_avg, App_pleasant_avg, App_arousal_avg, 
#                         Neg_avg, Neg_pleasant_avg, Neg_arousal_avg,
#                         ideology_sr, partyid, age, female, white, income, relig_att)
# 
# Corr_mat5 <- data.frame(App_avg, App_pleasant_avg, App_arousal_avg, 
#                         Neg_avg, Neg_pleasant_avg, Neg_arousal_avg,
#                         ideology_sr, partyid, age, female, white, income, relig_att)



#### Update Correlations for Table 2 ----------------------
rcorr(App_pressAsec, Neg_pressAsec) # r = -.44, p < 0.001
rcorr(Party_pressAsec_ingroup,Party_pressAsec_outgroup) #r= -.53, p < 0.001
rcorr(App_pressAsec, Party_pressAsec_ingroup) # r = 0.47 p < 0.001
rcorr(Party_pressAsec_outgroup, Neg_pressAsec)

rcorr(App_pleasant_avg, App_pressAsec)
rcorr(Neg_pressAsec, Neg_pleasant_avg )
rcorr(Party_pressAsec_ingroup, )

# #Regressions (27March18) ----------------------------


descriptive.data <- data %>%  dplyr::select(App_pressAsec, Neg_pressAsec_ABS,
                                            income, relig_att, female, white, partyid_F, WP_Strength, extraversion_avg, Agreeable_avg, EmoStability_avg, Openness_avg, Conscientious_avg, Bis_avg, Bas_avg
) %>% na.omit()

stargazer(descriptive.data)





lm1 <- lm(App_pressAsec ~ income + relig_att + female + white + partyid_F + WP_Strength + extraversion_avg + Agreeable_avg +
            EmoStability_avg + Openness_avg + Conscientious_avg + Bis_avg + Bas_avg, data = data)
summary(lm1)


lm2 <- lm(Neg_pressAsec_ABS ~ income + relig_att + female + white + partyid_F + WP_Strength + extraversion_avg + Agreeable_avg +
            EmoStability_avg + Openness_avg + Conscientious_avg + Bis_avg + Bas_avg, data = data)
summary(lm2)

#visreg(lm2, "id_cat_sr", ylab = "Negative Generic Images (Presses per Second)", xlab = "Ideology", type = "conditional")

lm3 <- lm(Party_pressAsec_ingroup ~ income + relig_att + female + white + partyid_F + WP_Strength + extraversion_avg + Agreeable_avg +
            EmoStability_avg + Openness_avg + Conscientious_avg + Bis_avg + Bas_avg, data = data)
summary(lm3)

lm4 <- lm(Party_pressAsec_outgroupABS ~ income + relig_att + female + white + partyid_F + WP_Strength + extraversion_avg + Agreeable_avg +
            EmoStability_avg + Openness_avg + Conscientious_avg + Bis_avg + Bas_avg, data = data)
summary(lm4)


stargazer(lm1, lm2, lm3, lm4, title = "Regression Results",
          dep.var.labels = c("Positive", "Negative", "Positive Political", "Negative Political"),
          covariate.labels = c("Income", "Religiosity", "Female", "White", "Republicans", "Ideological Extremity", "Extraversion", "Agreeableness",
                               "Emotional Stability", "Openness", "Conscientiousness", "BIS", "BAS"),
          omit.stat = c("LL", "ser", "f"), type = 'text')
# 
qqPlot(lm1)
qqPlot(lm2)
qqPlot(lm3)
qqPlot(lm4)

sresid <- studres(lm1) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")

sresid <- studres(lm2) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")

sresid <- studres(lm3) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")

sresid <- studres(lm4) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
#no ideology direction by extremity

## Regressions w/ Political Interactions (18April18) ----------------------------

lm1 <- lm(App_pressAsec ~ income + relig_att + female + white + partyid_F*WP_Strength + extraversion_avg + Agreeable_avg +
            EmoStability_avg + Openness_avg + Conscientious_avg + Bis_avg + Bas_avg, data = data)
summary(lm1)


lm2 <- lm(Neg_pressAsec_ABS ~ income + relig_att + female + white + partyid_F*WP_Strength + extraversion_avg + Agreeable_avg +
            EmoStability_avg + Openness_avg + Conscientious_avg + Bis_avg + Bas_avg, data = data)
summary(lm2)

#visreg(lm2, "id_cat_sr", ylab = "Negative Generic Images (Presses per Second)", xlab = "Ideology", type = "conditional")

lm3 <- lm(Party_pressAsec_ingroup ~ income + relig_att + female + white + partyid_F*WP_Strength + extraversion_avg + Agreeable_avg +
            EmoStability_avg + Openness_avg + Conscientious_avg + Bis_avg + Bas_avg, data = data)
summary(lm3)

lm4 <- lm(Party_pressAsec_outgroupABS ~ income + relig_att + female + white + partyid_F*WP_Strength + extraversion_avg + Agreeable_avg +
            EmoStability_avg + Openness_avg + Conscientious_avg + Bis_avg + Bas_avg, data = data)
summary(lm4)

# 
# stargazer(lm1, lm2, lm3, lm4, title = "Regression Results",
#           dep.var.labels = c("Positive", "Negative", "Positive Political", "Negative Political"),
#           covariate.labels = c("Income", "Religiosity", "Female", "White", "Republicans", "Ideological Extremity", "Extraversion", "Agreeableness",
#                                "Emotional Stability", "Openness", "Conscientiousness", "BIS", "BAS", "Interaction"),
#           omit.stat = c("LL", "ser", "f"), type = 'text')
############ NO Interaction #########