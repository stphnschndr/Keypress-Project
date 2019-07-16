#Keypress S18 Clean

library(Hmisc)
library(Rmisc)
library(stargazer)
library(psy)
library(xtable)
library(ggplot2)
library(stats)
library(plyr)
library(pwr)




#MAC
data <- read.csv("/Users/stephenschneider/Dropbox/1. Projects/Keypress/Keypress Data Analysis S18/Keypress S18 Data/Keypress_all_data_merged.csv")
##OH5

# data <- read.csv("/Users/stephenschneider/Dropbox/1. Projects/Keypress/Keypress Data Analysis S18/Keypress S18 Data/Keypress_data_merged_wOmnibus.csv")
##OH5
# data<- read.csv("C:\\Users\\sschneider8\\Dropbox\\1. Projects\\Keypress\\Keypress Data Analysis S18\\Keypress S18 Data\\Keypress_data_merged_wOmnibus.csv")
attach(data)

## Power Analysis 
pwr.f2.test(u = 13, v =114 , f2 = 0.1614402, sig.level = 0.05 , power =)

# 0.825 Power

#How common is double liking per image(Sk vs DK) ============================



# plot(Bernie_sk, Bernie_mx)
# plot(Biden_sk, Biden_mx)
# plot(BillClinton_sk, BillClinton_mx)
# plot(Hillary1_sk, Hillary1_mx)
# plot(Hillary2_sk, Hillary2_mx)
# plot(MichelleObama_sk, MichelleObama_mx)
# plot(Obama1_sk, Obama1_mx)
# plot(Pelosi_sk, Pelosi_mx)
# 
# 
# plot(Cruz_sk, Cruz_mx)
# plot(GWBush_sk, GWBush_mx)
# plot(Pence_sk, Pence_mx)
# plot(Ryan_sk, Ryan_mx)
# plot(Trump_sk, Trump_mx)
# plot(Trump2_sk, Trump2_mx)
# plot(McCain_sk, McCain_mx)
# plot(McConnell_sk, McConnell_mx)
# plot(Melania_sk, Melania_mx)

#Dem & Rep Group Keypress Behav to Political Actor images --------------------------------

##Dems Images - Group

##
#dem_sk <- (data.frame(Bernie_sk, Biden_sk, BillClinton_sk, Hillary1_sk,
#                             Hillary2_sk, MichelleObama_sk, Obama1_sk, Pelosi_sk))
#rcorr(as.matrix(dem_sk))

data$dem_sk <- (rowMeans(data[,c( "Hillary1_sk",
                                  "Hillary2_sk", "MichelleObama_sk", "Obama1_sk", "Pelosi_sk")], na.rm = T))
#summary(data$dem_sk)

#dem_mx <- (data.frame(Bernie_mx, Biden_mx, BillClinton_mx, Hillary1_mx,
#                      Hillary2_mx, MichelleObama_mx, Obama1_mx, Pelosi_mx))
#rcorr(as.matrix(dem_mx))

data$dem_mx <- (rowMeans(data[,c("Bernie_mx", "Biden_mx", "BillClinton_mx", "Hillary1_mx",
                                 "Hillary2_mx", "MichelleObama_mx", "Obama1_mx", "Pelosi_mx")], na.rm = T))
#summary(data$dem_mx)

data$Avg_dem <- data$dem_sk - data$dem_mx
#hist(data$Avg_dem)

##Republicans - Group

#rep_sk <- (data.frame(Cruz_sk, GWBush_sk, Pence_sk, Ryan_sk,
#                      Trump_sk, Trump2_sk, McCain_sk, McConnell_sk, Melania_sk))
#rcorr(as.matrix(rep_sk))

data$rep_sk <- (rowMeans(data[,c("Cruz_sk", "GWBush_sk", "Pence_sk", "Ryan_sk",
                                 "Trump_sk", "Trump2_sk", "McCain_sk", "McConnell_sk", "Melania_sk")]))
#summary(data$rep_sk)

#rep_mx <- (data.frame(Cruz_mx, GWBush_mx, Pence_mx, Ryan_mx,
#                      Trump_mx, Trump2_mx, McCain_mx, McConnell_mx, Melania_mx))
#rcorr(as.matrix(rep_mx))

data$rep_mx <- (rowMeans(data[,c("Cruz_mx", "GWBush_mx", "Pence_mx", "Ryan_mx",
                                 "Trump_mx", "Trump2_mx", "McCain_mx", "McConnell_mx", "Melania_mx")]))
#summary(data$rep_mx)

#Dem Ratings - Group

data$Avg_rep <- data$rep_sk - data$rep_mx

#hist(data$Avg_rep)

###Dems Group Pleasant/Unpleasant

###Need to change Q6 
#table(data$Q6) # no number 7 option
data[c("Q6")][data[c("Q6")]==8] <- 7
data[c("Q6")][data[c("Q6")]==9] <- 8
data[c("Q6")][data[c("Q6")]==10] <- 9
attach(data)

#dem_pleasant<- (data.frame(Q6,Q70, Q74, Q10, Q14, Q38, Q22, Q58))

#dem_pleasant

#rcorr(as.matrix(dem_pleasant))

data$dem_pleasant_avg <- (rowMeans(data[,c("Q6", "Q70", "Q74", "Q10", "Q14", "Q38", "Q22", "Q58")], na.rm =T))

#summary(data$dem_pleasant_avg)

#rcorr(data$dem_pleasant_avg, data$Avg_dem) #0.79, sig

###Rep Group Pleasant/Unpleasant

#table(data$Q26) # no 7 option
data[c("Q26")][data[c("Q26")]==8] <- 7
data[c("Q26")][data[c("Q26")]==9] <- 8
data[c("Q26")][data[c("Q26")]==10] <- 9

#table(data$Q42)
data[c("Q42")][data[c("Q42")]==9] <- 8
data[c("Q42")][data[c("Q42")]==10] <- 9
#attach(data)

#rep_pleasant <- (data.frame(Q46, Q78, Q62, Q54, Q26, Q30, Q82,
#                            Q50, Q42))
#rep_pleasant
#rcorr(as.matrix(rep_pleasant))


data$rep_pleasant_avg <- (rowMeans(data[,c("Q46", "Q78", "Q62", "Q54", "Q26", "Q30", "Q82",
                                           "Q50", "Q42")], na.rm = T))
#summary(data$rep_pleasant_avg)

#rcorr(data$rep_pleasant_avg, data$Avg_rep)

#Know Politicans - selecting the top 6 rep and dems --------------------------
#dem_know <- (data.frame(Q4, Q68, Q72, Q8, Q12, Q36, Q20, Q56))
#colMeans(dem_know) #Q20, Q36, Q12, Q8

data$dem_Know_sk <- (rowMeans(data[,c( "Hillary1_sk","Hillary2_sk",
                                       "MichelleObama_sk", "Obama1_sk" )], na.rm = T))
# summary(data$dem_Know_sk)

data$dem_Know_mx <- (rowMeans(data[,c("Hillary1_mx","Hillary2_mx",
                                      "MichelleObama_mx", "Obama1_mx")], na.rm = T))
#summary(data$dem_Know_mx)

data$dem_know_avg <- data$dem_Know_sk - data$dem_Know_mx
#summary(data$dem_know_avg)

data$dem_know_pleasant <- (rowMeans(data[,c("Q10" ,"Q14", "Q22", "Q38")], na.rm = T))
#summary(data$dem_know_pleasant)

#rep_know <- (data.frame(Q44, Q76, Q60, Q52, Q24, Q28, Q80, Q48, Q40))
#colMeans(rep_know)

data$rep_know_sk <- (rowMeans(data[,c( "GWBush_sk",  "Trump_sk", "Trump2_sk", 
                                       "Melania_sk")], na.rm = T))
#summary(data$rep_know_sk)

data$rep_know_mx <- (rowMeans(data[,c("GWBush_mx",  "Trump_mx", "Trump2_mx", 
                                      "Melania_mx")], na.rm = T))
#summary(data$rep_know_mx)

data$rep_know_avg <- data$rep_know_sk - data$rep_know_mx
summary(data$rep_know_avg)

data$rep_know_pleasant <- (rowMeans(data[,c("Q26", "Q30", "Q78",
                                            "Q42")],na.rm = T))
#summary(data$rep_know_pleasant)



###Appetitive/ Aversion Images ======================================================

#Appetitive

#App_df_pleasant <- na.omit(data.frame(Q147,	Q171,	Q144,	Q150,	Q153,	Q156,	Q159,	Q162,	Q165,	Q168))
#colMeans(App_df_pleasant)

data$App_pleasant_avg <- (rowMeans(data[,c("Q147",	"Q171",	"Q144",	"Q150",	"Q153", "Q156",	"Q159",	"Q162",	"Q165",	"Q168")], na.rm = T))
#summary(data$App_pleasant_avg)

#App_df_arousal <- na.omit(data.frame(Q148,	Q172,	Q145,	Q151,	Q154,	Q157,	Q160,	Q163,	Q166,	Q169))
#colMeans(App_df_arousal)

data$App_arousal_avg <- (rowMeans(data[,c("Q148",	"Q172",	"Q145",	"Q151","Q154","Q157",	"Q160", "Q163",	"Q166",	"Q169")], na.rm = T))
#summary(data$App_arousal_avg)

data$App_avg_sk <- (rowMeans(data[,c("Appetitive1_sk",	"Appetitive10_sk",	"Appetitive2_sk",	"Appetitive3_sk",	"Appetitive4_sk",	"Appetitive5_sk",	
                                     "Appetitive6_sk",	"Appetitive7_sk",	"Appetitive8_sk",	"Appetitive9_sk")], na.rm = T))

data$App_avg_mx <- (rowMeans(data[,c("Appetitive1_mx",	"Appetitive10_mx",	"Appetitive2_mx",	"Appetitive3_mx",	"Appetitive4_mx",	"Appetitive5_mx",	
                                     "Appetitive6_mx",	"Appetitive7_mx",	"Appetitive8_mx",	"Appetitive9_mx")], na.rm = T))

data$App_avg <- data$App_avg_sk - data$App_avg_mx
#summary(data$App_avg)

#disgust 

#Disgust_df_pleasant <- na.omit(data.frame(Q189,	Q192,	Q195,	Q198,	Q201))
#colMeans(Disgust_df_pleasant)

#Disgust_df_arousal <- na.omit(data.frame(Q190,	Q193,	Q196,	Q199,	Q202))
#colMeans(Disgust_df_arousal )

data$Disgust_pleasant_avg <- (rowMeans(data[,c("Q189",	"Q192",	"Q195",	"Q198",	"Q201")], na.rm = T))
#summary(data$Disgust_pleasant_avg)

data$Disgust_arousal_avg <- (rowMeans(data[,c("Q190",	"Q193",	"Q196",	"Q199",	"Q202")], na.rm = T))
#summary(data$Disgust_arousal_avg)

data$Disgust_avg_sk <- (rowMeans(data[,c("Disgust1_sk",	"Disgust2_sk",	"Disgust3_sk",	"Disgust4_sk",	"Disgust5_sk")], na.rm = T)) 
#summary(data$Disgust_avg_sk)

data$Disgust_avg_mx <- (rowMeans(data[,c("Disgust1_mx",	"Disgust2_mx",	"Disgust3_mx",	"Disgust4_mx",	"Disgust5_mx")], na.rm = T)) 
#summary(data$Disgust_avg_mx)

data$Disgust_avg <- data$Disgust_avg_sk - data$Disgust_avg_mx
#summary(data$Disgust_avg)



#Threat
#Threat_df_pleasant <- na.omit(data.frame(Q174,	Q177,	Q180,	Q183,	Q186))
#colMeans(Threat_df_pleasant)
#
#Threat_df_arousal <- na.omit(data.frame(Q175,	Q178,	Q181,	Q184,	Q187))
#colMeans(Threat_df_arousal)

data$Threat_pleasant_avg<- (rowMeans(data[,c("Q174",	"Q177",	"Q180",	"Q183",	"Q186")], na.rm = T))
#summary(data$Threat_pleasant_avg)

data$Threat_arousal_avg <- (rowMeans(data[,c("Q175",	"Q178",	"Q181",	"Q184",	"Q187")], na.rm = T))
#summary(data$Threat_arousal_avg)

data$Threat_avg_sk <- (rowMeans(data[,c("Threat1_sk",	"Threat2_sk",	"Threat3_sk",	"Threat4_sk",	"Threat5_sk")], na.rm = T)) 
#summary(data$Threat_avg_sk)
#hist(data$Threat_avg_sk)

data$Threat_avg_mx <- (rowMeans(data[,c("Threat1_mx",	"Threat2_mx",	"Threat3_mx",	"Threat4_mx",	"Threat5_mx")], na.rm = T)) 
#summary(data$Threat_avg_mx)
#hist(data$Threat_avg_mx)

data$Threat_avg <- data$Threat_avg_sk -data$Threat_avg_mx 
#summary(data$Threat_avg)


#negative images (disgust + threat)

data$Neg_avg_sk <- (rowMeans(data[,c("Threat1_sk",	"Threat2_sk",	"Threat3_sk",	"Threat4_sk",	"Threat5_sk",
                                     "Disgust1_sk",	"Disgust2_sk",	"Disgust3_sk",	"Disgust4_sk",	"Disgust5_sk")], na.rm = T)) 
#summary(data$Neg_avg_sk)

data$Neg_avg_mx <- (rowMeans(data[,c("Threat1_mx",	"Threat2_mx",	"Threat3_mx",	"Threat4_mx",	"Threat5_mx",
                                     "Disgust1_mx",	"Disgust2_mx",	"Disgust3_mx",	"Disgust4_mx",	"Disgust5_mx")], na.rm = T)) 
#summary(data$Neg_avg_mx)

data$Neg_avg <- data$Neg_avg_sk - data$Neg_avg_mx

#summary(data$Neg_avg)


data$Neg_pleasant_avg<- (rowMeans(data[,c("Q174",	"Q177",	"Q180",	"Q183",	"Q186",
                                          "Q189",	"Q192",	"Q195",	"Q198",	"Q201"   )], na.rm = T))
#summary(data$Neg_pleasant_avg)

data$Neg_arousal_avg <- (rowMeans(data[,c("Q175",	"Q178",	"Q181",	"Q184",	"Q187",
                                          "Q190",	"Q193",	"Q196",	"Q199",	"Q202")], na.rm = T))
#summary(data$Neg_arousal_avg)

###Demographics ---------------------------------------------------------------------

#Ideology
###Self-Report (Q270)
#table(data$Q270)
data$ideology_sr<- Q270
#table(data$ideology_sr)


data$id_cat_sr <- NA
data[,c("id_cat_sr")][data[,c("Q270")] > 4] <- 1
data[,c("id_cat_sr")][data[,c("Q270")] < 4] <- -1
data[,c("id_cat_sr")][data[,c("Q270")] == 4] <- 0
#table(data$id_cat_sr)

data$id_cat_f <- NA
data[,c("id_cat_f")][data[,c("Q270")] > 4] <- "cons"
data[,c("id_cat_f")][data[,c("Q270")] < 4] <- "libs"
data[,c("id_cat_f")][data[,c("Q270")] == 4] <- "mod"
table(data$id_cat_f)

#Party ID
#table(data$Q279)
#table(data$Q282)
data$partyid <- data$Q279
data$partyid <- ifelse(data$partyid > 2, data$Q282, data$partyid)
#table(data$partyid)

#Party Id continuous 

data$partyid_cont <- NA
data$partyid_cont <- data$Q280
data[,c("partyid_cont")][data[,c("Q282")] ==1] <-3
data[,c("partyid_cont")][data[,c("Q282")] ==2] <-4
data[,c("partyid_cont")][data[,c("Q281")] ==2] <-5
data[,c("partyid_cont")][data[,c("Q281")] ==1] <-6
# table(data$partyid_cont)

#partyid as factor
data$partyid_F <- factor(data$partyid, labels = c("Democrat", "Republican"))
# table(data$partyid_F)

#Age
summary(data$Q233)
#data$age <- data$Q233

#Sex
#table(data$Q234)
data$female <- data$Q234 - 1
#table(data$female)

#Race
#table(data$Q235)
data$white <- data$Q235
data[,c("white")][data[,c("white")]>1] <- 0
#table(data$white) #19 non-white, 111 whites.

#income
#table(data$Q239)
data$income <- data$Q239

#religious attendance
#table(data$Q237)
data$relig_att <- 5-data$Q237
#table(data$relig_att)

##Partyid Groupishness =======================================================================
#Mean Valence for my party and the other party 

data$Party_pleasant_ingroup <- ifelse(data$partyid_F == "Democrat", data$dem_know_pleasant, ifelse(data$partyid_F == "Republican", data$rep_know_pleasant, NA))

data$Party_pleasant_outgroup <- ifelse(data$partyid_F == "Democrat", data$rep_know_pleasant, ifelse(data$partyid_F == "Republican", data$dem_know_pleasant, NA))

#summary(data$Party_pleasant_ingroup)
#summary(data$Party_pleasant_outgroup)

data$Party_press_ingroup <- ifelse(data$partyid_F == "Democrat", data$dem_know_avg, ifelse(data$partyid_F == "Republican", data$rep_know_avg, NA))

data$Party_press_outgroup <- ifelse(data$partyid_F == "Democrat", data$rep_know_avg, ifelse(data$partyid_F == "Republican", data$dem_know_avg, NA))

#summary(data$Party_press_ingroup)
#summary(data$Party_press_outgroup)

#### Keypress Behavior for Each Political Actor Image##################################################

data$Bernie_keys <- data$Bernie_sk - data$Bernie_mx
#summary(data$Bernie_keys)

data$Biden_keys <- data$Biden_sk - data$Biden_mx
#summary(data$Biden_keys)

data$BillClinton_keys <- data$BillClinton_sk - data$BillClinton_mx 
#summary(data$BillClinton_keys) #billy is not going to be liked by Dems

data$Hillary1_keys <- data$Hillary1_sk - data$Hillary1_mx
#summary(data$Hillary1_keys) #not as liked as others, but more than Bill

data$Hillary2_keys <- data$Hillary2_sk - data$Hillary2_mx
#summary(data$Hillary2_keys) #hill2 less liked than hill1

data$MichelleObama_keys <- data$MichelleObama_sk - data$MichelleObama_mx 
#summary(data$MichelleObama_keys) #really liked

data$Obama1_keys = data$Obama1_sk - data$Obama1_mx
#summary(data$Obama1_keys) #liked but not as much as his wife

data$Pelosi_keys <- data$Pelosi_sk - data$Pelosi_mx
#summary(data$Pelosi_keys)

attach(data)

Dem_ind_keys<- na.omit(data.frame(Bernie_keys, Biden_keys, BillClinton_keys, Hillary1_keys,
                                  Hillary2_keys, MichelleObama_keys,Obama1_keys, Pelosi_keys, partyid_F ))
Dem_ind_keys_d <- subset(Dem_ind_keys, partyid_F == "Democrat") 
Dem_ind_keys_r <- subset(Dem_ind_keys, partyid_F == "Republican") 



Dem_ind_keys$Bernie_keys_AbsDif<- abs((mean(Dem_ind_keys_r$Bernie_keys)) -(mean(Dem_ind_keys_d$Bernie_keys)))
#summary(Dem_ind_keys$Bernie_keys_AbsDif)

Dem_ind_keys$Bernie_know <- mean(2-data$Q4)
#summary(Dem_ind_keys$Bernie_know)

Dem_ind_keys$Biden_keys_AbsDif<- abs(mean(Dem_ind_keys_r$Biden_keys) - (mean(Dem_ind_keys_d$Biden_keys)))
#summary(Dem_ind_keys$Biden_keys_AbsDif)

Dem_ind_keys$Biden_know <- mean(2-data$Q68)
#summary(Dem_ind_keys$Biden_know)

Dem_ind_keys$BillClinton_keys_AbsDif<- abs(mean(Dem_ind_keys_r$BillClinton_keys) - (mean(Dem_ind_keys_d$BillClinton_keys)))
#summary(Dem_ind_keys$BillClinton_keys_AbsDif)

Dem_ind_keys$BillClinton_know <- mean(2-data$Q72)
#summary(Dem_ind_keys$BillClinton_know)

Dem_ind_keys$Hillary1_keys_AbsDif<- abs((mean(Dem_ind_keys_r$Hillary1_keys)) - (mean(Dem_ind_keys_d$Hillary1_keys)))
#summary(Dem_ind_keys$Hillary1_keys_AbsDif)

Dem_ind_keys$Hillary1_know <- mean(2-data$Q8)
#summary(Dem_ind_keys$Hillary1_know)

Dem_ind_keys$Hillary2_keys_AbsDif<- abs((mean(Dem_ind_keys_r$Hillary2_keys)) - (mean(Dem_ind_keys_d$Hillary2_keys)))
#summary(Dem_ind_keys$Hillary2_keys_AbsDif)

Dem_ind_keys$Hillary2_know <- mean(2-data$Q12)
#summary(Dem_ind_keys$Hillary2_know)

#Michelle is positive/positive = not sure why we would need to cacualate differently
Dem_ind_keys$MichelleObama_keys_AbsDif<- abs((mean(Dem_ind_keys_r$MichelleObama_keys)) - (mean(Dem_ind_keys_d$MichelleObama_keys)))
#summary(Dem_ind_keys$MichelleObama_keys_AbsDif)

Dem_ind_keys$MichelleObama_know <- mean(2-data$Q36)
#summary(Dem_ind_keys$Hillary2_know)

Dem_ind_keys$Obama1_keys_AbsDif<- abs((mean(Dem_ind_keys_r$Obama1_keys)) - (mean(Dem_ind_keys_d$Obama1_keys)))
#summary(Dem_ind_keys$Obama1_keys_AbsDif)


Dem_ind_keys$Obama1_know <- mean(2-data$Q20)
#summary(Dem_ind_keys$Obama1_know) #everyone knows Obama

Dem_ind_keys$Pelosi_keys_AbsDif<- abs((mean(Dem_ind_keys_r$Pelosi_keys)) - (mean(Dem_ind_keys_d$Pelosi_keys)))
#summary(Dem_ind_keys$Pelosi_keys_AbsDif)

Dem_ind_keys$Pelosi_know <- mean(2-data$Q56)
#summary(Dem_ind_keys$Pelosi_know) #very low

attach(Dem_ind_keys)


#Republicans
data$Cruz_keys <-data$Cruz_sk - data$Cruz_mx 
#summary(data$Cruz_keys)

data$GWBush_keys <- data$GWBush_sk - data$GWBush_mx
#summary(data$GWBush_keys)

data$Pence_keys <- data$Pence_sk - data$Pence_mx
#summary(data$Pence_keys)

data$Ryan_keys <- data$Ryan_sk - data$Ryan_mx
#summary(data$Ryan_keys) #abt as popular as bush

data$Trump_keys <- data$Trump_sk - data$Trump_mx
#summary(data$Trump_keys) #lowest

data$Trump2_keys <- data$Trump2_sk - data$Trump2_mx
#summary(data$Trump2_keys)

data$McCain_keys <- data$McCain_sk - data$McCain_mx
#summary(data$McCain_keys)

data$McConnell_keys <- data$McConnell_sk - data$McConnell_mx
#summary(data$McConnell_keys) #3rd least popular

data$Melania_keys <- data$Melania_sk - data$Melania_mx
#summary(data$Melania_keys) #negative

attach(data)
Rep_ind_keys <- na.omit(data.frame(Cruz_keys, GWBush_keys, Pence_keys,
                                   Ryan_keys, Trump_keys,  Trump2_keys,
                                   McCain_keys, McConnell_keys, Melania_keys,
                                   partyid_F))
Rep_ind_keys_d <- subset(Rep_ind_keys, partyid_F == "Democrat")
Rep_ind_keys_r <- subset(Rep_ind_keys, partyid_F == "Republican")


attach(Rep_ind_keys)

Rep_ind_keys$Cruz_keys_AbsDif<- abs((mean(Rep_ind_keys_r$Cruz_keys)) - (mean(Rep_ind_keys_d$Cruz_keys)))
#summary(Rep_ind_keys$Cruz_keys_AbsDif)

Rep_ind_keys$Cruz_know <- mean(2-data$Q44)
#summary(Rep_ind_keys$Cruz_know)

Rep_ind_keys$GWBush_keys_AbsDif<- abs((mean(Rep_ind_keys_r$GWBush_keys)) - (mean(Rep_ind_keys_d$GWBush_keys)))
#summary(Rep_ind_keys$GWBush_keys_AbsDif)

Rep_ind_keys$GWBush_know <- mean(2-data$Q76)
#summary(Rep_ind_keys$GWBush_know)

Rep_ind_keys$Pence_keys_AbsDif<- abs((mean(Rep_ind_keys_r$Pence_keys)) - (mean(Rep_ind_keys_d$Pence_keys)))
#summary(Rep_ind_keys$Pence_keys_AbsDif)

Rep_ind_keys$Pence_know <- mean(2-data$Q60)
#summary(Rep_ind_keys$Pence_know)

Rep_ind_keys$Ryan_keys_AbsDif<- abs((mean(Rep_ind_keys_r$Ryan_keys)) - (mean(Rep_ind_keys_d$Ryan_keys)))
#summary(Rep_ind_keys$Ryan_keys_AbsDif)

Rep_ind_keys$Ryan_know <- mean(2-data$Q52)
#summary(Rep_ind_keys$Ryan_know)

Rep_ind_keys$Trump_keys_AbsDif<- abs((mean(Rep_ind_keys_r$Trump_keys)) - (mean(Rep_ind_keys_d$Trump_keys)))
#summary(Rep_ind_keys$Trump_keys_AbsDif)

Rep_ind_keys$Trump_know <- mean(2-data$Q24)
#summary(Rep_ind_keys$Trump_know)

Rep_ind_keys$Trump2_keys_AbsDif<- abs((mean(Rep_ind_keys_r$Trump2_keys)) - (mean(Rep_ind_keys_d$Trump2_keys)))
#summary(Rep_ind_keys$Trump2_keys_AbsDif)

Rep_ind_keys$Trump2_know <- mean(2-data$Q28)
#summary(Rep_ind_keys$Trump2_know)

Rep_ind_keys$McCain_keys_AbsDif<- abs((mean(Rep_ind_keys_r$McCain_keys)) - (mean(Rep_ind_keys_d$McCain_keys)))
#summary(Rep_ind_keys$McCain_keys_AbsDif)

Rep_ind_keys$McCain_know <- mean(2-data$Q80)
#summary(Rep_ind_keys$McCain_know)

Rep_ind_keys$McConnell_keys_AbsDif<- abs((mean(Rep_ind_keys_r$McConnell_keys)) - (mean(Rep_ind_keys_d$McConnell_keys)))
#summary(Rep_ind_keys$McConnell_keys_AbsDif)

Rep_ind_keys$McConnell_know <- mean(2-data$Q48)
#summary(Rep_ind_keys$McConnell_know)

Rep_ind_keys$Melania_keys_AbsDif<- abs((mean(Rep_ind_keys_r$Melania_keys)) - (mean(Rep_ind_keys_d$Melania_keys)))
#summary(Rep_ind_keys$Melania_keys_AbsDif)

Rep_ind_keys$Melania_know <- mean(2-data$Q40)
#summary(Rep_ind_keys$Melania_know)

attach(Rep_ind_keys)

Rep_ind_corr <- na.omit(data.frame(Cruz_keys_AbsDif, Cruz_know, 
                                   GWBush_keys_AbsDif, GWBush_know,
                                   Pence_keys_AbsDif, Pence_know,
                                   Ryan_keys_AbsDif, Ryan_know,
                                   Trump_keys_AbsDif, Trump_know,
                                   Trump2_keys_AbsDif, Trump2_know,
                                   McCain_keys_AbsDif, McCain_know,
                                   McConnell_keys_AbsDif, McConnell_know,
                                   Melania_keys_AbsDif, Melania_know ))


attach(Rep_ind_corr)



Actors_r= c("Cruz", "Bush", "Pence", "Ryan", "Trump", "Tump2", "Mccain", "Mcconnell", "Melania")
Abs_diff_r = c(mean(Cruz_keys_AbsDif), mean(GWBush_keys_AbsDif), mean(Pence_keys_AbsDif),mean(Ryan_keys_AbsDif), 
               mean(Trump_keys_AbsDif), mean(Trump2_keys_AbsDif), mean(McCain_keys_AbsDif), mean(McConnell_keys_AbsDif), mean(Melania_keys_AbsDif))
Recognize_r = c(mean(Cruz_know), mean(GWBush_know), mean(Pence_know), mean(Ryan_know), mean(Trump_know), mean(Trump2_know), mean(McCain_know),
                mean(McConnell_know), mean(Melania_know))
df2r<- data.frame(Actors_r, Abs_diff_r, Recognize_r)

names(df2r) <- c("Actors","Abs_dif", "Recognize")

#rm(df2d)

Actors_d = c("Bernie", "Biden", "BillClinton", 
             "Hillary1", "Hillary2", "MichelleObama",
             "Obama1", "Pelosi")

Abs_diff_d = c(mean(Bernie_keys_AbsDif), mean(Biden_keys_AbsDif), mean(BillClinton_keys_AbsDif),
               mean(Hillary1_keys_AbsDif), mean(Hillary2_keys_AbsDif), mean(MichelleObama_keys_AbsDif),
               mean(Obama1_keys_AbsDif), mean(Pelosi_keys_AbsDif))

Recongize_d = c(mean(Bernie_know), mean(Biden_know), mean(BillClinton_know),
                mean(Hillary1_know), mean(Hillary2_know), mean(MichelleObama_know),
                mean(Obama1_know), mean(Pelosi_know))

df2d<- data.frame(Actors_d, Abs_diff_d, Recongize_d)

names(df2d) <- c("Actors","Abs_dif", "Recognize")

df1<-rbind(df2r, df2d)

#file.choose()

#write.csv(df1, "C:\\Users\\sschneider8\\Dropbox\\1. Projects\\Keypress\\Keypress Data Analysis S18\\Keypress S18 Script\\Recongizability.csv")

#rcorr(df1$Abs_dif, df1$Recognize)

### Motivation to Express Prejudice ------------------------------------

##Mep  Prejudice Towards Repub.
table(data$Q295)
data$Q295 <- 10-data$Q295
#attach(data)

#dataMEP_rep <- na.omit(data.frame(Q284, Q285, Q286, Q287, Q288, Q289, Q290,
#                                  Q291, Q292, Q293, Q294, Q295))
#rcorr(as.matrix(dataMEP_rep))

#MEP Prejudce Towards Dems

table(data$Q308)
data$Q308 <- 10 - data$Q308 
#attach(data)

#dataMEP_dem<- na.omit(data.frame(Q297, Q298, Q299, Q300, Q301, Q302, Q303,
#                                Q304, Q305, Q306, Q307, Q308))
#rcorr(as.matrix(dataMEP_dem))

data$MEP_avg <- rowMeans(data[,c("Q284", "Q285", "Q286", "Q287", "Q288", "Q289", "Q290",
                                 "Q291", "Q292", "Q293", "Q294", "Q295",
                                 "Q297", "Q298", "Q299", "Q300", "Q301", "Q302", "Q303",
                                 "Q304", "Q305", "Q306", "Q307", "Q308")], na.rm = T)
#summary(data$MEP_avg)
#sd(na.omit(data$MEP_avg))
#mean = 2.812, sd = 1.352

### Wilson Patterson Ideology (direction & Strength)============================
data$Q275_2 <- 8-data$Q275_2
data$Q275_3 <- 8-data$Q275_3
data$Q275_4 <- 8-data$Q275_4
data$Q275_5 <- 8-data$Q275_5
data$Q275_9 <- 8-data$Q275_9
data$Q275_10 <- 8-data$Q275_10
data$Q275_11 <- 8-data$Q275_11
data$Q275_12 <- 8-data$Q275_12
data$Q275_14 <- 8-data$Q275_14
data$Q275_16 <- 8-data$Q275_16
data$Q275_19 <- 8-data$Q275_19
data$Q275_21 <- 8-data$Q275_21
data$Q275_22 <- 8-data$Q275_22
data$Q275_24 <- 8-data$Q275_24

#attach(data)

# datWP_corr <- na.omit(data.frame(Q275_1, Q275_2, Q275_3, Q275_4, Q275_5,
# Q275_6, Q275_7, Q275_8, Q275_9, Q275_10,
# Q275_11, Q275_12, Q275_13, Q275_14, Q275_15,
# Q275_16, Q275_17, Q275_18, Q275_19, Q275_20,
# Q275_21, Q275_22, Q275_23, Q275_24,  Q275_25))
#rcorr(as.matrix(datWP_corr))

data$WP_avg <- rowMeans(data[,c("Q275_1", "Q275_2", "Q275_3", "Q275_4", "Q275_5",
                                "Q275_6", "Q275_7", "Q275_8", "Q275_9", "Q275_10",
                                "Q275_11", "Q275_12", "Q275_13","Q275_14", "Q275_15",
                                "Q275_16", "Q275_17", "Q275_18","Q275_19","Q275_20",
                                "Q275_21", "Q275_22", "Q275_23", "Q275_24",  "Q275_25")], na.rm = TRUE)
#summary(data$WP_avg)
#sd(data$WP_avg)
#cronbach(datWP_corr)
#Mean = 3.486, s = 1.036, alpha = 0.922

##WP ideology Strenght
#table(data$WP_avg)
data$WP_Strength <- abs(data$WP_avg - 4)
#table(data$WP_Strength)



### Keypresses Per Second ====================
summary(data$Party_press_ingroup)
summary(data$Party_press_outgroup)

data$Party_press_ingroup_sec1 <- ifelse(data$Party_press_ingroup > 0, data$Party_press_ingroup*.05, ifelse(data$Party_press_ingroup < 0, data$Party_press_ingroup*.075, 0))
#summary(data$Party_press_ingroup_sec1)
data$Party_press_ingroup_sec2 <- 8 + data$Party_press_ingroup_sec1
#summary(data$Party_press_ingroup_sec2)

data$Party_pressAsec_ingroup <- (abs(data$Party_press_ingroup)/data$Party_press_ingroup_sec2)
##summary(data$Party_pressAsec_ingroup)
#hist(data$Party_pressAsec_ingroup)

data$Party_press_outgroup_sec1 <- ifelse(data$Party_press_outgroup > 0, data$Party_press_outgroup*.05, ifelse(data$Party_press_outgroup < 0, data$Party_press_outgroup*.075, 0))
##summary(data$Party_press_outgroup_sec1)
data$Party_press_outgroup_sec2 <- 8+ data$Party_press_outgroup_sec1
##summary(data$Party_press_outgroup_sec2)

data$Party_pressAsec_outgroup <- (abs(data$Party_press_outgroup)/data$Party_press_outgroup_sec2)
data$Party_pressAsec_outgroup <- data$Party_pressAsec_outgroup*-1
##summary(data$Party_pressAsec_outgroup)
#hist(data$Party_pressAsec_outgroup)

##summary(data$App_avg)
data$App_avg_sec1 <- ifelse(data$App_avg > 0, data$App_avg*.05, ifelse(data$App_avg < 0, data$App_avg*.075, 0))
##summary(data$App_avg_sec1)
data$App_avg_sec2 <- 8+ data$App_avg_sec1
##summary(data$App_avg_sec2)

data$App_pressAsec <- (abs(data$App_avg)/data$App_avg_sec2)
#summary(data$App_pressAsec)
#hist(data$App_pressAsec)

##summary(data$Neg_avg)
data$Neg_avg_sec1 <- ifelse(data$Neg_avg > 0, data$Neg_avg*.05, ifelse(data$Neg_avg < 0, data$Neg_avg*.075, 0))
##summary(data$Neg_avg_sec1)
data$Neg_avg_sec2 <- 8+ data$Neg_avg_sec1
##summary(data$Neg_avg_sec2)

data$Neg_pressAsec <- (abs(data$Neg_avg)/data$Neg_avg_sec2)
data$Neg_pressAsec <- data$Neg_pressAsec*-1
##summary(data$Neg_pressAsec)
#hist(data$Neg_pressAsec)

### Keypresses Per Seocnd - Know Politicans - selecting the top 4 rep and dems --------------------------

data$dem_know_avg <- data$dem_Know_sk - data$dem_Know_mx
#summary(data$dem_know_avg)

data$dem_know_avg_sec1 <- ifelse(data$dem_know_avg > 0, data$dem_know_avg*.05, ifelse(data$dem_know_avg < 0, data$dem_know_avg*.075, 0))
#summary(data$dem_know_avg_sec1)
data$dem_know_avg_sec2 <- 8+ data$dem_know_avg_sec1
#summary(data$dem_know_avg_sec2)

data$dem_know_avg_pressAsec <- (data$dem_know_avg /data$dem_know_avg_sec2)
#summary(data$dem_know_avg_pressAsec)


#summary(data$rep_know_avg)
#summary(data$rep_know_avg)

data$rep_know_avg_sec1 <- ifelse(data$rep_know_avg > 0, data$rep_know_avg*.05, ifelse(data$rep_know_avg < 0, data$rep_know_avg*.075, 0))
#summary(data$rep_know_avg_sec1)
data$rep_know_avg_sec2 <- 8+ data$rep_know_avg_sec1
#summary(data$rep_know_avg_sec2)

data$rep_know_avg_pressAsec <- (data$rep_know_avg /data$rep_know_avg_sec2)
#summary(data$rep_know_avg_pressAsec)

### DV: Keypress per second for regresssion (Abs) ----------------------------

#summary(data$App_pressAsec)
#summary(data$Neg_pressAsec)
#summary(data$Party_pressAsec_ingroup)
#summary(data$Party_pressAsec_outgroup)

data$Neg_pressAsec_ABS <-abs(data$Neg_pressAsec)
#summary(data$Neg_pressAsec_ABS)

data$Party_pressAsec_outgroupABS <- abs(data$Party_pressAsec_outgroup)
#summary(data$Party_pressAsec_outgroupABS) 

data$Party_pressAsec_diff_NP <- data$Party_pressAsec_outgroupABS - data$Party_pressAsec_ingroup
summary(data$Party_pressAsec_diff_NP)

### Recoding TIPI ----------------------------------------------------------------------------


#rcorr(data$Q242_1, data$Q242_6)
data$Q242_6 <- 8-data$Q242_6
data$extraversion_avg <- rowMeans(data[,c("Q242_1", "Q242_6")], na.rm = T)
#table(data$extraversion_avg)

data$Q242_2 <- 8 - data$Q242_2
#library(Hmisc)
#rcorr(data$Q242_2, data$Q242_7)
data$Agreeable_avg <- rowMeans(data[,c("Q242_7", "Q242_2")], na.rm = T)
#table(data$Agreeable_avg)

data$Q242_4 <- 8-data$Q242_4
#rcorr(data$Q242_4, data$Q242_9)
data$EmoStability_avg <- rowMeans(data[,c("Q242_9", "Q242_4")], na.rm = T)
#table(data$EmoStability_avg)

data$Q242_10 <- 8- data$Q242_10
#rcorr(data$Q242_10, data$Q242_5)
data$Openness_avg <- rowMeans(data[,c("Q242_10", "Q242_5")], na.rm = T)
#table(data$Openness_avg)

data$Q242_8 <- 8- data$Q242_8
#rcorr(data$Q242_8, data$Q242_3)
data$Conscientious_avg <- rowMeans(data[,c("Q242_8", "Q242_3")], na.rm = T)
#table(data$Conscientious_avg)

### Recode Bis/Bas ---------------------------

#table(data$Q245)
data$Q245 <- 8- data$Q245

#table(data$Q265) 
data$Q265 <- 8- data$Q265

#attach(data)

#datBis <- na.omit(data.frame(Q245, Q251, Q256, Q259, Q265, Q267))
#rcorr(as.matrix(datBis))

#datBAS <- na.omit(data.frame(Q246, Q247, Q248, Q250, Q252, Q253, Q255, Q257, Q258, Q261, Q263, Q264, Q266))
#rcorr(as.matrix(datBAS))

data$Bis_avg <- rowMeans(data[,c("Q245", "Q251", "Q256","Q259", "Q265", "Q267")], na.rm = T)
#summary(data$Bis_avg)

data$Bas_avg <- rowMeans(data[,c("Q246", "Q247", "Q248", "Q250", "Q252", "Q253", "Q255", "Q257", "Q258",
                                 "Q261", "Q263", "Q264", "Q266")], na.rm = T)
#summary(data$Bas_avg)

#rcorr(data$Bas_avg, data$Bis_avg)

### Participation ------------------



data$datPat <- rowMeans(data[,c("pmu1_1", "pmu1_2","pmu1_3","pmu1_4","pmu1_5","pmu1_6","pmu1_7","pmu1_9","pmu1_10",
                                "pmu1_11","pmu1_12","pmu1_13")], na.rm = T)
#table(data$datPat)
data$PolPat_avg <- 5- data$datPat
#table(data$PolPat_avg)

### Vote / Support --------

#Trump
table(data$Q277)
# 54 clinton, 36 Trump
data$Trump_2016 <- NA
data[,c("Trump_2016")][data[,c("Q277")]==1] <- 1
data[,c("Trump_2016")][data[,c("Q277")]!=1] <- 0
table(data$Trump_2016)


data$Clinton_2016 <- NA
data[,c("Clinton_2016")][data[,c("Q277")]==2] <- 1
data[,c("Clinton_2016")][data[,c("Q277")]!=2] <- 0
table(data$Clinton_2016)

##Do keypresses for trump and clinton predict vote choice? -----
table(data$vote_2016)

data$Trump_vote <- ifelse(data$vote_2016 == "Trump", 1, 0)
table(data$Trump_vote)

data$Clinton_vote <- ifelse(data$vote_2016 == "Clinton", 1, 0)
table(data$Clinton_vote)

data$Trump_pleas <- data$Q26
table(data$Trump_pleas)

data$Clinton_pleas <- data$Q10
table(data$Clinton_pleas)

rcorr(data$Clinton_pleas, data$Trump_pleas) # r = -0.42

#standardize keypress activity for Trump and Clinton images to presses pre second ------

table(data$Hillary1_sk)
data$Hillary_Key_pos <- (data$Hillary1_sk*0.05 +8)
table(data$Hillary_Key_pos)
data$Hillary_Key_pos <-(abs(data$Hillary1_sk)/(data$Hillary_Key_pos))
summary(data$Hillary_Key_pos)



table(data$Hillary1_mx)
data$Hillary_Key_neg <- (data$Hillary1_mx*0.075 +8)
table(data$Hillary_Key_neg)
data$Hillary_Key_neg <-(abs(data$Hillary1_mx)/(data$Hillary_Key_neg))
summary(data$Hillary_Key_neg)


data$Trump_sk
table(data$Trump_sk)
data$Trump_Key_pos <- (data$Trump_sk*0.05 +8)
table(data$Trump_Key_pos)
data$Trump_Key_pos <-(abs(data$Trump_sk)/(data$Trump_Key_pos))
summary(data$Trump_Key_pos)

table(data$Trump_mx)
data$Trump_Key_neg <- (data$Trump_mx*0.075 +8)
table(data$Trump_Key_neg)
data$Trump_Key_neg <-(abs(data$Trump_mx)/(data$Trump_Key_neg))
summary(data$Trump_Key_neg)


