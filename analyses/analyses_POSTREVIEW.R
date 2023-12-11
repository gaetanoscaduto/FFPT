########## ANALYSES, DEF POST REVIEW 

library(rio)
library(ggplot2)
library(margins)
library(dplyr)
library(corrplot)
library(texreg)
library(patchwork)
library(MASS)
library(gtools)
library(gt)
library(gtsummary)
library(openxlsx)
library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)
library(lmtest)
library(ordinal)
library(effects)

main_path = "C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/Papers and Chapters/FFPT"
setwd(main_path)
data = import(paste0(main_path,"/data_manipulation/FFPT_tidy.RDS"))

#i eliminate the few data that creates missing values in the model

datanomiss = data[!is.na(data$sex_r) & !is.na(data$citysize_r2) & !is.na(data$macroarea2), ]



###########################################################
################## H1a H1b, H1c, H1d #######################

############# CHI SQUARED

chisq = chisq.test(table(data$expideology_r6, data$group))

chisq$p.value
#Building a dataframe for residuals for each cell
dfaus = as.data.frame(chisq$residuals)

#rounding residuals for readability

dfaus$Freq = round(dfaus$Freq, 2)

#plotting residuals with heatmap

contrib <- 100*chisq$residuals^2/chisq$statistic

dfaus1 = as.data.frame(contrib)
dfaus$Freqperc = round(dfaus1$Freq, 2)

heatmap1 =ggplot(dfaus, aes(Var1, Var2))+
  geom_raster(aes(fill = Freq), show.legend = F)+
  xlab("Perceived Ideology")+
  ylab("Experimental group")+
  geom_text(aes(label = paste0(Freq, "\n(", Freqperc, "%)")), color = "white", size = 4) +
  scale_fill_continuous(name= "Pearson's \nresiduals")
#labs(title = "Pearson residuals for the Chi-square test\n of imputed ideology of the vignette (recoding 3)\n vs experimental group ")

heatmap1 

ggsave("Heatmap pearsons residuals.png", path = paste0(main_path,"/plots and tables/plots"), scale = 3)

# doing the same as before but with percentage contribution to chi squared
#statistics


heatmap2 = ggplot(dfaus1, aes(Var1, Var2))+
  geom_raster(aes(fill = Freq), show.legend = F)+
  geom_text(aes(label = paste0(Freq, "%")), color = "white", size = 4) +
  xlab("Perceived Ideology")+
  ylab("Experimental group")+
  scale_fill_continuous(name= "Percentage \ncontribution")

heatmap2

ggsave("Heatmap percentage contribution.png", path = paste0(main_path,"/plots and tables/plots"), scale = 3)

heatmap1+heatmap2

ggsave("Heatmap full.png", path = paste0(main_path,"/plots and tables/plots"), scale = 2)


####### for appendix: no grouping in ideology
############################################################

chisq = chisq.test(table(data$expideology_r7, data$group))

dfaus = as.data.frame(chisq$residuals)


dfaus$Freq = round(dfaus$Freq, 2)

contrib <- 100*chisq$residuals^2/chisq$statistic

dfaus1 = as.data.frame(contrib)
dfaus$Freqperc = round(dfaus1$Freq, 2)

heatmap1 =ggplot(dfaus, aes(Var1, Var2))+
  geom_raster(aes(fill = Freq), show.legend = F)+
  xlab("Perceived Ideology")+
  ylab("Experimental group")+
  geom_text(aes(label = paste0(Freq, "\n(", Freqperc, "%)")), color = "white", size = 4) +
  scale_fill_continuous(name= "Pearson's \nresiduals")
#labs(title = "Pearson residuals for the Chi-square test\n of imputed ideology of the vignette (recoding 3)\n vs experimental group ")
heatmap1
ggsave("Heatmap pearsons residuals no grouping.png", path = paste0(main_path,"/plots and tables/plots"), scale = 5)


##For appendix: H1a, H1b, H1c and H1d with partisanship


chisq = chisq.test(table(data$expparty_r0, data$group))

dfaus = as.data.frame(chisq$residuals)

dfaus$Freq = round(dfaus$Freq, 2)

contrib <- 100*chisq$residuals^2/chisq$statistic

dfaus1 = as.data.frame(contrib)
dfaus$Freqperc = round(dfaus1$Freq, 2)

heatmap1 =ggplot(dfaus, aes(Var1, Var2))+
  geom_raster(aes(fill = Freq), show.legend = F)+
  xlab("Perceived partisanship")+
  ylab("Experimental group")+
  geom_text(aes(label = paste0(Freq, "\n(", Freqperc, "%)")), color = "white", size = 4) +
  scale_fill_continuous(name= "Pearson's \nresiduals")
#labs(title = "Pearson residuals for the Chi-square test\n of imputed ideology of the vignette (recoding 3)\n vs experimental group ")
heatmap1
ggsave("Heatmap pearsons residuals parties.png", path = paste0(main_path,"/plots and tables/plots"), scale = 5)



#################### T TESTS

############# t tests with "doesn't know" as missing

tcontr= t.test(data[data$group=="Control", ]$expideology_r1, conf.level = 0.99)

tveg= t.test(data[data$group=="Vegan", ]$expideology_r1,  conf.level = 0.99)

tmeat= t.test(data[data$group=="Meat", ]$expideology_r1,  conf.level = 0.99)

ttyp= t.test(data[data$group=="Typical", ]$expideology_r1,  conf.level = 0.99)

tethn = t.test(data[data$group=="Ethnic", ]$expideology_r1, conf.level = 0.99)

tcontr$conf.int

tdataset = data.frame(group = c("Control", "Vegan", "Meat", "Typical", "Ethnic"),
                      mean = c(tcontr$estimate, tveg$estimate, tmeat$estimate, ttyp$estimate, tethn$estimate),
                      upper = c(tcontr$conf.int[1], tveg$conf.int[1], tmeat$conf.int[1], ttyp$conf.int[1], tethn$conf.int[1]),
                      lower = c(tcontr$conf.int[2], tveg$conf.int[2], tmeat$conf.int[2], ttyp$conf.int[2], tethn$conf.int[2]))

p1 =ggplot(tdataset, aes(x=mean, y=group))+
  #geom_point()+
  geom_pointrange(aes(xmax=upper, xmin=lower), col="red")+
  geom_vline(xintercept = 0)+
  ylab("Experiemental group")+
  #ylab("")+
  xlab("Ideology")+
  xlim(-1.5, 1.5)+
  labs(#title="T-test results regarding the perceived ideology \nof the character for each experimental group",
    caption ="Extreme Left =-3, Left=-2, Center-Left=-1, Center=0, \nCenter-right=1, Right=2, Extreme right=3, Don't know = NA\n 99% confidence intervals")
# theme(text = element_text(size=rel(4)))
p1

ggsave("T-test, without NA.png", path = paste0(main_path,"/plots and tables/plots"),
       width = 5, height = 5)


####### t-tests with "doesn't know" as 0



tcontr= t.test(data[data$group=="Control", ]$expideology_r5, conf.level = 0.99)

tveg= t.test(data[data$group=="Vegan", ]$expideology_r5,  conf.level = 0.99)

tmeat= t.test(data[data$group=="Meat", ]$expideology_r5,  conf.level = 0.99)

ttyp= t.test(data[data$group=="Typical", ]$expideology_r5,  conf.level = 0.99)

tethn = t.test(data[data$group=="Ethnic", ]$expideology_r5, conf.level = 0.99)


tdataset = data.frame(group = c("Control", "Vegan", "Meat", "Typical", "Ethnic"),
                      mean = c(tcontr$estimate, tveg$estimate, tmeat$estimate, ttyp$estimate, tethn$estimate),
                      upper = c(tcontr$conf.int[1], tveg$conf.int[1], tmeat$conf.int[1], ttyp$conf.int[1], tethn$conf.int[1]),
                      lower = c(tcontr$conf.int[2], tveg$conf.int[2], tmeat$conf.int[2], ttyp$conf.int[2], tethn$conf.int[2]))



p2 = ggplot(tdataset, aes(x=mean, y=group))+
  #geom_point()+
  geom_pointrange(aes(xmax=upper, xmin=lower), col="red")+
  geom_vline(xintercept = 0)+
  ylab("Experiemental group")+
  #ylab("")+
  xlab("Ideology")+
  xlim(-1.5, 1.5)+
  labs(#title="T-test results regarding the perceived ideology \nof the character for each experimental group",
    caption ="Extreme Left =-3, Left=-2, Center-Left=-1, Center=0, \nCenter-right=1, Right=2, Extreme right=3, Don't know = 0\n 99% confidence intervals")
# theme(text = element_text(size=rel(4))) 
p2

ggsave("T-test, with NA as 0.png", path = paste0(main_path,"/plots and tables/plots"), scale=2)


p1|p2

#ggsave("T-test, full.png", path = paste0(main_path,"/plots and tables/plots"), width=8, height = 6)

ggsave("T-test, with NA as 0FORSLIDES.png", path = paste0(main_path,"/plots and tables/plots"), width = 9, height = 6)


########## Pisati's distribution test: I do a Mann-Whitney U test aka
# Wilkox rank test.

# This test tests for whether TWO distributions differ in terms of the fact that,
# extracting a random observation from each distribtion, I have a probability
#of observing that the observation extracted from group 1 is greater than that extracted
#from group two P(x_1>y_1) is \neq .5!

wilcoxvegan = wilcox.test(datanomiss[datanomiss$group == "Control", ]$expideology_r1, #X
                          datanomiss[datanomiss$group == "Vegan", ]$expideology_r1, #Y
                          alternative = "greater", #I want to test that extracting a random
                          #observation from the control group is going to be greater 
                          # (more right) than one extracted from vegan
                          paired = F,
                          conf.int = T) # I do not want a paired test

wilcoxvegan

wilcoxethnic = wilcox.test(datanomiss[datanomiss$group == "Control", ]$expideology_r1, #X
                           datanomiss[datanomiss$group == "Ethnic", ]$expideology_r1, #Y
                           alternative = "greater", #I want to test that extracting a random
                           #observation from the control group is going to be greater 
                           # (more right) than one extracted from vegan
                           paired = F,
                           conf.int = T) # I do not want a paired test

wilcoxethnic

wilcoxmeat = wilcox.test(datanomiss[datanomiss$group == "Control", ]$expideology_r1, #X
                         datanomiss[datanomiss$group == "Meat", ]$expideology_r1, #Y
                         alternative = "less", #I want to test that extracting a random
                         #observation from the control group is going to be greater 
                         # (more right) than one extracted from vegan
                         paired = F,
                         conf.int = T) # I do not want a paired test

wilcoxmeat

wilcoxtypical = wilcox.test(datanomiss[datanomiss$group == "Control", ]$expideology_r1, #X
                            datanomiss[datanomiss$group == "Typical", ]$expideology_r1, #Y
                            alternative = "less", #I want to test that extracting a random
                            #observation from the control group is going to be greater 
                            # (more right) than one extracted from vegan
                            paired = F,
                            conf.int = T) # I do not want a paired test

wilcoxtypical$estimate

tablewilcox = data.frame("Group" = c("Vegan", "Meat", "Ethnic", "Traditional"),
                         "W statistic" = c(wilcoxvegan$statistic, wilcoxmeat$statistic,
                                           wilcoxethnic$statistic, wilcoxtypical$statistic),
                         "P-value" = c(wilcoxvegan$p.value, wilcoxmeat$p.value,
                                       wilcoxethnic$p.value, wilcoxtypical$p.value),
                         "Difference in location" = c(round(wilcoxvegan$estimate, digits = 0), round(wilcoxmeat$estimate, digits = 0),
                                                      round(wilcoxethnic$estimate, digits = 0), round(wilcoxtypical$estimate, digits = 0)),
                         "95% C.I." = c(paste0("[", round(wilcoxvegan$conf.int[1], digits = 0), ",", round(wilcoxvegan$conf.int[2], digits = 0), ")"), 
                                        paste0("(", round(wilcoxmeat$conf.int[1], digits = 0), ",", round(wilcoxmeat$conf.int[2], digits = 0), "]"),
                                        paste0("[", round(wilcoxethnic$conf.int[1], digits =0), ",", round(wilcoxethnic$conf.int[2], digits = 0), ")"),
                                        paste0("(", round(wilcoxtypical$conf.int[1], digits = 0), ",", round(wilcoxtypical$conf.int[2], digits = 0), "]")))



write.xlsx(tablewilcox, paste0(main_path,"/plots and tables/tables/tablewilcox.xlsx"))













################################################################################
######################## REGRESSION MODELS #####################################
################################################################################

################################# HYP H2 H3 H4 H5

# Models group A (Partisan PL as dependent variable)

# Models group B (Ideological PL as dependent variable)

#First model: only variables object of hypotheses + basic sociodemographics


model1a = glm(data=datanomiss, party_PL ~ 
                #polarizzazione
                ideology_r2 + exposure_r1 + culturalindex+
                AP_wagner_spread2 + knowsparty+  interest_r+#politica
                educ_r + age_r+ sex_r + macroarea2 + citysize_r2 +
                social_position_r_noNA,
              family = binomial(link = "logit"))

summary(model1a)

model1b = glm(data=datanomiss, ideology_PL ~ 
                ideology_r2 + exposure_r1 + culturalindex+
                AP_wagner_spread2 + knowsparty+  interest_r+#politica
                educ_r + age_r+ sex_r + macroarea2 + citysize_r2 +
                social_position_r_noNA,
              family = binomial(link = "logit"))

summary(model1b)


#Second model: As model 1 but with Collocated instead of Ideology


model2a = glm(data=datanomiss, party_PL ~ 
                #polarizzazione
                collocated + exposure_r1 + culturalindex+ #statbehav_r2+
                AP_wagner_spread2 + knowsparty+ interest_r+#politica
                educ_r + age_r+ sex_r + macroarea2 + citysize_r2 +
                social_position_r_noNA,
              family = binomial(link = "logit"))
summary(model2a)

model2b = glm(data=datanomiss, ideology_PL ~ 
                #polarizzazione
                collocated + exposure_r1 + culturalindex+
                AP_wagner_spread2 + knowsparty+  interest_r+#politica
                educ_r + age_r+ sex_r + macroarea2 + citysize_r2 +
                social_position_r_noNA,
              family = binomial(link = "logit"))

summary(model2b)


################  THE PISATI MODELS (INDEPENDENT VARIABLES SEPARATED)

model4aH2 = glm(data=datanomiss, party_PL ~ 
                  collocated + knowsparty+  interest_r+#politica
                  educ_r + age_r+ sex_r + macroarea2 + citysize_r2 +
                  social_position_r_noNA,
                family = binomial(link = "logit"))

summary(model4aH2)

model4bH2 = glm(data=datanomiss, ideology_PL ~ 
                  collocated + knowsparty+ interest_r+#politica
                  educ_r + age_r+ sex_r + macroarea2 + citysize_r2 +
                  social_position_r_noNA,
                family = binomial(link = "logit"))

summary(model4bH2)

model4aH3 = glm(data=datanomiss, party_PL ~ 
                  AP_wagner_spread2 + knowsparty+  interest_r+#politica
                  educ_r + age_r+ sex_r + macroarea2 + citysize_r2 +
                  social_position_r_noNA,
                family = binomial(link = "logit"))

summary(model4aH3)

model4bH3 = glm(data=datanomiss, ideology_PL ~ 
                  AP_wagner_spread2 + knowsparty+ interest_r+#politica
                  educ_r + age_r+ sex_r + macroarea2 + citysize_r2 +
                  social_position_r_noNA,
                family = binomial(link = "logit"))

summary(model4bH3)

model4aH4 = glm(data=datanomiss, party_PL ~ 
                  exposure_r1 + knowsparty+  interest_r+#politica
                  educ_r + age_r+ sex_r + macroarea2 + citysize_r2 +
                  social_position_r_noNA,
                family = binomial(link = "logit"))

summary(model4aH4)

model4bH4 = glm(data=datanomiss, ideology_PL ~ 
                  exposure_r1 + knowsparty+ interest_r+#politica
                  educ_r + age_r+ sex_r + macroarea2 + citysize_r2 +
                  social_position_r_noNA,
                family = binomial(link = "logit"))

summary(model4bH4)

model4aH5 = glm(data=datanomiss, party_PL ~ 
                  culturalindex+  knowsparty+  interest_r+#politica
                  educ_r + age_r+ sex_r + macroarea2 + citysize_r2 +
                  social_position_r_noNA,
                family = binomial(link = "logit"))

summary(model4aH5)

model4bH5 = glm(data=datanomiss, ideology_PL ~ 
                  culturalindex+ knowsparty+ interest_r+#politica
                  educ_r + age_r+ sex_r + macroarea2 + citysize_r2 +
                  social_position_r_noNA,
                family = binomial(link = "logit"))

summary(model4bH5)


####################### H6 AND H7
#h6
####### 

# Models group C (Interaction as dependent variable)
# Models group D (conversation as dependent variable)



# NOTICE THAT THIS TIME THE STORY IS DIFFERENT, IT'S NOT IDEOLOGY
# THAT DICTATES THE EXPECTATIONS ON WHETHER THRE CONVERSATION IS 
# GOING TO BE PLEASANT, BUT PARTISANSHIP!



#Alternative (NESTED) model alternatives for behav and exponv
# (discuss with prof!!!)


#Model one only PL and sociodemo

model1c =  polr(expbehav_r2ord ~ ideology_PL+
                  #politica
                  educ_r + age_r+ sex_r + macroarea2 + citysize_r2 +    
                  social_position_r_noNA, data = datanomiss, method = "logistic")



summary(model1c)


#### model2 PL+sociodemo+psicodemo

model2c =  polr(expbehav_r2ord ~ ideology_PL+
                  #politica
                  ideology_r2+
                  educ_r + age_r+ sex_r + macroarea2 + citysize_r2 +    
                  social_position_r_noNA+
                  conscientiousness + openness + agreeableness +
                  extraversion + neuroticism, data = datanomiss, method = "logistic")

summary(model2c)


### model 3 (all - the same as model 3 before) 
# this goes in the appendix


model3c = polr(expbehav_r2ord ~ ideology_PL+
                 AP_wagner_spread2+
                 #polarizzazione
                 interest_r + exposure_r1 + culturalindex+ideology_r2+
                 knowsparty+
                 #politica
                 educ_r + age_r+ sex_r + macroarea2 + citysize_r2 +    
                 social_position_r_noNA+
                 conscientiousness + openness + agreeableness +
                 extraversion + neuroticism, data = datanomiss, method = "logistic")

#H7
####### 

#Model one only PL and sociodemo

model1d =  polr(expconv_r2ord ~ ideology_PL+
                  #politica
                  educ_r + age_r+ sex_r + macroarea2 + citysize_r2 +    
                  social_position_r_noNA, data = datanomiss, method = "logistic")



summary(model1d)


#### model2 PL+sociodemo+psicodemo

model2d =  polr(expconv_r2ord ~ ideology_PL+
                  #politica
                  ideology_r2+
                  educ_r + age_r+ sex_r + macroarea2 + citysize_r2 +    
                  social_position_r_noNA+
                  conscientiousness + openness + agreeableness +
                  extraversion + neuroticism, data = datanomiss, method = "logistic")

summary(model2d)


### model 3 (all - the same as model 3 before) 
# this goes in the appendix


model3d = polr(expconv_r2ord ~ ideology_PL+
                 AP_wagner_spread2+
                 #polarizzazione
                 interest_r + exposure_r1 + culturalindex+ideology_r2+
                 knowsparty+
                 #politica
                 educ_r + age_r+ sex_r + macroarea2 + citysize_r2 +    
                 social_position_r_noNA+
                 conscientiousness + openness + agreeableness +
                 extraversion + neuroticism, data = datanomiss, method = "logistic")

summary(model3d)

########### try ordered logit














######## Export model results########################

##Party PL

setwd(paste0(main_path,"/plots and tables/tables"))

wordreg(list(model1a, model2a), file = "PartyPL.docx",
        single.row = T,
        # custom.coef.names = names,
        custom.model.names = c("Model 1a", "Model 2a"),
        caption = "Partisan PL")

## Ideology PL

wordreg(list(model1b, model2b), file = "IdeologyPL.docx",
        single.row = T,
        #custom.coef.names = names,
        custom.model.names = c("Model 1b", "Model 2b"),
        caption = "Ideological PL")

wordreg(list(model4aH2, model4aH3, model4aH4, model4aH5), file = "PisatiIndepModelsPartisan.docx",
        single.row = T,
        # custom.coef.names = names,
        custom.model.names = c("Partisan PL", "Partisan PL", "Partisan PL", "Partisan PL"),
        caption = "Models with only single IV+control")

wordreg(list(model4bH2, model4bH3, model4bH4, model4bH5), file = "PisatiIndepModelsIdeology.docx",
        single.row = T,
        # custom.coef.names = names,
        custom.model.names = c("Ideological PL", "Ideological PL", "Ideological PL", "Ideological PL"),
        caption = "Models with only single IV+control")





## Interaction (Behav AP) and conversations

wordreg(list(model1c, model2c, model1d, model2d), 
        file = "Tables H6H7 Main.docx",
        single.row = T,
        # custom.coef.names = names1,
        custom.model.names = c("Model 1c", "Model 2c",
                               "Model 1d", "Model 2d"),
        caption = "")

# Models for appendix

wordreg(list(model3c, model3d), 
        file = "Tables H6H7 Appendix.docx",
        single.row = T,
        # custom.coef.names = names1,
        custom.model.names = c("Model 3c", "Model 3d"),
        caption = "")


wordreg(list(model4c, model4d), 
        file = "Tables H6H7 Appendix Outgroup.docx",
        single.row = T,
        # custom.coef.names = names1,
        custom.model.names = c("Model 4c", "Model 4d"),
        caption = "")


setwd(main_path)



########## Predicted values for specific category of people



junkie = data.frame(collocated = "Collocated", exposure_r1="High",  culturalindex=4,
                    AP_wagner_spread2= mean(datanomiss$AP_wagner_spread),
                    knowsparty=7, interest_r="High", educ_r="High", 
                    age_r=mean(data$age_r), sex_r="Female", macroarea2="Center",
                    citysize_r2="Big", social_position_r_noNA=7)
predict(model2a, junkie,
        type = "response")

dontgiveafuck = data.frame(collocated = "Not collocated", exposure_r1="Low",
                           culturalindex=0,
                           AP_wagner_spread2= mean(datanomiss$AP_wagner_spread),
                           knowsparty=0, interest_r="Low", educ_r="Low", 
                           age_r=mean(data$age_r), sex_r="Female", macroarea2="Center",
                           citysize_r2="Small", social_position_r_noNA=3)

predict(model2a, dontgiveafuck,
        type = "response")


##############################################################################
##################################REGRESSION GRAFICS##########################
###############################################################################

#### Average marginal effects (library(margins))

mara=margins(model2a)
marb=margins(model2b)

sociodemo = c("age_r", 
              "citysize_r2Big",
              "educ_rHigh",
              "macroarea2North",
              "macroarea2South",
              "sex_rFemale",
              "social_position_r_noNA")
summary(mara)[summary(mara)$factor%in% sociodemo, ]

summary(mara)$factor

summary(marb)$factor

summary(marb)[summary(marb)$factor %in% sociodemo, ]


###################################

psociodemo = ggplot(data = summary(mara)[summary(mara)$factor%in% sociodemo, ])+
  geom_pointrange(aes(x = factor, 
                      y=AME,
                      ymax=AME+1.96*SE, 
                      ymin=AME-1.96*SE),
                  col='red',
                  shape=17,
                  alpha = 1,
                  position = position_nudge(x = 1/10),
                  show.legend = F)+
  geom_pointrange(data = summary(marb)[summary(marb)$factor%in% sociodemo, ],
                  aes(x = factor, 
                      y=AME,
                      ymax=AME+1.96*SE, 
                      ymin=AME-1.96*SE),
                  col='blue',
                  shape = 17,
                  alpha = 1,
                  position = position_nudge(x = -1/10),
                  show.legend = F)+
  geom_hline(yintercept = 0, col='black', alpha = 1/2)+
  scale_x_discrete(labels = 
                     c("Age","Big City", "Degree", "North \n(ref.Center)", "South \n(ref. Center)", "Female", "Social\nposition"))+
  xlab("")+
  ylab("AME")+
  #ylim(-0.31, 0.31)+
  labs(caption = "AMEs for models 2a and 2b\nRed triangle  = Partisan PL, Blue circle  = Ideological PL, 95% C.I.")+
  # theme(text = element_text(size=rel(4)))+
  coord_flip()




ppoli = ggplot(summary(mara)[!(summary(mara)$factor %in% sociodemo), ])+
  geom_pointrange(aes(x = factor, 
                      y=AME,
                      ymax=AME+1.96*SE, 
                      ymin=AME-1.96*SE),
                  col='red',
                  shape=17,
                  alpha = 1,
                  position = position_nudge(x = 1/10),
                  show.legend = F)+
  geom_pointrange(data = summary(marb)[!(summary(marb)$factor %in% sociodemo), ],
                  aes(x = factor, 
                      y=AME,
                      ymax=AME+1.96*SE, 
                      ymin=AME-1.96*SE),
                  col='blue',
                  shape = 17,
                  alpha = 1,
                  position = position_nudge(x = -1/10),
                  show.legend = F)+
  geom_hline(yintercept = 0, col='black', alpha = 1/2)+
  scale_x_discrete(labels = c("Aff. Pol.", "Colloc.", "Cult. cons.", "News \nexposure", "Interest", "Party \nKnowledge"))+
  xlab("")+
  ylab("AME")+
  # theme(text = element_text(size=rel(4)))
  #ylim(-0.31, 0.31)+
  coord_flip()

ppoli


ppoli/psociodemo

ggsave("AME Model2a and 2b.png", path = paste0(main_path,"/plots and tables/plots"), 
       width = 6, height = 8)

### those for slides

psociodemo_slides= psociodemo+theme(axis.text.x = element_text(face="bold", color="black", 
                                            size=15),
                 axis.text.y = element_text(face="bold", color="black", 
                                            size=15))
ppoli_slides = ppoli + theme(axis.text.x = element_text(face="bold", color="black", 
                                                        size=15),
                             axis.text.y = element_text(face="bold", color="black", 
                                                        size=15))

ppoli_slides|psociodemo_slides
ggsave("AME Model2a and 2bFORSLIDES.png", path = paste0(main_path,"/plots and tables/plots"), 
       width = 8, height = 6)


######### Marginsplot for the models with separated independent variables

mar4aH2=margins(model4aH2)
mar4bH2=margins(model4bH2)


mar4aH3=margins(model4aH3)
mar4bH3=margins(model4bH3)


mar4aH4=margins(model4aH4)
mar4bH4=margins(model4bH4)


mar4aH5=margins(model4aH5)
mar4bH5=margins(model4bH5)


summary(mar4aH2)$factor

aus_id = rbind(summary(mar4aH2)[5 ,], summary(mar4aH3)[4 ,], 
               summary(mar4aH4)[7 ,], summary(mar4aH5)[6 ,])

aus_party = rbind(summary(mar4bH2)[5 ,], summary(mar4bH3)[4 ,], 
                  summary(mar4bH4)[7 ,], summary(mar4bH5)[6 ,])



independentmodelsforid = ggplot(aus_id)+
  geom_pointrange(aes(x = factor, 
                      y=AME,
                      ymax=AME+1.96*SE, 
                      ymin=AME-1.96*SE),
                  col='red',
                  shape=17,
                  alpha = 1,
                  position = position_nudge(x = 1/10),
                  show.legend = F)+
  geom_pointrange(data = aus_party,
                  aes(x = factor, 
                      y=AME,
                      ymax=AME+1.96*SE, 
                      ymin=AME-1.96*SE),
                  col='blue',
                  shape = 17,
                  alpha = 1,
                  position = position_nudge(x = -1/10),
                  show.legend = F)+
  geom_hline(yintercept = 0, col='black', alpha = 1/2)+
  scale_x_discrete(labels = 
                     c("Aff. Pol.", "Colloc.", "Cult. cons.", "News \nexposure"))+
  xlab("")+
  ylab("AME")+
  #ylim(-0.31, 0.31)+
  labs(caption = "AMEs for Models 5a,5b,6a,6b,7a,7b,8a,8b.\nRed triangle = Partisan PL, Blue  = Ideological PL, 95% C.I.")+
  #theme(text = element_text(size=rel(5)))+
  coord_flip()

ggsave("AME Pisati Independent Models.png", path = paste0(main_path,"/plots and tables/plots"), 
       width = 8, height = 4)






################### LI PRESENTO PER CATEGORIWE (INTERESE SOCIODEMO-PSICOMETRICHE E COSì VIA
# è PIU' CARINO)

### Predictive margins per le variabili indipendenti continue, poi arrange  a 4

#### Average marginal effects (library(margins))

pred1 =cplot(model2a, "culturalindex", what = "prediction")
pred2 =cplot(model2a, "AP_wagner_spread2", what = "prediction")
pred3 =cplot(model2a, "social_position_r_noNA", what = "prediction")

pred6 =cplot(model2b, "culturalindex", what = "prediction")
pred7 =cplot(model2b, "AP_wagner_spread2", what = "prediction")
pred8 =cplot(model2b, "social_position_r_noNA", what = "prediction")


aux= datanomiss |>
  group_by(culturalindex) |>
  summarise(num = n(),
            rel = num/nrow(datanomiss))

p1=ggplot(pred1, aes(x = xvals)) + 
  geom_line(aes(y = yvals)) +
  geom_smooth(aes(y = upper), linetype = 2) +
  geom_smooth(aes(y = lower), linetype = 2) +
  #ggtitle("Predicted probabilities of Party PL vs \n values of cultural participation index") +
  #geom_hline(yintercept=0)+
  geom_col(data = aux, aes(x=culturalindex, y=rel),
           alpha = 1/2, fill ='grey', col='grey')+
  xlab("Cultural consumption") +
  ylab("Predicted probabilities")+
  ylim(0,1)

p1
### Predictive margins per AP_wagner_spread2

p2=ggplot(pred2, aes(x = xvals)) + 
  geom_line(aes(y = yvals)) +
  geom_smooth(aes(y = upper), linetype = 2) +
  geom_smooth(aes(y = lower), linetype = 2) +
  #ggtitle("Predicted probabilities of Party PL vs \n values of affective polarization index") +
  #geom_hline(yintercept=0)+
  geom_density(data = datanomiss, aes(x=AP_wagner_spread2),
               alpha = 1/2, fill ='grey', col='grey')+
  xlab("Affective polarization") +
  ylab("Predicted probabilities")+
  ylim(0,1)

p2

### Predictive margins per social_position_r_noNA

aux= datanomiss |>
  group_by(social_position_r_noNA) |>
  summarise(num = n(),
            rel = num/nrow(datanomiss))

p3=ggplot(pred3, aes(x = xvals)) + 
  geom_line(aes(y = yvals)) +
  geom_smooth(aes(y = upper), linetype = 2) +
  geom_smooth(aes(y = lower), linetype = 2) +
  #ggtitle("Predicted probabilities of Party PL vs \n values of self-perceived social position") +
  #geom_hline(yintercept=0)+
  geom_col(data = aux, aes(x=social_position_r_noNA, y=rel),
           alpha = 1/2, fill ='grey', col='grey')+
  xlab("Self-perceived social position") +
  ylab("Predicted probabilities")+
  ylim(0,1)

p3


(p3+p2)/(p1)#+labs(caption = "AMEs for model 4a and 4b\nRed  = Partisan PL, Blue  = Ideological PL\n 95% C.I.")

ggsave("Predicted probabilities, continuous variables, MODEL2A.png", path = paste0(main_path,"/plots and tables/plots"), 
       width = 8, height = 10)

# grafici combinati


###############################################################################
################################################################################
#### STESSI GRAFICI MA COL MODELLO CON ideology_PL COME VAR DIP



aux= datanomiss |>
  group_by(culturalindex) |>
  summarise(num = n(),
            rel = num/nrow(datanomiss))

p6=ggplot(pred6, aes(x = xvals)) + 
  geom_line(aes(y = yvals)) +
  geom_smooth(aes(y = upper), linetype = 2) +
  geom_smooth(aes(y = lower), linetype = 2) +
  #ggtitle("Predicted probabilities of Party PL vs \n values of cultural participation index") +
  #geom_hline(yintercept=0)+
  geom_col(data = aux, aes(x=culturalindex, y=rel),
           alpha = 1/2, fill ='grey', col='grey')+
  xlab("Cultural consumption") +
  ylab("Predicted probabilities")+
  ylim(0,1)

p6

### Predictive margins per AP_wagner_spread2

p7=ggplot(pred7, aes(x = xvals)) + 
  geom_line(aes(y = yvals)) +
  geom_smooth(aes(y = upper), linetype = 2) +
  geom_smooth(aes(y = lower), linetype = 2) +
  #ggtitle("Predicted probabilities of Party PL vs \n values of affective polarization index") +
  #geom_hline(yintercept=0)+
  geom_density(data = datanomiss, aes(x=AP_wagner_spread2),
               alpha = 1/2, fill ='grey', col='grey')+
  xlab("Affective polarization") +
  ylab("Predicted probabilities")+
  ylim(0,1)

p7

### Predictive margins per social_position_r_noNA

aux= datanomiss |>
  group_by(social_position_r_noNA) |>
  summarise(num = n(),
            rel = num/nrow(datanomiss))

p8=ggplot(pred8, aes(x = xvals)) + 
  geom_line(aes(y = yvals)) +
  geom_smooth(aes(y = upper), linetype = 2) +
  geom_smooth(aes(y = lower), linetype = 2) +
  #ggtitle("Predicted probabilities of Party PL vs \n values of self-perceived social position") +
  #geom_hline(yintercept=0)+
  geom_col(data = aux, aes(x=social_position_r_noNA, y=rel),
           alpha = 1/2, fill ='grey', col='grey')+
  xlab("Self-perceived social position") +
  ylab("Predicted probabilities")+
  ylim(0,1)

p8

#ggsave("PredProbOPPartyPL.png", path = paste0(main_path,"/plots and tables/plots"), scale=2)

(p8+p7)/(p6)

ggsave("Predicted probabilities, continuous variables, MODEL2B.png", path = paste0(main_path,"/plots and tables/plots"), 
       width = 8, height = 10)


# grafici combinati
# 
# names(pred1) = paste0("part.", names(pred1))
# names(pred2) = paste0("part.", names(pred2))
# names(pred3) = paste0("part.", names(pred3))
# 
# 
# names(pred6) = paste0("id.", names(pred6))
# names(pred7) = paste0("id.", names(pred7))
# names(pred8) = paste0("id.", names(pred8))

pred1$typePL = "Partisan PL"
pred2$typePL = "Partisan PL"
pred3$typePL = "Partisan PL"

pred6$typePL = "Ideological PL"
pred7$typePL = "Ideological PL"
pred8$typePL = "Ideological PL"

pred_cult_comb = rbind(pred1,pred6)
pred_aff_comb = rbind(pred2,pred7)
pred_soc_comb = rbind(pred3,pred8)

p1_comb=ggplot(pred_cult_comb, aes(x = xvals, col=typePL)) + 
  geom_line(aes(y = yvals)) +
  geom_smooth(aes(y = upper), linetype = 2) +
  geom_smooth(aes(y = lower), linetype = 2) +
  #geom_hline(yintercept=0)+
  geom_col(data = aux, aes(x=culturalindex, y=rel),
           alpha = 1/2, fill ='grey', col='grey')+
  xlab("Cultural consumption") +
  ylab("Predicted probabilities")+
  ylim(0,1)

p1_comb

p2_comb=ggplot(pred_aff_comb, aes(x = xvals, col=typePL)) + 
  geom_line(aes(y = yvals)) +
  geom_smooth(aes(y = upper), linetype = 2) +
  geom_smooth(aes(y = lower), linetype = 2) +
  #geom_hline(yintercept=0)+
  geom_col(data = aux, aes(x=culturalindex, y=rel),
           alpha = 1/2, fill ='grey', col='grey')+
  xlab("Cultural consumption") +
  ylab("Predicted probabilities")+
  ylim(0,1)

p2_comb

p3_comb=ggplot(pred_soc_comb, aes(x = xvals, col=typePL)) + 
  geom_line(aes(y = yvals)) +
  geom_smooth(aes(y = upper), linetype = 2) +
  geom_smooth(aes(y = lower), linetype = 2) +
  #geom_hline(yintercept=0)+
  geom_col(data = aux, aes(x=culturalindex, y=rel),
           alpha = 1/2, fill ='grey', col='grey')+
  xlab("Cultural consumption") +
  ylab("Predicted probabilities")+
  ylim(0,1)

p3_comb

(p1_comb|p2_comb)/p3_comb


################## AME FOR H6 and H7 (Models 2c and 2d)


summary(model2c)

# grafici combinatia_int_full = effect("ideology_PL", model2c, xlevels = 2)

df_0_int_full = data.frame("outcome"= c("Unlikely", "Neither Likely nor Unlikely", "Likely"), "predprob" = a_int_full$prob[1, ], "lower" = a_int_full$lower.prob[1, ], "upper" = a_int_full$upper.prob[1, ], row.names = c(1,2,3)) 
df_1_int_full = data.frame("outcome"= c("Unlikely", "Neither Likely nor Unlikely", "Likely"), "predprob" = a_int_full$prob[2, ], "lower" = a_int_full$lower.prob[2, ], "upper" = a_int_full$upper.prob[2, ], row.names = c(1,2,3)) 


ordered_logit_AME_int_full = ggplot(data= df_0_int_full)+
  geom_pointrange(aes(x = outcome, 
                      y=predprob,
                      ymax=upper, 
                      ymin=lower),
                  col='red',
                  shape=17,
                  alpha = 1,
                  position = position_nudge(x = 1/20),
                  show.legend = F)+
  geom_pointrange(data = df_1_int_full,
                  aes(x = outcome, 
                      y=predprob,
                      ymax=upper, 
                      ymin=lower),
                  col='blue',
                  alpha = 1,
                  position = position_nudge(x = -1/20),
                  show.legend = F)+
  xlab("")+
  ylab("Predicted values for Interaction")+
  #labs(title = "Predicted probabilities for likelihood to interact\nIdeological PL vs No Ideological PL")+
  scale_y_continuous(breaks = seq(0,0.7, by=0.1), limits = c(0.05,0.7))

ordered_logit_AME_int_full


### same but with conversation


summary(model2d)

a_conv_full = effect("ideology_PL", model2d, xlevels = 2)

df_0_conv_full = data.frame("outcome"= c("Unlikely", "Neither Likely nor Unlikely", "Likely"), "predprob" = a_conv_full$prob[1, ], "lower" = a_conv_full$lower.prob[1, ], "upper" = a_conv_full$upper.prob[1, ], row.names = c(1,2,3)) 
df_1_conv_full = data.frame("outcome"= c("Unlikely", "Neither Likely nor Unlikely", "Likely"), "predprob" = a_conv_full$prob[2, ], "lower" = a_conv_full$lower.prob[2, ], "upper" = a_conv_full$upper.prob[2, ], row.names = c(1,2,3)) 


ordered_logit_AME_conv_full = ggplot(data= df_0_conv_full)+
  geom_pointrange(aes(x = outcome, 
                      y=predprob,
                      ymax=upper, 
                      ymin=lower),
                  col='red',
                  shape=17,
                  alpha = 1,
                  position = position_nudge(x = 1/20),
                  show.legend = F)+
  geom_pointrange(data = df_1_conv_full,
                  aes(x = outcome, 
                      y=predprob,
                      ymax=upper, 
                      ymin=lower),
                  col='blue',
                  alpha = 1,
                  position = position_nudge(x = -1/20),
                  show.legend = F)+
  xlab("")+
  ylab("Predicted values for Conversation")+
  labs(#title = "Predicted probabilities for likelihood to converse\nIdeological PL vs No Ideological PL", 
       caption = "Red triangle = did not do ideological PL \nBlue circle = did ideology PL\n Predicted values for models 2c and 2d")+
  scale_y_continuous(breaks = seq(0,0.7, by=0.1), limits = c(0.05,0.7))


ordered_logit_AME_conv_full
ordered_logit_AME_int_full|ordered_logit_AME_conv_full
ggsave("Ordered Logit effects full MODELS 2C2D.png", path = paste0(main_path,"/plots and tables/plots"), width = 9, height = 6)

################### The same figure as above but with models 3c and 3d



summary(model3c)

a_int_full = effect("ideology_PL", model3c, xlevels = 2)

df_0_int_full = data.frame("outcome"= c("Unlikely", "Neither Likely nor Unlikely", "Likely"), "predprob" = a_int_full$prob[1, ], "lower" = a_int_full$lower.prob[1, ], "upper" = a_int_full$upper.prob[1, ], row.names = c(1,2,3)) 
df_1_int_full = data.frame("outcome"= c("Unlikely", "Neither Likely nor Unlikely", "Likely"), "predprob" = a_int_full$prob[2, ], "lower" = a_int_full$lower.prob[2, ], "upper" = a_int_full$upper.prob[2, ], row.names = c(1,2,3)) 


ordered_logit_AME_int_full = ggplot(data= df_0_int_full)+
  geom_pointrange(aes(x = outcome, 
                      y=predprob,
                      ymax=upper, 
                      ymin=lower),
                  col='red',
                  shape=17,
                  alpha = 1,
                  #size=1.3,
                  position = position_nudge(x = 1/20),
                  show.legend = F)+
  geom_pointrange(data = df_1_int_full,
                  aes(x = outcome, 
                      y=predprob,
                      ymax=upper, 
                      ymin=lower),
                  col='blue',
                  alpha = 1,
                  #size=1.3,
                  position = position_nudge(x = -1/20),
                  show.legend = F)+
  xlab("")+
  ylab("Predicted values for Interaction")+
  #labs(title = "Predicted probabilities for likelihood to interact\nIdeological PL vs No Ideological PL")+
  scale_y_continuous(breaks = seq(0,0.7, by=0.1), limits = c(0.05,0.7))

ordered_logit_AME_int_full


### same but with conversation


summary(model3d)

a_conv_full = effect("ideology_PL", model3d, xlevels = 2)

df_0_conv_full = data.frame("outcome"= c("Unlikely", "Neither Likely nor Unlikely", "Likely"), "predprob" = a_conv_full$prob[1, ], "lower" = a_conv_full$lower.prob[1, ], "upper" = a_conv_full$upper.prob[1, ], row.names = c(1,2,3)) 
df_1_conv_full = data.frame("outcome"= c("Unlikely", "Neither Likely nor Unlikely", "Likely"), "predprob" = a_conv_full$prob[2, ], "lower" = a_conv_full$lower.prob[2, ], "upper" = a_conv_full$upper.prob[2, ], row.names = c(1,2,3)) 


ordered_logit_AME_conv_full = ggplot(data= df_0_conv_full)+
  geom_pointrange(aes(x = outcome, 
                      y=predprob,
                      ymax=upper, 
                      ymin=lower),
                  col='red',
                  shape=17,
                  alpha = 1,
                  #size=1.3,
                  position = position_nudge(x = 1/20),
                  show.legend = F)+
  geom_pointrange(data = df_1_conv_full,
                  aes(x = outcome, 
                      y=predprob,
                      ymax=upper, 
                      ymin=lower),
                  col='blue',
                  alpha = 1,
                  #size=1.3,
                  position = position_nudge(x = -1/20),
                  show.legend = F)+
  xlab("")+
  ylab("Predicted values for Conversation")+
  labs(#title = "Predicted probabilities for likelihood to converse\nIdeological PL vs No Ideological PL", 
    caption = "Red triangle = did not do ideological PL \nBlue circle = did ideology PL\n Predicted values for models 3c and 3d")+
  scale_y_continuous(breaks = seq(0,0.7, by=0.1), limits = c(0.05,0.7))


ordered_logit_AME_conv_full
ordered_logit_AME_int_full|ordered_logit_AME_conv_full
ggsave("Ordered Logit effects full MODELS3C3D.png", path = paste0(main_path,"/plots and tables/plots"), width = 9, height = 6)


#### for slides

legend_info = data.frame("outcome" = c("Unlikely", "Unlikely"), "predprob" = c(0.6, 0.7),
                         "text" = c("No PL", "PL"))

  
ordered_logit_AME_int_full_slides = ordered_logit_AME_int_full +
  scale_x_discrete(labels = c("Likely", "Neither Likely\nnor Unlikely", "Unlikely"))+
                              theme(axis.text.x = element_text(face="bold", color="black",size=15),
                                     axis.text.y = element_text(face="bold", color="black",size=15))+
  geom_text(data = legend_info, aes(x=outcome,y=predprob, label=text), 
            fontface = "bold", size = 7,color=c("red", "blue"))

ordered_logit_AME_conv_full_slides = ordered_logit_AME_conv_full +
  scale_x_discrete(labels = c("Likely", "Neither Likely\nnor Unlikely", "Unlikely"))+
  theme(axis.text.x = element_text(face="bold", color="black",size=15),
        axis.text.y = element_text(face="bold", color="black",size=15))

ordered_logit_AME_int_full_slides|ordered_logit_AME_conv_full_slides
ggsave("Ordered Logit effects full MODELS3C3DSLIDES.png", path = paste0(main_path,"/plots and tables/plots"), width = 9, height = 6)





############## Predictive probabilities for numeric significant variables
# in models 2c and 2d (Agreeableness and Conscientiousness)

pred10 = effect("agreeableness", model2c, xlevels = 9)
pred11 = effect("extraversion", model2c, xlevels = 9)
pred12 = effect("agreeableness", model2d, xlevels = 9)
pred13 = effect("extraversion", model2d, xlevels = 9)

pred10 = data.frame("outcome"= c(rep("Unlikely", 9),rep("Neither Likely nor Unlikely", 9), rep("Likely", 9)), 
                    "xvals" = rep(seq(0,4, by=0.5),3),
                    "predprob" = c(pred10$prob[, 1], pred10$prob[, 2], pred10$prob[, 3]), 
                    "lower" = c(pred10$lower.prob[, 1], pred10$lower.prob[, 2], pred10$lower.prob[, 3]), 
                    "upper" = c(pred10$upper.prob[, 1], pred10$upper.prob[, 2], pred10$upper.prob[, 3])) 

aux= datanomiss |>
  group_by(agreeableness) |>
  summarise(num = n(),
            rel = num/nrow(datanomiss))


p10=ggplot(pred10, aes(x = xvals, col = outcome)) + 
  geom_line(aes(y = predprob), show.legend = F) +
  geom_smooth(aes(y = upper), linetype = 2, se=F, show.legend = F) +
  geom_smooth(aes(y = lower), linetype = 2, se=F, show.legend = F) +
  #ggtitle("Predicted probabilities of Party PL vs \n values of cultural participation index") +
  #geom_hline(yintercept=0)+
  geom_col(data = aux, aes(x=agreeableness, y=rel),
           alpha = 1/2, fill ='grey', col='grey')+
  xlab("Agreeableness") +
  ylab("Pred. Prob for Interaction")+
  ylim(0,0.75)

p10
#ggsave("PredProbCIPartyPL.png", path = paste0(main_path,"/plots and tables/plots"),scale=2)

pred11 = data.frame("outcome"= c(rep("Unlikely", 9),rep("Neither Likely nor Unlikely", 9), rep("Likely", 9)), 
                    "xvals" = rep(seq(0,4, by=0.5),3),
                    "predprob" = c(pred11$prob[, 1], pred11$prob[, 2], pred11$prob[, 3]), 
                    "lower" = c(pred11$lower.prob[, 1], pred11$lower.prob[, 2], pred11$lower.prob[, 3]), 
                    "upper" = c(pred11$upper.prob[, 1], pred11$upper.prob[, 2], pred11$upper.prob[, 3])) 

aux= datanomiss |>
  group_by(extraversion) |>
  summarise(num = n(),
            rel = num/nrow(datanomiss))


p11=ggplot(pred11, aes(x = xvals, col = outcome)) + 
  geom_line(aes(y = predprob), show.legend = F) +
  geom_smooth(aes(y = upper), linetype = 2, se=F, show.legend = F) +
  geom_smooth(aes(y = lower), linetype = 2, se=F, show.legend = F) +
  #ggtitle("Predicted probabilities of Party PL vs \n values of cultural participation index") +
  #geom_hline(yintercept=0)+
  geom_col(data = aux, aes(x=extraversion, y=rel),
           alpha = 1/2, fill ='grey', col='grey')+
  xlab("Extraversion") +
  ylab("Pred. Prob for Interaction")+
  ylim(0,0.75)

p11

### Agreeableness in conversation

pred12 = data.frame("outcome"= c(rep("Unlikely", 9),rep("Neither Likely nor Unlikely", 9), rep("Likely", 9)), 
                    "xvals" = rep(seq(0,4, by=0.5),3),
                    "predprob" = c(pred12$prob[, 1], pred12$prob[, 2], pred12$prob[, 3]), 
                    "lower" = c(pred12$lower.prob[, 1], pred12$lower.prob[, 2], pred12$lower.prob[, 3]), 
                    "upper" = c(pred12$upper.prob[, 1], pred12$upper.prob[, 2], pred12$upper.prob[, 3])) 

aux= datanomiss |>
  group_by(agreeableness) |>
  summarise(num = n(),
            rel = num/nrow(datanomiss))


p12=ggplot(pred12, aes(x = xvals, col = outcome)) + 
  geom_line(aes(y = predprob), show.legend = F) +
  geom_smooth(aes(y = upper), linetype = 2, se=F, show.legend = F) +
  geom_smooth(aes(y = lower), linetype = 2, se=F, show.legend = F) +
  #ggtitle("Predicted probabilities of Party PL vs \n values of cultural participation index") +
  #geom_hline(yintercept=0)+
  geom_col(data = aux, aes(x=agreeableness, y=rel),
           alpha = 1/2, fill ='grey', col='grey')+
  xlab("Agreeableness") +
  ylab("Pred. Prob for Conversation")+
  ylim(0,0.75)

p12

pred13 = data.frame("outcome"= c(rep("Unlikely", 9),rep("Neither Likely nor Unlikely", 9), rep("Likely", 9)), 
                    "xvals" = rep(seq(0,4, by=0.5),3),
                    "predprob" = c(pred13$prob[, 1], pred13$prob[, 2], pred13$prob[, 3]), 
                    "lower" = c(pred13$lower.prob[, 1], pred13$lower.prob[, 2], pred13$lower.prob[, 3]), 
                    "upper" = c(pred13$upper.prob[, 1], pred13$upper.prob[, 2], pred13$upper.prob[, 3])) 

aux= datanomiss |>
  group_by(extraversion) |>
  summarise(num = n(),
            rel = num/nrow(datanomiss))

names(pred13)[1] = "Outcome"
p13=ggplot(pred13, aes(x = xvals, col = Outcome)) + 
  geom_line(aes(y = predprob)) +
  geom_smooth(aes(y = upper), linetype = 2, se=F, show.legend = F) +
  geom_smooth(aes(y = lower), linetype = 2, se=F, show.legend = F) +
  #ggtitle("Predicted probabilities of Party PL vs \n values of cultural participation index") +
  #geom_hline(yintercept=0)+
  geom_col(data = aux, aes(x=extraversion, y=rel),
           alpha = 1/2, fill ='grey', col='grey')+
  xlab("Extraversion") +
  ylab("Pred. Prob for Conversation")+
  ylim(0,0.75)+
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        #legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=18), #change legend title font size
        legend.text = element_text(size=18))

p13

(p10+p11)/(p12+p13)

ggsave("Predicted probabilities, psicological vaiables, MODELs2c2d.png", path = paste0(main_path,"/plots and tables/plots"),
       width=12, height=12)





###########

datanomiss |>
  filter(!is.na(opennone)) |>
  ggplot(aes(x=opennone))+
  geom_bar(aes(fill=opennone))+
  xlab("")+
  ylab("Number of responses")+
  scale_x_discrete(labels = c("", "", "", ""))+
  scale_fill_discrete(name = "\"Why do you think you cannot\nsay anything about this person's\npolitical preferences?\"", 
                      labels = c("\"Their tastes are too common/generic,\nthey could mean anything\"\n",
                                 "\"A person's food tastes have\nno connection with their political views\"\n", 
                                 "\"I am not interested in knowing\nother people's political positions\"\n",
                                 "\"Other\n(specify in the comment)\"\n"))+
  scale_y_continuous(breaks = seq(0,310, by=20))+
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        #legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=13))
  

ggsave("Opennone.jpg", width = 7, height = 7)




#ALTERNATIVE VISUALIZATION H1A-H1D


ggplot(datanomiss, aes(x=expideology_r7))+
  geom_bar(aes(fill=expideology_r7))+
  facet_wrap(~group, nrow = 1)+
  xlab("Perceived ideology of the character")+
  scale_x_discrete(labels = c("", "", "", "", "", "", "", ""))+
  scale_fill_manual(name = "Ideology", values = c("Extreme-right" = "#0008AD",
                                                  "Right"= "#4988F0",
                                                   "Center-right"= "#8FEAFA",
                                                  "Center" = "grey",
                                                  "Center-left" = "#FF9179",
                                                  "Left" = "#F45533",
                                                  "Extreme-left" = "#AD3920",
                                                  "Don't know" = "black"))

ggsave("Distribution of ideological perception per experimental group.jpg",
       width = 7, height = 7)



ggplot(datanomiss, aes(x=expparty_r0))+
  geom_bar(aes(fill=expparty_r0))+
  facet_wrap(~group, nrow = 1)+
  xlab("Perceived ideology of the character")+
  scale_x_discrete(labels = c("", "", "", "", "", "", "", ""))+
  scale_fill_manual(name = "Ideology", values = c("Brothers of Italy" = "#284BF7",
                                                  "League"="#31F728",
                                                  "Go Italy"="#28F1F7",
                                                  "Action-Italy Alive" = "#F360FF",
                                                  "Five Star Movement" ="#FAFA2E",
                                                  "Democratic Party" = "#FA882E",
                                                  "Green-Left Alliance" = "#FB3317",
                                                  "Don't know" = "black"))

ggsave("Distribution of partisan perception per experimental group.jpg",
       width = 7, height = 7)

ggplot(datanomiss, aes(x=factor(ideology_PL), group=group))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)), stat = "count")+ 
  facet_wrap(~group, nrow = 1)+
  xlab("Ideological PL")+
  scale_fill_discrete(name="Ideology PL",
                      labels=c("Did not do PL", "Did PL"))+
  scale_x_discrete(labels = c("", ""))+
  scale_y_continuous(breaks = seq(0, 1, by=0.05))


ggsave("Distribution of ideological PL per experimental group.jpg",
       width = 7, height = 7)

ggplot(datanomiss, aes(x=factor(party_PL), group=group))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)), stat = "count")+ 
  facet_wrap(~group, nrow = 1)+
  xlab("Partisan PL")+
  scale_fill_discrete(name="Partisan PL",
                      labels=c("Did not do PL", "Did PL"))+
  scale_x_discrete(labels = c("", ""))+
  scale_y_continuous(breaks = seq(0, 1, by=0.05))

ggsave("Distribution of partisan PL per experimental group.jpg",
       width = 7, height = 7)



#######################################
########################
##### PISATI TABLE

###ORDER FOR IMAGE

datanomiss$expideology_r7 = factor(datanomiss$expideology_r7, 
                                   levels = c("Extreme-left",
                                              "Left",
                                              "Center-left",
                                              "Center",
                                              "Center-right",
                                              "Right",
                                              "Extreme-right", 
                                              "Don't know"))

datanomiss |>
  select(group, ideology_PL) |>
  group_by(group) |>
  summarise(n = n(),
            prop = sum(ideology_PL==1)/n)

datanomiss |>
  filter(group=="Control" & expideology_r7 != "Don't know") |>
  ggplot(aes(x=expideology_r7))+
  geom_bar(aes(y= (..count..)/sum(..count..),fill=expideology_r7), stat="count", show.legend = F)+
  xlab("")+
  ylab("")+
  scale_y_continuous(labels=scales::percent)+
  # scale_x_discrete(labels = c("", "", "", "", "", "", "", ""))+
  scale_fill_manual(name = "Ideology", values = c("Extreme-right" = "#0008AD",
                                                  "Right"= "#4988F0",
                                                   "Center-right"= "#8FEAFA",
                                                  "Center" = "grey",
                                                  "Center-left" = "#FF9179",
                                                  "Left" = "#F45533",
                                                  "Extreme-left" = "#AD3920",
                                                  "Don't know" = "black"))+
  theme(text = element_text(size=rel(7)))


ggsave("Distribution of ideological perception in Control group.jpg",
       width = 20, height = 6)



datanomiss |>
  filter(group=="Vegan" & expideology_r7 != "Don't know") |>
  ggplot(aes(x=expideology_r7))+
  geom_bar(aes(y= (..count..)/sum(..count..),fill=expideology_r7), stat="count", show.legend = F)+
  xlab("")+
  ylab("")+
  scale_y_continuous(labels=scales::percent)+
  # scale_x_discrete(labels = c("", "", "", "", "", "", "", ""))+
  scale_fill_manual(name = "Ideology", values = c("Extreme-right" = "#0008AD",
                                                  "Right"= "#4988F0",
                                                   "Center-right"= "#8FEAFA",
                                                  "Center" = "grey",
                                                  "Center-left" = "#FF9179",
                                                  "Left" = "#F45533",
                                                  "Extreme-left" = "#AD3920",
                                                  "Don't know" = "black"))+
  theme(text = element_text(size=rel(7)))

ggsave("Distribution of ideological perception in Vegan group.jpg",
       width = 20, height = 6)


datanomiss |>
  filter(group=="Ethnic" & expideology_r7 != "Don't know") |>
  ggplot(aes(x=expideology_r7))+
  geom_bar(aes(y= (..count..)/sum(..count..),fill=expideology_r7), stat="count", show.legend = F)+
  xlab("")+
  ylab("")+
  scale_y_continuous(labels=scales::percent)+
  # scale_x_discrete(labels = c("", "", "", "", "", "", "", ""))+
  scale_fill_manual(name = "Ideology", values = c("Extreme-right" = "#0008AD",
                                                  "Right"= "#4988F0",
                                                   "Center-right"= "#8FEAFA",
                                                  "Center" = "grey",
                                                  "Center-left" = "#FF9179",
                                                  "Left" = "#F45533",
                                                  "Extreme-left" = "#AD3920",
                                                  "Don't know" = "black"))+
  theme(text = element_text(size=rel(7)))

ggsave("Distribution of ideological perception in Ethnic group.jpg",
       width = 20, height = 6)


datanomiss |>
  filter(group=="Meat" & expideology_r7 != "Don't know") |>
  ggplot(aes(x=expideology_r7))+
  geom_bar(aes(y= (..count..)/sum(..count..),fill=expideology_r7), stat="count", show.legend = F)+
  xlab("")+
  ylab("")+
  scale_y_continuous(labels=scales::percent)+
  # scale_x_discrete(labels = c("", "", "", "", "", "", "", ""))+
  scale_fill_manual(name = "Ideology", values = c("Extreme-right" = "#0008AD",
                                                  "Right"= "#4988F0",
                                                   "Center-right"= "#8FEAFA",
                                                  "Center" = "grey",
                                                  "Center-left" = "#FF9179",
                                                  "Left" = "#F45533",
                                                  "Extreme-left" = "#AD3920",
                                                  "Don't know" = "black"))+
  theme(text = element_text(size=rel(7)))

ggsave("Distribution of ideological perception in Meat group.jpg",
       width = 20, height = 6)


datanomiss |>
  filter(group=="Typical" & expideology_r7 != "Don't know") |>
  ggplot(aes(x=expideology_r7))+
  geom_bar(aes(y= (..count..)/sum(..count..),fill=expideology_r7), stat="count", show.legend = F)+
  xlab("")+
  ylab("")+
  scale_y_continuous(labels=scales::percent)+
  # scale_x_discrete(labels = c("", "", "", "", "", "", "", ""))+
  scale_fill_manual(name = "Ideology", values = c("Extreme-right" = "#0008AD",
                                                  "Right"= "#4988F0",
                                                   "Center-right"= "#8FEAFA",
                                                  "Center" = "grey",
                                                  "Center-left" = "#FF9179",
                                                  "Left" = "#F45533",
                                                  "Extreme-left" = "#AD3920",
                                                  "Don't know" = "black"))+
  theme(text = element_text(size=rel(7)))

ggsave("Distribution of ideological perception in Traditional group.jpg",
       width = 20, height = 6)



datanomiss |>
  filter(expideology_r7 != "Don't know") |>
  ggplot(aes(x=expideology_r7))+
  geom_bar(aes(y= (..count..)/sum(..count..),fill=expideology_r7), stat="count", show.legend = F)+
  xlab("")+
  ylab("")+
  scale_y_continuous(labels=scales::percent)+
  # scale_x_discrete(labels = c("", "", "", "", "", "", "", ""))+
  scale_fill_manual(name = "Ideology", values = c("Extreme-right" = "#0008AD",
                                                  "Right"= "#4988F0",
                                                   "Center-right"= "#8FEAFA",
                                                  "Center" = "grey",
                                                  "Center-left" = "#FF9179",
                                                  "Left" = "#F45533",
                                                  "Extreme-left" = "#AD3920",
                                                  "Don't know" = "black"))+
  theme(text = element_text(size=rel(7)))

ggsave("Distribution of ideological perception in Overall group.jpg",
       width = 20, height = 6)






####

###############  PISATI TABLE WITH PARTIES (FOR APPENDIX OR THESIS)

####

datanomiss$expparty_r0 = factor(datanomiss$expparty_r0, 
                                levels = c("Green-Left Alliance",
                                           "Democratic Party",
                                           "Five Star Movement",
                                           "Action-Italy Alive",
                                           "Go Italy",
                                           "League",
                                           "Brothers of Italy",
                                           "Don't know"))



df_aus = datanomiss |>
  group_by(group, expparty_r0) |>
  summarise(nsubgroup = n())

i=1
df_aus$ngroup = 0
while(i<=nrow(df_aus))
{
  
  df_aus$ngroup[i] = nrow(datanomiss[datanomiss$group == df_aus$group[i], ])
  
  i=i+1
  
}

df_aus$subprop = round(df_aus$nsubgroup*100/df_aus$ngroup, digits = 2)


datanomiss |>
  filter(group=="Control" & expparty_r0 != "Don't know") |>
  ggplot(aes(x=expparty_r0))+
  geom_bar(aes(y= (..count..)/sum(..count..),fill=expparty_r0), stat="count", show.legend = F)+
  xlab("")+
  ylab("")+
  scale_x_discrete(labels = c("GLA","DP","FSM","A-IA","GI","L","BOI"))+
  scale_y_continuous(labels=scales::percent)+
  # scale_x_discrete(labels = c("", "", "", "", "", "", "", ""))+
  scale_fill_manual(name = "Party", values = c("Brothers of Italy" = "#284BF7",
                                               "League"="#31F728",
                                               "Go Italy"="#28F1F7",
                                               "Action-Italy Alive" = "#F360FF",
                                               "Five Star Movement" ="#FAFA2E",
                                               "Democratic Party" = "#FA882E",
                                               "Green-Left Alliance" = "#FB3317",
                                               "Don't know" = "black"))+
  theme(text = element_text(size=rel(7)))



ggsave("Distribution of partisan perception in Control group.jpg",
       width = 20, height = 6)

###

datanomiss |>
  filter(group=="Vegan" & expparty_r0 != "Don't know") |>
  ggplot(aes(x=expparty_r0))+
  geom_bar(aes(y= (..count..)/sum(..count..),fill=expparty_r0), stat="count", show.legend = F)+
  xlab("")+
  ylab("")+
  scale_x_discrete(labels = c("GLA","DP","FSM","A-IA","GI","L","BOI"))+
  scale_y_continuous(labels=scales::percent)+
  # scale_x_discrete(labels = c("", "", "", "", "", "", "", ""))+
  scale_fill_manual(name = "Party", values = c("Brothers of Italy" = "#284BF7",
                                               "League"="#31F728",
                                               "Go Italy"="#28F1F7",
                                               "Action-Italy Alive" = "#F360FF",
                                               "Five Star Movement" ="#FAFA2E",
                                               "Democratic Party" = "#FA882E",
                                               "Green-Left Alliance" = "#FB3317",
                                               "Don't know" = "black"))+
  theme(text = element_text(size=rel(7)))



ggsave("Distribution of partisan perception in Vegan group.jpg",
       width = 20, height = 6)

###

datanomiss |>
  filter(group=="Meat" & expparty_r0 != "Don't know") |>
  ggplot(aes(x=expparty_r0))+
  geom_bar(aes(y= (..count..)/sum(..count..),fill=expparty_r0), stat="count", show.legend = F)+
  xlab("")+
  ylab("")+
  scale_x_discrete(labels = c("GLA","DP","FSM","A-IA","GI","L","BOI"))+
  scale_y_continuous(labels=scales::percent)+
  # scale_x_discrete(labels = c("", "", "", "", "", "", "", ""))+
  scale_fill_manual(name = "Party", values = c("Brothers of Italy" = "#284BF7",
                                               "League"="#31F728",
                                               "Go Italy"="#28F1F7",
                                               "Action-Italy Alive" = "#F360FF",
                                               "Five Star Movement" ="#FAFA2E",
                                               "Democratic Party" = "#FA882E",
                                               "Green-Left Alliance" = "#FB3317",
                                               "Don't know" = "black"))+
  theme(text = element_text(size=rel(7)))



ggsave("Distribution of partisan perception in Meat group.jpg",
       width = 20, height = 6)

###

datanomiss |>
  filter(group=="Ethnic" & expparty_r0 != "Don't know") |>
  ggplot(aes(x=expparty_r0))+
  geom_bar(aes(y= (..count..)/sum(..count..),fill=expparty_r0), stat="count", show.legend = F)+
  xlab("")+
  ylab("")+
  scale_x_discrete(labels = c("GLA","DP","FSM","A-IA","GI","L","BOI"))+
  scale_y_continuous(labels=scales::percent)+
  # scale_x_discrete(labels = c("", "", "", "", "", "", "", ""))+
  scale_fill_manual(name = "Party", values = c("Brothers of Italy" = "#284BF7",
                                               "League"="#31F728",
                                               "Go Italy"="#28F1F7",
                                               "Action-Italy Alive" = "#F360FF",
                                               "Five Star Movement" ="#FAFA2E",
                                               "Democratic Party" = "#FA882E",
                                               "Green-Left Alliance" = "#FB3317",
                                               "Don't know" = "black"))+
  theme(text = element_text(size=rel(7)))



ggsave("Distribution of partisan perception in Ethnic group.jpg",
       width = 20, height = 6)


###

datanomiss |>
  filter(group=="Typical" & expparty_r0 != "Don't know") |>
  ggplot(aes(x=expparty_r0))+
  geom_bar(aes(y= (..count..)/sum(..count..),fill=expparty_r0), stat="count", show.legend = F)+
  xlab("")+
  ylab("")+
  scale_x_discrete(labels = c("GLA","DP","FSM","A-IA","GI","L","BOI"))+
  scale_y_continuous(labels=scales::percent)+
  # scale_x_discrete(labels = c("", "", "", "", "", "", "", ""))+
  scale_fill_manual(name = "Party", values = c("Brothers of Italy" = "#284BF7",
                                               "League"="#31F728",
                                               "Go Italy"="#28F1F7",
                                               "Action-Italy Alive" = "#F360FF",
                                               "Five Star Movement" ="#FAFA2E",
                                               "Democratic Party" = "#FA882E",
                                               "Green-Left Alliance" = "#FB3317",
                                               "Don't know" = "black"))+
  theme(text = element_text(size=rel(7)))



ggsave("Distribution of partisan perception in Traditional group.jpg",
       width = 20, height = 6)


###

datanomiss |>
  filter(expparty_r0 != "Don't know") |>
  ggplot(aes(x=expparty_r0))+
  geom_bar(aes(y= (..count..)/sum(..count..),fill=expparty_r0), stat="count", show.legend = F)+
  xlab("")+
  ylab("")+
  scale_x_discrete(labels = c("GLA","DP","FSM","A-IA","GI","L","BOI"))+
  scale_y_continuous(labels=scales::percent)+
  # scale_x_discrete(labels = c("", "", "", "", "", "", "", ""))+
  scale_fill_manual(name = "Party", values = c("Brothers of Italy" = "#284BF7",
                                               "League"="#31F728",
                                               "Go Italy"="#28F1F7",
                                               "Action-Italy Alive" = "#F360FF",
                                               "Five Star Movement" ="#FAFA2E",
                                               "Democratic Party" = "#FA882E",
                                               "Green-Left Alliance" = "#FB3317",
                                               "Don't know" = "black"))+
  theme(text = element_text(size=rel(7)))



ggsave("Distribution of partisan perception in Overall group.jpg",
       width = 20, height = 6)




















########### DO WE REALLY DISCRIMINATE FOR OUTPARTY?

data_LR = datanomiss |>
  filter(ideology_r5 == "Right" | ideology_r5 == "Left")

data_LR_PL = data_LR |>
  filter(!is.na(expideology_r1) & !(expideology_r1 == 0))


model4c <- polr(expbehav_r2ord ~ perceived_outgroup + educ_r + age_r+ 
                sex_r + macroarea2 + citysize_r2 + social_position_r_noNA +
                agreeableness + openness + neuroticism + conscientiousness + extraversion, data = data_LR_PL, method = "logistic")

summary(model4c)

a_int = effect("perceived_outgroup", model4c, xlevels = 2)

df_0_int = data.frame("outcome"= c("Unlikely", "Neither Likely nor Unlikely", "Likely"), "predprob" = a_int$prob[1, ], "lower" = a_int$lower.prob[1, ], "upper" = a_int$upper.prob[1, ], row.names = c(1,2,3)) 
df_1_int = data.frame("outcome"= c("Unlikely", "Neither Likely nor Unlikely", "Likely"), "predprob" = a_int$prob[2, ], "lower" = a_int$lower.prob[2, ], "upper" = a_int$upper.prob[2, ], row.names = c(1,2,3)) 


ordered_logit_AME_int = ggplot(data= df_0_int)+
  geom_pointrange(aes(x = outcome, 
                      y=predprob,
                      ymax=upper, 
                      ymin=lower),
                  col='red',
                  shape=17,
                  alpha = 1,
                  position = position_nudge(x = 1/20),
                  show.legend = F)+
  geom_pointrange(data = df_1_int,
                  aes(x = outcome, 
                      y=predprob,
                      ymax=upper, 
                      ymin=lower),
                  col='blue',
                  alpha = 1,
                  position = position_nudge(x = -1/20),
                  show.legend = F)+
  xlab("")+
  ylab("Predicted values for Interaction")+
 # labs(title = "Predicted probabilities for likelihood to interact\nIn-group vs Out-group")+
  scale_y_continuous(breaks = seq(0,0.8, by=0.1), limits = c(0,0.8))


ordered_logit_AME_int



#OPPURE
model4d <- polr(expconv_r2ord ~ perceived_outgroup + educ_r + age_r+ 
                 sex_r + macroarea2 + citysize_r2 + social_position_r_noNA+
                 agreeableness + openness + neuroticism + conscientiousness + extraversion, data = data_LR_PL, method = "logistic")

summary(model4d)

a_conv = effect("perceived_outgroup", model4d, xlevels = 2)

df_0_conv = data.frame("outcome"= c("Unlikely", "Neither Likely nor Unlikely", "Likely"), "predprob" = a_conv$prob[1, ], "lower" = a_conv$lower.prob[1, ], "upper" = a_conv$upper.prob[1, ], row.names = c(1,2,3)) 
df_1_conv = data.frame("outcome"= c("Unlikely", "Neither Likely nor Unlikely", "Likely"), "predprob" = a_conv$prob[2, ], "lower" = a_conv$lower.prob[2, ], "upper" = a_conv$upper.prob[2, ], row.names = c(1,2,3)) 


ordered_logit_AME_conv = ggplot(data= df_0_conv)+
  geom_pointrange(aes(x = outcome, 
                      y=predprob,
                      ymax=upper, 
                      ymin=lower),
                  col='red',
                  shape=17,
                  alpha = 1,
                  position = position_nudge(x = 1/20),
                  show.legend = F)+
  geom_pointrange(data = df_1_conv,
                  aes(x = outcome, 
                      y=predprob,
                      ymax=upper, 
                      ymin=lower),
                  col='blue',
                  alpha = 1,
                  position = position_nudge(x = -1/20),
                  show.legend = F)+
  xlab("")+
  ylab("Predicted values for Conversation")+
  labs(#title = "Predicted probabilities for likelihood to converse\nIn-group vs Out-group", 
       caption = "Red triangle = perceived ideological in-group\nBlue circle = perceived ideological out-group\n Predicted values for models 4c and 4d")+

    scale_y_continuous(breaks = seq(0,0.8, by=0.1), limits = c(0,0.8))


ordered_logit_AME_conv

ordered_logit_AME_int|ordered_logit_AME_conv

ggsave("Ordered Logit effects perceived outgroup full.png", path = paste0(main_path,"/plots and tables/plots"), width = 9, height = 6)

legend_info = data.frame("outcome" = c("Unlikely", "Unlikely"), "predprob" = c(0.8, 0.7),
                         "text" = c("Perc. In-group", "Perc. Out-group"))

ordered_logit_AME_int_SLIDES = ordered_logit_AME_int + scale_x_discrete(labels = c("Likely", "Neither Likely\nnor Unlikely", "Unlikely"))+
  theme(axis.text.x = element_text(face="bold", color="black",size=15),
        axis.text.y = element_text(face="bold", color="black",size=15))+
  geom_text(data = legend_info, aes(x=outcome,y=predprob, label=text), 
            fontface = "bold", size = 5,color=c("red", "blue"))

ordered_logit_AME_int_SLIDES  


ordered_logit_AME_conv_SLIDES = ordered_logit_AME_conv + scale_x_discrete(labels = c("Likely", "Neither Likely\nnor Unlikely", "Unlikely"))+
  theme(axis.text.x = element_text(face="bold", color="black",size=15),
        axis.text.y = element_text(face="bold", color="black",size=15))


ordered_logit_AME_int_SLIDES|ordered_logit_AME_conv_SLIDES

ggsave("Ordered Logit effects perceived outgroup fullSLIDES.png", path = paste0(main_path,"/plots and tables/plots"), width = 10, height = 6)
