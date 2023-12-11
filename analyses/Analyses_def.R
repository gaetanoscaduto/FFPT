########## ANALYSES, DEF

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


setwd(getwd())
data = import("FFPT_tidy.RDS")

#i eliminate the few data that creates missing values in the model

datanomiss = data[!is.na(data$sex_r) & !is.na(data$citysize_r2) & !is.na(data$macroarea2), ]



###########################################################
################## H1a H1b, H1c, H1d #######################
############################################################


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

ggsave("Heatmap pearsons residuals.png", scale = 3)

# doing the same as before but with percentage contribution to chi squared
#statistics


heatmap2 = ggplot(dfaus1, aes(Var1, Var2))+
  geom_raster(aes(fill = Freq), show.legend = F)+
  geom_text(aes(label = paste0(Freq, "%")), color = "white", size = 4) +
  xlab("Perceived Ideology")+
  ylab("Experimental group")+
  scale_fill_continuous(name= "Percentage \ncontribution")

heatmap2

ggsave("Heatmap percentage contribution.png", scale = 3)

heatmap1+heatmap2

ggsave("Heatmap full.png", scale = 2)


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
ggsave("Heatmap pearsons residuals no grouping.png", scale = 5)


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
ggsave("Heatmap pearsons residuals parties.png", scale = 5)



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

ggsave("T-test, without NA.png",
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
  
ggsave("T-test, with NA as 0.png", scale=2)


p1|p2

#ggsave("T-test, full.png", width=8, height = 6)

ggsave("T-test, with NA as 0FORSLIDES.png", width = 9, height = 6)


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
                        


write.xlsx(tablewilcox, "tablewilcox.xlsx")













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
                    educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +
                    social_position_r_noNA,
                  family = binomial(link = "logit"))

summary(model1a)

model1b = glm(data=datanomiss, ideology_PL ~ 
                       ideology_r2 + exposure_r1 + culturalindex+
                       AP_wagner_spread2 + knowsparty+  interest_r+#politica
                       educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +
                       social_position_r_noNA,
                     family = binomial(link = "logit"))

summary(model1b)


#Second model: as before but with big fives

model2a = glm(data=datanomiss, party_PL ~ 
                    ideology_r2 + exposure_r1 + culturalindex+
                    AP_wagner_spread2 + knowsparty+ interest_r+#politica
                    educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +
                    social_position_r_noNA+
                    conscientiousness + openness + agreeableness +
                    extraversion + neuroticism,
                  family = binomial(link = "logit"))

summary(model2a)

model2b = glm(data=datanomiss, ideology_PL ~ 
                       ideology_r2 + exposure_r1 + culturalindex+
                       AP_wagner_spread2 + knowsparty+  interest_r+#politica
                       educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +
                       social_position_r_noNA+
                       conscientiousness + openness + agreeableness +
                       extraversion + neuroticism,
                     family = binomial(link = "logit"))

summary(model2b)


#Third model: As model 1 but with Collocated instead of Ideology


model3a = glm(data=datanomiss, party_PL ~ 
                    #polarizzazione
                    collocated + exposure_r1 + culturalindex+ #statbehav_r2+
                    AP_wagner_spread2 + knowsparty+ interest_r+#politica
                    educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +
                    social_position_r_noNA,
                  family = binomial(link = "logit"))
summary(model3a)

model3b = glm(data=datanomiss, ideology_PL ~ 
                  #polarizzazione
                    collocated + exposure_r1 + culturalindex+
                    AP_wagner_spread2 + knowsparty+  interest_r+#politica
                    educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +
                    social_position_r_noNA,
                  family = binomial(link = "logit"))

summary(model3b)





#Fourth model: As model 3 but with Big fives

model4a = glm(data=datanomiss, party_PL ~ 
                    collocated + exposure_r1 + culturalindex+
                    AP_wagner_spread2 + knowsparty+  interest_r+#politica
                    educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +
                    social_position_r_noNA+
                    conscientiousness + openness + agreeableness +
                    extraversion + neuroticism,
                  family = binomial(link = "logit"))

summary(model4a)

model4b = glm(data=datanomiss, ideology_PL ~ 
                       collocated + exposure_r1 + culturalindex+
                       AP_wagner_spread2 + knowsparty+ interest_r+#politica
                       educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +
                       social_position_r_noNA+
                       conscientiousness + openness + agreeableness +
                       extraversion + neuroticism,
                     family = binomial(link = "logit"))

summary(model4b)


################  THE PISATI MODELS (INDEPENDENT VARIABLES SEPARATED)

model4aH2 = glm(data=datanomiss, party_PL ~ 
                collocated + knowsparty+  interest_r+#politica
                educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +
                social_position_r_noNA+
                conscientiousness + openness + agreeableness +
                extraversion + neuroticism,
              family = binomial(link = "logit"))

summary(model4aH2)

model4bH2 = glm(data=datanomiss, ideology_PL ~ 
                collocated + knowsparty+ interest_r+#politica
                educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +
                social_position_r_noNA+
                conscientiousness + openness + agreeableness +
                extraversion + neuroticism,
              family = binomial(link = "logit"))

summary(model4bH2)

model4aH3 = glm(data=datanomiss, party_PL ~ 
                AP_wagner_spread2 + knowsparty+  interest_r+#politica
                educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +
                social_position_r_noNA+
                conscientiousness + openness + agreeableness +
                extraversion + neuroticism,
              family = binomial(link = "logit"))

summary(model4aH3)

model4bH3 = glm(data=datanomiss, ideology_PL ~ 
                AP_wagner_spread2 + knowsparty+ interest_r+#politica
                educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +
                social_position_r_noNA+
                conscientiousness + openness + agreeableness +
                extraversion + neuroticism,
              family = binomial(link = "logit"))

summary(model4bH3)

model4aH4 = glm(data=datanomiss, party_PL ~ 
                exposure_r1 + knowsparty+  interest_r+#politica
                educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +
                social_position_r_noNA+
                conscientiousness + openness + agreeableness +
                extraversion + neuroticism,
              family = binomial(link = "logit"))

summary(model4aH4)

model4bH4 = glm(data=datanomiss, ideology_PL ~ 
                exposure_r1 + knowsparty+ interest_r+#politica
                educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +
                social_position_r_noNA+
                conscientiousness + openness + agreeableness +
                extraversion + neuroticism,
              family = binomial(link = "logit"))

summary(model4bH4)

model4aH5 = glm(data=datanomiss, party_PL ~ 
                culturalindex+  knowsparty+  interest_r+#politica
                educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +
                social_position_r_noNA+
                conscientiousness + openness + agreeableness +
                extraversion + neuroticism,
              family = binomial(link = "logit"))

summary(model4aH5)

model4bH5 = glm(data=datanomiss, ideology_PL ~ 
                culturalindex+ knowsparty+ interest_r+#politica
                educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +
                social_position_r_noNA+
                conscientiousness + openness + agreeableness +
                extraversion + neuroticism,
              family = binomial(link = "logit"))

summary(model4bH5)

controlmodelasociodemo = glm(data=datanomiss, party_PL ~ 
                  educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +
                  social_position_r_noNA+
                  conscientiousness + openness + agreeableness +
                  extraversion + neuroticism,
                family = binomial(link = "logit"))

summary(controlmodelasociodemo)

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

model1c = glm(data=datanomiss, interaction~ 
                      PL_full+
                      #politica
                      educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +    
                      social_position_r_noNA,  family = binomial(link = "logit"))

summary(model1c)


#### model2 PL+sociodemo+psicodemo

model2c = glm(data=datanomiss, interaction~ 
                      PL_full+
                      #politica
                      ideology_r2+
                      educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +    
                      social_position_r_noNA+
                      conscientiousness + openness + agreeableness +
                      extraversion + neuroticism, family = binomial(link = "logit"))

summary(model2c)


### model 3 (all - the same as model 3 before) 
# this goes in the appendix


model3c = glm(data=datanomiss, interaction~ 
                      PL_full+
                      AP_wagner_spread2+
                      #polarizzazione
                      interest_r + exposure_r1 + culturalindex+ideology_r2+
                      knowsparty+
                      #politica
                      educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +    
                      social_position_r_noNA+
                      conscientiousness + openness + agreeableness +
                      extraversion + neuroticism, family = binomial(link = "logit"))

summary(model3c)

# 
# model4ctest = glm(data=datanomiss, interaction~ 
#                 #PL_full+
#                 AP_wagner_spread2+
#                 #polarizzazione
#                 
#                 #politica
#                 
#                 educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +    
#                 social_position_r_noNA+
#                 conscientiousness + openness + agreeableness +
#                 extraversion + neuroticism)
# 
# lrtest(model3c, model4ctest)


######## questa è una storia CLAMOROSA secondo me però nota che sia nell'interazione
# 0 1 che nell'interazione 1 0 ci son trenta casi


#H7
####### 


model1d = glm(data=datanomiss, conversation~ 
                     PL_full+
                      #politica
                      educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +    
                      social_position_r_noNA, family = binomial(link = "logit"))

summary(model1d)

#### model2 PL+sociodemo+psicodemo

model2d = glm(data=datanomiss, conversation~ 
                     PL_full+
                      #politica
                     ideology_r2+
                      educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +    
                      social_position_r_noNA+
                      conscientiousness + openness + agreeableness +
                      extraversion + neuroticism,  family = binomial(link = "logit"))

summary(model2d)


### model 3 (all - the same as model 3 before)
# this goes in the appendix



model3d = glm(data=datanomiss, conversation~ 
                     PL_full+
                     AP_wagner_spread2+
                      #polarizzazione
                       interest_r + exposure_r1 + culturalindex+ideology_r2+
                      knowsparty+
                      #politica
                      educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +    
                      social_position_r_noNA+
                      conscientiousness + openness + agreeableness +
                      extraversion + neuroticism, family = binomial(link = "logit"))

summary(model3d)

########### try ordered logit

try <- polr(data=datanomiss, expbehav_r~ 
            friendPL+
            #politica
            educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +    
            social_position_r_noNA, Hess=TRUE)


summary(try)

#results are not that good. I should come back to it....












######## Export model results########################

##Party PL

wordreg(list(model1a, model2a, model3a, model4a), file = "PartyPL.docx",
        single.row = T,
        # custom.coef.names = names,
        custom.model.names = c("Model 1a", "Model 2a", "Model 3a", "Model 4a"),
        caption = "Partisan PL")

## Ideology PL

wordreg(list(model1b, model2b, model3b, model4b), file = "IdeologyPL.docx",
        single.row = T,
        #custom.coef.names = names,
        custom.model.names = c("Model 1b", "Model 2b", "Model 3b", "Model 4b"),
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






#############################################
# 
# IN APPENDICE AGGIUNGERE AL QUARTO MODELLO QUALCOSA SUL NETWORK (KNOWS), QUALCOSA SUL LAVORO
# (JOBS RECODED), VOLENDO POSSO RIFARE I MODELLI CON INCOME AL POSTO DI CLASSE SOCIALE
# LO SCOPO è FAR VEDERE CHE IL COEFFICIENTE REGGE BOTTA 
# 
# ############# 


########## Predicted values for specific cathegory of people



junkie = data.frame(collocated = "Collocated", exposure_r1="High",  culturalindex=4,
           AP_wagner_spread2= mean(datanomiss$AP_wagner_spread),
           knowsparty=7, interest_r="High", educ_r="High", 
           age_r2="Adult", sex_r="Female", macroarea2="Center",
           citysize_r2="Big", social_position_r_noNA=7, 
           conscientiousness=mean(datanomiss$conscientiousness),
           openness=mean(datanomiss$openness), 
           agreeableness=mean(datanomiss$agreeableness),
           extraversion=mean(datanomiss$extraversion),
           neuroticism=mean(datanomiss$neuroticism))
predict(model4a, junkie,
        type = "response")

dontgiveafuck = data.frame(collocated = "Not collocated", exposure_r1="Low",
                    culturalindex=0,
                    AP_wagner_spread2= mean(datanomiss$AP_wagner_spread),
                    knowsparty=0, interest_r="Low", educ_r="Low", 
                    age_r2="Adult", sex_r="Female", macroarea2="Center",
                    citysize_r2="Small", social_position_r_noNA=3, 
                    conscientiousness=mean(datanomiss$conscientiousness),
                    openness=mean(datanomiss$openness), 
                    agreeableness=mean(datanomiss$agreeableness),
                    extraversion=mean(datanomiss$extraversion),
                    neuroticism=mean(datanomiss$neuroticism))

predict(model4a, dontgiveafuck,
        type = "response")

#### 
plinker = data.frame(PL_full="Both PL",
                       ideology_r2 ="Center",
                       educ_r= "High", age_r2 ="Adult",
                       sex_r ="Male",
                       macroarea2 = "Center",
                       citysize_r2 ="Big",    
                       social_position_r_noNA = mean(data$social_position_r_noNA),
                     conscientiousness=mean(datanomiss$conscientiousness),
                     openness=mean(datanomiss$openness), 
                     agreeableness=mean(datanomiss$agreeableness),
                     extraversion=mean(datanomiss$extraversion),
                     neuroticism=mean(datanomiss$neuroticism))

plinker = data.frame(PL_full="Both PL",
                     ideology_r2 ="Center",
                     educ_r= "High", age_r2 ="Adult",
                     sex_r ="Male",
                     macroarea2 = "Center",
                     citysize_r2 ="Big",    
                     social_position_r_noNA = mean(data$social_position_r_noNA),
                     conscientiousness=mean(datanomiss$conscientiousness),
                     openness=mean(datanomiss$openness), 
                     agreeableness=mean(datanomiss$agreeableness),
                     extraversion=mean(datanomiss$extraversion),
                     neuroticism=mean(datanomiss$neuroticism))

notplinker = data.frame(PL_full="No PL",
                     ideology_r2 ="Center",
                     educ_r= "High", age_r2 ="Adult",
                     sex_r ="Male",
                     macroarea2 = "Center",
                     citysize_r2 ="Big",    
                     social_position_r_noNA = mean(data$social_position_r_noNA),
                     conscientiousness=mean(datanomiss$conscientiousness),
                     openness=mean(datanomiss$openness), 
                     agreeableness=mean(datanomiss$agreeableness),
                     extraversion=mean(datanomiss$extraversion),
                     neuroticism=mean(datanomiss$neuroticism))

predict(model2c, plinker, type = "response")

predict(model2c, notplinker, type = "response")

predict(model2d, plinker, type = "response")

predict(model2d, notplinker, type = "response")

##############################################################################
##################################REGRESSION GRAFICS##########################
###############################################################################

#### Average marginal effects (library(margins))

mara=margins(model4a)
marb=margins(model4b)

summary(mara)

summary(mara)$factor


#Now i compute a graph with the effect of all the IV on DV, but i arrange it
#dividing psychological, sociodemo and political variables

##################################

# fare unico grafico con iideology e party pl 







summary(mara)



###################################

psociodemo = ggplot(summary(mara)[c(1,2,5,9,14,15,18, 19), ])+
  geom_pointrange(aes(x = factor, 
                      y=AME,
                      ymax=AME+1.96*SE, 
                      ymin=AME-1.96*SE),
                  col='red',
                  alpha = 1,
                  position = position_nudge(x = 1/10),
                  show.legend = F)+
  geom_pointrange(data = summary(marb)[c(1,2,5,9,14,15,18,19), ],
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
                     c("Age>60\n (ref. 35-60)", "Age<35\n (ref. 35-60)","Big City", "Degree", "North \n(ref.Center)", "South \n(ref. Center)", "Female", "Social\nposition"))+
  xlab("")+
  ylab("AME")+
  #ylim(-0.31, 0.31)+
  labs(caption = "AMEs for model 4a and 4b\nRed  = Partisan PL, Blue  = Ideological PL, 95% C.I.")+
# theme(text = element_text(size=rel(4)))+
  coord_flip()

psociodemo


summary(mara)$factor

ppsico = ggplot(summary(mara)[c(3,7,11,16,17), ])+
  geom_pointrange(aes(x = factor, 
                      y=AME,
                      ymax=AME+1.96*SE, 
                      ymin=AME-1.96*SE),
                      col='red',
                      alpha = 1,
                      position = position_nudge(x = 1/10),
                      show.legend = F
                      )+
  geom_pointrange(data = summary(marb)[c(3,7,11,16,17), ],
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
  scale_x_discrete(labels = c("Agreeab.", "Consc.", "Extrav.", "Neur.", "Open."))+
  xlab("")+
  ylab("AME")+
  #ylim(-0.31, 0.31)+
# theme(text = element_text(size=rel(4)))
  coord_flip()

ppsico


summary(mara)$factor

ppoli = ggplot(summary(mara)[c(4,6,8,10,12,13), ])+
  geom_pointrange(aes(x = factor, 
                      y=AME,
                      ymax=AME+1.96*SE, 
                      ymin=AME-1.96*SE),
                  col='red',
                  alpha = 1,
                  position = position_nudge(x = 1/10),
                  show.legend = F)+
  geom_pointrange(data = summary(marb)[c(4,6,8,10,12,13), ],
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


(ppoli/ppsico)|(psociodemo)

ggsave("AME Model4a and 4b.png", 
       width = 8, height = 10)

ggsave("AME Model4a and 4bFORSLIDES.png", width = 10, height = 6)


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
  labs(caption = "AMEs for Models 5a,5b,6a,6b,7a,7b,8a,8b.\nRed= Partisan PL, Blue  = Ideological PL, 95% C.I.")+
   #theme(text = element_text(size=rel(5)))+
  coord_flip()

ggsave("AME Pisati Independent Models.png", 
       width = 8, height = 4)






################### LI PRESENTO PER CATEGORIWE (INTERESE SOCIODEMO-PSICOMETRICHE E COSì VIA
# è PIU' CARINO)

### Predictive margins per le variabili indipendenti continue, poi arrange  a 4

#### Average marginal effects (library(margins))

pred1 =cplot(model4a, "culturalindex", what = "prediction")
pred2 =cplot(model4a, "AP_wagner_spread2", what = "prediction")
pred3 =cplot(model4a, "social_position_r_noNA", what = "prediction")
pred4 =cplot(model4a, "openness", what = "prediction")

pred6 =cplot(model4b, "culturalindex", what = "prediction")
pred7 =cplot(model4b, "AP_wagner_spread2", what = "prediction")
pred8 =cplot(model4b, "social_position_r_noNA", what = "prediction")
pred9 =cplot(model4b, "openness", what = "prediction")


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
#ggsave("PredProbCIPartyPL.png",scale=2)




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
#ggsave("PredProbAPPartyPL.png", scale=2)


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
#ggsave("PredProbSPPartyPL.png", scale=2)

aux= datanomiss |>
  group_by(openness) |>
  summarise(num = n(),
            rel = num/nrow(datanomiss))

p4=ggplot(pred4, aes(x = xvals)) + 
  geom_line(aes(y = yvals)) +
  geom_smooth(aes(y = upper), linetype = 2) +
  geom_smooth(aes(y = lower), linetype = 2) +
  #ggtitle("Predicted probabilities of Party PL vs \n values openness to new experiences") +
  #geom_hline(yintercept=0)+
  geom_col(data = aux, aes(x=openness, y=rel),
               alpha = 1/2, fill ='grey', col='grey')+
  xlab("Openness") +
  ylab("Predicted probabilities")+
  labs(caption = "Values from model 4a\nDistribution of the variable is plotted in overlay, 95% C.I.")+
  ylim(0,1)

p4
#ggsave("PredProbOPPartyPL.png", scale=2)

(p1+p2)/(p3+p4)#+labs(caption = "AMEs for model 4a and 4b\nRed  = Partisan PL, Blue  = Ideological PL\n 95% C.I.")

ggsave("Predicted probabilities, continuous variables, MODEL4A.png", 
       width = 8, height = 10)

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
#ggsave("PredProbCIPartyPL.png",scale=2)
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
#ggsave("PredProbAPPartyPL.png", scale=2

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
#ggsave("PredProbSPPartyPL.png", scale=2)

aux= datanomiss |>
  group_by(openness) |>
  summarise(num = n(),
            rel = num/nrow(datanomiss))

p9=ggplot(pred9, aes(x = xvals)) + 
  geom_line(aes(y = yvals)) +
  geom_smooth(aes(y = upper), linetype = 2) +
  geom_smooth(aes(y = lower), linetype = 2) +
  #ggtitle("Predicted probabilities of Party PL vs \n values openness to new experiences") +
  #geom_hline(yintercept=0)+
  geom_col(data = aux, aes(x=openness, y=rel),
           alpha = 1/2, fill ='grey', col='grey')+
  xlab("Openness") +
  labs(caption = "Values from model 4b\nDistribution of the variable is plotted in overlay, 95% C.I.")+
  ylab("Predicted probabilities")+
  ylim(0,1)

p9
#ggsave("PredProbOPPartyPL.png", scale=2)

(p6+p7)/(p8+p9)

ggsave("Predicted probabilities, continuous variables, MODEL4B.png", 
       width = 8, height = 10)


################## AME FOR H6 and H7 (Models 2c and 2d)

marc=margins(model2c)
mard=margins(model2d)

summary(marc)
summary(mard)
summary(marc)$factor
summary(mard)$factor



psociodemo = ggplot(summary(marc)[c(1,2,4,6,11,12,18,19), ])+
  geom_pointrange(aes(x = factor, 
                      y=AME,
                      ymax=AME+1.96*SE, 
                      ymin=AME-1.96*SE),
                  col='purple',
                  alpha = 1,
                  position = position_nudge(x = 1/10),
                  show.legend = F)+
  geom_pointrange(data = summary(mard)[c(1,2,4,6,11,12,18,19), ],
                  aes(x = factor, 
                      y=AME,
                      ymax=AME+1.96*SE, 
                      ymin=AME-1.96*SE),
                  col='orange',
                  shape = 17,
                  alpha = 1,
                  position = position_nudge(x = -1/10),
                  show.legend = F)+
  geom_hline(yintercept = 0, col='black', alpha = 1/2)+
  scale_x_discrete(labels = 
                     c("Age>60\n (ref. 35-60)", 
                       "Age<35\n (ref. 35-60)",
                       "Big City", "Degree", "North \n(ref.Center)",
                       "South \n(ref. Center)", "Female", 
                       "Social\nposition"))+
  xlab("")+
  ylab("AME")+
  #ylim(-0.31, 0.31)+
  labs(caption = "AMEs for model 2c and 2d\nPurple  = Interaction, Orange  = Conversation, 95% C.I.")+
  coord_flip()
  theme(text = element_text(size=rel(4)))

psociodemo


summary(marc)$factor

ppsico = ggplot(summary(marc)[c(3,5,7,13,14), ])+
  geom_pointrange(aes(x = factor, 
                      y=AME,
                      ymax=AME+1.96*SE, 
                      ymin=AME-1.96*SE),
                  col='purple',
                  alpha = 1,
                  position = position_nudge(x = 1/10),
                  show.legend = F
  )+
  geom_pointrange(data = summary(mard)[c(3,5,7,13,14), ],
                  aes(x = factor, 
                      y=AME,
                      ymax=AME+1.96*SE, 
                      ymin=AME-1.96*SE),
                  col='orange',
                  shape = 17,
                  alpha = 1,
                  position = position_nudge(x = -1/10),
                  show.legend = F)+
  geom_hline(yintercept = 0, col='black', alpha = 1/2)+
  scale_x_discrete(labels = c("Agreeab.", "Consc.", "Extrav.", "Neur.", "Open."))+
  xlab("")+
  ylab("AME")+
  #ylim(-0.31, 0.31)+
  coord_flip()
  #theme(text = element_text(size=rel(4)))

ppsico



summary(marc)$factor



ppoli = ggplot(summary(marc)[c(8,9,10,15,16,17), ])+
  geom_pointrange(aes(x = factor, 
                      y=AME,
                      ymax=AME+1.96*SE, 
                      ymin=AME-1.96*SE),
                  col='purple',
                  alpha = 1,
                  
                  position = position_nudge(x = 1/10),
                  show.legend = F)+
  geom_pointrange(data = summary(mard)[c(8,9,10,15,16,17), ],
                  aes(x = factor, 
                      y=AME,
                      ymax=AME+1.96*SE, 
                      ymin=AME-1.96*SE),
                  col='orange',
                  shape = 17,
                  alpha = 1,
                  position = position_nudge(x = -1/10),
                  show.legend = F)+
  geom_hline(yintercept = 0, col='black', alpha = 1/2)+
 scale_x_discrete(labels = c("Left\n(ref. Center)", "Nowhere\n(ref. Center)",
                             "Right\n(ref. Center)","Both PL\n(ref. No PL)", 
                             "Only id. PL\n(ref. No PL)",
                             "Only part. PL\n(ref. No PL)" 
                            ))+
  xlab("")+
 ylab("AME")+
  #ylim(-0.42, 0.42)+
  coord_flip()
  theme(text = element_text(size=rel(4)))

ppoli

(ppoli/ppsico)|psociodemo

ggsave("AME Model2c and 2d.png",  
       width = 8, height = 10)

ggsave("AME Model2c and 2dFORSLIDES.png",  width = 10, height = 6)






############## Predictive probabilities for numeric significant variables
# in models 2c and 2d (Agreeableness and Conscientiousness)

pred10 =cplot(model2c, "agreeableness", what = "prediction")
pred11 =cplot(model2c, "conscientiousness", what = "prediction")
pred12 =cplot(model2d, "agreeableness", what = "prediction")
pred13 =cplot(model2d, "conscientiousness", what = "prediction")


aux= datanomiss |>
  group_by(agreeableness) |>
  summarise(num = n(),
            rel = num/nrow(datanomiss))

p10=ggplot(pred10, aes(x = xvals)) + 
  geom_line(aes(y = yvals)) +
  geom_smooth(aes(y = upper), linetype = 2) +
  geom_smooth(aes(y = lower), linetype = 2) +
  #ggtitle("Predicted probabilities of Party PL vs \n values of cultural participation index") +
  #geom_hline(yintercept=0)+
  geom_col(data = aux, aes(x=agreeableness, y=rel),
           alpha = 1/2, fill ='grey', col='grey')+
  xlab("Agreeableness") +
  ylab("Pred. Prob Interaction=1")+
  ylim(0,1)

p10
#ggsave("PredProbCIPartyPL.png",scale=2)
### Predictive margins per AP_wagner_spread2

aux= datanomiss |>
  group_by(conscientiousness) |>
  summarise(num = n(),
            rel = num/nrow(datanomiss))

p11=ggplot(pred11, aes(x = xvals)) + 
  geom_line(aes(y = yvals)) +
  geom_smooth(aes(y = upper), linetype = 2) +
  geom_smooth(aes(y = lower), linetype = 2) +
  #ggtitle("Predicted probabilities of Party PL vs \n values of cultural participation index") +
  #geom_hline(yintercept=0)+
  geom_col(data = aux, aes(x=conscientiousness, y=rel),
           alpha = 1/2, fill ='grey', col='grey')+
  xlab("Conscientiousness") +
  ylab("Pred. Prob Interaction=1")+
  ylim(0,1)

p11
#ggsave("PredProbAPPartyPL.png", scale=2

### Predictive margins per social_position_r_noNA

aux= datanomiss |>
  group_by(agreeableness) |>
  summarise(num = n(),
            rel = num/nrow(datanomiss))

p12=ggplot(pred12, aes(x = xvals)) + 
  geom_line(aes(y = yvals)) +
  geom_smooth(aes(y = upper), linetype = 2) +
  geom_smooth(aes(y = lower), linetype = 2) +
  #ggtitle("Predicted probabilities of Party PL vs \n values of cultural participation index") +
  #geom_hline(yintercept=0)+
  geom_col(data = aux, aes(x=agreeableness, y=rel),
           alpha = 1/2, fill ='grey', col='grey')+
  xlab("Agreeableness") +
  ylab("Pred. Prob Conversation=1")+
  ylim(0,1)

p12
#ggsave("PredProbSPPartyPL.png", scale=2)


aux= datanomiss |>
  group_by(conscientiousness) |>
  summarise(num = n(),
            rel = num/nrow(datanomiss))

p13=ggplot(pred13, aes(x = xvals)) + 
  geom_line(aes(y = yvals)) +
  geom_smooth(aes(y = upper), linetype = 2) +
  geom_smooth(aes(y = lower), linetype = 2) +
  #ggtitle("Predicted probabilities of Party PL vs \n values of cultural participation index") +
  #geom_hline(yintercept=0)+
  geom_col(data = aux, aes(x=conscientiousness, y=rel),
           alpha = 1/2, fill ='grey', col='grey')+
  xlab("Conscientiousness") +
  ylab("Pred. Prob Conversation=1")+
  ylim(0,1)

p13

#ggsave("PredProbOPPartyPL.png", scale=2)

(p10+p11)/(p12+p13)

ggsave("Predicted probabilities, numeric variables, MODELs2c2d.png",
       width=8, height=10)




########Why is female significant for interaction

summary(lm(data=datanomiss, expbehav_r1 ~ sex))

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
                                 "\"Other\n(specify in the comment)\"\n"))

ggsave("Opennone.jpg", width = 7, height = 7)




#ALTERNATIVE VISUALIZATION H1A-H1D


ggplot(datanomiss, aes(x=expideology_r7))+
  geom_bar(aes(fill=expideology_r7))+
  facet_wrap(~group, nrow = 1)+
  xlab("Perceived ideology of the character")+
  scale_x_discrete(labels = c("", "", "", "", "", "", "", ""))+
  scale_fill_manual(name = "Ideology", values = c("Extreme-right" = "#AD3920",
                                "Right"="#F45533",
                                "Center-right"="#FF9179",
                                "Center" = "grey",
                                "Center-left" ="#8FEAFA",
                                "Left" = "#4988F0",
                                "Extreme-left" = "#0008AD",
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
  scale_fill_manual(name = "Ideology", values = c("Extreme-right" = "#AD3920",
                                                  "Right"="#F45533",
                                                  "Center-right"="#FF9179",
                                                  "Center" = "grey",
                                                  "Center-left" ="#8FEAFA",
                                                  "Left" = "#4988F0",
                                                  "Extreme-left" = "#0008AD",
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
  scale_fill_manual(name = "Ideology", values = c("Extreme-right" = "#AD3920",
                                                  "Right"="#F45533",
                                                  "Center-right"="#FF9179",
                                                  "Center" = "grey",
                                                  "Center-left" ="#8FEAFA",
                                                  "Left" = "#4988F0",
                                                  "Extreme-left" = "#0008AD",
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
  scale_fill_manual(name = "Ideology", values = c("Extreme-right" = "#AD3920",
                                                  "Right"="#F45533",
                                                  "Center-right"="#FF9179",
                                                  "Center" = "grey",
                                                  "Center-left" ="#8FEAFA",
                                                  "Left" = "#4988F0",
                                                  "Extreme-left" = "#0008AD",
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
  scale_fill_manual(name = "Ideology", values = c("Extreme-right" = "#AD3920",
                                                  "Right"="#F45533",
                                                  "Center-right"="#FF9179",
                                                  "Center" = "grey",
                                                  "Center-left" ="#8FEAFA",
                                                  "Left" = "#4988F0",
                                                  "Extreme-left" = "#0008AD",
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
  scale_fill_manual(name = "Ideology", values = c("Extreme-right" = "#AD3920",
                                                  "Right"="#F45533",
                                                  "Center-right"="#FF9179",
                                                  "Center" = "grey",
                                                  "Center-left" ="#8FEAFA",
                                                  "Left" = "#4988F0",
                                                  "Extreme-left" = "#0008AD",
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
  scale_fill_manual(name = "Ideology", values = c("Extreme-right" = "#AD3920",
                                                  "Right"="#F45533",
                                                  "Center-right"="#FF9179",
                                                  "Center" = "grey",
                                                  "Center-left" ="#8FEAFA",
                                                  "Left" = "#4988F0",
                                                  "Extreme-left" = "#0008AD",
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



datanomiss |>
  select(group, party_PL) |>
  group_by(group) |>
  summarise(n = n(),
            prop = sum(party_PL==1)/n)


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


















# PROVARE A STIMARE UN PO' THeORY DRIVEN' I CHANGES IN PREDICTIVE PROBICTIVE PROBABILITIES
# (PREDICTIVE MARGINS) DI UNA SERIE DI SOGGETTI CHE FAREBBERO PIù STEREOTIPI. IN STATA GLI DIRESTI
# GFAMMI MARGINS QUINDI TIPO GLI AT SU UNA SERIE DI DEMOGRAFICHE FACENDO VARIARE AP
# QUINDI IO VI FACCIO VEDERE COMECAMBIA LA PROBABILITà DI EFFETTUARE PL PER UNA CERTO 
# TIPO DI SOGGETTO. QUELLO CHE VARIA è SEMPRE LA AP PERò DA UN PO' L'IDEA E AIUTA
# NELL'INTERPRETAZIONE '


#### let's see the change in PL for a subjec that is a old, south, big city, 
# collocated, highly news-exposed and highly cultural-exposed person

# mar2 = margins(model4a, at = liPst(exposure_r1 = c("High", "Low")))
#                # collocated = "Collocated",
#                                       culturalindex = 3, citysize_r2 = "Big",
#                                       social_position_r_noNA = 10, 
#                                       openness = 0, macroarea2 = "South", 
#                                       age_r2 = "Old", educ_r = "High"))













############################################
# 
# negli altri modelli 1) sostituisco collocated con dxsxcxnone
# 
# faccio anche un modello snza psicometriche 
# 
# 
# quindi di base metto sempre aff, interest, exposure, cultural + sociodemo di base
# 
# poi nei vari modelli ci aggiungo:
#   nel secondo aggiungo ideologia dxsxcx
#   nel terzo collocated al posto di ideologia
#   nel quarto aggiungo le psicometriche

#############################################
# 
# IN APPENDICE AGGIUNGERE AL QUARTO MODELLO QUALCOSA SUL NETWORK (KNOWS), QUALCOSA SUL LAVORO
# (JOBS RECODED), VOLENDO POSSO RIFARE I MODELLI CON INCOME AL POSTO DI CLASSE SOCIALE
# LO SCOPO è FAR VEDERE CHE IL COEFFICIENTE REGGE BOTTA 
# 
# 
#################################################
# 
# Queste sono le prime analisi. POi si potrebbero fare dei focus con degli incrocio sulla ase della collocazione
# ideologica del soggetto e il treatment che aveva avuto. Ulteriore sviluppo proviamo a vedere sulle domande behavioral
# (caffè e conversazione sperimentali io mi aspetto che non vogliano prendere il caffè)
# Quindi mi interessa sapere le conseguenze del pl sul behavioral affective polarization e sulla aspettativa della conversazione
# Quindi la affective polarization mi rientra come outcome

#####################################################

#Prepara bene i modelli per Baldassarri e vedi se le cose di network le devi mettere


### NOTA: SE NELLA REGRESSIONE SOPRA SI SOSTITUISCE AP_WAGNER_SPREAD_noNA AD 
# AP_WAGNER_SPREAD2 SI PERDE leggermentissimamente
# IN SIGNFICATIVITA' MA FORSE SI GUADAGNA IN COERENZA
# QUESTA COSA DEBBO DISCUTERLA BENE!!!!!!

############### TRASH



##### Fourth model: I add psychometrics (and keep collocated)






datanomiss |>
  filter(group=="Vegan" & expparty_r0 != "Don't know") |>
  ggplot(aes(x=expparty_r0))+
  geom_bar(aes(y= (..count..)/sum(..count..),fill=expparty_r0), stat="count", show.legend = F)+
  xlab("")+
  ylab("")+
  scale_x_discrete(labels = c("AVS","PD","M5S","AZ-IV","FI","LEGA","FDI"))+
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



ggsave("Instagram.jpg",
       width = 20, height = 6)

mean(data[data$ideology_r2=="Left", ]$AP_wagner_spread2)

mean(data[data$ideology_r2=="Right", ]$AP_wagner_spread2)

mean(data[data$ideology_r2=="Center", ]$AP_wagner_spread2)

mean(data[data$ideology_r2=="Nowhere", ]$AP_wagner_spread2)



t.test(data[data$ideology_r2=="Left", ]$AP_wagner_spread2,
       data[data$ideology_r2=="Right", ]$AP_wagner_spread2)

regr = lm(data = data, AP_wagner_spread2 ~ factor(ideology_r2, levels = c("Right", "Left", "Center", "Nowhere")))

summary(regr)







# 
# model1c = glm(data=datanomiss, interaction~ 
#                       party_PL+ knowsparty +#polarizzazione
#                       interest_r + exposure_r1 + culturalindex+collocated+
#                       #politica
#                       educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +    
#                       social_position_r_noNA+
#                       conscientiousness + openness + agreeableness +
#                       extraversion + neuroticism)
# 
# summary(model1c)
# 
# #the model for behav ap with expideology
# 
# model2c = glm(data=datanomiss, interaction~ 
#                       ideology_PL+ knowsparty +#polarizzazione
#                       interest_r + exposure_r1 + culturalindex+collocated+
#                       #politica
#                       educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +    
#                       social_position_r_noNA+
#                       conscientiousness + openness + agreeableness +
#                       extraversion + neuroticism)
# 
# summary(model2c)
# 
# #the model for behav ap with expparty and expideology
# 
# model3c = glm(data=datanomiss, interaction~ 
#                      PL_full+
#                       AP_wagner_spread2+
#                       #polarizzazione
#                       interest_r + exposure_r1 + culturalindex+collocated+
#                       #politica
#                       educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +    
#                       social_position_r_noNA+
#                       conscientiousness + openness + agreeableness +
#                       extraversion + neuroticism)
# 
# summary(model3c)
# 
# # ha senso metterle entrambe? Sono molto correlate e mi danno risultati 
# #poco carini...
# 
# 
# ####### NOTE: the story here is: it is the ideology who is commanding,
# # not partisanship! But with the interaction it's more invformative!!!
# 
# # note that party_PL and exppideology_r2 have a correlation of .88 
# #(run summary(lm(data=data, party_PL ~ideology_PL))).
# #It's not enough for multicollinearity but discuss it with prof
# 
# 
# ###################################### Like those above but with conversation
# 
# ###the model for conv ap with expparty
# model1d = glm(data=datanomiss, conversation~ 
#                      party_PL+ knowsparty +#polarizzazione
#                      interest_r + exposure_r1 + culturalindex+collocated+
#                      AP_wagner_spread2+
#                      #politica
#                      educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +    
#                      social_position_r_noNA+
#                      conscientiousness + openness + agreeableness +
#                      extraversion + neuroticism)
# 
# summary(model1d)
# 
# #the model for conv ap with expideology
# 
# model2d = glm(data=datanomiss, conversation~ 
#                      ideology_PL+ knowsparty +#polarizzazione
#                      interest_r + exposure_r1 + culturalindex+collocated+
#                      AP_wagner_spread2+
#                      #politica
#                      educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +    
#                      social_position_r_noNA+
#                      conscientiousness + openness + agreeableness +
#                      extraversion + neuroticism)
# 
# summary(model2d)
# 
# #the model for conv ap with expparty and expideology
# 
# # ha senso metterle entrambe? Sono molto correlate e mi danno risultati poco carini...
# 
# 
# model3d = glm(data=datanomiss, conversation~ 
#                     PL_full +
#                      AP_wagner_spread2+knowsparty+
#                      #polarizzazione
#                      interest_r + exposure_r1 + culturalindex+collocated+
#                      #politica
#                      educ_r + age_r2+ sex_r + macroarea2 + citysize_r2 +    
#                      social_position_r_noNA+
#                      conscientiousness + openness + agreeableness +
#                      extraversion + neuroticism)
# 
# summary(model3d)

####################OLD PLOTS

# 
# ppoli = ggplot(summary(mara)[c(4,6,8,10,12,13), ], aes(x = factor, y=AME))+
#   geom_point()+
#   geom_pointrange(aes(ymax=AME+1.96*SE, ymin=AME-1.96*SE))+
#   geom_hline(yintercept = 0, col='red')+
#   #labs(title = "Average Marginal Effects")+
#   scale_x_discrete(labels = c("Aff. Pol.", "Collocated", "Cult. cons.", "News \nexposure", "Interest", "Party \nKnowledge"))+
#   xlab("")+
#   ylab("AME")+
#   coord_flip()
# 
# 
# ppoli



# 
# psociodemo = ggplot(summary(mara)[c(1,2,5,9,14,15,18, 19), ], aes(x = factor, y=AME))+
#   geom_point()+
#   geom_pointrange(aes(ymax=AME+1.96*SE, ymin=AME-1.96*SE))+
#   geom_hline(yintercept = 0, col='red')+
#   #labs(title = "Average Marginal Effects for Party PL")+
#   scale_x_discrete(labels = 
#                      c("Age>60\n (ref. 35-60)", "Age<35\n (ref. 35-60)","Big City", "Degree", "North \n(ref.Center)", "South \n(ref. Center)", "Female", "Social position"))+
#   xlab("")+
#   ylab("AME")+
#   coord_flip()
# 
# psociodemo
# 
# 
# 
# psociodemo = ggplot(summary(marb)[c(1,2,5,9,13,14,17,18), ], aes(x = factor, y=AME))+
#   geom_point()+
#   geom_pointrange(aes(ymax=AME+1.96*SE, ymin=AME-1.96*SE))+
#   geom_hline(yintercept = 0, col='red')+
#   #labs(title = "Average Marginal Effects for Party PL")+
#   scale_x_discrete(labels = c("Age>60", "Age<35","Big City", "Degree", "North (ref.Center)", "South (ref. Center)", "Female", "Social position"))+
#   xlab("")+
#   ylab("AME")
# 
# psociodemo
# ppsico = ggplot(summary(marb)[c(3,7,11,15,16), ], aes(x = factor, y=AME))+
#   geom_point()+
#   geom_pointrange(aes(ymax=AME+1.96*SE, ymin=AME-1.96*SE))+
#   geom_hline(yintercept = 0, col='red')+
#   #labs(title = "Average Marginal Effects")+
#   scale_x_discrete(labels = c("Agreeab.", "Consc.", "Extrav.", "Neur.", "Open."))+
#   xlab("")+
#   ylab("AME")
# 
# ppsico
# 
# 
# ppoli = ggplot(summary(marb)[c(4,6,8,10,12,13), ], aes(x = factor, y=AME))+
#   geom_point()+
#   geom_pointrange(aes(ymax=AME+1.96*SE, ymin=AME-1.96*SE))+
#   geom_hline(yintercept = 0, col='red')+
#   #labs(title = "Average Marginal Effects")+
#   scale_x_discrete(labels = c("Aff. Pol.", "Collocated", "Cult. cons.", "News \nexposure", "Interest", "Party \nKnowledge"))+
#   xlab("")+
#   ylab("AME")
# 
# 
# ppoli
# 
# (ppoli+ppsico)/(psociodemo)
# 
# ggsave("AME - Ideology PL.png", scale=5)

