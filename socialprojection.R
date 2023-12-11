#### Analyses on Andrea


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


setwd("C:/Users/gasca/OneDrive - Universit� degli Studi di Milano-Bicocca/Dottorato/Qualifying paper/Food for (political) thought/Analisi preliminari R")
data = import("FFPT_tidy.RDS")

datanomiss = data[!is.na(data$sex_r) & !is.na(data$citysize_r2) & !is.na(data$macroarea2), ]


#i eliminate the few data that creates missing values in the model


ggplot(data, aes(x=statideology))+
  geom_bar()

ggplot(data, aes(x=statparty))+
  geom_bar()

#Tabulation between responent's and character's perceived ideology
table(data$statideology_r4, data$ideology_r4)


#Do the left wingers recognize more Andrea as left wing 
#or do the right wingers?


chisq = chisq.test(table(data$statideology_r4, data$ideology_r4))

chisq$p.value
chisq$statistic
#Building a dataframe for residuals for each cell
dfaus = as.data.frame(chisq$residuals)

#rounding residuals for readability

dfaus$Freq = round(dfaus$Freq, 2)

#plotting residuals with heatmap

contrib <- 100*chisq$residuals^2/chisq$statistic

dfaus1 = as.data.frame(contrib)
dfaus$Freqperc = round(dfaus1$Freq, 2)
dfaus$observed = as.vector(chisq$observed)


heatmap1 =ggplot(dfaus, aes(Var1, Var2))+
  geom_raster(aes(fill = Freq), show.legend = F)+
  xlab("Perceived Ideology of Andrea")+
  ylab("Respondent's Ideology")+
  geom_text(aes(label = paste0(Freq, "\n(", Freqperc, "%)")), color = "white", size = 4) +
  scale_fill_continuous(name= "Pearson's \nresiduals")+
  theme(axis.title.x = element_text(#face="bold", color="black",
    size=15),
    axis.title.y = element_text(#face="bold", color="black",
      size=15),
    axis.text.x = element_text(#face="bold", color="black",
      size=15),
    axis.text.y = element_text( #face="bold", color="black",
      size=15))
#labs(title = "Pearson residuals for the Chi-square test\n of imputed ideology of the vignette (recoding 3)\n vs experimental group ")

heatmap1 

ggsave("Heatmap with evidence of social projection for andrea with poor recoding.png", scale = 5)


################################

#reordering of ideology_r2 for better table
data$ideology_r2 = factor(data$ideology_r2, levels = c("Left", "Center", "Right", "Nowhere"))

chisq = chisq.test(table(data$statideology_r4, data$ideology_r2))

chisq$p.value
chisq$statistic
#Building a dataframe for residuals for each cell
dfaus = as.data.frame(chisq$residuals)

as.vector(chisq$observed)

chisq$residuals

#rounding residuals for readability

dfaus$Freq = round(dfaus$Freq, 2)

#plotting residuals with heatmap

contrib <- 100*chisq$residuals^2/chisq$statistic

dfaus1 = as.data.frame(contrib)
dfaus$Freqperc = round(dfaus1$Freq, 2)

#I add the absolute frequency of each cell to the df

dfaus$observed = as.vector(chisq$observed)

dfaus

heatmap2 =ggplot(dfaus, aes(Var1, Var2))+
  geom_raster(aes(fill = Freq), show.legend = F)+
  xlab("Perceived Ideology of Andrea")+
  ylab("Respondent's Ideology")+
  geom_text(aes(label = paste0(Freq, "\n(", Freqperc, "%)\n", observed)), color = "white", size = 4) +
  scale_fill_continuous(name= "Pearson's \nresiduals")+
  theme(axis.title.x = element_text(#face="bold", color="black",
    size=15),
    axis.title.y = element_text(#face="bold", color="black",
      size=15),
    axis.text.x = element_text(#face="bold", color="black",
      size=15),
    axis.text.y = element_text( #face="bold", color="black",
      size=15))
#labs(title = "Pearson residuals for the Chi-square test\n of imputed ideology of the vignette (recoding 3)\n vs experimental group ")

heatmap2

# Questa heatmap è fantastica!!!!!

ggsave("Heatmap with evidence of social projection for andrea.png", width=8, height=8)



###################Social projection hypothesis...

###notice that social projection works only 
#for those who identify at least ideologically

#L'idea �: vegani di sinistra diranno piu'
#spesso che i vegani sono di sinistra
#E che i carnivori sono di destra (counter-proj)
 
#Ci chiediamo se nel nstro campione ci sia qualche 
#evidenza riguardo al fatto che queli di sinistra 
#sono pi� vegani di quelli di destra

ggplot(data, aes(x=ideology_r2, y=vegan_index))+
  geom_count()

ggplot(data, aes(x=ideology_r2, y=vegan_index))+
  geom_boxplot()

#il boxplot non funziona benissimo perch� variabile
#continua

data |> 
  count(ideology_r2, vegan_index) |>
ggplot(aes(x=ideology_r2, y=vegan_index))+
  geom_tile(aes(fill=n))

#Qualche evidenza c'è...


#Now i try doing a regression to see if there is a connection between the tendency to perceive
 #the character as politically similar and the soc_projected index, whichagain expresses how similar are the 
#respondent's tastes with the vignette's one. In order to do this, i remove those who do not
#self place ideologicallty, because if they do not, they cannot prceive any similarity_ideology_exp since
#we do not distinguish, in the answers, between "i don't know" and "he doesn't hold any political
# ideology, just like me!

datanomiss=data[data$ideology_SQ001 != "Da nessuna parte", ]

datanomiss = datanomiss[!is.na(datanomiss$sex_r) & !is.na(datanomiss$citysize_r2) & !is.na(datanomiss$macroarea2), ]

#Notice that i select only those with soc_prioìojected index >=0 because i modeles ads -1 those in
#the control group and those who didn't know enogh kind of food to compute an index among those from which i compute the
# soc projected index. Hence, i have to throw those away.

regr = glm(data=datanomiss[datanomiss$soc_projected_index >=0, ], similarity_ideology_exp ~ soc_projected_index, family=binomial(link="logit"))

summary(regr)

# OKAY, C'è EVIDENZA CHE LA SOCIAL PROJECTION SIA SIGNIFICATIVAMENTE ASSOCIATA
# CON LA POSSIBILITA' DI IDENTIFICARE UNA IDEOLOGIA SIMILE NEL PERSONAGGIO!

#Proviamo a fare analisi più approfondite...

#Facciamo anzitutto anche un linear probability model

regr = lm(data=datanomiss[datanomiss$soc_projected_index >=0, ], similarity_ideology_exp ~ soc_projected_index)

summary(regr)

#La regressione di cui sopra i dice: più a me piace il tipo di cibo che mangia il tizio 
#nella vignetta, più è probabile che io pensi che lui è politicamente simila a me 
# (abbiamo gli stessi gusti in fatto di cibo ---> li abbiamo anche in fatto di politica!)

#Adesso mettiamo un po' di controlli e vediamo che succede. 

regr = lm(data=datanomiss[datanomiss$soc_projected_index >=0, ], similarity_ideology_exp ~ soc_projected_index+ 
          sex_r+ educ_r+social_position_r+ age_r+ macroarea2 + conscientiousness +
            openness + extraversion + neuroticism + agreeableness+ income_r )

summary(regr)

#torniam al logit (provo anche probit)
regr = glm(data=datanomiss[datanomiss$soc_projected_index >=0, ], similarity_ideology_exp ~ soc_projected_index+ 
            sex_r+ educ_r+social_position_r+ age_r+ macroarea2 + conscientiousness +
            openness + extraversion + neuroticism + agreeableness+ income_r, family = binomial(link = "logit") )

summary(regr)

#Aggiunto ancora altri controlli

#torniam al logit (provo anche probit)

regr = glm(data=datanomiss[datanomiss$soc_projected_index >=0, ], similarity_ideology_exp ~ soc_projected_index+ 
             sex_r+ ideology_r2+exposure_r1+culturalindex+knowsparty+ educ_r+social_position_r+ age_r+ macroarea2 + conscientiousness +
             openness + extraversion + neuroticism + agreeableness+ income_r, 
           family = binomial(link = "logit") )

summary(regr)

#Io francamente non mi spiego il coefficiente di cultural index. If anything, penso che
# chi ha grande consumo cultural dovrebbe assumere di meno la somiglianza. Forse conffondo questa
#cosa con cultural omnivorousness?

#Secondo me devo discutere sta regressione con N



ggplot(datanomiss, aes(x=soc_projected_index))+
  geom_bar()

ggplot(datanomiss, aes(x=similarity_ideology_exp))+
  geom_bar()







#Purtroppo non tiene l'associazione conscientiousness-destra

#Proviamo a vedere con tabella a doppia entrata la relazione fra soc_projected index e similarity_ideology_exp

chisq = chisq.test(table(data$soc_projected_index, data$similarity_ideology_exp))

chisq$p.value
chisq$statistic
#Building a dataframe for residuals for each cell
dfaus = as.data.frame(chisq$residuals)

as.vector(chisq$observed)

chisq$residuals

#rounding residuals for readability

dfaus$Freq = round(dfaus$Freq, 2)

#plotting residuals with heatmap

contrib <- 100*chisq$residuals^2/chisq$statistic

dfaus1 = as.data.frame(contrib)
dfaus$Freqperc = round(dfaus1$Freq, 2)

#I add the absolute frequency of each cell to the df

dfaus$observed = as.vector(chisq$observed)

dfaus
heatmap3 =ggplot(dfaus, aes(Var1, Var2))+
  geom_raster(aes(fill = Freq), show.legend = F)+
  xlab("Social_projection_index")+
  ylab("Similarity of character's ideology with respondent's")+
  geom_text(aes(label = paste0(Freq, "\n(", Freqperc, "%)\n", observed)), color = "white", size = 4) +
  scale_fill_continuous(name= "Pearson's \nresiduals")
#labs(title = "Pearson residuals for the Chi-square test\n of imputed ideology of the vignette (recoding 3)\n vs experimental group ")

heatmap3

#probabilmente la roba migliore sono dei boxplot

ggplot(data, aes(x=factor(similarity_ideology_exp), y=soc_projected_index))+
  geom_boxplot()

#rimuoviamo quelli che hanno soc_projected_index= a -1

data |> 
  filter(soc_projected_index>=0) |>
  ggplot(aes(x=factor(similarity_ideology_exp), y=soc_projected_index))+
  geom_boxplot()+
  xlab("Similarity between character's and respondent's ideology")+
  ylab("Social projection index")

ggsave("Boxplot showing differences in social projection.png", scale=5)


data |> 
  filter(soc_projected_index>=0) |>
  ggplot(aes(x=factor(similarity_ideology_exp), y=soc_projected_index))+
  geom_violin()+
  xlab("Similarity between character's and respondent's ideology")+
  ylab("Social projection index")

ggsave("Boxplot showing differences in social projection.png", scale=5)


mean(data[data$similarity_ideology_exp==0, ]$soc_projected_index)
mean(data[data$similarity_ideology_exp==1, ]$soc_projected_index)

p1=data|>
  filter(similarity_ideology_exp==0 & soc_projected_index>=0) |>
  ggplot(aes(x=soc_projected_index))+
  geom_bar()


p2=data|>
  filter(similarity_ideology_exp==1 & soc_projected_index>=0) |>
  ggplot(aes(x=soc_projected_index))+
  geom_bar()

p1|p2
##### Nessuna di queste evidenze è schiacciante e bestiale, però vorrei comunque
# il parere della prof su come possiamo evidenziare meglio questa relazione perché
#comuque qualcosa qui c'è a supporto dell'ipotesi di social projection


#proverei anche a farla coi partiti sta roba.

#First of all, I consider that one could indeed identify with one
#or multiple parties only if they have a feeling thermometer rating
# over 5. Otherwise, we could have the opposite effect (counter-projection)
# or, likely, no effect. 

#I start by doing a cross tabulation between the respondent's favorite parties
#and the party they imputed andrea to be, just like I did with ideology

# I want to do a cross tabulation between the respondent's favorite party(ies)
#and the imputed party of andrea (and later the character). The idea here is
#to show social projection on parties. People who have avs as favorite party will
#tend to think that avs is andrea's favorite party more than people who's favorite
#party is the PD. The problem here is that people can have multiple favorite
#parties, and so the cross tabulation is not one to one, but one to many.
#For example, if one says that andrea votes avs and their favorite party is both 
#avs and pd, I have to count this person in both cells.

ggplot(data, aes(x=factor(is_max_avs), y=statparty))+
  geom_count()


round(prop.table(table(data$is_max_avs, data$statparty), margin = 1), digits = 3)
round(prop.table(table(data$is_max_pd, data$statparty), margin = 1), digits = 3)
round(prop.table(table(data$is_max_m5s, data$statparty), margin = 1), digits = 3)
round(prop.table(table(data$is_max_aziv, data$statparty), margin = 1), digits = 3)
round(prop.table(table(data$is_max_fi, data$statparty), margin = 1), digits = 3)
round(prop.table(table(data$is_max_lega, data$statparty), margin = 1), digits = 3)
round(prop.table(table(data$is_max_fdi, data$statparty), margin = 1), digits = 3)

#The idea here is that I want to reproduce heatmap2 but with partisanship
#The problem here is that the party they project is not univoquous

#I also need to select only those who score>5 in partisanship

data$max_party_r1=ifelse(data$max_rating<=5 | is.na(data$max_rating), "None", data$max_party_r)
#Now I create the table

data$max_party_r_list <- strsplit(as.character(data$max_party_r1), ", ")



# Create a data frame to store the cross-tabulation
cross_table <- data.frame()

# Iterate over each row in data
for (i in 1:nrow(data)) {
  andrea_party <- data$statparty_r3[i]
  respondent_parties <- data$max_party_r_list[[i]]
  
  # Create a row for each of my favorite parties
  for (party in respondent_parties) {
    row <- data.frame(statparty_r3 = andrea_party, max_party_r = party)
    cross_table <- rbind(cross_table, row)
  }
}

# Create the cross-tabulation using table()
result <- table(cross_table$statparty_r3, cross_table$max_party_r)
# Print the cross-tabulation
print(result)

#Madonna che fatica far sto codice qui sopra zio fa

dfaus = as.data.frame(result)

dfaus$Var1 = factor(dfaus$Var1, levels = c("Brothers of Italy",
                                           "League", "Go Italy",
                                           "Action-Italy Alive",
                                           "Five Star Movement",
                                           "Democratic Party",
                                           "Green-Left Alliance",
                                           "I don't know"))


dfaus$Var2 = factor(dfaus$Var2, levels = c("Brothers of Italy",
                                           "League", "Go Italy",
                                           "Action-Italy Alive",
                                           "Five Star Movement",
                                           "Democratic Party",
                                           "Green-Left Alliance",
                                           "None"))
heatmap4 =ggplot(dfaus, aes(x=Var1, y=Var2))+
  geom_raster(aes(fill = Freq), show.legend = F)+
  xlab("Perceived party of Andrea")+
  ylab("Respondent's Favorite party/ies")+
  geom_text(aes(label = Freq), color = "white", size = 4) +
  scale_fill_continuous(name= "")
#labs(title = "Pearson residuals for the Chi-square test\n of imputed ideology of the vignette (recoding 3)\n vs experimental group ")

heatmap4

#Questa heatmap non dò proprio delle evidenze schiaccianti, però secondo me ci
#si potrebbe tornare magari facendo delle percentuali relative, non so.
#Dipende anche quanto vogliamo investire su social projection...




#As we can see from the above tables, people who identify with a certain party 
#will always think more that andrea is indeed of the same party as them, even if
#the steretype would suggest otherwise...



#Reegressione tanto per per vedere se le classiche associazioni con big five 
#tengono anche nel mio sample
regr1 = lm(data=data, ideology_r ~ 
             conscientiousness +
             openness +
             extraversion + 
             neuroticism +
             agreeableness + 
             educ_r + 
             age_r2 +
             macroarea2+
             sex_r +
             social_position_r+
             job1_r)

summary(regr1)

####################### Heatmap like heatmap2 but with exp instead of stat

data$expideology_r6 = factor(data$expideology_r6, levels = c("Right", "Center", "Left", "Don't know"))

data$ideology_r2 = factor(data$ideology_r2, levels = c("Right", "Center", "Left", "Nowhere"))
table(data$expideology_r6)
chisq = chisq.test(table(data$expideology_r6, data$ideology_r2))

chisq$p.value
chisq$statistic
#Building a dataframe for residuals for each cell
dfaus = as.data.frame(chisq$residuals)

as.vector(chisq$observed)

chisq$residuals

#rounding residuals for readability

dfaus$Freq = round(dfaus$Freq, 2)

#plotting residuals with heatmap

contrib <- 100*chisq$residuals^2/chisq$statistic

dfaus1 = as.data.frame(contrib)
dfaus$Freqperc = round(dfaus1$Freq, 2)

#I add the absolute frequency of each cell to the df

dfaus$observed = as.vector(chisq$observed)

dfaus
heatmap5 =ggplot(dfaus, aes(Var1, Var2))+
  geom_raster(aes(fill = Freq), show.legend = F)+
  xlab("Perceived Ideology of the character")+
  ylab("Respondent's Ideology")+
  geom_text(aes(label = paste0(Freq, "\n(", Freqperc, "%)\n", observed)), color = "white", size = 4) +
  scale_fill_continuous(name= "Pearson's \nresiduals")+
  theme(axis.title.x = element_text(#face="bold", color="black",
    size=15),
    axis.title.y = element_text(#face="bold", color="black",
      size=15),
    axis.text.x = element_text(#face="bold", color="black",
                                   size=15),
    axis.text.y = element_text( #face="bold", color="black",
                                   size=15))
  
#labs(title = "Pearson residuals for the Chi-square test\n of imputed ideology of the vignette (recoding 3)\n vs experimental group ")

heatmap5

ggsave("Heatmap with evidence of social projection for character in vignette.png", width = 8, height = 8)

a=round(prop.table(table(data$ideology_r2, data$group), 1), digits=3)*100

write.xlsx(a, "table.xlsx")
