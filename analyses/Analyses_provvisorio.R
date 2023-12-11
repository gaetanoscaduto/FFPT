########## ANALISI
library(rio)
library(ggplot2)
library(margins)
library(dplyr)
library(corrplot)
library(texreg)
library(patchwork)
library(lmtest)

setwd(getwd())
data = import("FFPT_tidy.RDS")



###########################################################
################## H1a H1b, H1c, H1d ###################################
############################################################

table(data$expideology, data$group)
round(prop.table(table(data$expideology, data$group), margin = 2), digits=2)

ggplot(data, aes(x=group))+
  geom_bar(aes(fill = expideology))


ggplot(data, aes(x=group))+
  geom_bar(aes(fill = expideology_r6), position="fill")

### tolgo i non sa

data |> 
  filter(expideology_r6 != "Non sa") |>
  ggplot(aes(x=group))+
  geom_bar(aes(fill = expideology_r6), position = "fill")

### con le sottocategorie e senza i non sa
data |> 
  filter(expideology != "Non saprei") |>
    ggplot(aes(x=group))+
      geom_bar(aes(fill = expideology))


### Let's make a chi square test
# http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r


chisq = chisq.test(table(data$expideology_r6, data$group))

chisq$statistic

chisq$p.value

round(chisq$expected, 2)
chisq$observed

round(chisq$residuals, 3)


# dfperplot = as.data.frame(chisq$residuals)
# names(dfperplot) = c("expideology", "group", "PearsRes")
#ggplot(dfperplot, aes(x=group, y=expideology))+
#  geom_tile(aes(fill=PearsRes))+
# scale_fill_gradient2(low = "#075AFF",
#                     mid = "#FFFFCC",
#                  high = "#FF0000") +
# labs(title = "Pearson residuals for each cell")

corrplot(chisq$residuals, method="number", is.cor = FALSE,
         title = "Person residuals for each cell")

contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)

corrplot(contrib, method = "number", is.cor = FALSE,
         title = "Percentage contribution \n
         for the Chi-squared statistic for each cell")


####### for appendix: no grouping in ideology

chisq1 = chisq.test(table(data$expideology, data$group))

corrplot(chisq1$residuals, method="number", is.cor = FALSE)

contrib1 <- 100*chisq1$residuals^2/chisq$statistic

corrplot(contrib1, method = "number", is.cor = FALSE)


############# t tests with "doesn't know" as missing

tcontr= t.test(data[data$group=="Control", ]$expideology_r1, conf.level = 0.90)

tveg= t.test(data[data$group=="Vegan", ]$expideology_r1,  conf.level = 0.99)

tmeat= t.test(data[data$group=="Meat", ]$expideology_r1,  conf.level = 0.99)

ttyp= t.test(data[data$group=="Typical", ]$expideology_r1,  conf.level = 0.99)

tethn = t.test(data[data$group=="Ethnic", ]$expideology_r1, conf.level = 0.99)

tcontr$conf.int

tdataset = data.frame(group = c("Control", "Vegan", "Meat", "Typical", "Ethnic"),
                      mean = c(tcontr$estimate, tveg$estimate, tmeat$estimate, ttyp$estimate, tethn$estimate),
                      upper = c(tcontr$conf.int[1], tveg$conf.int[1], tmeat$conf.int[1], ttyp$conf.int[1], tethn$conf.int[1]),
                      lower = c(tcontr$conf.int[2], tveg$conf.int[2], tmeat$conf.int[2], ttyp$conf.int[2], tethn$conf.int[2]))

ggplot(tdataset, aes(x=mean, y=group))+
  #geom_point()+
  geom_pointrange(aes(xmax=upper, xmin=lower), col="red")+
  geom_vline(xintercept = 0)+
  xlim(-1.5, 1.5)+
  labs(title="T-test results regarding the perceived ideology \nof the character for each experimental group",
       caption ="Extreme Left =-3, Left=-2, Center-Left=-1, Center=0, \nCenter-right=1, Right=2, Extreme right=3\n 99% confidence intervals")

ggsave("T-test, without NA.png", scale=3)


####### t-tests with "doesn't know" as 0

############# t tests with "doesn't know" as missing

tcontr= t.test(data[data$group=="Control", ]$expideology_r5, conf.level = 0.99)

tveg= t.test(data[data$group=="Vegan", ]$expideology_r5,  conf.level = 0.99)

tmeat= t.test(data[data$group=="Meat", ]$expideology_r5,  conf.level = 0.99)

ttyp= t.test(data[data$group=="Typical", ]$expideology_r5,  conf.level = 0.99)

tethn = t.test(data[data$group=="Ethnic", ]$expideology_r5, conf.level = 0.99)


tdataset = data.frame(group = c("Control", "Vegan", "Meat", "Typical", "Ethnic"),
                      mean = c(tcontr$estimate, tveg$estimate, tmeat$estimate, ttyp$estimate, tethn$estimate),
                      upper = c(tcontr$conf.int[1], tveg$conf.int[1], tmeat$conf.int[1], ttyp$conf.int[1], tethn$conf.int[1]),
                      lower = c(tcontr$conf.int[2], tveg$conf.int[2], tmeat$conf.int[2], ttyp$conf.int[2], tethn$conf.int[2]))

ggplot(tdataset, aes(x=mean, y=group))+
  #geom_point()+
  geom_pointrange(aes(xmax=upper, xmin=lower), col="red")+
  geom_vline(xintercept = 0)+
  xlim(-1.5, 1.5)+
  labs(title="T-test results regarding the perceived ideology \nof the character for each experimental group",
       caption ="Extreme Left =-3, Left=-2, Center-Left=-1, Center=0, \nCenter-right=1, Right=2, Extreme right=3")

ggsave("T-test, with NA as 0.png", scale=3)






########################################################
######################## REGRESSION MODELS #############
########################################################

# adesso via di logit

regr2 = glm(data=data, expideology_r2 ~ 
              AP_wagner_spread2 + statbehav_r+ expbehav_r+ #polarizzazione
              pol_soph +perceived_disagreement + perceived_heterogeneity+
              interest_r + exposure_r1 + #politica
              educ_r + age_r + sex_r + income_r+macroarea2+culturalindex, #controlli
             family = binomial(link = "logit"))

summary(regr2)

#### aggiungiamo citysize e social positon (coi missing imputati, altrimenti
# usare variabile social_position_r al posto di social_position_r_noNA)

regr3 = glm(data=data, expideology_r2 ~ 
              AP_wagner_spread2 + statbehav_r+ expbehav_r+ #polarizzazione
              pol_soph + perceived_disagreement + perceived_heterogeneity+
              interest_r + exposure_r2 + #politica
              educ_r + age_r + sex_r +income_r + macroarea2 + 
              citysize_r+culturalindex + social_position_r_noNA, #controlli
            family = binomial(link = "logit"))

summary(regr3)

#lrtest(regr3, regr2)

#nota che perceived disagreement ed eterogeneity introducono tanti missing e non
# sono significative. Per questo decido di rimuoverle

regr4 = glm(data=data, expideology_r2 ~ 
              AP_wagner_spread2 + statbehav_r+ expbehav_r+ #polarizzazione
              pol_soph + interest_r + exposure_r1 + #politica
              educ_r + age_r + sex_r + macroarea2 + 
              citysize_r+culturalindex + social_position_r_noNA, #controlli
            family = binomial(link = "logit"))

summary(regr4)


#nota che senza culturalindex social position sarebbe significativa

# aggiungiamo le psicometriche e togliamo statbehav che tanto secondo me non contribuisce molto

regr5 = glm(data=data, expideology_r2 ~ 
              AP_wagner_spread2 + expbehav_r+ #polarizzazione
              pol_soph + interest_r + exposure_r1 + #politica
              educ_r + age_r + sex_r + macroarea2 + citysize_r + 
              social_position_r_noNA + culturalindex+ #controlli
              openness + conscientiousness + extraversion +
              neuroticism + agreeableness, #bigfive
            family = binomial(link = "logit"))

summary(regr5)

lrtest(regr4,regr5)

#  Facciamo un po' di altre prove


### sole psicometriche e controlli

regrpsycho = glm(data=data, expideology_r2 ~
                   openness + conscientiousness + extraversion +
                   neuroticism + agreeableness + educ_r + age_r + sex_r + 
                   income_r + macroarea2 + citysize_r + social_position_r_noNA,
                         family = binomial(link = "logit"))

summary(regrpsycho)



### Aggiungiamo quelle di conversazione

regr6 = glm(data=data, expideology_r2 ~ 
              AP_wagner_spread2 + expbehav_r+ 
              expconv_r + statconv_r+
              pol_soph + interest_r + exposure_r2 + educ_r + age_r + sex_r + 
              macroarea2 + citysize_r + social_position_r_noNA +
              culturalindex+
              openness + conscientiousness + extraversion + neuroticism + agreeableness,
            family = binomial(link = "logit"))

summary(regr6)

lrtest(regr5,regr6)

# non contribuiscono molto,  posso rimuoverle alla luce del lrtest

## aggiungiamo l'ideologia (con la ricodifica r2 
# (dx-cx-sx-none) per non perdere informazione)

data$ideology_r2 = factor(data$ideology_r2, levels = c("Da nessuna parte", "Destra","Sinistra"))

regr7 = glm(data=data, expideology_r2 ~ 
              AP_wagner_spread2 + expbehav_r+ #polarizzazione
              pol_soph  + interest_r + exposure_r2 + ideology_r2+#politica
              educ_r + age_r + sex_r + macroarea2 + citysize_r + 
              social_position_r_noNA + culturalindex+ #controlli
              openness + conscientiousness + extraversion +
              neuroticism + agreeableness, #bigfive
            family = binomial(link = "logit"))

summary(regr7)


lrtest(regr5, regr7)

# nota che la polarizzazione affettiva era significativa prima
#che mettessimo l'ideologia (perchè probabilmente sono correlate)


## Facciamo la stessa di prima senza bigfive



regr7nobig5 = glm(data=data, expideology_r2 ~ 
              AP_wagner_spread2 + expbehav_r+ #polarizzazione
              pol_soph  + interest_r + exposure_r2 + ideology_r2+#politica
              educ_r + age_r + sex_r + macroarea2 + citysize_r + 
              social_position_r_noNA + culturalindex, #controlli
            family = binomial(link = "logit"))

summary(regr7nobig5)


# sembra che l'estremismo possa funzionare... (resta sto problema dei "Da nessuna parte".
# Ma gli scienziati politici come fanno? Imputano?). Intanto aggiungo l'estremismo


regr8 = glm(data=data, expideology_r2 ~ 
              AP_wagner_spread2 + expbehav_r+ #polarizzazione
              pol_soph + interest_r + exposure_r2 + ideology_r2+ extremism+#politica
              educ_r + age_r + sex_r + macroarea2 + citysize_r + 
              social_position_r_noNA + culturalindex, #controlli
              #openness + conscientiousness + extraversion +
              #neuroticism + agreeableness, #bigfive
            family = binomial(link = "logit"))

summary(regr8)

#no, non funziona
#### aggiungiamo group un attimo

regr9 = glm(data=data, expideology_r2 ~ 
              AP_wagner_spread2 + expbehav_r+ #polarizzazione
              pol_soph + interest_r + exposure_r2 + ideology_r2+ extremism+#politica
              educ_r + age_r + sex_r + macroarea2 + citysize_r + 
              social_position_r_noNA + culturalindex+randomnumber, #controlli
            #openness + conscientiousness + extraversion +
            #neuroticism + agreeableness, #bigfive
            family = binomial(link = "logit"))

summary(regr9)


# messo così non sembra funzionare, lo tolgo...


# proviamo ad usare la variabile "collocated" (0 se il rispondente non si colloca 
# nel righello ideologico, 1 altrimenti) al posto di estremismo e ideologia


regr11 = glm(data=data, expideology_r2 ~ 
               AP_wagner_spread2 + expbehav_r+ #polarizzazione
               pol_soph + interest_r + exposure_r1 + collocated+#politica
               educ_r + age_r + sex_r + macroarea2 + citysize_r + 
               social_position_r_noNA + culturalindex+ #controlli
             openness + conscientiousness,# + extraversion +
             #neuroticism + agreeableness, #bigfive
             family = binomial(link = "logit"))

summary(regr11)


# Collocated sembra funzionare benissimo. Ulteriore segnale del fatto che 
# quelli che si collocano "da nessuna parte" sono molto diversi da quelli che 
# si collocano nel righello


#Adesso voglio vedere se per esempio c'è un'interazione tra collocato ed estremismo



regr12 = glm(data=data, expideology_r2 ~ 
               AP_wagner_spread2 + expbehav_r+ #polarizzazione
               pol_soph + interest_r + exposure_r1 + collocated+extremism+#politica
               interaction(collocated, extremism)+
               educ_r + age_r + sex_r + macroarea2 + citysize_r + 
               social_position_r_noNA + culturalindex+ #controlli
              openness + conscientiousness,# + extraversion +
             #neuroticism + agreeableness, #bigfive
             family = binomial(link = "logit"))

summary(regr12)

lrtest(regr11, regr12)
# No, non funziona. 


### rinunciamo a Wagner AP per un attimo 

regr13 = glm(data=data, expideology_r2 ~ 
                expbehav_r+ #polarizzazione
               pol_soph + interest_r + exposure_r1 + collocated+extremism+#politica
               educ_r + age_r + sex_r + macroarea2 + citysize_r + 
               social_position_r_noNA + culturalindex+ #controlli
               openness + conscientiousness,
             # + extraversion + neuroticism + agreeableness, #bigfive
             family = binomial(link = "logit"))

summary(regr13)

## nota che con tutte e cinque le misure di bigfive cambiano un po' le cose.
#Non saprei dire perché (guarda regr successiva)


regr14 = glm(data=data, expideology_r2 ~ 
               expbehav_r+ #polarizzazione
               pol_soph + interest_r + exposure_r1 + collocated+extremism+#politica
               educ_r + age_r + sex_r + macroarea2 + citysize_r + 
               social_position_r_noNA + culturalindex+ #controlli
             openness + conscientiousness + extraversion +
             neuroticism + agreeableness, #bigfive
             family = binomial(link = "logit"))

summary(regr14)

lrtest(regr13, regr14) 
#rinunciamo a polsoph ed extremism che tanto non sono mai significativi

regr15 = glm(data=data, expideology_r2 ~ 
               expbehav_r+ #polarizzazione
               interest_r + exposure_r1 + collocated+#politica
               educ_r + age_r + sex_r + macroarea2 + citysize_r + 
               social_position_r_noNA + culturalindex+ #controlli
             openness + conscientiousness,# + extraversion +
             #neuroticism + agreeableness, #bigfive
             family = binomial(link = "logit"))

summary(regr15)

lrtest(regr13, regr15)
#proviam a rimettere tutte le psico e togliamo quei sette missing (perché sennò
# la funzione cplot mi rompe le scatole più tardi )

datanomiss = data[!is.na(data$sex_r) & !is.na(data$citysize_r) & !is.na(data$macroarea2), ]

regr16 = glm(data=datanomiss, expideology_r2 ~ 
               expbehav_r+ #polarizzazione
               interest_r + exposure_r1 + collocated+#politica
               educ_r + age_r + sex_r + macroarea2 + citysize_r + 
               social_position_r_noNA 
              + culturalindex+ #controlli
             openness + conscientiousness + extraversion +
             neuroticism + agreeableness, #bigfive
             family = binomial(link = "logit"))

summary(regr16)

#dovrei anche provare expbehav al quadrato perché magari l'effetto non è lineare

data$expbehav_r2 = (data$expbehav_r)^2

regr16 = glm(data=datanomiss, expideology_r2 ~ 
               expbehav_r + expbehav_r2+ #polarizzazione
               interest_r + exposure_r1 + collocated+#politica
               educ_r + age_r + sex_r + macroarea2 + citysize_r + 
               social_position_r_noNA 
             + culturalindex+ #controlli
               openness + conscientiousness + extraversion +
               neuroticism + agreeableness, #bigfive
             family = binomial(link = "logit"))

summary(regr16)

# no okay non serve metterlo al quadrato

regr16 = glm(data=datanomiss, expideology_r2 ~ 
               expbehav_r +#polarizzazione
               interest_r + exposure_r1 + collocated+#politica
               educ_r + age_r + sex_r + macroarea2 + citysize_r + 
               social_position_r_noNA 
             + culturalindex+ #controlli
               openness + conscientiousness + extraversion +
               neuroticism + agreeableness, #bigfive
             family = binomial(link = "logit"))

summary(regr16)

### Proviamo a farla PURE COI PARTITI!

regr16 = glm(data=datanomiss, expparty_r2 ~ 
               expbehav_r +#polarizzazione
               interest_r + exposure_r1 + collocated+#politica
               educ_r + age_r + sex_r + macroarea2 + citysize_r + 
               social_position_r_noNA 
             + culturalindex+ #controlli
               openness + conscientiousness + extraversion +
               neuroticism + agreeableness, #bigfive
             family = binomial(link = "logit"))

summary(regr16)

# nota che:

table(data$expparty_r2, data$expideology_r2)

# nota anche che:

table(data$expideology, data$expparty)

round(prop.table(table(data$expideology, data$expparty), margin = 1), digits=2)
round(prop.table(table(data$expideology, data$expparty), margin = 2), digits=2)

# carino che i 5 stelle siano spesso considerati al centro!
### se in questa regressione provi ad aggiungere expconv come predittore non è significativo



#################### facciamo un test di functional form misspecification? 
#Non saprei come far per logistiche, chiedi a prof


### Mettiamo le variabili di Andrea?


regr16b = glm(data=data, expparty_r2 ~ 
               expbehav_r + statbehav_r + statconv_r +
               statideology_r2+#polarizzazione
               interest_r + exposure_r1 + collocated+#politica
               educ_r + age_r + sex_r + macroarea2 + citysize_r + 
               social_position_r_noNA 
             + culturalindex+ #controlli
               openness + conscientiousness + extraversion +
               neuroticism + agreeableness, #bigfive
             family = binomial(link = "logit"))

summary(regr16b)

#### si però grazie al cavolo che statideology_r2 è significativa. Spiego le 
# mele con le mele (rimuovo)

regr16b = glm(data=data, expparty_r2 ~ 
                expbehav_r + statbehav_r + statconv_r +
                #polarizzazione
                interest_r + exposure_r1 + collocated+#politica
                educ_r + age_r + sex_r + macroarea2 + citysize_r + 
                social_position_r_noNA 
              + culturalindex+ #controlli
                openness + conscientiousness + extraversion +
                neuroticism + agreeableness, #bigfive
              family = binomial(link = "logit"))

summary(regr16b)

#testo la joint significance delle variabili statbehav_r e statconv_r


lrtest(regr16b,regr16)

###posso rimuovere quelle due variabili. Rimettiamo anche AP wagner_spread2


regr16ap = glm(data=data, expparty_r2 ~ 
               AP_wagner_spread2 +expbehav_r +#polarizzazione
               interest_r + exposure_r1 + collocated+#politica
               educ_r + age_r + sex_r + macroarea2 + citysize_r + 
               social_position_r_noNA 
             + culturalindex+ #controlli
               openness + conscientiousness + extraversion +
               neuroticism + agreeableness, #bigfive
             family = binomial(link = "logit"))

summary(regr16ap)

###################### Quindi nogta che AP_wagner_spread2 è borderline
# significativa se la variabile dip è expparty_r2. Chiaramente perché uno deve
# conoscere i partiti per collocarli. 

#Quindi in definitiva





data$age_r2 = ifelse(data$age_r >=60, "Old", 
                     ifelse(data$age_r<=35, "Young", "Adult"))


regrbestparty = glm(data=data, expparty_r2 ~ 
                 AP_wagner_spread2 +#polarizzazione
                 interest_r + exposure_r1 + collocated+#politica
                 educ_r + #age_r
                 age_r2+ sex_r + macroarea2 + citysize_r1 + 
                 social_position_r_noNA 
               + culturalindex+ #controlli
                 openness + conscientiousness + extraversion +
                 neuroticism + agreeableness, #bigfive
               family = binomial(link = "logit"))

summary(regrbestparty)

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



regrbestideology = glm(data=datanomiss, expideology_r2 ~ 
                      AP_wagner_spread2 +#polarizzazione
                      interest_r + exposure_r1 + collocated+#politica
                      educ_r + age_r + sex_r + macroarea2 + citysize_r + 
                      social_position_r_noNA 
                    + culturalindex+ #controlli
                      openness + conscientiousness + extraversion +
                      neuroticism + agreeableness, #bigfive
                    family = binomial(link = "logit"))

summary(regrbestideology)


############## In pratica credo che la variabile che rappresenta
# l'indice di Wagner funzioni ben meglio se utilizzo Ap.wagner.spread2
# Questo perché probabilmente è più corretto da un punto di vista teorico
#considerare meno polarizzati quelli che non conoscono tutti i partiti, 
# anche se forse così si mischia con la political knowledge. è una 
# questione delicata che va discussa con entrambe le supervisor 


#Notiamo anche che la storia cambia radicalmente se conideriamo l'ideologia. 
# Ciò perché AP_wagner_spread2 comunque cattura in parte anche la dimensione di
# conoscenza dei partiti. Expbehav funziona, però è post sperimentale. 
# Quindi riassumendo:
# -AP_wagner_spread2 funziona come predittore di expparty_r2be
# -AP_wagner_spread2 non funziona come expideology_r2
#     Ciò è probabilmente dovuto al fatto che AP_wagner_spread2 "copre" la conoscenza
#     dei partiti, che è una dimensione fondamentale
# expbehav_r funziona benissimo come predittore di entrambe ma è post sperimentale
# Quindi, AP funziona o no?


# CHIEDI ALLE PROF! NE VA DELLA TUA IPOTESI PRINCIPE!














########### PREMESSO CHE SERVIRA' LA VALIDAZIONE DELLA PROF SULLE PORCATE CHE HO FATTO QUI
# FINORA CREDO CHE POSSIAMO CONCLUDERE CHE SIGNIFICATIVI SONO
# - AP misurata behavioral (anche se ho dubbi dato che è registrata post, quindi 
# in realtà la causalità è all'inverso proprio. Avrei dovuto pensarci prima...)
# - Interest in politics, coerentemente con le ipotesi (SIGNIFICATIVA PER IDOLOGIA
#   MA NON PER PARTITO)
# - Exposure to news media, coerentemente con le ipotesi
# - Education (ci si poteva pensare...)
# - Collocarsi vs non collocarsi politicamente (e grazie al cavolo)
# - Posizione sociale (appena appena per l'ideologia)
# - Cultural index (incredibilmente significativo! E io che stavo per rinunciare a quella ipotesi...)
# - Social position è quasi significativa per l'ideologia e significativa per
# il partito ma considera che su quella  che sta nel modelloho fatto una imputazione
#  per i missing che non è facile difendere perché ho imputato i missing 
# come se fossero basse posizioni ecco.
# Forse dovrei imputare sulla base di un modello predittivo? Fin qui ho messo la
# media. Avevo provato anche con modello predittivo ma non cambiava troppo anche
# perché la media delle predizioni sui missing coincideva con la media del campione
# 





################################################################################
################################################################################
##################### GRAFICI E TABELLE (LOGISTICHE) ###########################
################################################################################
################################################################################



#####################################################################
################### risultati delle logistiche#######################
#####################################################################

####### prendiamo una regressione che ci piace, tipo regr16, ma
#comunque l'idea è che basta cambiare il nome della regressione e 
#abbiamo tutto pronto 

  #Facciamo una tabella confrontando i modelli

wordreg(list(regrXX, regrXY, regrXZ), file = "provatab.doc",
        custom.coef.names = c("Nomi", "Dei","Coefficienti"),
        custom.model.names = c("Nomi", "Dei", "Modelli"),
        caption = "Prova")


  ################ visualizzazioni delle logistiche

####NOTA CHE LA REGRESSIONE DEVE ESSERE FATTA SENZA MISSING VAI A
# CAPIRE PERCHé BAH

pred1 =cplot(regrbestparty, "expbehav_r")
pred2 =cplot(regrbestparty, "interest_r") 
pred3 =cplot(regrbestparty, "exposure_r1")
pred4 =cplot(regrbestparty, "collocated") # questi sono tutti predictive margins
pred5 =cplot(regrbestparty, "educ_r")
pred6 =cplot(regrbestparty, "culturalindex", what = "prediction")
#eff6 = cplot(regr16ap, "culturalindex", what = "effect")
pred7 =cplot(regrbestparty, "AP_wagner_spread2", what = "prediction")
#eff7 = cplot(regr16ap, "AP_wagner_spread2", what = "effect")


mar=margins(regrbestparty)

summary(mar)

plot(mar)  #con questo sto plottando gli average marginal effects ma viene male

#con ggplot viene meglio



p1 = ggplot(summary(mar)[1:round(nrow(summary(mar))/2), ], aes(x = factor, y=AME))+
  geom_point()+
  geom_pointrange(aes(ymax=AME+SE, ymin=AME-SE))+
  geom_hline(yintercept = 0, col='red')+
  labs(title = "Average Marginal Effects")+
  xlab("Variabile")+
  ylab("AME")


p2 = ggplot(summary(mar)[round(nrow(summary(mar))/2):nrow(summary(mar)), ], aes(x = factor, y=AME))+
  geom_point()+
  geom_pointrange(aes(ymax=AME+SE, ymin=AME-SE))+
  geom_hline(yintercept = 0, col='red')+
  labs(title = "Average Marginal Effects")+
  xlab("Variabile")+
  ylab("AME")

(p1/p2)

################### LI PRESENTO PER CATEGORIWE (INTERESE SOCIODEMO-PSICOMETRICHE E COSì VIA
# è PIU' CARINO)

### Predictive margins per culturalindex

library("ggplot2")
ggplot(pred6, aes(x = xvals)) + 
  geom_line(aes(y = yvals)) +
  geom_smooth(aes(y = upper), linetype = 2) +
  geom_smooth(aes(y = lower), linetype = 2) +
  ggtitle("Predicted margins cultural index") +
  #geom_hline(yintercept=0)+
  xlab("Cultural index") +
  ylab("Predicted margins")


# AME per culturalindex
#ggplot(eff6, aes(x = xvals)) + 
#  geom_line(aes(y = yvals)) +
#  geom_smooth(aes(y = upper), linetype = 2) +
#  geom_smooth(aes(y = lower), linetype = 2) +
#  ggtitle("AME cultural index") +
#  xlab("Cultural index") + ylab("AME")


### Predictive margins per AP_wagner_spread2

library("ggplot2")
ggplot(pred7, aes(x = xvals)) + 
  geom_line(aes(y = yvals)) +
  geom_smooth(aes(y = upper), linetype = 2) +
  geom_smooth(aes(y = lower), linetype = 2) +
  ggtitle("Predicted margins AP_wagner_spread2") +
  #geom_hline(yintercept=0)+
  xlab("AP_wagner_spread2") +
  ylab("Predicted margins")



########## gli stessi di prima in versione caterpillar


ggplot(pred6, aes(x = xvals, y=yvals))+
  geom_point()+
  geom_pointrange(aes(ymax=upper, ymin=lower))+
  labs(title = "Plot dei predictive margins di cultural index")+
  xlab("IV")+
  ylab("APE")


# POTREI PROVARE A METTERE SOTTO L'ISTOGRAMMA DELLA DISTRIBUZIONE DELLA VARIABILE
ggplot(pred7, aes(x = xvals, y=yvals))+
  geom_point()+
  geom_pointrange(aes(ymax=upper, ymin=lower))+
  labs(title = "Plot dei predictive margins di Ap wagner spread 2")+
  xlab("IV")+
  ylab("APE")




###############################################################################
################################################################################
#### STESSI GRAFICI MA COL MODELLO CON EXPIDEOLOGY_R2 COME VAR DIP



pred1 =cplot(regrbestideology, "expbehav_r")
pred2 =cplot(regrbestideology, "interest_r") 
pred3 =cplot(regrbestideology, "exposure_r1")
pred4 =cplot(regrbestideology, "collocated") # questi sono tutti predictive margins
pred5 =cplot(regrbestideology, "educ_r")
pred6 =cplot(regrbestideology, "culturalindex", what = "prediction")
#eff6 = cplot(regr16ap, "culturalindex", what = "effect")
pred7 =cplot(regrbestideology, "AP_wagner_spread2", what = "prediction")
#eff7 = cplot(regr16ap, "AP_wagner_spread2", what = "effect")


mar=margins(regrbestideology)

summary(mar)

plot(mar)  #con questo sto plottando gli average marginal effects ma viene male

#con ggplot viene meglio



p1 = ggplot(summary(mar)[1:round(nrow(summary(mar))/2), ], aes(x = factor, y=AME))+
  geom_point()+
  geom_pointrange(aes(ymax=AME+SE, ymin=AME-SE))+
  geom_hline(yintercept = 0, col='red')+
  labs(title = "Average Marginal Effects")+
  xlab("Variabile")+
  ylab("AME")


p2 = ggplot(summary(mar)[round(nrow(summary(mar))/2):nrow(summary(mar)), ], aes(x = factor, y=AME))+
  geom_point()+
  geom_pointrange(aes(ymax=AME+SE, ymin=AME-SE))+
  geom_hline(yintercept = 0, col='red')+
  labs(title = "Average Marginal Effects")+
  xlab("Variabile")+
  ylab("AME")

(p1/p2)



### Predictive margins per culturalindex

library("ggplot2")
ggplot(pred6, aes(x = xvals)) + 
  geom_line(aes(y = yvals)) +
  geom_smooth(aes(y = upper), linetype = 2) +
  geom_smooth(aes(y = lower), linetype = 2) +
  ggtitle("Predicted margins cultural index") +
  xlab("Cultural index") +
  ylab("Predicted margins")


############################################

# PROVARE A STIMARE UN PO' THEORY DRIVEN' I CHANGES IN PREDICTIVE PROBICTIVE PROBABILITIES
# (PREDICTIVE MARGINS) DI UNA SERIE DI SOGGETTI STEREOTIPICI. IN STATA GLI DIRESTI
# GFAMMI MARGINS QUINDI TIPO GLI AT SU UNA SERIE DI DEMOGRAFICHE FACENDO VARIARE AP
# QUINDI IO VI FACCIO VEDERE COMECAMBIA LA PROBABILITà DI EFFETTUARE AP PER UNA CERTO 
# TIPO DI SOGGETTO. QUELLO CHE VARIA è SEMPRE LA AP PERò DA UN PO' L'IDEA E AIUTA
# NELL'INTERPRETAZIONE '














# AME per culturalindex
#ggplot(eff6, aes(x = xvals)) + 
#  geom_line(aes(y = yvals)) +
#  geom_smooth(aes(y = upper), linetype = 2) +
#  geom_smooth(aes(y = lower), linetype = 2) +
#  ggtitle("AME cultural index") +
#  xlab("Cultural index") + ylab("AME")


### Predictive margins per AP_wagner_spread2

library("ggplot2")
ggplot(pred7, aes(x = xvals)) + 
  geom_line(aes(y = yvals)) +
  geom_smooth(aes(y = upper), linetype = 2) +
  geom_smooth(aes(y = lower), linetype = 2) +
  ggtitle("Predicted margins AP_wagner_spread2") +
  xlab("AP_wagner_spread2") +
  ylab("Predicted margins")



########## gli stessi di prima in versione caterpillar


ggplot(pred6, aes(x = xvals, y=yvals))+
  geom_point()+
  geom_pointrange(aes(ymax=upper, ymin=lower))+
  labs(title = "Plot dei predictive margins di cultural index")+
  xlab("IV")+
  ylab("APE")

ggplot(pred7, aes(x = xvals, y=yvals))+
  geom_point()+
  geom_pointrange(aes(ymax=upper, ymin=lower))+
  labs(title = "Plot dei predictive margins di Ap wagner spread 2")+
  xlab("IV")+
  ylab("APE")





####################### GUARDO SE COMBACIA PIU O MENO PER GENERE, ETA, AREA E TITOLO DI STUDIO
# DUE TABELLE VICINE













# 
# 
# #################### IMMONDIZIA
# 
# 
# data$AP_wagner_spread2_noNA = ifelse(is.na(data$AP_wagner_spread2), 
#                                      0,
#                                      data$AP_wagner_spread2)
# 
# #Riproviamo con questa variabile
# 
# regr16ap = glm(data=data, expparty_r2 ~ 
#                  AP_wagner_spread2_noNA +expbehav_r +#polarizzazione
#                  interest_r + exposure_r1 + collocated+#politica
#                  educ_r + age_r + sex_r + macroarea2 + citysize_r + 
#                  social_position_r_noNA 
#                + culturalindex+ #controlli
#                  openness + conscientiousness + extraversion +
#                  neuroticism + agreeableness, #bigfive
#                family = binomial(link = "logit"))
# 
# summary(regr16ap)
# 
# 
# 
# 
# 
# 
# ############### PORCO GIUDA COSì è SIGNIFICATIVA!!! PROVIAMO AD IMPUTARE
# # I MISSING CON UN MODELLO PREDITTIVO
# 
# predictive_model1 = lm(data=data, AP_wagner_spread2 ~ sex + educ + citysize + socposition_SQ001 +
#                          culturalindex +
#                          statbehav+ interest + exposure + ideology_SQ001+ taste_sshm+taste_rstt+
#                          taste_brgr + taste_plpt + taste_pstn + taste_crrt + taste_ltds+
#                          age_r + region + openness + conscientiousness + extraversion +
#                          neuroticism + agreeableness + statideology +
#                          +income+job1+pareduc+pdr+mdi+nmp+familyknows+
#                          friendsknows+statconv+statbehav, #bigfive
# )
# 
# summary(predictive_model1)
# 
# pred_df1 = data[is.na(data$AP_wagner_spread2)==T, c("sex", "educ", "citysize", "socposition_SQ001", "culturalindex", "statbehav", "interest","exposure", "ideology_SQ001", "taste_sshm","taste_rstt","taste_brgr", "taste_plpt", "taste_pstn", "taste_crrt", "taste_ltds","age_r", "region", "openness", "conscientiousness", "extraversion","neuroticism", "agreeableness", "statideology","income", "job1", "pareduc", "pdr", "mdi" ,"nmp", "familyknows", "friendsknows", "statconv","statbehav")]
# 
# predict(predictive_model1, pred_df1)
# 
# mean(predict(predictive_model1, pred_df1)) # la media è 1.23 quindi diversa da zero
# 
# data[is.na(data$AP_wagner_spread2)==T, ]$AP_wagner_spread2_noNA = predict(predictive_model1, pred_df1)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #proviamo a fare un modello predittivo per social position. Se questa cosa va bene, ri runniamo le analisi
# 
predictive_model = lm(data=data, social_position_r ~ sex + educ + citysize + culturalindex +
                        statbehav+ interest + exposure + ideology_SQ001+ taste_sshm+taste_rstt+
                        taste_brgr + taste_plpt + taste_pstn + taste_crrt + taste_ltds+
                        age_r + region + openness + conscientiousness + extraversion +
                        neuroticism + agreeableness + statideology +
                        +income+job1+pareduc+pdr+mdi+nmp+familyknows+
                        friendsknows+statconv+statbehav, #bigfive
 )
 
summary(predictive_model)

pred_df = data[!is.na(data$social_position_r), c("sex", "educ", "citysize", "culturalindex", "statbehav", "interest","exposure", "ideology_SQ001", "taste_sshm","taste_rstt","taste_brgr", "taste_plpt", "taste_pstn", "taste_crrt", "taste_ltds","age_r", "region", "openness", "conscientiousness", "extraversion","neuroticism", "agreeableness", "statideology","income", "job1", "pareduc", "pdr", "mdi" ,"nmp", "familyknows", "friendsknows", "statconv","statbehav")]

mean(predict(predictive_model, pred_df)) # la media è comunque sei 

predict(predictive_model, pred_df)
# # 
# data[is.na(data$social_position_r)==T, ]$social_position_r = predict(predictive_model, pred_df)
# 
# 
# 
# #infine, considera che non ho ancora utilizzato quasi per nulla le variabili statiche (andrea)
# 
# 
# 
# 
# #dove sono i missing
# 
# sum(is.na(data$AP_wagner_spread2)) #19 qui
# 
# sum(is.na(data$sex_r)) #3 qui
# 
#sum(is.na(data$citysize_r1)) #3 qui
# 
# sum(is.na(data$macroarea2)) #1 qui
# 
# 
