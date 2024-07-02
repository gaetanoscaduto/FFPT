#data tidying

library(rio)
library(tidyverse)
library(dplyr)

main_path = "C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/Papers and Chapters/FFPT"
setwd(paste0(main_path, "/data_manipulation"))
data = import("FFPT_def_pre_tidy.RDS", encoding = "UTF-8")

##############################################################################

################ DEFINING FUNCTION FOR WAGNER AP INDEX SPREAD OF SCORE #######

###############################################################################


AP.Wagner.spread = function(parties, results, like) {
  
  #NOTE: IF A RESPONDENT DOESN'T KNOW A PARTY I JUST REMOVE THAT
  # PARTY FROM THE COMPUTATION AND STANDARDIZE
  if(sum(is.na(like)) != length(parties))
  {
  #I standardize the results by the parties known by respondent
  results = results[!is.na(like)]
  results = results/sum(results)
  like = like[!is.na(like)]
  
  #I compute the average like as the vectorial product of results (expressed between 0 and 1)
  # and like (from 0 to 10)
  like_hat = results %*% like
  
  
  like_hat = rep(like_hat, length(like))
  AP = sqrt(results %*% (like-like_hat)^2)
  return(AP)
  }
  else
    return(0)
}



#QUESTA VERSIONE SMORZA L'AP DI CHI NON CONOSCE TUTTI I PARTITI

AP.Wagner.spread2 = function(parties, results, like) {
  
  #NOTE: IF A RESPONDENT DOESN'T KNOW A PARTY I JUST REMOVE THAT
  # PARTY FROM THE COMPUTATION but I DO NOT STANDARDIZE. So a person
  #that does not know many parties will be less polarized...
  if(sum(is.na(like)) < length(parties)) #if the respondent knows
    #at least two parties
  {
    #I standartdize the results by the electoral percentages but not
    # considering if the respodnent knows the party (standardization
    #before removing the rows where like is null. While in the previous
    #function the order is reversed)
    
    results = results/sum(results)
    results = results[!is.na(like)]
    like = like[!is.na(like)]
    
    #I compute the average like as the vectorial product of results (expressed between 0 and 1)
    # and like (from 0 to 10)
    like_hat = (results/(sum(results))) %*% like #the average like should
    # be standardized by the parties known by the respondent!
    
    
    like_hat = rep(like_hat, length(like))
    AP = sqrt(results %*% (like-like_hat)^2)
    return(AP)
  }
  # else
  #   if(sum(is.na(like)) == length(parties)-1) # if the respondent
  #     # knows just one party
  #   {
  #     results = results/sum(results)
  #     results = results[!is.na(like)]
  #     like = like[!is.na(like)]
  #     AP = sqrt(results %*% (like-5)^2)
  #   }
  else #if the respondent does not know any party
  {
    return(0)
  }
}


##############################################################################

################ DEFINING FUNCTION FOR WAGNER AP INDEX MEAN DISTANCE #######

###############################################################################


AP.Wagner.meandist = function(parties, results, like) {
  
  if(sum(is.na(like)) != length(parties))
  {
    #I standartdize the results by the parties known by respondent
    results = results[!is.na(like)]
    # I also standardize as if the percentage of the parties add up to 100 
    #(they dont)
    results = results/sum(results)
    
    
    like = like[!is.na(like)]
    like_max = rep(max(like), sum(!is.na(like)))
    
    AP = sqrt(results %*%(like-like_max)^2)
    return(AP)
  }
  else
    return(NA)
}



#QUESTA VERSIONE SMORZA L'AP DI CHI NON CONOSCE TUTTI I PARTITI


AP.Wagner.meandist2 = function(parties, results, like) {
  
  if(sum(is.na(like)) != length(parties))
  {
    #I standartdize the results by the electoral percentages but not
    # considering if the respodnent knows the party (standardization
    #before removing the rows where like is null. While in the previous
    #function the order is reversed)
    
    results = results/sum(results)
    results = results[!is.na(like)]
    like = like[!is.na(like)]
    
    like_max = rep(max(like), sum(!is.na(like)))
    
    AP = sqrt(results %*%(like-like_max)^2)
    return(AP)
  }
  else
    return(0)
}



########## DATA FOR THE ITALIAN CONTEXT 
# election results taken from
# https://elezioni.interno.gov.it/camera/scrutini/20220925/scrutiniCI


parties = c("FDI", "L", "FI", "AZIV", "M5S", "PD", "AVS")
results = c(0.2600, 0.0877, 0.0811, 0.0779, 0.1543, 0.1907, 0.0363)
#I want results to sum to 1, since I do not have all the parties i
# kind of standardize
results = results/sum(results)

##################################################




##############################################################################

############################ SOCIODEMOGRAFICS ################################

###############################################################################


#sex

data = data |>
  mutate(sex, 
         sex_r = case_when(
           sex == "Maschio" ~ "Male",
           sex == "Femmina" ~ "Female",
           sex != "Maschio" & sex != "Femmina" ~ NA,
           #IGNORO NON BINARIO? CHIEDERE PROF NEGRI
           is.na(sex) ~ NA
         ) 
  )

data$sex_r = factor(data$sex_r, levels = c("Male", "Female"))
table(data$sex, data$sex_r)

#age
data$age_r = as.numeric(as.character((data$age)))

data$age_r2 = ifelse(data$age_r >=60, "Old", 
                     ifelse(data$age_r<=35, "Young", "Adult"))


#region, recode for macroarea: NW, NE, C, S

data = data |>
  mutate(region, 
         macroarea1 = case_when(
           region %in% c("Lombardia", "Piemonte", "Val d'Aosta", "Liguria") ~ "North-West",
           region %in% c("Veneto", "Trentino-Alto Adige", "Emilia-Romagna", "Friuli-Venezia Giulia") ~ "North-East",
           region %in% c("Lazio", "Marche", "Toscana", "Umbria") ~ "Center",
           region %in% c("Abruzzo", "Basilicata", "Molise", "Calabria", "Campania", "Puglia", "Sardegna", "Sicilia") ~ "South",
           region == "Non abito in Italia" ~ NA,
           is.na(region) ~ NA
         ) 
  )

table(data$region, data$macroarea1)

# Recode for macroarea: N,C,S
data = data |>
  mutate(region, 
         macroarea2 = case_when(
           region %in% c("Lombardia", "Piemonte", "Val d'Aosta", "Liguria") ~ "North",
           region %in% c("Veneto", "Trentino-Alto Adige", "Emilia-Romagna", "Friuli-Venezia Giulia") ~ "North",
           region %in% c("Lazio", "Marche", "Toscana", "Umbria") ~ "Center",
           region %in% c("Abruzzo", "Basilicata", "Molise", "Calabria", "Campania", "Puglia", "Sardegna", "Sicilia") ~ "South",
           region == "Non abito in Italia" ~ NA,
           is.na(region) ~ NA
         ) 
  )
  
table(data$macroarea1, data$macroarea2)

#city dimension: first recode (linear)

data = data |>
  mutate(citysize, 
         citysize_r1 = case_when(
           citysize == "Una grande città" ~ 4,
           citysize == "I sobborghi o la periferia di una grande città" ~ 3,
           citysize == "Una città di medie dimensioni o cittadina" ~ 2,
           citysize == "Un paese" ~ 1,
           citysize == "Una casa isolata" ~ 0,
           citysize == "Non saprei" ~ 0,
           is.na(citysize) ~ NA
         ) 
  )

#city dimension: second recode (dummy)
data$citysize_r2 = ifelse(data$citysize_r >=3, "Big", "Small")
data$citysize_r2 = factor(data$citysize_r2, levels =c("Small", "Big"))


table(data$citysize, data$citysize_r2)




#education: recode dummy degree

data = data |>
  mutate(educ, 
         educ_r = case_when(
           grepl("diploma", educ, ignore.case = T) | grepl("licenza", educ, ignore.case = T)  ~ "Low", #Low
           grepl("laurea", educ, ignore.case = T) | grepl("dottorato", educ, ignore.case = T)  ~ "High", #High
           is.na(educ) ~ NA
         ) 
  )

#PROVA ANCHE LOW-MEDIUM-HIGH TE LO DICE LA PROF

data$educ_r = factor(data$educ_r, levels = c("Low", "High"))

table(data$educ, data$educ_r)

#job: recode employed-unemployed-inactive

data = data |>
  mutate(job1, 
         job1_r = case_when(
           job1 == "Lavoratore/lavoratrice dipendente" ~ "Employed",
           job1 == "Lavoratore/lavoratrice autonomo/a" ~ "Employed",
           job1 == "Disoccupato/a" ~ "Unemployed",
           job1 == "In cerca di prima occupazione" ~ "Unemployed",
           job1 == "Pensionato/a" ~ "Inactive",
           job1 == "Casalingo/a" ~ "Inactive",
           job1 == "Cassa integrazione guadagni o mobilità"~ "Inactive",
           job1 == "Studente/essa"~ "Inactive",
           job1 == "Nessuna delle precedenti"~ "Inactive",
           is.na(citysize) ~ NA
         ) 
  )



#income: recode linear

data = data |>
  mutate(income, 
         income_r = case_when(
           income == "Vivo senza problemi e riesco a mettere da parte dei risparmi" ~ 4,
           income == "Vivo senza problemi ma non riesco a mettere da parte dei risparmi" ~ 3,
           income == "Riesco ad arrangiarmi facendo qualche rinuncia" ~ 2,
           income == "Ho delle difficoltà e devo fare tante rinunce ma riesco a vivere col mio reddito" ~ 1,
           income == "Ho molte difficoltà e non riesco a vivere con il mio reddito" ~ 0,
           income == "Preferisco non rispondere" ~ NA, #qui ci stanno 21 persone che sarebbe meglio non perdere
           
           is.na(income) ~ NA
         ) 
  )

#INCOME IN QUALCHE MODO LA DEVO INFILARE (PROF)
# POSSO RICODIFICARE IN 3 (SENZA PROBLEMI - MI ARRANGIO - DIFFICOLTA')


table(data$income_r, data$income)


#self-collocation: linear recodi


data = data |>
  mutate(socposition_SQ001, 
         social_position_r = case_when(
           socposition_SQ001 == "10 - Al vertice della nostra società" ~ 10,
           socposition_SQ001 == "0 - Al fondo della nostra società" ~ 0,
           socposition_SQ001 %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9") ~ as.numeric(socposition_SQ001),
           is.na(socposition_SQ001) ~ NA,
           socposition_SQ001 == "Preferisco non rispondere" ~ NA 
         ) ) 

table(data$social_position_r, data$socposition_SQ001)

## Uncommenting the following lines, I set the missing at mean value. If I want them
# to be the result of predictions, go at the end of the script
# 
# data$social_position_r_noNA = ifelse(is.na(data$social_position_r), round(mean(data$social_position_r, na.rm=T), digits = 0), data$social_position_r)
# 
# table(data$social_position_r, data$social_position_r_noNA)


#Parent education: dummy recode

data = data |>
  mutate(pareduc, 
         pareduc_r = case_when(
           grepl("diploma", pareduc, ignore.case = T) | grepl("licenza", pareduc, ignore.case = T)  ~ "Low", #Low
           grepl("laurea", pareduc, ignore.case = T) | grepl("dottorato", pareduc, ignore.case = T)  ~ "High", #High
           is.na(pareduc) ~ NA
         ) 
  )

table(data$pareduc, data$pareduc_r)


#######indexes for taste (vegan, meat, typical, ethnic)

data = data |>
  mutate(
    across(
      starts_with("taste"),
     ~case_when(.=="5 - Mi piace molto" ~ 5, 
             .=="4" ~ 4,
             .=="3" ~ 3,
             .=="2" ~ 2,
             .=="1 - Non mi piace affatto" ~ 1,
             is.na(.) | .== "Non lo conosco" ~ NA),
  .names = "{.col}_r"))


prop.table(table(data$taste_sshm_r))
prop.table(table(data$taste_rstt_r))
prop.table(table(data$taste_plpt_r))
prop.table(table(data$taste_brgr_r))
prop.table(table(data$taste_pstn_r))
prop.table(table(data$taste_crrt_r))
prop.table(table(data$taste_trtr_r))
prop.table(table(data$taste_ltds_r))

data = data |>
  mutate(
    vegan_index = (taste_brgr_r + taste_ltds_r)/2
          )

data = data |>
  mutate(
    meat_index = (taste_plpt_r + taste_trtr_r)/2
  )

data = data |>
  mutate(
    ethnic_index = (taste_sshm_r + taste_crrt_r)/2 
  )

data = data |>
  mutate(
    typical_index = (taste_pstn_r + taste_rstt_r)/2 
  )



#The soc_projected_index is an index that tells me how much the respondent likes 
# the group for which they social project. Notice that we should also account for 
#counterprojection (square?)

##############################################################################

################ CULTURE POLITICS AND SOCIETY ################################

###############################################################################


############ CULTURAL ACTIVITIES

data = data |>
  mutate(
    across(
      starts_with("cultural"),
      ~case_when(.=="Ogni giorno" ~ 4, 
                 .=="Almeno una volta a settimana" ~ 3,
                 .=="Almeno una volta al mese" ~ 2,
                 .=="Almeno una volta nell'ultimo anno" ~ 1,
                 .=="Mai nell'ultimo anno" ~ 0,
                 .=="Non ricordo" ~ 0, # if someone doesn't remember, 
                 #probably it has been a while....
                 is.na(.) ~ NA),
      .names = "{.col}_r"))


table(data$cultural_cnma)
table(data$cultural_cnma_r)
table(data$cultural_sptt_r)
table(data$cultural_mnmt_r)
table(data$cultural_prtc_r)
############# CREATE INDEX OF CULTURAL PARTICIPATION

data = data |>
  mutate(
    culturalindex = (cultural_cnma_r + cultural_mnmt_r + cultural_sptt_r + cultural_prtc_r)/4
  )



######### BIG FIVE TRAITS


data = data |>
  mutate(
    across(
      starts_with("bigfive"),
      ~case_when(.=="Pienamente d'accordo" ~ 4, 
                 .=="Abbastanza d'accordo" ~ 3,
                 .=="Né d'accordo né in disaccordo" ~ 2,
                 .=="Poco d'accordo" ~ 1,
                 .=="Per niente d'accordo" ~ 0,
                 is.na(.) ~ NA),
      .names = "{.col}_r"))

table(data$bigfive_agr, data$bigfive_agr_r)
table(data$bigfive_neu, data$bigfive_neu_r)

##########################
# SOME OF THESE ITEMS ARE TO BE REVERSED (SEE CHIORRI ET AL., 2015)
data$bigfive_revagr_r = 4 -data$bigfive_revagr_r
data$bigfive_revext_r = 4 -data$bigfive_revext_r
data$bigfive_revcon_r = 4 -data$bigfive_revcon_r
data$bigfive_revope_r = 4 -data$bigfive_revope_r
data$bigfive_revneu_r = 4 -data$bigfive_revneu_r

############ COMPUTE BIG FIVE INDEXES
data$extraversion = (data$bigfive_ext_r +data$bigfive_revext_r)/2
data$agreeableness = (data$bigfive_agr_r + data$bigfive_revagr_r)/2
data$neuroticism = (data$bigfive_neu_r + data$bigfive_revneu_r)/2
data$openness = (data$bigfive_ope_r + data$bigfive_revope_r)/2
data$conscientiousness = (data$bigfive_con_r + data$bigfive_revcon_r)/2

prop.table(table(data$extraversion))
prop.table(table(data$agreeableness))
prop.table(table(data$neuroticism))
prop.table(table(data$openness))
prop.table(table(data$conscientiousness))



#Quanto ti interessi di politica

# RECODE
# data = data |>
#    mutate(interest, 
#     interest_r= case_when(
#          interest == "Molto" ~ 3,
#          interest == "Abbastanza" ~ 2,
#          interest == "Poco" ~ 1,
#          interest == "Per niente" ~ 0,
#          interest == "Non saprei" ~ NA,
#          is.na(interest) ~ NA
#         ) ) 


# RECODE DYCHOTOMIC HIGH-LOW (A LA CURINI-NAI-Casiraghi 2023)
data = data |>
  mutate(interest, 
         interest_r= case_when(
           interest == "Molto" ~ "High",
           interest == "Abbastanza" ~ "High",
           interest == "Poco" ~ "Low",
           interest == "Per niente" ~ "Low",
           is.na(interest) ~ NA
         ) ) 

data$interest_r = factor(data$interest_r, levels=c("Low", "High"))

table(data$interest_r, data$interest)

#News media exposure


  #RECODE: linear recode

#data = data |>
#  mutate(exposure, 
#         exposure_r = case_when(
#           exposure == "Più di due ore" ~ 6,
#           exposure == "Fra una e due ore" ~ 5,
#           exposure == "Fra mezz'ora e un'ora" ~ 4,
#           exposure == "Fra dieci minuti e mezz'ora" ~ 3,
#           exposure == "Meno di dieci minuti" ~ 2,
#           exposure == "Mi capita di farlo, ma raramente" ~ 1,
#           exposure == "Non lo faccio mai" ~ 0,
#           is.na(exposure) ~ NA
#            ) 
#         ) #|>
 # relocate(exposure_r, .after=exposure)


# RECODE EXPOSURE: dichotomous recode
data = data |>
  mutate(exposure, 
         exposure_r1 = case_when(
           exposure == "Più di due ore" ~ "High",
           exposure == "Fra una e due ore" ~ "High",
           exposure == "Fra mezz'ora e un'ora" ~ "High",
           exposure == "Fra dieci minuti e mezz'ora" ~ "High",
           exposure == "Meno di dieci minuti" ~ "Low",
           exposure == "Mi capita di farlo, ma raramente" ~ "Low",
           exposure == "Non lo faccio mai" ~ "Low",
           is.na(exposure) ~ NA
         ) 
  ) 

data$exposure_r1 = factor(data$exposure_r1, levels = c("Low", "High"))
table(data$exposure, data$exposure_r1)

#Recode exposure: trichotomous recode

data = data |>
  mutate(exposure, 
         exposure_r2 = case_when(
           exposure == "Più di due ore" ~ "High",
           exposure == "Fra una e due ore" ~ "High",
           exposure == "Fra mezz'ora e un'ora" ~ "High",
           exposure == "Fra dieci minuti e mezz'ora" ~ "Medium",
           exposure == "Meno di dieci minuti" ~ "Low",
           exposure == "Mi capita di farlo, ma raramente" ~ "Low",
           exposure == "Non lo faccio mai" ~ "Low",
           is.na(exposure) ~ NA
         ) 
  ) 

table(data$exposure, data$exposure_r2)

data$exposure_r2 = factor(data$exposure_r2, levels = c("Low", "Medium", "High"))


#political ideology: linear recode with missing

data = data |>
  mutate(ideology_SQ001, 
         ideology_r = 
           case_when(
             ideology_SQ001 == "10 - Estrema destra" ~ 10,
             ideology_SQ001 == "9" ~ 9,
             ideology_SQ001 == "8" ~ 8,
             ideology_SQ001 == "7" ~ 7,
             ideology_SQ001 == "6" ~ 6,
             ideology_SQ001 == "5 - Centro" ~ 5,
             ideology_SQ001 == "4" ~ 4,
             ideology_SQ001 == "3" ~ 3,
             ideology_SQ001 == "2" ~ 2,
             ideology_SQ001 == "1" ~ 1,
             ideology_SQ001 == "0 - Estrema sinistra" ~ 0,
             ideology_SQ001 == "Da nessuna parte" ~ NA, # are we sure???? ASK PROF NEGRI 
             is.na(ideology_SQ001) ~ NA
           ) 
  )


######### Political ideology: recode with 4 cathegories (including nowhere)

data = data |>
  mutate(ideology_SQ001, 
         ideology_r2 = 
           case_when(
             ideology_SQ001 == "10 - Estrema destra" ~ "Right",
             ideology_SQ001 == "9" ~ "Right",
             ideology_SQ001 == "8" ~ "Right",
             ideology_SQ001 == "7" ~ "Right",
             ideology_SQ001 == "6" ~ "Center",
             ideology_SQ001 == "5 - Centro" ~ "Center",
             ideology_SQ001 == "4" ~ "Center",
             ideology_SQ001 == "3" ~ "Left",
             ideology_SQ001 == "2" ~ "Left",
             ideology_SQ001 == "1" ~ "Left",
             ideology_SQ001 == "0 - Estrema sinistra" ~ "Left",
             ideology_SQ001 == "Da nessuna parte" ~ "Nowhere",  
             is.na(ideology_SQ001) ~ NA
           ) 
  )

data$ideology_r2 = factor(data$ideology_r2, levels = c("Center", "Nowhere", "Right", "Left"))
table(data$ideology_SQ001, data$ideology_r2)


######### Political ideology: extremism recoding

data = data |>
  mutate(ideology_SQ001, 
         extremism = 
           case_when(
             ideology_SQ001 == "10 - Estrema destra" ~ "Extreme",
             ideology_SQ001 == "9" ~ "Extreme",
             ideology_SQ001 == "8" ~ "Extreme",
             ideology_SQ001 == "7" ~ "Not Extreme",
             ideology_SQ001 == "6" ~ "Not Extreme",
             ideology_SQ001 == "5 - Centro" ~ "Not Extreme",
             ideology_SQ001 == "4" ~ "Not Extreme",
             ideology_SQ001 == "3" ~ "Not Extreme",
             ideology_SQ001 == "2" ~ "Extreme",
             ideology_SQ001 == "1" ~ "Extreme",
             ideology_SQ001 == "0 - Estrema sinistra" ~ "Extreme",
             ideology_SQ001 == "Da nessuna parte" ~ "Not Extreme",  
             is.na(ideology_SQ001) ~ NA
           ) 
  )

table(data$ideology_SQ001, data$extremism)


### Political ideology: linear recoding with nowhere as center


data = data |>
  mutate(ideology_SQ001, 
         ideology_r3 = 
           case_when(
             ideology_SQ001 == "10 - Estrema destra" ~ 10,
             ideology_SQ001 == "9" ~ 9,
             ideology_SQ001 == "8" ~ 8,
             ideology_SQ001 == "7" ~ 7,
             ideology_SQ001 == "6" ~ 6,
             ideology_SQ001 == "5 - Centro" ~ 5,
             ideology_SQ001 == "4" ~ 4,
             ideology_SQ001 == "3" ~ 3,
             ideology_SQ001 == "2" ~ 2,
             ideology_SQ001 == "1" ~ 1,
             ideology_SQ001 == "0 - Estrema sinistra" ~ 0,
             ideology_SQ001 == "Da nessuna parte" ~ 5,  
             is.na(ideology_SQ001) ~ NA
           ) 
  )

table(data$ideology_SQ001, data$ideology_r3)



data = data |>
  mutate(ideology_SQ001, 
         ideology_r4 = 
           case_when(
             ideology_SQ001 == "10 - Estrema destra" ~ "10",
             ideology_SQ001 == "9" ~ "9",
             ideology_SQ001 == "8" ~ "8",
             ideology_SQ001 == "7" ~ "7",
             ideology_SQ001 == "6" ~ "6",
             ideology_SQ001 == "5 - Centro" ~ "5",
             ideology_SQ001 == "4" ~ "4",
             ideology_SQ001 == "3" ~ "3",
             ideology_SQ001 == "2" ~ "2",
             ideology_SQ001 == "1" ~ "1",
             ideology_SQ001 == "0 - Estrema sinistra" ~ "0",
             ideology_SQ001 == "Da nessuna parte" ~ "Nowhere",  
             is.na(ideology_SQ001) ~ NA
           ) 
  )

data$ideology_r4 = factor(data$ideology_r4, 
                          levels = c("0", "1",
                                     "2", "3", 
                                     "4", "5",
                                     "6", "7",
                                     "8", "9",
                                     "10", "Nowhere"))

table(data$ideology_SQ001, data$ideology_r4)


data = data |>
  mutate(ideology_SQ001, 
         ideology_r5 = 
           case_when(
             ideology_SQ001 == "10 - Estrema destra" ~ "Right",
             ideology_SQ001 == "9" ~ "Right",
             ideology_SQ001 == "8" ~ "Right",
             ideology_SQ001 == "7" ~ "Right",
             ideology_SQ001 == "6" ~ "Right",
             ideology_SQ001 == "5 - Centro" ~ "Center",
             ideology_SQ001 == "4" ~ "Left",
             ideology_SQ001 == "3" ~ "Left",
             ideology_SQ001 == "2" ~ "Left",
             ideology_SQ001 == "1" ~ "Left",
             ideology_SQ001 == "0 - Estrema sinistra" ~ "Left",
             ideology_SQ001 == "Da nessuna parte" ~ "Nowhere",  
             is.na(ideology_SQ001) ~ NA
           ) 
  )

data$ideology_r5 = factor(data$ideology_r5, levels = c("Center", "Nowhere", "Right", "Left"))
table(data$ideology_SQ001, data$ideology_r5)



############### Political ideology: collocated dummy recoding
 

data = data |>
  mutate(ideology_SQ001, 
         collocated = 
           ifelse(ideology_SQ001 == "Da nessuna parte", "Not collocated", "Collocated")
           )

data$collocated = factor(data$collocated, c("Not collocated", "Collocated"))


table(data$ideology_SQ001, data$collocated)


#Affective polarization recode

data = data |>
  mutate(
    across(
      starts_with("feelterm"),
      ~case_when(.=="10 - Mi piace molto" ~ 10, 
                 .=="0 - Non mi piace affatto" ~ 0,
                 .=="1" ~ 1,
                 .=="2" ~ 2,
                 .=="3" ~ 3,
                 .=="4" ~ 4,
                 .=="5" ~ 5,
                 .=="6" ~ 6,
                 .=="7" ~ 7,
                 .=="8" ~ 8,
                 .=="9" ~ 9,
                 .=="Non conosco" ~ NA,
                 is.na(.) ~ NA),
      .names = "{.col}_r"))

table(data$feelterm_avs, data$feelterm_avs_r)


#### Computing WAGNER'S SPREAD-of-score index 

i=1
while(i<=nrow(data))
{ 
  data$AP_wagner_spread[i] = 
  AP.Wagner.spread(parties, results, data[i, which(grepl("feelterm", names(data)) & grepl("_r", names(data)))])
  i=i+1
}

data$AP_wagner_spread_noNA = ifelse(is.na(data$AP_wagner_spread), 0, data$AP_wagner_spread)


#### Computing WAGNER'S SPREAD-of-score index with the function that penalyzes people
# who don't know some parties


i=1
while(i<=nrow(data))
{ 
  data$AP_wagner_spread2[i] = 
    AP.Wagner.spread2(parties, results, data[i, which(grepl("feelterm", names(data)) & grepl("_r", names(data)))])
  i=i+1
}


i=1
while(i<=nrow(data))
{
data$knowsparty[i] = sum(!is.na(data[i, which(grepl("feelterm", names(data)) & grepl("_r", names(data)))]))

# this is then number of parties that the respondent knows
i=i+1
}


#### Wagner's (2021) mean distance index

i=1
while(i<=nrow(data))
{ 
  data$AP_wagner_meandist[i] = 
    AP.Wagner.meandist(parties, results, as.numeric(data[i, which(grepl("feelterm", names(data)) & grepl("_r", names(data)))]))
  i=i+1
}

#### Computing WAGNER'S mean-distance index with the function that penalyzes people
# who don't know some parties

i=1
while(i<=nrow(data))
{ 
  data$AP_wagner_meandist2[i] = 
    AP.Wagner.meandist2(parties, results, as.numeric(data[i, which(grepl("feelterm", names(data)) & grepl("_r", names(data)))]))
  i=i+1
}

ggplot(data[is.na(data$AP_wagner_meandist)==F, ], aes(x=AP_wagner_meandist))+
  geom_density()
ggplot(data, aes(x=AP_wagner_spread))+
  geom_density()

ggplot(data[is.na(data$AP_wagner_meandist)==F, ], aes(x=AP_wagner_meandist))+
  geom_boxplot()
ggplot(data, aes(x=AP_wagner_spread))+
  geom_boxplot()


cor(data$AP_wagner_meandist, data$AP_wagner_spread, use="complete.obs")

#CORRELATION very high. IT IS THE SAME VARIABLE!


### I compute each respondent's ranking of parties

i=1

dataaus = data[, grepl("feelterm_", names(data)) & grepl("_r", names(data))]

while(i<=nrow(data))
{
  max_party = "None"
  max_rating = NA
  if(sum(is.na(dataaus[i, 1:7]))<7)
  {
  max_rating = max(dataaus[i, 1:7], na.rm = T)
  max_party = names(dataaus)[which(dataaus[i, 1:7] == max_rating)]
  max_party = toString(max_party)
  max_party = gsub("feelterm_", "", max_party)
  max_party = gsub("_r", "", max_party)
  }
  dataaus$max_party[i] = max_party
  dataaus$max_rating[i] = max_rating
  
  i=i+1
}

data$max_party = dataaus$max_party
data$max_rating = dataaus$max_rating

#I compute a dummy indicating that a party is or not in the list of favorite

data$is_max_fdi = ifelse(grepl("fdi", data$max_party), 1, 0)
data$is_max_lega = ifelse(grepl("lega", data$max_party), 1, 0)
data$is_max_fi = ifelse(grepl("fi", data$max_party), 1, 0)
data$is_max_aziv = ifelse(grepl("aziv", data$max_party), 1, 0)
data$is_max_m5s = ifelse(grepl("m5s", data$max_party), 1, 0)
data$is_max_pd = ifelse(grepl("pd", data$max_party), 1, 0)
data$is_max_avs = ifelse(grepl("avs", data$max_party), 1, 0)


data = data |>
  mutate(max_party, 
         max_party_r = gsub("fdi", "Brothers of Italy", max_party),
         max_party_r = gsub("lega", "League", max_party_r),
         max_party_r = gsub("fi", "Go Italy", max_party_r),
         max_party_r = gsub("aziv", "Action-Italy Alive", max_party_r),
         max_party_r = gsub("m5s", "Five Star Movement", max_party_r),
         max_party_r = gsub("pd", "Democratic Party", max_party_r),
         max_party_r = gsub("avs", "Green-Left Alliance", max_party_r) 
  )


table(data$statparty, data$statparty_r3)




rm(dataaus)






#Three questions on political sophistication

data = data |>
  mutate(pdr, 
         pdr_r = case_when(
           pdr == "Sergio Mattarella" ~ 1,
           pdr != "Sergio Mattarella" ~ 0,
           is.na(pdr) ~ NA
         ) 
  )

table(data$pdr, data$pdr_r)

data = data |>
  mutate(mdi, 
         mdi_r = case_when(
           mdi == "Matteo Piantedosi" ~ 1,
           mdi != "Matteo Piantedosi" ~ 0,
           is.na(mdi) ~ NA
         ) 
  )

table(data$mdi, data$mdi_r)


data = data |>
  mutate(nmp, 
         nmp_r = case_when(
           nmp == "600" ~ 1,
           nmp != "600" ~ 0,
           is.na(nmp) ~ NA
         ) 
  )

table(data$nmp, data$nmp_r)


###### INDEX OF POLitical  SOPHistication

data$pol_soph = (data$pdr_r+data$mdi_r+data$nmp_r)/3

data$pol_soph2 = ifelse(data$pol_soph == 1, 1, 0)
#### THE POLSOPH IS HIGHER THAN I THOUGHT. MAYBE I SHOULD DYCHOTOMIZE (POLSOPH =1 ->HIGH VS OTHER->LOW)

############## hetereogeneity ###################

# Members of family that R knows political deology of


data = data |>
  mutate(familyknows, 
         familyknows_r = 
           case_when(
             familyknows == "Nessuno" ~ 0,
             familyknows == "Quasi nessuno" ~ 0.1,
             familyknows == "Alcuni" ~ 0.25,
             familyknows == "Circa la metà" ~ 0.5,
             familyknows == "Molti" ~ 0.75,
             familyknows == "Quasi tutti" ~ 0.9,
             familyknows == "Tutti" ~ 1,
             is.na(familyknows) ~ NA
           ) 
  )

table(data$familyknows_r, data$familyknows)
### Members of family that R agrees with politically


data = data |>
  mutate(familydisagr, 
         familydisagr_r = 
           case_when(
             familydisagr == "Nessuno" ~ 0,
             familydisagr == "Quasi nessuno" ~ 0.1,
             familydisagr == "Alcuni" ~ 0.25,
             familydisagr == "Circa la metà" ~ 0.5,
             familydisagr == "Molti" ~ 0.75,
             familydisagr == "Quasi tutti" ~ 0.9,
             familydisagr == "Tutti" ~ 1,
             is.na(familydisagr) ~ NA
           ) 
  )

table(data$familydisagr_r, data$familydisagr)
# Friends that R knows political ideology of

data = data |>
  mutate(friendsknows, 
         friendsknows_r = 
           case_when(
             friendsknows == "Nessuno" ~ 0,
             friendsknows == "Quasi nessuno" ~ 0.1,
             friendsknows == "Alcuni" ~ 0.25,
             friendsknows == "Circa la metà" ~ 0.5,
             friendsknows == "Molti" ~ 0.75,
             friendsknows == "Quasi tutti" ~ 0.9,
             friendsknows == "Tutti" ~ 1,
             is.na(friendsknows) ~ NA
           ) 
  )


table(data$friendsknows, data$friendsknows_r)



### Friends that R agrees with politically

data = data |>
  mutate(friendsdisagr, 
         friendsdisagr_r = 
           case_when(
             friendsdisagr == "Nessuno" ~ 0,
             friendsdisagr == "Quasi nessuno" ~ 0.1,
             friendsdisagr == "Alcuni" ~ 0.25,
             friendsdisagr == "Circa la metà" ~ 0.5,
             friendsdisagr == "Molti" ~ 0.75,
             friendsdisagr == "Quasi tutti" ~ 0.9,
             friendsdisagr == "Tutti" ~ 1,
             is.na(friendsdisagr) ~ NA
           ) 
  )



############## POTREI IMPUTARE AI MISSING DELLE SECONDE DUE ZERO E VEDERE SE FUNZIONA

table(data$friendsdisagr, data$friendsdisagr_r)

table(data$friendsdisagr)

###An estimate of the portion of the network that the respondent knows the ideology of

data$knows_r = (data$familyknows_r + data$friendsknows_r)/2



# THE FOLLOWING  INDEX IS COMPUTED IN GUIDETTI-CAVAZZA-GRAZIANI 2026
#FASHION



data = data |>
  mutate(
    perceived_disagreement = (friendsdisagr_r + familydisagr_r)/2
  )

# The following index I invented it
data = data |>
  mutate(
    perceived_heterogeneity = (1 -2*(abs(0.5-perceived_disagreement)))
    
  )

data$hetero_r = ifelse(is.na(data$friendsdisagr_r) | is.na(data$familydisagr_r),
                       NA, 
                       1-(data$familydisagr_r + data$friendsdisagr_r)/2)




##check the missing

sum(is.na(data$perceived_disagreement)) == sum(is.na(data$friendsdisagr_r) | is.na(data$familydisagr_r))

##############################################################################

################ VIGNETTES ##################################################

###############################################################################



### experimental group

data = data |>
  mutate(randomnumber, 
         group = case_when(
           randomnumber == "1" ~ "Control",
           randomnumber == "2" ~ "Vegan",
           randomnumber == "3" ~ "Meat",
           randomnumber == "4" ~ "Typical",
           randomnumber == "5" ~ "Ethnic"
         ) 
  )

data$group = factor(data$group, levels = c("Control", "Vegan", "Ethnic", "Meat", "Typical"), ordered = T)


table(data$randomnumber, data$group)


#The soc_projected_index is an index that tells me how much the respondent likes 
# the group for which they social project. Notice that we should also account for 
#counterprojection (square?).
#Notice also that the indexes (vegan, typical, meat) from which we compute the soc_projected_idex
# have missing values sometimes (because people said they dont know thai curry or stuff like that)
# in these cases, plus in the cases where people had the control group, we set the value of the soc_proj
# index at -1 For every other case, the value ranges from 0 to 4
data$soc_projected_index=ifelse(data$group =="Vegan" & !is.na(data$vegan_index), data$vegan_index,
                                ifelse(data$group =="Meat" & !is.na(data$meat_index), data$meat_index,
                                       ifelse(data$group =="Typical" & !is.na(data$typical_index), data$typical_index,
                                              ifelse(data$group =="Ethnic" & !is.na(data$ethnic_index), data$ethnic_index, 
                                                    ifelse(data$group =="Control", 0, 0)))))


data$soc_projected_index = data$soc_projected_index-1

table(data$soc_projected_index)

sum(table(data$soc_projected_index))

#Like_food is for coontrolling the propensity to like food overall...

data$like_food = NA

i=1
while(i<=nrow(data))
{
  data$like_food[i] = sum(data$vegan_index[i],
                          data$ethnic_index[i],
                          data$meat_index[i],
                          data$typical_index[i],
                          na.rm = T)
  i=i+1
}

table(data$like_food)

sum(table(data$like_food))
################# WHERE WOULD YOU PLACE THE VIGNETTE IDEOLOGICALLY
#We would do different recodes here. 


#RECODE NUMBER 1: don't know goes missing

data = data |>
  mutate(expideology, 
         expideology_r1 = case_when(
           expideology == "Estrema destra" ~ 3,
           expideology == "Destra" ~ 2,
           expideology == "Centro-destra" ~ 1,
           expideology == "Centro" ~ 0,
           expideology == "Centro-sinistra" ~ -1,
           expideology == "Sinistra" ~ -2,
           expideology == "Estrema sinistra" ~ -3,
           expideology == "Non saprei" ~ NA,
           is.na(expideology) ~ NA
         ) 
  )

table(data$expideology,data$expideology_r1)
table(data$expideology_r1)
table(data$expideology_r1, data$group)

# i create the variable perceived_outgroup

data$perceived_outgroup = ifelse(data$ideology_r5=="Right" & data$expideology_r1<0 & !(is.na(data$expideology_r1)), 1,
                                 ifelse(data$ideology_r5 == "Left" & data$expideology_r1>0 & !(is.na(data$expideology_r1)), 1, 0))

data$perceived_ingroup = ifelse(data$ideology_r5=="Right" & data$expideology_r1>0 & !(is.na(data$expideology_r1)), 1,
                                 ifelse(data$ideology_r5 == "Left" & data$expideology_r1<0 & !(is.na(data$expideology_r1)), 1, 0))

#I check the number of rows in the subset

row(datanomiss[(datanomiss$ideology_r5 == "Right" | datanomiss$ideology_r5 == "Left") & (datanomiss$expideology_r1<0 |  datanomiss$expideology_r1>0) & (!is.na(datanomiss$expideology_r1)), ])

#RECODE NUMBER 2: don't know =0, other =1

data = data |>
  mutate(expideology, 
         ideology_PL = case_when(
           expideology == "Estrema destra" ~ 1,
           expideology == "Destra" ~ 1,
           expideology == "Centro-destra" ~ 1,
           expideology == "Centro" ~ 1,
           expideology == "Centro-sinistra" ~ 1,
           expideology == "Sinistra" ~ 1,
           expideology == "Estrema sinistra" ~ 1,
           expideology == "Non saprei" ~ 0,
           is.na(expideology) ~ NA
         ) 
  )

table(data$ideology_PL, data$expideology)

######### RECODE NUMBER 3: EXPECTED VS UNEXPECTED DIRECTION

data = data |>
  mutate(expideology, 
         expideology_r3 = 
           ifelse(data$group =="Control", 
                  #CONTROL GROUP : CENTRO = 1 O = 0?
                  case_when(
                    expideology == "Estrema destra" ~ 0,
                    expideology == "Destra" ~ 0,
                    expideology == "Centro-destra" ~ 0,
                    expideology == "Centro" ~ 0,
                    expideology == "Centro-sinistra" ~ 0,
                    expideology == "Sinistra" ~ 0,
                    expideology == "Estrema sinistra" ~ 0,
                    expideology == "Non saprei" ~ 1,
                    is.na(expideology) ~ NA
                  ),
                  # VEGAN AND ETHNIC GROUP
                  ifelse(data$group =="Vegan" | data$randomnumber =="Ethnic",
                         case_when(
                           expideology == "Estrema destra" ~ 0,
                           expideology == "Destra" ~ 0,
                           expideology == "Centro-destra" ~ 0,
                           expideology == "Centro" ~ 0,
                           expideology == "Centro-sinistra" ~ 1,
                           expideology == "Sinistra" ~ 1,
                           expideology == "Estrema sinistra" ~ 1,
                           expideology == "Non saprei" ~ 0,
                           is.na(expideology) ~ NA
                         ),
                         # MEAT AND TRADITIONAL GROUP
                         case_when(
                           expideology == "Estrema destra" ~ 1,
                           expideology == "Destra" ~ 1,
                           expideology == "Centro-destra" ~ 1,
                           expideology == "Centro" ~ 0,
                           expideology == "Centro-sinistra" ~ 0,
                           expideology == "Sinistra" ~ 0,
                           expideology == "Estrema sinistra" ~ 0,
                           expideology == "Non saprei" ~ 0,
                           is.na(expideology) ~ NA
                         ) 
                  ))
  )

#### I should work more on the above recoding...


table(data$expideology_r3, data$expideology)

table(data$expideology_r3, data$group)

# RECODE NUMBER 4: LEFT VS RIGHT VS CENTER

data = data |>
  mutate(expideology, 
         expideology_r4 = case_when(
           expideology == "Estrema destra" ~ 1,
           expideology == "Destra" ~ 1,
           expideology == "Centro-destra" ~ 1,
           expideology == "Centro" ~ 0,
           expideology == "Centro-sinistra" ~ -1,
           expideology == "Sinistra" ~ -1,
           expideology == "Estrema sinistra" ~ -1,
           expideology == "Non saprei" ~ 999,
           is.na(expideology) ~ NA
         ) 
  )

table(data$expideology_r4, data$group)

## Recode NUMBER5: like recode number 1 but Don't know goes to the center


data = data |>
  mutate(expideology, 
         expideology_r5 = case_when(
           expideology == "Estrema destra" ~ 3,
           expideology == "Destra" ~ 2,
           expideology == "Centro-destra" ~ 1,
           expideology == "Centro" ~ 0,
           expideology == "Centro-sinistra" ~ -1,
           expideology == "Sinistra" ~ -2,
           expideology == "Estrema sinistra" ~ -3,
           expideology == "Non saprei" ~ 0,
           is.na(expideology) ~ NA
         ) 
  )


table(data$expideology, data$expideology_r5)

#RECODE NUMBER &: Right vs Left vs Center vs don't know

data = data |>
  mutate(expideology, 
         expideology_r6 = case_when(
           expideology == "Estrema destra" ~ "Right",
           expideology == "Destra" ~ "Right",
           expideology == "Centro-destra" ~ "Right",
           expideology == "Centro" ~ "Center",
           expideology == "Centro-sinistra" ~ "Left",
           expideology == "Sinistra" ~ "Left",
           expideology == "Estrema sinistra" ~ "Left",
           expideology == "Non saprei" ~ "Don't know",
           is.na(expideology) ~ NA
         ) 
  )
data$expideology_r6 = factor(data$expideology_r6, levels = c("Right", "Center", "Left", "Don't know"))

table(data$expideology, data$expideology_r6)

################ the same as expideology but translated in english

data = data |>
  mutate(expideology, 
         expideology_r7 = case_when(
           expideology == "Estrema destra" ~ "Extreme-right",
           expideology == "Destra" ~ "Right",
           expideology == "Centro-destra" ~ "Center-right",
           expideology == "Centro" ~ "Center",
           expideology == "Centro-sinistra" ~ "Center-left",
           expideology == "Sinistra" ~ "Left",
           expideology == "Estrema sinistra" ~ "Extreme-left",
           expideology == "Non saprei" ~ "Don't know",
           is.na(expideology) ~ NA
         ) 
  )
data$expideology_r7 = factor(data$expideology_r7, levels = c("Extreme-right", "Right", "Center-right", "Center",
                                                             "Center-left", "Left", "Extreme-left", "Don't know"))

table(data$expideology, data$expideology_r7)

###################### PARTISANSHIP      #####################



################# WHERE WOULD YOU PLACE THE VIGNETTE IDEOLOGICALLY
#We would do different recodes here. 

#Recode #0: translation

data = data |>
  mutate(expparty, 
         expparty_r0 = case_when(
           expparty == "Fratelli d'Italia" ~ "Brothers of Italy",
           expparty == "Lega" ~ "League",
           expparty == "Forza Italia" ~ "Go Italy",
           expparty == "Azione-Italia Viva" ~ "Action-Italy Alive",
           expparty == "Movimento 5 Stelle" ~ "Five Star Movement",
           expparty == "Partito Democratico" ~ "Democratic Party",
           expparty == "Alleanza Verdi-Sinistra" ~ "Green-Left Alliance",
           expparty == "Non saprei" ~ "Don't know",
           is.na(expparty) ~ NA
         ) 
  )
data$expparty_r0 = factor(data$expparty_r0, levels = c("Brothers of Italy", 
                                                       "League", 
                                                       "Go Italy",
                                                       "Action-Italy Alive",
                                                       "Five Star Movement",
                                                       "Democratic Party",
                                                       "Green-Left Alliance", "Don't know"))

table(data$expparty, data$expparty_r0)

#RECODE NUMBER 1: don't know goes missing

data = data |>
  mutate(expparty, 
         expparty_r1 = case_when(
           expparty == "Fratelli d'Italia" ~ 3,
           expparty == "Lega" ~ 2,
           expparty == "Forza Italia" ~ 1,
           expparty == "Azione-Italia Viva" ~ 0,
           expparty == "Movimento 5 Stelle" ~ -1,
           expparty == "Partito Democratico" ~ -2,
           expparty == "Alleanza Verdi-Sinistra" ~ -3,
           expparty == "Non saprei" ~ NA,
           is.na(expparty) ~ NA
         ) 
  )

table(data$expparty, data$expparty_r1)


data$perceived_outgroup_2 = ifelse(data$ideology_r5=="Right" & data$expparty_r1<0, 1,
                                 ifelse(data$ideology_r5 == "Left" & data$expparty_r1>0, 1, 0))

#RECODE NUMBER 2: don't know =0, other =1

data = data |>
  mutate(expparty, 
         party_PL = case_when(
           expparty == "Fratelli d'Italia" ~ 1,
           expparty == "Lega" ~ 1,
           expparty == "Forza Italia" ~ 1,
           expparty == "Azione-Italia Viva" ~ 1,
           expparty == "Movimento 5 Stelle" ~ 1,
           expparty == "Partito Democratico" ~ 1,
           expparty == "Alleanza Verdi-Sinistra" ~ 1,
           expparty == "Non saprei" ~ 0,
           is.na(expparty) ~ NA
         ) 
  )

table(data$expparty, data$party_PL)


######### RECODE NUMBER 3: EXPECTED VS UNEXPECTED DIRECTION

data = data |>
  mutate(expparty, 
         expparty_r3 = 
           ifelse(data$group == "Control", 
                  #CONTROL GROUP : Azione-Italia Viva = 1 O = 0?
                  case_when(
                    expparty == "Fratelli d'Italia" ~ 0,
                    expparty == "Lega" ~ 0,
                    expparty == "Forza Italia" ~ 0,
                    expparty == "Azione-Italia Viva" ~ 0,
                    expparty == "Movimento 5 Stelle" ~ 0,
                    expparty == "Partito Democratico" ~ 0,
                    expparty == "Alleanza Verdi-Sinistra" ~ 0,
                    expparty == "Non saprei" ~ 1,
                    is.na(expparty) ~ NA
                  ),
                  # VEGAN AND ETHNIC GROUP
                  ifelse(data$group == "Vegan" | data$group == "Ethnic",
                         case_when(
                           expparty == "Fratelli d'Italia" ~ 0,
                           expparty == "Lega" ~ 0,
                           expparty == "Forza Italia" ~ 0,
                           expparty == "Azione-Italia Viva" ~ 0,
                           expparty == "Movimento 5 Stelle" ~ 1,
                           expparty == "Partito Democratico" ~ 1,
                           expparty == "Alleanza Verdi-Sinistra" ~ 1,
                           expparty == "Non saprei" ~ 0,
                           is.na(expparty) ~ NA
                         ),
                         # MEAT AND TRADITIONAL GROUP
                         case_when(
                           expparty == "Fratelli d'Italia" ~ 1,
                           expparty == "Lega" ~ 1,
                           expparty == "Forza Italia" ~ 1,
                           expparty == "Azione-Italia Viva" ~ 0,
                           expparty == "Movimento 5 Stelle" ~ 0,
                           expparty == "Partito Democratico" ~ 0,
                           expparty == "Alleanza Verdi-Sinistra" ~ 0,
                           expparty == "Non saprei" ~ 0,
                           is.na(expparty) ~ NA
                         ) 
                  ))
  )

table(data$expparty, data$expparty_r3)


# RECODE NUMBER 4/5/6: yet to do
 ######### yet to do



###### would grab coffee with character in the vignette (behavioral AP)
# linear recoding

data = data |>
  mutate(expbehav, 
         expbehav_r = case_when(
           expbehav == "Molto probabile" ~ "Coffee Yes",
           expbehav == "Probabile" ~ "Coffee Yes",
           expbehav == "Né probabile n* improbabile" ~ "Irrelevant",
           expbehav == "Poco probabile" ~ "Coffee No",
           expbehav == "Per nulla probabile" ~ "Coffee No",
           is.na(expbehav) ~ NA
         ) 
  )

data$expbehav_r = factor(data$expbehav_r, levels = c("Coffee No", "Irrelevant", "Coffee Yes"))

table(data$expbehav, data$expbehav_r)

# recode as dummy per logit


data = data |>
  mutate(expbehav, 
         interaction = case_when(
           expbehav == "Molto probabile" ~ 1,
           expbehav == "Probabile" ~ 1,
           expbehav == "Né probabile né improbabile" ~ 0,
           expbehav == "Poco probabile" ~ 1,
           expbehav == "Per nulla probabile" ~ 1,
           is.na(expbehav) ~ NA
         ) 
  )


table(data$expbehav, data$interaction)


data = data |>
  mutate(expbehav, 
         expbehav_r1 = case_when(
           expbehav == "Molto probabile" ~ 2,
           expbehav == "Probabile" ~ 1,
           expbehav == "Né probabile né improbabile" ~ 0,
           expbehav == "Poco probabile" ~ -1,
           expbehav == "Per nulla probabile" ~ -2,
           is.na(expbehav) ~ NA
         ) 
  )

table(data$expbehav, data$expbehav_r1)

data = data |>
  mutate(expbehav, 
         expbehav_r2 = case_when(
           expbehav == "Molto probabile" ~ 1,
           expbehav == "Probabile" ~ 1,
           expbehav == "Né probabile né improbabile" ~ 0,
           expbehav == "Poco probabile" ~ -1,
           expbehav == "Per nulla probabile" ~ -1,
           is.na(expbehav) ~ NA
         ) 
  )

table(data$expbehav, data$expbehav_r2)

data = data |>
  mutate(expbehav, 
         expbehav_r2ord = case_when(
           expbehav == "Molto probabile" ~ "Likely",
           expbehav == "Probabile" ~ "Likely",
           expbehav == "Né probabile né improbabile" ~ "Neither Likely nor Unlikely",
           expbehav == "Poco probabile" ~ "Unlikely",
           expbehav == "Per nulla probabile" ~ "Unlikely",
           is.na(expbehav) ~ NA
         ) 
  )

data$expbehav_r2ord = factor(data$expbehav_r2ord, levels = c("Unlikely", "Neither Likely nor Unlikely",
                                                             "Likely"))
table(data$expbehav, data$expbehav_r2ord)


##### political conversation with the character in the vignette 
# would be pleasant (linear recoding)

data = data |>
  mutate(expconv, 
         expconv_r1 = case_when(
           expconv == "Molto probabile" ~ 2,
           expconv == "Probabile" ~ 1,
           expconv == "Né probabile né improbabile" ~ 0,
           expconv == "Poco probabile" ~ -1,
           expconv == "Per niente probabile" ~ -2,
           is.na(expconv) ~ NA
         ) 
  )



table(data$expconv, data$expconv_r1)

data = data |>
  mutate(expconv, 
         expconv_r2 = case_when(
           expconv == "Molto probabile" ~ 1,
           expconv == "Probabile" ~ 1,
           expconv == "Né probabile né improbabile" ~ 0,
           expconv == "Poco probabile" ~ -1,
           expconv == "Per niente probabile" ~ -1,
           is.na(expconv) ~ NA
         ) 
  )

#####

table(data$expconv, data$expconv_r2)

data = data |>
  mutate(expconv, 
         expconv_r2ord = case_when(
           expconv == "Molto probabile" ~ "Likely",
           expconv == "Probabile" ~ "Likely",
           expconv == "Né probabile né improbabile" ~ "Neither Likely nor Unlikely",
           expconv == "Poco probabile" ~ "Unlikely",
           expconv == "Per niente probabile" ~ "Unlikely",
           is.na(expconv) ~ NA
         ) 
  )

data$expconv_r2ord = factor(data$expconv_r2ord, levels = c("Unlikely", "Neither Likely nor Unlikely",
                                                           "Likely"))


table(data$expconv, data$expconv_r2ord)

### recode as dummy


data = data |>
  mutate(expconv, 
         conversation = case_when(
           expconv == "Molto probabile" ~ 1,
           expconv == "Probabile" ~ 1,
           expconv == "Né probabile né improbabile" ~ 0,
           expconv == "Poco probabile" ~ 1,
           expconv == "Per niente probabile" ~ 1,
           is.na(expconv) ~ NA
         ) 
  )



table(data$expconv, data$conversation)

### I create the main IV of the models for interaction and conversation:
# The interaction between Party PL and Ideology PL (both, only one, none)

data$PL_full=
  ifelse(data$party_PL == 1 & data$ideology_PL ==1,
         "Both PL",
         ifelse(data$party_PL==1,
                "Only partisan PL",
                ifelse(data$ideology_PL==1,
                       "Only ideological PL",
                       "No PL")))

data$PL_full = factor(data$PL_full, levels=c("No PL", "Only ideological PL", "Only partisan PL", "Both PL"))

table(data$PL_full)
########## Response that people who didn't place the character not ideologically
#not by party had to provide... This recoding should be adjested and it's not
# definitive.


data = data |>
  mutate(opennone, 
         opennone_r = case_when(
           opennone == "I suoi gusti sono troppo comuni/generici, potrebbero significare qualsiasi cosa" ~ 2,
           opennone == "I gusti alimentari di una persona non hanno nessun legame con le sue opinioni politiche" ~ 1,
           opennone == "Non mi interessa conoscere le posizioni politiche delle altre persone" ~ 0,
           opennone == "Altro (specificare nel commento)" ~ -1,
           is.na(opennone) & !is.na(opennone_comment) ~ -1
         ) 
  )

table(data$opennone, data$opennone_r)

data$open = ifelse(!is.na(data$opennone_comment), data$opennone_comment,
                   ifelse(!is.na(data$openideology), data$openideology,
                          ifelse(!is.na(data$openparty), data$openparty, NA)))


############ Out of simple curiosity and for future QTA

data[!is.na(data$openideology), ]$openideology
data[!is.na(data$openparty), ]$openparty
data[!is.na(data$opennone_comment), ]$opennone_comment


### Notice: almost 600 responded to openparty
# 27 to open ideology
# i think there is margin for doing a QTA...

################# WHERE WOULD YOU PLACE THE ***STATIC****VIGNETTE IDEOLOGICALLY
#We would do different recodes here. 

data = data |>
  mutate(statideology, 
         statideology_r1 = case_when(
           statideology == "Estrema destra" ~ 3,
           statideology == "Destra" ~ 2,
           statideology == "Centro-destra" ~ 1,
           statideology == "Centro" ~ 0,
           statideology == "Centro-sinistra" ~ -1,
           statideology == "Sinistra" ~ -2,
           statideology == "Estrema sinistra" ~ -3,
           statideology == "Non saprei" ~ NA,
           is.na(statideology) ~ NA
         ) 
  )

table(data$statideology, data$statideology_r1)
#recode something vs don't know 

data = data |>
  mutate(statideology, 
         statideology_r2 = case_when(
           statideology == "Estrema destra" ~ 1,
           statideology == "Destra" ~ 1,
           statideology == "Centro-destra" ~ 1,
           statideology == "Centro" ~ 1,
           statideology == "Centro-sinistra" ~ 1,
           statideology == "Sinistra" ~ 1,
           statideology == "Estrema sinistra" ~ 1,
           statideology == "Non saprei" ~ 0,
           is.na(statideology) ~ NA
         ) 
  )

######### recode expected vs unexpeced & don't know STATIC VIGNETTE

data = data |>
  mutate(statideology, 
         statideology_r3 = case_when(
           statideology == "Estrema destra" ~ "Extreme right",
           statideology == "Destra" ~ "Right",
           statideology == "Centro-destra" ~ "Center-right",
           statideology == "Centro" ~ "Center",
           statideology == "Centro-sinistra" ~ "Center-left",
           statideology == "Sinistra" ~ "Left",
           statideology == "Estrema sinistra" ~ "Extreme left",
           statideology == "Non saprei" ~ "Don't know",
           is.na(statideology) ~ NA
         ) 
  )

data$statideology_r3 = factor(data$statideology_r3, 
                              levels = c("Extreme left", "Left", "Center-left", "Center",
                                         "Center-right", "Right", "Extreme right", "Don't know"))

table(data$statideology, data$statideology_r3)



data = data |>
  mutate(statideology, 
         statideology_r4 = case_when(
           statideology == "Estrema destra" ~ "Right",
           statideology == "Destra" ~ "Right",
           statideology == "Centro-destra" ~ "Right",
           statideology == "Centro" ~ "Center",
           statideology == "Centro-sinistra" ~ "Left",
           statideology == "Sinistra" ~ "Left",
           statideology == "Estrema sinistra" ~ "Left",
           statideology == "Non saprei" ~ "Don't know",
           is.na(statideology) ~ NA
         ) 
  )

data$statideology_r4 = factor(data$statideology_r4, 
                              levels = c("Left", "Center",
                                         "Right", "Don't know"))
table(data$statideology, data$statideology_r4)


### recode statparty as dummy


data = data |>
  mutate(statparty, 
         statparty_r2 = case_when(
           statparty == "Fratelli d'Italia" ~ 1,
           statparty == "Lega" ~ 1,
           statparty == "Forza Italia" ~ 1,
           statparty == "Azione-Italia Viva" ~ 1,
           statparty == "Movimento 5 Stelle" ~ 1,
           statparty == "Partito Democratico" ~ 1,
           statparty == "Alleanza Verdi-Sinistra" ~ 1,
           statparty == "Non saprei" ~ 0,
           is.na(statparty) ~ NA
         ) 
  )


table(data$statparty, data$statparty_r2)

data = data |>
  mutate(statparty, 
         statparty_r3 = case_when(
           statparty == "Fratelli d'Italia" ~ "Brothers of Italy",
           statparty == "Lega" ~ "League",
           statparty == "Forza Italia" ~ "Go Italy",
           statparty == "Azione-Italia Viva" ~ "Action-Italy Alive",
           statparty == "Movimento 5 Stelle" ~ "Five Star Movement",
           statparty == "Partito Democratico" ~ "Democratic Party",
           statparty == "Alleanza Verdi-Sinistra" ~ "Green-Left Alliance",
           statparty == "Non saprei" ~ "Don't know",
           is.na(statparty) ~ NA
         ) 
  )

data$statparty_r3 = factor(data$statparty_r3, levels = c("Green-Left Alliance",
                                                         "Democratic Party",
                                                         "Five Star Movement",
                                                         "Action-Italy Alive",
                                                         "Go Italy",
                                                         "League", 
                                                         "Brothers of Italy",
                                                         "Don't know"))




table(data$statparty, data$statparty_r3)


####### BEHAVIORAL AP STATIC VIGNETTE


data = data |>
  mutate(statbehav, 
         statbehav_r = case_when(
           statbehav == "Molto probabile" ~ 2,
           statbehav == "Probabile" ~ 1,
           statbehav == "Né probabile né improbabile" ~ 0,
           statbehav == "Poco probabile" ~ -1,
           statbehav == "Per nulla probabile" ~ -2,
           is.na(statbehav) ~ NA
         ) 
  )

table(data$statbehav, data$statbehav_r)


data = data |>
  mutate(statbehav, 
         statbehav_r2 = case_when(
           statbehav == "Molto probabile" ~ 1,
           statbehav == "Probabile" ~ 1,
           statbehav == "Né probabile né improbabile" ~ 0,
           statbehav == "Poco probabile" ~ 1,
           statbehav == "Per nulla probabile" ~ 1,
           is.na(statbehav) ~ NA
         ) 
  )

table(data$statbehav, data$statbehav_r2)
### Willingness to have a political conversation with static vignette 

data = data |>
  mutate(statconv, 
         statconv_r = case_when(
           statconv == "Molto probabile" ~ 2,
           statconv == "Probabile" ~ 1,
           statconv == "Né probabile né improbabile" ~ 0,
           statconv == "Poco probabile" ~ -1,
           statconv == "Per niente probabile" ~ -2,
           is.na(statconv) ~ NA
         ) 
  )

table(data$statconv, data$statconv_r)

## the indipendent variable for ordered logit (Friend pl, enemy pl, no pl)

data$friendPL= ifelse(is.na(data$expideology_r1), #R does not do ideological PL
                      0, #both left wing or both not left wing,
                      ifelse(data$ideology_r>4 | is.na(data$ideology_r),
                             ifelse(data$expideology_r1>=0, 1, -1),
                             ifelse(data$expideology_r1>=0, -1, 1))) #both left wing or both not left wing (including nowhere)


table(data$friendPL)
########################## removing potential outliers ########################

data = data |> select(!starts_with("Tempo"))


####### I have spotted a liar. 6 respondents have repspondent to the questionnaire
# with almost the exact response set, including open questions.
#This led me to think that I should remove those responses,

#Here I show how I manage to spot the liar

spotliars = data |>
  filter(!is.na(openparty)) |>
  group_by(openparty) |>
  summarise(n=n())

spotliars = spotliars |>
  filter(n>1)

################### spot liars culturalindex

spotliars1 = data |>
  filter(culturalindex==4)

#These are the responses
data[grep("Efficiente", data$openparty), ]


# I remove the person with 6 different accounts from the my data. 

data = data[-grep("Efficiente", data$openparty), ]

# I remove the people that respnded casually at cultural questions from the sample

#data = data[data$culturalindex != 4, ]



######################## imputation of missing in variable socia_position_r

#by commenting the following lines, social_position_r_noNA will be the one at the mean

predictive_model = lm(data=data, social_position_r ~ sex + educ + citysize +
                        statbehav+ interest + exposure + ideology_SQ001+ taste_sshm+taste_rstt+
                        taste_brgr + taste_plpt + taste_pstn + taste_crrt + taste_ltds+
                        age_r + region + openness  + 
                        conscientiousness + extraversion + 
                        neuroticism + agreeableness + statideology +
                        +income+job1+pareduc+pdr+mdi+nmp+familyknows+
                        friendsknows+statconv+ cultural_cnma +
                        cultural_sptt + cultural_mnmt +cultural_prtc+feelterm_fdi+
                        feelterm_lega + feelterm_fi + feelterm_aziv + feelterm_m5s+
                        feelterm_pd + feelterm_avs + ideology_PL + interaction#bigfive
)

summary(predictive_model)

#t1 <- tbl_regression(predictive_model, include = "sex")


data$social_position_r_noNA = data$social_position_r

pred_df = data[is.na(data$social_position_r), c("sex", "educ", "citysize", 
                                                 "statbehav",
                                                 "interest","exposure", 
                                                 "ideology_SQ001", "taste_sshm",
                                                 "taste_rstt","taste_brgr", 
                                                 "taste_plpt", "taste_pstn",
                                                 "taste_crrt", "taste_ltds",
                                                 "age_r", "region", "openness",
                                                 "conscientiousness", 
                                                 "extraversion","neuroticism", 
                                                 "agreeableness", "statideology",
                                                 "income", "job1", "pareduc", 
                                                 "pdr", "mdi" ,"nmp", "familyknows", "friendsknows",
                                                 "statconv", "cultural_cnma",
                                                  "cultural_sptt", "cultural_mnmt", "cultural_prtc", "feelterm_fdi",
                                                  "feelterm_lega", "feelterm_fi", "feelterm_aziv", "feelterm_m5s",
                                                  "feelterm_pd", "feelterm_avs", "ideology_PL", "interaction")]

predictions = predict(predictive_model, pred_df)

data[is.na(data$social_position_r_noNA), ]$social_position_r_noNA = predictions



sum(is.na(data$social_position_r_noNA)) # la media è comunque sei 

#i adjust values higher than 10 or lower than zero!
data$social_position_r_noNA = ifelse(data$social_position_r_noNA>10, 
       10,
       ifelse(data$social_position_r_noNA<0, 
              0,
              data$social_position_r_noNA))

# I round to the closer integer (the scale is integer)
data$social_position_r_noNA= round(data$social_position_r_noNA, digits = 0)


#######################################################################


# I create a dummy called "similarity" that tells me if the respondent perceive
# the vignette's ideology to be similar as his. In case the respondent doesn' t
# declare an ideology (nowhere) i say he has nothing to project so saying that he doesn't 
# know where the vignette's character would stand (which is different from: he stands nowhere)
# is not a preception of similarity
data$similarity_ideology_exp = ifelse(data$ideology_r<5 & !is.na(data$ideology_r) & grepl("sinistra", data$expideology, ignore.case = T),
                         1,
                         ifelse(data$ideology_r>5 & !is.na(data$ideology_r) & grepl("destra", data$expideology, ignore.case = T), 1,
                                ifelse(data$ideology_r==5 & !is.na(data$ideology_r) & grepl("centro", data$expideology, ignore.case = T), 1, 0)))

data$similarity_ideology_exp_factor = ifelse(data$ideology_r<5 & !is.na(data$ideology_r) & grepl("sinistra", data$expideology, ignore.case = T),
                                "Similar",
                                ifelse(data$ideology_r>5 & !is.na(data$ideology_r) & grepl("destra", data$expideology, ignore.case = T), "Similar",
                                       ifelse(data$ideology_r==5 & !is.na(data$ideology_r) & grepl("centro", data$expideology, ignore.case = T), "Similar", "Not similar")))

#I compute the index fror party_similarity with andrea and with the character
# in the vignette. I say that the respondent deems andrea or the character
# to be similar to them if they impute to them of their favorite parties
data$similarity_party_stat=0
i=1
while(i<=nrow(data))
{
data$similarity_party_stat[i] = ifelse(grepl(data$statparty_r3[i], data$max_party_r[i]), 1, 0)
i=i+1
}

table(data$similarity_party_stat)


data$similarity_party_exp=0
i=1
while(i<=nrow(data))
{
  data$similarity_party_exp[i] = ifelse(grepl(data$expparty_r0[i], data$max_party_r[i]), 1, 0)
  i=i+1
}

table(data$similarity_party_exp)

#I save the data

saveRDS(data, "FFPT_tidy.RDS")














# proviamo anche a fare una doppia ricodifica per un effetto di interazione
# creo una variabile "collocated" che è una dummy che dice se uno si colloca e poi
# la faccio interagire con il quadrato dell'ideologia centrata
# PROVO ANCHE CON LE PREDIZIONI
# CHIEDIAMO A MANCOSU SE PER CASO C'ERA UNA DOMANDA SU GALTAN



# ALLA PROF SEMBRAVANO UNA PROXY DELLA TENDENZA DEL SOGGETTO A RAGIONRE DI POLITICA, SE IO NON HO IDEA DI COME
# VOTANO I MIEI FAMILIARI VUOL DIRE CHE  NON CI HO MAI PARLATO DI POLITICA E NON MI è VANUTA LA CURIOSIOT?
# DI INFERIRE L'ORIENTAMENTO POLITICA

# Dovrei fare dei modelli con behavioural ap (caffee) e expconv alla
# luce della distnza ideologica del rispondente... Quindi con quelle due come Y