######### DESCRIPTIVES FFPT


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

main_path = "C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/Papers and Chapters/FFPT"
setwd(main_path)
data = import(paste0(main_path, "/data_manipulation/FFPT_tidy.RDS"))

setwd(paste0(main_path, "/descriptives/output"))
datanomiss = data[!is.na(data$sex_r) & !is.na(data$citysize_r2) & !is.na(data$macroarea2), ]

####################### descriptive tables of the sample

descriptive <- datanomiss |> 
  select(sex_r, age_r, macroarea2, educ_r, citysize_r2, social_position_r_noNA) |>
  tbl_summary(
    missing = "ifany",
    digits = everything() ~ 1, #every statistic shows 1 decimal place
    #  type = all_continuous() ~ "continuous2",
    # statistic = all_continuous() ~ c("{mean}", "{min}", "{max}"),
    label = list(
      sex_r ~ "Sex",
      age_r ~ "Age",
      macroarea2 ~ "Macroarea",
      educ_r ~ "Education",
      citysize_r2 ~ "Size of the city",
      social_position_r_noNA ~ "Placement on 0-10 social scale"
    )
  )

#export to word

descriptive |>
  as_gt() |>
  gtsave(filename = "Descriptive.docx")


descriptivepergroup <- datanomiss |> 
  select(group, sex_r, age_r, macroarea2, educ_r, citysize_r2, social_position_r_noNA) |>
  tbl_summary(
    by="group",
    missing = "ifany",
    digits = everything() ~ 1, #every statistic shows 1 decimal place
    #  type = all_continuous() ~ "continuous2",
    # statistic = all_continuous() ~ c("{mean}", "{min}", "{max}"),
    label = list(
      sex_r ~ "Sex",
      age_r ~ "Age",
      macroarea2 ~ "Macroarea",
      educ_r ~ "Education",
      citysize_r2 ~ "Size of the city",
      social_position_r_noNA ~ "Placement on 0-10 social scale"
    )
  )

descriptivepergroup |>
  as_gt() |>
  gtsave(filename = "Descriptivepergrop.docx")



############ descriptive table of the IV variable in the models


descriptive2 <- datanomiss |> 
  select(AP_wagner_spread2, knowsparty, #polarizzazione
         interest_r, exposure_r1, culturalindex, collocated, ideology_r2,#politica
         conscientiousness, openness, agreeableness,
         extraversion, neuroticism) |>
  tbl_summary(
    missing = "ifany",
    digits = everything() ~ 1, #every statistic shows 1 decimal place
    type = list(conscientiousness ~ "continuous",
                openness ~ "continuous",
                agreeableness ~ "continuous",
                extraversion ~ "continuous",
                neuroticism ~ "continuous"),
    label = list(
      AP_wagner_spread2 ~ "Aff. pol",
      knowsparty ~ "# Parties known",
      interest_r ~ "High political interest", 
      exposure_r1 ~ "High news exposure",
      culturalindex ~ "Index of cult. cons",
      collocated ~ "Ideologically collocated",
      ideology_r2 ~ "Ideology",#politica
      conscientiousness ~ "Conscientiousness",
      openness ~  "Openness",
      agreeableness ~ "Agreeableness",
      extraversion ~ "Extraversion",
      neuroticism ~ "Neuroticism"
    )
  )

descriptive2 |>
  as_gt() |>
  gtsave(filename = "Descriptive2.docx")


descriptivepergroup2 <- datanomiss |> 
  select(group, AP_wagner_spread2, knowsparty, #polarizzazione
         interest_r, exposure_r1, culturalindex, collocated, ideology_r2,#politica
         conscientiousness, openness, agreeableness,
         extraversion, neuroticism) |>
  tbl_summary(
    by="group",
    missing = "ifany",
    digits = everything() ~ 1, #every statistic shows 1 decimal place
    type = list(conscientiousness ~ "continuous",
                openness ~ "continuous",
                agreeableness ~ "continuous",
                extraversion ~ "continuous",
                neuroticism ~ "continuous"),
    label = list(
      AP_wagner_spread2 ~ "Aff. pol",
      knowsparty ~ "# Parties known",
      interest_r ~ "High political interest", 
      exposure_r1 ~ "High news exposure",
      culturalindex ~ "Index of cult. cons",
      collocated ~ "Ideologically collocated",
      ideology_r2 ~ "Ideology",#politica
      conscientiousness ~ "Conscientiousness",
      openness ~  "Openness",
      agreeableness ~ "Agreeableness",
      extraversion ~ "Extraversion",
      neuroticism ~ "Neuroticism"
    )
  )

descriptivepergroup2 |>
  as_gt() |>
  gtsave(filename = "descriptivepergroup2.docx")


######################################################

# Descriptive tables of DV in the models


descriptive3 <- datanomiss |> 
  select(party_PL, ideology_PL, interaction, conversation) |>
  tbl_summary(
    missing = "ifany",
    digits = everything() ~ 1, #every statistic shows 1 decimal place
    #  type = all_continuous() ~ "continuous2",
    # statistic = all_continuous() ~ c("{mean}", "{min}", "{max}"),
    label = list(
      party_PL ~ "Party PL=1", 
      ideology_PL ~ "Ideology PL=1",
      interaction ~ "Interaction=1",
      conversation ~ "Conversation=1"
      
    )
  )

#export to word
descriptive3 |>
  as_gt() |>
  gtsave(filename = "Descriptive3.docx")




descriptivepergroup3 <- datanomiss |> 
  select(group, party_PL, ideology_PL, interaction, conversation) |>
  tbl_summary(
    by="group",
    missing = "ifany",
    digits = everything() ~ 1, #every statistic shows 1 decimal place
    #  type = all_continuous() ~ "continuous2",
    # statistic = all_continuous() ~ c("{mean}", "{min}", "{max}"),
    label = list(
      party_PL ~ "Party PL=1", 
      ideology_PL ~ "Ideology PL=1",
      interaction ~ "Interaction=1",
      conversation ~ "Conversation=1"
      
    )
  )

#export to word
descriptivepergroup3 |>
  as_gt() |>
  gtsave(filename = "Descriptivepergroup3.docx")






#############################################################
################### free responses exploration

toprint = datanomiss |> 
  filter(!is.na(open)) |>
  select(ideology_SQ001, group, expideology, expparty, open)

write.xlsx(toprint, "Open Answers.xlsx")

######################## Free responses descriptives

#Tutte le risposte non vuote
setwd(main_path)
data=import("Open_answers_coded.xlsx")
setwd(paste0(main_path, "/descriptives/output"))

data$FN = ifelse(grepl("Ideology", data$Note), gsub("O","I",data$FN), data$FN)
data$GS = ifelse(grepl("Ideology", data$Note), gsub("O","I",data$GS), data$GS)

#Rimuovo quelle che sono insensate,inintelleggibili, prive di qualunque valore

data_no_N = data |> 
  filter(GS!="N")

datanomiss = data_no_N |>
  filter(GS!="." & FN !=".")

#togliamo i casi in cui io e Fedra siamo discordi

####funzione che mi dice se condividiamo caratteri

shareAnyCharacters <- function(str1, str2) {
  # Split the strings into characters
  chars1 <- strsplit(str1, NULL)[[1]]
  chars2 <- strsplit(str2, NULL)[[1]]
  
  # Check if there are any common characters using the 'any' function
  common_chars <- any(chars1 %in% chars2) | any(chars2 %in% chars1)
  
  return(common_chars)
}

i=1
diversi1 = rep(0,nrow(datanomiss))
while(i<=nrow(datanomiss))
{
  if(!shareAnyCharacters(datanomiss$GS[i], datanomiss$FN[i]))
    diversi1[i] = 1
  i=i+1
}

diversi1_df = datanomiss[which(diversi1==1), ]

i=1
diversi2 = rep(0,nrow(datanomiss))
while(i<=nrow(datanomiss))
{
  if(!is.na(datanomiss$GS2[i]) & !is.na(datanomiss$FN2[i]) & !shareAnyCharacters(datanomiss$GS2[i], datanomiss$FN2[i]))
    diversi2[i] = 1
  i=i+1
}

diversi2_df = datanomiss[which(diversi2==1), ]
diversi2_df = datanomiss[(datanomiss$GS2 != datanomiss$FN2) & !is.na(datanomiss$GS2 ) & !is.na(datanomiss$FN2),]

nrow(datanomiss[grepl("A", datanomiss$GS) & grepl("A", datanomiss$FN),])

#CASI IN CUI IO E FEDRA SIAMO D'ACCORDO SU ALMENO UNA DELLE CLASSIFICAZIONI NELLA COLONNA 1
datanomiss = datanomiss[(grepl("P", datanomiss$GS) & grepl("P", datanomiss$FN)) | 
             (grepl("I", datanomiss$GS) & grepl("I", datanomiss$FN)) |
             (grepl("O", datanomiss$GS) & grepl("O", datanomiss$FN)) |
             (grepl("G", datanomiss$GS) & grepl("G", datanomiss$FN)) | 
             (grepl("A", datanomiss$GS) & grepl("A", datanomiss$FN)), ]

#CASI IN CUI IO E FEDRA SIAMO D'ACCORDO SU ALMENO UNA DELLE CLASSIFICAZIONI NELLA COLONNA 1

datanomiss_2 = datanomiss[(grepl("P", datanomiss$GS2) & grepl("P", datanomiss$FN2)) | 
                  (grepl("C", datanomiss$GS2) & grepl("C", datanomiss$FN2)) |
                  (grepl("E", datanomiss$GS2) & grepl("E", datanomiss$FN2)) |
                  (grepl("R", datanomiss$GS2) & grepl("R", datanomiss$FN2)) | 
                  (grepl("O", datanomiss$GS2) & grepl("O", datanomiss$FN2)), ]




df_descr_OR = data.frame("Projection" = sum(grepl("P", datanomiss$GS) | grepl("P", datanomiss$FN)),
                        "Stereotype"= sum(grepl("A", datanomiss$GS) | grepl("A", datanomiss$FN)),
                        "Issue"= sum(grepl("I", datanomiss$GS) | grepl("I", datanomiss$FN)),
                        "Gut"= sum(grepl("G", datanomiss$GS) | grepl("G", datanomiss$FN)),
                        "Other"= sum(grepl("O", datanomiss$GS) | grepl("O", datanomiss$FN)))

sum(df_descr_OR)

df_descr_OR = data.frame("inference" = c("Projection", "Stereotype", "Issue", "Gut", "Other"),
                          "cases" = c(df_descr_OR$Projection,df_descr_OR$Stereotype,df_descr_OR$Issue,
                                      df_descr_OR$Gut, df_descr_OR$Other))

df_descr_OR

df_descr_AND = data.frame("Projection" = sum(grepl("P", datanomiss$GS) & grepl("P", datanomiss$FN)),
                         "Stereotype"= sum(grepl("A", datanomiss$GS) & grepl("A", datanomiss$FN)),
                         "Issue"= sum(grepl("I", datanomiss$GS) & grepl("I", datanomiss$FN)),
                         "Gut"= sum(grepl("G", datanomiss$GS) & grepl("G", datanomiss$FN)),
                         "Other"= sum(grepl("O", datanomiss$GS) & grepl("O", datanomiss$FN)))

sum(df_descr_AND)

df_descr_AND = data.frame("inference" = c("Projection", "Stereotype", "Issue", "Gut", "Other"),
                          "cases" = c(df_descr_AND$Projection,df_descr_AND$Stereotype,df_descr_AND$Issue,
                                      df_descr_AND$Gut, df_descr_AND$Other))

sum(table(datanomiss_2$GS2))
sum(table(datanomiss_2$FN2))



df_descr_ASS_OR = data.frame("Food" = sum(grepl("C", datanomiss_2$GS2) | grepl("C", datanomiss_2$FN2)),
                              "Economic" = sum(grepl("E", datanomiss_2$GS2) | grepl("E", datanomiss_2$FN2)),
                              "Personality" = sum(grepl("P", datanomiss_2$GS2) | grepl("P", datanomiss_2$FN2)),
                              "Regional" = sum(grepl("R", datanomiss_2$GS2) | grepl("R", datanomiss_2$FN2)),
                              "Other" = sum(grepl("O", datanomiss_2$GS2) | grepl("O", datanomiss_2$FN2)))
sum(df_descr_ASS_OR)


df_descr_ASS_OR = data.frame("type" = c("Food", "Economic", "Personality", "Regional", "Other"),
                            "cases" = c(df_descr_ASS_OR$Food,df_descr_ASS_OR$Economic,df_descr_ASS_OR$Personality,
                                        df_descr_ASS_OR$Regional, df_descr_ASS_OR$Other))

sum(df_descr_ASS_OR$cases)

df_descr_ASS_AND = data.frame("Food" = sum(grepl("C", datanomiss_2$GS2) & grepl("C", datanomiss_2$FN2)),
                            "Economic" = sum(grepl("E", datanomiss_2$GS2) & grepl("E", datanomiss_2$FN2)),
                            "Personality" = sum(grepl("P", datanomiss_2$GS2) & grepl("P", datanomiss_2$FN2)),
                            "Regional" = sum(grepl("R", datanomiss_2$GS2) & grepl("R", datanomiss_2$FN2)),
                            "Other" = sum(grepl("O", datanomiss_2$GS2) & grepl("O", datanomiss_2$FN2)))
                            
sum(df_descr_ASS_AND)

df_descr_ASS_AND = data.frame("type" = c("Food", "Economic", "Personality", "Regional"),
                             "cases" = c(df_descr_ASS_AND$Food, df_descr_ASS_AND$Economic,df_descr_ASS_AND$Personality,
                                         df_descr_ASS_AND$Regional))

sum(df_descr_ASS_AND$cases)


#######Graphs

p2=ggplot(df_descr_AND, aes(x=inference, y=cases))+
  geom_col(aes(fill=inference), show.legend = F)+
  scale_y_continuous(breaks=seq(0,300,by=30))+
  xlab("Source of inference")+
  ylab("Number of cases")
  

p2

df_descr_AND$perc = round(df_descr_AND$cases/sum(df_descr_AND$cases)*100, digits = 2)

ggsave("Barplot type of Inference AND.png", width = 8, height = 8)

p3=ggplot(df_descr_ASS_AND, aes(x=type, y=cases))+
  geom_col(aes(fill=type), show.legend = F)+
  scale_y_continuous(breaks=seq(0,160,by=20))+
  xlab("Type of stereotype")+
  ylab("Number of cases")+
  labs(type = "Type of Inference")
 

p3

ggsave("Barplot type of stereotype AND.png", width = 8, height = 8)

df_descr_ASS_AND$perc = round(df_descr_ASS_AND$cases/sum(df_descr_ASS_AND$cases)*100, digits = 2)

sum(df_descr_ASS_AND$perc)

p2/p3


  

ggsave("Barplot open answers full.png", width = 8)

p2SLIDES=ggplot(df_descr_AND, aes(x=inference, y=cases))+
  geom_col(aes(fill=inference))+
  scale_y_continuous(breaks=seq(0,300,by=30))+
  labs(fill = "\nSource of Inference")+ 
  ylab("Number of cases")+
  theme(legend.key.size = unit(1.5, 'cm'), #change legend key size
        legend.key.height = unit(1.2, 'cm'), #change legend key height
        #legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=15)) #change legend text font size

p3SLIDES=ggplot(df_descr_ASS_AND, aes(x=type, y=cases))+
  geom_col(aes(fill=type), show.legend = T)+
  scale_y_continuous(breaks=seq(0,160,by=20))+
  labs(fill = "Type of Stereotype")+ 
  ylab("Number of cases")+
  labs(type = "Type of Inference")+
  theme(legend.key.size = unit(1.5, 'cm'), #change legend key size
        legend.key.height = unit(1.5, 'cm'), #change legend key height
        #legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=15)) 


p2SLIDES/p3SLIDES
ggsave("Barplot open answers fullFORSLIDES.png", scale = 1.5)
ggsave("Barplot open answers fullFORSLIDES.png", width = 8, height = 6)


#select epic phrases

epic = datanomiss[!is.na(datanomiss$`Frase_iconica (METTERE X lowercase)`), ]

View(epic)
epic$Open




#####IMMONDIZIA

data = import(paste0(main_path, "/data_manipulation/FFPT_tidy.RDS"))
aus = as.data.frame(round(prop.table(table(data$ideology_SQ001))*100, digits=2))
aus = cbind(aus, paste0(aus$Var1, " (", aus$Freq, "%)"))                   
aus                    
export(aus, "stat.xlsx")

aus = as.data.frame(round(prop.table(table(datanomiss$exposure))*100, digits=2))
aus

aus = as.data.frame(round(prop.table(table(datanomiss$citysize))*100, digits=2))
aus

############### descrittive su andrea

aus = as.data.frame(table(data$statideology_r3))

andrea_ideology = ggplot(aus, aes(x=Var1, y=Freq, fill = Var1))+
  geom_col()+
  xlab("Perceived Ideology of Andrea")+
  ylab("Frequency")+
  scale_y_continuous(breaks = seq(0,360,30))+
  scale_x_discrete(labels = rep("",8))+
  scale_fill_manual(name = "Perc. Ideology", values = c("Extreme right" = "#0008AD" ,
                                                  "Right"= "#4988F0" ,
                                                  "Center-right"= "#8FEAFA", 
                                                  "Center" = "grey",
                                                  "Center-left" = "#FF9179",
                                                  "Left" = "#F45533",
                                                  "Extreme left" = "#AD3920",
                                                  "Don't know" = "black"))+
  theme(axis.title.x = element_text(#face="bold", color="black",
  size=15),
  axis.title.y = element_text(#face="bold", color="black",
    size=15),
  axis.text.x = element_text(#face="bold", color="black",
    size=15),
  axis.text.y = element_text( #face="bold", color="black",
    size=15),
  legend.key.size = unit(1, 'cm'), #change legend key size
  legend.key.height = unit(1, 'cm'), #change legend key height
  #legend.key.width = unit(1, 'cm'), #change legend key width
  legend.title = element_text(size=15), #change legend title font size
  legend.text = element_text(size=15))

andrea_ideology

ggsave("Perceived ideology of Andrea.png", width = 12, height=8)

aus = as.data.frame(table(data$statparty_r3))

andrea_partisanship = ggplot(aus, aes(x=Var1, y=Freq, fill = Var1))+
  geom_col()+
  xlab("Perceived Partisanship of Andrea")+
  ylab("Frequency")+
  scale_y_continuous(breaks = seq(0,360,30))+
  scale_x_discrete(labels = rep("",8))+
  scale_fill_manual(name = "Perc. Partisanship", values = c("Brothers of Italy" = "#284BF7",
                                                            "League"="#31F728",
                                                            "Go Italy"="#28F1F7",
                                                            "Action-Italy Alive" = "#F360FF",
                                                            "Five Star Movement" ="#FAFA2E",
                                                            "Democratic Party" = "#FA882E",
                                                            "Green-Left Alliance" = "#FB3317",
                                                            "Don't know" = "black"))+
  theme(axis.title.x = element_text(#face="bold", color="black",
    size=15),
    axis.title.y = element_text(#face="bold", color="black",
      size=15),
    axis.text.x = element_text(#face="bold", color="black",
      size=12),
    axis.text.y = element_text( #face="bold", color="black",
      size=15),
    legend.key.size = unit(1, 'cm'), #change legend key size
    legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=15), #change legend title font size
    legend.text = element_text(size=15))


andrea_partisanship

ggsave("Perceived partisanship of Andrea.png", width = 12, height=8)

