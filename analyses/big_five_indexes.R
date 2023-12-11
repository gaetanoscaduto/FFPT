# SOME OF THESE ITEMS ARE TO BE REVERSED (SEE CHIORRI ET AL., 2015)
data$agr_r = 4 -data$revagr
data$ext_r = 4 -data$revext
data$con_r = 4 -data$revcon
data$ope_r = 4 -data$revope
data$neu_r = 4 -data$revneu

############ COMPUTE BIG FIVE INDEXES
data$extraversion = (data$ext +data$ext_r)/2
data$agreeableness = (data$agr + data$agr_r)/2
data$neuroticism = (data$neu + data$neu_r)/2
data$openness = (data$ope + data$ope_r)/2
data$conscientiousness = (data$con + data$con_r)/2