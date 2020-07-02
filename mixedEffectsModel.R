# CODE FOR DEPRESSION, ANXIETY SYMPTOMS IN KENYAN ADOLESCENTS ->  LOGISTIC REGRESSION MODEL
## JUNE 2020; FOR MORE INFORMATION SEE:https://doi.org/10.17605/OSF.IO/KSX6Y OR EMAIL: OSBORN@SHAMIRI.INSTITUTE

#LOAD PACKAGES
#install packages 
if (!require(devtools)) {install.packages("devtools"); require(devtools)} 
if (!require(psych)) {install.packages("psych"); require(psych)} 
if (!require(tidyverse)) {install.packages("tidyverse"); require(tidyverse)} 
if (!require(pastecs)) {install.packages("pastecs"); require(pastecs)} 
if (!require(bootstrap)) {install.packages("bootstrap"); require(bootstrap)}
if (!require(car)) {install.packages('car'); require(car)}
if (!require(ggstatsplot)) {install.packages("ggstatsplot"); require(ggstatsplot)} 
if (!require(groupedstats)) {install.packages("groupedstats"); require(groupedstats)} 
if (!require(ggpubr)) {install.packages("ggpubr"); require(ggpubr)} 
if (!require(apaTables)) {install.packages("apaTables"); require(apaTables)} 
if (!require(glm2)) {install.packages("glm2"); require(glm2)} 
if (!require(ROCR)) {install.packages("ROCR"); require(ROCR)} 
if (!require(caTools)) {install.packages("caTools"); require(caTools)} 
if (!require("mice")) {install.packages("mice"); require("mice")}
if (!require("VIM")) {install.packages("VIM"); require("VIM")}
if (!require("glmnet")) {install.packages("glmnet"); require("glmnet")}
if (!require("sjstats")) {install.packages("sjstats"); require("sjstats")}
if (!require("sjPlot")) {install.packages("sjPlot"); require("sjPlot")}
if (!require(lmerTest)) {install.packages("lmerTest"); require(lmerTest)}     ## uses lme4 package, gives p-values for regression parameters
if (!require(nlme)) {install.packages("nlme"); require(nlme)}
if (!require(lme4)) {install.packages("lme4"); require(lme4)}



base::rm(list = ls())

#Load data
groupedstats:::set_cwd()
df <- read_csv('data/shamiri_cleaned_baseline.csv')


#clean up 
#categorical variables

df$Tribal_Classification <- factor(df$Tribal_Classification, labels = c('Majority','Minority'))
df$Tribal_Classification <- relevel(df$Tribal_Classification, ref = 'Majority')

df$Gender <- factor(df$Gender,labels = c('Female','Male'))
df$Gender <- relevel(df$Gender, ref = "Male")

df$Form <- factor(df$Form, labels = c('Form 1','Form 2','Form 3','Form 4'))
df$Form <- relevel(df$Form, ref = 'Form 1')

df$Financial_Status <- factor(df$Financial_Status,
                              labels = c("Wealthy","Quite well-off","Not quite well-off","Poor"),
                              exclude = NA)

df$Financial_Status <- relevel(df$Financial_Status, ref = 'Wealthy')

df$Home <- factor(df$Home,labels = c("Rural area","Small town","Big town","City"),exclude = NA)

df$Siblings <- factor(df$Siblings, levels = c("1","2","3","4","> 4"), labels = c('1','2','3','4','> 4'),exclude = NA)

df$Religion <- factor(df$Religion,
                      labels = c('Christian protestant','Christian catholic',
                                 'Muslim','Hindu','Buddhist','Traditional African',
                                 'No religion','Other'), exclude = NA)
df$Parents_Dead <- factor(df$Parents_Dead, 
                          labels = c('Father','Mother','Both','None'))
#add df number of parents dead
df$Num_parents_dead <- 0
df$Num_parents_dead[df$Parents_Dead=='Father'] <- 1
df$Num_parents_dead[df$Parents_Dead=='Mother'] <- 1
df$Num_parents_dead[df$Parents_Dead=='Both']<-2

df$Fathers_Education <- factor(df$Fathers_Education,
                               labels = c("Not aware","Primary school","Secondary school","University"), 
                               exclude = NA)
df$Mothers_Education <- factor(df$Mothers_Education,  
                               labels = c("Not aware","Primary school","Secondary school","University"), 
                               exclude = NA)
df$Parents_Living_With <- factor(df$Parents,
                                 labels = c('No parent','Single parents','Both parents'),
                                 exclude = NA)

df$Co_Curricular <- factor(df$Co_Curricular, 
                             labels = c("Not involved at all","Quite involved","Extremely involved"))

df$Sports <- factor(df$Sports, labels = c('No','Yes'), exclude = NA)
df$School_type <- factor(df$School_type)

df$Percieved_Academic_Abilities <- factor(df$Percieved_Academic_Abilities, 
                                          labels = c('Not satisfactory','Satisfactory','Good','Very good','Excellent'),
                                          exclude = NA)


#IMPUTE DATA 
md.pattern(df)
aggr(df, col=c('navyblue','yellow'),
     numbers = TRUE, sortVars = TRUE,
     labels = names(df), cex.axis = .7,
     gap=3, ylab = c('Missing Data', 'Pattern'))

set.seed(507)
df.imputed <- mice(df, m=5, maxit = 5, method = 'pmm', 
                   seed = 507)

df <- complete(df.imputed)

df <- df %>%
  dplyr::mutate(
    PHQ_Total = PHQ_1 + PHQ_2 + PHQ_3 + PHQ_4 + PHQ_5 + PHQ_6 + PHQ_7 + PHQ_8,
    MSPSS_Total = MSSS_1+MSSS_2+MSSS_3+MSSS_4+MSSS_5+MSSS_6+MSSS_7+MSSS_8+MSSS_9+MSSS_10+MSSS_11+MSSS_12,
    MSPSS_Family = MSSS_3 + MSSS_4 + MSSS_8 + MSSS_11,
    MSPSS_Friends = MSSS_6 + MSSS_7 + MSSS_9 + MSSS_12,
    MSPSS_SO = MSSS_1+ MSSS_2+ MSSS_5+ MSSS_10,
    Gratitude =  Gratitude_1+Gratitude_2+Gratitude_3+Gratitude_4+Gratitude_5+Gratitude_6,
    Happiness = EPOCH_Happiness_1+EPOCH_Happiness_2+EPOCH_Happiness_3+EPOCH_Happiness_4,
    PCS_Academic = PCS_Academic_01+PCS_Academic_02+PCS_Academic_03+PCS_Academic_04+
      PCS_Academic_05+PCS_Academic_06+ PCS_Academic_07+PCS_Academic_08
  )

#depression model
df %>%
  dplyr::select(
    Shamiri_ID, School, PHQ_Total, MSPSS_Total,MSPSS_Family, MSPSS_Friends, MSPSS_SO, Gratitude, Happiness,
      PCS_Academic, School_type, Age, Form, Gender, Tribe, Tribal_Classification,
    Financial_Status, Home, Siblings, Religion, Parents_Living_With, 
    Num_parents_dead, Fathers_Education, Mothers_Education, Co_Curricular, Sports,Percieved_Academic_Abilities
  ) -> df.phq

#add depressed not depression 

depressionModel_1 <- lmer(
  PHQ_Total ~  MSPSS_Total +Gratitude +  Happiness +  
    PCS_Academic + (1|School), control = lmerControl(optimizer = "Nelder_Mead"),
  data = df.phq, 
)


depressionModel_2 <- lmer(
  PHQ_Total ~  Gender + Age + Tribal_Classification + Financial_Status + Home+ Siblings+ Parents_Living_With+ 
    Num_parents_dead+ Fathers_Education+ Mothers_Education+ Co_Curricular+  Sports
  + (1|School),control = lmerControl(optimizer = "Nelder_Mead"),
  data = df.phq
)                  

#anxiety model

df %>%
  dplyr::select(
    Shamiri_ID, School, GAD_Total, MSPSS_Total,MSPSS_Family, MSPSS_Friends, MSPSS_SO, Gratitude, Happiness,
    PCS_Academic, School_type, Age, Form, Gender, Tribe, Tribal_Classification,
    Financial_Status, Home, Siblings, Religion, Parents_Living_With, 
    Num_parents_dead, Fathers_Education, Mothers_Education, Co_Curricular, Sports,Percieved_Academic_Abilities
  ) -> df.gad

#add depressed not depression 

anxietyModel_1 <- lmer(
  GAD_Total ~  MSPSS_Total +Gratitude +  Happiness +  
    PCS_Academic + (1|School), control = lmerControl(optimizer = "Nelder_Mead"),
  data = df.gad, 
)

anxietyModel_2 <- lmer(
  GAD_Total ~  Gender + Age + Tribal_Classification + Financial_Status + Home+ Siblings+ Parents_Living_With+ 
    Num_parents_dead+ Fathers_Education+ Mothers_Education+ Co_Curricular+  Sports
  + (1|School),control = lmerControl(optimizer = "Nelder_Mead"),
  data = df.gad
)       


#OUTPUT
tab_model(depressionModel_1, anxietyModel_1, file = "output/psychosocialTable.doc",show.est = FALSE, show.std = TRUE)
tab_model(depressionModel_2, anxietyModel_2, file = "output/sociodemographicTable.doc",show.est = FALSE, show.std = TRUE, digits.p = 3)
