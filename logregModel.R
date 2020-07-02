# LOGREG MODELS for machine learning approach to psychopathology. Email OSBORN@SHAMIRI.INSTITUTE FOR QUESTIONS

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

df$depressed <- "Not Depressed"
df$depressed[df$PHQ_Total>=10] = "Depressed"
df$depressed <- factor(df$depressed, labels = c('Not Depressed','Depressed'))
df$anxious <- "Not Anxious"
df$anxious[df$GAD_Total>=10] = "Anxious"
df$anxious <- factor(df$anxious, labels = c("Not Anxious","Anxious"))

#DEPRESSION MODEL
df%>%
  select(
    Shamiri_ID, depressed, School, School_type, Age, Form, Gender, Tribe, Tribal_Classification,
    Financial_Status, Home, Siblings, Religion, Parents_Living_With, 
    Num_parents_dead, Fathers_Education, Mothers_Education, Co_Curricular, Sports
  ) -> df.phq

set.seed(504)
split = sample.split(df.phq$depressed, SplitRatio = .8)
phq.TRAIN <- subset(df.phq, split==TRUE)
phq.TEST <- subset(df.phq, split==FALSE)

x_train.PHQ <-  phq.TRAIN %>%
  select(
    -c(depressed)
  )
y_train.PHQ <- phq.TRAIN %>%
  select(
    c(depressed)
  )

#GLM
depressionLog <- stats::glm(depressed ~ Gender+  Age + 
                              Form+ Tribal_Classification+ 
                              Financial_Status+ Home+ Siblings+ Parents_Living_With+ 
                              Num_parents_dead+ Fathers_Education+ Mothers_Education+ Co_Curricular+ 
                              Sports,
                            data = phq.TRAIN, family = 'binomial')

summary(depressionLog)
tab_model(depressionLog)

#predict train
predictTrain.PHQ = predict(depressionLog, type = "response")
summary(predictTrain.PHQ)
tapply(predictTrain.PHQ, phq.TRAIN$depressed, mean)
#confusion matrix
train.actual.PHQ <- phq.TRAIN$depressed
train.predicted.PHQ <- ifelse(predictTrain.PHQ >=.6, "Depressed","Not Depressed")
train.predicted.PHQ <- factor(train.predicted.PHQ, levels = c("Not Depressed", "Depressed"))

table(train.actual.PHQ, train.predicted.PHQ)

#choose threshhold using ROC curve
ROCRpred.PHQ <- prediction(predictTrain.PHQ, phq.TRAIN$depressed)
ROCRperf.PHQ <- performance(ROCRpred.PHQ, "tpr",'fpr')
plot(ROCRperf.PHQ, colorize = TRUE, print.cutoffs.at = seq(0,1, by = 0.1), text.adj=c(-0.2,1.7))

#train accuracy
mean(train.predicted.PHQ==train.actual.PHQ)

#predict test
predictTest.PHQ = predict(depressionLog, type = "response", newdata = phq.TEST)
table(phq.TEST$depressed, predictTest.PHQ >=.6)

test.actual.PHQ <- phq.TEST$depressed
test.predicted.PHQ <- ifelse(predictTest.PHQ >=.6, "Depressed","Not Depressed")
test.predicted.PHQ <- factor(test.predicted.PHQ, levels = c("Not Depressed", "Depressed"))

#test accuracy
mean(test.predicted.PHQ==test.actual.PHQ)
table(test.actual.PHQ, test.predicted.PHQ)


#ANXIETY MODEL
df%>%
  select(
    Shamiri_ID, anxious, School, School_type, Age, Form, Gender, Tribe, Tribal_Classification,
    Financial_Status, Home, Siblings, Religion, Parents_Living_With, Religion,
    Num_parents_dead, Fathers_Education, Mothers_Education, Co_Curricular, Sports, Percieved_Academic_Abilities
  ) -> df.gad

set.seed(504)
split = sample.split(df.gad$anxious, SplitRatio = .8)
gad.TRAIN <- subset(df.gad, split==TRUE)
gad.TEST <- subset(df.gad, split==FALSE)

anxietyLog <- stats::glm(anxious ~ Gender + Age +  Form + 
                           Tribal_Classification+ 
                           Financial_Status+ Home+ Siblings+ Parents_Living_With+
                           Num_parents_dead+ Fathers_Education+ Mothers_Education+ Co_Curricular+ 
                           Sports, 
                         data = gad.TRAIN, family = 'binomial')


summary(anxietyLog)
anova(anxietyLog, test = "Chisq")
tab_model(anxietyLog)
#predict train
predictTrain.GAD = predict(anxietyLog, type = "response")
summary(predictTrain.GAD)
tapply(predictTrain.GAD, gad.TRAIN$anxious, mean)

#confusion matrix
table(gad.TRAIN$anxious, predictTrain.GAD>.6)

#choose threshhold using ROC curve
ROCRpred.GAD <- prediction(predictTrain.GAD, gad.TRAIN$anxious)
ROCRperf.GAD <- performance(ROCRpred.GAD, "tpr",'fpr')
plot(ROCRperf.GAD, colorize = TRUE, print.cutoffs.at = seq(0,1, by = 0.1), text.adj=c(-0.2,1.7))

train.actual.GAD <- gad.TRAIN$anxious
train.predicted.GAD <- ifelse(predictTrain.GAD >=.6, "Anxious","Not Anxious")
train.predicted.GAD <- factor(train.predicted.GAD, levels = c("Not Anxious","Anxious"))

table(train.actual.GAD, train.predicted.GAD)

#train accuracy
mean(train.predicted.GAD==train.actual.GAD)

#predict test
predictTest.GAD = predict(anxietyLog, type = "response", newdata = gad.TEST)
table(gad.TEST$anxious, predictTest.GAD >=.6)

test.actual.GAD <- gad.TEST$anxious
test.predicted.GAD <- ifelse(predictTest.GAD >=.6, "Anxious","Not Anxious")
test.predicted.GAD <- factor(test.predicted.GAD, levels = c("Not Anxious","Anxious"))

#test accuracy
mean(test.predicted.GAD==test.actual.GAD)
table(test.actual.GAD, test.predicted.GAD)

#output

tab_model(depressionLog, anxietyLog, file = "output/depression_anxietyLog.doc")
