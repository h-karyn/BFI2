library(psych)
library(lavaan)
library(semTools)
#Column “self_BFI_1” to “self_BFI_60” refers the participants’ response in the BFI scale

#Column “self_D_1” to “self_D_7_1” refers to participants’ response in some demographic questions

# Extroversion: items 1, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56
# Agreeableness: items 2, 7, 12, 17, 22, 27, 32, 37, 42, 47, 52, 57
# Consciousness: items 3, 8, 13, 18, 23, 28, 33, 38, 43, 48, 53, 58
# Negative Emotionality: items 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59
# Open-Mindedness: items 5, 10, 15, 20, 25, 30, 35,40, 45, 50, 55

# Sociability: 1, 16R, 31R, 46
# Assertiveness: 6, 21, 36R, 51R
# Energy Level: 11R, 26R, 41, 56

# Compassion: 2, 17R, 32, 47R
# Respectfulness: 7, 22R, 37R, 52
# Trust: 12R, 27, 42R, 57

# Organization: 3R, 18, 33, 48R
# Productiveness: 8R, 23R, 38, 53
# Responsibility: 13, 28R, 43, 58R


# Anxiety: 4R, 19, 34, 49R
# Depression: 9R, 24R, 39, 54
# Emotional Volatility: 14, 29R, 44R, 59

# Intellectual Curiosity: 10, 25R, 40, 55R
# Aesthetic Sensitivity: 5R, 20, 35, 50R
# Creative Imagination: 15, 30R, 45R, 60 

# D_1 Sex: Male  (1), Female  (2), Gender Variant  (3) 
# D_2 Age
# D_3 Ethnicity: European  (1), East Asian  (2), African  (3), Hispanic  (4), 
#                Latino  (5),	Aboriginal  (6), Other  (7) _
# D_4 In what country were you born? 
# D_5 What is your primary language?
# D_6 Number of years of formal education (counting from grade 1):
# D_7 English language reading/comprehension ability: Poor (Difficulty Reading/ Comprehending Written English) (1)	Click to write Scale Point 2 (2)	
#                                                     Fair (Some Difficulties Reading/ Comprehending Written English) (3)	Click to write Scale Point 4 (4)	
#                                                     Excellent (No Difficulties Reading/ Comprehending Written English) (5)
  
# select variables relevant to this study
load("/Users/mhhuang/OneDrive - UBC/BFI Study/Analysis/bfi.cleaned.data.participant.v1.RData")
data <- bfi.cleaned.data.participant.v1[,c(1:61,122:129)]

# filter data basing on condition 
likert_data <- subset(data, data$self_condition == "O") #“O” refers to the Original Likert version
expanded_data <- subset(data, data$self_condition == "E") #“E” refers to the Expanded version 


#### Section 1: Demographic report ####
full_data <-rbind(likert_data,expanded_data)

# D_1 Sex: Male  (1), Female  (2), Gender Variant  (3) 
table(full_data$self_D_1)

# D_2 Age
hist(full_data$self_D_2, breaks = 20)
table(full_data$self_D_2)

# D_3 Ethnicity: European  (1), East Asian  (2), African  (3), Hispanic  (4), 
#                Latino  (5),	Aboriginal  (6), Other  (7) _
table(full_data$self_D_3)

#### Section 2: item analysis ####
###### Likert format ######

# Univariate Descriptive Statistics 
describe(likert_data) 
# Potentially problematic items
# # item 2 (mean = 4.36, sd = 0.82), item 7 (mean = 4.57, sd = 0.65), item 10 (mean = 4.21, sd = 0.87)
# # item 35 (mean = 4.10 sd = 0.97), item 43 (mean = 4.05, sd = 0.90), item 47 (mean = 4.33, sd = 0.95)
# # item 52 (mean = 4.40, sd = 0.75)

# Examine the correlations 
vec<- c() # to reorder items by their facets and domains 
for (j in 0:4){
  for (i in 0:2){
    vec <- c(vec, 1+i*5+j)
    vec <- c(vec, 1+15+i*5+j)
    vec <- c(vec, 1+30+i*5+j)
    vec <- c(vec, 1+45+i*5+j)
  }
}
likert_data_reordered <- likert_data[,vec]
options(max.print= 5000)
round(cor(likert_data_reordered[,1:60], use = "complete.obs"),2)

# # item 1 (Sociability in Extraversion) and item 9,24,39,54 (Depression in Negative Emotionality): cor ~= -.3 
# # item 6 and 21 (Assertiveness in Extraversion) and item 15 and 60 (Creative Imagination in Openmindedness): cor ~=.3
# # item 41, 56 (Energy Level in Extraversion) and item 9,24,39,54 (Depression in Negative Emotionality): cor ~= -.4
# # item 17 (Compassion in Agreeableness) and item 37 (Respectfulness in Agreeableness) and 12 (Trust in Agreeableness): cor ~= -0.04 

###### Expanded format ######

# Univariate Descriptive Statistics 
describe(expanded_data) 
# Potentially problematic items
# item 2 (mean = 4.02, sd = 0.77), item 7 (mean = 4.37, sd = 0.72), item 17 (mean = 4.09, sd = 0.80), item 37 (mean = 4.15, sd = 0.81)
# item 43 (mean = 4.10, sd = 0.74), item 45 (mean = 4.07. sd = 0.99), item 47 (mean = 4.03, sd = 0.84), item 52 (mean = 4.35, sd = 0.72)

# Examine the correlations 
expand_data_reordered <- expanded_data[,vec]
round(cor(expand_data_reordered[,1:60], use = "complete.obs"),2)

# item 1 and 46 (Sociability in Extroversion) and item 47 (Compassion in Agreeableness): cor ~= .32
# item 11 and 56 (Energy Level in Extraversion) and item 10 (Intellectual Curiosity in Openmindedness): cor ~= .32
# item 32 and 47 (Compassion in Agreeableness) and item 43 (Responsibility in Consciousness): cor = .3~.4
# item 8 (Productiveness in Consciousness) and item 26 (Energy Level in Extraversion): cor = .42
# item 15 and item 45 (both from Creative Imagination in Openmindedness): cor = .21



#### Section 3: Damian Level Analysis  ####

##### Likert  #####
Likert_E <- likert_data[,c(1, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56)]
Likert_A <- likert_data[,c(2, 7, 12, 17, 22, 27, 32, 37, 42, 47, 52, 57)]
Likert_C <- likert_data[,c(3, 8, 13, 18, 23, 28, 33, 38, 43, 48, 53, 58)]
Likert_N <- likert_data[,c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59)]
Likert_O <- likert_data[,c(5, 10, 15, 20, 25, 30, 35,40, 45, 50, 55, 60)]

domain_size = 12 

###### Extroversion  ######

# Sociability: 1, 16R, 31R, 46
# Assertiveness: 6, 21, 36R, 51R
# Energy_Level: 11R, 26R, 41, 56

Likert_E_Sociability <- Likert_E[,seq(1,domain_size,3)]
Likert_E_Assertiveness <- Likert_E[,seq(2,domain_size,3)]
Likert_E_Energy_Level <- Likert_E[,seq(3,domain_size,3)]

########## Model 0 ##########
Model0_Likert_E <- '
Sociability  =~ self_BFI_1 + self_BFI_16 + self_BFI_31 + self_BFI_46
Assertiveness  =~ self_BFI_6 + self_BFI_21 + self_BFI_36 + self_BFI_51
Energy_Level  =~ self_BFI_11 + self_BFI_26 + self_BFI_41 + self_BFI_56

#Extroversion =~ Sociability + Assertiveness + Energy_Level
'
Run_Model0_Likert_E <- sem(Model0_Likert_E,dat = Likert_E, missing = "ML")
summary(Run_Model0_Likert_E, standardized = TRUE,fit.measures = TRUE)


########## Model 1 ##########
#likert_data[,c(1, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56)]
# key <- c(1,1,-1,-1,1,-1,-1,-1,1,1,-1,1)
# Likert_E_reverse_coded <- as.data.frame(reverse.code(key,Likert_E))
# names(Likert_E_reverse_coded)<-names(Likert_E)

Model1_Likert_E <- '
Sociability  =~ self_BFI_1 + self_BFI_16 + self_BFI_31 + self_BFI_46
Assertiveness  =~ self_BFI_6 + self_BFI_21 + self_BFI_36 + self_BFI_51
Energy_Level  =~ self_BFI_11 + self_BFI_26 + self_BFI_41 + self_BFI_56

#Extroversion =~ Sociability + Assertiveness + Energy_Level

Sociability ~~ 0*Acquiescence
Energy_Level ~~ 0*Acquiescence
Assertiveness ~~ 0*Acquiescence

Acquiescence =~ 1*self_BFI_1 + (-1)*self_BFI_16 + (-1)*self_BFI_31 + 1*self_BFI_46 + 
1*self_BFI_6 + 1*self_BFI_21 + (-1)*self_BFI_36 + (-1)*self_BFI_51 + 
(-1)*self_BFI_11 + (-1)*self_BFI_26 + 1*self_BFI_41 + 1*self_BFI_56
'
Run_Model1_Likert_E <- sem(Model1_Likert_E,dat = Likert_E, missing = "ML")
summary(Run_Model1_Likert_E,standardized = TRUE,fit.measures = TRUE)

########## Model 2 ##########
Model2_Likert_E <- '
Sociability  =~ self_BFI_1 + self_BFI_16 + self_BFI_31 + self_BFI_46
Assertiveness  =~ self_BFI_6 + self_BFI_21 + self_BFI_36 + self_BFI_51
Energy_Level  =~ self_BFI_11 + self_BFI_26 + self_BFI_41 + self_BFI_56

#Extroversion =~ Sociability + Assertiveness + Energy_Level

Sociability ~~ 0*Reverse
Energy_Level ~~ 0*Reverse
Assertiveness ~~ 0*Reverse

Reverse =~ self_BFI_16 + self_BFI_31 + self_BFI_36 + self_BFI_51 + self_BFI_11 + self_BFI_26 
'
Run_Model2_Likert_E <- sem(Model2_Likert_E,dat = Likert_E,missing = "ML")
summary(Run_Model2_Likert_E,standardized = TRUE,fit.measures = TRUE)

######### Model 3 #########
Model3_Likert_E <- '
Sociability  =~ self_BFI_1 + self_BFI_16 + self_BFI_31 + self_BFI_46
Assertiveness  =~ self_BFI_6 + self_BFI_21 + self_BFI_36 + self_BFI_51
Energy_Level  =~ self_BFI_11 + self_BFI_26 + self_BFI_41 + self_BFI_56

#Extroversion =~ Sociability + Assertiveness + Energy_Level

Sociability ~~ 0*Positive
Energy_Level ~~ 0*Positive
Assertiveness ~~ 0*Positive

Positive =~ self_BFI_1 + self_BFI_46 + self_BFI_6 + self_BFI_21 + self_BFI_41 + self_BFI_56 
'
Run_Model3_Likert_E <- sem(Model3_Likert_E,dat = Likert_E,missing = "ML")
summary(Run_Model3_Likert_E,standardized = TRUE,fit.measures = TRUE)

# Model comparision
summary(compareFit(Run_Model3_Likert_E,Run_Model2_Likert_E,Run_Model1_Likert_E,Run_Model0_Likert_E,nested = FALSE))

##### Expanded  #####
Expanded_E <- expanded_data[,c(1, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56)]
Expanded_A <- expanded_data[,c(2, 7, 12, 17, 22, 27, 32, 37, 42, 47, 52, 57)]
Expanded_C <- expanded_data[,c(3, 8, 13, 18, 23, 28, 33, 38, 43, 48, 53, 58)]
Expanded_N <- expanded_data[,c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59)]
Expanded_O <- expanded_data[,c(5, 10, 15, 20, 25, 30, 35,40, 45, 50, 55, 60)]

###### Extroversion  ######

# Sociability: 1, 16R, 31R, 46
# Assertiveness: 6, 21, 36R, 51R
# Energy_Level: 11R, 26R, 41, 56

Expanded_E_Sociability <- Expanded_E[,seq(1,domain_size,3)]
Expanded_E_Assertiveness <- Expanded_E[,seq(2,domain_size,3)]
Expanded_E_Energy_Level <- Expanded_E[,seq(3,domain_size,3)]
########## Model 0 ##########
Model0_Expanded_E <- '
Sociability  =~ self_BFI_1 + self_BFI_16 + self_BFI_31 + self_BFI_46
Assertiveness  =~ self_BFI_6 + self_BFI_21 + self_BFI_36 + self_BFI_51
Energy_Level  =~ self_BFI_11 + self_BFI_26 + self_BFI_41 + self_BFI_56

#Extroversion =~ Sociability + Assertiveness + Energy_Level

Sociability ~~ 0*Assertiveness
Energy_Level ~~ 0*Assertiveness
Sociability ~~ 0*Energy_Level
'
Run_Model0_Expanded_E <- sem(Model0_Expanded_E,dat = Expanded_E, missing = "ML")
summary(Run_Model0_Expanded_E, standardized = TRUE,fit.measures = TRUE)


########## Model 1 ##########
#likert_data[,c(1, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56)]
# key <- c(1,1,-1,-1,1,-1,-1,-1,1,1,-1,1)
# Expanded_E_reverse_coded <- as.data.frame(reverse.code(key,Expanded_E))
# names(Expanded_E_reverse_coded)<-names(Expanded_E)

Model1_Expanded_E <- '
Sociability  =~ self_BFI_1 + self_BFI_16 + self_BFI_31 + self_BFI_46
Assertiveness  =~ self_BFI_6 + self_BFI_21 + self_BFI_36 + self_BFI_51
Energy_Level  =~ self_BFI_11 + self_BFI_26 + self_BFI_41 + self_BFI_56

#Extroversion =~ Sociability + Assertiveness + Energy_Level

Sociability ~~ 0*Acquiescence
Energy_Level ~~ 0*Acquiescence
Assertiveness ~~ 0*Acquiescence

Acquiescence =~ 1*self_BFI_1 + (-1)*self_BFI_16 + (-1)*self_BFI_31 + 1*self_BFI_46 + 
1*self_BFI_6 + 1*self_BFI_21 + (-1)*self_BFI_36 + (-1)*self_BFI_51 + 
(-1)*self_BFI_11 + (-1)*self_BFI_26 + 1*self_BFI_41 + 1*self_BFI_56
'
Run_Model1_Expanded_E <- sem(Model1_Expanded_E,dat = Expanded_E, missing = "ML")
summary(Run_Model1_Expanded_E,standardized = TRUE,fit.measures = TRUE)

########## Model 2 ##########
Model2_Expanded_E <- '
Sociability  =~ self_BFI_1 + self_BFI_16 + self_BFI_31 + self_BFI_46
Assertiveness  =~ self_BFI_6 + self_BFI_21 + self_BFI_36 + self_BFI_51
Energy_Level  =~ self_BFI_11 + self_BFI_26 + self_BFI_41 + self_BFI_56

#Extroversion =~ Sociability + Assertiveness + Energy_Level

Sociability ~~ 0*Reverse
Energy_Level ~~ 0*Reverse
Assertiveness ~~ 0*Reverse

Reverse =~ self_BFI_16 + self_BFI_31 + self_BFI_36 + self_BFI_51 + self_BFI_11 + self_BFI_26 
'
Run_Model2_Expanded_E <- sem(Model2_Expanded_E,dat = Expanded_E,missing = "ML")
summary(Run_Model2_Expanded_E,standardized = TRUE,fit.measures = TRUE)

######### Model 3 #########
Model3_Expanded_E <- '
Sociability  =~ self_BFI_1 + self_BFI_16 + self_BFI_31 + self_BFI_46
Assertiveness  =~ self_BFI_6 + self_BFI_21 + self_BFI_36 + self_BFI_51
Energy_Level  =~ self_BFI_11 + self_BFI_26 + self_BFI_41 + self_BFI_56

#Extroversion =~ Sociability + Assertiveness + Energy_Level

Sociability ~~ 0*Positive
Energy_Level ~~ 0*Positive
Assertiveness ~~ 0*Positive

self_BFI_56~~ a* self_BFI_56
a>0

Positive =~ self_BFI_1 + self_BFI_46 + self_BFI_6 + self_BFI_21 + self_BFI_41 + self_BFI_56 
'
Run_Model3_Expanded_E <- sem(Model3_Expanded_E,dat = Expanded_E,missing = "ML")
summary(Run_Model3_Expanded_E,standardized = TRUE,fit.measures = TRUE)

# Model comparision
summary(compareFit(Run_Model3_Expanded_E,Run_Model2_Expanded_E,Run_Model1_Expanded_E,Run_Model0_Expanded_E,nested = FALSE))



# ###### Agreeableness  ######
# 
# # Compassion: 2, 17R, 32, 47R
# # Respectfulness: 7, 22R, 37R, 52
# # Trust: 12R, 27, 42R, 57
# 
# Likert_A_Compassion <- Likert_A[,seq(1,domain_size,3)]
# Likert_A_Respectfulness <- Likert_A[,seq(2,domain_size,3)]
# Likert_A_Trust <- Likert_A[,seq(3,domain_size,3)]
# 
# ###### Consciousness  ######
# 
# # Organization: 3R, 18, 33, 48R
# # Productiveness: 8R, 23R, 38, 53
# # Responsibility: 13, 28R, 43, 58R
# 
# Likert_C_Compassion <- Likert_C[,seq(1,domain_size,3)]
# Likert_C_Respectfulness <- Likert_C[,seq(2,domain_size,3)]
# Likert_C_Trust <- Likert_C[,seq(3,domain_size,3)]
# 
# ###### Negative Emotionality  ######
# 
# # Anxiety: 4R, 19, 34, 49R
# # Depression: 9R, 24R, 39, 54
# # Emotional Volatility: 14, 29R, 44R, 59
# 
# Likert_N_Compassion <- Likert_N[,seq(1,domain_size,3)]
# Likert_N_Respectfulness <- Likert_N[,seq(2,domain_size,3)]
# Likert_N_Trust <- Likert_N[,seq(3,domain_size,3)]
# 
# ###### Open-mindedness ######
# 
# # Intellectual Curiosity: 10, 25R, 40, 55R
# # Aesthetic Sensitivity: 5R, 20, 35, 50R
# # Creative Imagination: 15, 30R, 45R, 60 
# 
# Likert_O_Compassion <- Likert_O[,seq(1,domain_size,3)]
# Likert_O_Respectfulness <- Likert_O[,seq(2,domain_size,3)]
# Likert_O_Trust <- Likert_O[,seq(3,domain_size,3)]
# 
# 
