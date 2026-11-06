
##########################
# Authors: Valex Nacci & Sofia Gil-Clavel for the article
#
# Date: November 6th, 2026.
#
# Description:
# "Environmental and Climate Obstruction Narratives in Mexico: 
#   A Labeled Dataset of Meta Ads Circulating in Mexico between 2025 and 2026."
#
# Computer Environment:
# - Windows 
# - R - 4.5.0 (2025-04-11 ucrt)
# - RStudio (2025.05.0 )
# - Microsoft 10 Pro
#
# R Packages:
# - tidyverse (2.0.0)
# - irr (0.85)
##########################

rm(list=ls())
gc()

library(tidyverse)
library(irr)

### Second Gold Standard - CCO Narrative ####

### **** Loading the Data - First Round **** ####

# This is literally the values that come when you download the data from ASReview.

Coder1 <- read_csv("DATA/FirstRound/Intercoder1_Valex.csv")
Coder2 <- read_csv("DATA/FirstRound/Intercoder1_Sofia.csv")
Coder3 <-read_csv("DATA/FirstRound/Intercoder1_Ellen.csv")


# Renaming the columns to something easier to understand

# The column "label" has been changed to the names "Coder1", "Coder2" and "Coder3" 
#   to identify the relevant labels from each coders. 
# The columns of the tags have been shorten into "Type1", "Type2" and "Type3". 

Coder1 <- Coder1 %>%
  rename(coder1 = asreview_label, type1_1 = asreview_tag_related_to_environmental_discourse_phenomenon_obstruction_type_1_ ,
         type2_1 = asreview_tag_related_to_environmental_discourse_action_obstruction_type_2 , 
         type3_1 = asreview_tag_related_to_environmental_discourse_source_credibility_obstruction_type_3)

Coder2 <- Coder2 %>%
  rename(coder2 = asreview_label , type1_2 = asreview_tag_related_to_environmental_discourse_phenomenon_obstruction_type_1,
         type2_2 = asreview_tag_related_to_environmental_discourse_action_obstruction_type_2, type3_2 = asreview_tag_related_to_environmental_discourse_source_credibility_obstruction_type_3)

Coder3 <- Coder3 %>%
  rename(coder3 = asreview_label, type1_3 = asreview_tag_related_to_environmenal_discourse_phenomen_obstruction_type_1,
         type2_3 = asreview_tag_related_to_environmenal_discourse_action_obstruction_type_2,
         type3_3 = asreview_tag_related_to_environmenal_discourse_source_credibility_obstruction_type_3)

# Joining all the data 

Coder1_ = ifelse(Coder1$type1_1==1,"Type_1",
                 ifelse(Coder1$type2_1==1,"Type_2",
                        ifelse(Coder1$type3_1==1,"Type_3","Other")))
Coder2_ =  ifelse(Coder2$type1_2==1,"Type_1",
                  ifelse(Coder2$type2_2==1,"Type_2",
                         ifelse(Coder2$type3_2==1,"Type_3","Other")))
Coder3_ =  ifelse(Coder3$type1_3==1,"Type_1",
                  ifelse(Coder3$type2_3==1,"Type_2",
                         ifelse(Coder3$type3_3==1,"Type_3","Other")))

TYPE <- data.frame(Coder1=factor(Coder1_),
                   Coder2=factor(Coder2_),
                   Coder3=factor(Coder3_))

kappam.fleiss(TYPE)

### **** Loading the Data - Second Round **** ####

#### Opening the labels 

# This is literally the values that come when you download the data from ASReview.

Coder1 <- read_csv("DATA/SecondRound/Intercoder2_Coder1.csv")
Coder2 <- read_csv("DATA/SecondRound/Intercoder2_Coder2.csv")
Coder3 <-read_csv("DATA/SecondRound/Intercoder2_Coder3.csv")

#### Data Handling

# Renaming the columns to something easier to understand

# The column "label" has been changed to the names "Coder1", "Coder2" and "Coder3" 
#   to identify the relevant labels from each coders. 
# The columns of the tags have been shorten into "Type1", "Type2" and "Type3". 

Coder1 <- Coder1 %>%
  rename(coder1 = asreview_label, type1_1 = asreview_tag_related_to_environmental_discourse_phenomenon_obstruction_type_1_ ,
         type2_1 = asreview_tag_related_to_environmental_discourse_action_obstruction_type_2 , 
         type3_1 = asreview_tag_related_to_environmental_discourse_source_credibility_obstruction_type_3)

Coder2 <- Coder2 %>%
  rename(coder2 = asreview_label , type1_2 = asreview_tag_related_to_environmental_discourse_phenomenon_obstruction_type_1,
         type2_2 = asreview_tag_related_to_environmental_discourse_action_obstruction_type_2, type3_2 = asreview_tag_related_to_environmental_discourse_source_credibility_obstruction_type_3)


Coder3<- Coder3 %>%
  rename(coder3 = asreview_label, type1_3 = asreview_tag_related_to_environmental_discourse_phenomenon_obstruction_type_1,
         type2_3 = asreview_tag_related_to_environmental_discourse_action_obstruction_type_2,
         type3_3 = asreview_tag_related_to_environmental_discourse_source_credibility_obstruction_type_3)

# Creating one dataframe per each same column

# Columns with tags Relevant and No-relevant
Relevant <- data.frame(
  coder1 = Coder1$coder1,
  coder2 = Coder2$coder2,
  coder3 = Coder3$coder3
)

#### IRR 

kappam.fleiss(Relevant)

#### Checking Fleiss-K with all the tags and labelers

Coder1_ = ifelse(Coder1$type1_1==1,"Type_1",
                ifelse(Coder1$type2_1==1,"Type_2",
                       ifelse(Coder1$type3_1==1,"Type_3","Other")))
Coder2_ =  ifelse(Coder2$type1_2==1,"Type_1",
                 ifelse(Coder2$type2_2==1,"Type_2",
                        ifelse(Coder2$type3_2==1,"Type_3","Other")))
Coder3_ =  ifelse(Coder3$type1_3==1,"Type_1",
                 ifelse(Coder3$type2_3==1,"Type_2",
                        ifelse(Coder3$type3_3==1,"Type_3","Other")))

TYPE <- data.frame(Coder1=factor(Coder1_),
                   Coder2=factor(Coder2_),
                   Coder3=factor(Coder3_))

KAPPA<-kappam.fleiss(TYPE)
