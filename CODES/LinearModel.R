
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
#
##########################

rm(list=ls())
gc()

library(tidyverse)

#### Opening the functions ####
DIR="C:\\Users\\oae694\\OneDrive - Vrije Universiteit Amsterdam\\PersonalProjects\\NorthVsSouth_CCmissinfo\\CODES\\"
source(paste0(DIR,"Functions.R"))

#### Second Gold Standard - CCO Narrative ####

Coder1 <- read_csv("DATA/FinalData/800_Valex_completed.csv",)
colnames(Coder1)[2]<-"id"

# Coder2 <- read_csv("DATA/FinalData/800_Sofia_completed.csv")

Coder3 <-read_csv("DATA/FinalData/800_Valex_completed.csv")
colnames(Coder3)[2]<-"id"


#### Data Handling

# Renaming the columns to something easier to understand

# The column "label" has been changed to the names "Coder1", "Coder2" and "Coder3" 
#   to identify the relevant labels from each coders. 
# The columns of the tags have been shorten into "Type1", "Type2" and "Type3". 

Coder1_2 <- Coder1 %>%
  rename(environment = asreview_label, 
         type1 = asreview_tag_related_to_environmental_discourse_phenomenon_obstruction_type_1_ ,
         type2 = asreview_tag_related_to_environmental_discourse_action_obstruction_type_2 , 
         type3 = asreview_tag_related_to_environmental_discourse_source_credibility_obstruction_type_3)%>%
  mutate(id=as.character(id))%>%
  select(id,abstract,environment,type1,type2,type3)%>%
  mutate(coder="Coder1")

# Coder2 <- Coder2 %>%
#   rename(environment = asreview_label , 
#         type1_2 = asreview_tag_related_to_environmental_discourse_phenomenon_obstruction_type_1,
#         type2_2 = asreview_tag_related_to_environmental_discourse_action_obstruction_type_2, 
#         type3_2 = asreview_tag_related_to_environmental_discourse_source_credibility_obstruction_type_3)


Coder3_2 <- Coder3 %>%
  rename(environment = asreview_label, 
         type1 = asreview_tag_related_to_environmental_discourse_phenomenon_obstruction_type_1_,
         type2 = asreview_tag_related_to_environmental_discourse_action_obstruction_type_2,
         type3 = asreview_tag_related_to_environmental_discourse_source_credibility_obstruction_type_3)%>%
  mutate(id=as.character(id))%>%
  select(id,abstract,environment,type1,type2,type3)%>%
  mutate(coder="Coder1")


ALL_CCO<-rbind(Coder1_2,Coder3_2)

#### Joining with the original data to get the regions and target audience ####

FOR_ANNOTATION<-read_csv("DATA/FinalData/ForAnnotation_ids.csv")

FOR_ANNOTATION2<-FOR_ANNOTATION%>%
  mutate(id2=str_remove_all(id,'\"'))%>%
  mutate(id=str_remove_all(id,'\"'),
         id2=substr(id2, 1, 5))

ALL_CCO2<-ALL_CCO%>%
  mutate(id=as.character(id))%>%
  mutate(id=substr(id, 1, 6))%>%
  mutate(id=str_remove(id,"\\."))

CHECKED=FOR_ANNOTATION2%>%
  left_join(ALL_CCO2,by=join_by(ad_creative_bodies==abstract,id2==id))%>%
  drop_na()%>%
  distinct()

# Keeping only the identifiers from the coding sessions

load(file = "DATA/FinalData/all_ads.RData")

IDS<-unique(CHECKED$id)

# Checking the number of ads per region
ALL<-all%>%
  right_join(CHECKED,by=join_by(id==id))%>%
  unnest(delivery_by_region)%>%
  filter(region%in%MEX_ENTS)%>%
  filter(!TERMS%in%c("clima","climatico","tiempo"))%>%
  distinct()

ALL<-ALL%>%
  filter(region%in%c("Quintana Roo","Oaxaca"))


#### linear model ####

ALL$region=factor(ALL$region)
ALL$type2=factor(ALL$type2)

model<-glm(type2~region,
           data = ALL,
           family = "binomial")

summary(model)

exp(coef(model)[2])

TO_SAVE=ALL%>%
  select(id,region,ad_creative_bodies.x,environment,type1,type2,type3,coder)

# save(TO_SAVE,file = "DATA/FinalData/FOR_ANALYSIS.RData")

