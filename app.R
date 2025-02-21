# install.packages("bslib")
# install.packages("bsicons")
# install.packages("fontawesome")
# install.packages("paletteer")
# install.packages("gt")
# install.packages("rvest")
# install.packages("spsComps")
# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("rsconnect")
library(rsconnect)
# rsconnect::setAccountInfo(name='siteammm', token='3CE75D54A2BA060CF8FF848005D54B1A', secret='Iao974jyXGy43bkslGwz8cMdBs0LKtvSuyBci8jS')
# rsconnect::deployApp('path/to/your/app-directory')

#library(conflicted)
#conflicted::conflict_prefer("filter", "dplyr")
#conflicted::conflict_prefer("lag", "dplyr")

library(gt)
library(rvest)

library(bslib)
library(shiny)
library(fontawesome)
library(paletteer)

# install.packages("shinycssloaders")
library(bsicons)
library(shiny)
library(ggrepel)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)
library(shinyjs)        # improve user experience with JavaScript
library(shinythemes)    # themes for shiny
library(tidyverse)      # data manipulation
library(openxlsx)
library(httr)
library(do)
library(shinydashboard)
#install.packages("reshape2")                                  # Install reshape2
library("reshape2") 
library("stringr")
library("shinycssloaders")
library("htmltools")
library(gridExtra)
library(gridtext)
library(grid)

#install.packages("fmsb")

library(readxl)

library(plotly)
library(fmsb)
#install.packages('rjson')
library(rjson)
library(htmlTable)
library(stringr)
library(htmltools)
library(r2symbols)
library(kableExtra)
library(fontawesome)
library(forcats)
library(spsComps)

#------ Read main submission Data from ODK Central-------

# jscode <- "shinyjs.closeWindow = function() { window.close(); }"
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

url <- "https://nhlmmr-icap-vl.org/v1/projects/11/forms/VL_SCORECARD_1/submissions.csv"

GET(url, authenticate("nhlmmr.qa@gmail.com", "Nhl@#!1005x1005"), content_type("text/csv"), write_disk("temp.csv", overwrite = TRUE))
data <- read.csv('temp.csv')


#hide the error/practice records

data <- subset(data,

               KEY!='uuid:dc452d94-2017-44cd-9291-1d756fb6c751' &
                 KEY!='uuid:1de936b4-0e0b-4d40-9d8b-0198c365ba5f' &
                 KEY!='uuid:01f8bea6-085f-4c56-bbaf-9821c6316618' &
                 KEY!='uuid:03fd3e70-6931-4383-ad4e-5df784619a66' &
                 KEY!='uuid:fffeb18e-8cb6-4861-91c9-44b86546a1d0' &
                 KEY!='uuid:25767ad7-4e08-4f45-947b-2e7851fc5e2e' &
                 KEY!='uuid:5c7dead4-5dbe-405d-89f1-7676c2b02072' &
                 KEY!='uuid:42103195-b139-4056-b759-6cd25b9961b6' &
                 KEY!='uuid:8bae0f66-e3d9-4655-9e12-5cc6eb02a1e4' &
                 KEY!='uuid:c7179726-152c-4c3e-b431-f9815e793b58' &
                 KEY!='uuid:cf314646-26c8-4819-a90e-ca4a9b06b762' &
                 KEY!='uuid:2ea02d35-47a1-4178-bf5e-a5ff49eb5c1a' &
                 KEY!='uuid:69e20d0e-63f8-4066-a60a-a0bfa7a62125' &
                 KEY!='uuid:b748e90a-fd58-4a86-b0a0-f0fb753fdf42' &
                 KEY!='uuid:9464a06b-b29d-42b8-bde8-e9e54c8a014a' &
                 KEY!='uuid:04e05880-b3b9-44bf-b87e-59de1b76dd1a' &
                 KEY!='uuid:0de85849-459b-445d-8202-ea944ad4017b' &
                 KEY!='uuid:d8ddd5a6-b30d-4ced-aa7e-395d34e8f47f' &
                 KEY!='uuid:7dc1b5a2-641b-418f-892c-cb988179291c' &
                 KEY!='uuid:2392db30-9e35-4a7c-86af-f8f34815bb2e' &
                 KEY!='uuid:859468f8-6fe4-45ca-bfe6-98f719b46f4b' &
                 KEY!='uuid:93d538be-3822-4393-b553-13f3b385556f' &
                 KEY!='uuid:9b387184-7dd0-4cca-8370-0d4c09d0b2b3' &
                 KEY!='uuid:9f600794-9e4c-4a7a-aac7-55a290ca3922' &
                 KEY!='uuid:cf2d069c-653e-465b-bf8c-68483c2e0948' &
                 KEY!='uuid:e672a940-983b-4887-91d9-804f9cb1f46f' &
                 KEY!='uuid:e50c3b6f-93a2-449b-b632-0a401815d210' &
                 KEY!='uuid:6df5036e-6f60-4bf1-9a2e-0ed0c5ea9509'

)



#------rename variables for easy analysis----
data <-  data %>%
  rename("country" = group_part1.Country ) %>%
  rename("state_region" = group_part1.State_Region ) %>%
  rename("township" = group_part1.City_Town ) %>%
  rename("platform" = group_part1.Type_of_VL_Platform ) %>%
  # rename("sub_platform" = group_part1.Type_of_VL_Platform_1) %>%
  rename("lab_name" = group_part1.Laboratory_Name) %>%
  rename("assessment_date" = group_part1.Date) %>%
  rename("first_assessment" = group_part1.First_assessment) %>%
  rename("accessor_name_1" = group_part1.Assessor_Name_1) %>%
  rename("accessor_name_2" = group_part1.Assessor_Name_2) %>%
  rename("previous_assessment_date" = group_part1.Date_Last_Assessment) %>%
  rename("Q1_1" = group_1_0_PERSONNAL.Q_1_1) %>%
  rename("C1_1" = group_1_0_PERSONNAL.Comments_001) %>%
  rename("Q1_2" = group_1_0_PERSONNAL.Q_1_2) %>%
  rename("C1_2" = group_1_0_PERSONNAL.Comments_002) %>%
  rename("Q1_3" = group_1_0_PERSONNAL.Q_1_3) %>%
  rename("C1_3" = group_1_0_PERSONNAL.Comments_003) %>%
  rename("Q1_4" = group_1_0_PERSONNAL.Q_1_4) %>%
  rename("C1_4" = group_1_0_PERSONNAL.Comments_004) %>%
  rename("Q1_5" = group_1_0_PERSONNAL.Q_1_5) %>%
  rename("C1_5" = group_1_0_PERSONNAL.Comments_005) %>%
  rename("Q1_6" = group_1_0_PERSONNAL.Q_1_6) %>%
  rename("C1_6" = group_1_0_PERSONNAL.Comments_006) %>%
  rename("Q1_7" = group_1_0_PERSONNAL.Q_1_7) %>%
  rename("C1_7" = group_1_0_PERSONNAL.Comments_007) %>%
  rename("Q1_8" = group_1_0_PERSONNAL.Q_1_8) %>%
  rename("C1_8" = group_1_0_PERSONNAL.Comments_008) %>%
  rename("Q1_9" = group_1_0_PERSONNAL.Q_1_9) %>%
  rename("C1_9" = group_1_0_PERSONNAL.Comments_009) %>%
  rename("Q1_10" = group_1_0_PERSONNAL.Q_1_10) %>%
  rename("C1_10" = group_1_0_PERSONNAL.Comments_010) %>%
  rename("Q1_11" = group_1_0_PERSONNAL.Q_1_11_Highthrough) %>%
  rename("C1_11" = group_1_0_PERSONNAL.Comments_011) %>%
  rename("Q2_1" = group_2_0_PHYSICAL.Q_2_1) %>%
  rename("Q2_2" = group_2_0_PHYSICAL.Q_2_2) %>%
  rename("Q2_3" = group_2_0_PHYSICAL.Q_2_3) %>%
  rename("Q2_4" = group_2_0_PHYSICAL.Q_2_4) %>%
  rename("Q2_5" = group_2_0_PHYSICAL.Q_2_5) %>%
  rename("Q2_6" = group_2_0_PHYSICAL.Q_2_6) %>%
  rename("Q2_7" = group_2_0_PHYSICAL.Q_2_7) %>%
  rename("Q2_8" = group_2_0_PHYSICAL.Q_2_8) %>%
  rename("Q2_9" = group_2_0_PHYSICAL.Q_2_9) %>%
  rename("Q2_10" = group_2_0_PHYSICAL.Q_2_10) %>%
  rename("Q2_11" = group_2_0_PHYSICAL.Q_2_11) %>%
  rename("Q2_12_G" = group_2_0_PHYSICAL.Q_2_12_GeneXpert) %>%
  rename("Q2_12_H" = group_2_0_PHYSICAL.Q_2_12_Highthrough) %>%
  rename("Q2_13" = group_2_0_PHYSICAL.Q_2_13_Highthrough) %>%
  rename("Q2_14" = group_2_0_PHYSICAL.Q_2_14_Highthrough) %>%
  rename("C2_1" = group_2_0_PHYSICAL.Comments_012) %>%
  rename("C2_2" = group_2_0_PHYSICAL.Comments_013) %>%
  rename("C2_3" = group_2_0_PHYSICAL.Comments_014) %>%
  rename("C2_4" = group_2_0_PHYSICAL.Comments_015) %>%
  rename("C2_5" = group_2_0_PHYSICAL.Comments_016) %>%
  rename("C2_6" = group_2_0_PHYSICAL.Comments_017) %>%
  rename("C2_7" = group_2_0_PHYSICAL.Comments_018) %>%
  rename("C2_8" = group_2_0_PHYSICAL.Comments_019) %>%
  rename("C2_9" = group_2_0_PHYSICAL.Comments_020) %>%
  rename("C2_10" = group_2_0_PHYSICAL.Comments_021) %>%
  rename("C2_11" = group_2_0_PHYSICAL.Comments_022) %>%
  rename("C2_12" = group_2_0_PHYSICAL.Comments_023) %>%
  rename("C2_13" = group_2_0_PHYSICAL.Comments_024) %>%
  rename("C2_14" = group_2_0_PHYSICAL.Comments_025) %>%
  rename("Q3_1" = group_3_0_SAFETY_WASTE_MANAGEMANT.Q_3_1) %>%
  rename("Q3_2" = group_3_0_SAFETY_WASTE_MANAGEMANT.Q_3_2) %>%
  rename("Q3_3" = group_3_0_SAFETY_WASTE_MANAGEMANT.Q_3_3) %>%
  rename("Q3_4" = group_3_0_SAFETY_WASTE_MANAGEMANT.Q_3_4) %>%
  rename("Q3_5" = group_3_0_SAFETY_WASTE_MANAGEMANT.Q_3_5) %>%
  rename("Q3_6" = group_3_0_SAFETY_WASTE_MANAGEMANT.Q_3_6) %>%
  rename("Q3_7" = group_3_0_SAFETY_WASTE_MANAGEMANT.Q_3_7) %>%
  rename("Q3_8" = group_3_0_SAFETY_WASTE_MANAGEMANT.Q_3_8) %>%
  rename("Q3_9" = group_3_0_SAFETY_WASTE_MANAGEMANT.Q_3_9) %>%
  rename("Q3_10" = group_3_0_SAFETY_WASTE_MANAGEMANT.Q_3_10) %>%
  rename("Q3_11" = group_3_0_SAFETY_WASTE_MANAGEMANT.Q_3_11_Highthrough) %>%
  rename("Q3_12" = group_3_0_SAFETY_WASTE_MANAGEMANT.Q_3_12_Highthrough) %>%
  rename("C3_1" = group_3_0_SAFETY_WASTE_MANAGEMANT.Comments_026) %>%
  rename("C3_2" = group_3_0_SAFETY_WASTE_MANAGEMANT.Comments_027) %>%
  rename("C3_3" = group_3_0_SAFETY_WASTE_MANAGEMANT.Comments_028) %>%
  rename("C3_4" = group_3_0_SAFETY_WASTE_MANAGEMANT.Comments_029) %>%
  rename("C3_5" = group_3_0_SAFETY_WASTE_MANAGEMANT.Comments_030) %>%
  rename("C3_6" = group_3_0_SAFETY_WASTE_MANAGEMANT.Comments_031) %>%
  rename("C3_7" = group_3_0_SAFETY_WASTE_MANAGEMANT.Comments_032) %>%
  rename("C3_8" = group_3_0_SAFETY_WASTE_MANAGEMANT.Comments_033) %>%
  rename("C3_9" = group_3_0_SAFETY_WASTE_MANAGEMANT.Comments_034) %>%
  rename("C3_10" = group_3_0_SAFETY_WASTE_MANAGEMANT.Comments_035) %>%
  rename("C3_11" = group_3_0_SAFETY_WASTE_MANAGEMANT.Comments_036) %>%
  rename("C3_12" = group_3_0_SAFETY_WASTE_MANAGEMANT.Comments_037) %>%
  rename("G4_Who_Decide" = group_4_0_PROCUREMENT.Who_decides_quantifies_lab_re) %>% #For adding a table b4 G4 
  rename("G4_Who_Decide_other" = group_4_0_PROCUREMENT.Specify_for_Other_002) %>%
  rename("G4_Wh_Qty_base" = group_4_0_PROCUREMENT.What_is_the_quantification_bas) %>%
  rename("G4_Wh_Qty_base_other" = group_4_0_PROCUREMENT.Specify_for_Other_003) %>%
  rename("G4_How_often" = group_4_0_PROCUREMENT.How_often_are_reagen_s_for_VL_IVT_ordered) %>%
  rename("G4_Comments" = group_4_0_PROCUREMENT.Comments_002_002) %>%
  rename("Q4_1" = group_4_0_PROCUREMENT.Q_4_1) %>%
  rename("Q4_2" = group_4_0_PROCUREMENT.Q_4_2) %>%
  rename("Q4_3" = group_4_0_PROCUREMENT.Q_4_3) %>%
  rename("Q4_4" = group_4_0_PROCUREMENT.Q_4_4) %>%
  rename("Q4_5" = group_4_0_PROCUREMENT.Q_4_5) %>%
  rename("Q4_6_H" = group_4_0_PROCUREMENT.Q_4_6_Highthrough) %>%
  rename("Q4_6_G" = group_4_0_PROCUREMENT.Q_4_6_GeneXpert) %>%
  rename("Q4_7" = group_4_0_PROCUREMENT.Q_4_7_Highthrough) %>%
  rename("Q4_8" = group_4_0_PROCUREMENT.Q_4_8_Highthrough) %>%
  rename("C4_1" = group_4_0_PROCUREMENT.Comments_038) %>% 
  rename("C4_2" = group_4_0_PROCUREMENT.Comments_039) %>% 
  rename("C4_3" = group_4_0_PROCUREMENT.Comments_040) %>%  
  rename("C4_4" = group_4_0_PROCUREMENT.Comments_041) %>%  
  rename("C4_5" = group_4_0_PROCUREMENT.Comments_042) %>%  
  rename("C4_6" = group_4_0_PROCUREMENT.Comments_043) %>%  
  rename("C4_7" = group_4_0_PROCUREMENT.Comments_044) %>%  
  rename("C4_8" = group_4_0_PROCUREMENT.Comments_045) %>%
  rename("Q5_1" = group_5_0_SAMPLE_MANAGEMENT_1.Q_5_1) %>%
  rename("Q5_2" = group_5_0_SAMPLE_MANAGEMENT_1.Q_5_2) %>%
  rename("Q5_3" = group_5_0_SAMPLE_MANAGEMENT_1.Q_5_3) %>%
  rename("Q5_4" = group_5_0_SAMPLE_MANAGEMENT_1.Q_5_4) %>%
  rename("Q5_5" = group_5_0_SAMPLE_MANAGEMENT_1.Q_5_5) %>%
  rename("Q5_6" = group_5_0_SAMPLE_MANAGEMENT_1.Q_5_6) %>%
  rename("Q5_7" = group_5_0_SAMPLE_MANAGEMENT_1.Q_5_7) %>%
  rename("Q5_8" = group_5_0_SAMPLE_MANAGEMENT_1.Q_5_8_Highthrough)%>%
  rename("C5_1" = group_5_0_SAMPLE_MANAGEMENT_1.Comments_046) %>%
  rename("C5_2" = group_5_0_SAMPLE_MANAGEMENT_1.Comments_047) %>%
  rename("C5_3" = group_5_0_SAMPLE_MANAGEMENT_1.Comments_048) %>%
  rename("C5_4" = group_5_0_SAMPLE_MANAGEMENT_1.Comments_049) %>%
  rename("C5_5" = group_5_0_SAMPLE_MANAGEMENT_1.Comments_050) %>%
  rename("C5_6" = group_5_0_SAMPLE_MANAGEMENT_1.Comments_051) %>%
  rename("C5_7" = group_5_0_SAMPLE_MANAGEMENT_1.Comments_052) %>%
  rename("C5_8" = group_5_0_SAMPLE_MANAGEMENT_1.Comments_053) %>%
  rename("Q6_1" = group_6_0_EQUIPMENT.Q_6_1) %>%
  rename("Q6_2" = group_6_0_EQUIPMENT.Q_6_2) %>%
  rename("Q6_3" = group_6_0_EQUIPMENT.Q_6_3) %>%
  rename("Q6_4" = group_6_0_EQUIPMENT.Q_6_4) %>%
  rename("Q6_5" = group_6_0_EQUIPMENT.Q_6_5) %>%
  rename("C6_1" = group_6_0_EQUIPMENT.Comments_054) %>%
  rename("C6_2" = group_6_0_EQUIPMENT.Comments_055) %>%
  rename("C6_3" = group_6_0_EQUIPMENT.Comments_056) %>%
  rename("C6_4" = group_6_0_EQUIPMENT.Comments_057) %>%
  rename("C6_5" = group_6_0_EQUIPMENT.Comments_059) %>%
  rename('Q7_1'=group_7_0_PROCESS_CONTROLS.Q_7_1 ) %>%
  rename('C7_1'=group_7_0_PROCESS_CONTROLS.Comments_060) %>%
  rename('Q7_2'=group_7_0_PROCESS_CONTROLS.Q_7_2) %>%
  rename('C7_2'=group_7_0_PROCESS_CONTROLS.Comments_061) %>%
  rename('Q7_3'=group_7_0_PROCESS_CONTROLS.Q_7_3) %>%
  rename('C7_3'=group_7_0_PROCESS_CONTROLS.Comments_062) %>%
  rename('Q7_4_H'=group_7_0_PROCESS_CONTROLS.Q_7_4_Highthrough) %>%
  rename('C7_4_H'=group_7_0_PROCESS_CONTROLS.Comments_063) %>%
  rename('Q7_5_H'=group_7_0_PROCESS_CONTROLS.Q_7_5_Highthrough) %>%
  rename('C7_5_H'=group_7_0_PROCESS_CONTROLS.Comments_064) %>%
  rename('Q7_6_H'=group_7_0_PROCESS_CONTROLS.Q_7_6_Highthrough) %>%
  rename('C7_6_H'=group_7_0_PROCESS_CONTROLS.Comments_065) %>%
  rename('Q7_4_G_Q7_7_H'=group_7_0_PROCESS_CONTROLS.Q_7_4) %>%
  rename('C7_4_G_C7_7_H'=group_7_0_PROCESS_CONTROLS.Comments_066) %>%
  rename('Q7_5_G_Q7_8_H'=group_7_0_PROCESS_CONTROLS.Q_7_5) %>%
  rename('C7_5_G_C7_8_H'=group_7_0_PROCESS_CONTROLS.Comments_067) %>%
  rename('Q7_6_G_Q7_9_H'=group_7_0_PROCESS_CONTROLS.Q_7_6) %>%
  rename('C7_6_G_C7_9_H'=group_7_0_PROCESS_CONTROLS.Comments_068) %>%
  rename('Q7_7_G_Q7_10_H'=group_7_0_PROCESS_CONTROLS.Q_7_7) %>%
  rename('C7_7_G_C7_10_H'=group_7_0_PROCESS_CONTROLS.Comments_069) %>%
  rename('Q7_8_G_Q7_11_H'=group_7_0_PROCESS_CONTROLS.Q_7_8) %>%
  rename('C7_8_G_C7_11_H'=group_7_0_PROCESS_CONTROLS.Comments_070) %>%
  rename('Q7_9_G_Q7_12_H'=group_7_0_PROCESS_CONTROLS.Q_7_9) %>%
  rename('C7_9_G_C7_12_H'=group_7_0_PROCESS_CONTROLS.Comments_071) %>%
  rename('Q7_10_G_Q7_13_H'=group_7_0_PROCESS_CONTROLS.Q_7_10) %>%
  rename('C7_10_G_C7_13_H'=group_7_0_PROCESS_CONTROLS.Comments_072) %>%
  rename('Q8_1'= POST_TESTING_PHASE.calculate_Q8_1) %>%
  rename('C8_1'= POST_TESTING_PHASE.Comments_075) %>%
  rename('Q8_2'= POST_TESTING_PHASE.Q_8_2) %>%
  rename('C8_2'= POST_TESTING_PHASE.Comments_076) %>%
  rename('Q8_3'= POST_TESTING_PHASE.Q_8_3) %>%
  rename('C8_3'= POST_TESTING_PHASE.Comments_077) %>%
  rename('Q8_4_G'= POST_TESTING_PHASE.Q_8_4_GeneXpert) %>%
  rename('Q8_4_H'= POST_TESTING_PHASE.Q_8_4_Highthrough) %>%
  rename('C8_4'= POST_TESTING_PHASE.Comments_078) %>%
  rename('Q8_5_G'= POST_TESTING_PHASE.Q_8_5_GeneXpert) %>%
  rename('Q8_5_H'= POST_TESTING_PHASE.Q_8_5_Highthrough) %>%
  rename('C8_5'= POST_TESTING_PHASE.Comments_079) %>%
  rename('Q8_6'= POST_TESTING_PHASE.Q_8_6) %>%
  rename('C8_6'= POST_TESTING_PHASE.Comments_080) %>%
  rename('Q8_7'= POST_TESTING_PHASE.Q_8_7) %>%
  rename('C8_7'= POST_TESTING_PHASE.Comments_081) %>%
  rename('Q8_8'= POST_TESTING_PHASE.Q_8_8) %>%
  rename('C8_8'= POST_TESTING_PHASE.Comments_082) %>%
  rename('Q8_9'= POST_TESTING_PHASE.Q_8_9) %>%
  rename('C8_9'= POST_TESTING_PHASE.Comments_083) %>%
  rename('Q8_10'= POST_TESTING_PHASE.Q_8_10) %>%
  rename('C8_10'= POST_TESTING_PHASE.Comments_084) %>%
  rename('Q8_11'= POST_TESTING_PHASE.Q_8_11) %>%
  rename('C8_11'= POST_TESTING_PHASE.Comments_085) %>%
  rename('Q9_1'= group_9_0_INTERNAL_QUALITY_AUDITS.Q_9_1) %>%
  rename('C9_1'= group_9_0_INTERNAL_QUALITY_AUDITS.Comments_086) %>%
  rename('Q9_2'= group_9_0_INTERNAL_QUALITY_AUDITS.Q_9_2) %>%
  rename('C9_2'= group_9_0_INTERNAL_QUALITY_AUDITS.Comments_087) %>%
  rename('Q9_3'= group_9_0_INTERNAL_QUALITY_AUDITS.Q_9_3) %>%
  rename('C9_3'= group_9_0_INTERNAL_QUALITY_AUDITS.Comments_088) %>%
  rename('Q9_4'= group_9_0_INTERNAL_QUALITY_AUDITS.Q_9_4) %>%
  rename('C9_4'= group_9_0_INTERNAL_QUALITY_AUDITS.Comments_089) %>%
  rename('Q9_5'= group_9_0_INTERNAL_QUALITY_AUDITS.Q_9_5) %>%
  rename('C9_5'= group_9_0_INTERNAL_QUALITY_AUDITS.Comments_090) %>%
  rename('Q9_6'= group_9_0_INTERNAL_QUALITY_AUDITS.Q_9_6) %>%
  rename('C9_6'= group_9_0_INTERNAL_QUALITY_AUDITS.Comments_091) %>%
  rename('Q9_7'= group_9_0_INTERNAL_QUALITY_AUDITS.Q_9_7) %>%
  rename('C9_7'= group_9_0_INTERNAL_QUALITY_AUDITS.Comments_092) %>%
  rename('Q9_8'= group_9_0_INTERNAL_QUALITY_AUDITS.Q_9_8) %>%
  rename('C9_8'= group_9_0_INTERNAL_QUALITY_AUDITS.Comments_093) 
  
  
  
 
#------Replace data values------- 
# replace_sub_platform = c('genexpert_1'='GeneXpert','genexpert_abbott'='Abbott','genexpert_biocentric'='Biocentric')
# data$sub_platform <- str_replace_all(data$sub_platform, replace_sub_platform)

data <- data %>% mutate_all(~replace(., is.nan(.), 0)) 
  
#------add re-coded new variables to the dataset------
data <- data %>%
  mutate(assessment_date = as.Date(assessment_date, format="%Y-%m-%d"))%>%
  mutate(year_month = format(assessment_date, "%Y %m")) %>%
  mutate(year = format(assessment_date, "%Y"))%>%
  mutate(max_score = ifelse(
    platform == "high_throughput", 106,91)
  )%>%
  mutate(sec1_max = 
         if_else(platform == "high_throughput", 11,
         if_else(platform == "genexpert",10,NA)),
         
         sec2_max = if_else(platform == "high_throughput", 14,
                            if_else(platform == "genexpert",12,NA)),
         
         sec3_max = if_else(platform == "high_throughput", 12,
                            if_else(platform == "genexpert",10,NA)),
         
         sec4_max = if_else(platform == "high_throughput", 8,
                            if_else(platform == "genexpert",6,NA)),
         
         sec5_max = if_else(platform == "high_throughput", 8,
                            if_else(platform == "genexpert",7,NA)),
         
         sec6_max = if_else(platform == "high_throughput", 5,
                            if_else(platform == "genexpert",5,NA)),
         
         sec7_max = if_else(platform == "high_throughput", 21,
                            if_else(platform == "genexpert",14,NA)),
         
         sec8_max = if_else(platform == "high_throughput", 19,
                            if_else(platform == "genexpert",19,NA)),
         
         sec9_max = if_else(platform == "high_throughput", 8,
                            if_else(platform == "genexpert",8,NA))
         
         ) %>%
  mutate(sec1_total = rowSums(across(c("Q1_1", "Q1_2", "Q1_3", "Q1_4", "Q1_5", "Q1_6", "Q1_7", "Q1_8", "Q1_9", "Q1_10", "Q1_11")), na.rm = TRUE),
         sec2_total = rowSums(across(c("Q2_1", "Q2_2", "Q2_3", "Q2_4", "Q2_5", "Q2_6", "Q2_7", "Q2_8", "Q2_9", "Q2_10", "Q2_11","Q2_12_G", "Q2_12_H", "Q2_13", "Q2_14" )), na.rm = TRUE),
         sec3_total = rowSums(across(c("Q3_1", "Q3_2", "Q3_3", "Q3_4", "Q3_5", "Q3_6", "Q3_7", "Q3_8", "Q3_9", "Q3_10", "Q3_11","Q3_12" )), na.rm = TRUE),
         sec4_total = rowSums(across(c("Q4_1", "Q4_2", "Q4_3", "Q4_4", "Q4_5", "Q4_6_H", "Q4_6_G", "Q4_7", "Q4_8")), na.rm = TRUE),
         sec5_total = rowSums(across(c("Q5_1", "Q5_2", "Q5_3", "Q5_4", "Q5_5", "Q5_6", "Q5_7", "Q5_8")), na.rm = TRUE),
         sec6_total = rowSums(across(c("Q6_1", "Q6_2", "Q6_3", "Q6_4", "Q6_5")), na.rm = TRUE),
         # sec7_total = rowSums(across(c('Q7_1','Q7_2','Q7_3','Q7_4_H','Q7_5_H','Q7_6_H','Q7_4_G_Q7_7_H','Q7_5_G_Q7_8_H','Q7_6_G_Q7_9_H','Q7_7_G_Q7_10_H','Q7_8_G_Q7_11_H','Q7_9_G_Q7_12_H','Q7_10_G_Q7_13_H')), na.rm = TRUE),
         sec7_total = group_7_0_PROCESS_CONTROLS.calculate_Q7,
         sec8_total = POST_TESTING_PHASE.calculate_Q8,
         sec9_total = group_9_0_INTERNAL_QUALITY_AUDITS.calculate_Q9
         )




#----removed records (testing data and incorrect data )-------
data <- subset(data,
                 KEY!= 'uuid:aea31654-2b8a-45c2-87b1-b027beeadb9d' &
                 KEY!= 'uuid:29b8c32e-7a6b-4773-bff0-8354bb5af3a9'
)


#-----reading user account excel sheet----
account <- read_xlsx("accounts.xlsx")


#----reading org_level excel sheet----
org_level <- read_xlsx("org_level.xlsx")


#-----join main data and org_level----

data <- data %>% left_join(org_level, 
                           by = "lab_name")


#----read the summary report from ODK central-----
url2 <- "https://nhlmmr-icap-vl.org/v1/projects/11/forms/VL_SCORECARD_1.svc/Submissions.summary_report"
r <- GET(url2, authenticate("nhlmmr.qa@gmail.com", "Nhl@#!1005x1005"))

text_content <- content(r, "text", encoding = "UTF-8")

library(jsonlite)

data_raw <- jsonlite::fromJSON(text_content)

summary <- data_raw$value

summary_join <- summary %>% left_join( data, 
                                             by=c('__Submissions-id'= 'KEY'))%>%
  select('assessment_date','org','state_region','lab_name','org_central',c(1:5)) #'sub_platform', 

#----read the group_personnel from ODK central-----
url3 <- "https://nhlmmr-icap-vl.org/v1/projects/11/forms/VL_SCORECARD_1.svc/Submissions.group_personnel"
r2 <- GET(url3, authenticate("nhlmmr.qa@gmail.com", "Nhl@#!1005x1005"))

text_content <- content(r2, "text", encoding = "UTF-8")

library(jsonlite)

data_raw <- jsonlite::fromJSON(text_content)

summary <- data_raw$value

summary_join <- summary %>% left_join( data, 
                                       by=c('__Submissions-id'= 'KEY'))%>%
  select('assessment_date','org','state_region','lab_name','org_central',c(1:5)) #'sub_platform', 



#-------defining theme of application------
theme <- bs_theme(version = 5,
                  bootswatch = "flatly",
                 "navbar-bg"="rgb(51,87,129)"
)

#---------------UI------------------

ui <-
  
  page_sidebar(
  theme = theme,
 
  title = "Welcome to Virological Testing Scorecard Dashboard, Myanmar",
  
#-----loading message css --------
  tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 200px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
             }
          ")),
  
  #----- login -----
  useShinyjs(),
  
 hidden(div(
    id = "login-fail", 
    style = "width: 500px; max-width: 100%; ",
    
    div(
      class = "well",
      style = "background-color: #CA1818; ",
      h3(class = "text-center", "Sorry!"),
      h4(class = "text-center", "Logged In Failed, Please recheck credentials and Try Again.")
    ))),
  
  
  div(
    id = "front_image",
    style = "height: 120px; text-align: center;  position:relative;",
    imageOutput("front_img")%>% withSpinner()
  ),
 
 div(
   id = "title",
   style = "height: 50px; text-align: center;  position:relative;",
   h5(class = "text-center", "HIV Viral Load and Infant Virological Testing Scorecard", style = "color: #073980;"),
   p(class = "text-center", "Myanmar", style = "color: #073980;")
 ),
  
 
  div(
    id = "login-basic", 
    style = "width: 500px; background-color: #F0F8FF; max-width: 100%; margin: 0 auto;opacity: 0.8; position:relative; z-index:1;",
    
    div(
      class = "well",
      h6(class = "text-center", "Please login"),
      
      textInput(
        inputId     = "ti_user_name_basic", 
        label       = tagList(icon("user"), 
                              "User Name"),
        placeholder = "Enter user name"
      ),
      
      passwordInput(
        inputId     = "ti_password_basic", 
        label       = tagList(icon("unlock-alt"), 
                              "Password"), 
        placeholder = "Enter password"
      ), 
      
      actionButton(inputId = "log",label = "Log In",
                   style= "background-color:#002D62; color:white;")
    )
  ),

#-------error message hidden --------
 tags$style(type="text/css",
            ".shiny-output-error { visibility: hidden; }",
            ".shiny-output-error:before { visibility: hidden; }"
 ),
  
#-------------DASHBOARD----------------------  
   
  div(id = "dash",
      
  navset_card_tab(
                  title = "Dashboard",
                  
                  
        
#-------------Overview Nav Panel-------------                  
            nav_panel("Overview", 
                  layout_columns(
                  card(
                    card_header("Frequency of Assessments by Year"),
                    plotOutput("visits_by_year")%>% withSpinner(),
                    card_footer("Frequency: A site can be visited more than once")
                    ),
                  
                  card(
                    card_header("Frequency of Assessments by States"),
                    plotOutput("visits_by_sr")%>% withSpinner(),
                    card_footer("Frequency: A site can be visited more than once")
                  ),
                  
                  card(
                    card_header("Frequency of Assessments by Organizations"),
                    plotOutput("visits_by_org_pie")%>% withSpinner(),
                    card_footer("AHRN: Asian Harm Reduction Network | Myanmar,  LPK: Lan Pya Kyal ,
                                MAM: Medical Action Myanmar, MSF:Médecins Sans Frontières (CH: SWitzerland, H: Horland) ",  style = "font-size: 11px;")

                  ),
                  
                  
                  card(
                    card_header("Frequency of Assessments by Year, Month and Platform Types"),
                    plotOutput("visits_by_year_month")%>% withSpinner()
                    ),
                  
                  card(
                    card_header("List of sites with assessment scores"),
                    downloadButton("dl_visit_table","Download Site list table"),
                    DT::DTOutput("visit_table")
                    
                  ),
                  
                  col_widths = c(4,4,4,12,12)
                  )
                  
              ), # end of nav_panel overview


#-----overall scores nav panel-----
   nav_panel("Overall Scores",
             
             htmlOutput("description"),
             
             layout_columns(
               
               card(
                 card_header("Overall scores of each section (High Throughput)"),
                 plotOutput("overall_scores_h")%>% withSpinner(),
                 card_footer("Overall scores is the cumulative % of all sites if no site is selected from the side-bar filter")
               ),
               
               card(
                 card_header("Overall scores of each section (GeneXpert)"),
                 plotOutput("overall_scores_g")%>% withSpinner(),
                 card_footer("Overall scores is the cumulative % of all sites if no site is selected from the side-bar filter")
               ),
               
               card(
                 card_header("Spider Chart of section scores (High Throughput)"),
                 plotOutput("radar_scores_h")%>% withSpinner()
                 
               ),
               
               card(
                 card_header("Spider Chart of section scores (GeneXpert)"),
                 plotOutput("radar_scores_g")%>% withSpinner()
                 
               ),
               
              
               card(
                 card_header("Table of Section Scores and Final Scores"),
                 downloadButton("dl_overall_scores_table","Download table"),
                 DT::DTOutput("overall_scores_table")
                 
               ),
               
               
               col_widths = c(6,6,6,6,12)
               
             ) # end of layout_columns
             
             
             ), #end of nav_panel overall_scores


#----Section 1 Nav Panel-----  
   nav_panel ("Sec 1 Scores",
              layout_columns(
                
                card(
                  height = 600,
                  full_screen = TRUE,
                  card_header("Section 1: Personnel Scores: Hightroughput Sites"),
                  plotOutput("sec1_scores_h")%>% withSpinner()
                ),
                
                
                card(
                  height = 600,
                  full_screen = TRUE,
                  card_header("Section 1: Personnel Scores: GeneXpert Sites"),
                  plotOutput("sec1_scores_g")%>% withSpinner()
                ),
                
                
                card(
                  height = 600,
                  card_header("Section 1: Detail Question-wise scores"),
                  downloadButton("dl_sec1_table","Download table"),
                  DT::DTOutput("sec1_table")

                ),

                
                col_widths = c(6,6,12)
              )# end of layput_columns
              
              
              ), #end of nav_panel sec1 scores



#----Section 2 Nav Panel-----  
nav_panel ("Sec 2 Scores",
           layout_columns(
             
             card(
               height = 600,
               full_screen = TRUE,
               card_header("Section 2: Physical Facility Scores: Hightroughput Sites"),
               plotOutput("sec2_scores_h")%>% withSpinner()
             ),
             
             card(
               height = 600,
               full_screen = TRUE,
               card_header("Section 2: Physical Facility Scores: GeneXpert Sites"),
               plotOutput("sec2_scores_g")%>% withSpinner()
             ),
             
             
             
             card(
               height = 600,
               card_header("Section 2: Detail Question-wise scores"),
               downloadButton("dl_sec2_table","Download table"),
               DT::DTOutput("sec2_table")
               
             ),
             
             
             col_widths = c(6,6,12)
           )# end of layput_columns
           
           
), #end of nav_panel sec2 scores



#----Section 3 Nav Panel-----  
nav_panel ("Sec 3 Scores",
           layout_columns(
             
             card(
               height = 600,
               full_screen = TRUE,
               card_header("Section 3: Safety Scores: Hightroughput Sites"),
               plotOutput("sec3_scores_h")%>% withSpinner()
             ),
             
             
             card(
               height = 600,
               full_screen = TRUE,
               card_header("Section 3: Safety Scores: GeneXpert Sites"),
               plotOutput("sec3_scores_g")%>% withSpinner()
             ),
             
            
             card(
               height = 600,
               card_header("Section 3: Detail Question-wise scores"),
               downloadButton("dl_sec3_table","Download table"),
               DT::DTOutput("sec3_table")
               
             ),
             
             
             col_widths = c(6,6,12)
           )# end of layput_columns
           
           
), #end of nav_panel sec3 scores


#----Section 4 Nav Panel-----  
nav_panel ("Sec 4 Scores",
           layout_columns(
             
             card(
               height = 600,
               full_screen = TRUE,
               card_header("Section 4: Inventory Scores: Hightroughput Sites"),
               plotOutput("sec4_scores_h")%>% withSpinner()
             ),
             
             card(
               height = 600,
               full_screen = TRUE,
               card_header("Section 4: Inventory Scores: GeneXpert Sites"),
               plotOutput("sec4_scores_g")%>% withSpinner()
             ),
             
             
             
             card(
               height = 600,
               card_header("Section 4: Detail Question-wise scores"),
               downloadButton("dl_sec4_table","Download table"),
               DT::DTOutput("sec4_table")
               
             ),
             
             
             col_widths = c(6,6,12)
           )# end of layput_columns
           
           
), #end of nav_panel sec4 scores


#----Section 5 Nav Panel-----  
nav_panel ("Sec 5 Scores",
           layout_columns(
             
             card(
               height = 600,
               full_screen = TRUE,
               card_header("Section 5: Sample Management: Hightroughput Sites"),
               plotOutput("sec5_scores_h")%>% withSpinner()
             ),
             
             card(
               height = 600,
               full_screen = TRUE,
               card_header("Section 5: Sample Management: GeneXpert Sites"),
               plotOutput("sec5_scores_g")%>% withSpinner()
             ),
             
            
             
             card(
               height = 600,
               card_header("Section 5: Detail Question-wise scores"),
               downloadButton("dl_sec5_table","Download table"),
               DT::DTOutput("sec5_table")
               
             ),
             
             
             col_widths = c(6,6,12)
           )# end of layput_columns
           
           
), #end of nav_panel sec5 scores


#----Section 6 Nav Panel-----  
nav_panel ("Sec 6 Scores",
           layout_columns(
             
             card(
               height = 600,
               full_screen = TRUE,
               card_header("Section 6: Equipment: Hightroughput Sites"),
               plotOutput("sec6_scores_h")%>% withSpinner()
             ),
             
             
             card(
               height = 600,
               full_screen = TRUE,
               card_header("Section 6: Equipment: GeneXpert Sites"),
               plotOutput("sec6_scores_g")%>% withSpinner()
             ),
             
             
             card(
               height = 600,
               card_header("Section 6: Detail Question-wise scores"),
               downloadButton("dl_sec6_table","Download table"),
               DT::DTOutput("sec6_table")
               
             ),
             
             
             col_widths = c(6,6,12)
           )# end of layput_columns
           
           
), #end of nav_panel sec6 scores



#----Section 7 Nav Panel-----  
nav_panel ("Sec 7 Scores",
           layout_columns(
             
             card(
               height = 600,
               full_screen = TRUE,
               card_header("Section 7: Process Controls: Hightroughput Sites"),
               plotOutput("sec7_scores_h")%>% withSpinner()
             ),
             
             card(
               height = 600,
               full_screen = TRUE,
               card_header("Section 7: Process Controls: GeneXpert Sites"),
               plotOutput("sec7_scores_g")%>% withSpinner()
             ),
             
             
             
             card(
               height = 600,
               card_header("Section 7: Detail Question-wise scores"),
               downloadButton("dl_sec7_table","Download table"),
               DT::DTOutput("sec7_table")
               
             ),
             
             
             col_widths = c(6,6,12)
           )# end of layput_columns
           
           
), #end of nav_panel sec7 scores



#----Section 8 Nav Panel-----  
nav_panel ("Sec 8 Scores",
           layout_columns(
             
             card(
               height = 600,
               full_screen = TRUE,
               card_header("Section 8: M and E : Hightroughput Sites"),
               plotOutput("sec8_scores_h")%>% withSpinner()
             ),
             
             card(
               height = 600,
               full_screen = TRUE,
               card_header("Section 8: M and E : GeneXpert Sites"),
               plotOutput("sec8_scores_g")%>% withSpinner()
             ),
             
            
             
             card(
               height = 600,
               card_header("Section 8: Detail Question-wise scores"),
               downloadButton("dl_sec8_table","Download table"),
               DT::DTOutput("sec8_table")
               
             ),
             
             
             col_widths = c(6,6,12)
           )# end of layput_columns
           
           
), #end of nav_panel sec8 scores



#----Section 9 Nav Panel-----  
nav_panel ("Sec 9 Scores",
           layout_columns(
             
             card(
               height = 600,
               full_screen = TRUE,
               card_header("Section 9: Internal Quality Audits  : Hightroughput Sites"),
               plotOutput("sec9_scores_h")%>% withSpinner()
             ),
             
             card(
               height = 600,
               full_screen = TRUE,
               card_header("Section 9: Internal Quality Audits : GeneXpert Sites"),
               plotOutput("sec9_scores_g")%>% withSpinner()
             ),
             
            
             
             card(
               height = 600,
               card_header("Section 9: Detail Question-wise scores"),
               downloadButton("dl_sec9_table","Download table"),
               DT::DTOutput("sec9_table")
               
             ),
             
             
             col_widths = c(6,6,12)
           )# end of layput_columns
           
           
), #end of nav_panel sec9 scores


#----summary recommendations Tab-----  
nav_panel ("Summary Recommendations",
           layout_columns(
             
            
             card(
               height = 600,
               card_header("AUDITOR’S SUMMARY REPORT"),
               downloadButton("dl_summary","Download table"),
               DT::DTOutput("summary")
               
             ),
             
             
             col_widths = c(12)
           )# end of layput_columns
           
           
), #end of summary recommendations


#---------Site Level Report H Panel Tab -----
nav_panel(
  "Site-level-Report_Highthroughput",
  htmlOutput("fac_info_H"),
  htmlOutput("personnal_H"),
  htmlOutput("sec1_report_H"),
  htmlOutput("sec2_report_H"),
  htmlOutput("sec3_report_H"),
  htmlOutput("sec4_report_H"),
  htmlOutput("sec5_report_H"),
  htmlOutput("sec6_report_H"),
  htmlOutput("sec7_report_H"),
  htmlOutput("sec8_report_H"),
  htmlOutput("sec9_report_H"),
  # print(data$G4_Comments) #checking data in a field with print in the Console
), # end of site level report


#---------Site Level Report G Panel Tab -----
nav_panel(
  #"Site-level-Report_GeneXpert",
  "Laboratory Audit Report",
  htmlOutput("fac_info_G"),
  htmlOutput("sec1_report_G"),
  htmlOutput("sec2_report_G"),
  htmlOutput("sec3_report_G"),
  htmlOutput("sec4_report_G"),
  htmlOutput("sec5_report_G"),
  htmlOutput("sec6_report_G"),
  htmlOutput("sec7_report_G"),
  htmlOutput("sec8_report_G"),
  htmlOutput("sec9_report_G")
  
), # end of site level report

nav_panel("Three", p("Third page content.")))

  ),
    
  
#-----sidebar panel-------
  sidebar = sidebar(
    open = "closed",
    useShinyjs(),
    hidden(div(id = "side", 
        dateInput("startdate",
                  "Start Date:",
                  value = Sys.Date()-(365),
                  format = "yyyy/mm/dd"),
        
        dateInput("enddate",
                  "End Date:",
                  value = Sys.Date(),
                  format = "yyyy/mm/dd"),
        
        selectInput("platform",
                    "Platfrom:",
                    choices = c("High throughput – Abbott", "High throughput – Biocentric", "GeneXpert")), #choices = c("ALL","genexpert", "high_throughput")),
        
        selectInput("org",
                    "Organization:",
                    choices= NULL),
        
        selectInput("sr",
                    "State/Region:",
                    choices= NULL),
        
        selectInput("facility",
                    "Facility:",
                    choices= NULL)
        
      
        
    )))
  
  )

  
    
#--------SERVER---------------------------------


server <- function(input,output,session) {
  
  account_filtered <- reactive({
    filter(account,
           username == input$ti_user_name_basic & 
             pass == input$ti_password_basic)
    
  })
  
#----------images------
  
  output$front_img <- renderImage({
    
    list(src = "emoji.png",
         width = "250px",
         Height = "50px"
         )
    
  }, deleteFile = F)
  
#----- data1 and data2-----
  data1 <- eventReactive(input$log,{ 
    
    
    if (input$ti_user_name_basic == "nhl_central" 
       
    ) {
      filter(data,
             assessment_date >= ymd(input$startdate) & 
               assessment_date <= ymd(input$enddate) &
               org_central == "nhl" |
               org_central == "nhl,phl" |
               org_central == "nhl,mam" |
               org_central == "nhl,msf" |
               org_central == "nhl,ahrn" 
             )
    } 
    
    else if (
        input$ti_user_name_basic == "phl_central" 
       
    ) {
      filter(data,
             assessment_date >= ymd(input$startdate) & 
               assessment_date <= ymd(input$enddate) &
             
               org_central == "nhl,phl" 
              
      )
    }
    
    
    else if (
     
      input$ti_user_name_basic == "mam_central" 
     
    ) {
      filter(data,
             assessment_date >= ymd(input$startdate) & 
               assessment_date <= ymd(input$enddate) &
               
               org_central == "nhl,mam" 
             
      )
    }
    
    
    else if (
      
      
      input$ti_user_name_basic == "msf_central" 
     
    ) {
      filter(data,
             assessment_date >= ymd(input$startdate) & 
               assessment_date <= ymd(input$enddate) &
               
               org_central == "nhl,msf" 
             
      )
    }
    
    else if (
      input$ti_user_name_basic == "ahrn_central"
    ) {
      filter(data,
             assessment_date >= ymd(input$startdate) & 
               assessment_date <= ymd(input$enddate) &
               
               org_central == "nhl,ahrn" 
             
      )
    }
    
    else {
      filter(data,
             assessment_date >= ymd(input$startdate) & 
               assessment_date <= ymd(input$enddate) &
               org == account_filtered()$org &
               state_region == account_filtered()$sr &
               lab_name == account_filtered()$fac &
               platform == account_filtered()$platform #&
               # sub_platform == account_filtered()$sub_platform
             )
    }
    
  
   
    
  })
  
  
  data2 <- eventReactive(input$log,{ 
    
    
    if (input$ti_user_name_basic == "nhl_central" 
        
    ) {
      filter(summary_join,
             assessment_date >= ymd(input$startdate) & 
               assessment_date <= ymd(input$enddate) &
               org_central == "nhl" |
               org_central == "nhl,phl" |
               org_central == "nhl,mam" |
               org_central == "nhl,msf" |
               org_central == "nhl,ahrn" 
      )
    } 
    
    else if (
      input$ti_user_name_basic == "phl_central" 
      
    ) {
      filter(summary_join,
             assessment_date >= ymd(input$startdate) & 
               assessment_date <= ymd(input$enddate) &
               
               org_central == "nhl,phl" 
             
      )
    }
    
    
    else if (
      
      input$ti_user_name_basic == "mam_central" 
      
    ) {
      filter(summary_join,
             assessment_date >= ymd(input$startdate) & 
               assessment_date <= ymd(input$enddate) &
               
               org_central == "nhl,mam" 
             
      )
    }
    
    
    else if (
      
      
      input$ti_user_name_basic == "msf_central" 
      
    ) {
      filter(summary_join,
             assessment_date >= ymd(input$startdate) & 
               assessment_date <= ymd(input$enddate) &
               
               org_central == "nhl,msf" 
             
      )
    }
    
    else if (
      input$ti_user_name_basic == "ahrn_central"
    ) {
      filter(summary_join,
             assessment_date >= ymd(input$startdate) & 
               assessment_date <= ymd(input$enddate) &
               
               org_central == "nhl,ahrn" 
             
      )
    }
    
    else {
      filter(summary_join,
             assessment_date >= ymd(input$startdate) & 
               assessment_date <= ymd(input$enddate) &
               org == account_filtered()$org &
               state_region == account_filtered()$sr &
               lab_name == account_filtered()$fac &
               platform == account_filtered()$platform #&
               # sub_platform == account_filtered()$sub_platform
      )
    }
    
    
    
    
  })
  
  

  
  
  
  
#----user account check ------- 
  
  # create userbase
  user_base_basic_tbl <- tibble(
    user_name = account$username,
    password  = account$pass)
  
  
  # check credentials vs tibble
  validate_password_basic <- eventReactive(input$log, {
    
    validate <- FALSE
    
    if (input$ti_user_name_basic %in% user_base_basic_tbl$user_name &&
        input$ti_password_basic %in%  user_base_basic_tbl$password && (which(user_base_basic_tbl$user_name == input$ti_user_name_basic) == which(user_base_basic_tbl$password == input$ti_password_basic))) 
    {validate <- TRUE
    }
  })
  
  validate_password_basic_f <- eventReactive(input$log,{
    validate <- FALSE
    
    if ((!input$ti_user_name_basic %in% user_base_basic_tbl$user_name &&
         !input$ti_password_basic %in% user_base_basic_tbl$password) |
        (input$ti_user_name_basic %in% user_base_basic_tbl$user_name &&
         !input$ti_password_basic %in% user_base_basic_tbl$password) |
        (!input$ti_user_name_basic %in% user_base_basic_tbl$user_name &&
         input$ti_password_basic %in% user_base_basic_tbl$password)
    )
    {validate <- TRUE}
  })
  
#--------show/hide form------
  
  shinyjs::hide(id = "dash")

  observeEvent(validate_password_basic(), {
    shinyjs::hide(id = "login-basic")
    shinyjs::show(id = "side")
    shinyjs::show(id = "dash")
    shinyjs::hide(id = "login-fail")
    shinyjs::hide(id = "front_image")
    shinyjs::hide(id = "title")
  })
  
  observeEvent(validate_password_basic_f(), {
    shinyjs::show(id = "login-fail")
  })
  
#-------- Choice Filter ---------- 
  
##------ Choice filter for platform------  
  observe( {
    if(input$ti_user_name_basic == "nhl_central" 
       ){
      x <- data %>% 
        filter(
          org_central == "nhl" |
            org_central == "nhl,phl" |
            org_central == "nhl,mam" |
            org_central == "nhl,msf" |
            org_central == "nhl,ahrn" 
        )
      updateSelectInput( session,"platform",
                        "Platform:",
                        choices = c("ALL",unique(x$platform)))
    } else if(input$ti_user_name_basic == "phl_central" 
    ){
      x <- data %>% 
        filter(
            org_central == "nhl,phl" 
        )
      updateSelectInput( session,"platform",
                         "Platform:",
                         choices = c("ALL",unique(x$platform)))
    } else if(input$ti_user_name_basic == "mam_central" 
    ){
      x <- data %>% 
        filter(
          org_central == "nhl,mam" 
        )
      updateSelectInput( session,"platform",
                         "Platform:",
                         choices = c("ALL",unique(x$platform)))
    } else if(input$ti_user_name_basic == "msf_central" 
    ){
      x <- data %>% 
        filter(
          org_central == "nhl,msf" 
        )
      updateSelectInput( session,"platform",
                         "Platform:",
                         choices = c("ALL",unique(x$platform)))
    } else if(input$ti_user_name_basic == "ahrn_central" 
    ){
      x <- data %>% 
        filter(
          org_central == "nhl,ahrn" 
        )
      updateSelectInput( session,"platform",
                         "Platform:",
                         choices = c("ALL",unique(x$platform)))
    } 
    
    else if (input$ti_user_name_basic != "" & input$ti_password_basic != "") { 
      x <- data %>% 
        filter(
          org == account_filtered()$org
        )
      updateSelectInput( session,"platform",
                         "Platform:",
                         choices = c("ALL",unique(x$platform)))
    } else{
      updateSelectInput( session,"org",
                         "Organization:",
                         choices = c("ALL"))
    }
    
  })

    
##----Choice filter for organization-------
  observe({
    if(input$ti_user_name_basic == "nhl_central" & input$platform == "ALL"){
      x <- data %>% 
        filter(
            (org_central == "nhl" |
            org_central == "nhl,phl" |
            org_central == "nhl,mam" |
            org_central == "nhl,msf" |
            org_central == "nhl,ahrn" ) & 
            (platform == "high_throughput" | platform == "genexpert") )
       
      updateSelectInput( session,"org",
                         "Organization:",
                         choices = c("ALL",unique(x$org)))
      
    } else if (input$ti_user_name_basic == "nhl_central" & input$platform != "ALL") {
      x <- data %>% 
        filter(
            (org_central == "nhl" |
            org_central == "nhl,phl" |
            org_central == "nhl,mam" |
            org_central == "nhl,msf" |
            org_central == "nhl,ahrn" ) &
            (platform == input$platform)
        ) 
      updateSelectInput( session,"org",
                         "Organization:",
                         choices = c("ALL",unique(x$org)))
      
    }
    
    else if(input$ti_user_name_basic == "phl_central" & input$platform == "ALL"
    ){
      x <- data %>% 
        filter(
          org_central == "nhl,phl"
        )
      updateSelectInput( session,"org",
                         "Organization:",
                         choices = c("ALL",unique(x$org)))
    }  else if(input$ti_user_name_basic == "phl_central" & input$platform != "ALL"
    ){
      x <- data %>% 
        filter(
          org_central == "nhl,phl" &
            (platform == input$platform)
        )
      updateSelectInput( session,"org",
                         "Organization:",
                         choices = c("ALL",unique(x$org)))
    } else if(input$ti_user_name_basic == "mam_central" & input$platform == "ALL"
    ){
      x <- data %>% 
        filter(
          org_central == "nhl,mam" 
        )
      updateSelectInput( session,"org",
                         "Organization:",
                         choices = c("ALL",unique(x$org)))
    } else if(input$ti_user_name_basic == "mam_central" & input$platform != "ALL"
    ){
      x <- data %>% 
        filter(
          org_central == "nhl,mam" &
            (platform == input$platform)
        )
      updateSelectInput( session,"org",
                         "Organization:",
                         choices = c("ALL",unique(x$org)))
    }else if(input$ti_user_name_basic == "msf_central" & input$platform == "ALL"
    ){
      x <- data %>% 
        filter(
          org_central == "nhl,msf" 
        )
      updateSelectInput( session,"org",
                         "Organization:",
                         choices = c("ALL",unique(x$org)))
    }else if(input$ti_user_name_basic == "msf_central" & input$platform != "ALL"
    ){
      x <- data %>% 
        filter(
          org_central == "nhl,msf" &
            (platform == input$platform)
        )
      updateSelectInput( session,"org",
                         "Organization:",
                         choices = c("ALL",unique(x$org)))
    } else if(input$ti_user_name_basic == "ahrn_central" & input$platform == "ALL"
    ){
      x <- data %>% 
        filter(
          org_central == "nhl,ahrn" 
        )
      updateSelectInput( session,"org",
                         "Organization:",
                         choices = c("ALL",unique(x$org)))

    }else if(input$ti_user_name_basic == "ahrn_central" & input$platform != "ALL"
    ){
      x <- data %>% 
        filter(
          org_central == "nhl,ahrn" &
            (platform == input$platform)
        )
      updateSelectInput( session,"org",
                         "Organization:",
                         choices = c("ALL",unique(x$org)))
      
    } else if (input$ti_user_name_basic != "" & input$ti_password_basic != "") {
      x <- data %>% 
        filter(
          org == account_filtered()$org & 
          platform == account_filtered()$platform
        )
      updateSelectInput( session,"org",
                         "Organization:",
                         choices = c("ALL",unique(x$org)))
    } else{
      updateSelectInput( session,"org",
                         "Organization:",
                         choices = c("ALL"))
    }
    
    
  })  
  
  ##----Choice filter for state_region-------
  observe({
    if(input$ti_user_name_basic == "nhl_central" & input$platform == "ALL" & input$org == "ALL"
    ){
      x <- data %>% 
        filter(
          org_central == "nhl" |
            org_central == "nhl,phl" |
            org_central == "nhl,mam" |
            org_central == "nhl,msf" |
            org_central == "nhl,ahrn" 
        )
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL",unique(x$state_region)))
    } else if (input$ti_user_name_basic == "nhl_central" & input$platform == "ALL" & input$org != "ALL"
    ){
      x <- data %>% 
        filter(
          (org_central == "nhl" |
            org_central == "nhl,phl" |
            org_central == "nhl,mam" |
            org_central == "nhl,msf" |
            org_central == "nhl,ahrn" ) &
            (org == input$org)
        )
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL",unique(x$state_region))) }
    else if (input$ti_user_name_basic == "nhl_central" & input$platform != "ALL" & input$org == "ALL"
    ){
      x <- data %>% 
        filter(
          (org_central == "nhl" |
             org_central == "nhl,phl" |
             org_central == "nhl,mam" |
             org_central == "nhl,msf" |
             org_central == "nhl,ahrn" ) &
            (platform == input$platform)
        )
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL",unique(x$state_region))) }
    
    else if (input$ti_user_name_basic == "nhl_central" & input$platform != "ALL" & input$org != "ALL"
    ){
      x <- data %>% 
        filter(
          (org_central == "nhl" |
             org_central == "nhl,phl" |
             org_central == "nhl,mam" |
             org_central == "nhl,msf" |
             org_central == "nhl,ahrn" ) &
            (platform == input$platform) &
            (org == input$org)
        )
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL",unique(x$state_region))) }
    
    
    else if(input$ti_user_name_basic == "phl_central" & input$platform == "ALL" & input$org == "ALL"
    ){
      x <- data %>% 
        filter(
         
            org_central == "nhl,phl" 
           
        )
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL",unique(x$state_region)))
    } else if (input$ti_user_name_basic == "phl_central" & input$platform == "ALL" & input$org != "ALL"
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,phl"  ) &
            (org == input$org)
        )
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL",unique(x$state_region))) }
    else if (input$ti_user_name_basic == "phl_central" & input$platform != "ALL" & input$org == "ALL"
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,phl"  ) &
            (platform == input$platform)
        )
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL",unique(x$state_region))) }
    
    else if (input$ti_user_name_basic == "phl_central" & input$platform != "ALL" & input$org != "ALL"
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,phl" ) &
            (platform == input$platform) &
            (org == input$org)
        )
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL",unique(x$state_region))) }
    
    else if(input$ti_user_name_basic == "mam_central" & input$platform == "ALL" & input$org == "ALL"
    ){
      x <- data %>% 
        filter(
          
            org_central == "nhl,mam"  
        )
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL",unique(x$state_region)))
    } else if (input$ti_user_name_basic == "mam_central" & input$platform == "ALL" & input$org != "ALL"
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,mam" 
             ) &
            (org == input$org)
        )
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL",unique(x$state_region))) }
    else if (input$ti_user_name_basic == "mam_central" & input$platform != "ALL" & input$org == "ALL"
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,mam"  ) &
            (platform == input$platform)
        )
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL",unique(x$state_region))) }
    
    else if (input$ti_user_name_basic == "mam_central" & input$platform != "ALL" & input$org != "ALL"
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,mam"  ) &
            (platform == input$platform) &
            (org == input$org)
        )
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL",unique(x$state_region))) }
    
    
    else if(input$ti_user_name_basic == "msf_central" & input$platform == "ALL" & input$org == "ALL"
    ){
      x <- data %>% 
        filter(
         
            org_central == "nhl,msf"  
        )
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL",unique(x$state_region)))
    } else if (input$ti_user_name_basic == "msf_central" & input$platform == "ALL" & input$org != "ALL"
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,msf" 
             ) &
            (org == input$org)
        )
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL",unique(x$state_region))) }
    else if (input$ti_user_name_basic == "msf_central" & input$platform != "ALL" & input$org == "ALL"
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,msf" 
              ) &
            (platform == input$platform)
        )
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL",unique(x$state_region))) }
    
    else if (input$ti_user_name_basic == "msf_central" & input$platform != "ALL" & input$org != "ALL"
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,msf"  ) &
            (platform == input$platform) &
            (org == input$org)
        )
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL",unique(x$state_region))) }
    
    
    else if(input$ti_user_name_basic == "ahrn_central" & input$platform == "ALL" & input$org == "ALL"
    ){
      x <- data %>% 
        filter(
         
            org_central == "nhl,ahrn" 
        )
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL",unique(x$state_region)))
    } else if (input$ti_user_name_basic == "ahrn_central" & input$platform == "ALL" & input$org != "ALL"
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,ahrn" ) &
            (org == input$org)
        )
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL",unique(x$state_region))) }
    else if (input$ti_user_name_basic == "ahrn_central" & input$platform != "ALL" & input$org == "ALL"
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,ahrn" ) &
            (platform == input$platform)
        )
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL",unique(x$state_region))) }
    
    else if (input$ti_user_name_basic == "ahrn_central" & input$platform != "ALL" & input$org != "ALL"
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,ahrn" ) &
            (platform == input$platform) &
            (org == input$org)
        )
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL",unique(x$state_region))) }
    
    
    else if (input$ti_user_name_basic != "" & input$ti_password_basic != "") {
      x <- data %>% 
        filter(
          lab_name == account_filtered()$fac 
        )
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL",unique(x$state_region)))
    }
    
    else {
      updateSelectInput( session,"sr",
                         "State/Region:",
                         choices = c("ALL"))
    }
    
  })  
  
  
  ##----Choice filter for facility-------
  observe({
    if(input$ti_user_name_basic == "nhl_central" &
      input$platform == "ALL" &
      input$org == "ALL" &
      input$sr == "ALL"
       
    ){
      x <- data %>% 
        filter(
            org_central == "nhl" |
            org_central == "nhl,phl" |
            org_central == "nhl,mam" |
            org_central == "nhl,msf" |
            org_central == "nhl,ahrn" 
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name)))
    } else if (input$ti_user_name_basic == "nhl_central" &
               input$platform == "ALL" &
               input$org == "ALL" &
               input$sr != "ALL"
               
    ){
      x <- data %>% 
        filter(
            (org_central == "nhl" |
            org_central == "nhl,phl" |
            org_central == "nhl,mam" |
            org_central == "nhl,msf" |
            org_central == "nhl,ahrn") &
            state_region == input$sr
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name)))
    }
      else if (input$ti_user_name_basic == "nhl_central" &
               input$platform == "ALL" &
               input$org != "ALL" &
               input$sr != "ALL"
               
      ){
        x <- data %>% 
          filter(
            (org_central == "nhl" |
               org_central == "nhl,phl" |
               org_central == "nhl,mam" |
               org_central == "nhl,msf" |
               org_central == "nhl,ahrn") &
              state_region == input$sr &
              org == input$org
          )
        updateSelectInput( session,"facility",
                           "Facility:",
                           choices = c("ALL",unique(x$lab_name)))
      }
        
        else if (input$ti_user_name_basic == "nhl_central" &
                 input$platform != "ALL" &
                 input$org != "ALL" &
                 input$sr != "ALL"
                 
        ){
          x <- data %>% 
            filter(
              (org_central == "nhl" |
                 org_central == "nhl,phl" |
                 org_central == "nhl,mam" |
                 org_central == "nhl,msf" |
                 org_central == "nhl,ahrn") &
                state_region == input$sr &
                org == input$org &
                platform == input$platform
            )
          updateSelectInput( session,"facility",
                             "Facility:",
                             choices = c("ALL",unique(x$lab_name))) } 
      
    else if (input$ti_user_name_basic == "nhl_central" &
             input$platform != "ALL" &
             input$org == "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (org_central == "nhl" |
             org_central == "nhl,phl" |
             org_central == "nhl,mam" |
             org_central == "nhl,msf" |
             org_central == "nhl,ahrn") &
            org == input$org &
            
            platform == input$platform
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "nhl_central" &
             input$platform != "ALL" &
             input$org != "ALL" &
             input$sr == "ALL"
             
    ){
      x <- data %>% 
        filter(
          (org_central == "nhl" |
             org_central == "nhl,phl" |
             org_central == "nhl,mam" |
             org_central == "nhl,msf" |
             org_central == "nhl,ahrn") &
            state_region == input$sr &
            
            platform == input$platform
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "nhl_central" &
             input$platform == "ALL" &
             input$org != "ALL" &
             input$sr == "ALL"
             
    ){
      x <- data %>% 
        filter(
          (org_central == "nhl" |
             org_central == "nhl,phl" |
             org_central == "nhl,mam" |
             org_central == "nhl,msf" |
             org_central == "nhl,ahrn") &
            org == input$org
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "nhl_central" &
             input$platform != "ALL" &
             input$org == "ALL" &
             input$sr == "ALL"
             
    ){
      x <- data %>% 
        filter(
          (org_central == "nhl" |
             org_central == "nhl,phl" |
             org_central == "nhl,mam" |
             org_central == "nhl,msf" |
             org_central == "nhl,ahrn") &
            platform == input$platform 
            
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "nhl_central" &
             input$platform == "ALL" &
             input$org != "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (org_central == "nhl" |
             org_central == "nhl,phl" |
             org_central == "nhl,mam" |
             org_central == "nhl,msf" |
             org_central == "nhl,ahrn") &
            org == input$org &
            state_region == input$sr
          
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    
    #phl_central
    
    if(input$ti_user_name_basic == "nhl_central" &
       input$platform == "ALL" &
       input$org == "ALL" &
       input$sr == "ALL"
       
    ){
      x <- data %>% 
        filter(
          org_central == "nhl" |
            org_central == "nhl,phl" |
            org_central == "nhl,mam" |
            org_central == "nhl,msf" |
            org_central == "nhl,ahrn" 
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name)))
    } else if (input$ti_user_name_basic == "nhl_central" &
               input$platform == "ALL" &
               input$org == "ALL" &
               input$sr != "ALL"
               
    ){
      x <- data %>% 
        filter(
          (org_central == "nhl" |
             org_central == "nhl,phl" |
             org_central == "nhl,mam" |
             org_central == "nhl,msf" |
             org_central == "nhl,ahrn") &
            state_region == input$sr
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name)))
    }
    else if (input$ti_user_name_basic == "nhl_central" &
             input$platform == "ALL" &
             input$org != "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (org_central == "nhl" |
             org_central == "nhl,phl" |
             org_central == "nhl,mam" |
             org_central == "nhl,msf" |
             org_central == "nhl,ahrn") &
            state_region == input$sr &
            org == input$org
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name)))
    }
    
    else if (input$ti_user_name_basic == "nhl_central" &
             input$platform != "ALL" &
             input$org != "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (org_central == "nhl" |
             org_central == "nhl,phl" |
             org_central == "nhl,mam" |
             org_central == "nhl,msf" |
             org_central == "nhl,ahrn") &
            state_region == input$sr &
            org == input$org &
            platform == input$platform
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "nhl_central" &
             input$platform != "ALL" &
             input$org == "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (org_central == "nhl" |
             org_central == "nhl,phl" |
             org_central == "nhl,mam" |
             org_central == "nhl,msf" |
             org_central == "nhl,ahrn") &
            org == input$org &
            
            platform == input$platform
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "nhl_central" &
             input$platform != "ALL" &
             input$org != "ALL" &
             input$sr == "ALL"
             
    ){
      x <- data %>% 
        filter(
          (org_central == "nhl" |
             org_central == "nhl,phl" |
             org_central == "nhl,mam" |
             org_central == "nhl,msf" |
             org_central == "nhl,ahrn") &
            state_region == input$sr &
            
            platform == input$platform
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "nhl_central" &
             input$platform == "ALL" &
             input$org != "ALL" &
             input$sr == "ALL"
             
    ){
      x <- data %>% 
        filter(
          (org_central == "nhl" |
             org_central == "nhl,phl" |
             org_central == "nhl,mam" |
             org_central == "nhl,msf" |
             org_central == "nhl,ahrn") &
            org == input$org
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "nhl_central" &
             input$platform != "ALL" &
             input$org == "ALL" &
             input$sr == "ALL"
             
    ){
      x <- data %>% 
        filter(
          (org_central == "nhl" |
             org_central == "nhl,phl" |
             org_central == "nhl,mam" |
             org_central == "nhl,msf" |
             org_central == "nhl,ahrn") &
            platform == input$platform 
          
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "nhl_central" &
             input$platform == "ALL" &
             input$org != "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (org_central == "nhl" |
             org_central == "nhl,phl" |
             org_central == "nhl,mam" |
             org_central == "nhl,msf" |
             org_central == "nhl,ahrn") &
            org == input$org &
            state_region == input$sr
          
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) }
    
    
    ## phl central
    
    else if(input$ti_user_name_basic == "phl_central" &
       input$platform == "ALL" &
       input$org == "ALL" &
       input$sr == "ALL"
       
    ){
      x <- data %>% 
        filter(
          
            org_central == "nhl,phl" 
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name)))
    } else if (input$ti_user_name_basic == "phl_central" &
               input$platform == "ALL" &
               input$org == "ALL" &
               input$sr != "ALL"
               
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,phl" ) &
            state_region == input$sr
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name)))
    }
    else if (input$ti_user_name_basic == "phl_central" &
             input$platform == "ALL" &
             input$org != "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,phl" ) &
            state_region == input$sr &
            org == input$org
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name)))
    }
    
    else if (input$ti_user_name_basic == "phl_central" &
             input$platform != "ALL" &
             input$org != "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,phl" ) &
            state_region == input$sr &
            org == input$org &
            platform == input$platform
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "phl_central" &
             input$platform != "ALL" &
             input$org == "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,phl" ) &
            org == input$org &
            
            platform == input$platform
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "phl_central" &
             input$platform != "ALL" &
             input$org != "ALL" &
             input$sr == "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,phl" ) &
            state_region == input$sr &
            
            platform == input$platform
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "phl_central" &
             input$platform == "ALL" &
             input$org != "ALL" &
             input$sr == "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,phl" ) &
            org == input$org
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "phl_central" &
             input$platform != "ALL" &
             input$org == "ALL" &
             input$sr == "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,phl" ) &
            platform == input$platform 
          
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "phl_central" &
             input$platform == "ALL" &
             input$org != "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,phl" ) &
            org == input$org &
            state_region == input$sr
          
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) }
    
    #### MAM ####
    
    
    else if(input$ti_user_name_basic == "mam_central" &
       input$platform == "ALL" &
       input$org == "ALL" &
       input$sr == "ALL"
       
    ){
      x <- data %>% 
        filter(
         
            org_central == "nhl,mam" 
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name)))
    } else if (input$ti_user_name_basic == "mam_central" &
               input$platform == "ALL" &
               input$org == "ALL" &
               input$sr != "ALL"
               
    ){
      x <- data %>% 
        filter(
          (
             
             org_central == "nhl,mam") &
            state_region == input$sr
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name)))
    }
    else if (input$ti_user_name_basic == "mam_central" &
             input$platform == "ALL" &
             input$org != "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,mam" ) &
            state_region == input$sr &
            org == input$org
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name)))
    }
    
    else if (input$ti_user_name_basic == "mam_central" &
             input$platform != "ALL" &
             input$org != "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,mam" ) &
            state_region == input$sr &
            org == input$org &
            platform == input$platform
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "mam_central" &
             input$platform != "ALL" &
             input$org == "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,mam" ) &
            org == input$org &
            
            platform == input$platform
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "mam_central" &
             input$platform != "ALL" &
             input$org != "ALL" &
             input$sr == "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,mam" ) &
            state_region == input$sr &
            
            platform == input$platform
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "mam_central" &
             input$platform == "ALL" &
             input$org != "ALL" &
             input$sr == "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,mam" ) &
            org == input$org
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "mam_central" &
             input$platform != "ALL" &
             input$org == "ALL" &
             input$sr == "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,mam" ) &
            platform == input$platform 
          
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "mam_central" &
             input$platform == "ALL" &
             input$org != "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,mam" ) &
            org == input$org &
            state_region == input$sr
          
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) }
    
    
    ### MSF ####
    else if(input$ti_user_name_basic == "msf_central" &
       input$platform == "ALL" &
       input$org == "ALL" &
       input$sr == "ALL"
       
    ){
      x <- data %>% 
        filter(
          
            org_central == "nhl,msf"  
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name)))
    } else if (input$ti_user_name_basic == "msf_central" &
               input$platform == "ALL" &
               input$org == "ALL" &
               input$sr != "ALL"
               
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,msf" ) &
            state_region == input$sr
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name)))
    }
    else if (input$ti_user_name_basic == "msf_central" &
             input$platform == "ALL" &
             input$org != "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,msf" ) &
            state_region == input$sr &
            org == input$org
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name)))
    }
    
    else if (input$ti_user_name_basic == "msf_central" &
             input$platform != "ALL" &
             input$org != "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,msf" ) &
            state_region == input$sr &
            org == input$org &
            platform == input$platform
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "msf_central" &
             input$platform != "ALL" &
             input$org == "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,msf" ) &
            org == input$org &
            
            platform == input$platform
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "msf_central" &
             input$platform != "ALL" &
             input$org != "ALL" &
             input$sr == "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,msf" ) &
            state_region == input$sr &
            
            platform == input$platform
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "msf_central" &
             input$platform == "ALL" &
             input$org != "ALL" &
             input$sr == "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,msf" ) &
            org == input$org
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "msf_central" &
             input$platform != "ALL" &
             input$org == "ALL" &
             input$sr == "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,msf" ) &
            platform == input$platform 
          
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "msf_central" &
             input$platform == "ALL" &
             input$org != "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,msf" ) &
            org == input$org &
            state_region == input$sr
          
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) }
    
    ### ahrn ####
    
   else if(input$ti_user_name_basic == "ahrn_central" &
       input$platform == "ALL" &
       input$org == "ALL" &
       input$sr == "ALL"
       
    ){
      x <- data %>% 
        filter(
         
            org_central == "nhl,ahrn" 
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name)))
    } else if (input$ti_user_name_basic == "ahrn_central" &
               input$platform == "ALL" &
               input$org == "ALL" &
               input$sr != "ALL"
               
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,ahrn") &
            state_region == input$sr
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name)))
    }
    else if (input$ti_user_name_basic == "ahrn_central" &
             input$platform == "ALL" &
             input$org != "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,ahrn") &
            state_region == input$sr &
            org == input$org
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name)))
    }
    
    else if (input$ti_user_name_basic == "ahrn_central" &
             input$platform != "ALL" &
             input$org != "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,ahrn") &
            state_region == input$sr &
            org == input$org &
            platform == input$platform
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "ahrn_central" &
             input$platform != "ALL" &
             input$org == "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,ahrn") &
            org == input$org &
            
            platform == input$platform
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "ahrn_central" &
             input$platform != "ALL" &
             input$org != "ALL" &
             input$sr == "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,ahrn") &
            state_region == input$sr &
            
            platform == input$platform
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "ahrn_central" &
             input$platform == "ALL" &
             input$org != "ALL" &
             input$sr == "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,ahrn") &
            org == input$org
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "ahrn_central" &
             input$platform != "ALL" &
             input$org == "ALL" &
             input$sr == "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,ahrn") &
            platform == input$platform 
          
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) } 
    
    else if (input$ti_user_name_basic == "ahrn_central" &
             input$platform == "ALL" &
             input$org != "ALL" &
             input$sr != "ALL"
             
    ){
      x <- data %>% 
        filter(
          (
             org_central == "nhl,ahrn") &
            org == input$org &
            state_region == input$sr
          
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name))) }
    
    
    else if (input$ti_user_name_basic != "" & input$ti_password_basic != "") {
      x <- data %>% 
        filter(
          org == account_filtered()$org &
          state_region == account_filtered()$sr
        )
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL",unique(x$lab_name)))
    } else {
      updateSelectInput( session,"facility",
                         "Facility:",
                         choices = c("ALL"))
    }
    
  })  
  




#----------Data Filter------------


  df_final <- reactive({
    if (input$platform == "ALL" & (input$org) == "ALL" & (input$sr) == "ALL" & (input$facility) == "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data1()}
    else if (input$platform == "ALL" & (input$org) != "ALL" & (input$sr) == "ALL" & (input$facility) == "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data1() %>%
        filter(org == input$org)}
    else if (input$platform == "ALL" & (input$org) != "ALL" & (input$sr) != "ALL" & (input$facility) == "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data1() %>%
        filter(org == input$org) %>%
        filter(state_region == input$sr) }
    else if (input$platform == "ALL" & (input$org) != "ALL" & (input$sr) != "ALL" & (input$facility) != "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data1() %>%
        filter(org == input$org) %>%
        filter(state_region == input$sr) %>%
        filter(lab_name == input$facility)}
    else if (input$platform == "high_throughput" & (input$org) == "ALL" & (input$sr) == "ALL" & (input$facility) == "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data1() %>%
        filter(platform == "high_throughput")}
    else if (input$platform == "high_throughput" & (input$org) != "ALL" & (input$sr) == "ALL" & (input$facility) == "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data1() %>%
        filter(platform == "high_throughput") %>%
        filter(org == input$org)}
    else if (input$platform == "high_throughput" & (input$org) != "ALL" & (input$sr) != "ALL" & (input$facility) == "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data1() %>%
        filter(platform == "high_throughput") %>%
        filter(org == input$org) %>%
        filter(state_region == input$sr) }
    else if (input$platform == "high_throughput" & (input$org) != "ALL" & (input$sr) != "ALL" & (input$facility) != "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data1() %>%
        filter(platform == "high_throughput")%>%
        filter(org == input$org) %>%
        filter(state_region == input$sr) %>%
        filter(lab_name == input$facility)}
    else if (input$platform == "genexpert" & (input$org) == "ALL" & (input$sr) == "ALL" & (input$facility) == "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data1() %>%
        filter(platform == "genexpert")}
    else if (input$platform == "genexpert" & (input$org) != "ALL" & (input$sr) == "ALL" & (input$facility) == "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data1() %>%
        filter(platform == "genexpert") %>%
        filter(org == input$org)}
    else if (input$platform == "genexpert" & (input$org) != "ALL" & (input$sr) != "ALL" & (input$facility) == "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data1() %>%
        filter(platform == "genexpert") %>%
        filter(org == input$org) %>%
        filter(state_region == input$sr) }
    else if (input$platform == "genexpert" & (input$org) != "ALL" & (input$sr) != "ALL" & (input$facility) != "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data1() %>%
        filter(platform == "genexpert")%>%
        filter(org == input$org) %>%
        filter(state_region == input$sr) %>%
        filter(lab_name == input$facility)}

  })


  
  df_final_2 <- reactive({
    if (input$platform == "ALL" & (input$org) == "ALL" & (input$sr) == "ALL" & (input$facility) == "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data2()} 
    else if (input$platform == "ALL" & (input$org) != "ALL" & (input$sr) == "ALL" & (input$facility) == "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data2() %>%
        filter(org == input$org)} 
    else if (input$platform == "ALL" & (input$org) != "ALL" & (input$sr) != "ALL" & (input$facility) == "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data2() %>%
        filter(org == input$org) %>%
        filter(state_region == input$sr) }
    else if (input$platform == "ALL" & (input$org) != "ALL" & (input$sr) != "ALL" & (input$facility) != "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data2() %>%
        filter(org == input$org) %>%
        filter(state_region == input$sr) %>%
        filter(lab_name == input$facility)}
    else if (input$platform == "high_throughput" & (input$org) == "ALL" & (input$sr) == "ALL" & (input$facility) == "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data2() %>%
        filter(platform == "high_throughput")} 
    else if (input$platform == "high_throughput" & (input$org) != "ALL" & (input$sr) == "ALL" & (input$facility) == "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data2() %>%
        filter(platform == "high_throughput") %>%
        filter(org == input$org)} 
    else if (input$platform == "high_throughput" & (input$org) != "ALL" & (input$sr) != "ALL" & (input$facility) == "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data2() %>%
        filter(platform == "high_throughput") %>%
        filter(org == input$org) %>%
        filter(state_region == input$sr) }
    else if (input$platform == "high_throughput" & (input$org) != "ALL" & (input$sr) != "ALL" & (input$facility) != "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data2() %>%
        filter(platform == "high_throughput")%>%
        filter(org == input$org) %>%
        filter(state_region == input$sr) %>%
        filter(lab_name == input$facility)}
    else if (input$platform == "genexpert" & (input$org) == "ALL" & (input$sr) == "ALL" & (input$facility) == "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data2() %>%
        filter(platform == "genexpert")} 
    else if (input$platform == "genexpert" & (input$org) != "ALL" & (input$sr) == "ALL" & (input$facility) == "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data2() %>%
        filter(platform == "genexpert") %>%
        filter(org == input$org)} 
    else if (input$platform == "genexpert" & (input$org) != "ALL" & (input$sr) != "ALL" & (input$facility) == "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data2() %>%
        filter(platform == "genexpert") %>%
        filter(org == input$org) %>%
        filter(state_region == input$sr) }
    else if (input$platform == "genexpert" & (input$org) != "ALL" & (input$sr) != "ALL" & (input$facility) != "ALL" & (!is.null(input$startdate)) & (!is.null(input$enddate))) {
      data2() %>%
        filter(platform == "genexpert")%>%
        filter(org == input$org) %>%
        filter(state_region == input$sr) %>%
        filter(lab_name == input$facility)}
    
  })
    

 
  
  
  
#-----plot of visits by year----
  
  output$visits_by_year <- renderPlot({
    
    
    ggplot(df_final(), aes(x = year , fill = year))+
      geom_bar(fill = "#6CB4EE")+
      labs(y = "Frequency of assessments", x = "Year")+
      geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black")+
      theme(axis.text.x = element_text(size = 12,colour = 'blue') )+
      
      theme_classic()+
      guides(fill = "none")
    
    
    
  })
  
#-----Plot of visits by SR-------  
  output$visits_by_sr <- renderPlot({

    ggplot(df_final(), aes(x= fct_rev(fct_infreq(state_region)) , fill=state_region))+
      geom_bar(fill = "#335781" )+
      labs(y = "Frequency of assessments", x = "States/Regions")+
      geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black")+
      theme_classic()+
      theme(axis.text.x = element_text(size = 10, angle = 90, colour = 'black') )+
      theme(axis.title.x=element_blank())+
      guides(fill = "none") 
  
  })

#-----plot of visits by org----  
  output$visits_by_org_pie <- renderPlot({
    
    
    df_final <- df_final() %>%
      group_by(org)%>%
      summarise(value = n())%>%
      mutate(csum = rev(cumsum(rev(value))), 
             pos = value/2 + lead(csum, 1),
             pos = if_else(is.na(pos), value/2, pos),
             percentage = paste0(round((value/sum(value)*100),digits = 1),"%"),
             total_tests = sum(value))
    
    
    
    ggplot(df_final, aes(x="", y=value, fill=org)) + 
      geom_col(width = 1, color = 1) +
      geom_label_repel(aes(y = pos,
                           label = glue::glue("{org} {value} ({percentage})"), 
                           fill = org),
                       size = 5,
                       nudge_x = 1, force = 20, nudge_y = 0, color = 'white',
                       show.legend = FALSE) +
      ggtitle(paste0("Total Number of Assessemnts:", df_final$total_tests))+
      labs(  fill = "org" ) +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Dark2")+
      guides(fill="none")+
      theme_void() +
      theme(plot.title= element_text(size=16,
                                     color="#26355D",
                                     face="bold",
                                     family = "Tahoma",
                                     hjust = 0.5))
    
    
    
    
  })
 
#-----plot of visits by year-month and platform---- 
  
  output$visits_by_year_month <- renderPlot({
    
    df_final <- df_final() %>%
      mutate(platform = ifelse( platform == "genexpert",
                                "GeneXpert",
                                "High Throughput")) %>%
      mutate(platform = factor(platform, levels = c("High Throughput", "GeneXpert"))) %>%
      group_by(year_month,platform)%>% #, sub_platform
      summarise(value = n())
    
    ggplot(df_final, aes(x = year_month, y = value, fill = platform ))+ #fill = sub_platform
      geom_col(width = 0.5)+
      facet_wrap(~platform, strip.position = "bottom")+
      theme_minimal()+
      theme(strip.placement = "outside",
           panel.spacing = unit(2,"cm"),
           legend.position = "bottom",
           legend.text = element_text(size = 14)) +  # Adjust legend text size here
            labs(fill = "", x = "") +
      theme(
        strip.text = element_text(colour = "white",
                                  size = 12,
                                  face = "bold"),
        strip.background = element_rect(
          fill = "#335781", 
          color = "grey80", 
          linewidth = 1
        )
      ) +
      geom_text(
        aes(label = value),
        position = "stack",
        size = 7,
        face = "bold",
        vjust = 0
      ) +
      theme(axis.text.x = element_text(size = 12, colour = 'black', face="bold") )
      
    
  })
  
#-----table of visits and assessement scores--------
  
  output$visit_table <- DT::renderDT({
    
    df_final <- df_final() %>%
     dplyr::select(assessment_date,
            org,
            lab_name,
            # sub_platform,
            TOTAL_SCORE,
            max_score,
            SCORE_PERCENTAGE
            # Level
            )%>%
      rename(Assessment_Date = assessment_date,
             Org = org,
             Lab_Name = lab_name,
             Total_Score = TOTAL_SCORE,
             Max_Score = max_score,
             Score_Percentage = SCORE_PERCENTAGE)  %>%
      mutate(Level = ifelse(Score_Percentage < 55, "Level0", 
                            ifelse(Score_Percentage < 64, "Level1",
                                   ifelse(Score_Percentage < 74, "Level2",
                                          ifelse(Score_Percentage < 84, "Level3",
                                                 ifelse(Score_Percentage < 94, "Level4", "Level5"))))))
    
    output$dl_visit_table <- downloadHandler(
      filename = function() {
        paste("data-sites-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(df_final, file)
      }
    )
    
    
    DT::datatable(df_final,extensions = c('Buttons','FixedColumns'),
                  options = list(lengthChange = TRUE, fixedColumns = list(leftColumns = 2),
                                 scrollX = TRUE, scrollY = "250px", pageLength = 4), 
                  style = "bootstrap5",
                  caption = "List of Sites and their assessment scores")
    
    
})
  
  
#----overall scores for each section----
  
##----- Description of scores------
  
  output$description <- renderUI({
    tags$table(
      style = "width:100%; ",
      border = "1",
      
      tags$thead(
      
        tags$tr(
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "VL/IVT Level"),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "% SCORE"),
          tags$th( style = "text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "DESCRIPTION OF RESULTS")
          )
        
      ), # end of tags$thead
      
      tags$tbody(
        tags$tr( style = "border-top: thin solid;  text-align: center;",
                 tags$td( style = "background-color:red;",
                   "Level 0"),
                 tags$td( "<55%"),
                 tags$td("Needs improvement in all areas and immediate remediation")
        ),
     
          tags$tr( style = "border-top: thin solid;  text-align: center;",
                   tags$td( style = "background-color:#f09648;",
                     "Level 1"),
                   tags$td("55%-64%"),
                   tags$td("Needs improvement in specific areas ")
          ),
        
        tags$tr( style = "border-top: thin solid;  text-align: center;",
                 tags$td( style = "background-color:#FFD580;",
                   "Level 2"),
                 tags$td("65%-74%"),
                 tags$td(" ")
        ),
        
        tags$tr( style = "border-top: thin solid;  text-align: center;",
                 tags$td( style = "background-color:yellow;",
                   "Level 3"),
                 tags$td("75%-84%"),
                 tags$td(" ")
        ),
        
        tags$tr( style = "border-top: thin solid;  text-align: center; ",
                 tags$td( style = "background-color:green;color:white;",
                   "Level 4"),
                 tags$td("85%-94%"),
                 tags$td(" ")
        ),
        
        tags$tr( style = "border-top: thin solid;  text-align: center; ",
                 tags$td( style = "background-color:darkgreen; color:white;",
                   "Level 5"),
                 tags$td(">= 95%"),
                 tags$td(" ")
        )
        
      ) # end of tbody
      
    )# end of table
  })
    
 
  
##-----genexpert----
  output$overall_scores_g <- renderPlot({
    
    df_final <- df_final()%>%  
      filter(platform == "genexpert") %>%
      summarise( S1_Personal = round(((sum(sec1_total)/sum(sec1_max))*100), digits = 0),
                 S2_Physical = round(((sum(sec2_total)/sum(sec2_max))*100), digits = 0),
                 S3_Safety = round(((sum(sec3_total)/sum(sec3_max))*100), digits = 0),
                 S4_Inventory = round(((sum(sec4_total)/sum(sec4_max))*100), digits = 0),
                 S5_Sample_Mx = round(((sum(sec5_total)/sum(sec5_max))*100), digits = 0),
                 S6_Equipment = round(((sum(sec6_total)/sum(sec6_max))*100), digits = 0),
                 S7_Process_Control = round(((sum(sec7_total)/sum(sec7_max))*100), digits = 0),
                 S8_MnE = round(((sum(sec8_total)/sum(sec8_max))*100), digits = 0),
                 S9_Quality = round(((sum(sec9_total)/sum(sec9_max))*100), digits = 0)
                 
      )%>%
      melt(.)
    
    
    
    cols    <- ifelse(df_final$value<55, "red", 
                      ifelse(df_final$value<64, "#f09648",
                             ifelse(df_final$value<74, "#FFD580",  
                                    ifelse(df_final$value<84, "yellow",
                                    ifelse(df_final$value<94, "green",  "darkgreen")))))
    
    ggplot(df_final, aes(x = variable, y = value))+
      geom_bar(stat = 'identity',fill = cols)+
      labs(
        y = "Percentage", x = "Section Name")+
      geom_text(aes(label = paste0(value, "%")), vjust = -1)+
      ylim(0,100)+ # scale_x_discrete(labels = function(x) 
      #   stringr::str_wrap(x, width = 5))+
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 5))+ 
      theme_classic()+
      theme(axis.text.x = element_text(size = 10, angle = 90, colour = 'black') )
      
    
    
  })  
  
  
##----highthroughput---- 
  output$overall_scores_h <- renderPlot({
    
    df_final <- df_final()%>%  
      filter(platform == "high_throughput") %>%
      summarise( S1_Personal = round(((sum(sec1_total)/sum(sec1_max))*100), digits = 0),
                 S2_Physical = round(((sum(sec2_total)/sum(sec2_max))*100), digits = 0),
                 S3_Safety = round(((sum(sec3_total)/sum(sec3_max))*100), digits = 0),
                 S4_Inventory = round(((sum(sec4_total)/sum(sec4_max))*100), digits = 0),
                 S5_Sample_Mx = round(((sum(sec5_total)/sum(sec5_max))*100), digits = 0),
                 S6_Equipment = round(((sum(sec6_total)/sum(sec6_max))*100), digits = 0),
                 S7_Process_Control = round(((sum(sec7_total)/sum(sec7_max))*100), digits = 0),
                 S8_MnE = round(((sum(sec8_total)/sum(sec8_max))*100), digits = 0),
                 S9_Quality = round(((sum(sec9_total)/sum(sec9_max))*100), digits = 0)
                 
      )%>%
      melt(.)
    
    
    
    
    cols    <- ifelse(df_final$value<55, "red", 
                      ifelse(df_final$value<64, "#f09648",
                             ifelse(df_final$value<74, "#FFD580",  
                                    ifelse(df_final$value<84, "yellow",
                                           ifelse(df_final$value<94, "green",  "darkgreen")))))
    
    ggplot(df_final, aes(x = variable, y = value))+
      geom_bar(stat = 'identity',fill = cols)+
      labs(
        y = "Percentage", x = "Section Name")+
      geom_text(aes(label = paste0(value, "%")), vjust = -1)+
      ylim(0,100)+      #scale_x_discrete(labels = function(x) 
      #  stringr::str_wrap(x, width = 5))+
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 5))+
      theme_classic()+
      theme(axis.text.x = element_text(size = 10, angle = 90, colour = 'black') )
      
    
    
  })  
  
  
  
#-----Radar Chart of scores-------
##--genexpert-----
  
  output$radar_scores_g <- renderPlot({
    
    max_min <- data.frame(
      year_month = c("max","min"),
      n = c(100,0),
      S1_Personal = c(100, 0), 
      S2_Physical = c(100, 0),
      S3_Safety = c(100, 0),
      S4_Inventory = c(100, 0),
      S5_Sample_Mx = c(100, 0),
      S6_Equipment = c(100, 0),
      S7_Process_Control = c(100, 0),
      S8_MnE = c(100,0),
      S9_Quality = c(100,0)
    )
    rownames(max_min) <- c("max", "min")
    
    
    df_final <- df_final() %>%  
      filter(platform == "genexpert") %>%
      group_by(year_month) %>%
      summarise(  n = n(),
                  S1_Personal = round(((sum(sec1_total)/sum(sec1_max))*100), digits = 0),
                  S2_Physical = round(((sum(sec2_total)/sum(sec2_max))*100), digits = 0),
                  S3_Safety = round(((sum(sec3_total)/sum(sec3_max))*100), digits = 0),
                  S4_Inventory = round(((sum(sec4_total)/sum(sec4_max))*100), digits = 0),
                  S5_Sample_Mx = round(((sum(sec5_total)/sum(sec5_max))*100), digits = 0),
                  S6_Equipment = round(((sum(sec6_total)/sum(sec6_max))*100), digits = 0),
                  S7_Process_Control = round(((sum(sec7_total)/sum(sec7_max))*100), digits = 0),
                  S8_MnE = round(((sum(sec8_total)/sum(sec8_max))*100), digits = 0),
                  S9_Quality = round(((sum(sec9_total)/sum(sec9_max))*100), digits = 0)
      )
    
    rownames(df_final) <- df_final$year_month
    df_final <- rbind(max_min, df_final)
    df_final <- df_final %>% dplyr::select(3:11)
    
    p <- radarchart(df_final,
                    axistype=1 , 
                    plty = 1,
                    calcex = 0.9, 
                    seg = 5,
                    vlcex = 0.8,
                    caxislabels=NULL,
                    axislabcol='blue',
                    plwd = 2
    )
    
    legend('bottomleft',
           legend = rownames(df_final[-c(1,2),]),
           title = "visit date \n  [Year Month]",
           bty = "n", pch = 20, col = 1:8,
           text.col = "grey25", pt.cex = 1.5, cex = 0.7)
    
    ggplotly(p)
  })
  
  ##--highthroughput-----  
 
  output$radar_scores_h <- renderPlot({
    
    max_min <- data.frame(
      year_month = c("max","min"),
      n = c(100,0),
      S1_Personal = c(100, 0), 
      S2_Physical = c(100, 0),
      S3_Safety = c(100, 0),
      S4_Inventory = c(100, 0),
      S5_Sample_Mx = c(100, 0),
      S6_Equipment = c(100, 0),
      S7_Process_Control = c(100, 0),
      S8_MnE = c(100,0),
      S9_Quality = c(100,0)
    )
    rownames(max_min) <- c("max", "min")
    
    
    df_final <- df_final() %>%  
      filter(platform == "high_throughput") %>%
      group_by(year_month) %>%
      summarise(  n = n(),
                  S1_Personal = round(((sum(sec1_total)/sum(sec1_max))*100), digits = 0),
                  S2_Physical = round(((sum(sec2_total)/sum(sec2_max))*100), digits = 0),
                  S3_Safety = round(((sum(sec3_total)/sum(sec3_max))*100), digits = 0),
                  S4_Inventory = round(((sum(sec4_total)/sum(sec4_max))*100), digits = 0),
                  S5_Sample_Mx = round(((sum(sec5_total)/sum(sec5_max))*100), digits = 0),
                  S6_Equipment = round(((sum(sec6_total)/sum(sec6_max))*100), digits = 0),
                  S7_Process_Control = round(((sum(sec7_total)/sum(sec7_max))*100), digits = 0),
                  S8_MnE = round(((sum(sec8_total)/sum(sec8_max))*100), digits = 0),
                  S9_Quality = round(((sum(sec9_total)/sum(sec9_max))*100), digits = 0)
      )
    
    rownames(df_final) <- df_final$year_month
    df_final <- rbind(max_min, df_final)
    df_final <- df_final %>% dplyr::select(3:11)
    
    p <- radarchart(df_final,
                    axistype=1 , 
                    plty = 1,
                    calcex = 0.9, 
                    seg = 5,
                    vlcex = 0.8,
                    caxislabels=NULL,
                    axislabcol='blue',
                    plwd = 2
    )
    
    legend('bottomleft',
           legend = rownames(df_final[-c(1,2),]),
           title = "visit date \n  [Year Month]",
           bty = "n", pch = 20, col = 1:8,
           text.col = "grey25", pt.cex = 1.5, cex = 0.7)
    
    ggplotly(p)
  })
  
  
#-----Table of overall scores-----


  output$overall_scores_table = DT::renderDT({

    df_final <- df_final()%>%   group_by(assessment_date,lab_name) %>% #,sub_platform
      summarise( n = n(),
                 S1_Personal = round(((sum(sec1_total)/sum(sec1_max))*100), digits = 0),
                 S2_Physical = round(((sum(sec2_total)/sum(sec2_max))*100), digits = 0),
                 S3_Safety = round(((sum(sec3_total)/sum(sec3_max))*100), digits = 0),
                 S4_Inventory = round(((sum(sec4_total)/sum(sec4_max))*100), digits = 0),
                 S5_Sample_Mx = round(((sum(sec5_total)/sum(sec5_max))*100), digits = 0),
                 S6_Equipment = round(((sum(sec6_total)/sum(sec6_max))*100), digits = 0),
                 S7_Process_Control = round(((sum(sec7_total)/sum(sec7_max))*100), digits = 0),
                 S8_MnE = round(((sum(sec8_total)/sum(sec8_max))*100), digits = 0),
                 S9_Quality = round(((sum(sec9_total)/sum(sec9_max))*100), digits = 0),
                 final_overall_score = SCORE_PERCENTAGE,
                 Level = ifelse(SCORE_PERCENTAGE<55, "Level0", 
                                ifelse(SCORE_PERCENTAGE<64, "Level1",
                                       ifelse(SCORE_PERCENTAGE<74, "Level2",
                                              ifelse(SCORE_PERCENTAGE<84, "Level3",
                                                     ifelse(SCORE_PERCENTAGE<94, "Level4",  "Level5")))))
      )%>%
      melt(.,measure.vars = c("S1_Personal","S2_Physical",'S3_Safety','S4_Inventory',
                              'S5_Sample_Mx','S6_Equipment','S7_Process_Control',
                              "S8_MnE", "S9_Quality", "final_overall_score","Level"), variable.name = "section")%>%
      pivot_wider(names_from = section, values_from = value)

    df_final <- df_final[, -which(names(df_final) == "n")]
    
    
    output$dl_overall_scores_table <- downloadHandler(
      filename = function() {
        paste("scores-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(df_final, file)
      }
    )

    DT::datatable(df_final,extensions = c('Buttons','FixedColumns'),
                  options = list(lengthChange = TRUE, fixedColumns = list(leftColumns = 2),
                                 scrollX = TRUE, scrollY = "300px", pageLength = 7),

                  caption = "Section Scores by Periods ") %>%
      formatStyle(
        columns = c(3:13),
        backgroundColor = styleInterval(c(0, 55, 64,74, 84, 94, 95, 100 ), c('red','red','#FFAC1C',"#FFD580", 'yellow', 'green','green', 'darkgreen', 'darkgreen')),
        fontWeight = 'bold')


  })

  #-------Section 1 score ------
  
  output$sec1_scores_g <- renderPlot({
    
    df_final <- df_final()%>% 
      filter(platform == "genexpert") %>%
      group_by(lab_name,assessment_date,year_month)%>% #,sub_platform
      summarise( S1_Personal = round(((sum(sec1_total)/sum(sec1_max))*100), digits = 0))%>%
      mutate(cols = case_when(
        (S1_Personal >= 95 & S1_Personal <= 100) ~ "#357435",
        (S1_Personal >= 85 & S1_Personal < 95)  ~ "#90dc67",
        (S1_Personal >=75 & S1_Personal < 85) ~ "#FFFF00",
        (S1_Personal >=65 & S1_Personal < 75) ~ "#FFD580",
        (S1_Personal >=55 & S1_Personal < 65) ~ "#FFAC1C",
        (S1_Personal >0 & S1_Personal < 55) ~ "#ff2701",
        TRUE ~ "#808080"  #anything that does not meet the criteria above
      ))
    
    
    ggplot(df_final, aes(x = year_month, y = S1_Personal))+
      geom_bar(stat = 'identity', width = 0.3, aes(fill = cols))+
      scale_fill_identity()+
      labs(
        y = "Percentage", x = "Facility Name & Period")+
      geom_text(aes(label = paste0(S1_Personal, "%")), vjust = -1, size = 3, fontface = 'bold')+
      ylim(0,140)+
      facet_wrap( ~ lab_name, strip.position = "bottom", scales = "free_x", ncol = 4, nrow = 10)+
      theme_classic()+
      theme(axis.text = element_text(size = 9))+
      theme(strip.text.x = element_text(size = 9, colour = "blue"))
    
    
    
  })
  
  
  
  output$sec1_scores_h <- renderPlot({
    
    df_final <- df_final()%>% 
      filter(platform == "high_throughput") %>%
      group_by(lab_name,assessment_date,year_month)%>% #,sub_platform
      summarise( S1_Personal = round(((sum(sec1_total)/sum(sec1_max))*100), digits = 0))%>%
      mutate(cols = case_when(
        (S1_Personal >= 95 & S1_Personal <= 100) ~ "#357435",
        (S1_Personal >= 85 & S1_Personal < 95)  ~ "#90dc67",
        (S1_Personal >=75 & S1_Personal < 85) ~ "#FFFF00",
        (S1_Personal >=65 & S1_Personal < 75) ~ "#FFD580",
        (S1_Personal >=55 & S1_Personal < 65) ~ "#FFAC1C",
        (S1_Personal >0 & S1_Personal < 55) ~ "#ff2701",
        TRUE ~ "#808080"  #anything that does not meet the criteria above
      ))
    
    
    ggplot(df_final, aes(x = year_month, y = S1_Personal))+
      geom_bar(stat = 'identity', width = 0.3, aes(fill = cols))+
      scale_fill_identity()+
      labs(
        y = "Percentage", x = "Facility Name & Period")+
      geom_text(aes(label = paste0(S1_Personal, "%")), vjust = -1, size = 3, fontface = 'bold')+
      ylim(0,140)+
      facet_wrap( ~ lab_name, strip.position = "bottom", scales = "free_x", ncol = 3, nrow = 10)+
      theme_classic()+
      theme(axis.text = element_text(size = 9))+
      theme(strip.text.x = element_text(size = 9, colour = "blue"))
  })


  
    
  #---- Sec1 Detail Table-----  
  
  
  
  output$sec1_table = DT::renderDT({
    df_final <- df_final()%>%  group_by(assessment_date, lab_name) %>% #,sub_platform
      summarise( 
        Q1 = sum(Q1_1),
        Q2 = sum(Q1_2),
        Q3 = sum(Q1_3),
        Q4 = sum(Q1_4),
        Q5 = sum(Q1_5),
        Q6 = sum(Q1_6),
        Q7 = sum(Q1_7),
        Q8 = sum(Q1_8),
        Q9 = sum(Q1_9),
        Q10 = sum(Q1_10),
        Q11 = sum(Q1_11),
        Audit_Score = sum(sec1_total),
        Max_Score = sum(sec1_max),
        Score_Percent = round(((sum(sec1_total)/sum(sec1_max))*100), digits = 0)
        
      ) %>%
      rename("Laboratory_Name" = lab_name)%>%
      rename("Date" = assessment_date)
    

    
    output$dl_sec1_table <- downloadHandler(
      filename = function() {
        paste("data-sec1-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(df_final, file)
      }
    )   
    
    
    DT::datatable(df_final,
                  
                  extensions = c('Buttons','FixedColumns'),
                  options = list(lengthChange = TRUE, fixedColumns = list(leftColumns = 3),
                                 scrollX = TRUE, scrollY = "300px",pageLength = 5), 
                  caption = "[1 = Yes, 0.5 = Partial, 0 = No] ") %>%
      formatStyle(
        columns = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11"),
        backgroundColor = styleInterval(c(0, 0.5, 1), c('red','yellow', 'green','grey')),
        fontWeight = 'bold')
    
    
    
  })
  
  
  
  #-------Section 2 score ------
  
  output$sec2_scores_g <- renderPlot({
    
    df_final <- df_final()%>% 
      filter(platform == "genexpert") %>%
      group_by(lab_name,assessment_date,year_month)%>% #,sub_platform
      summarise( S2_Physical = round(((sum(sec2_total)/sum(sec2_max))*100), digits = 0))%>%
      mutate(cols = case_when(
        (S2_Physical >= 95 & S2_Physical <= 100) ~ "#357435",
        (S2_Physical >= 85 & S2_Physical < 95)  ~ "#90dc67",
        (S2_Physical >=75 & S2_Physical < 85) ~ "#FFFF00",
        (S2_Physical >=65 & S2_Physical < 75) ~ "#FFD580",
        (S2_Physical >=55 & S2_Physical < 65) ~ "#FFAC1C",
        (S2_Physical >0 & S2_Physical < 55) ~ "#ff2701",
        TRUE ~ "#808080"  #anything that does not meet the criteria above
      ))
    
    
    ggplot(df_final, aes(x = year_month, y = S2_Physical))+
      geom_bar(stat = 'identity', width = 0.3, aes(fill = cols))+
      scale_fill_identity()+
      labs(
        y = "Percentage", x = "Facility Name & Period")+
      geom_text(aes(label = paste0(S2_Physical, "%")), vjust = -1, size = 3, fontface = 'bold')+
      ylim(0,140)+
      facet_wrap( ~ lab_name, strip.position = "bottom", scales = "free_x", ncol = 4, nrow = 10)+
      theme_classic()+
      theme(axis.text = element_text(size = 9))+
      theme(strip.text.x = element_text(size = 9, colour = "blue"))
    
    
    
  })
  
  
  
  output$sec2_scores_h <- renderPlot({
    
    df_final <- df_final()%>% 
      filter(platform == "high_throughput") %>%
      group_by(lab_name,assessment_date,year_month)%>% #,sub_platform
      summarise( S2_Physical = round(((sum(sec2_total)/sum(sec2_max))*100), digits = 0))%>%
      mutate(cols = case_when(
        (S2_Physical >= 95 & S2_Physical <= 100) ~ "#357435",
        (S2_Physical >= 85 & S2_Physical < 95)  ~ "#90dc67",
        (S2_Physical >=75 & S2_Physical < 85) ~ "#FFFF00",
        (S2_Physical >=65 & S2_Physical < 75) ~ "#FFD580",
        (S2_Physical >=55 & S2_Physical < 65) ~ "#FFAC1C",
        (S2_Physical >0 & S2_Physical < 55) ~ "#ff2701",
        TRUE ~ "#808080"  #anything that does not meet the criteria above
      ))
    
    
    ggplot(df_final, aes(x = year_month, y = S2_Physical))+
      geom_bar(stat = 'identity', width = 0.3, aes(fill = cols))+
      scale_fill_identity()+
      labs(
        y = "Percentage", x = "Facility Name & Period")+
      geom_text(aes(label = paste0(S2_Physical, "%")), vjust = -1, size = 3, fontface = 'bold')+
      ylim(0,140)+
      facet_wrap( ~ lab_name, strip.position = "bottom", scales = "free_x", ncol = 3, nrow = 10)+
      theme_classic()+
      theme(axis.text = element_text(size = 9))+
      theme(strip.text.x = element_text(size = 9, colour = "blue"))
  })
  
  
  
  
  #---- Sec2 Detail Table-----  
  
  
  
  output$sec2_table = DT::renderDT({
    df_final <- df_final()%>%  group_by(assessment_date, lab_name) %>% #,sub_platform
      summarise( 
        Q1 = sum(Q2_1),
        Q2 = sum(Q2_2),
        Q3 = sum(Q2_3),
        Q4 = sum(Q2_4),
        Q5 = sum(Q2_5),
        Q6 = sum(Q2_6),
        Q7 = sum(Q2_7),
        Q8 = sum(Q2_8),
        Q9 = sum(Q2_9),
        Q10 = sum(Q2_10),
        Q11 = sum(Q2_11),
        Q12_G = sum(Q2_12_G),
        Q12_H = sum(Q2_12_H),
        Q13 = sum(Q2_13),
        Q14 = sum(Q2_14),
        Audit_Score = sum(sec2_total),
        Max_Score = sum(sec2_max),
        Score_Percent = round(((sum(sec2_total)/sum(sec2_max))*100), digits = 0)
        
      ) %>%
      rename("Laboratory_Name" = lab_name)%>%
      rename("Date" = assessment_date)
    
    
    
    output$dl_sec2_table <- downloadHandler(
      filename = function() {
        paste("data-sec2-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(df_final, file)
      }
    )   
    
    
    DT::datatable(df_final,
                  
                  extensions = c('Buttons','FixedColumns'),
                  options = list(lengthChange = TRUE, fixedColumns = list(leftColumns = 3),
                                 scrollX = TRUE, scrollY = "300px",pageLength = 5), 
                  caption = "[1 = Yes, 0.5 = Partial, 0 = No] ") %>%
      formatStyle(
        columns = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12_G", "Q12_H", "Q13", "Q14" ),
        backgroundColor = styleInterval(c(0, 0.5, 1), c('red','yellow', 'green','grey')),
        fontWeight = 'bold')
    
    
    
  })
  
  
  #-------Section 3 score ------
  
  output$sec3_scores_g <- renderPlot({
    
    df_final <- df_final()%>% 
      filter(platform == "genexpert") %>%
      group_by(lab_name,assessment_date,year_month)%>% #,sub_platform
      summarise( S3_Safety = round(((sum(sec3_total)/sum(sec3_max))*100), digits = 0))%>%
      mutate(cols = case_when(
        (S3_Safety >= 95 & S3_Safety <= 100) ~ "#357435",
        (S3_Safety >= 85 & S3_Safety < 95)  ~ "#90dc67",
        (S3_Safety >=75 & S3_Safety < 85) ~ "#FFFF00",
        (S3_Safety >=65 & S3_Safety < 75) ~ "#FFD580",
        (S3_Safety >=55 & S3_Safety < 65) ~ "#FFAC1C",
        (S3_Safety >0 & S3_Safety < 55) ~ "#ff2701",
        TRUE ~ "#808080"  #anything that does not meet the criteria above
      ))
    
    
    ggplot(df_final, aes(x = year_month, y = S3_Safety))+
      geom_bar(stat = 'identity', width = 0.3, aes(fill = cols))+
      scale_fill_identity()+
      labs(
        y = "Percentage", x = "Facility Name & Period")+
      geom_text(aes(label = paste0(S3_Safety, "%")), vjust = -1, size = 3, fontface = 'bold')+
      ylim(0,140)+
      facet_wrap( ~ lab_name, strip.position = "bottom", scales = "free_x", ncol = 4, nrow = 10)+
      theme_classic()+
      theme(axis.text = element_text(size = 9))+
      theme(strip.text.x = element_text(size = 9, colour = "blue"))
    
    
    
  })
  
  
  
  output$sec3_scores_h <- renderPlot({
    
    df_final <- df_final()%>% 
      filter(platform == "high_throughput") %>%
      group_by(lab_name,assessment_date,year_month)%>% #,sub_platform
      summarise( S3_Safety = round(((sum(sec3_total)/sum(sec3_max))*100), digits = 0))%>%
      mutate(cols = case_when(
        (S3_Safety >= 95 & S3_Safety <= 100) ~ "#357435",
        (S3_Safety >= 85 & S3_Safety < 95)  ~ "#90dc67",
        (S3_Safety >=75 & S3_Safety < 85) ~ "#FFFF00",
        (S3_Safety >=65 & S3_Safety < 75) ~ "#FFD580",
        (S3_Safety >=55 & S3_Safety < 65) ~ "#FFAC1C",
        (S3_Safety >0 & S3_Safety < 55) ~ "#ff2701",
        TRUE ~ "#808080"  #anything that does not meet the criteria above
      ))
    
    
    ggplot(df_final, aes(x = year_month, y = S3_Safety))+
      geom_bar(stat = 'identity', width = 0.3, aes(fill = cols))+
      scale_fill_identity()+
      labs(
        y = "Percentage", x = "Facility Name & Period")+
      geom_text(aes(label = paste0(S3_Safety, "%")), vjust = -1, size = 3, fontface = 'bold')+
      ylim(0,140)+
      facet_wrap( ~ lab_name, strip.position = "bottom", scales = "free_x", ncol = 3, nrow = 10)+
      theme_classic()+
      theme(axis.text = element_text(size = 9))+
      theme(strip.text.x = element_text(size = 9, colour = "blue"))
  })
  
  
  
  
  #---- Sec3 Detail Table-----  
  
  
  
  output$sec3_table = DT::renderDT({
    df_final <- df_final()%>%  group_by(assessment_date, lab_name) %>% #,sub_platform
      summarise( 
        Q1 = sum(Q3_1),
        Q2 = sum(Q3_2),
        Q3 = sum(Q3_3),
        Q4 = sum(Q3_4),
        Q5 = sum(Q3_5),
        Q6 = sum(Q3_6),
        Q7 = sum(Q3_7),
        Q8 = sum(Q3_8),
        Q9 = sum(Q3_9),
        Q10 = sum(Q3_10),
        Q11 = sum(Q3_11),
        Q12 = sum(Q3_12),
        
        Audit_Score = sum(sec3_total),
        Max_Score = sum(sec3_max),
        Score_Percent = round(((sum(sec3_total)/sum(sec3_max))*100), digits = 0)
        
      ) %>%
      rename("Laboratory_Name" = lab_name)%>%
      rename("Date" = assessment_date)
    
    
    
    output$dl_sec3_table <- downloadHandler(
      filename = function() {
        paste("data-sec3-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(df_final, file)
      }
    )   
    
    
    DT::datatable(df_final,
                  
                  extensions = c('Buttons','FixedColumns'),
                  options = list(lengthChange = TRUE, fixedColumns = list(leftColumns = 3),
                                 scrollX = TRUE, scrollY = "300px",pageLength = 5), 
                  caption = "[1 = Yes, 0.5 = Partial, 0 = No] ") %>%
      formatStyle(
        columns = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12" ),
        backgroundColor = styleInterval(c(0, 0.5, 1), c('red','yellow', 'green','grey')),
        fontWeight = 'bold')
    
    
    
  })
  

  #-------Section 4 score ------
  
  output$sec4_scores_g <- renderPlot({
    
    df_final <- df_final()%>% 
      filter(platform == "genexpert") %>%
      group_by(lab_name,assessment_date,year_month)%>% #,sub_platform
      summarise( S4_Inventory = round(((sum(sec4_total)/sum(sec4_max))*100), digits = 0))%>%
      mutate(cols = case_when(
        (S4_Inventory >= 95 & S4_Inventory <= 100) ~ "#357435",
        (S4_Inventory >= 85 & S4_Inventory < 95)  ~ "#90dc67",
        (S4_Inventory >=75 & S4_Inventory < 85) ~ "#FFFF00",
        (S4_Inventory >=65 & S4_Inventory < 75) ~ "#FFD580",
        (S4_Inventory >=55 & S4_Inventory < 65) ~ "#FFAC1C",
        (S4_Inventory >0 & S4_Inventory < 55) ~ "#ff2701",
        TRUE ~ "#808080"  #anything that does not meet the criteria above
      ))
    
    
    ggplot(df_final, aes(x = year_month, y = S4_Inventory))+
      geom_bar(stat = 'identity', width = 0.3, aes(fill = cols))+
      scale_fill_identity()+
      labs(
        y = "Percentage", x = "Facility Name & Period")+
      geom_text(aes(label = paste0(S4_Inventory, "%")), vjust = -1, size = 3, fontface = 'bold')+
      ylim(0,140)+
      facet_wrap( ~ lab_name, strip.position = "bottom", scales = "free_x", ncol = 4, nrow = 10)+
      theme_classic()+
      theme(axis.text = element_text(size = 9))+
      theme(strip.text.x = element_text(size = 9, colour = "blue"))
    
    
    
  })
  
  
  
  output$sec4_scores_h <- renderPlot({
    
    df_final <- df_final()%>% 
      filter(platform == "high_throughput") %>%
      group_by(lab_name,assessment_date,year_month)%>% #,sub_platform
      summarise( S4_Inventory = round(((sum(sec4_total)/sum(sec4_max))*100), digits = 0))%>%
      mutate(cols = case_when(
        (S4_Inventory >= 95 & S4_Inventory <= 100) ~ "#357435",
        (S4_Inventory >= 85 & S4_Inventory < 95)  ~ "#90dc67",
        (S4_Inventory >=75 & S4_Inventory < 85) ~ "#FFFF00",
        (S4_Inventory >=65 & S4_Inventory < 75) ~ "#FFD580",
        (S4_Inventory >=55 & S4_Inventory < 65) ~ "#FFAC1C",
        (S4_Inventory >0 & S4_Inventory < 55) ~ "#ff2701",
        TRUE ~ "#808080"  #anything that does not meet the criteria above
      ))
    
    
    ggplot(df_final, aes(x = year_month, y = S4_Inventory))+
      geom_bar(stat = 'identity', width = 0.3, aes(fill = cols))+
      scale_fill_identity()+
      labs(
        y = "Percentage", x = "Facility Name & Period")+
      geom_text(aes(label = paste0(S4_Inventory, "%")), vjust = -1, size = 3, fontface = 'bold')+
      ylim(0,140)+
      facet_wrap( ~ lab_name, strip.position = "bottom", scales = "free_x", ncol = 3, nrow = 10)+
      theme_classic()+
      theme(axis.text = element_text(size = 9))+
      theme(strip.text.x = element_text(size = 9, colour = "blue"))
  })
  
  
  
  
  #---- Sec4 Detail Table-----  
  
  
  
  output$sec4_table = DT::renderDT({
    df_final <- df_final()%>%  group_by(assessment_date, lab_name) %>% #,sub_platform
      summarise( 
        Q1 = sum(Q4_1),
        Q2 = sum(Q4_2),
        Q3 = sum(Q4_3),
        Q4 = sum(Q4_4),
        Q5 = sum(Q4_5),
        Q6_G = sum(Q4_6_G),
        Q6_H = sum(Q4_6_H),
        Q7 = sum(Q4_7),
        Q8 = sum(Q4_8),
       
        
        Audit_Score = sum(sec4_total),
        Max_Score = sum(sec4_max),
        Score_Percent = round(((sum(sec4_total)/sum(sec4_max))*100), digits = 0)
        
      ) %>%
      rename("Laboratory_Name" = lab_name)%>%
      rename("Date" = assessment_date)
    
    
    
    output$dl_sec4_table <- downloadHandler(
      filename = function() {
        paste("data-sec4-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(df_final, file)
      }
    )   
    
    
    DT::datatable(df_final,
                  
                  extensions = c('Buttons','FixedColumns'),
                  options = list(lengthChange = TRUE, fixedColumns = list(leftColumns = 3),
                                 scrollX = TRUE, scrollY = "300px",pageLength = 5), 
                  caption = "[1 = Yes, 0.5 = Partial, 0 = No] ") %>%
      formatStyle(
        columns = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6_G","Q6_H", "Q7", "Q8" ),
        backgroundColor = styleInterval(c(0, 0.5, 1), c('red','yellow', 'green','grey')),
        fontWeight = 'bold')
    
    
    
  })  
  
  
  
  #-------Section 5 score ------
  
  output$sec5_scores_g <- renderPlot({
    
    df_final <- df_final()%>% 
      filter(platform == "genexpert") %>%
      group_by(lab_name,assessment_date,year_month)%>% #,sub_platform
      summarise( S5_Sample_Mx = round(((sum(sec5_total)/sum(sec5_max))*100), digits = 0))%>%
      mutate(cols = case_when(
        (S5_Sample_Mx >= 95 & S5_Sample_Mx <= 100) ~ "#357435",
        (S5_Sample_Mx >= 85 & S5_Sample_Mx < 95)  ~ "#90dc67",
        (S5_Sample_Mx >=75 & S5_Sample_Mx < 85) ~ "#FFFF00",
        (S5_Sample_Mx >=65 & S5_Sample_Mx < 75) ~ "#FFD580",
        (S5_Sample_Mx >=55 & S5_Sample_Mx < 65) ~ "#FFAC1C",
        (S5_Sample_Mx >0 & S5_Sample_Mx < 55) ~ "#ff2701",
        TRUE ~ "#808080"  #anything that does not meet the criteria above
      ))
    
    
    ggplot(df_final, aes(x = year_month, y = S5_Sample_Mx))+
      geom_bar(stat = 'identity', width = 0.3, aes(fill = cols))+
      scale_fill_identity()+
      labs(
        y = "Percentage", x = "Facility Name & Period")+
      geom_text(aes(label = paste0(S5_Sample_Mx, "%")), vjust = -1, size = 3, fontface = 'bold')+
      ylim(0,140)+
      facet_wrap( ~ lab_name, strip.position = "bottom", scales = "free_x", ncol = 4, nrow = 10)+
      theme_classic()+
      theme(axis.text = element_text(size = 9))+
      theme(strip.text.x = element_text(size = 9, colour = "blue"))
    
    
    
  })
  
  
  
  output$sec5_scores_h <- renderPlot({
    
    df_final <- df_final()%>% 
      filter(platform == "high_throughput") %>%
      group_by(lab_name,assessment_date,year_month)%>% #,sub_platform
      summarise( S5_Sample_Mx = round(((sum(sec5_total)/sum(sec5_max))*100), digits = 0))%>%
      mutate(cols = case_when(
        (S5_Sample_Mx >= 95 & S5_Sample_Mx <= 100) ~ "#357435",
        (S5_Sample_Mx >= 85 & S5_Sample_Mx < 95)  ~ "#90dc67",
        (S5_Sample_Mx >=75 & S5_Sample_Mx < 85) ~ "#FFFF00",
        (S5_Sample_Mx >=65 & S5_Sample_Mx < 75) ~ "#FFD580",
        (S5_Sample_Mx >=55 & S5_Sample_Mx < 65) ~ "#FFAC1C",
        (S5_Sample_Mx >0 & S5_Sample_Mx < 55) ~ "#ff2701",
        TRUE ~ "#808080"  #anything that does not meet the criteria above
      ))
    
    
    ggplot(df_final, aes(x = year_month, y = S5_Sample_Mx))+
      geom_bar(stat = 'identity', width = 0.3, aes(fill = cols))+
      scale_fill_identity()+
      labs(
        y = "Percentage", x = "Facility Name & Period")+
      geom_text(aes(label = paste0(S5_Sample_Mx, "%")), vjust = -1, size = 3, fontface = 'bold')+
      ylim(0,140)+
      facet_wrap( ~ lab_name, strip.position = "bottom", scales = "free_x", ncol = 3, nrow = 10)+
      theme_classic()+
      theme(axis.text = element_text(size = 9))+
      theme(strip.text.x = element_text(size = 9, colour = "blue"))
  })
  
  
  
  
  #---- Sec5 Detail Table-----  
  
  
  
  output$sec5_table = DT::renderDT({
    df_final <- df_final()%>%  group_by(assessment_date, lab_name) %>% #,sub_platform
      summarise( 
        Q1 = sum(Q5_1),
        Q2 = sum(Q5_2),
        Q3 = sum(Q5_3),
        Q4 = sum(Q5_4),
        Q5 = sum(Q5_5),
        Q6 = sum(Q5_6),
        Q7 = sum(Q5_7),
        Q8 = sum(Q5_8),
        
        
        Audit_Score = sum(sec5_total),
        Max_Score = sum(sec5_max),
        Score_Percent = round(((sum(sec5_total)/sum(sec5_max))*100), digits = 0)
        
      ) %>%
      rename("Laboratory_Name" = lab_name)%>%
      rename("Date" = assessment_date)
    
    
    
    output$dl_sec5_table <- downloadHandler(
      filename = function() {
        paste("data-sec5-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(df_final, file)
      }
    )   
    
    
    DT::datatable(df_final,
                  
                  extensions = c('Buttons','FixedColumns'),
                  options = list(lengthChange = TRUE, fixedColumns = list(leftColumns = 3),
                                 scrollX = TRUE, scrollY = "300px",pageLength = 5), 
                  caption = "[1 = Yes, 0.5 = Partial, 0 = No] ") %>%
      formatStyle(
        columns = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8" ),
        backgroundColor = styleInterval(c(0, 0.5, 1), c('red','yellow', 'green','grey')),
        fontWeight = 'bold')
    
    
    
  })  
   
  #-------Section 6 score ------
  
  output$sec6_scores_g <- renderPlot({
    
    df_final <- df_final()%>% 
      filter(platform == "genexpert") %>%
      group_by(lab_name,assessment_date,year_month)%>% #,sub_platform
      summarise( S6_Equipment = round(((sum(sec6_total)/sum(sec6_max))*100), digits = 0))%>%
      mutate(cols = case_when(
        (S6_Equipment >= 95 & S6_Equipment <= 100) ~ "#357435",
        (S6_Equipment >= 85 & S6_Equipment < 95)  ~ "#90dc67",
        (S6_Equipment >=75 & S6_Equipment < 85) ~ "#FFFF00",
        (S6_Equipment >=65 & S6_Equipment < 75) ~ "#FFD580",
        (S6_Equipment >=55 & S6_Equipment < 65) ~ "#FFAC1C",
        (S6_Equipment >0 & S6_Equipment < 55) ~ "#ff2701",
        TRUE ~ "#808080"  #anything that does not meet the criteria above
      ))
    
    
    ggplot(df_final, aes(x = year_month, y = S6_Equipment))+
      geom_bar(stat = 'identity', width = 0.3, aes(fill = cols))+
      scale_fill_identity()+
      labs(
        y = "Percentage", x = "Facility Name & Period")+
      geom_text(aes(label = paste0(S6_Equipment, "%")), vjust = -1, size = 3, fontface = 'bold')+
      ylim(0,140)+
      facet_wrap( ~ lab_name, strip.position = "bottom", scales = "free_x", ncol = 4, nrow = 10)+
      theme_classic()+
      theme(axis.text = element_text(size = 9))+
      theme(strip.text.x = element_text(size = 9, colour = "blue"))
    
    
    
  })
  
  
  
  output$sec6_scores_h <- renderPlot({
    
    df_final <- df_final()%>% 
      filter(platform == "high_throughput") %>%
      group_by(lab_name,assessment_date,year_month)%>% #,sub_platform
      summarise( S6_Equipment = round(((sum(sec6_total)/sum(sec6_max))*100), digits = 0))%>%
      mutate(cols = case_when(
        (S6_Equipment >= 95 & S6_Equipment <= 100) ~ "#357435",
        (S6_Equipment >= 85 & S6_Equipment < 95)  ~ "#90dc67",
        (S6_Equipment >=75 & S6_Equipment < 85) ~ "#FFFF00",
        (S6_Equipment >=65 & S6_Equipment < 75) ~ "#FFD580",
        (S6_Equipment >=55 & S6_Equipment < 65) ~ "#FFAC1C",
        (S6_Equipment >0 & S6_Equipment < 55) ~ "#ff2701",
        TRUE ~ "#808080"  #anything that does not meet the criteria above
      ))
    
    
    ggplot(df_final, aes(x = year_month, y = S6_Equipment))+
      geom_bar(stat = 'identity', width = 0.3, aes(fill = cols))+
      scale_fill_identity()+
      labs(
        y = "Percentage", x = "Facility Name & Period")+
      geom_text(aes(label = paste0(S6_Equipment, "%")), vjust = -1, size = 3, fontface = 'bold')+
      ylim(0,140)+
      facet_wrap( ~ lab_name, strip.position = "bottom", scales = "free_x", ncol = 3, nrow = 10)+
      theme_classic()+
      theme(axis.text = element_text(size = 9))+
      theme(strip.text.x = element_text(size = 9, colour = "blue"))
  })
  
  
  
  
  #---- Sec6 Detail Table-----  
  
  
  
  output$sec6_table = DT::renderDT({
    df_final <- df_final()%>%  group_by(assessment_date, lab_name) %>% #,sub_platform
      summarise( 
        Q1 = sum(Q6_1),
        Q2 = sum(Q6_2),
        Q3 = sum(Q6_3),
        Q4 = sum(Q6_4),
        Q5 = sum(Q6_5),
        
        Audit_Score = sum(sec6_total),
        Max_Score = sum(sec6_max),
        Score_Percent = round(((sum(sec6_total)/sum(sec6_max))*100), digits = 0)
        
      ) %>%
      rename("Laboratory_Name" = lab_name)%>%
      rename("Date" = assessment_date)
    
    
    
    output$dl_sec6_table <- downloadHandler(
      filename = function() {
        paste("data-sec6-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(df_final, file)
      }
    )   
    
    
    DT::datatable(df_final,
                  
                  extensions = c('Buttons','FixedColumns'),
                  options = list(lengthChange = TRUE, fixedColumns = list(leftColumns = 3),
                                 scrollX = TRUE, scrollY = "300px",pageLength = 5), 
                  caption = "[1 = Yes, 0.5 = Partial, 0 = No] ") %>%
      formatStyle(
        columns = c("Q1", "Q2", "Q3", "Q4", "Q5" ),
        backgroundColor = styleInterval(c(0, 0.5, 1), c('red','yellow', 'green','grey')),
        fontWeight = 'bold')
    
    
    
  })   
  
  
  #-------Section 7 score ------
  
  output$sec7_scores_g <- renderPlot({
    
    df_final <- df_final()%>% 
      filter(platform == "genexpert") %>%
      group_by(lab_name,assessment_date,year_month)%>% #,sub_platform
      summarise( S7_Process_Control = round(((sum(sec7_total)/sum(sec7_max))*100), digits = 0))%>%
      mutate(cols = case_when(
        (S7_Process_Control >= 95 & S7_Process_Control <= 100) ~ "#357435",
        (S7_Process_Control >= 85 & S7_Process_Control < 95)  ~ "#90dc67",
        (S7_Process_Control >=75 & S7_Process_Control < 85) ~ "#FFFF00",
        (S7_Process_Control >=65 & S7_Process_Control < 75) ~ "#FFD580",
        (S7_Process_Control >=55 & S7_Process_Control < 65) ~ "#FFAC1C",
        (S7_Process_Control >0 & S7_Process_Control < 55) ~ "#ff2701",
        TRUE ~ "#808080"  #anything that does not meet the criteria above
      ))
    
    
    ggplot(df_final, aes(x = year_month, y = S7_Process_Control))+
      geom_bar(stat = 'identity', width = 0.3, aes(fill = cols))+
      scale_fill_identity()+
      labs(
        y = "Percentage", x = "Facility Name & Period")+
      geom_text(aes(label = paste0(S7_Process_Control, "%")), vjust = -1, size = 3, fontface = 'bold')+
      ylim(0,140)+
      facet_wrap( ~ lab_name, strip.position = "bottom", scales = "free_x", ncol = 4, nrow = 10)+
      theme_classic()+
      theme(axis.text = element_text(size = 9))+
      theme(strip.text.x = element_text(size = 9, colour = "blue"))
    
    
    
  })
  
  
  
  output$sec7_scores_h <- renderPlot({
    
    df_final <- df_final()%>% 
      filter(platform == "high_throughput") %>%
      group_by(lab_name,assessment_date,year_month)%>% #,sub_platform
      summarise( S7_Process_Control = round(((sum(sec7_total)/sum(sec7_max))*100), digits = 0))%>%
      mutate(cols = case_when(
        (S7_Process_Control >= 95 & S7_Process_Control <= 100) ~ "#357435",
        (S7_Process_Control >= 85 & S7_Process_Control < 95)  ~ "#90dc67",
        (S7_Process_Control >=75 & S7_Process_Control < 85) ~ "#FFFF00",
        (S7_Process_Control >=65 & S7_Process_Control < 75) ~ "#FFD580",
        (S7_Process_Control >=55 & S7_Process_Control < 65) ~ "#FFAC1C",
        (S7_Process_Control >0 & S7_Process_Control < 55) ~ "#ff2701",
        TRUE ~ "#808080"  #anything that does not meet the criteria above
      ))
    
    
    ggplot(df_final, aes(x = year_month, y = S7_Process_Control))+
      geom_bar(stat = 'identity', width = 0.3, aes(fill = cols))+
      scale_fill_identity()+
      labs(
        y = "Percentage", x = "Facility Name & Period")+
      geom_text(aes(label = paste0(S7_Process_Control, "%")), vjust = -1, size = 3, fontface = 'bold')+
      ylim(0,140)+
      facet_wrap( ~ lab_name, strip.position = "bottom", scales = "free_x", ncol = 3, nrow = 10)+
      theme_classic()+
      theme(axis.text = element_text(size = 9))+
      theme(strip.text.x = element_text(size = 9, colour = "blue"))
  })
  
  
  
  
  #---- Sec7 Detail Table-----  
  
  
  
  output$sec7_table = DT::renderDT({
    df_final <- df_final()%>%  group_by(assessment_date, lab_name) %>% #,sub_platform
      summarise( 
        Q1 = sum(Q7_1),
        Q2 = sum(Q7_2),
        Q3 = sum(Q7_3),
        Q4_H = sum(Q7_4_H),
        Q5_H = sum(Q7_5_H),
        Q6_H = sum(Q7_6_H),
        Q7H_Q4G = sum(Q7_4_G_Q7_7_H),
        Q8H_Q5G = sum(Q7_5_G_Q7_8_H),
        Q9H_Q6G = sum(Q7_6_G_Q7_9_H),
        Q10H_Q7G = sum(Q7_7_G_Q7_10_H),
        Q11H_Q8G = sum(Q7_8_G_Q7_11_H),
        Q12H_Q9G = sum(Q7_9_G_Q7_12_H),
        Q13H_Q10G = sum(Q7_10_G_Q7_13_H),
        
        Audit_Score = sum(sec7_total),
        Max_Score = sum(sec7_max),
        Score_Percent = round(((sum(sec7_total)/sum(sec7_max))*100), digits = 0)
        
      ) %>%
      rename("Laboratory_Name" = lab_name)%>%
      rename("Date" = assessment_date)
    
    
    
    output$dl_sec7_table <- downloadHandler(
      filename = function() {
        paste("data-sec7-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(df_final, file)
      }
    )   
    
    
    DT::datatable(df_final,
                  
                  extensions = c('Buttons','FixedColumns'),
                  options = list(lengthChange = TRUE, fixedColumns = list(leftColumns = 3),
                                 scrollX = TRUE, scrollY = "300px",pageLength = 5), 
                  caption = "[1 = Yes, 0.5 = Partial, 0 = No] ") %>%
      formatStyle(
        columns = c("Q1", "Q2", "Q3", "Q4_H", "Q5_H", "Q6_H", "Q7H_Q4G", "Q8H_Q5G","Q9H_Q6G", "Q10H_Q7G", "Q11H_Q8G", "Q12H_Q9G", "Q13H_Q10G" ),
        backgroundColor = styleInterval(c(0, 0.5, 1), c('red','yellow', 'green','grey')),
        fontWeight = 'bold')
    
    
    
  })   
  
  
  
  #-------Section 8 score ------
  
  output$sec8_scores_g <- renderPlot({
    
    df_final <- df_final()%>% 
      filter(platform == "genexpert") %>%
      group_by(lab_name,assessment_date,year_month)%>% #,sub_platform
      summarise( S8_MnE = round(((sum(sec8_total)/sum(sec8_max))*100), digits = 0))%>%
      mutate(cols = case_when(
        (S8_MnE >= 95 & S8_MnE <= 100) ~ "#357435",
        (S8_MnE >= 85 & S8_MnE < 95)  ~ "#90dc67",
        (S8_MnE >=75 & S8_MnE < 85) ~ "#FFFF00",
        (S8_MnE >=65 & S8_MnE < 75) ~ "#FFD580",
        (S8_MnE >=55 & S8_MnE < 65) ~ "#FFAC1C",
        (S8_MnE >0 & S8_MnE < 55) ~ "#ff2701",
        TRUE ~ "#808080"  #anything that does not meet the criteria above
      ))
    
    
    ggplot(df_final, aes(x = year_month, y = S8_MnE))+
      geom_bar(stat = 'identity', width = 0.3, aes(fill = cols))+
      scale_fill_identity()+
      labs(
        y = "Percentage", x = "Facility Name & Period")+
      geom_text(aes(label = paste0(S8_MnE, "%")), vjust = -1, size = 3, fontface = 'bold')+
      ylim(0,140)+
      facet_wrap( ~ lab_name, strip.position = "bottom", scales = "free_x", ncol = 4, nrow = 10)+
      theme_classic()+
      theme(axis.text = element_text(size = 9))+
      theme(strip.text.x = element_text(size = 9, colour = "blue"))
    
    
    
  })
  
  
  
  output$sec8_scores_h <- renderPlot({
    
    df_final <- df_final()%>% 
      filter(platform == "high_throughput") %>%
      group_by(lab_name,assessment_date,year_month)%>% #,sub_platform
      summarise( S8_MnE = round(((sum(sec8_total)/sum(sec8_max))*100), digits = 0))%>%
      mutate(cols = case_when(
        (S8_MnE >= 95 & S8_MnE <= 100) ~ "#357435",
        (S8_MnE >= 85 & S8_MnE < 95)  ~ "#90dc67",
        (S8_MnE >=75 & S8_MnE < 85) ~ "#FFFF00",
        (S8_MnE >=65 & S8_MnE < 75) ~ "#FFD580",
        (S8_MnE >=55 & S8_MnE < 65) ~ "#FFAC1C",
        (S8_MnE >0 & S8_MnE < 55) ~ "#ff2701",
        TRUE ~ "#808080"  #anything that does not meet the criteria above
      ))
    
    
    ggplot(df_final, aes(x = year_month, y = S8_MnE))+
      geom_bar(stat = 'identity', width = 0.3, aes(fill = cols))+
      scale_fill_identity()+
      labs(
        y = "Percentage", x = "Facility Name & Period")+
      geom_text(aes(label = paste0(S8_MnE, "%")), vjust = -1, size = 3, fontface = 'bold')+
      ylim(0,140)+
      facet_wrap( ~ lab_name, strip.position = "bottom", scales = "free_x", ncol = 3, nrow = 10)+
      theme_classic()+
      theme(axis.text = element_text(size = 9))+
      theme(strip.text.x = element_text(size = 9, colour = "blue"))
  })
  
  
  
  
  #---- Sec8 Detail Table-----  
  
  
  
  output$sec8_table = DT::renderDT({
    df_final <- df_final()%>%  group_by(assessment_date, lab_name) %>% #,sub_platform
      summarise( 
        Q1 = sum(Q8_1),
        Q2 = sum(Q8_2),
        Q3 = sum(Q8_3),
        Q4_G = sum(Q8_4_G),
        Q4_H = sum(Q8_4_H),
        Q5_G = sum(Q8_5_G),
        Q5_H = sum(Q8_5_H),
        Q6 = sum(Q8_6),
        Q7 = sum(Q8_7),
        Q8 = sum(Q8_8),
        Q9 = sum(Q8_9),
        Q10 = sum(Q8_10),
        Q11 = sum(Q8_11),
        
        Audit_Score = sum(sec8_total),
        Max_Score = sum(sec8_max),
        Score_Percent = round(((sum(sec8_total)/sum(sec8_max))*100), digits = 0)
        
      ) %>%
      rename("Laboratory_Name" = lab_name)%>%
      rename("Date" = assessment_date)
    
    
    
    output$dl_sec8_table <- downloadHandler(
      filename = function() {
        paste("data-sec8-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(df_final, file)
      }
    )   
    
    
    DT::datatable(df_final,
                  
                  extensions = c('Buttons','FixedColumns'),
                  options = list(lengthChange = TRUE, fixedColumns = list(leftColumns = 3),
                                 scrollX = TRUE, scrollY = "300px",pageLength = 5), 
                  caption = "[1 = Yes, 0.5 = Partial, 0 = No] ") %>%
      formatStyle(
        columns = c("Q2", "Q3","Q4_G", "Q4_H", "Q5_G","Q5_H", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11" ),
        backgroundColor = styleInterval(c(0, 0.5, 1), c('red','yellow', 'green','grey')),
        fontWeight = 'bold') %>%
      formatStyle(
        columns = c("Q1"),
        backgroundColor = styleInterval(c(0, 5, 8,10), c('red','yellow', 'green','green','grey')),
        fontWeight = 'bold'
      )
    
    
    
    
    
  })   
  
  
  
  #-------Section 9 score ------
  
  output$sec9_scores_g <- renderPlot({
    
    df_final <- df_final()%>% 
      filter(platform == "genexpert") %>%
      group_by(lab_name,assessment_date,year_month)%>% #,sub_platform
      summarise( S9_IQA = round(((sum(sec9_total)/sum(sec9_max))*100), digits = 0))%>%
      mutate(cols = case_when(
        (S9_IQA >= 95 & S9_IQA <= 100) ~ "#357435",
        (S9_IQA >= 85 & S9_IQA < 95)  ~ "#90dc67",
        (S9_IQA >=75 & S9_IQA < 85) ~ "#FFFF00",
        (S9_IQA >=65 & S9_IQA < 75) ~ "#FFD580",
        (S9_IQA >=55 & S9_IQA < 65) ~ "#FFAC1C",
        (S9_IQA >0 & S9_IQA < 55) ~ "#ff2701",
        TRUE ~ "#808080"  #anything that does not meet the criteria above
      ))
    
    
    ggplot(df_final, aes(x = year_month, y = S9_IQA))+
      geom_bar(stat = 'identity', width = 0.3, aes(fill = cols))+
      scale_fill_identity()+
      labs(
        y = "Percentage", x = "Facility Name & Period")+
      geom_text(aes(label = paste0(S9_IQA, "%")), vjust = -1, size = 3, fontface = 'bold')+
      ylim(0,140)+
      facet_wrap( ~ lab_name, strip.position = "bottom", scales = "free_x", ncol = 4, nrow = 10)+
      theme_classic()+
      theme(axis.text = element_text(size = 9))+
      theme(strip.text.x = element_text(size = 9, colour = "blue"))
    
    
    
  })
  
  
  
  output$sec9_scores_h <- renderPlot({
    
    df_final <- df_final()%>% 
      filter(platform == "high_throughput") %>%
      group_by(lab_name,assessment_date,year_month)%>% #,sub_platform
      summarise( S9_IQA = round(((sum(sec9_total)/sum(sec9_max))*100), digits = 0))%>%
      mutate(cols = case_when(
        (S9_IQA >= 95 & S9_IQA <= 100) ~ "#357435",
        (S9_IQA >= 85 & S9_IQA < 95)  ~ "#90dc67",
        (S9_IQA >=75 & S9_IQA < 85) ~ "#FFFF00",
        (S9_IQA >=65 & S9_IQA < 75) ~ "#FFD580",
        (S9_IQA >=55 & S9_IQA < 65) ~ "#FFAC1C",
        (S9_IQA >0 & S9_IQA < 55) ~ "#ff2701",
        TRUE ~ "#808080"  #anything that does not meet the criteria above
      ))
    
    
    ggplot(df_final, aes(x = year_month, y = S9_IQA))+
      geom_bar(stat = 'identity', width = 0.3, aes(fill = cols))+
      scale_fill_identity()+
      labs(
        y = "Percentage", x = "Facility Name & Period")+
      geom_text(aes(label = paste0(S9_IQA, "%")), vjust = -1, size = 3, fontface = 'bold')+
      ylim(0,140)+
      facet_wrap( ~ lab_name, strip.position = "bottom", scales = "free_x", ncol = 3, nrow = 10)+
      theme_classic()+
      theme(axis.text = element_text(size = 9))+
      theme(strip.text.x = element_text(size = 9, colour = "blue"))
  })
  
  
  
  
  #---- Sec9 Detail Table-----  
  
  
  
  output$sec9_table = DT::renderDT({
    df_final <- df_final()%>%  group_by(assessment_date, lab_name) %>% #,sub_platform
      summarise( 
        Q1 = sum(Q9_1),
        Q2 = sum(Q9_2),
        Q3 = sum(Q9_3),
        Q4 = sum(Q9_4),
        Q5 = sum(Q9_5),
        Q6 = sum(Q9_6),
        Q7 = sum(Q9_7),
        Q8 = sum(Q9_8),
       
        
        Audit_Score = sum(sec9_total),
        Max_Score = sum(sec9_max),
        Score_Percent = round(((sum(sec9_total)/sum(sec9_max))*100), digits = 0)
        
      ) %>%
      rename("Laboratory_Name" = lab_name)%>%
      rename("Date" = assessment_date)
    
    
    
    output$dl_sec9_table <- downloadHandler(
      filename = function() {
        paste("data-sec9-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(df_final, file)
      }
    )   
    
    
    DT::datatable(df_final,
                  
                  extensions = c('Buttons','FixedColumns'),
                  options = list(lengthChange = TRUE, fixedColumns = list(leftColumns = 3),
                                 scrollX = TRUE, scrollY = "300px",pageLength = 5), 
                  caption = "[1 = Yes, 0.5 = Partial, 0 = No] ") %>%
      formatStyle(
        columns = c("Q1","Q2", "Q3","Q4",  "Q5","Q6", "Q7", "Q8" ),
        backgroundColor = styleInterval(c(0, 0.5, 1), c('red','yellow', 'green','grey')),
        fontWeight = 'bold') 
    
  })  
  
  
  
  
  #------- Summary Report ------------
  
  output$summary = DT::renderDT({
    
    df_final <- df_final_2()%>%  
      filter(!is.na(assessment_date))%>%
      filter(!is.na(section))%>%
      group_by(section, assessment_date, lab_name ) %>%
      summarise(
                Summary_Comments_Recommendations = summary_comments,
                Timeline = timeline
      )
    
    output$dl_summary <- downloadHandler(
      filename = function() {
        paste("data-summary-report-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx(df_final, file)
      }
    ) 
    
    
    DT::datatable(df_final,extensions = c('Buttons','FixedColumns'),
                  options = list(lengthChange = TRUE, fixedColumns = list(leftColumns = 3),
                                 scrollX = TRUE, scrollY = "500px")
                  )
    
  }) # end of ca_all 
  
  #----------SITE-LEVEL REPORT HTP------- 
    ##----------Facility Info HTP------------ 
  output$fac_info_H <- renderUI({
    site <- df_final()%>% 
      filter(`lab_name` == input$facility)%>%
      filter(platform == "high_throughput")%>%
      group_by(lab_name, assessment_date)%>%
      summarise(
          Lab_Name = lab_name,
          State_Region = state_region,
          Township = township,
          Affiliation = affiliation,
          Assessment_Date = format(assessment_date,"%d-%b-%y") ,
          Start_Time = group_part1.start_time,
          End_Time = last_page.end_time,
          # Platform = sub_platform,
          
          Accessor_Name_1= accessor_name_1,
          Accessor_Name_2= accessor_name_2,
          First_Assessment = first_assessment,
          Date_Last_Assessemnt = previous_assessment_date
      )
    site <- site %>% 
      group_by(lab_name)%>%
      slice(which(assessment_date == max(assessment_date)))
    
  ##----- Table -----
    tags$table(
      style = "width:100%; background-color:#d2e6b5;",
      border = 2,
      
      tags$thead(
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
          vertical-align: middle; background-color:#06326b; color:white;", "HIV Viral Load and Infant Virological Testing Scorecard: Site-level Report")
        ),
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
          vertical-align: middle; background-color:#50C878; color:white ", "FACILITY INFORMATION")
        )
        
      ),
  ###------ Table Body ------
      tags$tbody(
        tags$tr(
          tags$td("Laboratory Name: "),
          tags$td(paste(site$Lab_Name)),
          tags$td("Affiliation: "),
          tags$td(paste(site$Affiliation))
        ),
        
        tags$tr( style = "background-color:#c1db9b;",
                 tags$td("State/Region: "),
                 tags$td(paste(site$State_Region)),
                 tags$td("Township: "),
                 tags$td(paste(site$Township))
        ),
        
        tags$tr( 
                 tags$td("Date: "),
                 tags$td(paste(site$Assessment_Date)),
                 tags$td("Platform:  "),
                 tags$td(paste(site$Platform))
        ),
        
        tags$tr( style = "background-color:#c1db9b;",
                 tags$td("Start Time: "),
                 tags$td(paste(site$Start_Time)),
                 tags$td("End Time: "),
                 tags$td(paste(site$End_Time))
        ),
        
        tags$tr(
          tags$td("Accessor Name 1: "),
          tags$td(paste(site$Accessor_Name_1)),
          tags$td("Accessor Name 2: "),
          tags$td(paste(site$Accessor_Name_2))
        ),
        
        tags$tr(style = "background-color:#c1db9b;",
          tags$td("First Assessment ?: "),
          tags$td(paste(site$First_Assessment)),
          tags$td("If no: Date of last assessment: "),
          tags$td(paste(site$Date_Last_Assessemnt))
        )
        
      ) #end of table body
    ) # end of table
  }) 
  
  ##----------Personnal HTP------------
  output$personnal_H <- renderUI({

    # site <- df_final()%>% filter(`lab_name` == input$facility)%>%
    #   filter(platform == "high_throughput")%>%
    #   group_by(lab_name, assessment_date)%>%
    # summarise( Lab_Name = lab_name,
    #            State_Region = state_region,
    #            Township = township,
    #            Affiliation = affiliation,
    #            Assessment_Date = format(assessment_date,"%d-%b-%y") ,
    #            Start_Time = group_part1.start_time,
    #            End_Time = last_page.end_time,
    #            # Platform = sub_platform,
    #            Accessor_Name_1= accessor_name_1,
    #            Accessor_Name_2= accessor_name_2,
    #            First_Assessment = first_assessment,
    #            Date_Last_Assessemnt = previous_assessment_date
    # )
    # site <- site %>% group_by(lab_name)%>%
    #   slice(which(assessment_date == max(assessment_date)))

    ##----- Table -----
    tags$table(
      # style = "width:100%; background-color:#d2e6b5;",
      # border = 2,

      tags$thead(
              tags$tr(
                tags$th(colspan = 4, style = "width:100%; text-align: center;
        vertical-align: middle; background-color:#002D62; color:white;", "SECTION 1: PERSONNEL (Yes = 1, Partial = 0.5, No = 0, NA = 99)")
              )

      )
      ###------ Table Body ------
      # tags$tbody(
      #   tags$tr(
      #     tags$td("Laboratory Name: "),
      #     tags$td(paste(site$Lab_Name)),
      #     tags$td("Affiliation: "),
      #     tags$td(paste(site$Affiliation))
      #   ),
      #   tags$tr( style = "background-color:#c1db9b;",
      #            tags$td("State/Region: "),
      #            tags$td(paste(site$State_Region)),
      #            tags$td("Township: "),
      #            tags$td(paste(site$Township))
      #   ),
      #   tags$tr(
      #     tags$td("Date: "),
      #     tags$td(paste(site$Assessment_Date)),
      #     tags$td("Platform:  "),
      #     tags$td(paste(site$Platform))
      #   ),
      #   tags$tr( style = "background-color:#c1db9b;",
      #            tags$td("Start Time: "),
      #            tags$td(paste(site$Start_Time)),
      #            tags$td("End Time: "),
      #            tags$td(paste(site$End_Time))
      #   ),
      #   tags$tr(
      #     tags$td("Accessor Name 1: "),
      #     tags$td(paste(site$Accessor_Name_1)),
      #     tags$td("Accessor Name 2: "),
      #     tags$td(paste(site$Accessor_Name_2))
      #   ),
      #   tags$tr(style = "background-color:#c1db9b;",
      #           tags$td("First Assessment ?: "),
      #           tags$td(paste(site$First_Assessment)),
      #           tags$td("If no: Date of last assessment: "),
      #           tags$td(paste(site$Date_Last_Assessemnt))
      #   )
      # ) #end of table body
    ) # end of table

  })

  ##-----Section 1 HTP report-----
  output$sec1_report_H <- renderUI({
    
    df_final <- df_final()%>%
      filter(lab_name == input$facility )%>%
      filter(platform == "high_throughput")%>%
      group_by(lab_name) %>% 
      slice(which(assessment_date == max(assessment_date))) 
    
    df_final <- df_final%>%  group_by(assessment_date, lab_name, platform) %>% #,sub_platform
      summarise(
        Audit_Score = sum(sec1_total),
        Max_Score = sum(sec1_max),
        Score_Percent = round(((sum(sec1_total)/sum(sec1_max))*100), digits = 0),
        Q1 = sum(Q1_1),
        Q2 = sum(Q1_2),
        Q3 = sum(Q1_3),
        Q4 = sum(Q1_4),
        Q5 = sum(Q1_5),
        Q6 = sum(Q1_6),
        Q7 = sum(Q1_7),
        Q8 = sum(Q1_8),
        Q9 = sum(Q1_9),
        Q10 = sum(Q1_10),
        Q11 = sum(Q1_11),
        C1 = C1_1,
        C2 = C1_2,
        C3 = C1_3,
        C4 = C1_4,
        C5 = C1_5,
        C6 = C1_6,
        C7 = C1_7,
        C8 = C1_8,
        C9 = C1_9,
        C10 = C1_10,
        C11 = C1_11)
      
  
   table <-  tags$table(
      style = "width:100%; background-color:#d2e6b5;",
      border = 2,
      
      tags$thead(
  #       tags$tr(
  #         tags$th(colspan = 4, style = "width:100%; text-align: center;
  # vertical-align: middle; background-color:#002D62; color:white;", "SECTION 1: PERSONNEL (Yes = 1, Partial = 0.5, No = 0, NA = 99)")
  #       ),
        
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", 
                  paste("SECTION 1 Total Score: ",
                        df_final$Audit_Score,
                        "out of ",
                        df_final$Max_Score,
                        "( ",
                        df_final$Score_Percent,
                        " % )"
                  ))
        ),
        tags$tr(
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "No."),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Question "),
          tags$th( style = "text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Score"),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Comment")
        )
      ), # end of tags$thead
      
      
      tags$tbody(
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "1.1"),
                 tags$td(
                   "Is the Viral Load (VL)/Infant Virological Testing (IVT) training program based on national policy?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q1, "out of 1")),
                 tags$td(
                   df_final$C1)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "1.2"),
                 tags$td(
                   "Have all laboratory personnel received comprehensive training on VL/IVT testing using approved Standard Operating Procedures (SOPs)?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q2, "out of 1")),
                 tags$td(
                   df_final$C2)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "1.3"),
                 tags$td(
                   "Are laboratory personnel trained on using standardized VL/IVT testing registers/log book/LIMS?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q3, "out of 1")),
                 tags$td(
                   df_final$C3)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "1.4"),
                 tags$td(
                   "Are laboratory personnel trained on sample management from collection to disposal?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q4, "out of 1")),
                 tags$td(
                   df_final$C4)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "1.5"),
                 tags$td(
                   "Are laboratory personnel trained on routine preventive equipment maintenance?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q5, "out of 1")),
                 tags$td(
                   df_final$C5)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "1.6"),
                 tags$td(
                   "Are laboratory personnel trained on the quality control process?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q6, "out of 1")),
                 tags$td(
                   df_final$C6)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "1.7"),
                 tags$td(
                   "Are laboratory personnel trained on safety and waste management procedures and practices?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q7, "out of 1")),
                 tags$td(
                   df_final$C7)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "1.8"),
                 tags$td(
                   "Are only trained/competent laboratory personnel allowed to perform VL/IVT testing?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q8, "out of 1")),
                 tags$td(
                   df_final$C8)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "1.9"),
                 tags$td(
                   "Are approved/signed records of all trainings for all laboratory personnel kept on file?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q9, "out of 1")),
                 tags$td(
                   df_final$C9)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "1.10"),
                 tags$td(
                   "Have all VL/IVT testing personnel received refresher training, according to the approved training program?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q10, "out of 1")),
                 tags$td(
                   df_final$C10)
        ),
        
        
          tags$tr( style = "border-top: thin solid;",
                   tags$td(style = "background-color:#c1db9b; ",
                           "1.11"),
                   tags$td(
                     "Do records indicate all laboratory personnel were deemed competent before independently testing client VL/IVT samples?"),
                   tags$td(style = "background-color:#c1db9b; ",
                           paste(df_final$Q11, "out of 1")),
                   tags$td(
                     df_final$C11)
          )
        
      ) # end of tbody
    ) # end of table 
   
  })
  
 
  ##-----Section 2 HTP report-----
  output$sec2_report_H <- renderUI({
    
    df_final <- df_final()%>%
      filter(lab_name == input$facility )%>%
      filter(platform == "high_throughput")%>%
      group_by(lab_name) %>% 
      slice(which(assessment_date == max(assessment_date))) 
    
    df_final <- df_final%>%  group_by(assessment_date, lab_name, platform) %>% #,sub_platform
      summarise( 
        Audit_Score = sum(sec2_total),
        Max_Score = sum(sec2_max),
        Score_Percent = round(((sum(sec2_total)/sum(sec2_max))*100), digits = 0),
        Q1 = sum(Q2_1),
        Q2 = sum(Q2_2),
        Q3 = sum(Q2_3),
        Q4 = sum(Q2_4),
        Q5 = sum(Q2_5),
        Q6 = sum(Q2_6),
        Q7 = sum(Q2_7),
        Q8 = sum(Q2_8),
        Q9 = sum(Q2_9),
        Q10 = sum(Q2_10),
        Q11 = sum(Q2_11),
        Q12 = sum(Q2_12_H),
        Q13 = sum(Q2_13),
        Q14 = sum(Q2_14),
        C1 = C2_1,
        C2 = C2_2,
        C3 = C2_3,
        C4 = C2_4,
        C5 = C2_5,
        C6 = C2_6,
        C7 = C2_7,
        C8 = C2_8,
        C9 = C2_9,
        C10 = C2_10,
        C11 = C2_11,
        C12 = C2_12,
        C13 = C2_13,
        C14 = C2_14)
    
    
    table <-  tags$table(
      style = "width:100%; background-color:#d2e6b5;",
      border = 2,
      
      tags$thead(
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", "SECTION 2: PHYSICAL FACILITY/ENVIRONMENT (Yes = 1, Partial = 0.5, No = 0)")
        ),
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", 
                  paste("SECTION 2 Total Score: ",
                        df_final$Audit_Score,
                        "out of ",
                        df_final$Max_Score,
                        "( ",
                        df_final$Score_Percent,
                        " % )"
                  ))
        ),
        tags$tr(
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "No."),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Question "),
          tags$th( style = "text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Score"),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Comment")
        )
      ), # end of tags$thead
      
      
      tags$tbody(
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.1"),
                 tags$td(
                   "Is there a designated area exclusively for VL/IVT testing?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q1, "out of 1")),
                 tags$td(
                   df_final$C1)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.2"),
                 tags$td(
                   "Does testing area meet manufacturer’s requirements for equipment installation?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q2, "out of 1")),
                 tags$td(
                   df_final$C2)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.3"),
                 tags$td(
                   "Is the VL/IVT testing area clean, and organized?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q3, "out of 1")),
                 tags$td(
                   df_final$C3)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.4"),
                 tags$td(
                   "Are SOPs for cleaning work areas in place and followed?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q4, "out of 1")),
                 tags$td(
                   df_final$C4)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.5"),
                 tags$td(
                   "Are reagents/supplies kept in a temperature controlled environment according to manufacturer’s instructions?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q5, "out of 1")),
                 tags$td(
                   df_final$C5)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.6"),
                 tags$td(
                   "Are SOPs in place and followed for temperature monitoring?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q6, "out of 1")),
                 tags$td(
                   df_final$C6)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.7"),
                 tags$td(
                   "Are acceptable temperature ranges defined for temperature dependent equipment?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q7, "out of 1")),
                 tags$td(
                   df_final$C7)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.8"),
                 tags$td(
                   "Are temperatures recorded daily for?
                   Freezers,
                   Refrigerators,
                   Room temperature"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q8, "out of 1")),
                 tags$td(
                   df_final$C8)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.9"),
                 tags$td(
                   "Is there documentation of corrective action taken in response to out of range temperatures?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q9, "out of 1")),
                 tags$td(
                   df_final$C9)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.10"),
                 tags$td(
                   "Are UPS in place for testing equipment?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q10, "out of 1")),
                 tags$td(
                   df_final$C10)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.11"),
                 tags$td(
                   "Is there a functional back-up generator?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q11, "out of 1")),
                 tags$td(
                   df_final$C11)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.12"),
                 tags$td(
                   "Is there secure storage space for consumables?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q12, "out of 1")),
                 tags$td(
                   df_final$C12)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.13"),
                 tags$td(
                   "Is there secure cold chain storage space?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q13, "out of 1")),
                 tags$td(
                   df_final$C13)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.14"),
                 tags$td(
                   "Is there secure backup cold chain storage space?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q14, "out of 1")),
                 tags$td(
                   df_final$C14)
        )
        

        
      ) # end of tbody
    ) # end of table 
    
  })   
 
 
  ##-----Section 3 HTP report-----
  output$sec3_report_H <- renderUI({
    
    df_final <- df_final()%>%
      filter(lab_name == input$facility )%>%
      filter(platform == "high_throughput")%>%
      group_by(lab_name) %>% 
      slice(which(assessment_date == max(assessment_date))) 
    
    df_final <- df_final%>%  group_by(assessment_date, lab_name, platform) %>% #,sub_platform
      summarise( 
        Audit_Score = sum(sec3_total),
        Max_Score = sum(sec3_max),
        Score_Percent = round(((sum(sec3_total)/sum(sec3_max))*100), digits = 0),
        Q1 = sum(Q3_1),
        Q2 = sum(Q3_2),
        Q3 = sum(Q3_3),
        Q4 = sum(Q3_4),
        Q5 = sum(Q3_5),
        Q6 = sum(Q3_6),
        Q7 = sum(Q3_7),
        Q8 = sum(Q3_8),
        Q9 = sum(Q3_9),
        Q10 = sum(Q3_10),
        Q11 = sum(Q3_11),
        Q12 = sum(Q3_12),
       
        C1 = C2_1,
        C2 = C2_2,
        C3 = C2_3,
        C4 = C2_4,
        C5 = C2_5,
        C6 = C2_6,
        C7 = C2_7,
        C8 = C2_8,
        C9 = C2_9,
        C10 = C2_10,
        C11 = C2_11,
        C12 = C2_12
        )
    
    
    table <-  tags$table(
      style = "width:100%; background-color:#d2e6b5;",
      border = 2,
      
      tags$thead(
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", "SECTION 3: SAFETY/ WASTE MANAGEMENT(Yes = 1, Partial = 0.5, No = 0)")
        ),
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", 
                  paste("SECTION 3 Total Score: ",
                        df_final$Audit_Score,
                        "out of ",
                        df_final$Max_Score,
                        "( ",
                        df_final$Score_Percent,
                        " % )"
                  ))
        ),
        tags$tr(
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "No."),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Question "),
          tags$th( style = "text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Score"),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Comment")
        )
      ), # end of tags$thead
      
      
      tags$tbody(
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.1"),
                 tags$td(
                   "Are SOPs in place and followed for personnel safety practices?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q1, "out of 1")),
                 tags$td(
                   df_final$C1)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.2"),
                 tags$td(
                   "Are SOPs in place and followed for disposal of infectious and non-infectious waste?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q2, "out of 1")),
                 tags$td(
                   df_final$C2)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.3"),
                 tags$td(
                   "Are SOPs in place and followed to manage biohazardous spills, e.g. blood?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q3, "out of 1")),
                 tags$td(
                   df_final$C3)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.4"),
                 tags$td(
                   "Are SOPs in place and followed to address accidental exposure to potentially infectious body fluids through needle-stick injury, splash or other sharps injury?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q4, "out of 1")),
                 tags$td(
                   df_final$C4)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.5"),
                 tags$td(
                   "Is personnel protective equipment (PPE) always available to the VL/IVT testing personnel?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q5, "out of 1")),
                 tags$td(
                   df_final$C5)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.6"),
                 tags$td(
                   "Do all laboratory personnel properly use PPE throughout the VL/IVT testing process?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q6, "out of 1")),
                 tags$td(
                   df_final$C6)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.7"),
                 tags$td(
                   "Are clean water and soap available for hand washing?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q7, "out of 1")),
                 tags$td(
                   df_final$C7)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.8"),
                 tags$td(
                   "Is an appropriate disinfectant available to clean the work area and equipment?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q8, "out of 1")),
                 tags$td(
                   df_final$C8)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.9"),
                 tags$td(
                   "Are sharps, infectious and non-infectious waste handled properly?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q9, "out of 1")),
                 tags$td(
                   df_final$C9)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.10"),
                 tags$td(
                   "Are containers for infectious and non-infectious waste emptied regularly in accordance with SOPs?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q10, "out of 1")),
                 tags$td(
                   df_final$C10)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.11"),
                 tags$td(
                   "Are eye wash and/or safety shower facilities readily accessible to laboratory personnel?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q11, "out of 1")),
                 tags$td(
                   df_final$C11)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.12"),
                 tags$td(
                   "Are SOPs in place and followed for proper handling of chemical waste?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q12, "out of 1")),
                 tags$td(
                   df_final$C12)
        )
        
       
        
        
      ) # end of tbody
    ) # end of table 
    
  })  
  
  
  ##-----Section 4 HTP report-----
  output$sec4_report_H <- renderUI({
    
    df_final <- df_final()%>%
      filter(lab_name == input$facility )%>%
      filter(platform == "high_throughput")%>%
      group_by(lab_name) %>%
      slice(which(assessment_date == max(assessment_date)))
    
    df_final <- df_final%>%  group_by(assessment_date, lab_name, platform) %>% #,sub_platform
      summarise( 
        Audit_Score = sum(sec4_total),
        Max_Score = sum(sec4_max),
        Score_Percent = round(((sum(sec4_total)/sum(sec4_max))*100), digits = 0),
        
        Who_Decide = paste(G4_Who_Decide, collapse = ", "),
        Who_Decide_other = paste(G4_Who_Decide_other, collapse = ", "),
        wh_Qty = paste(G4_Wh_Qty_base, collapse = ", "),
        wh_Qty_other = paste(G4_Wh_Qty_base_other, collapse = ", "),
        How_often = paste(G4_How_often, collapse = ", "),
        Comments = paste(G4_Comments, collapse = ", "),
        print(wh_Qty),
        
        Q1 = sum(Q4_1),
        Q2 = sum(Q4_2),
        Q3 = sum(Q4_3),
        Q4 = sum(Q4_4),
        Q5 = sum(Q4_5),
        Q6 = sum(Q4_6_H),
        
        Q7 = sum(Q4_7),
        Q8 = sum(Q4_8),
        
        C1 = C4_1,
        C2 = C4_2,
        C3 = C4_3,
        C4 = C4_4,
        C5 = C4_5,
        
        C6 = C2_6,
        C7 = C2_7,
        C8 = C2_8
      )

    table <-  tags$table(
      style = "width:100%; background-color:#d2e6b5;",
      border = 2,
      
      tags$thead(
        tags$tr( #Section Level1 Header
          tags$th(colspan = 4, style = "width:50%; text-align: center;
            vertical-align: middle; background-color:#002D62; color:white;", "SECTION 4: PROCUREMENT AND INVENTORY(Yes = 1, Partial = 0.5, No = 0)")
        ),
        ##New request Table
        # tags$tr( #Table Header
        #   tags$th( style = " text-align: center;
        #     vertical-align: middle; background-color:#50C878; color:white;", #background-color:#50C878 << Green
        #            "No."),
        #   tags$th( style = " text-align: center;
        #     vertical-align: middle; background-color:#50C878; color:white;", 
        #            "Question "),
          # tags$th( style = "text-align: center;
          #   vertical-align: middle; background-color:#50C878; color:white;", 
          #          "Score"),
          # tags$th( style = " text-align: center;
          #   vertical-align: middle; background-color:#50C878; color:white;", 
          #          "Comment")
        # )
      ), # end of tags$thead
      
      tags$tbody(
        tags$tr(
          tags$td("Who decides/quantities lab reagents/supplies to be procured: "),
          tags$td(paste(df_final$Who_Decide)),
          tags$td("Other, specify__: "),
          tags$td(paste(df_final$Who_Decide_other)),
          print(df_final$Who_Decide)
        ),
        tags$tr( style = "background-color:#c1db9b;",
          tags$td("What is the quantification based on: "),
          tags$td(paste(df_final$Wh_Qty)),
          tags$td("Other, specify__: "),
          tags$td(paste(df_final$Wh_Qty_other))
        ),
        tags$tr(
          tags$td("How often are reagents/supplies for VL/IVT ordered: "),
          tags$td(paste(df_final$How_often)),
          tags$td("Comments: "),
          tags$td(paste(df_final$Comments))
        ),
        # tags$tr( style = "border-top: thin solid;",
        #          tags$td(style = "background-color:#c1db9b; ",
        #                  "4.1"),
        #          tags$td(
        #            "Is there a SOP for inventory control?"),
        #          tags$td(style = "background-color:#c1db9b; ",
        #                  paste(df_final$Q1, "out of 1")),
        #          tags$td(
        #            df_final$C1)
        # ),
        # 
        # tags$tr( style = "border-top: thin solid;",
        #          tags$td(style = "background-color:#c1db9b; ",
        #                  "4.2"),
        #          tags$td(
        #            "Are SOPs in place and followed for receipt, inspection and storage of reagent/supplies?"),
        #          tags$td(style = "background-color:#c1db9b; ",
        #                  paste(df_final$Q2, "out of 1")),
        #          tags$td(
        #            df_final$C2)
        # ),
        # 
        # tags$tr( style = "border-top: thin solid;",
        #          tags$td(style = "background-color:#c1db9b; ",
        #                  "4.3"),
        #          tags$td(
        #            "Are reagents/supplies labeled with the date received and initials?"),
        #          tags$td(style = "background-color:#c1db9b; ",
        #                  paste(df_final$Q3, "out of 1")),
        #          tags$td(
        #            df_final$C3)
        # ),
        # 
        # tags$tr( style = "border-top: thin solid;",
        #          tags$td(style = "background-color:#c1db9b; ",
        #                  "4.4"),
        #          tags$td(
        #            "Are all reagents/supplies, currently in use, within their expiration period?"),
        #          tags$td(style = "background-color:#c1db9b; ",
        #                  paste(df_final$Q4, "out of 1")),
        #          tags$td(
        #            df_final$C4)
        # ),
        # 
        # tags$tr( style = "border-top: thin solid;",
        #          tags$td(style = "background-color:#c1db9b; ",
        #                  "4.5"),
        #          tags$td(
        #            "Are SOPs for disposal of reagents and consumables in place and followed?"),
        #          tags$td(style = "background-color:#c1db9b; ",
        #                  paste(df_final$Q5, "out of 1")),
        #          tags$td(
        #            df_final$C5)
        # ),
        # 
        # 
        # tags$tr( style = "border-top: thin solid;",
        #          tags$td(style = "background-color:#c1db9b; ",
        #                  "4.6"),
        #          tags$td(
        #            "Have all consumables/supplies been in stock during the past 6 months? If no or partial record number of stock outs in comment section."),
        #          tags$td(style = "background-color:#c1db9b; ",
        #                  paste(df_final$Q6, "out of 1")),
        #          tags$td(
        #            df_final$C6)
        # ),
        # 
        # tags$tr( style = "border-top: thin solid;",
        #          tags$td(style = "background-color:#c1db9b; ",
        #                  "4.7"),
        #          tags$td(
        #            "Have all reagents been in stock during the past 6 months? If no or partial record the number of stock outs in comment section."),
        #          tags$td(style = "background-color:#c1db9b; ",
        #                  paste(df_final$Q7, "out of 1")),
        #          tags$td(
        #            df_final$C7)
        # ),
        # 
        # 
        # tags$tr( style = "border-top: thin solid;",
        #          tags$td(style = "background-color:#c1db9b; ",
        #                  "4.8"),
        #          tags$td(
        #            "Are reagents/supplies appropriate for molecular testing (e.g. powder-free gloves, filtered tips, RNAse/DNAse-free)?"),
        #          tags$td(style = "background-color:#c1db9b; ",
        #                  paste(df_final$Q8, "out of 1")),
        #          tags$td(
        #            df_final$C8)
        # )
      ), # end of tbody
      
      ##Section4 Table2
      tags$thead(
        tags$tr( #Section Level2 Header
          tags$th(colspan = 4, style = "width:100%; text-align: center;
            vertical-align: middle; background-color:#002D62; color:white;", #background-color:#002D62 << Dark-Blue
                  paste("SECTION 4 Total Score: ",
                        df_final$Audit_Score,
                        "out of ",
                        df_final$Max_Score,
                        "( ",
                        df_final$Score_Percent,
                        " % )"
                  )
          )
        ),
        tags$tr( #Table Header
          tags$th( style = " text-align: center;
            vertical-align: middle; background-color:#50C878; color:white;", #background-color:#50C878 << Green
                   "No."),
          tags$th( style = " text-align: center;
            vertical-align: middle; background-color:#50C878; color:white;", 
                   "Question "),
          tags$th( style = "text-align: center;
            vertical-align: middle; background-color:#50C878; color:white;", 
                   "Score"),
          tags$th( style = " text-align: center;
            vertical-align: middle; background-color:#50C878; color:white;", 
                   "Comment")
        )
      ),
      tags$tbody(
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "4.1"),
                 tags$td(
                   "Is there a SOP for inventory control?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q1, "out of 1")),
                 tags$td(
                   df_final$C1)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "4.2"),
                 tags$td(
                   "Are SOPs in place and followed for receipt, inspection and storage of reagent/supplies?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q2, "out of 1")),
                 tags$td(
                   df_final$C2)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "4.3"),
                 tags$td(
                   "Are reagents/supplies labeled with the date received and initials?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q3, "out of 1")),
                 tags$td(
                   df_final$C3)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "4.4"),
                 tags$td(
                   "Are all reagents/supplies, currently in use, within their expiration period?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q4, "out of 1")),
                 tags$td(
                   df_final$C4)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "4.5"),
                 tags$td(
                   "Are SOPs for disposal of reagents and consumables in place and followed?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q5, "out of 1")),
                 tags$td(
                   df_final$C5)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "4.6"),
                 tags$td(
                   "Have all consumables/supplies been in stock during the past 6 months? If no or partial record number of stock outs in comment section."),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q6, "out of 1")),
                 tags$td(
                   df_final$C6)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "4.7"),
                 tags$td(
                   "Have all reagents been in stock during the past 6 months? If no or partial record the number of stock outs in comment section."),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q7, "out of 1")),
                 tags$td(
                   df_final$C7)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "4.8"),
                 tags$td(
                   "Are reagents/supplies appropriate for molecular testing (e.g. powder-free gloves, filtered tips, RNAse/DNAse-free)?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q8, "out of 1")),
                 tags$td(
                   df_final$C8)
        )
      ) # end of tbody
    ) # end of table 
    
  })    
  
  
  ##-----Section 5 HTP report-----
  output$sec5_report_H <- renderUI({
    
    df_final <- df_final()%>%
      filter(lab_name == input$facility )%>%
      filter(platform == "high_throughput")%>%
      group_by(lab_name) %>% 
      slice(which(assessment_date == max(assessment_date))) 
    
    df_final <- df_final%>%  group_by(assessment_date, lab_name, platform) %>% #,sub_platform
      summarise( 
        Audit_Score = sum(sec5_total),
        Max_Score = sum(sec5_max),
        Score_Percent = round(((sum(sec5_total)/sum(sec5_max))*100), digits = 0),
        Q1 = sum(Q5_1),
        Q2 = sum(Q5_2),
        Q3 = sum(Q5_3),
        Q4 = sum(Q5_4),
        Q5 = sum(Q5_5),
        Q6 = sum(Q5_6),
        Q7 = sum(Q5_7),
        Q8 = sum(Q5_8),
        
        
        C1 = C5_1,
        C2 = C5_2,
        C3 = C5_3,
        C4 = C5_4,
        C5 = C5_5,
        
        C6 = C5_6,
        C7 = C5_7,
        C8 = C5_8
        
      )
    
    
    table <-  tags$table(
      style = "width:100%; background-color:#d2e6b5;",
      border = 2,
      
      tags$thead(
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", "SECTION 5: SAMPLE MANAGEMENT(Yes = 1, Partial = 0.5, No = 0)")
        ),
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", 
                  paste("SECTION 5 Total Score: ",
                        df_final$Audit_Score,
                        "out of ",
                        df_final$Max_Score,
                        "( ",
                        df_final$Score_Percent,
                        " % )"
                  ))
        ),
        tags$tr(
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "No."),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Question "),
          tags$th( style = "text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Score"),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Comment")
        )
      ), # end of tags$thead
      
      
      tags$tbody(
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "5.1"),
                 tags$td(
                   "Are SOPs in place and followed for sample transport and processing in the laboratory?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q1, "out of 1")),
                 tags$td(
                   df_final$C1)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "5.2"),
                 tags$td(
                   "Does the laboratory highlight issues with sample processing/transport to implementing partner or referring facilities for remediation?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q2, "out of 1")),
                 tags$td(
                   df_final$C2)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "5.3"),
                 tags$td(
                   "Are SOPs in place and followed for evaluating sample acceptability upon receipt in the laboratory?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q3, "out of 1")),
                 tags$td(
                   df_final$C3)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "5.4"),
                 tags$td(
                   "Are requesters notified of rejected samples within 24 hours according to SOPs?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q4, "out of 1")),
                 tags$td(
                   df_final$C4)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "5.5"),
                 tags$td(
                   "Does a sample transport form accompany samples and does it account for chain of sample custody?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q5, "out of 1")),
                 tags$td(
                   df_final$C5)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "5.6"),
                 tags$td(
                   "Are sample transport time and conditions maintained according to assay requirements from collection until reception in laboratory?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q6, "out of 1")),
                 tags$td(
                   df_final$C6)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "5.7"),
                 tags$td(
                   "Is the monthly sample rejection rate <3%?
                     If NO, please note most common reason(s) for rejection in comments section, and do records indicate the appropriate implementing partner, sample hub, or referring facility was contacted to address the issue(s)?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q7, "out of 1")),
                 tags$td(
                   df_final$C7)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "5.8"),
                 tags$td(
                   "Are SOPs for sample storage written according to manufacturer’s requirements, in place and followed?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q8, "out of 1")),
                 tags$td(
                   df_final$C8)
        )
      ) # end of tbody
    ) # end of table 
    
  })    
  
  
  ##-----Section 6 HTP report-----
  output$sec6_report_H <- renderUI({
    
    df_final <- df_final()%>%
      filter(lab_name == input$facility )%>%
      filter(platform == "high_throughput")%>%
      group_by(lab_name) %>% 
      slice(which(assessment_date == max(assessment_date))) 
    
    df_final <- df_final%>%  group_by(assessment_date, lab_name, platform) %>% #,sub_platform
      summarise( 
        Audit_Score = sum(sec6_total),
        Max_Score = sum(sec6_max),
        Score_Percent = round(((sum(sec6_total)/sum(sec6_max))*100), digits = 0),
        Q1 = sum(Q6_1),
        Q2 = sum(Q6_2),
        Q3 = sum(Q6_3),
        Q4 = sum(Q6_4),
        Q5 = sum(Q6_5),
        
        
        C1 = C6_1,
        C2 = C6_2,
        C3 = C6_3,
        C4 = C6_4,
        C5 = C6_5
        
        
        
      )
    
    
    table <-  tags$table(
      style = "width:100%; background-color:#d2e6b5;",
      border = 2,
      
      tags$thead(
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", "SECTION 6: EQUIPMENT (Yes = 1, Partial = 0.5, No = 0)")
        ),
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", 
                  paste("SECTION 6 Total Score: ",
                        df_final$Audit_Score,
                        "out of ",
                        df_final$Max_Score,
                        "( ",
                        df_final$Score_Percent,
                        " % )"
                  ))
        ),
        tags$tr(
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "No."),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Question "),
          tags$th( style = "text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Score"),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Comment")
        )
      ), # end of tags$thead
      
      
      tags$tbody(
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "6.1"),
                 tags$td(
                   "Is all equipment, required for VL/IVT testing, present?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q1, "out of 1")),
                 tags$td(
                   df_final$C1)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "6.2"),
                 tags$td(
                   "Is all equipment, required for VL/IVT testing, functional?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q2, "out of 1")),
                 tags$td(
                   df_final$C2)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "6.3"),
                 tags$td(
                   "Do equipment records include documentation of routine preventive maintenance?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q3, "out of 1")),
                 tags$td(
                   df_final$C3)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "6.4"),
                 tags$td(
                   "Are equipment maintenance contracts in place?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q4, "out of 1")),
                 tags$td(
                   df_final$C4)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "6.5"),
                 tags$td(
                   "Are Instrument Manuals for all VL/IVT equipment available to laboratory personnel?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q5, "out of 1")),
                 tags$td(
                   df_final$C5)
        )
        
        
        
      ) # end of tbody
    ) # end of table 
    
  })  
  
  
  ##-----Section 7 HTP report-----
  output$sec7_report_H <- renderUI({
    
    df_final <- df_final()%>%
      filter(lab_name == input$facility )%>%
      filter(platform == "high_throughput")%>%
      group_by(lab_name) %>% 
      slice(which(assessment_date == max(assessment_date))) 
    
    df_final <- df_final%>%  group_by(assessment_date, lab_name, platform) %>% #,sub_platform
      summarise( 
        Audit_Score = sum(sec7_total),
        Max_Score = sum(sec7_max),
        Score_Percent = round(((sum(sec7_total)/sum(sec7_max))*100), digits = 0),
        Q1 = sum(Q7_1),
        Q2 = sum(Q7_2),
        Q3 = sum(Q7_3),
        Q4 = sum(Q7_4_H),
        Q5 = sum(Q7_5_H),
        Q6 = sum(Q7_6_H),
        Q7 = sum(Q7_4_G_Q7_7_H),
        Q8 = sum(Q7_5_G_Q7_8_H),
        Q9 = sum(Q7_6_G_Q7_9_H),
        Q10 = sum(Q7_7_G_Q7_10_H),
        Q11 = sum(Q7_8_G_Q7_11_H),
        Q12 = sum(Q7_9_G_Q7_12_H),
        Q13 = sum(Q7_10_G_Q7_13_H),
        
        C1 = C7_1,
        C2 = C7_2,
        C3 = C7_3,
        C4 = C7_4_H,
        C5 = C7_5_H,
        C6 = C7_6_H,
        C7 = C7_4_G_C7_7_H,
        C8 = C7_5_G_C7_8_H,
        C9 = C7_6_G_C7_9_H,
        C10 = C7_7_G_C7_10_H,
        C11 = C7_8_G_C7_11_H,
        C12 = C7_9_G_C7_12_H,
        C13 = C7_10_G_C7_13_H
      )
    
    
    table <-  tags$table(
      style = "width:100%; background-color:#d2e6b5;",
      border = 2,
      
      tags$thead(
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", "SECTION 7: PROCESS CONTROLS (Yes = 1, Partial = 0.5, No = 0)")
        ),
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", 
                  paste("SECTION 7 Total Score: ",
                        df_final$Audit_Score,
                        "out of ",
                        df_final$Max_Score,
                        "( ",
                        df_final$Score_Percent,
                        " % )"
                  ))
        ),
        tags$tr(
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "No."),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Question "),
          tags$th( style = "text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Score"),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Comment")
        )
      ), # end of tags$thead
      
      
      tags$tbody(
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.1"),
                 tags$td(
                   "Are VL/IVT testing job aids and/or SOPs available at the testing site?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q1, "out of 1")),
                 tags$td(
                   df_final$C1)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.2"),
                 tags$td(
                   "Do records indicate equipment performance was verified prior to beginning VL/IVT testing per SOP?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q2, "out of 1")),
                 tags$td(
                   df_final$C2)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.3"),
                 tags$td(
                   "Are SOPs in place and followed for running, recording, and reviewing quality control (QC) results?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q3, "out of 1")),
                 tags$td(
                   df_final$C3)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.4"),
                 tags$td(
                   "Are QC results properly recorded, including invalid and out-of-range results?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q4, "out of 1")),
                 tags$td(
                   df_final$C4)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.5"),
                 tags$td(
                   "Are appropriate steps taken and documented when QC results are out-of-range and/or invalid per SOP?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q5, "out of 1")),
                 tags$td(
                   df_final$C5)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.6"),
                 tags$td(
                   "Is there documented evidence of supervisor review of quality control records per SOP?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q6, "out of 1")),
                 tags$td(
                   df_final$C6)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.7"),
                 tags$td(
                   "Is the laboratory enrolled in Proficiency Testing (PT) for VL/IVT?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q7, "out of 1")),
                 tags$td(
                   df_final$C7)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.8"),
                 tags$td(
                   "In the past 12 months, has the laboratory passed all PT panels for VL?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q8, "out of 1")),
                 tags$td(
                   df_final$C8)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.9"),
                 tags$td(
                   "Is PT testing rotated among all VL/IVT testing staff?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q9, "out of 1")),
                 tags$td(
                   df_final$C9)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.10"),
                 tags$td(
                   "Are PT samples tested in the same manner as patient samples?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q10, "out of 1")),
                 tags$td(
                   df_final$C10)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.11"),
                 tags$td(
                   "Are there records of supervisor review of PT result prior to submission?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q11, "out of 1")),
                 tags$td(
                   df_final$C11)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.12"),
                 tags$td(
                   "Do records indicate that lab staff review PT result reports prior to submission?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q12, "out of 1")),
                 tags$td(
                   df_final$C12)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.13"),
                 tags$td(
                   "Do records indicate that lab staff conduct investigation and corrective action for any failed PT results?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q13, "out of 1")),
                 tags$td(
                   df_final$C13)
        )
        
        
        
        
      ) # end of tbody
    ) # end of table 
    
  })  
  
  
  ##-----Section 8 HTP report-----
  output$sec8_report_H <- renderUI({
    
    df_final <- df_final()%>%
      filter(lab_name == input$facility )%>%
      filter(platform == "high_throughput")%>%
      group_by(lab_name) %>% 
      slice(which(assessment_date == max(assessment_date))) 
    
    df_final <- df_final%>%  group_by(assessment_date, lab_name, platform) %>% #,sub_platform
      summarise( 
        Audit_Score = sum(sec8_total),
        Max_Score = sum(sec8_max),
        Score_Percent = round(((sum(sec8_total)/sum(sec8_max))*100), digits = 0),
        Q1 = sum(Q8_1),
        Q2 = sum(Q8_2),
        Q3 = sum(Q8_3),
        Q4 = sum(Q8_4_H),
        Q5 = sum(Q8_5_H),
        Q6 = sum(Q8_6),
        Q7 = sum(Q8_7),
        Q8 = sum(Q8_8),
        Q9 = sum(Q8_9),
        Q10 = sum(Q8_10),
        Q11 = sum(Q8_11),
        
        C1 = C8_1,
        C2 = C8_2,
        C3 = C8_3,
        C4 = C8_4,
        C5 = C8_5,
        C6 = C8_6,
        C7 = C8_7,
        C8 = C8_8,
        C9 = C8_9,
        C10 = C8_10,
        C11 = C8_11
        
      )
    
    
    table <-  tags$table(
      style = "width:100%; background-color:#d2e6b5;",
      border = 2,
      
      tags$thead(
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", "SECTION 8: M&E DOCUMENTS AND RECORDS - RESULT REPORTING ")
        ),
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", 
                  paste("SECTION 8 Total Score: ",
                        df_final$Audit_Score,
                        "out of ",
                        df_final$Max_Score,
                        "( ",
                        df_final$Score_Percent,
                        " % )"
                  ))
        ),
        tags$tr(
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "No."),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Question "),
          tags$th( style = "text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Score"),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Comment")
        )
      ), # end of tags$thead
      
      
      tags$tbody(
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.1"),
                 tags$td(
                   "Are the required data elements below recorded in the laboratory?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q1, "out of 1")),
                 tags$td(
                   df_final$C1)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.2"),
                 tags$td(
                   "Unique patient ID"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q2, "out of 1")),
                 tags$td(
                   df_final$C2)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.3"),
                 tags$td(
                   "Invalid Test Results"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q3, "out of 1")),
                 tags$td(
                   df_final$C3)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.4"),
                 tags$td(
                   "Are virally unsuppressed VL test results (≥1000 cp/ml) and positive IVT results identified at labs and reported as priority results to referring facilities? Please note in comments section how unsuppressed VL/positive IVT results are reported."),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q4, "out of 1")),
                 tags$td(
                   df_final$C4)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.5"),
                 tags$td(
                   "Are VL/IVT results returned from labs to clinic sites?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q5, "out of 1")),
                 tags$td(
                   df_final$C5)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.6"),
                 tags$td(
                   "Do lab records or documents indicate receipt of results at clinics? Please indicate how in the comments."),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q6, "out of 1")),
                 tags$td(
                   df_final$C6)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.7"),
                 tags$td(
                   "Are all client documents and records securely kept throughout all phases of the testing process in the lab?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q7, "out of 1")),
                 tags$td(
                   df_final$C7)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.8"),
                 tags$td(
                   "Are all lab registers or logbooks and other documents kept in a secure location when not in use? If applicable, does the LIMS prevent unauthorized access to patient results?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q8, "out of 1")),
                 tags$td(
                   df_final$C8)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.9"),
                 tags$td(
                   "Are registers or logbooks in the lab properly labeled and archived when full? If applicable, does the LIMS get routinely backed-up according to an SOP?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q9, "out of 1")),
                 tags$td(
                   df_final$C9)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.10"),
                 tags$td(
                   "Are records or documents stored in accordance with national/local record retention requirements?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q10, "out of 1")),
                 tags$td(
                   df_final$C10)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.11"),
                 tags$td(
                   "Is there a dashboard or tool for routine review of VL data in the LIS?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q11, "out of 1")),
                 tags$td(
                   df_final$C11)
        )
        

        
      ) # end of tbody
    ) # end of table 
    
  })  
  
  
  ##-----Section 9 HTP report-----
  output$sec9_report_H <- renderUI({
    
    df_final <- df_final()%>%
      filter(lab_name == input$facility )%>%
      filter(platform == "high_throughput")%>%
      group_by(lab_name) %>% 
      slice(which(assessment_date == max(assessment_date))) 
    
    df_final <- df_final%>%  group_by(assessment_date, lab_name, platform) %>% #,sub_platform
      summarise( 
        Audit_Score = sum(sec9_total),
        Max_Score = sum(sec9_max),
        Score_Percent = round(((sum(sec9_total)/sum(sec9_max))*100), digits = 0),
        Q1 = sum(Q9_1),
        Q2 = sum(Q9_2),
        Q3 = sum(Q9_3),
        Q4 = sum(Q9_4),
        Q5 = sum(Q9_5),
        Q6 = sum(Q9_6),
        Q7 = sum(Q9_7),
        Q8 = sum(Q9_8),
        
        C1 = C9_1,
        C2 = C9_2,
        C3 = C9_3,
        C4 = C9_4,
        C5 = C9_5,
        C6 = C9_6,
        C7 = C9_7,
        C8 = C9_8
      )
    
    
    table <-  tags$table(
      style = "width:100%; background-color:#d2e6b5;",
      border = 2,
      
      tags$thead(
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", "SECTION 9: INTERNAL QUALITY AUDIT - QUALITY INDICATORS - CONTINUAL IMPROVEMENT")
        ),
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", 
                  paste("SECTION 9 Total Score: ",
                        df_final$Audit_Score,
                        "out of ",
                        df_final$Max_Score,
                        "( ",
                        df_final$Score_Percent,
                        " % )"
                  ))
        ),
        tags$tr(
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "No."),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Question "),
          tags$th( style = "text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Score"),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Comment")
        )
      ), # end of tags$thead
      
      
      tags$tbody(
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "9.1"),
                 tags$td(
                   "Does the laboratory staff record non-conforming events associated with VL/IVT sample receiving, testing, reporting, and supply chain?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q1, "out of 1")),
                 tags$td(
                   df_final$C1)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "9.2"),
                 tags$td(
                   "Do records indicate management review of non- conforming events for trends?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q2, "out of 1")),
                 tags$td(
                   df_final$C2)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "9.3"),
                 tags$td(
                   "Do records indicate investigation of corrective action taken for non-conforming events?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q3, "out of 1")),
                 tags$td(
                   df_final$C3)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "9.4"),
                 tags$td(
                   "Does the laboratory have an internal audit SOP?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q4, "out of 1")),
                 tags$td(
                   df_final$C4)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "9.5"),
                 tags$td(
                   "Do records indicate internal audits are performed per SOP?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q5, "out of 1")),
                 tags$td(
                   df_final$C5)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "9.6"),
                 tags$td(
                   "Do records indicate corrective action is taken on audit findings?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q6, "out of 1")),
                 tags$td(
                   df_final$C6)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "9.7"),
                 tags$td(
                   "Does the laboratory identify and monitor quality indicators?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q7, "out of 1")),
                 tags$td(
                   df_final$C7)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "9.8"),
                 tags$td(
                   "Has the lab been recognized or accredited by any agency?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q8, "out of 1")),
                 tags$td(
                   df_final$C8)
        )
        
        
      ) # end of tbody
    ) # end of table 
    
  })  
  
  
  
  
  
  
  #----------SITE-LEVEL REPORT GXP------- 
    ##----------Facility Info GXP------------ 
  output$fac_info_G <- renderUI({
    
    site <- df_final()%>% filter(`lab_name` == input$facility)%>%
      filter(platform == "genexpert")%>%
      group_by(lab_name, assessment_date)%>%
      summarise( Lab_Name = lab_name,
                 State_Region = state_region,
                 Township = township,
                 Affiliation = affiliation,
                 Assessment_Date = format(assessment_date,"%d-%b-%y") ,
                 Start_Time = group_part1.start_time,
                 End_Time = last_page.end_time,
                 # Platform = sub_platform,
                 
                 Accessor_Name_1= accessor_name_1,
                 Accessor_Name_2= accessor_name_2,
                 First_Assessment = first_assessment,
                 Date_Last_Assessemnt = previous_assessment_date
                 
                 
      )
    site <- site %>% group_by(lab_name)%>%
      slice(which(assessment_date == max(assessment_date)))
    
    
    tags$table(
      style = "width:100%; background-color:#d2e6b5;",
      border = 2,
      
      tags$thead(
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#06326b; color:white;", "HIV Viral Load and Infant Virological Testing Scorecard: Site-level Report")
        ),
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#50C878; color:white ", "FACILITY INFORMATION")
        )
        
      ),
      
      tags$tbody(
        tags$tr(
          tags$td("Laboratory Name: "),
          tags$td(paste(site$Lab_Name)),
          tags$td("Affiliation: "),
          tags$td(paste(site$Affiliation))
        ),
        
        tags$tr( style = "background-color:#c1db9b;",
                 tags$td("State/Region: "),
                 tags$td(paste(site$State_Region)),
                 tags$td("Township: "),
                 tags$td(paste(site$Township))
        ),
        
        
        
        tags$tr( 
          tags$td("Date: "),
          tags$td(paste(site$Assessment_Date)),
          tags$td("Platform:  "),
          tags$td(paste(site$Platform))
          
          
        ),
        
        tags$tr( style = "background-color:#c1db9b;",
                 tags$td("Start Time: "),
                 tags$td(paste(site$Start_Time)),
                 tags$td("End Time: "),
                 tags$td(paste(site$End_Time))
                 
        ),
        
        tags$tr(
          tags$td("Accessor Name 1: "),
          tags$td(paste(site$Accessor_Name_1)),
          tags$td("Accessor Name 2: "),
          tags$td(paste(site$Accessor_Name_2))
        ),
        
        tags$tr(style = "background-color:#c1db9b;",
                tags$td("First Assessment ?: "),
                tags$td(paste(site$First_Assessment)),
                tags$td("If no: Date of last assessment: "),
                tags$td(paste(site$Date_Last_Assessemnt))
        )
        
        
        
      ) #end of table body
    ) # end of table
  
  }) 
  
  ##-----Section 1 GXP report-----
  
  output$sec1_report_G <- renderUI({
    
    df_final <- df_final()%>%
      filter(lab_name == input$facility )%>%
      filter(platform == "genexpert")%>%
      group_by(lab_name) %>% 
      slice(which(assessment_date == max(assessment_date))) 
    
    df_final <- df_final%>%  group_by(assessment_date, lab_name, platform) %>% #, sub_platform
      summarise( 
        Audit_Score = sum(sec1_total),
        Max_Score = sum(sec1_max),
        Score_Percent = round(((sum(sec1_total)/sum(sec1_max))*100), digits = 0),
        Q1 = sum(Q1_1),
        Q2 = sum(Q1_2),
        Q3 = sum(Q1_3),
        Q4 = sum(Q1_4),
        Q5 = sum(Q1_5),
        Q6 = sum(Q1_6),
        Q7 = sum(Q1_7),
        Q8 = sum(Q1_8),
        Q9 = sum(Q1_9),
        Q10 = sum(Q1_10),
        Q11 = sum(Q1_11),
        C1 = C1_1,
        C2 = C1_2,
        C3 = C1_3,
        C4 = C1_4,
        C5 = C1_5,
        C6 = C1_6,
        C7 = C1_7,
        C8 = C1_8,
        C9 = C1_9,
        C10 = C1_10,
        C11 = C1_11)
    
    
    table <-  tags$table(
      style = "width:100%; background-color:#d2e6b5;",
      border = 2,
      
      tags$thead(
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", "SECTION 1: PERSONNEL (Yes = 1, Partial = 0.5, No = 0, NA = 99)")
        ),
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", 
                  paste("SECTION 1 Total Score: ",
                        df_final$Audit_Score,
                        "out of ",
                        df_final$Max_Score,
                        "( ",
                        df_final$Score_Percent,
                        " % )"
                  ))
        ),
        tags$tr(
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "No."),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Question "),
          tags$th( style = "text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Score"),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Comment")
        )
      ), # end of tags$thead
      
      
      tags$tbody(
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "1.1"),
                 tags$td(
                   "Is the Viral Load (VL)/Infant Virological Testing (IVT) training program based on national policy?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q1, "out of 1")),
                 tags$td(
                   df_final$C1)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "1.2"),
                 tags$td(
                   "Have all laboratory personnel received comprehensive training on VL/IVT testing using approved Standard Operating Procedures (SOPs)?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q2, "out of 1")),
                 tags$td(
                   df_final$C2)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "1.3"),
                 tags$td(
                   "Are laboratory personnel trained on using standardized VL/IVT testing registers/log book/LIMS?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q3, "out of 1")),
                 tags$td(
                   df_final$C3)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "1.4"),
                 tags$td(
                   "Are laboratory personnel trained on sample management from collection to disposal?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q4, "out of 1")),
                 tags$td(
                   df_final$C4)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "1.5"),
                 tags$td(
                   "Are laboratory personnel trained on routine preventive equipment maintenance?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q5, "out of 1")),
                 tags$td(
                   df_final$C5)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "1.6"),
                 tags$td(
                   "Are laboratory personnel trained on the quality control process?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q6, "out of 1")),
                 tags$td(
                   df_final$C6)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "1.7"),
                 tags$td(
                   "Are laboratory personnel trained on safety and waste management procedures and practices?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q7, "out of 1")),
                 tags$td(
                   df_final$C7)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "1.8"),
                 tags$td(
                   "Are only trained/competent laboratory personnel allowed to perform VL/IVT testing?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q8, "out of 1")),
                 tags$td(
                   df_final$C8)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "1.9"),
                 tags$td(
                   "Are approved/signed records of all trainings for all laboratory personnel kept on file?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q9, "out of 1")),
                 tags$td(
                   df_final$C9)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "1.10"),
                 tags$td(
                   "Have all VL/IVT testing personnel received refresher training, according to the approved training program?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q10, "out of 1")),
                 tags$td(
                   df_final$C10)
        )
        
        
        
        
        
        
        
        
      ) # end of tbody
    ) # end of table 
    
  }) 
  
  
  
  ##-----Section 2 GXP report-----
  output$sec2_report_G <- renderUI({
    
    df_final <- df_final()%>%
      filter(lab_name == input$facility )%>%
      filter(platform == "genexpert")%>%
      group_by(lab_name) %>% 
      slice(which(assessment_date == max(assessment_date))) 
    
    df_final <- df_final%>%  group_by(assessment_date, lab_name, platform) %>% #, sub_platform
      summarise( 
        Audit_Score = sum(sec2_total),
        Max_Score = sum(sec2_max),
        Score_Percent = round(((sum(sec2_total)/sum(sec2_max))*100), digits = 0),
        Q1 = sum(Q2_1),
        Q2 = sum(Q2_2),
        Q3 = sum(Q2_3),
        Q4 = sum(Q2_4),
        Q5 = sum(Q2_5),
        Q6 = sum(Q2_6),
        Q7 = sum(Q2_7),
        Q8 = sum(Q2_8),
        Q9 = sum(Q2_9),
        Q10 = sum(Q2_10),
        Q11 = sum(Q2_11),
        Q12 = sum(Q2_12_G),
       
        C1 = C2_1,
        C2 = C2_2,
        C3 = C2_3,
        C4 = C2_4,
        C5 = C2_5,
        C6 = C2_6,
        C7 = C2_7,
        C8 = C2_8,
        C9 = C2_9,
        C10 = C2_10,
        C11 = C2_11,
        C12 = C2_12
        )
    
    
    table <-  tags$table(
      style = "width:100%; background-color:#d2e6b5;",
      border = 2,
      
      tags$thead(
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", "SECTION 2: PHYSICAL FACILITY/ENVIRONMENT (Yes = 1, Partial = 0.5, No = 0)")
        ),
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", 
                  paste("SECTION 2 Total Score: ",
                        df_final$Audit_Score,
                        "out of ",
                        df_final$Max_Score,
                        "( ",
                        df_final$Score_Percent,
                        " % )"
                  ))
        ),
        tags$tr(
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "No."),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Question "),
          tags$th( style = "text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Score"),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Comment")
        )
      ), # end of tags$thead
      
      
      tags$tbody(
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.1"),
                 tags$td(
                   "Is there a designated area exclusively for VL/IVT testing?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q1, "out of 1")),
                 tags$td(
                   df_final$C1)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.2"),
                 tags$td(
                   "Does testing area meet manufacturer’s requirements for equipment installation?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q2, "out of 1")),
                 tags$td(
                   df_final$C2)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.3"),
                 tags$td(
                   "Is the VL/IVT testing area clean, and organized?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q3, "out of 1")),
                 tags$td(
                   df_final$C3)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.4"),
                 tags$td(
                   "Are SOPs for cleaning work areas in place and followed?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q4, "out of 1")),
                 tags$td(
                   df_final$C4)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.5"),
                 tags$td(
                   "Are reagents/supplies kept in a temperature controlled environment according to manufacturer’s instructions?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q5, "out of 1")),
                 tags$td(
                   df_final$C5)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.6"),
                 tags$td(
                   "Are SOPs in place and followed for temperature monitoring?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q6, "out of 1")),
                 tags$td(
                   df_final$C6)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.7"),
                 tags$td(
                   "Are acceptable temperature ranges defined for temperature dependent equipment?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q7, "out of 1")),
                 tags$td(
                   df_final$C7)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.8"),
                 tags$td(
                   "Are temperatures recorded daily for?
                   Freezers,
                   Refrigerators,
                   Room temperature"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q8, "out of 1")),
                 tags$td(
                   df_final$C8)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.9"),
                 tags$td(
                   "Is there documentation of corrective action taken in response to out of range temperatures?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q9, "out of 1")),
                 tags$td(
                   df_final$C9)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.10"),
                 tags$td(
                   "Are UPS in place for testing equipment?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q10, "out of 1")),
                 tags$td(
                   df_final$C10)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.11"),
                 tags$td(
                   "Is there a functional back-up generator?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q11, "out of 1")),
                 tags$td(
                   df_final$C11)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "2.12"),
                 tags$td(
                   "Is there secure storage space for cartridges and consumables?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q12, "out of 1")),
                 tags$td(
                   df_final$C12)
        )
        
        
      
        
        
      ) # end of tbody
    ) # end of table 
    
  })   

  
  ##-----Section 3 GXP report-----
  output$sec3_report_G <- renderUI({
    
    df_final <- df_final()%>%
      filter(lab_name == input$facility )%>%
      filter(platform == "genexpert")%>%
      group_by(lab_name) %>% 
      slice(which(assessment_date == max(assessment_date))) 
    
    df_final <- df_final%>%  group_by(assessment_date, lab_name, platform) %>% #, sub_platform
      summarise( 
        Audit_Score = sum(sec3_total),
        Max_Score = sum(sec3_max),
        Score_Percent = round(((sum(sec3_total)/sum(sec3_max))*100), digits = 0),
        Q1 = sum(Q3_1),
        Q2 = sum(Q3_2),
        Q3 = sum(Q3_3),
        Q4 = sum(Q3_4),
        Q5 = sum(Q3_5),
        Q6 = sum(Q3_6),
        Q7 = sum(Q3_7),
        Q8 = sum(Q3_8),
        Q9 = sum(Q3_9),
        Q10 = sum(Q3_10),
       
        
        C1 = C2_1,
        C2 = C2_2,
        C3 = C2_3,
        C4 = C2_4,
        C5 = C2_5,
        C6 = C2_6,
        C7 = C2_7,
        C8 = C2_8,
        C9 = C2_9,
        C10 = C2_10
        
      )
    
    
    table <-  tags$table(
      style = "width:100%; background-color:#d2e6b5;",
      border = 2,
      
      tags$thead(
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", "SECTION 3: SAFETY/ WASTE MANAGEMENT(Yes = 1, Partial = 0.5, No = 0)")
        ),
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", 
                  paste("SECTION 3 Total Score: ",
                        df_final$Audit_Score,
                        "out of ",
                        df_final$Max_Score,
                        "( ",
                        df_final$Score_Percent,
                        " % )"
                  ))
        ),
        tags$tr(
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "No."),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Question "),
          tags$th( style = "text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Score"),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Comment")
        )
      ), # end of tags$thead
      
      
      tags$tbody(
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.1"),
                 tags$td(
                   "Are SOPs in place and followed for personnel safety practices?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q1, "out of 1")),
                 tags$td(
                   df_final$C1)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.2"),
                 tags$td(
                   "Are SOPs in place and followed for disposal of infectious and non-infectious waste?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q2, "out of 1")),
                 tags$td(
                   df_final$C2)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.3"),
                 tags$td(
                   "Are SOPs in place and followed to manage biohazardous spills, e.g. blood?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q3, "out of 1")),
                 tags$td(
                   df_final$C3)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.4"),
                 tags$td(
                   "Are SOPs in place and followed to address accidental exposure to potentially infectious body fluids through needle-stick injury, splash or other sharps injury?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q4, "out of 1")),
                 tags$td(
                   df_final$C4)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.5"),
                 tags$td(
                   "Is personnel protective equipment (PPE) always available to the VL/IVT testing personnel?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q5, "out of 1")),
                 tags$td(
                   df_final$C5)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.6"),
                 tags$td(
                   "Do all laboratory personnel properly use PPE throughout the VL/IVT testing process?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q6, "out of 1")),
                 tags$td(
                   df_final$C6)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.7"),
                 tags$td(
                   "Are clean water and soap available for hand washing?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q7, "out of 1")),
                 tags$td(
                   df_final$C7)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.8"),
                 tags$td(
                   "Is an appropriate disinfectant available to clean the work area and equipment?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q8, "out of 1")),
                 tags$td(
                   df_final$C8)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.9"),
                 tags$td(
                   "Are sharps, infectious and non-infectious waste handled properly?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q9, "out of 1")),
                 tags$td(
                   df_final$C9)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "3.10"),
                 tags$td(
                   "Are containers for infectious and non-infectious waste emptied regularly in accordance with SOPs?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q10, "out of 1")),
                 tags$td(
                   df_final$C10)
        )
        
        
        
        
      ) # end of tbody
    ) # end of table   
  })
  
  
  
  ##-----Section 4 GXP report-----
  output$sec4_report_G <- renderUI({
    
    df_final <- df_final()%>%
      filter(lab_name == input$facility )%>%
      filter(platform == "genexpert")%>%
      group_by(lab_name) %>%
      slice(which(assessment_date == max(assessment_date)))
    
    df_final <- df_final%>%  group_by(assessment_date, lab_name, platform) %>% #,sub_platform
      summarise( 
        Audit_Score = sum(sec4_total),
        Max_Score = sum(sec4_max),
        Score_Percent = round(((sum(sec4_total)/sum(sec4_max))*100), digits = 0),
        
        # Who_Decide = paste(G4_Who_Decide, collapse = ", "),
        # Who_Decide_other = paste(G4_Who_Decide_other, collapse = ", "),
        # wh_Qty = paste(G4_Wh_Qty_base, collapse = ", "),
        # wh_Qty_other = paste(G4_Wh_Qty_base_other, collapse = ", "),
        # How_often = paste(G4_How_often, collapse = ", "),
        # Comments = paste(G4_Comments, collapse = ", "),
        #print(wh_Qty),
        Who_Decide = G4_Who_Decide,
        Who_Decide_other = G4_Who_Decide_other,
        wh_Qty = G4_Wh_Qty_base,
        wh_Qty_other = G4_Wh_Qty_base_other,
        How_often = G4_How_often,
        Comments = G4_Comments,
        
        Q1 = sum(Q4_1),
        Q2 = sum(Q4_2),
        Q3 = sum(Q4_3),
        Q4 = sum(Q4_4),
        Q5 = sum(Q4_5),
        Q6 = sum(Q4_6_G),
        
        # Q7 = sum(Q4_7),
        # Q8 = sum(Q4_8),
        
        C1 = C4_1,
        C2 = C4_2,
        C3 = C4_3,
        C4 = C4_4,
        C5 = C4_5,
        
        C6 = C2_6,
        # C7 = C2_7,
        # C8 = C2_8
      )
    
    table <-  tags$table(
      style = "width:100%; background-color:#d2e6b5;",
      border = 2,
      
      tags$thead(
        tags$tr( #Section Level1 Header
          tags$th(colspan = 4, style = "width:50%; text-align: center;
            vertical-align: middle; background-color:#002D62; color:white;", "SECTION 4: PROCUREMENT AND INVENTORY(Yes = 1, Partial = 0.5, No = 0)")
        ),
      ), # end of tags$thead
      
      tags$tbody(
        tags$tr(
          tags$td("Who decides/quantities lab reagents/supplies to be procured: "),
          tags$td(paste(df_final$Who_Decide)),
          tags$td("Other, specify_: "),
          tags$td(paste(df_final$Who_Decide_other)),
              console.log("Who decides/quantities lab reagents/supplies to be procured: ",df_final$Who_Decide)
        ),
        tags$tr( style = "background-color:#c1db9b;",
                 tags$td("What is the quantification based on: "),
                 tags$td(paste(df_final$Wh_Qty)),
                 tags$td("Other, specify__: "),
                    console.log("What is the quantification based on: ", wh_Qty),
                 tags$td(paste(df_final$Wh_Qty_other))
        ),
        tags$tr(
          tags$td("How often are reagents/supplies for VL/IVT ordered: "),
          tags$td(paste(df_final$How_often)),
          tags$td("Comments: "),
          tags$td(paste(df_final$Comments))
        ),
      ), # end of tbody
      
      ##Section4 Table2
      tags$thead(
        tags$tr( #Section Level2 Header
          tags$th(colspan = 4, style = "width:100%; text-align: center;
            vertical-align: middle; background-color:#002D62; color:white;", #background-color:#002D62 << Dark-Blue
                  paste("SECTION 4 Total Score: ",
                        df_final$Audit_Score,
                        "out of ",
                        df_final$Max_Score,
                        "( ",
                        df_final$Score_Percent,
                        " % )"
                  )
          )
        ),
        tags$tr( #Table Header
          tags$th( style = " text-align: center;
            vertical-align: middle; background-color:#50C878; color:white;", #background-color:#50C878 << Green
                   "No."),
          tags$th( style = " text-align: center;
            vertical-align: middle; background-color:#50C878; color:white;", 
                   "Question "),
          tags$th( style = "text-align: center;
            vertical-align: middle; background-color:#50C878; color:white;", 
                   "Score"),
          tags$th( style = " text-align: center;
            vertical-align: middle; background-color:#50C878; color:white;", 
                   "Comment")
        )
      ),
      tags$tbody(
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "4.1"),
                 tags$td(
                   "Is there a SOP for inventory control?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q1, "out of 1")),
                 tags$td(
                   df_final$C1)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "4.2"),
                 tags$td(
                   "Are SOPs in place and followed for receipt, inspection and storage of reagent/supplies?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q2, "out of 1")),
                 tags$td(
                   df_final$C2)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "4.3"),
                 tags$td(
                   "Are reagents/supplies labeled with the date received and initials?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q3, "out of 1")),
                 tags$td(
                   df_final$C3)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "4.4"),
                 tags$td(
                   "Are all reagents/supplies, currently in use, within their expiration period?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q4, "out of 1")),
                 tags$td(
                   df_final$C4)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "4.5"),
                 tags$td(
                   "Are SOPs for disposal of reagents and consumables in place and followed?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q5, "out of 1")),
                 tags$td(
                   df_final$C5)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "4.6"),
                 tags$td(
                   "Have all consumables/supplies been in stock during the past 6 months? If no or partial record number of stock outs in comment section."),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q6, "out of 1")),
                 tags$td(
                   df_final$C6)
        ),
      ) # end of tbody
    ) # end of table 
    
  })
  # output$sec4_report_G <- renderUI({
  #   
  #   df_final <- df_final()%>%
  #     filter(lab_name == input$facility )%>%
  #     filter(platform == "genexpert")%>%
  #     group_by(lab_name) %>% 
  #     slice(which(assessment_date == max(assessment_date))) 
  #   
  #   df_final <- df_final%>%  group_by(assessment_date, lab_name, platform) %>% #, sub_platform
  #     summarise( 
  #       Audit_Score = sum(sec4_total),
  #       Max_Score = sum(sec4_max),
  #       Score_Percent = round(((sum(sec4_total)/sum(sec4_max))*100), digits = 0),
  #       Q1 = sum(Q4_1),
  #       Q2 = sum(Q4_2),
  #       Q3 = sum(Q4_3),
  #       Q4 = sum(Q4_4),
  #       Q5 = sum(Q4_5),
  #       Q6 = sum(Q4_6_G),
  #       
  #       
  #       C1 = C4_1,
  #       C2 = C4_2,
  #       C3 = C4_3,
  #       C4 = C4_4,
  #       C5 = C4_5,
  #       
  #       C6 = C2_6
  #      
  #     )
  #   
  #   
  #   table <-  tags$table(
  #     style = "width:100%; background-color:#d2e6b5;",
  #     border = 2,
  #     
  #     tags$thead(
  #       tags$tr(
  #         tags$th(colspan = 4, style = "width:100%; text-align: center;
  # vertical-align: middle; background-color:#002D62; color:white;", "SECTION 4: PROCUREMENT AND INVENTORY(Yes = 1, Partial = 0.5, No = 0)")
  #       ),
  #       tags$tr(
  #         tags$th(colspan = 4, style = "width:100%; text-align: center;
  # vertical-align: middle; background-color:#002D62; color:white;", 
  #                 paste("SECTION 4 Total Score: ",
  #                       df_final$Audit_Score,
  #                       "out of ",
  #                       df_final$Max_Score,
  #                       "( ",
  #                       df_final$Score_Percent,
  #                       " % )"
  #                 ))
  #       ),
  #       tags$tr(
  #         tags$th( style = " text-align: center;
  # vertical-align: middle; background-color:#50C878; color:white;", 
  #                  "No."),
  #         tags$th( style = " text-align: center;
  # vertical-align: middle; background-color:#50C878; color:white;", 
  #                  "Question "),
  #         tags$th( style = "text-align: center;
  # vertical-align: middle; background-color:#50C878; color:white;", 
  #                  "Score"),
  #         tags$th( style = " text-align: center;
  # vertical-align: middle; background-color:#50C878; color:white;", 
  #                  "Comment")
  #       )
  #     ), # end of tags$thead
  #     
  #     
  #     tags$tbody(
  #       tags$tr( style = "border-top: thin solid;",
  #                tags$td(style = "background-color:#c1db9b; ",
  #                        "4.1"),
  #                tags$td(
  #                  "Is there a SOP for inventory control?"),
  #                tags$td(style = "background-color:#c1db9b; ",
  #                        paste(df_final$Q1, "out of 1")),
  #                tags$td(
  #                  df_final$C1)
  #       ),
  #       
  #       tags$tr( style = "border-top: thin solid;",
  #                tags$td(style = "background-color:#c1db9b; ",
  #                        "4.2"),
  #                tags$td(
  #                  "Are SOPs in place and followed for receipt, inspection and storage of reagent/supplies?"),
  #                tags$td(style = "background-color:#c1db9b; ",
  #                        paste(df_final$Q2, "out of 1")),
  #                tags$td(
  #                  df_final$C2)
  #       ),
  #       
  #       tags$tr( style = "border-top: thin solid;",
  #                tags$td(style = "background-color:#c1db9b; ",
  #                        "4.3"),
  #                tags$td(
  #                  "Are reagents/supplies labeled with the date received and initials?"),
  #                tags$td(style = "background-color:#c1db9b; ",
  #                        paste(df_final$Q3, "out of 1")),
  #                tags$td(
  #                  df_final$C3)
  #       ),
  #       
  #       tags$tr( style = "border-top: thin solid;",
  #                tags$td(style = "background-color:#c1db9b; ",
  #                        "4.4"),
  #                tags$td(
  #                  "Are all reagents/supplies, currently in use, within their expiration period?"),
  #                tags$td(style = "background-color:#c1db9b; ",
  #                        paste(df_final$Q4, "out of 1")),
  #                tags$td(
  #                  df_final$C4)
  #       ),
  #       
  #       tags$tr( style = "border-top: thin solid;",
  #                tags$td(style = "background-color:#c1db9b; ",
  #                        "4.5"),
  #                tags$td(
  #                  "Are SOPs for disposal of reagents and consumables in place and followed?"),
  #                tags$td(style = "background-color:#c1db9b; ",
  #                        paste(df_final$Q5, "out of 1")),
  #                tags$td(
  #                  df_final$C5)
  #       ),
  #       
  #       
  #       tags$tr( style = "border-top: thin solid;",
  #                tags$td(style = "background-color:#c1db9b; ",
  #                        "4.6"),
  #                tags$td(
  #                  "Have all cartridges/consumables/supplies been in stock during the past 6 months? If no or partial record number of stock outs in comment section."),
  #                tags$td(style = "background-color:#c1db9b; ",
  #                        paste(df_final$Q6, "out of 1")),
  #                tags$td(
  #                  df_final$C6)
  #       )
  #       
  #       
  #     ) # end of tbody
  #   ) # end of table 
  #   
  # })  
  
  
  ##-----Section 5 GXP report-----
  output$sec5_report_G <- renderUI({
    
    df_final <- df_final()%>%
      filter(lab_name == input$facility )%>%
      filter(platform == "genexpert")%>%
      group_by(lab_name) %>% 
      slice(which(assessment_date == max(assessment_date))) 
    
    df_final <- df_final%>%  group_by(assessment_date, lab_name, platform) %>% #, sub_platform
      summarise( 
        Audit_Score = sum(sec5_total),
        Max_Score = sum(sec5_max),
        Score_Percent = round(((sum(sec5_total)/sum(sec5_max))*100), digits = 0),
        Q1 = sum(Q5_1),
        Q2 = sum(Q5_2),
        Q3 = sum(Q5_3),
        Q4 = sum(Q5_4),
        Q5 = sum(Q5_5),
        Q6 = sum(Q5_6),
        Q7 = sum(Q5_7),
        Q8 = sum(Q5_8),
        
        
        C1 = C5_1,
        C2 = C5_2,
        C3 = C5_3,
        C4 = C5_4,
        C5 = C5_5,
        
        C6 = C5_6,
        C7 = C5_7,
        C8 = C5_8
        
      )
    
    
    table <-  tags$table(
      style = "width:100%; background-color:#d2e6b5;",
      border = 2,
      
      tags$thead(
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", "SECTION 5: SAMPLE MANAGEMENT(Yes = 1, Partial = 0.5, No = 0)")
        ),
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", 
                  paste("SECTION 5 Total Score: ",
                        df_final$Audit_Score,
                        "out of ",
                        df_final$Max_Score,
                        "( ",
                        df_final$Score_Percent,
                        " % )"
                  ))
        ),
        tags$tr(
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "No."),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Question "),
          tags$th( style = "text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Score"),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Comment")
        )
      ), # end of tags$thead
      
      
      tags$tbody(
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "5.1"),
                 tags$td(
                   "Are SOPs in place and followed for sample transport and processing in the laboratory?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q1, "out of 1")),
                 tags$td(
                   df_final$C1)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "5.2"),
                 tags$td(
                   "Does the laboratory highlight issues with sample processing/transport to implementing partner or referring facilities for remediation?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q2, "out of 1")),
                 tags$td(
                   df_final$C2)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "5.3"),
                 tags$td(
                   "Are SOPs in place and followed for evaluating sample acceptability upon receipt in the laboratory?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q3, "out of 1")),
                 tags$td(
                   df_final$C3)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "5.4"),
                 tags$td(
                   "Are requesters notified of rejected samples within 24 hours according to SOPs?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q4, "out of 1")),
                 tags$td(
                   df_final$C4)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "5.5"),
                 tags$td(
                   "Does a sample transport form accompany samples and does it account for chain of sample custody?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q5, "out of 1")),
                 tags$td(
                   df_final$C5)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "5.6"),
                 tags$td(
                   "Are sample transport time and conditions maintained according to assay requirements from collection until reception in laboratory?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q6, "out of 1")),
                 tags$td(
                   df_final$C6)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "5.7"),
                 tags$td(
                   "Is the monthly sample rejection rate <3%?
                     If NO, please note most common reason(s) for rejection in comments section, and do records indicate the appropriate implementing partner, sample hub, or referring facility was contacted to address the issue(s)?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q7, "out of 1")),
                 tags$td(
                   df_final$C7)
        )
        
        
       
      ) # end of tbody
    ) # end of table 
    
  })    
 
  
  ##-----Section 6 GXP report-----
  output$sec6_report_G <- renderUI({
    
    df_final <- df_final()%>%
      filter(lab_name == input$facility )%>%
      filter(platform == "genexpert")%>%
      group_by(lab_name) %>% 
      slice(which(assessment_date == max(assessment_date))) 
    
    df_final <- df_final%>%  group_by(assessment_date, lab_name, platform) %>% #, sub_platform
      summarise( 
        Audit_Score = sum(sec6_total),
        Max_Score = sum(sec6_max),
        Score_Percent = round(((sum(sec6_total)/sum(sec6_max))*100), digits = 0),
        Q1 = sum(Q6_1),
        Q2 = sum(Q6_2),
        Q3 = sum(Q6_3),
        Q4 = sum(Q6_4),
        Q5 = sum(Q6_5),
        
        
        C1 = C6_1,
        C2 = C6_2,
        C3 = C6_3,
        C4 = C6_4,
        C5 = C6_5
        
        
        
      )
    
    
    table <-  tags$table(
      style = "width:100%; background-color:#d2e6b5;",
      border = 2,
      
      tags$thead(
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", "SECTION 6: EQUIPMENT (Yes = 1, Partial = 0.5, No = 0)")
        ),
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", 
                  paste("SECTION 6 Total Score: ",
                        df_final$Audit_Score,
                        "out of ",
                        df_final$Max_Score,
                        "( ",
                        df_final$Score_Percent,
                        " % )"
                  ))
        ),
        tags$tr(
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "No."),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Question "),
          tags$th( style = "text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Score"),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Comment")
        )
      ), # end of tags$thead
      
      
      tags$tbody(
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "6.1"),
                 tags$td(
                   "Is all equipment, required for VL/IVT testing, present?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q1, "out of 1")),
                 tags$td(
                   df_final$C1)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "6.2"),
                 tags$td(
                   "Is all equipment, required for VL/IVT testing, functional?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q2, "out of 1")),
                 tags$td(
                   df_final$C2)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "6.3"),
                 tags$td(
                   "Do equipment records include documentation of routine preventive maintenance?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q3, "out of 1")),
                 tags$td(
                   df_final$C3)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "6.4"),
                 tags$td(
                   "Are equipment maintenance contracts in place?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q4, "out of 1")),
                 tags$td(
                   df_final$C4)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "6.5"),
                 tags$td(
                   "Are Instrument Manuals for all VL/IVT equipment available to laboratory personnel?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q5, "out of 1")),
                 tags$td(
                   df_final$C5)
        )
        
        
        
      ) # end of tbody
    ) # end of table 
    
  })  
  
  
  
  ##-----Section 7 GXP report-----
  output$sec7_report_G <- renderUI({
    
    df_final <- df_final()%>%
      filter(lab_name == input$facility )%>%
      filter(platform == "genexpert")%>%
      group_by(lab_name) %>% 
      slice(which(assessment_date == max(assessment_date))) 
    
    df_final <- df_final%>%  group_by(assessment_date, lab_name, platform) %>% #, sub_platform
      summarise( 
        Audit_Score = sum(sec7_total),
        Max_Score = sum(sec7_max),
        Score_Percent = round(((sum(sec7_total)/sum(sec7_max))*100), digits = 0),
        Q1 = sum(Q7_1),
        Q2 = sum(Q7_2),
        Q3 = sum(Q7_3),
        Q4 = sum(Q7_4_G_Q7_7_H),
        Q5 = sum(Q7_5_G_Q7_8_H),
        Q6 = sum(Q7_6_G_Q7_9_H),
        Q7 = sum(Q7_7_G_Q7_10_H),
        Q8 = sum(Q7_8_G_Q7_11_H),
        Q9 = sum(Q7_9_G_Q7_12_H),
        Q10 = sum(Q7_10_G_Q7_13_H),
        
        
        C1 = C7_1,
        C2 = C7_2,
        C3 = C7_3,
        C4 = C7_4_G_C7_7_H,
        C5 = C7_5_G_C7_8_H,
        C6 = C7_6_G_C7_9_H,
        C7 = C7_7_G_C7_10_H,
        C8 = C7_8_G_C7_11_H,
        C9 = C7_9_G_C7_12_H,
        C10 = C7_10_G_C7_13_H
        
      )
    
    
    table <-  tags$table(
      style = "width:100%; background-color:#d2e6b5;",
      border = 2,
      
      tags$thead(
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", "SECTION 7: PROCESS CONTROLS (Yes = 1, Partial = 0.5, No = 0)")
        ),
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", 
                  paste("SECTION 7 Total Score: ",
                        df_final$Audit_Score,
                        "out of ",
                        df_final$Max_Score,
                        "( ",
                        df_final$Score_Percent,
                        " % )"
                  ))
        ),
        tags$tr(
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "No."),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Question "),
          tags$th( style = "text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Score"),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Comment")
        )
      ), # end of tags$thead
      
      
      tags$tbody(
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.1"),
                 tags$td(
                   "Are VL/IVT testing job aids and/or SOPs available at the testing site?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q1, "out of 1")),
                 tags$td(
                   df_final$C1)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.2"),
                 tags$td(
                   "Do records indicate equipment performance was verified prior to beginning VL/IVT testing per SOP?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q2, "out of 1")),
                 tags$td(
                   df_final$C2)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.3"),
                 tags$td(
                   "Are SOPs in place and followed for running, recording, and reviewing quality control (QC) results?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q3, "out of 1")),
                 tags$td(
                   df_final$C3)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.4"),
                 tags$td(
                   "Is the laboratory enrolled in Proficiency Testing (PT) for VL/IVT?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q4, "out of 1")),
                 tags$td(
                   df_final$C4)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.5"),
                 tags$td(
                   "In the past 12 months, has the laboratory passed all PT panels for VL?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q5, "out of 1")),
                 tags$td(
                   df_final$C5)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.6"),
                 tags$td(
                   "Is PT testing rotated among all VL/IVT testing staff?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q6, "out of 1")),
                 tags$td(
                   df_final$C6)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.7"),
                 tags$td(
                   "Are PT samples tested in the same manner as patient samples?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q7, "out of 1")),
                 tags$td(
                   df_final$C7)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.8"),
                 tags$td(
                   "Are there records of supervisor review of PT result prior to submission?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q8, "out of 1")),
                 tags$td(
                   df_final$C8)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.9"),
                 tags$td(
                   "Do records indicate that lab staff review PT result reports prior to submission?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q9, "out of 1")),
                 tags$td(
                   df_final$C9)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "7.10"),
                 tags$td(
                   "Do records indicate that lab staff conduct investigation and corrective action for any failed PT results?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q10, "out of 1")),
                 tags$td(
                   df_final$C10)
        )
        
        
      
        
        
        
        
      ) # end of tbody
    ) # end of table 
    
  })  
  
  
  
  ##-----Section 8 GXP report-----
  output$sec8_report_G <- renderUI({
    
    df_final <- df_final()%>%
      filter(lab_name == input$facility )%>%
      filter(platform == "genexpert")%>%
      group_by(lab_name) %>% 
      slice(which(assessment_date == max(assessment_date))) 
    
    df_final <- df_final%>%  group_by(assessment_date, lab_name, platform) %>% #, sub_platform
      summarise( 
        Audit_Score = sum(sec8_total),
        Max_Score = sum(sec8_max),
        Score_Percent = round(((sum(sec8_total)/sum(sec8_max))*100), digits = 0),
        Q1 = sum(Q8_1),
        Q2 = sum(Q8_2),
        Q3 = sum(Q8_3),
        Q4 = sum(Q8_4_G),
        Q5 = sum(Q8_5_G),
        Q6 = sum(Q8_6),
        Q7 = sum(Q8_7),
        Q8 = sum(Q8_8),
        Q9 = sum(Q8_9),
        Q10 = sum(Q8_10),
        Q11 = sum(Q8_11),
        
        C1 = C8_1,
        C2 = C8_2,
        C3 = C8_3,
        C4 = C8_4,
        C5 = C8_5,
        C6 = C8_6,
        C7 = C8_7,
        C8 = C8_8,
        C9 = C8_9,
        C10 = C8_10,
        C11 = C8_11
        
      )
    
    
    table <-  tags$table(
      style = "width:100%; background-color:#d2e6b5;",
      border = 2,
      
      tags$thead(
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", "SECTION 8: M&E DOCUMENTS AND RECORDS - RESULT REPORTING ")
        ),
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", 
                  paste("SECTION 8 Total Score: ",
                        df_final$Audit_Score,
                        "out of ",
                        df_final$Max_Score,
                        "( ",
                        df_final$Score_Percent,
                        " % )"
                  ))
        ),
        tags$tr(
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "No."),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Question "),
          tags$th( style = "text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Score"),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Comment")
        )
      ), # end of tags$thead
      
      
      tags$tbody(
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.1"),
                 tags$td(
                   "Are the required data elements below recorded in the laboratory?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q1, "out of 1")),
                 tags$td(
                   df_final$C1)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.2"),
                 tags$td(
                   "Unique patient ID"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q2, "out of 1")),
                 tags$td(
                   df_final$C2)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.3"),
                 tags$td(
                   "Invalid Test Results"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q3, "out of 1")),
                 tags$td(
                   df_final$C3)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.4"),
                 tags$td(
                   "Are virally unsuppressed VL test results (≥1000 cp/ml) and positive IVT results identified at labs and reported as priority results to referring facilities? Please note in comments section how unsuppressed VL/positive IVT results are reported."),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q4, "out of 1")),
                 tags$td(
                   df_final$C4)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.5"),
                 tags$td(
                   "Are VL/IVT results returned from labs to clinic sites?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q5, "out of 1")),
                 tags$td(
                   df_final$C5)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.6"),
                 tags$td(
                   "Do lab records or documents indicate receipt of results at clinics? Please indicate how in the comments."),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q6, "out of 1")),
                 tags$td(
                   df_final$C6)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.7"),
                 tags$td(
                   "Are all client documents and records securely kept throughout all phases of the testing process in the lab?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q7, "out of 1")),
                 tags$td(
                   df_final$C7)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.8"),
                 tags$td(
                   "Are all lab registers or logbooks and other documents kept in a secure location when not in use? If applicable, does the LIMS prevent unauthorized access to patient results?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q8, "out of 1")),
                 tags$td(
                   df_final$C8)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.9"),
                 tags$td(
                   "Are registers or logbooks in the lab properly labeled and archived when full? If applicable, does the LIMS get routinely backed-up according to an SOP?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q9, "out of 1")),
                 tags$td(
                   df_final$C9)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.10"),
                 tags$td(
                   "Are records or documents stored in accordance with national/local record retention requirements?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q10, "out of 1")),
                 tags$td(
                   df_final$C10)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "8.11"),
                 tags$td(
                   "Is there a dashboard or tool for routine review of VL data in the LIS?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q11, "out of 1")),
                 tags$td(
                   df_final$C11)
        )
        
        
        
      ) # end of tbody
    ) # end of table 
    
  })  
  
  
  
  ##-----Section 9 GXP report-----
  output$sec9_report_G <- renderUI({
    
    df_final <- df_final()%>%
      filter(lab_name == input$facility )%>%
      filter(platform == "genexpert")%>%
      group_by(lab_name) %>% 
      slice(which(assessment_date == max(assessment_date))) 
    
    df_final <- df_final%>%  group_by(assessment_date, lab_name, platform) %>% #, sub_platform
      summarise( 
        Audit_Score = sum(sec9_total),
        Max_Score = sum(sec9_max),
        Score_Percent = round(((sum(sec9_total)/sum(sec9_max))*100), digits = 0),
        Q1 = sum(Q9_1),
        Q2 = sum(Q9_2),
        Q3 = sum(Q9_3),
        Q4 = sum(Q9_4),
        Q5 = sum(Q9_5),
        Q6 = sum(Q9_6),
        Q7 = sum(Q9_7),
        Q8 = sum(Q9_8),
        
        C1 = C9_1,
        C2 = C9_2,
        C3 = C9_3,
        C4 = C9_4,
        C5 = C9_5,
        C6 = C9_6,
        C7 = C9_7,
        C8 = C9_8
      )
    
    
    table <-  tags$table(
      style = "width:100%; background-color:#d2e6b5;",
      border = 2,
      
      tags$thead(
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", "SECTION 9: INTERNAL QUALITY AUDIT - QUALITY INDICATORS - CONTINUAL IMPROVEMENT")
        ),
        tags$tr(
          tags$th(colspan = 4, style = "width:100%; text-align: center;
  vertical-align: middle; background-color:#002D62; color:white;", 
                  paste("SECTION 9 Total Score: ",
                        df_final$Audit_Score,
                        "out of ",
                        df_final$Max_Score,
                        "( ",
                        df_final$Score_Percent,
                        " % )"
                  ))
        ),
        tags$tr(
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "No."),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Question "),
          tags$th( style = "text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Score"),
          tags$th( style = " text-align: center;
  vertical-align: middle; background-color:#50C878; color:white;", 
                   "Comment")
        )
      ), # end of tags$thead
      
      
      tags$tbody(
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "9.1"),
                 tags$td(
                   "Does the laboratory staff record non-conforming events associated with VL/IVT sample receiving, testing, reporting, and supply chain?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q1, "out of 1")),
                 tags$td(
                   df_final$C1)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "9.2"),
                 tags$td(
                   "Do records indicate management review of non- conforming events for trends?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q2, "out of 1")),
                 tags$td(
                   df_final$C2)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "9.3"),
                 tags$td(
                   "Do records indicate investigation of corrective action taken for non-conforming events?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q3, "out of 1")),
                 tags$td(
                   df_final$C3)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "9.4"),
                 tags$td(
                   "Does the laboratory have an internal audit SOP?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q4, "out of 1")),
                 tags$td(
                   df_final$C4)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "9.5"),
                 tags$td(
                   "Do records indicate internal audits are performed per SOP?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q5, "out of 1")),
                 tags$td(
                   df_final$C5)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "9.6"),
                 tags$td(
                   "Do records indicate corrective action is taken on audit findings?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q6, "out of 1")),
                 tags$td(
                   df_final$C6)
        ),
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "9.7"),
                 tags$td(
                   "Does the laboratory identify and monitor quality indicators?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q7, "out of 1")),
                 tags$td(
                   df_final$C7)
        ),
        
        
        tags$tr( style = "border-top: thin solid;",
                 tags$td(style = "background-color:#c1db9b; ",
                         "9.8"),
                 tags$td(
                   "Has the lab been recognized or accredited by any agency?"),
                 tags$td(style = "background-color:#c1db9b; ",
                         paste(df_final$Q8, "out of 1")),
                 tags$td(
                   df_final$C8)
        )
        
        
      ) # end of tbody
    ) # end of table 
    
  })  
    
  
} # end of server


# Run the application 
shinyApp(ui = ui, server = server)
