
# Install the required packages/library. 

if (!require(shiny)){install.packages("shiny")}
if (!require(tidyverse)){install.packages("tidyverse")}
if (!require(shinydashboard)){install.packages("shinydashboard")}
if (!require(DT)){install.packages("DT")}
if (!require(plotly)){install.packages("plotly")}
if (!require(pROC)){install.packages("pROC")}
if (!require(caret)){install.packages("caret")}
if (!require(ggplot2)){install.packages("ggplot2")}
if (!require(dplyr)){install.packages("dplyr")}
if (!require(e1071)){install.packages("e1071")}
if (!require(GGally)){install.packages("GGally")}
if (!require(corrplot)){install.packages("corrplot")}
if (!require(psych)){install.packages("psych")}
if (!require(rpart)){install.packages("rpart")}
if (!require(randomForest)){install.packages("randomForest")}
if (!require(nnet)){install.packages("nnet")}
if (!require(naivebayes)){install.packages("naivebayes")}



# Import the CSV file.

df<- read.csv(file = "./diabetic_data.csv",stringsAsFactors = FALSE)
filename <- './diabetic_data_base.csv'

raw_data <- read.table(filename, sep = ",", header = T, na.strings = "?")
data <- select(raw_data,  -encounter_id, -patient_nbr, -weight,-(25:41),-(43:47))

#



#


df$race[df$race == ""] = "Not Registered"
df$race[df$race == "AfricanAmerican"] = "African American"


##  Admission Type - 

df = df %>%
  mutate(.,admission_type_id = case_when(admission_type_id == 1 ~ "Emergency",
                                         admission_type_id == 2 ~ "Urgent",
                                         admission_type_id == 3 ~ "Elective",
                                         admission_type_id == 4 ~ "Newborn",
                                         admission_type_id == 5 ~ "Unknown",
                                         admission_type_id == 7 ~ "Trauma Center"))


# Age

df = df %>%
  mutate(.,age = case_when(age == 1 ~ "0 - 10",
                           age == 2 ~ "10 - 20",
                           age == 3 ~ "20 - 30",
                           age == 4 ~ "30 - 40",
                           age == 5 ~ "40 - 50",
                           age == 6 ~ "50 - 60",
                           age == 7 ~ "60 - 70",
                           age == 8 ~ "70 - 80",
                           age == 9 ~ "80 - 90",
                           age == 10 ~ "90 - 100"))

# Discharge

df = df %>%
  mutate(.,discharge_disposition = case_when(discharge_disposition == "hhealth" ~ "Healthcare at Home",
                                             discharge_disposition == "home" ~ "Home, No Healthcare Required",
                                             discharge_disposition == "hospice" ~ "Hospice Care",
                                             discharge_disposition == "hospital" ~ "Back to Hospital",
                                             discharge_disposition == "leftAMA" ~ "Left Against Medical Advice",
                                             discharge_disposition == "nursing" ~ "Into Care of Nurse",
                                             discharge_disposition == "outpatient" ~ "Outpatient Care",
                                             discharge_disposition == "psych" ~ "Mental Health Institution",
                                             discharge_disposition == "unknown" ~ "Not Registered"))

## Source

df = df %>%
  mutate(.,admission_source_id = case_when(admission_source_id == 1 ~ "Physician Referral",
                                           admission_source_id == 2 ~ "Clinic Referral",
                                           admission_source_id == 3 ~ "HMO Referral",
                                           admission_source_id == 4 ~ "Transfer from a Hospital",
                                           admission_source_id == 5 ~ "Transfer from a Skilled Nursing Facility",
                                           admission_source_id == 6 ~ "Transfer from another Health Care Facility",
                                           admission_source_id == 7 ~ "Emergency Room",
                                           admission_source_id == 8 ~ "Other"))


df = df %>%
  mutate(.,primarydiag = case_when(primarydiag == "blooddis" ~ "Blood Disorder",
                                   primarydiag == "circulatory" ~ "Circulatory",
                                   primarydiag == "digestive" ~ "Digestive",
                                   primarydiag == "infection" ~ "Infection",
                                   primarydiag == "injury" ~ "Injury",
                                   primarydiag == "mentaldis" ~ "Mental Disorder",
                                   primarydiag == "metabolic" ~ "Metabolic",
                                   primarydiag == "musculoskeletal" ~ "Musculoskeletal",
                                   primarydiag == "neoplasm" ~ "Neoplasm",
                                   primarydiag == "nervous" ~ "Nervous",
                                   primarydiag == "Nothing" ~ "Diabetes",
                                   primarydiag == "other" ~ "Other",
                                   primarydiag == "pregnancy" ~ "Pregnancy",
                                   primarydiag == "respiratory" ~ "Respiratory",
                                   primarydiag == "skin" ~ "Skin",
                                   primarydiag == "urogenital" ~ "Urogenital"))


df = df %>%
  mutate(.,A1Cresult = case_when(A1Cresult == "NotTaken" ~ "Not Taken",
                                 A1Cresult == "Norm" ~ "Normal",
                                 A1Cresult == ">7" ~ ">7",
                                 A1Cresult == ">8" ~ ">8"))

df = df %>%
  mutate(.,max_glu_serum = case_when(max_glu_serum == "NotTaken" ~ "Not Taken",
                                     max_glu_serum == "Norm" ~ "Normal",
                                     max_glu_serum == ">200" ~ ">200",
                                     max_glu_serum == ">300" ~ ">300"))



df_train<-df%>%filter(IsTrain==1)
df_test<-df%>%filter(IsTrain==0) 


### Start of EDA tab code###

m = list(
  l = 80,
  r = 100,
  b = 100,
  t = 50,
  pad = 0
)


# EDA

# Graph

outcome <- ggplotly(group_by(df_train, readmitted ) %>%
                      summarise(count=n())%>%
                      ggplot(aes(x=readmitted,y=count,fill=readmitted))+geom_bar(stat="identity",position='dodge') 
                    + theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1)) 
                    +labs(title = "Patients for Readmission Status",x = "Readmitted", y = "Patients"))%>%
  layout(margin=m)

# A1C

A1C1 <- ggplotly(group_by(df_train, A1Cresult, readmitted) %>%
                   summarise(count=n())%>%
                   ggplot(aes(x=A1Cresult,y=count,fill=readmitted))+geom_bar(stat="identity",position='dodge') 
                 + theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1)) 
                 +labs(title = "Readmission Based on A1C Results", x = "A1C Results", y = "Patients") 
                 + scale_x_discrete(limits=c("Not Taken","Normal",">7",">8")))%>%
layout(margin=m)

# A1C2

A1C2 <-ggplotly(df_train %>%
                  group_by(., A1Cresult) %>% 
                  summarise(., percentage = sum(readmitted == "Yes") / n() ) %>%
                  ggplot(., aes(x = A1Cresult, y = percentage, fill = A1Cresult)) + geom_col() 
                + theme(plot.subtitle = element_text(vjust = 1),
                        plot.caption = element_text(vjust = 1))
                + labs(title = "A1C Result vs. Proportion of Readmittance", 
                       x = "A1C Result", ##Change X
                       y = "Percentage of Readmittance",
                       fill = "A1Cresult")
                + scale_x_discrete(limits=c("Not Taken","Normal",">7",">8")))%>% ##Change Fill
  layout(margin=m)

#graph for diagnoses  in EDA
diagnoses1 <- ggplotly(group_by(df_train, number_diagnoses, readmitted) %>%
                         summarise(count=n()) %>%
                         ggplot(aes(x=number_diagnoses,y=count,fill=readmitted))
                       +geom_bar(stat="identity",position='dodge') 
                       + scale_x_continuous(breaks = seq(1,16,by = 1)) 
                       + theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1)) 
                       +labs(title = "Patients Based Vs. Number of Diagnoses", x = "Diagnoses", y = "Patients"))%>%
  layout(margin=m)

diagnoses2 <- ggplotly(df_train %>%
                         group_by(., number_diagnoses) %>% ##Change group by
                         summarise(., percentage = sum(readmitted == "Yes") / n() ) %>%
                         ggplot(., aes(x = number_diagnoses, y = percentage, fill = number_diagnoses)) + geom_col() ##Change X and Fill
                       + theme(plot.subtitle = element_text(vjust = 1),
                               plot.caption = element_text(vjust = 1))
                       + labs(title = "Diagnoses vs. Proportion of Readmittance", ##Change everything before "vs."
                              x = "Diagnoses", ##Change X
                              y = "Percentage of Readmittance",
                              fill = "number_diagnoses"))%>% ##Change Fill
  layout(margin=m)


#graph for days in hospital  in EDA

days_hospital1 <- ggplotly(group_by(df_train,time_in_hospital,readmitted)%>%
                             summarise(count=n())%>%
                             ggplot(aes(x=time_in_hospital,y=count,fill=readmitted))+geom_bar(stat="identity",position='dodge') 
                           + theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1)) 
                           +labs(title = "Patients Vs. Days Spent in Hospital", x = "Time in Hospital (# Days)", y = "Number of Patients"))%>%
  layout(margin=m)

days_hospital2 <- ggplotly(df_train %>%
                             group_by(., time_in_hospital) %>% ##Change group by
                             summarise(., percentage = sum(readmitted == "Yes") / n()) %>%
                             ggplot(., aes(x = time_in_hospital, y = percentage, fill = time_in_hospital)) + geom_col() ##Change X and Fill
                           + theme(plot.subtitle = element_text(vjust = 1),
                                   plot.caption = element_text(vjust = 1))
                           + labs(title = "Days in Hospital vs. Proportion of Readmittance", 
                                  x = "Days in Hospital", ##Change X
                                  y = "Percentage of Readmittance",
                                  fill = "time_in_hospital"))%>% ##Change Fill
  layout(margin=m)

#graph for number of labs in EDA
num_lab1<-ggplotly(group_by(df_train,num_lab_procedures,readmitted)%>%
                     summarise(count=n())%>%
                     ggplot(aes(x=num_lab_procedures,y=count,fill=readmitted))+geom_bar(stat="identity",position='dodge') 
                   + theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1)) 
                   +labs(title = "Patients Vs. Lab Procedures",x = "Lab Procedures", y = "Patients"))%>%
  layout(margin=m)

num_lab2 <- ggplotly(df_train %>%
                       group_by(., num_lab_procedures) %>% 
                       summarise(., percentage = sum(readmitted == "Yes") / n()) %>%
                       ggplot(., aes(x = num_lab_procedures, y = percentage, fill = num_lab_procedures)) + geom_col() 
                     + theme(plot.subtitle = element_text(vjust = 1),
                             plot.caption = element_text(vjust = 1))
                     + labs(title = " Lab Procedures vs. Proportion of Readmittance",
                            x = "Lab Procedures",
                            y = "Percentage of Readmittance",
                            fill = "num_lab_procedures"))%>% ##Change Fill
layout(margin=m)


#graph for number of medications in EDA
num_meds1<-ggplotly(group_by(df_train,num_medications,readmitted)%>%
                      summarise(count=n())%>%
                      ggplot(aes(x=num_medications,y=count,fill=readmitted))+geom_bar(stat="identity",position='dodge') 
                    + theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1)) 
                    +labs(title = "Patients Vs. Medications", x = "Medications", y = "Patients"))%>%
  layout(margin=m)

num_meds2 <- ggplotly(df_train %>%
                        group_by(., num_medications) %>% 
                        summarise(., percentage = sum(readmitted == "Yes") / n()) %>%
                        ggplot(., aes(x = num_medications, y = percentage, fill = num_medications)) + geom_col() 
                      + theme(plot.subtitle = element_text(vjust = 1),
                              plot.caption = element_text(vjust = 1))
                      + labs(title = "Medications vs. Proportion of Readmittance", 
                             x = "Medications", 
                             y = "Percentage of Readmittance",
                             fill = "num_medications"))%>% 
  layout(margin=m)

#graph for age in EDA
age1<-ggplotly(group_by(df_train,age,readmitted)%>%
                 summarise(count=n())%>%
                 ggplot(aes(x=age,y=count,fill=readmitted))+geom_bar(stat="identity",position='dodge') 
               + theme(plot.subtitle = element_text(vjust = 1),  plot.caption = element_text(vjust = 1)) 
               +labs(title = "Number of Patients Vs. Age Bracket", x = "Age Bracket", y = "Patients"))%>%

layout(margin=m)
age2 <- ggplotly(df_train %>%
                   group_by(., age) %>% ##Change group by
                   summarise(., percentage = sum(readmitted == "Yes") / n()) %>%
                   ggplot(., aes(x = age, y = percentage, fill = age)) + geom_col() 
                 + theme(plot.subtitle = element_text(vjust = 1),
                         plot.caption = element_text(vjust = 1))
                 + labs(title = "Age Bracket vs. Proportion of Readmittance",
                        x = "Age Bracket", ##Change X
                        y = "Percentage of Readmittance",
                        fill = "age"))%>% ##Change Fill
  layout(margin=m)

#graph for Metformin in EDA

metformin<-ggplotly(group_by(df_train,med_metformin,readmitted)%>%
                      summarise(count=n())%>%
                      ggplot(aes(x=med_metformin,y=count,fill=readmitted))+geom_bar(stat="identity",position='dodge') 
                    + theme(plot.subtitle = element_text(vjust = 1),  plot.caption = element_text(vjust = 1)) 
                    +labs(title = "Patients Vs. Metformin Use", x = "Metformin Use", y = "Patients") 
                    + scale_x_discrete(limits=c("No","Steady","Down","Up")))%>%
  layout(margin=m)

#graph for insulin in EDA

insulin<-ggplotly(group_by(df_train,med_insulin,readmitted)%>%
                    summarise(count=n())%>%
                    ggplot(aes(x=med_insulin,y=count,fill=readmitted))+geom_bar(stat="identity",position='dodge') 
                  + theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1)) 
                  +labs(title = "Patients Vs. Insulin Use", x = "Insulin Use", y = "Patients",fill = "Readmitted") 
                  + scale_x_discrete(limits=c("No","Steady","Down","Up")))%>%
  layout(margin=m)

#graph for race in EDA
race1<-ggplotly(group_by(df_train,race,readmitted)%>%
                  summarise(count=n())%>%
                  ggplot(aes(x=race,y=count,fill=readmitted))+geom_bar(stat="identity",position='dodge') 
                + theme(plot.subtitle = element_text(vjust = 1),plot.caption = element_text(vjust = 1)) 
                +labs(title = "Patients Vs. Race",x = "Race", y = "Patients"))%>%
layout(margin=m)

race2 <- ggplotly(df_train %>%
                    group_by(., race) %>% 
                    summarise(., percentage = sum(readmitted == "Yes") / n()) %>%
                    ggplot(., aes(x = race, y = percentage, fill = race)) + geom_col() 
                  + theme(plot.subtitle = element_text(vjust = 1),
                          plot.caption = element_text(vjust = 1))
                  + labs(title = "Race vs. Proportion of Readmittance", 
                         x = "Race", ##Change X
                         y = "Percentage of Readmittance",
                         fill = "race"))%>% ##Change Fill
layout(margin=m)


### End of EDA tab code ###



