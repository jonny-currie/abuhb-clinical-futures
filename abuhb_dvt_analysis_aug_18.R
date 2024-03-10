#Code to provide analytical insights to inform planning of community DVT pathway for Aneurin Bevan University Health Board

#Code by Jonny Currie Specialty Registrar in Primary Care and Public Health

#August 2018

#Load packages
library(tidyverse)
library(readxl)
library(stringr)
library(lubridate)
library(RColorBrewer)

#Load DVT data - password protected so use XLConnect package
dvt_dataset <- read_excel("dvt_dataset.xlsx")

#Load MAU dataset for more detailed insight into patient pathways 
mau_dataset <- read.csv("c://users/jo122989/desktop/phst2/abuhb/mau admissions project/data/mau.admissions.2017.v2.csv", stringsAsFactors = FALSE)

#Explore and inspect dataset
head(dvt_dataset)
glimpse(dvt_dataset)

head(mau_dataset)
glimpse(mau_dataset)

#Two main insights the data can provide are: (1) an estimate of incidence of presentation (i.e. how many people with DVT or suspected DVT an area would expect to see per day/week/month/year), which could reveal a hypothesis of how many patients could be expected to be diverted to a new community DVT service (from the 'DVT dataset')

#and 

#(2) What tests and treatment those with suspected or confirmed DVT tend to need (from the 'MAU dataset')

#First generate count of how many patients with suspected and confirmed DVT were seen overall in 2017 and then break this down by week & monthly count

#Condition #1: only those attendances referred from primary care coded as confirmed DVT or those diagnosed with Baker's cyst [common mistaken condition for DVT], given heterogeneous soft tissue diagnoses in rest of the dataset

#Filter DVT dataset to identify:
#-only patients referred from primary care
#-patients diagnosed with a DVT
#-patients diagnosed with a Baker's cyst

dvt_data_gp_1 <- dvt_dataset %>% filter(admission_method_text=="Emergency - GP" & (diagnosis_description=="Deep vein thrombosis" | diagnosis_description=="Synovial cyst of popliteal space [Baker]"))

#Inspect new dataset

head(dvt_data_gp_1)
glimpse(dvt_data_gp_1)

#This patient cohort comprises 114 attendances of patients referred from primary care who were diagnosed as having a DVT or Baker's cyst

#Explore dataset by age, length of stay, GP practice, NCN, admitting hospital, gender

ggplot(data=dvt_data_gp_1) + geom_density(aes(x=age))

#There is a binomial age distribution with the modal age being approximately 75 and the 2nd most common age being approximately 30 years

ggplot(data=dvt_data_gp_1) + geom_density(aes(x=episode_los))

#By far the majority of patients were discharged within 24 hours with a minority being admitted for approximately 24 hours or more

ggplot(data=dvt_data_gp_1) + geom_bar(aes(x=registered_gp_practice_ncn, fill=registered_gp_practice_ncn)) + coord_flip() + theme(legend.position="none")

#While figures are not population-adjusted (or age-), Caerphilly North NCN appears to have referred more patients with suspected or confirmed DVT in 2017 than other NCNs

ggplot(data=dvt_data_gp_1) + geom_bar(aes(x=gp_practice_name, fill=gp_practice_name)) + coord_flip() + theme(legend.position="none")

#Number of referrals by practice varies from ~1 per year to 7 - given this subdataset is largely confirmed DVTs this likely represents chance (main determining factor of variation), demographics and practice population (significant contributor) and perhaps some under-diagnosis (all hypothetical assumptions however)

ggplot(data=dvt_data_gp_1) + geom_bar(aes(x=hospital, y=((..count../sum(..count..))*100))) + coord_flip() + labs(y="Percentage", x="Hospital")

#Majority of patients with confirmed or suspected DVT attended RGH (~47%), approximately 37% attended YYF and least common hospital admission site was NHH (~15%)

ggplot(data=dvt_data_gp_1) + geom_bar(aes(x=sex, y=((..count../sum(..count..))*100))) + labs(y="Percentage")

#There is a fairly equal distribution of male and female patients among those with confirmed or suspected DVT i.e. little variation

#Now create graphs showing monthly and weekly attendances for this cohort of patients

#Extract admission date from dataset

dvt_data_gp_1 %>% 
  ggplot(aes(episode_start_date)) + 
  geom_freqpoly(binwidth=86400)

#Appears to be a fairly even rate of attendances with occassional spikes, particularly in spring of 2017

#Create bar chart for monthly attendances throughout 2017

dvt_data_gp_1 %>% 
  mutate(mn=month(episode_start_date, label = TRUE)) %>%
           ggplot(aes(x=mn)) + 
           geom_bar()

#Create line graph of weekly attendances throughout 2017

dvt_data_gp_1 %>% 
  mutate(wk=week(episode_start_date)) %>% 
  ggplot(aes(x=wk)) + 
  geom_line(stat="count")
  
#Weekly attendance figures are too low to be meaningful so best to focus on monthly presentations in final analysis

month_data <- dvt_data_gp_1 %>% 
  mutate(mn=month(episode_start_date, label = TRUE)) %>%
  group_by(mn) %>% count()

month_data %>% 
  ggplot(aes(x=mn, y=n)) + 
  geom_bar(stat="identity") + 
  geom_hline(yintercept=mean(month_data$n), linetype=5, color = "red") + 
  annotate("text", label = "Mean = 9.5", x=6, y=12, size = 5, color = "red")

#Now explore MAU dataset focussing on those with confirmed or suspected DVT and exploring duration of stay, tests & treatments etc

glimpse(mau_dataset)

#Three variables may contain character string for DVT (or other conditions): reason.for.admission, original.diagnosis..audit and PAS.Diagnosis

#Start with reason for admission variable

mau_dataset$Reason.For.Admission

#Strings to look for are DVT, calf pain, swollen leg, leg swelling, calf swelling, leg pain

mau_dataset$reason_dvt <- ifelse(grepl("DVT", mau_dataset$Reason.For.Admission) | grepl("calf", mau_dataset$Reason.For.Admission) | grepl("swollen", mau_dataset$Reason.For.Admission) | grepl("swelling", mau_dataset$Reason.For.Admission) | grepl("leg pain", mau_dataset$Reason.For.Admission), "Yes", "No")

#As a test of robustness of estimations of case loads can look at average number of attendances with suspected or confirmed DVT from MAU dataset (these are single DAYS of attendances Jan-July 2017) and look at average daily attendances from DVT dataset

#Filter only MAU attendances that have a confirmed or suspected DVT

mau_dvt_data <- mau_dataset %>% filter(reason_dvt=="Yes")

#First export the subdata for inspection externally

write.csv(mau_dvt_data, "dvt_mau_data.csv")

#There are cases that have been included that are not appropriate to include (tongue Ca, myeloma, reactive arthritis)

mau_dvt_data <- filter(mau_dvt_data, Original.diagnosis..audit. != "known ca tongue, palliative, low K" & Original.diagnosis..audit. != "Myelomatous soft tissue deposits" & Original.diagnosis..audit. != "relapsed myeloma" & Original.diagnosis..audit. != "? Reactive arthritis ? Sarcoidosis" & Original.diagnosis..audit. != "delerium , AKI, chronically swollen leg" & Original.diagnosis..audit. != "symptomatic myeloma" & Reason.For.Admission != "SOB and abdominal swelling" & Reason.For.Admission != "left leg pain" & Reason.For.Admission != "swollen in fected legs")
                       
#Re-export for inspection

write.csv(mau_dvt_data, "dvt_mau_data.csv")

#Date and time in variable is currently a character variable, change its class using Lubridate package

mau_dvt_data$Date.and.Time.in <- dmy_hm(mau_dvt_data$Date.and.Time.in)

#Extract data just for daily presentations without time element

mau_dvt_data <- mau_dvt_data %>% mutate(date=day(Date.and.Time.in))

est_cases_daily <- mau_dvt_data %>% group_by(date) %>% count() %>% ungroup()

est_cases_daily

summary(est_cases_daily)

#Mean number of daily referrals from MAU dataset of confirmed or suspected DVT was 2.5 (with 95% CI based approximately 0-5.6)

daily_cases_dvt_dataset <- dvt_data_gp_1 %>% group_by(episode_start_date) %>% count() %>% ungroup()

daily_cases_dvt_dataset

summary(daily_cases_dvt_dataset)

#It would seem the newer DVT dataset estimates a mean of 1.2 cases of confirmed or suspected DVT from its conservative approach (including only confirmed cases and those with suspected Baker's cyst). This is likely to be an underestimate and could be corrected by a factor of around 2 (the mean daily case number from the MAU dataset was 2.5) with corroboration from current literature around incidence etc.

#Explore MAU dataset to look at tests, treatment and length of stay of those with suspected or confirmed DVT

#Starting with length of stay

mau_dvt_data %>% 
  ggplot(aes(x=Days.in.hospital)) +
  geom_density()

summary(mau_dvt_data$Days.in.hospital)

#The distribution of length of stay data from the MAU dataset for those with suspected or confirmed DVT is highly right-skewed with the median LoS being 0.25 (<1 day). 

#Now look at tests and treatment (in exported CSV file)

#Probably best to summarise this data in a table - can create separately from CSV file - summarising those who had a d-dimer, USS doppler and were discharged (including mean length of stay) and remainder with detail of what tests and treatment these patients had. There are only 15 attendances to provide insight though.

#Finally create final graphs for data briefing from above data exploration

#Firstly a graph estimating what proportion of cases are suspected or confirmed DVT out of all cases

mau_dvt_count <- mau_dataset %>% group_by(reason_dvt) %>% count() %>% ungroup() %>% mutate(percentage=(n/sum(n)*100))

#Load Trebuchet MS font for plot text

windowsFonts(TrebuchetMS=windowsFont("Trebuchet MS"))

mau_dvt_count %>%
  ggplot(aes(x=reason_dvt, y=percentage, fill=reason_dvt)) + 
  geom_bar(stat="identity", width=0.5) + coord_flip() + 
  labs(title="Suspected or confirmed DVT accounted for approximately one in twenty attendances \nat Medical Assessment Units in ABUHB in 2017", y= "Percentage of attendances", caption = "Source: ABUHB, 2018. This estimate comes from a dataset of 470 MAU attendances between Jan-July 2017") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 22),
        plot.caption=element_text(hjust = 0), 
        plot.margin = unit(c(0,2,1,0.5),"cm"), 
        axis.ticks=element_blank(), 
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.title.x=element_text(size=12), 
        axis.text.x = element_text(size=10), 
        legend.position = "none")  + 
  scale_fill_manual(values = c("#999999", "dark red")) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) + 
    geom_text(aes(label=ifelse((reason_dvt=="Yes"), paste0(round(percentage, 1), '%'), "")), hjust=-1, size = 5, fontface = "bold", color = "black")

#Next guaging monthly and weekly presentation attendances

#Create bar chart for monthly attendances throughout 2017 with 95% CIs

month_data <- month_data %>% mutate(se=(sqrt(n))*1.96)

month_data %>% 
  ggplot(aes(x=mn, y=n)) + 
  geom_bar(stat="identity", fill="dark red") + 
  geom_errorbar(aes(ymin=n-se, ymax=n+se), width=0.3) +
  geom_hline(yintercept=mean(month_data$n), linetype=5, color = "red") + 
  annotate("text", label = "Mean = 9.5", x=6, y=25, size = 6, color = "red", family = "TrebuchetMS") + 
  labs(title="Mean monthly presentation of patients with suspected or confirmed DVT throughout \n2017 was 9.5 attendances per month", 
       caption = "Source: ABUHB, 2018. This estimate comes from a cohort of 114 attendances.", 
       y="Count") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 22),
        plot.caption=element_text(hjust = 0), 
        axis.title.x=element_blank(), 
        axis.text.x = element_text(size=10),
        axis.ticks.x = element_blank(),
        legend.position = "none")

#Create line graph of weekly attendances throughout 2017, applying above corrective factor

wk_data <- dvt_data_gp_1 %>% mutate(wk=week(episode_start_date)) %>% group_by(wk) %>% count()

wk_data %>%
  ggplot(aes(wk, n)) + 
  geom_smooth() + 
  geom_hline(yintercept=mean(wk_data$n), linetype=5, color = "red") + 
  annotate("text", label = "Mean = 2.85", x=25, y=5, size = 6, color = "red", family = "TrebuchetMS") + 
  labs(title="Mean weekly presentation of patients with suspected or confirmed DVT throughout \n2017 was 2.85 attendances per week", 
       caption = "Source: ABUHB, 2018. This estimate comes from a cohort of 114 attendances.", 
       y="Count") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 22),
        plot.caption=element_text(hjust = 0), 
        plot.margin = unit(c(0,2,1,0.5),"cm"), 
        axis.title.x=element_blank(), 
        axis.text.x = element_text(size=10),
        axis.ticks.x = element_blank(),
        legend.position = "none")

dy_data <- dvt_data_gp_1 %>% mutate(dy=yday(episode_start_date)) %>% group_by(dy) %>% count()

dy_data %>% 
  ggplot(aes(x=dy, y=n)) + 
  geom_smooth() + 
  geom_hline(yintercept=mean(dy_data$n), linetype=5, color = "red") + 
  annotate("text", label = "Mean = 1.2", x=200, y=1.75, size = 6, color = "red", family = "TrebuchetMS") + 
  labs(title="Mean daily presentation of patients with suspected or confirmed DVT throughout \n2017 was 1.2 attendances per week", 
       caption = "Source: ABUHB, 2018. This estimate comes from a cohort of 114 attendances.", 
       y="Count") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 22),
        plot.caption=element_text(hjust = 0), 
        plot.margin = unit(c(0,2,1,0.5),"cm"), 
        axis.title.x=element_blank(), 
        axis.text.x = element_text(size=10),
        axis.ticks.x = element_blank(),
        legend.position = "none")

#Examining characteristics of patient cohort with confirmed or suspected DVT

#Age

ggplot(data=dvt_data_gp_1) + geom_density(aes(x=age, fill = "dark red")) +
  labs(title = "Presentations were commoner among older people, with potential implications \nfor service planning (safe discharge, transport and other considerations)", 
       caption = "Source: ABUHB, 2018. This estimate comes from a cohort of 114 attendances.", 
       x="Age") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 22),
        plot.caption=element_text(hjust = 0), 
        axis.title.y=element_blank(), 
        axis.text.x = element_text(size=10),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")

#Length of stay

ggplot(data=dvt_data_gp_1) + geom_density(aes(x=episode_los), fill = "dark blue") + 
  labs(title = "While length of stay was largely short, several patients were admitted for longer than 24 hours", 
       caption = "Source: ABUHB, 2018. This estimate comes from a cohort of 114 attendances.", 
       x="Length of stay (days)") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 20),
        plot.caption=element_text(hjust = 0), 
        axis.title.y=element_blank(), 
        axis.text.x = element_text(size=10),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")

#NCN cluster

#There are 13 NCNs and Brewer package only has qual colours up to max of 12, so need to extend this to cover all range of NCNs

colourCount = length(unique(dvt_data_gp_1$registered_gp_practice_ncn))

getPalette = colorRampPalette(brewer.pal(9, "Paired"))

ncn_data <- dvt_data_gp_1 %>% group_by(registered_gp_practice_ncn) %>%
  count() %>% ungroup() %>% mutate(se = (sqrt(n))*1.96)

ggplot(ncn_data) + geom_bar(aes(x=registered_gp_practice_ncn, y=n, fill=registered_gp_practice_ncn), fill = getPalette(colourCount), 
                            stat="identity") + coord_flip() + 
  geom_errorbar(aes(x = registered_gp_practice_ncn, ymin=n-se, 
                    ymax=n+se), width=0.3, linetype=2, color = "gray") + 
  labs(title = "Referral rates for suspected or confirmed DVT vared significantly between NCNs \nduring 2017", subtitle = "Lowest referring NCNs: South Powys 1 and Blaenau Gwent East 2 \nHighest referring NCN: Caerphilly North 25", y="Count",
       caption = "Source: ABUHB, 2018. This estimate comes from a cohort of 114 attendances.") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 20),
        plot.caption=element_text(hjust = 0), 
        axis.title.y=element_blank(), 
        axis.text.x = element_text(size=10),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")

#Hospital site

hosp_data <- dvt_data_gp_1 %>% group_by(hospital) %>% count() %>% ungroup() %>% 
  mutate(perc = (n/sum(n))*100) %>%
  mutate(se = (((sqrt(n))*1.96)/(sum(n))*100))

ggplot(hosp_data) + geom_bar(aes(x=hospital, y = perc, fill = hospital), stat = "identity") + coord_flip() + 
  labs(y="Percentage", x="Hospital", 
       title = "Finally, YYF and the Royal Gwent saw significantly more referrals with \nsuspected or confirmed DVT than Nevil Hall (~40-50% vs 15%)", caption = "Source: ABUHB, 2018. This estimate comes from a cohort of 114 attendances.") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 20),
        plot.caption=element_text(hjust = 0), 
        axis.title.y=element_blank(), 
        axis.text.x = element_text(size=10),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") + 
  scale_fill_manual(values = c("#999999", "dark red", "dark red")) + 
  geom_errorbar(aes(x = hospital, ymin=perc-se, ymax=perc+se), width=0.3)

#There is little variation in gender to plot for the data briefing

#Finally, create plots of common tests & treatments for patients from the MAU dataset [mau_dvt_data]

#Manually explore data in CSV file to generate dataframes for tests and investigations

dvt_treatments <- c("Oral antibiotics and blood transfusion", "IV antibiotics", "Anticoagulation", "Diuretics", "Reassurance")
dvt_treatments_figs <- c(1, 1, 4, 1, 8)
dvt_care_pathways_rx <- as.data.frame(cbind(dvt_treatments, dvt_treatments_figs))
dvt_care_pathways_rx$dvt_treatments_figs <- as.numeric(as.character(dvt_care_pathways_rx$dvt_treatments_figs))

dvt_care_pathways_rx %>% 
  mutate(highlight_flag = ifelse(dvt_treatments=="Reassurance", T, F) | ifelse(dvt_treatments=="Anticoagulation", T, F)) %>%
  ggplot() + geom_bar(aes(x=dvt_treatments, y = dvt_treatments_figs, fill = highlight_flag), stat = "identity") + coord_flip() + 
  labs(y="Count", 
       title = "Most patients from an analysis of 2017 MAU admissions of patients with \nconfirmed or suspected DVT received reassurance or anticoagulation only", caption = "Source: ABUHB, 2018. This estimate is based on a subanalysis of 15 attendances.") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 20),
        plot.caption=element_text(hjust = 0), 
        axis.title.y=element_blank(), 
        axis.text.x = element_text(size=10),
        axis.ticks.y = element_blank(),
        legend.position = "none") + 
  scale_fill_manual(values = c("#999999", "dark red"))

dvt_tests <- c("USS", "D-dimer", "CXR", "CTPA", "Echo", "ECG", "AXR", "None")
dvt_tests_figs <- c(6, 6, 4, 1, 1, 3, 1, 3)
dvt_care_pathways_tests <- as.data.frame(cbind(dvt_tests, dvt_tests_figs))
dvt_care_pathways_tests$dvt_tests_figs <- as.numeric(as.character(dvt_care_pathways_tests$dvt_tests_figs))

dvt_care_pathways_tests %>% 
  mutate(highlight_flag = ifelse(dvt_tests=="USS", T, F) | ifelse(dvt_tests=="D-dimer", T, F)) %>%
  ggplot() + geom_bar(aes(x=dvt_tests, y = dvt_tests_figs, fill = highlight_flag), stat = "identity") + coord_flip() + 
  labs(y="Count", 
       title = "USS Duplex and D-dimer were the commonest investigations for patients referred to \nMAU in 2017 with suspected or confirmed DVT", caption = "Source: ABUHB, 2018. This estimate is based on a subanalysis of 15 attendances; due to multiple investigations for some attendances the sum of these figures exceeds 15.") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 20),
        plot.caption=element_text(hjust = 0), 
        axis.title.y=element_blank(), 
        axis.text.x = element_text(size=10),
        axis.ticks.y = element_blank(),
        legend.position = "none") + 
  scale_fill_manual(values = c("#999999", "dark red"))
