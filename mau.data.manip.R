#===SET WORKING DIRECTORY===
setwd("c://users/jo122989/desktop/phst2/abuhb/mau admissions project/Data")
#===LOAD PACKAGES===
library(dplyr)
library(ggplot2)
library(RColorBrewer)
#===LOAD MASTER FILE OF MAU ADMISSIONS===
master <- read.csv("mau.admissions.2017.v2.csv", stringsAsFactors = FALSE)
#===REPLACE SPACES IN VARIABLE TITLES WITH _===
names(master) <- gsub(" ", "_", names(master))
#===PLOT BAR CHART OF DURATIONS OF ADMISSION
durations <- master %>% mutate(Days=cut(Days.in.hospital, breaks=c(-Inf, 1, 2, 3, 5, 7, Inf), labels=c("<1 days", "1-2 days", "2-3 days ", "3-5 days", "5-7 days", ">7 days")))
duration_plot <- ggplot(durations, aes(x = factor(""), fill = Days)) + geom_bar() + coord_polar(theta = "y") + 
scale_x_discrete("") + theme(plot.title = element_text(size=14, face="bold", hjust=0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
panel.background=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(), 
plot.caption = element_text(face="bold")) + 
labs(title = "Referrals to MAU by duration of admission", caption = "Source: ABUHB Business Intelligence") + scale_fill_brewer(palette="Blues")
duration_plot
#===SUBSET MASTER DATA FRAME TO FILTER ONLY PATIENTS ADMITTED FOR LESS THAN 24 HOURS===
discharged <- filter(master, Days.in.hospital<1.0)
#===PLOT BAR CHART OF LENGTHS OF STAY OF PATIENTS ADMITTED FOR LESS THAN 24 HOURS===
los = select(discharged, Days.in.hospital)
los$grp <- cut(los$Days.in.hospital, breaks=c(seq(0,1,by=0.1666)), labels = c("0-4 hours", "4-8 hours", "8-12 hours", "12-16 hours", "16-20 hours", 
"20-24 hours"))
los <- los %>% count(grp)
los <- mutate(los, percent=n/sum(n) * 100)
los_plot <- ggplot(data=los, aes(x=grp, y=percent, fill=grp)) + geom_bar(stat="identity") + 
theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), plot.title=element_text(size=14, face="bold", hjust=0.5), 
plot.subtitle=element_text(size=12, face="bold", hjust=0.5), plot.caption=element_text(face="bold")) + 
labs(title="Breakdown of 2017 MAU referrals admitted <24 hours", subtitle="by length of stay", caption="Source:ABUHB Business Intelligence") + 
scale_x_discrete("Length of stay") + scale_y_discrete("Percentage") + 
geom_text(aes(label=round(percent,1)), nudge_y = 0.5) + 
scale_fill_brewer("OrRd") + 
guides(fill=guide_legend(title="Duration of admission"))
#===ORDER DATA FRAME BY DIAGNOSIS, TESTS & INTERVENTIONS===
discharged <- arrange(discharged, ICD10, PAS.Diagnosis)
#===SAVE SUBSET DATASET TO CSV FILE===
write.csv(discharged, "discless24h.csv")
#=== CREATE BINS OF AGE GROUPS, SPLITTING AGE VARIABLE INTO GROUPS OF 10 YEARS===
discharged$age_grp <- cut(discharged$age, breaks=seq(0,110,10))
#===PLOT BAR CHART OF AGE GROUPS
age_grp_plot <- ggplot(data=discharged, aes(x=age_grp, na.rm = TRUE)) + geom_bar(stat="count", fill = "dodgerblue4") + scale_x_discrete("Age", 
labels = c("(10,20]" = "10-20 years", "(20,30]" = "20-30 years", "(30,40]" = "30-40 years", "(40,50]" = "40-50 years", "(50,60]" = "50-60 years", 
"(60,70]" = "60-70 years", "(70,80]" = "70-80 years", "(80,90]" = "80-90 years", "(90,100]" = "90-100 years", "(100,110]" = "100-110 years")) + 
scale_y_discrete("Number of MAU patients") + labs(title = "Breakdown of 2017 MAU referrals admitted <24 hours", 
subtitle = "by age category", caption = "Source: ABUHB Business Intelligence") + 
theme(plot.title = element_text(size=14, face="bold", hjust=0.5), plot.subtitle = 
element_text(size=14, face="bold", hjust=0.5), plot.caption = element_text(face="bold")) + 
geom_text(stat = "count", aes(label=..count.., y = ..count..), nudge_y = 1)
age_grp_plot
#=== PLOT BAR CHART OF PLACE OF ADMISSION===
hosp_plot <- ggplot(discharged, aes(x=current.DGH, y=..count.., fill=current.DGH)) + geom_bar(position="dodge", width = 0.5) + 
theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(size = 14, face="bold", hjust=0.5), 
plot.subtitle = element_text(size = 12, face="bold", hjust=0.5), plot.caption = element_text(face="bold")) + 
labs(title = "Breakdown of 2017 MAU referrals admitted <24 hours", subtitle = "by place of admission", caption = "Source: ABUHB Business Intelligence") + 
scale_x_discrete("Place of admission") + scale_y_discrete("Number of MAU patients") + 
scale_fill_manual("Legend", values = c("Nevill Hall Hospital" = "dodgerblue4", "Royal Gwent Hospital" = "skyblue2", "Ysbyty Ystrad Fawr" = "turquoise4")) + 
geom_text(stat = "count", aes(label=..count.., y=..count..), nudge_y = -2)
hosp_plot
#===PLOT MOST COMMON DIAGNOSIS ON DISCHARGE===
#===EXTRACT COUNT OF DIAGNOSES===
diag <- discharged %>% count(PAS.Diagnosis)
#===REMOVE ROWS WITH BLANK DIAGNOSES===
diag <- diag[!(diag$PAS.Diagnosis==""), ]
#===ADD COLUMN WITH PERCENTAGES===
diag <- diag %>% mutate(percent = n/sum(n)*100)
#===ARRANGE DATA IN DESCENDING ORDER===
diag <- diag %>% arrange(desc(percent))
#===PLOT TOP 10 DIAGNOSES===
diag_top10_plot <- top_n(diag, n=10, percent) %>% ggplot(., aes(x=PAS.Diagnosis, y=percent, fill=PAS.Diagnosis)) + geom_bar(stat="identity") + 
theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(size=14, face="bold", hjust=0.5), 
plot.caption = element_text(face="bold")) + labs(title = "Top 10 diagnoses on discharge of patients admitted <24 hours", 
caption = "Source:ABUHB Business Intelligence") + 
scale_x_discrete("Discharge diagnosis") + scale_y_discrete("Percentage of patients") + scale_fill_brewer(palette="RdYlBu") + 
geom_text(aes(label=round(percent,1)), nudge_y = 0.5)
diag_top10_plot
#===CREATE PLOT OF DIAGNOSES GROUPS=
diag.groups <- read.csv("diag.groups.csv")
diag <- merge(diag, diag.groups, by = c("PAS.Diagnosis"))
diag.group.count <- diag %>% count(Diagnosis.Group)
diag.group.count <- diag.group.count %>% mutate(percent=nn/sum(nn)*100)
diag_top5_grp_plot <- top_n(diag.group.count, n=5, percent) %>% ggplot(., aes(x=Diagnosis.Group, y=percent, fill=Diagnosis.Group)) + geom_bar(stat="identity") + 
theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(size=14, face="bold", hjust=0.5), 
plot.caption = element_text(face="bold")) + labs(title = "Top 5 diagnosis groups on discharge of patients admitted <24 hours", 
caption = "Source:ABUHB Business Intelligence") + 
scale_x_discrete("Discharge diagnosis group") + scale_y_discrete("Percentage of patients") + scale_fill_brewer(palette="RdYlBu") + 
geom_text(aes(label=round(percent,1)), nudge_y = 0.5)
diag_top5_grp_plot
#===CREATE PLOTS OF ICD10 CATEGORIES
icd10.cats <- read.csv("icd10.cats.csv")
icd10.data = select(discharged, ICD10)
icd10.data$ICD10 <- substr(icd10.data$ICD10, 0, 1)
icd10.groups <- merge(icd10.data, icd10.cats, by = c("ICD10"))
icd10.groups <- icd10.groups %>% count(ICD10.Category)
icd10.groups <- icd10.groups %>% mutate(percent = n/sum(n) *100)
diag_icd10_grp_plot <- top_n(icd10.groups, n=5, percent) %>% ggplot(., aes(x=ICD10.Category, y=percent, fill=ICD10.Category)) + geom_bar(stat="identity") + 
theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(size=14, face="bold", hjust=0.5), 
plot.caption = element_text(face="bold")) + labs(title = "Top 5 ICD-10 categories on discharge of patients admitted <24 hours", 
caption = "Source:ABUHB Business Intelligence") + 
scale_x_discrete("Discharge ICD-10 category") + scale_y_discrete("Percentage of patients") + scale_fill_brewer(palette="Blues") + 
geom_text(aes(label=round(percent,1)), nudge_y = 0.5)
diag_icd10_grp_plot
#===CREATE PLOT OF INTERVENTIONS===
interventions = select(discharged, Intervention.1)
interventions$Intervention.1[interventions$Intervention.1 == "Antibiotics"] <- "Antibiotics - Oral"
interventions$Intervention.1[interventions$Intervention.1 == "oral clindamycin"] <- "Antibiotics - Oral"
interventions$Intervention.1[interventions$Intervention.1 == "Beta blockers - stopped"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "diuretics"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "hydrocortisone"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "oral hypoglycaemics changed"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "stopped ACE"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "losartan stopped"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "laxatives"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "diuretics"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "keppra increased"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "cinnarizine"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "Nifedipine"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "medication increased"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "Beta blockers - increase"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "Bisoprolol"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "diltiazem"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "omeprazole"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "double lansoprazole"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "omeprazole dose inc"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "oral pred"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "NSAIDS"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "Diuretics orally"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "steroids"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "inhalers started"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "chlorphenamine"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "topiramate dose increased"] <- "Medications changed"
interventions$Intervention.1[interventions$Intervention.1 == "lisinopril stopped"] <- "Medications changed"
interventions <- interventions %>% count(Intervention.1)
interventions <- interventions[!(interventions$Intervention.1==""),]
interventions <- interventions %>% mutate(percent = n/sum(n) * 100) 
intervention_plot <- top_n(interventions, n=5, percent) %>% ggplot(., aes(x=Intervention.1, y=percent, fill=Intervention.1)) + geom_bar(stat="identity") + 
theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(size=14, face="bold", hjust=0.5), 
plot.caption = element_text(face="bold")) + labs(title = "Top 5 Interventions provided to patients admitted to MAU for < 24 hours", 
caption = "Source:ABUHB Business Intelligence") + 
scale_x_discrete("Intervention") + scale_y_discrete("Percentage of patients") + scale_fill_brewer(palette="PRGn") + 
geom_text(aes(label=round(percent,1)), nudge_y = 0.5)
intervention_plot
#===CREATE PLOT OF TESTS===
tests = select(discharged, Test.1)
tests$Test.1[tests$Test.1 == "CT brain"] <- "CT head"
tests$Test.1[tests$Test.1 == "nil"] <- "none"
tests$Test.1[tests$Test.1 =="D-dimer"] <- "Bloods"
tests$Test.1[tests$Test.1 =="D-Dimer"] <- "Bloods"
tests$Test.1[tests$Test.1 =="Troponin"] <- "Bloods"
tests <- tests %>% count(Test.1)
tests <- tests[!(tests$Test.1==""),]
tests <- tests %>% mutate(percent = n/sum(n) * 100)
test_plot <- top_n(tests, n=5, percent) %>% ggplot(., aes(x=Test.1, y=percent, fill=Test.1)) + geom_bar(stat="identity") + 
theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(size=14, face="bold", hjust=0.5), 
plot.caption = element_text(face="bold")) + labs(title = "Top 5 Tests provided to patients admitted to MAU for < 24 hours", 
caption = "Source:ABUHB Business Intelligence") + 
scale_x_discrete("Intervention") + scale_y_discrete("Percentage of patients") + scale_fill_brewer(palette="RdPu") + 
geom_text(aes(label=round(percent,1)), nudge_y = 0.5)
test_plot
#===SAVE HISTORY===
savehistory("mau.data.manip.RHistory")



