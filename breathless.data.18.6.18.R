#This is a script for analysis of ABUHB hospital activity data for patients with breathlessness

#the data captured represents a proportion of patients presenting each month between Jan 2017 and July 2017

#data is broken down by place of referral (primary care vs emergency department)

#Load packages
library(tidyverse)
library(rvest)
library(ggmap)
library(readxl)

#Set working directory
setwd("c://users/jo122989/desktop/phst2/abuhb/mau admissions project/data/")

#Load data
gp.refs <- read_excel("copy of breathless meeting data.xlsx", sheet = 3, col_names = TRUE)

ed.refs <- read_excel("copy of breathless meeting data.xlsx", sheet = 4, col_names = TRUE)

#Tidy column names
names(gp.refs) <- str_replace_all(names(gp.refs), c(" " = ".", "`" = "", "`" = ""))
names(ed.refs) <- str_replace_all(names(ed.refs), c(" " = ".", "`" = "", "`" = ""))

#Inspect dataframes
glimpse(gp.refs)
head(gp.refs)

glimpse(ed.refs)
head(ed.refs)

#Rename columns not tidied by above functions
gp.refs <- gp.refs %>% rename(gp.ed.ref = `GP./.ED.ref`, streamed = `streamed.?`, eLGH = `eLGH?`, streamed.site = `Where.to.be.streamed?`, time.in.grange = `Time.in.Grange.(if.tranferred.out)`)

ed.refs <- ed.refs %>% rename(gp.ed.ref = `GP./.ED.ref`, streamed = `streamed.?`, eLGH = `eLGH?`, streamed.site = `Where.to.be.streamed?`, time.in.grange = `Time.in.Grange.(if.tranferred.out)`)

#Reinspect data
glimpse(gp.refs)
glimpse(ed.refs)


#Explore data focussing on discharge diagnoses within each cohort of patients (ED vs GP), then exploring investigations and interventions received by patients

#Inspect discharge diagnoses in GP patient cohort
gp.refs$diagnosis

#There are 76 patients, each of whom have a diagnosis written in free text
#This data will need cleaning, before any analysis or visualisation can take place

#First try and arrange GP referral data by discharge diagnosis to tidy before recoding

gp.refs <- gp.refs %>% arrange(diagnosis)

#Reinspect GP data
gp.refs$diagnosis

#Data now more ordered but some diagnoses need to be recoded for uniformity

#Export diagnosis data into CSV file to review further
write.csv(gp.refs$diagnosis, "gp.refs.diagnoses.csv")

#Recode character strings of discharge diagnoses to allow meaningful analysis

gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*pleurisy.*", "Pleurisy", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*PE.*", "Suspected PE", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis =  gsub(".*CTPA.*", "Suspected PE", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis =  gsub(".*bronchitis.*", "Bronchitis", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*sthma.*", "Asthma", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*ngina.*", "Ischaemic heart disease", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*NSTEMI.*", "Ischaemic heart disease", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*GORD.*", "GORD", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*pneumonia.*", "Community-acquired pneumonia", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*LRTI.*", "Community-acquired pneumonia", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*CAP.*", "Community-acquired pneumonia", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*Angina.*", "Ischaemic heart disease", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*usculoskeletal.*", "Atypical/MSK chest pain", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*MSK.*", "Atypical/MSK chest pain", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*typical.*", "Atypical/MSK chest pain", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*non-cardiac.*", "Atypical/MSK chest pain", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*COPD.*", "COPD", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*SOB.*", "Misc respiratory", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*URTI.*", "Misc respiratory", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*tightness.*", "Other", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*Panic.*", "Other", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*exacerbation.*", "Misc respiratory", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*breathing.*", "Misc respiratory", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*pulmonale.*", "Misc respiratory", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*pneumonia.*", "CAP", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*Atypical.*", "Atypical/MSK chest pain", diagnosis))
gp.refs <- gp.refs %>% mutate(diagnosis = gsub(".*heart.*", "IHD", diagnosis))

#Rearrange and reinspect GP referral diagnoses data

gp.refs <- gp.refs %>% 
  arrange(diagnosis)

gp.refs$diagnosis

#Now start visualising diagnosis data for GP referrals

#First group data by diagnosis and generate count summary for each category

gp.refs.summ <- gp.refs %>% group_by(diagnosis) %>% count() %>% arrange(desc(n))

#Rearrange data by count for plotting

#Generate percentage variable for plotting

gp.refs.summ <- gp.refs.summ %>% ungroup(diagnosis) %>% mutate(perc = (n/sum(n))*100)

#Round percentage to one decimal point for annotation on plot

gp.refs.summ[,'perc']=round(gp.refs.summ[,'perc'], 1)

gp.refs.summ <- transform(gp.refs.summ, diagnosis = reorder(diagnosis, perc,))

#Load Trebuchet MS font for plot text

windowsFonts(TrebuchetMS=windowsFont("Trebuchet MS"))

#Now generate graph

gp.refs.summ %>% ggplot(aes(x=diagnosis, y=perc)) + geom_bar(stat="identity") + coord_flip()

gp.refs.summ %>% mutate(highlight_flag = ifelse(diagnosis=="Bronchitis", T, F) | ifelse(diagnosis=="COPD", T, F) | ifelse(diagnosis=="Misc respiratory", T, F) | ifelse(diagnosis == "CAP", T,F) | ifelse(diagnosis=="Suspected PE", T, F) | ifelse(diagnosis=="Pleurisy", T,F) | ifelse(diagnosis=="Asthma", T, F)) %>%
  ggplot(aes(x=diagnosis, y=perc)) + geom_bar(aes(fill = highlight_flag), stat="identity") + coord_flip() + 
  labs(x="Diagnosis \ncategory", y="Proportion of attendances",
       title="Patients with Bronchitis, COPD, miscellaneous diagnoses and CAP \nwere the commonest respiratory diagnosis among GP referrals",
       subtitle="Respiratory diagnoses accounted for over 50% of referrals made",
       caption="Source: ABUHB, 2018. *This data comprises 76 attendances on consecutive days between Jan-July 2017. \nDiagnoses have been categorised to allow meaningful analysis.") + 
  geom_text(aes(label=ifelse((diagnosis=="Bronchitis") | (diagnosis=="COPD") | (diagnosis=="Misc respiratory") | (diagnosis=="CAP") | (diagnosis=="Suspected PE") | (diagnosis=="Pleurisy") | (diagnosis=="Asthma"), paste0(perc, '%'), "")), hjust=1, size=5, fontface="bold", color="white") + 
 theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 22),
        plot.caption=element_text(hjust = 0), 
        plot.margin = unit(c(0,2,1,0.5),"cm"), 
        legend.position="none", 
        axis.ticks=element_blank(), 
        axis.title=element_text(size=14), 
        axis.text = element_text(size=10),
        axis.title.y=element_text(angle=0, vjust=.5)) + 
  scale_fill_manual(values = c("#999999", "dark red"))

#Now do similar for ED patient cohort

#Start with inspecting diagnoses in dataset

glimpse(ed.refs)
ed.refs$diagnosis

#Arrange data alphabetically prior to recoding

ed.refs <- ed.refs %>% arrange(diagnosis)

#Reinspect data
ed.refs$diagnosis

#Recode data

ed.refs <- ed.refs %>% mutate(diagnosis = gsub(".*chest.*", "Atypical/MSK chest pain", diagnosis))
ed.refs <- ed.refs %>% mutate(diagnosis = gsub(".*chondritis.*", "Atypical/MSK chest pain", diagnosis))
ed.refs <- ed.refs %>% mutate(diagnosis = gsub(".*PE.*", "Suspected PE", diagnosis))
ed.refs <- ed.refs %>% mutate(diagnosis = gsub(".*effusion.*", "Pleural effusion", diagnosis))
ed.refs <- ed.refs %>% mutate(diagnosis = gsub(".*angina.*", "IHD", diagnosis))
ed.refs <- ed.refs %>% mutate(diagnosis = gsub(".*sinus.*", "Arrythmia", diagnosis))
ed.refs <- ed.refs %>% mutate(diagnosis = gsub(".*IHD.*", "IHD", diagnosis))
ed.refs <- ed.refs %>% mutate(diagnosis = gsub(".*COPD.*", "COPD", diagnosis))
ed.refs <- ed.refs %>% mutate(diagnosis = gsub(".*acopia.*", "Other", diagnosis))
ed.refs <- ed.refs %>% mutate(diagnosis = gsub(".*bronchitis.*", "Bronchitis", diagnosis))
ed.refs <- ed.refs %>% mutate(diagnosis = gsub(".*anxiety.*", "Other", diagnosis))
ed.refs <- ed.refs %>% mutate(diagnosis = gsub(".*congenital.*", "Other", diagnosis))
ed.refs <- ed.refs %>% mutate(diagnosis = gsub(".*GORD.*", "GORD", diagnosis))
ed.refs <- ed.refs %>% mutate(diagnosis = gsub(".*none.*", "Other", diagnosis))

#Reinspect data again
ed.refs <- ed.refs %>% arrange(diagnosis)
ed.refs$diagnosis

#Now start visualising diagnosis data for ED referrals

ed.refs.all <- ed.refs %>% group_by(diagnosis) %>% count() %>% arrange(desc(n))

ed.refs.all <- transform(ed.refs.all, diagnosis = reorder(diagnosis, n,))

ed.refs.all <- ed.refs.all %>% ungroup(diagnosis) %>% mutate(perc=(n/sum(n))*100)

ed.refs.all[,'perc']=round(ed.refs.all[,'perc'], 1)

ed.refs.all %>% ggplot(aes(x=diagnosis, y=perc)) + geom_bar(stat="identity") + coord_flip()

ed.refs.all %>% mutate(highlight_flag = ifelse(diagnosis=="Suspected PE", T, F) | ifelse(diagnosis=="COPD", T, F) | ifelse(diagnosis=="Pleural effusion", T, F) | ifelse(diagnosis == "CAP", T,F) | ifelse(diagnosis=="Bronchitis", T, F)) %>%
  ggplot(aes(x=diagnosis, y=perc)) + geom_bar(aes(fill = highlight_flag), stat="identity") + coord_flip() + 
  labs(x="Diagnosis \ncategory", y="Proportion of attendances",
       title="For ED referrals, suspected PE, COPD exacerbation, pleural effusion \nand bronchitis were the most common respiratory diagnoses",
       subtitle="Respiratory conditions in this cohort accounted for just over 25% of referrals",
       caption="Source: ABUHB, 2018. This data comprises 26 attendances on consecutive days between Jan-July 2017. \nDiagnoses have been categorised to allow meaningful analysis.") + 
  geom_text(aes(label=ifelse((diagnosis=="Suspected PE") | (diagnosis=="COPD") | (diagnosis=="Pleural effusion") | (diagnosis=="Bronchitis"), paste0(perc, '%'), "")), hjust=1, size=5, fontface="bold", color="white") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 22),
        plot.caption=element_text(hjust = 0), 
        plot.margin = unit(c(0,2,1,0.5),"cm"), 
        legend.position="none", 
        axis.ticks=element_blank(), 
        axis.title=element_text(size=14), 
        axis.text = element_text(size=10),
        axis.title.y=element_text(angle=0, vjust=.5)) + 
  scale_fill_manual(values = c("#999999", "dark red"))

#Finally for diagnosis data, combine datasets to show overall (raw, unweighted) mean proportions across both cohorts to provide overall picture

#First extract columns from each dataset that we want

glimpse(gp.refs)

#From this dataset we need 'diagnosis', 'intervention' and 'test'

glimpse(ed.refs)

#Again from the ED referral dataset we need 'diagnosis', 'intervention' and also a column labelled 'tests'

#Now create two new dataframes with suffix 'dit' (diagnosis, intervention, test) prior to amending column names and merging row-wise

gp.refs.dit <- gp.refs %>% select(diagnosis, intervention, test)

ed.refs.dit <- ed.refs %>% select(diagnosis, intervention, tests)

#Create unified variable names 

ed.refs.dit <- ed.refs.dit %>% rename(diagnosis = diagnosis, intervention = intervention, test = tests)

#Now merge rows to create combined dataset with diagnosis, intervention and test data

gp.ed.refs.dit <- rbind(gp.refs.dit, ed.refs.dit)

#Inspect new dataset, focussing on diagnosis data

glimpse(gp.ed.refs.dit)

gp.ed.refs.dit <- gp.ed.refs.dit %>% arrange(diagnosis)

gp.ed.refs.dit$diagnosis

#Now start visualising diagnosis data for ED referrals

gp.ed.refs.dit <- gp.ed.refs.dit %>% group_by(diagnosis) %>% count() %>% arrange(desc(n))

gp.ed.refs.dit <- gp.ed.refs.dit %>% ungroup(diagnosis) %>% mutate(perc=(n/sum(n))*100)

gp.ed.refs.dit[,'perc']=round(gp.ed.refs.dit[,'perc'], 1)

gp.ed.refs.dit <- transform(gp.ed.refs.dit, diagnosis = reorder(diagnosis, perc,))

gp.ed.refs.dit %>% ggplot(aes(x=diagnosis, y=perc)) + geom_bar(stat="identity") + coord_flip()

gp.ed.refs.dit %>% mutate(highlight_flag = ifelse(diagnosis=="COPD", T, F) | ifelse(diagnosis=="Bronchitis", T, F) | ifelse(diagnosis=="Suspected PE", T, F) | ifelse(diagnosis == "Misc respiratory", T,F) | ifelse(diagnosis=="CAP", T, F) | ifelse(diagnosis == "Pleurisy", T, F) | ifelse(diagnosis == "Asthma", T, F) | ifelse(diagnosis == "Pleural effusion", T, F)) %>%
  ggplot(aes(x=diagnosis, y=perc)) + geom_bar(aes(fill = highlight_flag), stat="identity") + coord_flip() + 
  labs(x="Diagnosis \ncategory", y="Proportion of attendances",
       title="COPD, Bronchitis and Suspected PE were the commonest respiratory \ndiagnoses across both cohorts",
       subtitle="respiratory conditions comprised almost 50% of all diagnostic classifications",
       caption="Source: ABUHB, 2018. This data comprises 102 attendances on consecutive days between Jan-July 2017. \nDiagnoses have been categorised to allow meaningful analysis.") + 
  geom_text(aes(label=ifelse((diagnosis=="Suspected PE") | (diagnosis=="COPD") | (diagnosis=="Pleural effusion") | (diagnosis=="Bronchitis") | (diagnosis == "CAP") | (diagnosis == "Misc respiratory") | (diagnosis == "Asthma") | (diagnosis == "Pleurisy"), paste0(perc, '%'), "")), hjust=1, size=4, fontface="bold", color="white") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 22),
        plot.caption=element_text(hjust = 0), 
        plot.margin = unit(c(0,2,1,0.5),"cm"), 
        legend.position="none", 
        axis.ticks=element_blank(), 
        axis.title=element_text(size=14), 
        axis.text = element_text(size=10),
        axis.title.y=element_text(angle=0, vjust=.5)) + 
  scale_fill_manual(values = c("#999999", "dark red"))

############################

#Now explore tests + treatment for each cohort

#Reinspect referral and test/treatment data

glimpse(gp.ed.refs.dit)

#Recreate combined ED + GP referral and Ix/Rx data

gp.ed.refs.dit <- rbind(gp.refs.dit, ed.refs.dit)

#Inspect diagnosis data

gp.ed.refs.dit$diagnosis

#Diagnosis data needs little more cleaning prior to exploration of tests + treatment data

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(diagnosis = gsub(".*pleurisy.*", "Viral pleurisy", diagnosis))

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(diagnosis = gsub(".*COPD.*", "COPD", diagnosis))

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(diagnosis = gsub(".*ablation.*", "Arrythmia", diagnosis))

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(diagnosis = gsub(".*angina.*", "IHD", diagnosis))

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(diagnosis = gsub(".*pneumonia.*", "CAP", diagnosis))

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(diagnosis = gsub(".*thma.*", "Asthma", diagnosis))

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(diagnosis = gsub(".*Arrythmia.*", "Arrythmia", diagnosis))

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(diagnosis = gsub(".*degree HB.*", "Arrythmia", diagnosis))

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(diagnosis = gsub(".*effusion.*", "Misc respiratory", diagnosis))

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(diagnosis = gsub(".*chest pain.*", "Atypical/MSK chest pain", diagnosis))

#NOTE some character string manipulations have not been saved here - when rerunning code will need to add these in

#Reinspect diagnosis data after arranging alphabetically

gp.ed.refs.dit <- gp.ed.refs.dit %>% arrange(diagnosis)
gp.ed.refs.dit$diagnosis

#Inspect investigations data for GP referrals

glimpse(gp.ed.refs.dit)
gp.ed.refs.dit$test

#Split character strings in test variable into multiple columns

gp.ed.refs.dit <- gp.ed.refs.dit %>% separate(test, c("test.1", "test.2", "test.3"), ",")

gp.ed.refs.dit <- gp.ed.refs.dit %>% separate(test.1, c("test.4", "test.5"), "and")

#Inspect test data in new format

gp.ed.refs.dit[,3:6]
gp.ed.refs.dit <- gp.ed.refs.dit %>% arrange(test.4, test.5, test.2, test.3)
write.csv(gp.ed.refs.dit[,3:6], "tests.csv")

#Create logical variables based on whether patients had CXR, blood tests, peak flows, CT scans or echocardiogram

#First for CXR

gp.ed.refs.dit$cxr <- ifelse(grepl("CXR", gp.ed.refs.dit$test.4) |  grepl("CXR", gp.ed.refs.dit$test.5) | grepl("CXR", gp.ed.refs.dit$test.2) | grepl("CXR", gp.ed.refs.dit$test.3), "Yes", "No")

#Next for bloods

bloods <- c(".*loods.*", ".*dimer.*", ".*roponin.*", "trop", "cultures")

gp.ed.refs.dit$bloods <- ifelse(grepl(paste(bloods, collapse = "|"), gp.ed.refs.dit$test.4) | grepl(paste(bloods, collapse = "|"), gp.ed.refs.dit$test.5) | grepl(paste(bloods, collapse = "|"), gp.ed.refs.dit$test.3) | grepl(paste(bloods, collapse = "|"), gp.ed.refs.dit$test.2), "Yes", "No")

#Now for ECG

gp.ed.refs.dit$ecg <- ifelse(grepl("ECG", gp.ed.refs.dit$test.4) | grepl("ECG", gp.ed.refs.dit$test.5) | grepl("ECG", gp.ed.refs.dit$test.3) | grepl("ECG", gp.ed.refs.dit$test.2), "Yes", "No")

#Run similar code for d-dimers and troponins (could be useful to know for point of care testing/risk assessment)

gp.ed.refs.dit$ddimer <- ifelse(grepl(".*dimer.*", gp.ed.refs.dit$test.4) | grepl(".*dimer.*", gp.ed.refs.dit$test.5) | grepl(".*dimer.*", gp.ed.refs.dit$test.3) | grepl(".*dimer.*", gp.ed.refs.dit$test.2), "Yes", "No")

gp.ed.refs.dit$trop <- ifelse(grepl(".*rop.*", gp.ed.refs.dit$test.4) | grepl(".*rop.*", gp.ed.refs.dit$test.5) | grepl(".*rop.*", gp.ed.refs.dit$test.3) | grepl(".*rop.*", gp.ed.refs.dit$test.2), "Yes", "No")

#Explore provision of specialist imaging (CTPAs, echos etc)

gp.ed.refs.dit$ctpa <- ifelse(grepl("CTPA", gp.ed.refs.dit$test.4) | grepl("CTPA", gp.ed.refs.dit$test.5) | grepl("CTPA", gp.ed.refs.dit$test.3) | grepl("CTPA", gp.ed.refs.dit$test.2), "Yes", "No")

gp.ed.refs.dit$echo <- ifelse(grepl("echo", gp.ed.refs.dit$test.4) | grepl("echo", gp.ed.refs.dit$test.5) | grepl("echo", gp.ed.refs.dit$test.3) | grepl("echo", gp.ed.refs.dit$test.2), "Yes", "No")

#Now start visualising these new variables

ggplot(gp.ed.refs.dit, aes(x=ctpa, y=..count..)) + geom_bar(stat="count")
ggplot(gp.ed.refs.dit, aes(x=echo, y=..count..)) + geom_bar(stat="count")
ggplot(gp.ed.refs.dit, aes(x=trop, y=..count..)) + geom_bar(stat="count")
ggplot(gp.ed.refs.dit, aes(x=ddimer, y=..count..)) + geom_bar(stat="count")
ggplot(gp.ed.refs.dit, aes(x=ecg, y=..count..)) + geom_bar(stat="count")
ggplot(gp.ed.refs.dit, aes(x=cxr, y=..count..)) + geom_bar(stat="count")
ggplot(gp.ed.refs.dit, aes(x=bloods, y=..count..)) + geom_bar(stat="count")

#COPD, Bronchitis and Suspected PE were the commonest diagnoses across both cohorts, so visualise distribution of tests for these categories

top.cats <- c("COPD", "Bronchitis", "Suspected PE")

gp.ed.refs.dit %>% filter(diagnosis %in% top.cats) %>% ggplot(aes(x=bloods, y=..count..)) + geom_bar(stat="count") + facet_wrap(~diagnosis)

#The dataset is not large enough to allow meaningful analysis of testing (small numbers) - but we can present the raw figures along with a note on the limitations

bloods <- gp.ed.refs.dit %>% group_by(diagnosis) %>% count(bloods) %>% mutate(total=sum(n)) %>% ungroup() %>% mutate(prop=(n/total)*100)

cxr <- gp.ed.refs.dit %>% group_by(diagnosis) %>% count(cxr) %>% mutate(total=sum(n)) %>% ungroup() %>% mutate(prop=(n/total)*100)

ddimer <- gp.ed.refs.dit %>% group_by(diagnosis) %>% count(ddimer) %>% mutate(total=sum(n)) %>% ungroup() %>% mutate(prop=(n/total)*100)
                        
trop <- gp.ed.refs.dit %>% group_by(diagnosis) %>% count(trop) %>% mutate(total=sum(n)) %>% ungroup() %>% mutate(prop=(n/total)*100)

ctpa <- gp.ed.refs.dit %>% group_by(diagnosis) %>% count(ctpa) %>% mutate(total=sum(n)) %>% ungroup() %>% mutate(prop=(n/total)*100)

echo <- gp.ed.refs.dit %>% group_by(diagnosis) %>% count(echo) %>% mutate(total=sum(n)) %>% ungroup() %>% mutate(prop=(n/total)*100)

#Start visualing this test data

#Start with bloods for overall summary across both cohorts

ggplot(data=bloods, aes(x=bloods, y=transform(prop/10), fill=bloods)) + geom_bar(stat="identity") + coord_flip() + 
  labs(title="A significant proportion of patients across both cohorts required blood tests \nprior to discharge", 
       subtitle="These included routine, d-dimer, troponin testing and blood cultures", 
       x="Patients required blood tests during admission", 
       y="Proportion of attendances", 
       caption = "Source: ABUHB, 2018. This data comprises 102 attendances on consecutive days between Jan-July 2017. \nDiagnoses have been categorised to allow meaningful analysis.") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 22),
        plot.caption=element_text(hjust = 0), 
        plot.margin = unit(c(0,2,1,0.5),"cm"), 
        axis.ticks=element_blank(), 
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.title.x=element_text(size=12), 
        axis.text.x = element_text(size=10)) + 
  scale_fill_manual(values = c("#999999", "dark red"))
 
#Now visualise by diagnosis using facet_wrap

ggplot(data=bloods, aes(x=bloods, y=transform(prop), fill=bloods)) + geom_bar(stat="identity") + facet_wrap(~diagnosis) + 
  labs(title="Patients with respiratory diagnoses were less likely overall \nto require blood tests compared with other diagnostic categories", 
       subtitle="However a significant proportion still required routine blood tests (range 20-66.7%) prior to discharge", 
       x="Patients required blood tests during admission", 
       y="Proportion of attendances", 
       caption = "Source: ABUHB, 2018. This data comprises 102 attendances on consecutive days between Jan-July 2017. \nDiagnoses have been categorised to allow meaningful analysis.") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 22),
        plot.caption=element_text(hjust = 0), 
        plot.margin = unit(c(0,2,1,0.5),"cm"), 
        axis.ticks=element_blank(), 
        axis.title=element_blank(), 
        axis.text.x=element_blank(),
        axis.text.y = element_text(size=10)) + 
  scale_fill_manual(values = c("#999999", "dark red"))

#Next repeat this for CXR data

ggplot(data=cxr, aes(x=cxr, y=transform(prop/10), fill=cxr)) + geom_bar(stat="identity") + coord_flip() + 
  labs(title="Chest x-ray was a key investigation for the majority of patients \nacross both cohorts prior to discharge", 
       subtitle="This is unsurprising given recognised paramount importance in assessing patients with cardiorespiratory problems", 
       y="Proportion of attendances", 
       caption = "Source: ABUHB, 2018. This data comprises 102 attendances on consecutive days between Jan-July 2017. \nDiagnoses have been categorised to allow meaningful analysis.") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 22),
        plot.caption=element_text(hjust = 0), 
        plot.margin = unit(c(0,2,1,0.5),"cm"), 
        axis.ticks=element_blank(), 
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.title.x=element_text(size=12), 
        axis.text.x = element_text(size=10)) + 
  scale_fill_manual(values = c("#999999", "dark red"))

#Now visualise by diagnosis using facet_wrap

ggplot(data=cxr, aes(x=cxr, y=transform(prop), fill=cxr)) + geom_bar(stat="identity") + facet_wrap(~diagnosis) + 
  labs(title="This trend was more pronounced in patients with respiratory diagnoses \nwho were more likely overall to require plain chest radiographs", 
       subtitle="Range = 57.1% (Suspected PE) - 100% (COPD & Bronchitis attendances)", 
       x="Patients required blood tests during admission", 
       y="Proportion of attendances", 
       caption = "Source: ABUHB, 2018. This data comprises 102 attendances on consecutive days between Jan-July 2017. \nDiagnoses have been categorised to allow meaningful analysis.") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 22),
        plot.caption=element_text(hjust = 0), 
        plot.margin = unit(c(0,2,1,0.5),"cm"), 
        axis.ticks=element_blank(), 
        axis.title=element_blank(), 
        axis.text.x=element_blank(),
        axis.text.y = element_text(size=10)) + 
  scale_fill_manual(values = c("#999999", "dark red"))

#Visualise ctpa/echo investigation data

ggplot(data=ctpa, aes(x=ctpa, y=(prop/10), fill=ctpa)) + geom_bar(stat="identity") + coord_flip() + 
  labs(title="CT pulmonary angiogram (CTPA) was a relatively uncommon investigation \nfor patients with cardiorespiratory diagnoses in the cohort", 
       subtitle="Reflecting its likely exceptional status for reserved groups of patients among whom clinicians need definitive diagnostic testing", 
       y="Proportion of attendances", 
       caption = "Source: ABUHB, 2018. This data comprises 102 attendances on consecutive days between Jan-July 2017. \nDiagnoses have been categorised to allow meaningful analysis.") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 22),
        plot.caption=element_text(hjust = 0), 
        plot.margin = unit(c(0,2,1,0.5),"cm"), 
        axis.ticks=element_blank(), 
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.title.x=element_text(size=12), 
        axis.text.x = element_text(size=10)) + 
  scale_fill_manual(values = c("#999999", "dark red"))

ggplot(data=echo, aes(x=echo, y=(prop/10), fill=echo)) + geom_bar(stat="identity") + coord_flip() + 
  labs(title="Echocardiograms were even less common across the cohort as an investigation \nduring patients' diagnostic work-up", 
       y="Proportion of attendances", 
       caption = "Source: ABUHB, 2018. This data comprises 102 attendances on consecutive days between Jan-July 2017. \nDiagnoses have been categorised to allow meaningful analysis.") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 22),
        plot.caption=element_text(hjust = 0), 
        plot.margin = unit(c(0,2,1,0.5),"cm"), 
        axis.ticks=element_blank(), 
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.title.x=element_text(size=12), 
        axis.text.x = element_text(size=10)) + 
  scale_fill_manual(values = c("#999999", "dark red"))

#Finally visualise investigation data for d-dimer and troponin testing

ggplot(data=ddimer, aes(x=ddimer, y=(prop/10), fill=ddimer)) + geom_bar(stat="identity") + coord_flip() + 
  labs(title="D-dimer to exclude venous thromboembolism was more common, yet still did not \nfeature in a large proportion of patients' investigations when looking across the cohort", 
       y="Proportion of attendances", 
       caption = "Source: ABUHB, 2018. This data comprises 102 attendances on consecutive days between Jan-July 2017. \nDiagnoses have been categorised to allow meaningful analysis.") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 22),
        plot.caption=element_text(hjust = 0), 
        plot.margin = unit(c(0,2,1,0.5),"cm"), 
        axis.ticks=element_blank(), 
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.title.x=element_text(size=12), 
        axis.text.x = element_text(size=10)) + 
  scale_fill_manual(values = c("#999999", "dark red"))

ggplot(data=trop, aes(x=trop, y=(prop/10), fill=trop)) + geom_bar(stat="identity") + coord_flip() + 
  labs(title="Troponin testing to elicit likelihood of cardiac event was more common still, \nthough this was more applicable in the evaluation of patients in the cohort \nwith chest pain of unknown origin", 
       y="Proportion of attendances", 
       caption = "Source: ABUHB, 2018. This data comprises 102 attendances on consecutive days between Jan-July 2017. \nDiagnoses have been categorised to allow meaningful analysis.") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 22),
        plot.caption=element_text(hjust = 0), 
        plot.margin = unit(c(0,2,1,0.5),"cm"), 
        axis.ticks=element_blank(), 
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.title.x=element_text(size=12), 
        axis.text.x = element_text(size=10)) + 
  scale_fill_manual(values = c("#999999", "dark red"))

#Finally explore treatment data and generate similar summary data highlighting treatments given by diagnosis

#Inspect treatment data (and arrange)

gp.ed.refs.dit <- gp.ed.refs.dit %>% arrange(intervention)
gp.ed.refs.dit$intervention
write.csv(gp.ed.refs.dit$intervention, "treatments.csv")

#There appear to be a number of common treatments (Antibiotics, Steroids, Medication Changes, Diuretics, Nebuliser/Nebs, Outpatient Referral and Reassurance only). 

#Before this data is visualised...

#Clean data, but actually since the code before looked for a character string and produced logical output, there probably isn't a need to recode every row of data or to categorise

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(intervention = gsub(".*bisoprolol.*", "Medication change", intervention))

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(intervention = gsub(".*candesartan.*", "Medication change", intervention))

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(intervention = gsub(".*altered.*", "Medication change", intervention))

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(intervention = gsub(".*losartan.*", "Medication change", intervention))

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(intervention = gsub(".nil.*", "Reassurance only", intervention))

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(intervention = gsub(".*none.*", "Reassurance only", intervention))

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(intervention = gsub(".*none.*", "Reassurance only", intervention))

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(intervention = gsub(".*NSAIDS.*", "Medication change", intervention))

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(intervention = gsub(".*omeprazole.*", "Medication change", intervention))

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(intervention = gsub(".*ral AB.*", "Oral antibiotics", intervention))

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(intervention = gsub(".*Oral medication*", "Medication change", intervention))

gp.ed.refs.dit <- gp.ed.refs.dit %>% mutate(intervention = gsub(".*PE excluded.*", "Reassurance only", intervention))

#Now create a number of logical variables based on the commonest treatments provided during admissions 

#First re-inspect treatment data

gp.ed.refs.dit <- gp.ed.refs.dit %>% arrange(intervention)
gp.ed.refs.dit$intervention
write.csv(gp.ed.refs.dit$intervention, "treatments.csv")

#Now start visualising test data for patients who received Antibiotics, Steroids, Medication Changes, Nebuliser/Nebs, Outpatient Referral and Reassurance only

#First for oral antibiotics

gp.ed.refs.dit$abx <- ifelse(grepl(".*biotic.*",gp.ed.refs.dit$intervention), "Yes", "No")

abx <- gp.ed.refs.dit %>% count(abx) %>% mutate(total=sum(n)) %>% mutate(prop=round((n/total)*100), digits=2)

ggplot(data=abx, aes(x=abx, y=prop, fill=abx)) + geom_bar(stat="identity") + coord_flip() + 
  labs(title="Oral antibiotics were a common intervention across the patient cohort, either \nalone or with steroid or other medication changes",
       y="Proportion of attendances", 
       caption = "Source: ABUHB, 2018. This data comprises 102 attendances on consecutive days between Jan-July 2017. \nDiagnoses have been categorised to allow meaningful analysis.") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 22),
        plot.caption=element_text(hjust = 0), 
        plot.margin = unit(c(0,2,1,0.5),"cm"), 
        axis.ticks=element_blank(), 
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.title.x=element_text(size=12), 
        axis.text.x = element_text(size=10)) + 
  scale_fill_manual(values = c("#999999", "dark red")) + 
  geom_text(aes(label=ifelse((abx=="Yes"), paste0(prop, '%'), "")), hjust=2, size=5, fontface="bold", color="white")

#NOTE: all patients receiving steroids also received ABx, so little point in running code again for this intervention type

#Next run for medication changes

gp.ed.refs.dit$meds <- ifelse(grepl(".*cation.*",gp.ed.refs.dit$intervention), "Yes", "No")

meds <- gp.ed.refs.dit %>% count(meds) %>% mutate(total=sum(n)) %>% ungroup() %>% mutate(prop=round((n/total)*100), digits=2)

ggplot(data=meds, aes(x=meds, y=prop, fill=meds)) + geom_bar(stat="identity") + coord_flip() + 
  labs(title="Approximately one in ten patients in the cohort had a relatively simple change in their \nusual oral medication prior to discharge",
       y="Proportion of attendances", 
       caption = "Source: ABUHB, 2018. This data comprises 102 attendances on consecutive days between Jan-July 2017. \nDiagnoses have been categorised to allow meaningful analysis.") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 22),
        plot.caption=element_text(hjust = 0), 
        plot.margin = unit(c(0,2,1,0.5),"cm"), 
        axis.ticks=element_blank(), 
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.title.x=element_text(size=12), 
        axis.text.x = element_text(size=10)) + 
  scale_fill_manual(values = c("#999999", "dark red")) + 
  geom_text(aes(label=ifelse((meds=="Yes"), paste0(prop, '%'), "")), hjust=2, size=5, fontface="bold", color="white")

#Next run for medication changes

gp.ed.refs.dit$reass <- ifelse(grepl(".*reassur.*",gp.ed.refs.dit$intervention), "Yes", "No")

reass <- gp.ed.refs.dit %>% count(reass) %>% mutate(total=sum(n)) %>% ungroup() %>% mutate(prop=round((n/total)*100), digits=2)

ggplot(data=reass, aes(x=reass, y=prop, fill=reass)) + geom_bar(stat="identity") + coord_flip() + 
  labs(title="Finally, almost one in five patients in the cohort were discharged with reassurance \nand no further treatments or referrals",
       y="Proportion of attendances", 
       caption = "Source: ABUHB, 2018. This data comprises 102 attendances on consecutive days between Jan-July 2017. \nDiagnoses have been categorised to allow meaningful analysis.") + 
  theme(text = element_text(family = "TrebuchetMS", color = "#444444"), 
        plot.title = element_text(size = 22),
        plot.caption=element_text(hjust = 0), 
        plot.margin = unit(c(0,2,1,0.5),"cm"), 
        axis.ticks=element_blank(), 
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.title.x=element_text(size=12), 
        axis.text.x = element_text(size=10), 
        legend.title = element_blank()) + 
  scale_fill_manual(values = c("#999999", "dark red")) + 
  geom_text(aes(label=ifelse((reass=="Yes"), paste0(prop, '%'), "")), hjust=2, size=5, fontface="bold", color="white")
