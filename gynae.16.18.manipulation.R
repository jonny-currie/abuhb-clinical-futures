#Analysis of gynae and sexual health referrals for Gwent for 2016-2018
#for hysteroscopy, pessary fittings and coils

#Load packages
library(ggplot2)
library(dplyr)
library(readxl)
library(zoo)

#Set working director
setwd("c://users/jo122989/desktop/phst2/abuhb/gynae & sh referrals/data/")

#Load hysteroscopy data
hyst.data <- read_excel("gynae.data.16.18.xlsx", 1)

#Load pessary data
pess.data <- read_excel("gynae.data.16.18.xlsx", 2)

#Load SRH coil data
coil.data <- read_excel("gynae.data.16.18.xlsx", 3)

#Load gynae referral data
referral.outcome <- read_excel("gynae.data.16.18.xlsx", 4)

referral.dates <- read_excel("gynae.data.16.18.xlsx", 5)

#Load outcome lookup
outcome.lookup <- read_excel("gynae.data.16.18.xlsx", 6)

#Merge gynae referral data with lookup
referral.data <- left_join(referral.outcome, outcome.lookup, by = "outcome.code")
referral.data <- inner_join(referral.data, referral.dates, by = c("crn", "dob", "postcode", "gp.code", "gp.practice", "service"))

#Merge hysteroscopy and referral data
hyst.data$crn <- as.character(hyst.data$crn)

hyst.referrals <- left_join(referral.data, hyst.data, by = c("crn", "dob", "postcode", "gp.code"))

#Create logical variable to reflect whether patient had hysteroscopy

hyst.referrals$hyst <- as.numeric(!is.na(hyst.referrals$procedure.code))
hyst.referrals$hyst[hyst.referrals$hyst=="0"] <- "No"
hyst.referrals$hyst[hyst.referrals$hyst=="1"] <- "Yes"    

#Order data
hyst.referrals <- arrange(hyst.referrals, desc(hyst), crn, att.date, att.category, outcome.desc)

#Remove unecessary variables
hyst.referrals = select(hyst.referrals, crn, referral.date, att.date, att.category, outcome.desc, hyst)

#Add pessary fitting data
pess.data$crn <- as.character(pess.data$crn)

pess.hyst <- left_join(hyst.referrals, pess.data, by="crn")

#Create logical variable to reflect whether patient had pessary fitted

pess.hyst$pess <- as.numeric(!is.na(pess.hyst$procedure.code))
pess.hyst$pess[pess.hyst$pess=="0"] <- "No"
pess.hyst$pess[pess.hyst$pess=="1"] <- "Yes"  

#Add coil fitting data (retain dataframe name for ease)
pess.hyst <- pess.hyst %>% select(crn, referral.date, att.date, att.category, outcome.desc, hyst, dob, postcode, gp.code, pess)

coil.data$crn <- as.character(coil.data$crn)

pess.hyst <- left_join(pess.hyst, coil.data, by="crn")

#Create logical variable to reflect whether patient had coil fitted
pess.hyst$coil <- as.numeric(!is.na(pess.hyst$procedure.code))
pess.hyst$coil[pess.hyst$coil=="0"] <- "No"
pess.hyst$coil[pess.hyst$coil=="1"] <- "Yes" 

#Create count of attendances
atts <- pess.hyst %>% group_by(crn) %>% count()

#Merge attendance counts with dataset
pess.hyst <- left_join(pess.hyst, atts, by = "crn")

#Create logicical variable based on count
pess.hyst <- pess.hyst %>% mutate(atts.low = if_else(n<=3, "Yes", "No"))

#Create plot

pick <- function(condition){
  function(d) d %>% filter_(condition)
}

pess.hyst %>% 
  filter(lubridate::year(att.date) %in% 2017) %>% ggplot(.) + aes(x=lubridate::month(att.date, label=TRUE, abbr=TRUE), group=factor(lubridate::year(att.date)), colour=factor(lubridate::year(att.date))) + geom_line(stat="count", aes(colour="Overall"), size=2) + geom_line(data=pick(~hyst=="Yes" & atts.low=="Yes"), aes(colour="Hysteroscopy"), stat="count", size=2) + 
  geom_line(data=pick(~pess=="Yes" & atts.low=="Yes"), aes(colour="Pessary"), stat="count", size=2) +
  geom_line(data=pick(~coil=="Yes" & atts.low=="Yes"), aes(colour="Coil"), stat="count", size=2) +
  scale_y_continuous(breaks=seq(0,2000,100)) + 
  theme(panel.grid.major=element_line(colour="gray87"), 
        panel.grid.minor=element_blank(), 
        panel.background=element_rect("white"), 
        plot.title=element_text(size=20), 
        plot.subtitle=element_text(size=16)) + 
  labs(title="Monthly attendances in gynaecology services in ABUHB in 2017", 
       subtitle="Showing overall figures and trends for patients swiftly discharged \nfollowing hysteroscopy and pessary fitting", 
       caption="Source: Welsh Clinical Communications Gateway", 
       x="Month", 
       y="Monthly clinic attendances") +
  scale_colour_manual(name="Legend", values=c(Overall="red", Hysteroscopy="blue", Pessary="orange", Coil="green"))

  