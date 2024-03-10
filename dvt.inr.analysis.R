#===SET WORKING DIRECTORY===
setwd("c://users/jo122989/desktop/phst2/abuhb/mau admissions project/Data")
#===LOAD PACKAGES===
library(dplyr)
library(ggplot2)
library(stringr)
#===LOAD MASTER FILE OF MAU ADMISSIONS===
df <- read.csv("mau.admissions.2017.v2.csv", stringsAsFactors = FALSE)
#===REPLACE SPACES IN VARIABLE TITLES WITH _===
names(df) <- gsub(" ", "_", names(df))
df <- df %>% mutate(Days=cut(Days.in.hospital, breaks=c(-Inf, 1, 2, 3, 5, 7, Inf), labels=c("<1 days", "1-2 days", "2-3 days ", "3-5 days", "5-7 days", ">7 days")))
#Filter data using PAS diagnosis, original reason for admission and MAU-coded diagnosis variables
filter.pas <- df %>% filter(str_detect(PAS.Diagnosis, "Deep vein thrombosis"))
filter.reason <- df %>% filter(str_detect(Reason.For.Admission, "DVT|calf|swelling|swollen"))
filter.original <- df %>% filter(str_detect(Original.diagnosis..audit., "calf|DVT|leg pain|leg swelling"))

duprows <- which(!is.na(match(rownames(filter.pas),rownames(filter.reason), rownames(filter.original))))

dvt.all <- rbind(filter.pas, filter.reason, filter.original[!duprows,])

#Inspect data
write.csv(dvt.all, "dvt.2017.csv")

#Create plot of monthly presentations with suspected DVT against overall
#attendances

dvt.all$dvt <- "Yes"
master.dvt <- left_join(df, dvt.all, by = c("Date.and.Time.in"))
master.dvt$dvt <- ifelse(is.na(master.dvt$dvt), "No", master.dvt$dvt)

pick <- function(condition){
  function(d) d %>% filter_(condition)
}

master.dvt %>% ggplot(.) + aes(x=lubridate::month(Date.and.Time.in, label=TRUE, abbr=TRUE), group=factor(lubridate::year(Date.and.Time.in)), colour=factor(lubridate::year(Date.and.Time.in))) + geom_bar(stat="count", aes(colour="Overall")) + geom_bar(data=pick(~dvt=="Yes"), aes(colour="DVT"), stat="count") + 
  theme(panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.background=element_rect("white"), 
        plot.title=element_text(size=20), 
        plot.subtitle=element_text(size=16)) + 
  labs(title="Monthly attendances in ABUHB Medical Assessment Units in 2017", 
       subtitle="Showing overall figures and attendances for patients with suspected DVT", 
       caption="Source: ABUHB Business Intelligence", 
       x="Month", 
       y="Monthly MAU attendances") +
  scale_fill_manual(name="Legend", values=c(Overall="blue4", DVT="goldenrod2"))

master.dvt %>% ggplot(.) + aes(x=lubridate::month(Date.and.Time.in, label=TRUE, abbr=TRUE)) + geom_bar(stat="count", aes(fill="Overall")) + geom_bar(data=pick(~dvt=="Yes"), aes(fill="DVT"), stat="count") + 
  theme(panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.background=element_rect("white"), 
        plot.title=element_text(size=20), 
        plot.subtitle=element_text(size=16)) + 
  labs(title="Monthly attendances in ABUHB Medical Assessment Units Jan-July 2017", 
       subtitle="Showing overall figures and attendances for patients with suspected DVT", 
       caption="Source: ABUHB Business Intelligence", 
       x="Month", 
       y="Monthly MAU attendances") +
  scale_fill_manual(name="Legend", values=c(Overall="blue4", DVT="goldenrod2")) + 
  annotate("text", x=6, y=90, label = "5% of attendances = pts with \nsuspected DVT")
