#This is code for data manipulation/vis of Mirena coil uptake data for GP practices
#in Aneurin Bevan Health Board area in Wales for 2014-2017

#Load packages
library(tidyverse)
library(ggmap)
library(readxl)

#Set working directory
setwd("c://users/jo122989/desktop/phst2/abuhb/gynae & sh referrals/data/")

#Load data
#First for no/population-rate of coils fitted in 2017 by GP practice

coils_fitted_2017 <- read_excel("Copy of Monstercalc.xlsx", sheet = 1, col_names = FALSE)

#Next for trends in prescribing of diff contraceptive products by GP practice for 2014-2017
trend_data <- read_excel("Copy of Numbersofdevicesfromprescribingdata.xlsx", sheet=3, col_names = TRUE)

#Clean data
#Starting with 2017 prescribing data file

#Visualise data frame first
head(coils_fitted_2017)
glimpse(coils_fitted_2017)
print(coils_fitted_2017)

#Remove first 7 rows of defunct data
coils_fitted_2017 <- slice(coils_fitted_2017, 8:n())

#Reinspect data
head(coils_fitted_2017)
glimpse(coils_fitted_2017)

#Now remove unnecessary columns from data frame

coils_fitted_2017 <- coils_fitted_2017 %>% select(-9:-12)

#Rename variables

coils_fitted_2017 <- coils_fitted_2017 %>% rename(practice.code = X__1, practice.name = X__2, cluster = X__3, fertile.popn = X__4, coils.at.practice = X__5, coils.at.shs = X__6, total.coils = X__7, rate.fitted = X__8)

#Remove first row
coils_fitted_2017 <- coils_fitted_2017 %>% slice(2:n())

#Change variable classes
coils_fitted_2017$fertile.popn <- as.integer(as.character(coils_fitted_2017$fertile.popn))
coils_fitted_2017$coils.at.practice <- as.integer(as.character(coils_fitted_2017$coils.at.practice))
coils_fitted_2017$coils.at.shs <- as.integer(as.character(coils_fitted_2017$coils.at.shs))
coils_fitted_2017$total.coils <- as.integer(as.character(coils_fitted_2017$total.coils))
coils_fitted_2017$rate.fitted <- as.double(as.character(coils_fitted_2017$rate.fitted))

#Reinspect data
coils_fitted_2017
glimpse(coils_fitted_2017)

#Remove final row with NAs
coils_fitted_2017 <- coils_fitted_2017 %>% slice(1:78)

#Coil data for 2017 is now ready for exploration and visualisation

#First explore distribution of GP-fitted coils and SHS-fitted coils (+ total)
ggplot(data = coils_fitted_2017, aes(x = coils.at.practice)) + geom_density()
ggplot(data = coils_fitted_2017, aes(x = coils.at.shs)) + geom_density()

#And summarise coil data by site
summary(coils_fitted_2017$coils.at.practice)
summary(coils_fitted_2017$coils.at.shs)

#Data for coil fittings by practice, i.e. there are a larger number of practices with smaller than the median value of coils being fitted, with a relatively smaller number of practices 'performing' highly with high numbers being fitted. 

#Therefore any categorisation will need to take into account this

#Create quantiles of coil fitting data by practice with data coded 'Red' if below median (16.50), 'Amber if between median and 3rd quartile (40.5), and 'Green' if above 3rd quartile (40.5)

#Categorise coil fitting at GP practice into bins as above

perc_00 = min(coils_fitted_2017$coils.at.practice)
perc_50 = quantile(coils_fitted_2017$coils.at.practice, 0.50)
perc_75 = quantile(coils_fitted_2017$coils.at.practice, 0.75)
perc_100 = max(coils_fitted_2017$coils.at.practice)
RB <- rbind(perc_00, perc_50, perc_75, perc_100)
dimnames(RB)[[2]] = "Value"
RB

coils_fitted_2017$rag.practice <- NULL

coils_fitted_2017$rag.practice[coils_fitted_2017$coils.at.practice >= perc_00 & coils_fitted_2017$coils.at.practice < perc_50] <-"RED"

coils_fitted_2017$rag.practice[coils_fitted_2017$coils.at.practice >= perc_50 & coils_fitted_2017$coils.at.practice < perc_75] <-"AMBER"

coils_fitted_2017$rag.practice[coils_fitted_2017$coils.at.practice >= perc_75]<-"GREEN"

coils_fitted_2017$rag.practice <- factor(coils_fitted_2017$rag.practice, levels=c("RED", "AMBER", "GREEN"))

#Reinspect data
head(coils_fitted_2017)
glimpse(coils_fitted_2017)

#Now create a map of Gwent with ggmap and overlay GP practice-level data using GP surgery postcodes scraped from the web

gp.postcodes <- read_excel("abuhb.postcodes.xlsx", sheet=1, col_names = TRUE)

gp.postcodes <- gp.postcodes %>% select(practice.name, practice.postcode)

coils_fitted_2017 <- left_join(coils_fitted_2017, gp.postcodes, by = "practice.name")

map <- get_map(location = c(left = -2.85, bottom = 51.55, right = -3.20, top = 51.80), source = "google", zoom = 10)

ggmap(map)

#Extract GP practice postcodes to identify coordinates

write.csv(coils_fitted_2017$practice.postcode, "postcodes.csv")

#Coordinates extracted from https://gridreferencefinder.com/postcodeBatchConverter/

#Load practice coordinates

practice.coordinates <- read.csv("practice.coordinates.csv")

practice.coordinates <- practice.coordinates %>% select(Postcode, Latitude, Longitude)

practice.coordinates$Postcode <- as.character(practice.coordinates$Postcode)

practice.coordinates <- practice.coordinates %>% rename(practice.postcode = Postcode)

#Merge practice coordinates data with coils data

#after removing trailing whitespace from postcode variable in coils data

coils_fitted_2017$practice.postcode <- str_trim(coils_fitted_2017$practice.postcode)

coils_fitted_2017 <- left_join(coils_fitted_2017, practice.coordinates, by = "practice.postcode")

#Create map of practices in Gwent by RAG rating of coils uptake/delivery

windowsFonts(TrebuchetMS=windowsFont("Trebuchet MS"))

ggmap(map) + 
  geom_point(data=coils_fitted_2017, aes(x=Longitude, y=Latitude, colour = factor(rag.practice)), size = 4) + 
  scale_colour_manual("Practice category", values=c("red", "orange", "green")) +   labs(title="Over half of GP surgeries in ABUHB in 2017 inserted < 16 coils", 
       subtitle="Close to the recommended minimum of 12 for reaccreditation", 
       caption = "Source: ABUHB, 2018. Practices have been categorised according to number of coils inserted.") + 
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        plot.title=element_text(face="bold", family="TrebuchetMS", size=16), 
        plot.subtitle=element_text(face="italic", family="TrebuchetMS", size=12), 
        plot.caption=element_text(family="TrebuchetMS"))

#Next create map of rates of coil fitting based on fertile female population

#First explore data

ggplot(coils_fitted_2017, aes(x=rate.fitted)) + geom_density()
ggplot(coils_fitted_2017, aes(x=coils.at.practice)) + geom_density()

#Variable remains right-skewed, though somewhat less so

summary(coils_fitted_2017$rate.fitted)

#Categorise RATES of coil fitting at GP practice into bins as before

perc_00 = min(coils_fitted_2017$rate.fitted)
perc_25 = quantile(coils_fitted_2017$rate.fitted, 0.25)
perc_50 = quantile(coils_fitted_2017$rate.fitted, 0.50)
perc_75 = quantile(coils_fitted_2017$rate.fitted, 0.75)
perc_100 = max(coils_fitted_2017$rate.fitted)
RB <- rbind(perc_00, perc_25, perc_50, perc_75, perc_100)
dimnames(RB)[[2]] = "Value"

coils_fitted_2017$rag.rate <- NULL

coils_fitted_2017$rag.rate[coils_fitted_2017$rate.fitted >= perc_00 & coils_fitted_2017$rate.fitted < perc_25] <-"RED"

coils_fitted_2017$rag.rate[coils_fitted_2017$rate.fitted >= perc_25 & coils_fitted_2017$rate.fitted < perc_75] <-"AMBER"

coils_fitted_2017$rag.rate[coils_fitted_2017$rate.fitted >= perc_75]<-"GREEN"

coils_fitted_2017$rag.rate <- factor(coils_fitted_2017$rag.rate, levels=c("RED", "AMBER", "GREEN"))

#Create map using population-standardised rates

ggmap(map) + 
  geom_point(data=coils_fitted_2017, aes(x=Longitude, y=Latitude, colour = factor(rag.rate)), size = 4) + 
  scale_colour_manual("Practice category", values=c("red", "orange", "green")) +   labs(title="Adjusting for practice population size attenuates this effect somewhat", 
                                                                                        subtitle="However there remains significant variation across ABUHB and within NCNs in the numbers of coils \nfitted annually by GP practices", 
                                                                                        caption = "Source: ABUHB, 2018. Practices have been categorised according to number of coils inserted. \nResident population size of women aged 35-55 has been used to standardise rates of insertion.") + 
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        plot.title=element_text(face="bold", family="TrebuchetMS", size=16), 
        plot.subtitle=element_text(face="italic", family="TrebuchetMS", size=12), 
        plot.caption=element_text(family="TrebuchetMS")) + 
  ggsave("Population_standardised_GP_coil_insertions_2017_ABUHB.pdf", width = 20, height = 20, units = "cm")

#Now code for tidying and visualisation of coil trend data 

head(trend_data)
glimpse(trend_data)

#Remove unecessary variables
trend_data <- trend_data %>% select(1, 2, 6, 7, 8, 9, 10)
glimpse(trend_data)

#Convert wide data to long

trend_data_long <- trend_data %>% gather(year, inserted, `41640`:`42736`, factor_key = FALSE)

#Replace data for years lost in extraction

trend_data_long$year[trend_data_long$year=="41640"] <- "2014"
trend_data_long$year[trend_data_long$year=="42005"] <- "2015"
trend_data_long$year[trend_data_long$year=="42370"] <- "2016"
trend_data_long$year[trend_data_long$year=="42736"] <- "2017"

#Filter out data for Nexplanon implants

trend_data_long <- trend_data_long %>% filter(Product !="Nexplanon")

#Summarise data by aggregating at NCN level

ncn_summary <- trend_data_long %>% group_by(NCN, year) %>% summarise(total.coils = sum(inserted))

#Now graph trend data

ggplot(data = ncn_summary) + geom_line(aes(x=year, y=total.coils, group=1, colour=NCN), size=1.5) + 
  facet_wrap(~NCN, nrow=4) + 
  labs(title="Rates of coil insertions across ABUHB are fairly stable, with wide variation between NCNs as shown below",
       subtitle="Figures remain particularly low in Blaenau Gwent, while Monmouthshire continues to show the highest figures for across Gwent",
    y="Total number of Coils and Intra-uterine systems inserted", 
    caption="Source: ABUHB, 2018. Graphs show combined figures for Mirena, intra-uterine contraceptive devices and Jaydess coils.") +
  theme(legend.position="none", 
        axis.title.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        plot.title=element_text(face="bold", family="TrebuchetMS"), 
        plot.subtitle=element_text(face="italic", family="TrebuchetMS"))
