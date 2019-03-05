library(dplyr)
library(lubridate)
library(xlsx)
setwd("Q:/MultiCare Inpatient Specialists/MIS Manager/Jack MIS Manager/Performance Management/Scorecards")

# time frames
this.month <- mdy("12-31-18")
one.month <- mdy("12-1-18")
three.months <- mdy("10-1-18")
six.months <- mdy("7-1-18")
twelve.months <- mdy("1-1-18")

# read in EPIC IDs 
med.staff <- read.xlsx("Q:/MultiCare Inpatient Specialists/MIS Manager/Jack MIS Manager/Analysis & Reporting/Dashboard/Performance Dashboards/Index/Epic IDs.xlsx", sheetIndex = "All")
physician.ID1 <- na.omit(med.staff$EPIC.IDs.2)
physician.ID2 <- med.staff$EPIC.ID

# read in HCAHPS 
hcahps <- read.csv("Q:/MultiCare Inpatient Specialists/MIS Manager/Jack MIS Manager/Performance Management/Scorecards/Q4 2018/hcahps.csv")
provider.scores <- matrix(ncol = 7,nrow = length(physician.ID1))
x <- c("IDs", "Recommend", "Comms", "Courtesy", "Listen", "Explain", "n")
colnames(provider.scores) <- x
provider.scores <- as.data.frame(provider.scores)
provider.scores$IDs <- as.numeric(cbind(physician.ID1))


# load utilization data, filter to MIS, break into time frames
hist.data <- read.csv("Q:/MultiCare Inpatient Specialists/MIS Manager/Jack MIS Manager/Analysis & Reporting/Dashboard/Performance Dashboards/Index/Current Month/inpatienthistorical.csv")
mis.data <- hist.data %>% filter(Attending.ID %in% physician.ID2 & LOS < 30)
mis.data$Dischg.Date <- mdy(mis.data$Dischg.Date)
mis.data$Total.Costs <- gsub("[^0-9.]","",mis.data$Total.Costs)
mis.data$Net.Margin <- gsub("[^0-9.]","",mis.data$Net.Margin)
mis.data$NetRev <- gsub("[^0-9.]","",mis.data$NetRev)
mis.data$Direct <- gsub("[^0-9.]","",mis.data$Direct)
utilization.twelve.months <- mis.data %>% filter(Dischg.Date %in% this.month:twelve.months)
trailing.nine <- mis.data %>% filter(Dischg.Date %in% three.months:twelve.months)

# load readmit data, filter to MIS, break into time frames
discharges <- read.csv("Q:/MultiCare Inpatient Specialists/MIS Manager/Jack MIS Manager/Analysis & Reporting/Dashboard/Performance Dashboards/Index/Current Month/12 month readmission data.csv")
mis.discharges <- discharges %>% filter(Discharge.Provider.ID %in% physician.ID1)
mis.discharges$Discharge.Date <- mdy(mis.discharges$Discharge.Date)
discharges.twelve.months <- mis.discharges %>% filter(Discharge.Date %in% this.month:twelve.months)


# Loop through HCAHPS and store 

n <- 1

for (i in provider.scores$IDs){
  if(sum(grepl(i,hcahps$FILTER)!=0)){
   provider.scores[n,"Recommend"] <- as.numeric(as.character(hcahps[grep(i,hcahps$FILTER)+19,3]))
   provider.scores[n,"Comms"] <- as.numeric(as.character(hcahps[grep(i,hcahps$FILTER)+27,3]))
   provider.scores[n,"Courtesy"] <- as.numeric(as.character(hcahps[grep(i,hcahps$FILTER)+35,3]))
   provider.scores[n,"Listen"] <- as.numeric(as.character(hcahps[grep(i,hcahps$FILTER)+43,3]))
   provider.scores[n,"Explain"] <- as.numeric(as.character(hcahps[grep(i,hcahps$FILTER)+51,3]))
   provider.scores[n,"n"] <- as.numeric(as.character(hcahps[grep(i,hcahps$FILTER)+20,2]))
  }
  n <- n+1
}

write.csv(provider.scores, file = "provider.hcahps.csv")

# expense info
rounder.expense <- trailing.nine %>% group_by(Attending.Name) %>%
  summarise(expense = mean(as.numeric(as.character(Total.Costs)),na.rm = TRUE),
            cmi.expense = mean(MSDRG.Weight)) %>%
  mutate(adjusted.expense = expense / cmi.expense)

admitter.expense <- trailing.nine %>% filter(Admitting.ID %in% physician.ID2) %>%  group_by(Admitting.Name) %>%
  summarise(expense = mean(as.numeric(as.character(Total.Costs)),na.rm = TRUE),
            cmi.expense = mean(MSDRG.Weight)) %>%
  mutate(adjusted.expense = expense / cmi.expense)

# Provider Discharge performance
mis.twelve <- utilization.twelve.months %>% group_by(Attending.Name) %>% 
  summarise(los = mean(LOS),
            cmi = mean(MSDRG.Weight),
            dc.home = sum(grepl("Home, self care",Disch.Status)),
            dc.home.health = sum(grepl("Home health service",Disch.Status)),
            dc.snf = sum(grepl("SNF",Disch.Status)),
            n = n()) %>%
  mutate(adjusted.los = los / cmi)

re.re <- mis.discharges %>% filter(Discharge.Date %in% this.month:twelve.months) %>%
  group_by(Discharge.Provider.Name) %>% 
  summarise(seven.day.readmimssions = sum(X7.Day.Readmission.Y.N == "Y"),
            thirty.day.readmissions = sum(Readmissions..with.Exclusions.),
            discharges = sum(Discharges..with.Exclusions.)) %>%
  mutate(seven.day.rate = seven.day.readmimssions / discharges,
         thirty.day.rate = thirty.day.readmissions / discharges)

colnames(re.re) <- c("Attending.Name","seven.day.readmimssions","thirty.day.readmissions",
                     "discharges","seven.day.rate","thirty.day.rate")

merged <- left_join(mis.twelve,re.re, by = "Attending.Name")
merged <- left_join(merged,rounder.expense, by = "Attending.Name")

write.csv(merged, file = "rounder.performance.csv")

# Provider Admit performance
mis.twelve.2 <- utilization.twelve.months %>% filter(Admitting.ID %in% physician.ID2) %>%  group_by(Admitting.Name) %>% 
  summarise(los = mean(LOS),
            cmi = mean(MSDRG.Weight),
            n = n()) %>%
  mutate(adjusted.los = los / cmi)

mis.twelve.2 <- left_join(mis.twelve.2,admitter.expense, by = "Admitting.Name")

write.csv(mis.twelve.2, file = "admitter.performance.csv")
