library(readxl)
library(finalfit)
library(summarytools)
library(nnet)
library(gtsummary)
library(tidyverse)
chisq <- read_excel("C:/Users/LENOVO/Downloads/zhibon/chisq.xlsx")

colnames(chisq)

chisq %>% select(Day, NightnoStreetLight, NightStreetLightoff,NightStreetLighton) %>% chisq.test()
chisq %>% select(Clear,Fog, Rain,Other...9) %>% chisq.test()
chisq %>% select(Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday) %>% chisq.test()
chisq %>% select('00-02am', '02-04am', '04-06am', '06-08am', '08-10am', '10-12noon', '12-2pm', '2-4pm','4-6pm', '6-8pm', '8-10pm', '10-12midnight') %>% chisq.test()
chisq %>% select(NotJunction, CrossJunction, TJunction, StagX, YJunction, Roundabout, Railway, other) %>% chisq.test()
chisq %>% select(HeadOn, Rearend, '90degree', SideSwipe, RanoffRoad, ObjectonRoad, ObjectoffRoad, ParkedVehicle, Pedestrian, HitAnimal, Other...47) %>% chisq.test()
chisq %>% select(None, Inexperience, Inattentive, TooFast, TooClose, NoSignal, ImproperOvertaking, ImproperTurning, FatiguedAsleep, OtherLostControl, Unknown) %>% chisq.test()



explanatory <- c("Monday", "Tuesday", "Wednesday", "Thursday", 'Friday', 'Saturday', "Sunday")
dependent <- 'casualties'
chisq %>% 
  finalfit(dependent, explanatory) -> tbl
knitr::kable(tbl, row.names = FALSE, align = c("l", "l", "r", "r", "r"))


explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  finalfit(dependent, explanatory) -> t3
knitr::kable(t3, row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))



probitmodel <- glm(casualties ~ Monday + Tuesday, data = chisq, family = binomial(link = "probit"))
