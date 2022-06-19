library(readr)
library(dplyr)
library(tidyselect)
library(ggplot2)
library(tidyverse)

data <- na.omit(ACTL31425110AssignmentData2022)

#sort by policy_id
data_new <- data %>%
  group_by(policy_id) %>%
  summarise(Claims=n(),
    Class = as.numeric(str_remove_all(vehicle_class,"Class")),
          State = risk_state_name,
          Manufactured_year = year_of_manufacture,
          AvgClaimsCost = mean(total_claims_cost))
summary(select(data_new))

#create class bar plot
tmp <- data0 %>%
  group_by(as.numeric(str_remove_all(vehicle_class,"Class"))) %>%
  summarise(count = n())
barplot(tmp$count, xlab="Class", ylab="Number of claims")
            
summary(select(tmp))

#create state pie chart
tmp2 <- data0 %>%
  group_by(risk_state_name) %>%
  summarise(count = n())
pie(tmp2$count,labels=c("ACT","NSW","NT","QLD","SA","TAS","VIC","WA"))
