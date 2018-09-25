getwd()
setwd('C:/Users/user/Desktop/ABTesting_Project')

library(tidyverse)
library(lubridate)

mightyHive_abandoned <- read.csv('Abandoned_Data_Seed.csv', header=T)
mightyHive_reservation <- read.csv('Reservation_Data_Seed.csv', header=T)

colnames(mightyHive_abandoned)

#RIGHT THE DATE DATATYPE

mightyHive_abandoned$Session <- ymd_hms(mightyHive_abandoned$Session)
mightyHive_reservation$Session <- ymd_hms(mightyHive_reservation$Session)

#CHECK THE AMOUNTS OF "TEST & "CONTROL" GROUPS

nrow(mightyHive_reservation[mightyHive_reservation$Test_Control=="test",])
nrow(mightyHive_reservation[mightyHive_reservation$Test_Control=="control",])

#   Purchase dataset
#  ----------------------
#   test     =>  18728 (89.98%)
#   control  =>  2086  (10.02%)

nrow(mightyHive_abandoned[mightyHive_abandoned$Test_Control=="test",])
nrow(mightyHive_abandoned[mightyHive_abandoned$Test_Control=="control",])

#   No Purchase dataset
#  ----------------------
#   test     =>  4266 (50.53%)
#   control  =>  4176 (49.47%)

# Find out the individuals exist in both datasets by matching different variables
# with maximum matching amounts. After investigation, the candidated variables including : 
# 
# 1."Email" 2. "Incoming_Phone" 3. "Contact_Phone"
# (Noted that we first aim to find out the matching data in the datasets resulted in reservation)


# "Email" matching amounts
matches_email <- mightyHive_reservation$Email %in% mightyHive_abandoned$Email & mightyHive_reservation$Email != ""
sum(matches_email)

# "Incoming_Phone" matching amounts
matches_incoming <- mightyHive_reservation$Incoming_Phone %in% mightyHive_abandoned$Incoming_Phone & mightyHive_reservation$Incoming_Phone != ""
sum(matches_incoming)

# "Contact_Phone" matching amounts
matches_contact <- mightyHive_reservation$Contact_Phone %in% mightyHive_abandoned$Contact_Phone & mightyHive_reservation$Contact_Phone != ""
sum(matches_contact)


# Final Outcome =>
# -------------------------------------------------------------
# "Email"           =>  88 matches
# "Incoming_Phone"  => 341 matches
# "Contact_Phone"   => 175 matches
#
# ...Thus, we use "Incoming_Phone" as matching data indicator

matches <- matches_incoming
matching_data <- mightyHive_reservation[matches, ]

# Remove duplicated data point, remaining only the earliest reservation record
# First arrange the data in ascending date order, then find out the duplicates data point for
# removal.

matching_data_order <- matching_data[with(matching_data,order(Session)),]

duplicates <- duplicated(matching_data$Incoming_Phone)

sum(duplicates)

matching_yes_clean <- matching_data_order[!duplicates,]

# Check the "test, "control" groups amounts...
#     
#   Reservation
#   ----------------------
#   test     =>  244 (78.46%)
#   control  =>  67 (21.54%)

nrow(matching_yes_clean[matching_yes_clean$Test_Control == "test",])
nrow(matching_yes_clean[matching_yes_clean$Test_Control == "control",])


# Performing Z-test to justify that the the reservation due to ads retargeting 
# is not due to chance...
# ================================================================================================
# Null Hypothesis: Ho : 
# (reservation without ads retargeting) = (reservation with ads retargeting)
# 
# Alternative Hypothesis :
# (reservation without ads retargeting) < (reservation with ads retargeting)

control_proportion <- nrow(matching_yes_clean[matching_yes_clean$Test_Control == "control",]) / nrow(mightyHive_abandoned[mightyHive_abandoned$Test_Control=="control",])

test_proportion <- nrow(matching_yes_clean[matching_yes_clean$Test_Control == "test",]) / nrow(mightyHive_abandoned[mightyHive_abandoned$Test_Control=="test",])

total_proportion <- (nrow(matching_yes_clean[matching_yes_clean$Test_Control == "control",])+nrow(matching_yes_clean[matching_yes_clean$Test_Control == "test",]))/(nrow(mightyHive_abandoned))

test <- sum(matching_yes_clean$Test_Control == "test")
control <- sum(matching_yes_clean$Test_Control == "control")

test_p <- nrow(mightyHive_abandoned[mightyHive_abandoned$Test_Control=="test",])
control_p <- nrow(mightyHive_abandoned[mightyHive_abandoned$Test_Control=="control",])

z_score <- abs(control_proportion - test_proportion)/sqrt(total_proportion*(1 - total_proportion)*((1/test_p)+(1/control_p)))

pnorm(z_score, lower.tail = F)

# P-value : 5.299723*10^-24
# Meaning we sucessfully reject null hypothesis, 
# and the ads retargeting has a significant reservation boost, 5.72% boosts in specific for
# the known data point.


# Data Visualization
#
# Map the States reservation amounts of Martin's Travel Agency Products

library(ggplot2)
library(grid)
library(gridExtra)
library(usmap)

reservation_no <- as.data.frame(table(mightyHive_abandoned$Address))
reservation_no <- reservation_no[reservation_no$Var1 != "", ]
names(reservation_no) <- c("state","freq")
reservation_no$state <- factor(reservation_no$state)


reservation_yes <- as.data.frame(table(mightyHive_reservation$Address))
reservation_yes <- reservation_yes[reservation_yes$Var1 != "", ]
names(reservation_yes) <- c("state","freq")
reservation_yes$state <- factor(reservation_yes$state)


p3 <- plot_usmap(data = reservation_no, values = "freq", lines = "blue")+
      scale_fill_continuous( 
        low = "white", high = "blue", name = "No Reservation", label = scales::comma
      )+ theme(legend.position = "right")+
      labs(title = "States Density without Reservation of Martin's Travel Agency")

p4 <- plot_usmap(data = reservation_yes, values = "freq", lines = "red")+
  scale_fill_continuous( 
    low = "white", high = "red", name = "Reservation", label = scales::comma
  )+ theme(legend.position = "right")+
  labs(title = "States Density of Reservation of Martin's Travel Agency")


grid.arrange(p3, p4)

reservation_test <- as.data.frame(table(mightyHive_reservation[mightyHive_reservation$Test_Control=="test",]$Address))
reservation_test <- reservation_test[reservation_test$Var1 != "", ]
names(reservation_test) <- c("state","freq")
reservation_test$state <- factor(reservation_test$state)

# Map Plot by States Density of MightyHive Ads Retargeting Success

p5 <- plot_usmap(data = reservation_test, values = "freq", lines = "#00265f")+
      scale_fill_continuous( 
      low = "white", high = "#00265f", name = "Remarketing Success", label = scales::comma
      )+ theme(legend.position = "right")+
      labs(title = "States Density of MightyHive Ads Retargeting Success")

# Bar chart of reservation success amounts in each state, in descending order
#
# Reservation amounts for each state

reservation_yes$state <- factor(reservation_yes$state, levels = 
                                  reservation_yes$state[order(-reservation_yes$freq)])

ggplot(reservation_yes, aes(x=state, y=freq))+geom_bar(stat = "identity", aes(fill = state ))+
  ggtitle("Reservation amounts in each state")+
  labs(x = "States", y= "Reservation Amounts")+
  theme(axis.text.x=element_blank())+
  theme(plot.title = element_text(family = "Trebuchet MS", 
                                  color="#666666", face="bold", size=16, hjust=0))+
  theme(axis.title = element_text(family = "Trebuchet MS", 
                                  color="#666666", face="bold", size=12))


# Remarketing Success amounts for each state

reservation_test$state <- factor(reservation_test$state, levels = 
                                  reservation_test$state[order(-reservation_test$freq)])

ggplot(reservation_test, aes(x=state, y=freq))+geom_bar(stat = "identity", aes(fill = state ))+
  ggtitle("Remarketing success amounts in each state")+
  labs(x = "States", y= "Testing Success Amounts")+
  theme(axis.text.x=element_blank())+
  theme(plot.title = element_text(family = "Trebuchet MS", 
                                  color="#666666", face="bold", size=16, hjust=0))+
  theme(axis.title = element_text(family = "Trebuchet MS", 
                                  color="#666666", face="bold", size=12))

