library(tidyverse)

fraud_trans_m <- fraud_transactions %>% filter(category == "shopping_net")

#Frauds by gender
#We will examine whether one gender is more susceptible to fraud than the other
gen_count <- fraud_trans_m %>% count(gender)

names(gen_count)[2] <- "fraud_count"

ggplot(data = gen_count, 
       aes(x = reorder(gender, -fraud_count), y = fraud_count)) +
  geom_col(width = 0.5, fill = "cornflowerblue") +
  labs(x = "Gender", y = "Fraud Count", title = "Fraud Count Vs Gender [Merchant Category = shopping_net]", 
       caption = "2019 Credit Card Fraud Data") +
  geom_text(aes(y = fraud_count - 100, label = fraud_count), color = "white") +
  theme_classic()

rm(gen_count)

################################################################################

#Frauds by males
male_count <- fraud_trans_m %>% filter(gender == "M") %>% count(card_owner) %>% 
  arrange(desc(n)) %>% head(5)

names(male_count)[2] <- "fraud_count"

ggplot(data = male_count, 
       aes(x = reorder(card_owner, -fraud_count), y = fraud_count)) +
  geom_col(width = 0.5, fill = "cornflowerblue") +
  labs(x = "Gender", y = "Fraud Count", title = "Top 5 male fraudsters [Merchant Category = shopping_net]", 
       caption = "2019 Credit Card Fraud Data") +
  theme_classic() +
  scale_y_continuous(breaks=seq(0, 14,by=2)) +
  theme(axis.text.x=element_text(angle=30, hjust=1))

rm(male_count)

################################################################################

#Frauds by females
fem_count <- fraud_trans_m %>% filter(gender == "F") %>% count(card_owner) %>% 
  arrange(desc(n)) %>% head(5)

names(fem_count)[2] <- "fraud_count"

ggplot(data = fem_count, 
       aes(x = reorder(card_owner, -fraud_count), y = fraud_count)) +
  geom_col(width = 0.5, fill = "cornflowerblue") +
  labs(x = "Gender", y = "Fraud Count", title = "Top 5 female fraudsters [Merchant Category = shopping_net]", 
       caption = "2019 Credit Card Fraud Data") +
  theme_classic() +
  scale_y_continuous(breaks=seq(0, 14,by=2)) +
  theme(axis.text.x=element_text(angle=30, hjust=1))

rm(fem_count)

################################################################################

#Fraud Amount 
ggplot(data = fraud_trans_m, aes(x = amount)) +
  geom_histogram(binwidth = 50, fill = "cornflowerblue") +
  labs(x = "Amount", y = "Fraud Count", title = "Frauds w.r.t. Fraud Amount [Merchant Category = shopping_net]", 
       caption = "2019 Credit Card Fraud Data") + 
  scale_x_continuous(breaks=seq(700, 1400,by=50)) +
  scale_y_continuous(breaks=seq(0, 500,by=25)) +
  theme_light()

################################################################################

#Frauds by the card owners at different age groups

#Histogram
ggplot(data = fraud_trans_m, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "cornflowerblue") + 
  labs(x = "age", y = "Fraud Count", title = "Fraud Count Vs Age  [Merchant Category = shopping_net]", 
       caption = "2019 Credit Card Fraud Data") + 
  scale_x_continuous(breaks=seq(10,100,by=5)) +
  scale_y_continuous(breaks=seq(0,150,by=20)) +
  theme_light()

################################################################################

#Frauds in different months of a year
month_count <- fraud_trans_m %>% count(trans_month) 

names(month_count)[1] <- "trans_month"
names(month_count)[2] <- "count"

month_level <- data.frame (
  month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov","Dec"),
  level = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
)

month_count <- merge(month_count, month_level, by.x = 'trans_month', by.y = 'month')

ggplot(data = month_count, aes(x = reorder(trans_month, level), y = count)) +
  geom_col(width = 0.7, fill= "cornflowerblue") + 
  labs(x = "Month", y = "Fraud Count", title = "Frauds in Different Months of a Year  [Merchant Category = shopping_net]", 
       caption = "2019 Credit Card Fraud Data") + 
  theme_classic()

rm(month_count)
rm(month_level)

################################################################################

#Hourly Trend of Frauds
hourly <- fraud_trans_m %>% count(trans_hour)

names(hourly)[1] <- "trans_hour"
names(hourly)[2] <- "count"

hour_lvl <- data.frame (
  hour = c("12:00 AM", "01:00 AM", "02:00 AM", "03:00 AM", "04:00 AM", "05:00 AM","06:00 AM", 
           "07:00 AM", "08:00 AM", "09:00 AM", "10:00 AM","11:00 AM", "12:00 PM", "01:00 PM", 
           "02:00 PM", "03:00 PM", "04:00 PM", "05:00 PM","06:00 PM", "07:00 PM", "08:00 PM", 
           "09:00 PM", "10:00 PM","11:00 PM"),
  level = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
)

hourly <- merge(hourly, hour_lvl, by.x = 'trans_hour', by.y = 'level')

names(hourly)[1] <- "level"
names(hourly)[3] <- "trans_hour"

ggplot(data = hourly, 
       aes(x = reorder(trans_hour, level), y = count)) +
  geom_col(fill = "cornflowerblue") +
  labs(x = "Hour", y = "Fraud Count", title = "Hourly trend of frauds [Merchant Category = shopping_net]", 
       caption = "2019 Credit Card Fraud Data") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 

rm(hourly)
rm(hour_lvl)

################################################################################

#Fraud at different Merchants 
merchant <- fraud_trans_m %>% group_by(merchant)  %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(10)

ggplot(data = merchant, 
       aes(x = reorder(merchant, -n) , y = n)) +
  geom_col(width = 0.7, fill= "cornflowerblue") +
  labs(x = "Merchant", y = "Fraud Count", title = "Frauds on Different Merchants [Merchant Category = shopping_net]",
       subtitle = "Top 10 mechants w.r.t. fraud count",
       caption = "2019 Credit Card Fraud Data") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=30, hjust=1))

rm(merchant)

################################################################################

#Top 10 states where most frauds occur
state <- fraud_trans_m %>% group_by(state) %>% 
  count() %>%  arrange(desc(n)) %>% head(10)

ggplot(data = state, 
       aes(x = reorder(state, -n) , y = n)) +
  geom_col(width = 0.7, fill= "cornflowerblue") +
  labs(x = "State", y = "Fraud Count", 
       title = "Frauds in Different States [Merchant Category = shopping_net]", 
       caption = "2019 Credit Card Fraud Data") +
  theme_classic()

rm(state)

################################################################################

