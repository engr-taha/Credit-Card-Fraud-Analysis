library(tidyverse)

#Frauds by gender
#We will examine whether one gender is more susceptible to fraud than the other
gen_count <- fraud_transactions %>% count(gender)

names(gen_count)[2] <- "fraud_count"

ggplot(data = gen_count, 
       aes(x = reorder(gender, -fraud_count), y = fraud_count)) +
  geom_col(width = 0.5, fill = "cornflowerblue") +
  labs(x = "Gender", y = "Fraud Count", title = "Fraud Count Vs Gender", 
       caption = "2019 Credit Card Fraud Data") +
  geom_text(aes(y = fraud_count - 200, label = fraud_count), color = "white") +
  theme_classic()

rm(gen_count)

################################################################################

#Frauds by the card owners at different age groups
age_count <- fraud_transactions %>% count(age)

names(age_count)[2] <- "count"

#Line Graph
ggplot(data = age_count, 
       aes(x = age, y = count)) +
  geom_point() +
  geom_line() +
  geom_smooth(stat = "smooth", position = "identity", se = FALSE) +
  labs(x = "Age", y = "Fraud Count", title = "Fraud Count Vs Age", 
       caption = "2019 Credit Card Fraud Data") +
  coord_cartesian(xlim =c(15, 95)) +
  scale_x_continuous(breaks=seq(15,95,by=5)) +
  scale_y_continuous(breaks=seq(0,250,by=20)) +
  theme_light()

#Histogram
ggplot(data = fraud_transactions, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "cornflowerblue") + 
  labs(x = "age", y = "Fraud Count", title = "Fraud Count Vs Age", 
       caption = "2019 Credit Card Fraud Data") + 
  scale_x_continuous(breaks=seq(10,100,by=5)) +
  scale_y_continuous(breaks=seq(0,1200,by=100)) +
  theme_classic()

rm(age_count)

################################################################################

#Frauds in different months of a year
month_count <- fraud_transactions %>% count(trans_month) 

names(month_count)[2] <- "count"

month_level <- data.frame (
  month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov","Dec"),
  level = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
)

month_count <- merge(month_count, month_level, by.x = 'trans_month', by.y = 'month')

ggplot(data = month_count, aes(x = reorder(trans_month, level), y = count)) +
  geom_col(width = 0.7, fill= "cornflowerblue") + 
  labs(x = "Month", y = "Fraud Count", title = "Frauds in Different Months of a Year", 
       caption = "2019 Credit Card Fraud Data") + 
  theme_classic()

rm(month_count)
rm(month_level)

################################################################################

#Frauds in different days of a month
#Line Graph
day_count <- fraud_transactions %>% count(trans_day)

names(day_count)[2] <- "count"

ggplot(data = day_count, 
       aes(x = trans_day, y = count)) +
  geom_point() +
  geom_line() +
  geom_smooth(stat = "smooth", position = "identity", se = FALSE) +
  labs(x = "Days of a Month", y = "Fraud Count", title = "Fraud Count on Different Days of a month", 
       caption = "2019 Credit Card Fraud Data") + 
  coord_cartesian(xlim =c(0, 32)) +
  scale_x_continuous(breaks=seq(1,32,by=1)) +
  scale_y_continuous(breaks=seq(0,270,by=10)) +
  theme_light()

#Histogram
ggplot(data = fraud_transactions, aes(x = trans_day)) +
  geom_histogram(binwidth = 1, fill = "cornflowerblue") +
  labs(x = "Days of a Month", y = "Fraud Count", title = "Fraud Count on Different Days of Month", 
       caption = "2019 Credit Card Fraud Data") + 
  scale_x_continuous(breaks=seq(1,31,by=1)) +
  scale_y_continuous(breaks=seq(0,270,by=20)) +
  theme_light()

rm(day_count)

################################################################################

#Frauds in different days of week 
week_day_count <- fraud_transactions %>% count(trans_week_day) 

names(week_day_count)[2] <- "count"

week_level <- data.frame (
  day = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat","Sun"),
  level = c(1, 2, 3, 4, 5, 6, 7)
)

week_day_count <- merge(week_day_count,week_level, by.x = 'trans_week_day', by.y = 'day')

ggplot(data = week_day_count, aes(x = reorder(trans_week_day, level), y = count)) +
  geom_col(width = 0.7, fill= "cornflowerblue") +
  labs(x = "Days of Week", y = "Fraud Count", title = "Frauds on Different Days of Weeks", 
       caption = "2019 Credit Card Fraud Data") +
  theme_classic()

rm(week_day_count)
rm(week_level)

################################################################################

#Hourly Trend of Frauds
hourly <- fraud_transactions %>% count(trans_hour)

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
  labs(x = "Hour", y = "Fraud Count", title = "Hourly trend of frauds", 
       caption = "2019 Credit Card Fraud Data") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 

rm(hourly)
rm(hour_lvl)

################################################################################

#Frauds by birth month of card owner
birth_month_count <- fraud_transactions %>% count(birth_month) 

names(birth_month_count)[2] <- "count"

month_level <- data.frame (
  month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov","Dec"),
  level = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
)

birth_month_count <- merge(birth_month_count, month_level, by.x = 'birth_month', by.y = 'month')

ggplot(data = birth_month_count, aes(x = reorder(birth_month, level), y = count)) +
  geom_col(width = 0.7, fill= "cornflowerblue") +
  labs(x = "Birth month of Card Owner", y = "Fraud Count", title = "Frauds Vs Bith Month",
       subtitle = "Frauds by card owners born in different months",
       caption = "2019 Credit Card Fraud Data") +
  theme_classic()

rm(birth_month_count)
rm(month_level)

################################################################################

#Fraud Count by individuals
card_owners <- fraud_transactions %>% group_by(card_owner, street)  %>% 
  count()  

names(card_owners)[3] <- "count"

card_owners <- card_owners %>% 
  arrange(desc(count)) %>% head(10)

ggplot(data = card_owners, 
       aes(x = reorder(card_owner, -count) , y = count)) +
  geom_col(width = 0.7, fill= "cornflowerblue") +
  labs(x = "Card Owner", y = "Fraud Count", title = "Frauds by each Card Owner",
       subtitle = "Top 10 most frauds by Card Owners",
       caption = "2019 Credit Card Fraud Data") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=30, hjust=1))

rm(card_owners)

################################################################################

#Frauds by different job holders
job_type_count <- fraud_transactions %>% count(job) 

names(job_type_count)[1] <- "job_type"
names(job_type_count)[2] <- "count"

job_type_count <- job_type_count %>% 
  arrange(desc(count)) %>% head(10)

ggplot(data = job_type_count, 
       aes(x = reorder(job_type, -count) , y = count)) +
  geom_col(width = 0.7, fill= "cornflowerblue") +
  labs(x = "Job Type", y = "Fraud Count", title = "Frauds by each Job Type",
       subtitle = "Top 10 most frauds by different Job Types",
       caption = "2019 Credit Card Fraud Data") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=30, hjust=1))

rm(job_type_count)

################################################################################

#Fraud at different Merchants 
merchant <- fraud_transactions %>% group_by(merchant)  %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(10)

ggplot(data = merchant, 
       aes(x = reorder(merchant, -n) , y = n)) +
  geom_col(width = 0.7, fill= "cornflowerblue") +
  labs(x = "Merchant", y = "Fraud Count", title = "Frauds on Different Merchants",
       subtitle = "Top 10 mechants w.r.t. fraud count",
       caption = "2019 Credit Card Fraud Data") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=30, hjust=1))

rm(merchant)

################################################################################

#Fraud on different Merchant's Categories 
merchant_category <- fraud_transactions %>% group_by(category)  %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(5)

ggplot(data = merchant_category, 
       aes(x = reorder(category, -n) , y = n)) +
  geom_col(width = 0.7, fill= "cornflowerblue") +
  labs(x = "Merchant's Category", y = "Fraud Count", title = "Frauds Vs Merchant Category") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=30, hjust=1))

rm(merchant_category)

################################################################################

#Fraud Amount 
ggplot(data = fraud_transactions, aes(x = amount)) +
  geom_histogram(binwidth = 100, fill = "cornflowerblue") +
  labs(x = "Amount", y = "Fraud Count", title = "Frauds w.r.t. Fraud Amount", 
       caption = "2019 Credit Card Fraud Data") + 
  scale_x_continuous(breaks=seq(1, 1500,by=100)) +
  scale_y_continuous(breaks=seq(0,1300,by=100)) +
  theme_classic()

#Fraud amount less than 100
less_100 <- fraud_transactions %>% filter(amount <= 100)

ggplot(data = less_100, aes(x = amount)) +
  geom_histogram(binwidth = 5, fill = "cornflowerblue") +
  labs(x = "Amount", y = "Fraud Count", title = "Frauds w.r.t. Fraud Amount less than 100$", 
       caption = "2019 Credit Card Fraud Data") + 
  scale_x_continuous(breaks=seq(1, 100,by=5)) +
  scale_y_continuous(breaks=seq(0,450,by=50)) +
  theme_light()

rm(less_100)

################################################################################

#Fraud amount by top 10 fraudsters
card_owners <- fraud_transactions %>% group_by(card_owner, street)  %>% 
  count() %>% arrange(desc(n)) %>% head(10)

top_10_co <- merge(fraud_transactions, card_owners, by = "card_owner")

ggplot(data = top_10_co, aes(x = amount)) +
  geom_histogram(binwidth = 100, fill = "cornflowerblue") +
  labs(x = "Amount", y = "Fraud Count", title = "Frauds w.r.t. Fraud Amount by top 10 Fraudsters", 
       caption = "2019 Credit Card Fraud Data") + 
  scale_x_continuous(breaks=seq(1, 1500,by=100)) +
  scale_y_continuous(breaks=seq(0,50,by=10)) +
  theme_light()

rm(card_owners)
rm(top_10_co)

################################################################################

#Top 10 states where most frauds occur
state <- fraud_transactions %>% group_by(state) %>% 
  count() %>%  arrange(desc(n)) %>% head(10)

ggplot(data = state, 
       aes(x = reorder(state, -n) , y = n)) +
  geom_col(width = 0.7, fill= "cornflowerblue") +
  labs(x = "State", y = "Fraud Count", 
       title = "Frauds in Different States", 
       caption = "2019 Credit Card Fraud Data") +
  theme_classic()

rm(state)

################################################################################

#Top 10 cities where most frauds occur
city <- fraud_transactions %>%
  group_by(city_state) %>% 
  count()  %>%  arrange(desc(n)) %>% head(10)

ggplot(data = city, 
       aes(x = reorder(city_state, -n) , y = n)) +
  geom_col(width = 0.7, fill= "cornflowerblue") +
  labs(x = "City, State", y = "Fraud Count", 
       title = "Frauds in Different Cities", 
       caption = "2019 Credit Card Fraud Data") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=30, hjust=1))

rm(city)

################################################################################

#Frauds Vs City to Population Ratio 
frauds_by_city <- fraud_transactions %>% group_by(city_state, city_pop)  %>% 
  count() 

names(frauds_by_city)[3] <- "count"

frauds_by_city <- frauds_by_city %>% 
  mutate(fraud_to_pop = (count / city_pop) * 100) 

top_cities_fraud_percent <- frauds_by_city %>% 
  arrange(desc(fraud_to_pop)) %>% head(10)

ggplot(data = top_cities_fraud_percent, 
       aes(x = reorder(city_state, -fraud_to_pop) , y = fraud_to_pop)) +
  geom_col(width = 0.7, fill= "cornflowerblue") +
  labs(x = "City, State", y = "Fraud to Population Ratio", 
       title = "Cities having Highest Fraud to Population Ratio", 
       caption = "2019 Credit Card Fraud Data") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=30, hjust=1)) 

rm(frauds_by_city)





