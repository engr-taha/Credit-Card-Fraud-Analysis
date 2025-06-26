library(tidyverse)

fraud_trans_t <- fraud_transactions %>% 
  filter(trans_hour == 22 | trans_hour == 23)

#Frauds by gender
#We will examine whether one gender is more susceptible to fraud than the other
gen_count <- fraud_trans_t %>% count(gender)

names(gen_count)[2] <- "fraud_count"

ggplot(data = gen_count, 
       aes(x = reorder(gender, -fraud_count), y = fraud_count)) +
  geom_col(width = 0.5, fill = "cornflowerblue") +
  labs(x = "Gender", y = "Fraud Count", title = "Fraud Count Vs Gender",
       subtitle = "Frauds between 10 PM to 12 PM",
       caption = "2019 Credit Card Fraud Data") +
  geom_text(aes(y = fraud_count - 200, label = fraud_count), color = "white") +
  theme_classic()

rm(gen_count)

################################################################################

#Frauds by males
male_count <- fraud_trans_t %>% filter(gender == "M") %>% count(card_owner) %>% 
  arrange(desc(n)) %>% head(5)

names(male_count)[2] <- "fraud_count"

ggplot(data = male_count, 
       aes(x = reorder(card_owner, -fraud_count), y = fraud_count)) +
  geom_col(width = 0.5, fill = "cornflowerblue") +
  labs(x = "Gender", y = "Fraud Count", title = "Top 5 male fraudsters",
       subtitle = "Frauds between 10 PM to 12 PM",
       caption = "2019 Credit Card Fraud Data") +
  theme_classic() +
  scale_y_continuous(breaks=seq(0, 14,by=2)) +
  theme(axis.text.x=element_text(angle=30, hjust=1))

rm(male_count)

################################################################################

#Frauds by females
fem_count <- fraud_trans_t %>% filter(gender == "F") %>% count(card_owner) %>% 
  arrange(desc(n)) %>% head(5)

names(fem_count)[2] <- "fraud_count"

ggplot(data = fem_count, 
       aes(x = reorder(card_owner, -fraud_count), y = fraud_count)) +
  geom_col(width = 0.5, fill = "cornflowerblue") +
  labs(x = "Gender", y = "Fraud Count", title = "Top 5 female fraudsters",
       subtitle = "Frauds between 10 PM to 12 PM",
       caption = "2019 Credit Card Fraud Data") +
  theme_classic() +
  scale_y_continuous(breaks=seq(0, 14,by=2)) +
  theme(axis.text.x=element_text(angle=30, hjust=1))

rm(fem_count)

################################################################################

#Fraud at different Merchants 
merchant <- fraud_trans_t %>% group_by(merchant)  %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(10)

ggplot(data = merchant, 
       aes(x = reorder(merchant, -n) , y = n)) +
  geom_col(width = 0.7, fill= "cornflowerblue") +
  labs(x = "Merchant", y = "Fraud Count", title = "Frauds on Different Merchants",
       subtitle = "Frauds between 10 PM to 12 PM",
       caption = "2019 Credit Card Fraud Data") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=30, hjust=1))

rm(merchant)

################################################################################

#Fraud on different Merchant's Categories 
merchant_category <- fraud_trans_t %>% group_by(category)  %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(10)

ggplot(data = merchant_category, 
       aes(x = reorder(category, -n) , y = n)) +
  geom_col(width = 0.7, fill= "cornflowerblue") +
  labs(x = "Merchant's Category", y = "Fraud Count", title = "Frauds Vs Merchant Category",
       subtitle = "Top 10 mechant's categories w.r.t. fraud count",
       caption = "2019 Credit Card Fraud Data") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=30, hjust=1))

rm(merchant_category)

################################################################################

bw_700_1100 <- fraud_trans_t %>% filter(amount <= 1100, amount >= 700)

ggplot(data = bw_700_1100, aes(x = amount)) +
  geom_histogram(binwidth = 100, fill = "cornflowerblue") +
  labs(x = "Amount", y = "Fraud Count", title = "Frauds w.r.t. Fraud Amount between 700$ to 1100$",
       subtitle = "Frauds between 10 PM to 12 PM",
       caption = "2019 Credit Card Fraud Data") + 
  scale_x_continuous(breaks=seq(700, 1100,by = 100)) +
  scale_y_continuous(breaks=seq(0,450,by = 50)) +
  theme_classic()

rm(bw_700_1100)


#Fraud amount between 300 to 400$
bw_300_400 <- fraud_trans_t %>% filter(amount <= 400, amount >= 300)

ggplot(data = bw_300_400, aes(x = amount)) +
  geom_histogram(binwidth = 10, fill = "cornflowerblue") +
  labs(x = "Amount", y = "Fraud Count", title = "Frauds w.r.t. Fraud Amount between 300$ to 400$",
       subtitle = "Frauds between 10 PM to 12 PM",
       caption = "2019 Credit Card Fraud Data") + 
  scale_x_continuous(breaks=seq(300, 400,by = 10)) +
  scale_y_continuous(breaks=seq(0,500,by = 5)) +
  theme_classic()

rm(bw_300_400)

#Fraud amount less than 100
less_100 <- fraud_trans_t %>% filter(amount <= 100)

ggplot(data = less_100, aes(x = amount)) +
  geom_histogram(binwidth = 5, fill = "cornflowerblue") +
  labs(x = "Amount", y = "Fraud Count", title = "Frauds w.r.t. Fraud Amount less than 100$",
       subtitle = "Frauds between 10 PM to 12 PM",
       caption = "2019 Credit Card Fraud Data") + 
  scale_x_continuous(breaks=seq(1, 100,by=5)) +
  scale_y_continuous(breaks=seq(0,450,by=50)) +
  theme_classic()

rm(less_100)

################################################################################

#Top 10 states where most frauds occur
state <- fraud_trans_t %>% group_by(state) %>% 
  count() %>%  arrange(desc(n)) %>% head(5)

ggplot(data = state, 
       aes(x = reorder(state, -n) , y = n)) +
  geom_col(width = 0.7, fill= "cornflowerblue") +
  labs(x = "State", y = "Fraud Count", 
       title = "Frauds in Different States", 
       subtitle = "Frauds between 10 PM to 12 PM",
       caption = "2019 Credit Card Fraud Data") +
  theme_classic()

rm(state)

################################################################################







