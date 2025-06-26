library(tidyverse)
#RFM Analysis 

#Frequency 
freq = fraud_transactions %>% count(card_owner)
 

names(freq)[2] <- "frequency"

freq =  freq %>% mutate(f = ntile(frequency, 5))

#Monetary
monetary = fraud_transactions %>% group_by(card_owner) %>% 
  summarise(monetary = sum(amount))

monetary =  monetary %>% mutate(m = ntile(monetary, 5))

#Recency
rec = fraud_transactions %>% group_by(card_owner) %>% 
  summarise(recency = max(trans_date))

rec = mutate(rec, r = ntile(rec$recency, 5))

rfm = mutate(rec, freq)
rfm = mutate(rfm, monetary)
rfm = rfm %>% mutate(rfm = m + (f*10) + (r*100)) %>% select(card_owner, rfm)

rm(freq)
rm(monetary)
rm(rec)

