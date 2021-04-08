

library(ggplot2)
library(dplyr)
library(ggrepel)

allegations <- read.csv("allegations_202007271729.csv")

names(allegations)



###### New Column: Officer Full Name (combined first and last)
allegations$full_name <- paste(allegations$first_name, allegations$last_name)





###
# Officer Name Frequency
###

officer_freq <- allegations %>% 
  group_by(full_name) %>% tally

# Top down order
officer_freq <- officer_freq %>% 
  arrange(desc(n))

head(officer_freq, 15)



###
# Officer ID Frequency
###

id_freq <- allegations %>% 
  group_by(unique_mos_id) %>% tally

# Top down order
id_freq <- id_freq %>% 
  arrange(desc(n))

head(id_freq, 15)



### - Notes: Names and IDs to not match: 3996 to 3958.  Some officers likely have overlapping names; id is better identifier


###
# Precinct frequency
###

precinct_freq <- allegations %>% 
  group_by(precinct) %>% tally

precinct_freq <- precinct_freq %>% 
  

head(precinct_freq, 15)


## geom_point of complaints in each precinct
ggplot(data = precinct_freq,
       mapping = aes(x = precinct, y = n))+
  geom_point()+
  geom_label(x = 185, y = 2172, label = "Precinct 75")+
  labs(title = "Number of complaints in each Precinct")



################# Precinct 75 ################

## Precinct 75 Notes:

* Historically corrupt precinct in NYPD
* Covers an area in Brooklyn, NY
* By far the most numerous complain precinct in our data-set


NYPD Page: https://www1.nyc.gov/site/nypd/bureaus/patrol/precincts/75th-precinct.page

Documentary Wikipedia Info: https://en.wikipedia.org/wiki/The_Seven_Five

Recent article on misconduct complaints: https://theintercept.com/2020/08/23/nypd-75th-precinct-police-misconduct/



## High score champion: Precinct 75
  
## Allegation data for Precinct 75
precinct_75 <- allegations %>% 
  filter(allegations$precinct == 75)


## Allegations Data minus Precinct 75
allegations_no_75 <- allegations %>% 
  filter(allegations$precinct != 75)




## Precinct 75 allegation freq
precinct_75_allegation <- precinct_75 %>% 
  group_by(allegation) %>% tally %>% 
  arrange(desc(n))

head(precinct_75_allegation, 15)

##top 15
top_15 <- precinct_75_allegation %>% 
  filter(n >= 35)




## Precinct 75: Top 15 Allegations
ggplot(data = top_15,
       mapping = aes(x = allegation, y = n, fill = allegation))+
  geom_bar(stat = 'identity', alpha = .8, color = 'black')+
  theme_dark()+
  scale_fill_discrete()+
  guides(fill = FALSE, alpha = FALSE, size = FALSE, color = FALSE)+
  labs(title = "Precinct 75: Top 15 Allegations", x = "Allegation", y = "Frequency (n)")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 10))
  


## Precinct 75: Complaint Ethnicity (Year Wrap)
ggplot(data = precinct_75,
       mapping = aes(x = complainant_ethnicity, y = ..count.., fill = complainant_ethnicity))+
  geom_bar()+
  facet_wrap(~ year_received)+
  labs(title = "Precinct 75 Complaintant Ethnicity")+
  theme(axis.text.x=element_blank())



## Precinct 75: Officer Ethnicity (Year Wrap)
ggplot(data = precinct_75,
       mapping = aes(x = mos_ethnicity, y = ..count.., fill = mos_ethnicity))+
  geom_bar()+
  facet_wrap(~ year_received)+
  labs(title = "Precinct 75 Officer Ethnicity")+
  theme(axis.text.x=element_blank())




######



ggplot()+
  geom_bar(data = allegations_no_75,
           mapping = aes(x = complainant_ethnicity, y = ..count.., fill = complainant_ethnicity))+
  geom_bar()+
  labs(title = "NYDP Allegations Complaintant Ethnicity", subtitle = "No Precinct 75")+
  theme(axis.text.x=element_blank())



ggplot()+
  geom_bar(data = precinct_75,
           mapping = aes(x = complainant_ethnicity, y = ..count.., fill = complainant_ethnicity))+
  geom_bar()+
  labs(title = "NYDP Allegations Complaintant Ethnicity", subtitle = "No Precinct 75")+
  theme(axis.text.x=element_blank())

