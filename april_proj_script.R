

library(ggplot2)
library(dplyr)
library(ggrepel)


## Read in data
allegations <- read.csv("allegations_202007271729.csv")

## Check names
names(allegations)


###### New Column: Officer Full Name (combined first and last)
allegations$full_name <- paste(allegations$first_name, allegations$last_name)



## Board disposition transformation
## Substantiated variants merged into one category: 'Substantiated"

table(allegations$board_disposition)

allegations$board_disposition[allegations$board_disposition == 'Substantiated (Charges)'] <- 'Substantiated'
allegations$board_disposition[allegations$board_disposition == 'Substantiated (Command Discipline A)'] <- 'Substantiated'
allegations$board_disposition[allegations$board_disposition == 'Substantiated (Command Discipline B)'] <- 'Substantiated'
allegations$board_disposition[allegations$board_disposition == 'Substantiated (Command Discipline)'] <- 'Substantiated'
allegations$board_disposition[allegations$board_disposition == 'Substantiated (Command Lvl Instructions)'] <- 'Substantiated'
allegations$board_disposition[allegations$board_disposition == 'Substantiated (Formalized Training)'] <- 'Substantiated'
allegations$board_disposition[allegations$board_disposition == 'Substantiated (Instructions)'] <- 'Substantiated'
allegations$board_disposition[allegations$board_disposition == 'Substantiated (MOS Unidentified)'] <- 'Substantiated'
allegations$board_disposition[allegations$board_disposition == 'Substantiated (No Recommendations)'] <- 'Substantiated'




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

# * Historically corrupt precinct in NYPD
# * Covers an area in Brooklyn, NY
# * By far the most numerous complain precinct in our data-set
# 
# 
# NYPD Page: https://www1.nyc.gov/site/nypd/bureaus/patrol/precincts/75th-precinct.page
# 
# Documentary Wikipedia Info: https://en.wikipedia.org/wiki/The_Seven_Five
# 
# Recent article on misconduct complaints: https://theintercept.com/2020/08/23/nypd-75th-precinct-police-misconduct/



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












############ Last 10 years #############


## last 10 years
recent_allegations <- allegations %>% 
  filter(allegations$year_received >= 2009)

precinct_75_last10 <- recent_allegations %>% 
  filter(recent_allegations$precinct == 75)


##### Allegations Data minus Precinct 75 #####
allegations_no_75_last10 <- recent_allegations %>% 
  filter(recent_allegations$precinct != 75)


## Board disposition 75 vs field

#75
g1 <- ggplot(data = precinct_75_last10,
             mapping = aes(x = board_disposition, y = ..count.., fill = board_disposition))+
  geom_bar()+
  labs(title = "Precinct 75 Board Disposition", subtitle = "Last 10 Years")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 10))+
  guides(fill = FALSE)+
  ylim(0,1000)

## Rest of precincts
g2 <- ggplot(data = allegations_no_75_last10,
             mapping = aes(x = board_disposition, y = ..count../77, fill = board_disposition))+
  geom_bar()+
  labs(title = "NYDP All Other Precincts: Average", subtitle = "Last 10 Years")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 10))+
  guides(fill = FALSE)+
  ylim(0,1000)

ggarrange(g1,g2)


## Number of precincts in last 10 years (not including 75): 77
number_of_precincts <- allegations_no_75_last10 %>% 
  group_by(precinct) %>% tally





#### Complaints 75 vs Field last 10

g1 <- ggplot(data = precinct_75_last10,
             mapping = aes(x = fado_type, y = ..count.., fill = board_disposition))+
  geom_bar()+
  labs(title = "Precinct 75 Complaints", subtitle = "Last 10 Years")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 10))+
  guides(fill = FALSE)+
  ylim(0,1000)

## Rest of precincts
g2 <- ggplot(data = allegations_no_75_last10,
             mapping = aes(x = fado_type, y = ..count../77, fill = board_disposition))+
  geom_bar()+
  labs(title = "NYPD All Other Precinct Complaints, Average", subtitle = "Last 10 Years")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 10))+
  guides()+
  ylim(0,1000)

ggarrange(g1,g2)




##### Ethnicity


g1 <- ggplot(data = precinct_75_last10,
             mapping = aes(x = fado_type, y = ..count.., fill = board_disposition))+
  geom_bar()+
  labs(title = "Precinct 75 Complaints", subtitle = "Last 10 Years")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 10))+
  guides(fill = FALSE)+
  ylim(0,1000)


## Rest of precincts
g2 <- ggplot(data = allegations_no_75_last10,
             mapping = aes(x = fado_type, y = ..count../77, fill = board_disposition))+
  geom_bar()+
  labs(title = "NYPD All Other Precinct Complaints, Average", subtitle = "Last 10 Years")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 10))+
  guides()+
  ylim(0,1000)

ggarrange(g1,g2)








g1 <- ggplot(data = precinct_75_last10,
       mapping = aes(x = complainant_ethnicity, y = ..count.., fill = complainant_ethnicity,
                     alpha = .9))+
  geom_bar(color = "black")+
  facet_wrap(~fado_type)+
  labs(title = "Complaintant Ethnicity Bar Chart", subtitle = "Seperated by Complaint Type.  Note: First Value is Blank", caption = "Figure 2", x = "Complaint Ethnicity")+
  theme_dark()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_discrete(name = "Complaint Ethnicity")+
  guides(alpha = FALSE)+
  ylim(0,600)

g2 <- ggplot(data = allegations_no_75_last10,
       mapping = aes(x = complainant_ethnicity, y = ..count../77, fill = complainant_ethnicity,
                     alpha = .9))+
  geom_bar(color = "black")+
  facet_wrap(~fado_type)+
  labs(title = "Complaintant Ethnicity Bar Chart", subtitle = "Seperated by Complaint Type.  Note: First Value is Blank", caption = "Figure 2", x = "Complaint Ethnicity")+
  theme_dark()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_discrete(name = "Complaint Ethnicity")+
  guides(alpha = FALSE)+
  ylim(0,600)

ggarrange(g1,g2)










##### Board Disposition by Officer Ethnicity | Last 10 Years


## Filter extremely low value
allegations_no_75_last10_OE <- allegations_no_75_last10 %>% 
  filter(mos_ethnicity != "American Indian")


#75
g1 <- ggplot(data = precinct_75_last10,
       mapping = aes(x = board_disposition, fill = board_disposition))+
  geom_bar(color = "black")+
  facet_wrap(~mos_ethnicity)+
  theme_cleveland()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(title = "Precinct 75 Board Disposition Outcome", subtitle = "Seperated by Officer Ethnicity", caption = "Last 10 Years", x = "Board Disposition (Complaint Outcome)")+
  scale_fill_futurama(name = "Complaint Outcome")+
  ylim(0,470)

#NYPD
g2<- ggplot(data = allegations_no_75_last10_OE,
       mapping = aes(x = board_disposition, y = ..count../77, fill = board_disposition))+
  geom_bar(color = "black")+
  facet_wrap(~mos_ethnicity)+
  theme_cleveland()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(title = "NYPD Board Disposition Outcome", subtitle = "Seperated by Officer Ethnicity", caption = "", x = "Board Disposition (Complaint Outcome)")+
  scale_fill_futurama(name = "Complaint Outcome")+
  ylim(0,460)


ggarrange(g1,g2)


#########



######## Outcome by Complainant Ethnicity
## Considered filter(), decided was better to include in combined group

## Low Value Clean
#75
precinct_75_last10_CE <- precinct_75_last10
precinct_75_last10_CE$complainant_ethnicity[precinct_75_last10_CE$complainant_ethnicity == 'American Indian'] <- 'Other Race'
precinct_75_last10_CE$complainant_ethnicity[precinct_75_last10_CE$complainant_ethnicity == 'Refused'] <- 'Other Race'
precinct_75_last10_CE$complainant_ethnicity[precinct_75_last10_CE$complainant_ethnicity == 'Unknown'] <- 'Other Race'
precinct_75_last10_CE$complainant_ethnicity[precinct_75_last10_CE$complainant_ethnicity == ''] <- 'Other Race'
## Rest of NYPD
allegations_no_75_last10_CE <- allegations_no_75_last10
allegations_no_75_last10_CE$complainant_ethnicity[allegations_no_75_last10_CE$complainant_ethnicity == 'American Indian'] <- 'Other Race'
allegations_no_75_last10_CE$complainant_ethnicity[allegations_no_75_last10_CE$complainant_ethnicity == 'Refused'] <- 'Other Race'
allegations_no_75_last10_CE$complainant_ethnicity[allegations_no_75_last10_CE$complainant_ethnicity == 'Unknown'] <- 'Other Race'
allegations_no_75_last10_CE$complainant_ethnicity[allegations_no_75_last10_CE$complainant_ethnicity == ''] <- 'Other Race'

#75
g1 <- ggplot(data = precinct_75_last10_CE,
             mapping = aes(x = board_disposition, fill = board_disposition))+
  geom_bar(color = "black")+
  facet_wrap(~complainant_ethnicity)+
  theme_cleveland()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(title = "Precinct 75 Board Disposition Outcome", subtitle = "Seperated by Complainant Ethnicity", caption = "Last 10 Years", x = "Board Disposition (Complaint Outcome)")+
  scale_fill_futurama(name = "Complaint Outcome")+
  ylim(0,540)

#NYPD
g2<- ggplot(data = allegations_no_75_last10_CE,
            mapping = aes(x = board_disposition, y = ..count../77, fill = board_disposition))+
  geom_bar(color = "black")+
  facet_wrap(~complainant_ethnicity)+
  theme_cleveland()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(title = "NYPD Board Disposition Outcome", subtitle = "Seperated by Complainant Ethnicity", caption = "", x = "Board Disposition (Complaint Outcome)")+
  scale_fill_futurama(name = "Complaint Outcome")+
  ylim(0,540)


ggarrange(g1,g2)









######


ggplot(data = allegations







#

precinct_75_last10_CE <- precinct_75_last10 %>% 
  filter(complainant_ethnicity != "American Indian" & complainant_ethnicity != "Refused" & complainant_ethnicity != "Unknown" & complainant_ethnicity != '')

allegations_no_75_last10_CE <- allegations_no_75_last10 %>% 
  filter(complainant_ethnicity != "American Indian" & complainant_ethnicity != "Refused" & complainant_ethnicity != "Unknown" & complainant_ethnicity != '')



