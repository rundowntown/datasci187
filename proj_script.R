

library(ggplot2)
library(dplyr)

allegations <- read.csv("allegations_202007271729.csv")

names(allegations)



###### New Column: Officer Full Name (combined first and last)
allegations$full_name <- paste(allegations$first_name, allegations$last_name)





###
# Officer Name Frequency
###
officer_freq <- allegations %>% 
  group_by(full_name) %>% tally


## descending order for viewing ease




### 
#
# Unique ID Frequency
#
###


### - Notes: Names and IDs to not match: 3996 to 3958.  Some officers likely have overlapping names; id is better identifier



precinct_freq <- allegations %>% 
  group_by(precinct) %>% tally


id_freq <- allegations %>% 
  group_by(unique_mos_id) %>% tally
