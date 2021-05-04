
## Machine Learning

## ML Packages
library('psych')
library('rpart')
library('rpart.plot')
library('lattice')
library('Cubist')



## Data Clean



allegations_ML <- data.frame(allegations$mos_ethnicity, allegations$complainant_ethnicity, allegations$fado_type, 
                             allegations$rank_incident, allegations$precinct, allegations$board_disposition)

allegations_ML <- rename(allegations_ML, c('mos_ethnicity' = 'allegations.mos_ethnicity',
                                           'complainant_ethnicity' = 'allegations.complainant_ethnicity',
                                           'fado_type'= 'allegations.fado_type',
                                           'rank_incident' = 'allegations.rank_incident',
                                           'precinct' = 'allegations.precinct',
                                           'board_disposition' = 'allegations.board_disposition'))


### Police 75
police_75 <- police %>% 
  filter(police$precinct == 75)



## C5.0 Tests: Police

## Sample
big_sample <- sample(1:26279, 10000)

## Check Sample
str(big_sample)




### Test and Training
big_training <- police[big_sample, ]
big_test <- police[-big_sample, ]

## Prop table test and training
prop.table(table(big_training$board_disposition))
prop.table(table(big_test$board_disposition))           


big_model <- C5.0(x = big_training[c(1,2,3,4,5,7,9)],
                  y = as.factor(big_training$board_disposition),
                  trials = 10)

summary(big_model)


#### C5: 75

## Sample
big_sample <- sample(1:1680, 800)

## Check Sample
str(big_sample)




### Test and Training
big_training <- police_75[big_sample, ]
big_test <- police_75[-big_sample, ]

## Prop table test and training
prop.table(table(big_training$board_disposition))
prop.table(table(big_test$board_disposition))           


big_model <- C5.0(x = big_training[c(1,2,3,4,5,7,9)],
                  y = as.factor(big_training$board_disposition),
                  trials = 10)

summary(big_model)


