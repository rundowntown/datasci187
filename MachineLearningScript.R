
## Machine Learning

## ML Packages
library('psych')
library('rpart')
library('rpart.plot')
library('lattice')
library('Cubist')

## NAs and blanks causing errors
test_75 <- lm(board_disposition ~ command_at_incident + rank_now + mos_ethnicity +
                mos_gender + mos_age_incident + complainant_ethnicity + 
                complainant_gender + complainant_age_incident + fado_type +
                allegation + outcome_description, data = precinct_75)

summary(test_75)

## Still not running - trying remove black next
precinct_75NA <- precinct_75 %>% drop_na()

test_75 <- lm(board_disposition ~ command_at_incident + rank_now + mos_ethnicity +
                mos_gender + mos_age_incident + complainant_ethnicity + 
                complainant_gender + complainant_age_incident + fado_type +
                allegation + outcome_description, data = precinct_75NA)



#### Data Frame with relevant ML categories

precinct_ML <- 


### Regression Tree Attempt 

## sample set
p_sample <- sample(1:2172, 1000)

## View sample
head(p_sample)

## Train and Test Sets
p_train <- precinct_75[p_sample,]
p_test <- precinct_75[-p_sample, ]


### .rpart algo

p_rpart <- rpart(board_disposition ~ complainant_ethnicity + 
                   mos_ethnicity + full_name, data = p_train)

p_rpart

## plot
rpart.plot(p_rpart, digits = 3,
           fallen.leave = TRUE,
           type = 5, extra = 101,
           shadow.col = 'darkgrey',
           box.palette = 'RdYlGn',
           branch = .8)


## Generate predictions

pred_rpart <- predict(p_rpart, p_test)

summary(pred_rpart)

cor(pred_rpart, p_test$board_disposition)




### Retry with cleaner values



## Tree Attempt 2

## Sample Generation
p2_sample <- sample(1:1538, 1200)

## View random
head(p2_sample, 10)

## Train and Test Sets
p2_train <- precinct_75_last10_CE[p2_sample, ]
p2_test <- precinct_75_last10_CE[-p2_sample, ]

## Summary view Board
precinct_75_last10_CE %>% group_by(board_disposition) %>% 
  count()


## Summary Vew outcome_desc
precinct_75_last10_CE %>% group_by(outcome_description) %>% 
  count()


## .rpart attempt

p2_rpart <- rpart(outcome_description ~ mos_ethnicity + complainant_ethnicity + 
                    mos_gender + complainant_gender, data = p2_train)

p2_rpart

rpart.plot(p2_rpart, digits = 3,
           fallen.leave = TRUE,
           type = 5, extra = 101,
           shadow.col = 'darkgrey',
           box.palette = 'RdYlGn',
           branch = .8)

rpart.plot(p2_rpart)




### Clean Try

## Sample Generation
big_sample <- sample(1:33358, 9000)

## View Random
head(big_sample, 10)


## Train and Test Sets
big_train <- allegations[big_sample, ]
big_test <- allegations[-big_sample, ]


##
big_rpart <- rpart(board_disposition ~ fado_type + complainant_ethnicity + mos_ethnicity +
                     precinct, data = big_train)

rpart.plot(big_rpart, digits = 3,
           fallen.leave = TRUE,
           type = 5, extra = 101,
           shadow.col = 'darkgrey',
           box.palette = 'RdYlGn',
           branch = .8)

rpart.plot(big_rpart)


