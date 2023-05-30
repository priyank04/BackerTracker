# load libraries
# install.packages("predictrace")
# install.packages("scales")
#install.packages("writexl")
library(tidyverse)
library(stringi)
library(rpart.plot)
library("predictrace")
library("scales")
library(randomForest)
library("writexl")
library(readxl)

#set working directory
setwd("C:/Users/Nisarg Patel/Desktop/Spring 2022/BUDT758T - Data Mining and Predictive Analytics/DMPA Group Project Team 4")

# load data files
train_x <- read_csv("small_training_X.csv", show_col_types = FALSE)
train_y <- read_csv("small_training_y.csv", show_col_types = FALSE)
test_x <- read_csv("ks_test_X.csv", show_col_types = FALSE)

# Importing additional dataset to work on determining the gender by
# first names of project creators.
# Importing Social security data on gender of babies born by their first names
names = read_csv("baby_names.csv",show_col_types = FALSE) %>%
  mutate(firstname = toupper(name),
         flag_female = ifelse(percent_female >= .5, 1, 0)) %>%
  select(firstname, flag_female)

# combining train and test data. Also, adding a flag distinguishing both the data
train_x <- train_x %>%
  mutate(testx = "NO")
test_x <- test_x %>%
  mutate(testx = "YES")
total_train <- rbind(train_x, test_x)

### Feature Engineering

# checking unique values before moving ahead
unique(total_train$isTextPic)
unique(total_train$isCalendarPic)
unique(total_train$isDiagramPic)
unique(total_train$isLogoPic)
unique(total_train$isShapePic)
unique(total_train$contains_youtube)
unique(total_train$location_type)

# making sure all variables are clean and usable in a model
total_train <- total_train %>%
  
  # finding interquartile range to treat outliers if occurs
  mutate(
    first_quartile = quantile(goal, probs = 0.25),
    third_quartile = quantile(goal, probs = 0.75),
    iqr = third_quartile - first_quartile
  ) %>%
  
  # replacing missing categorical with an "Other" or "NULL" category
  mutate(
    isTextPic = ifelse(is.na(isTextPic), "NULL", isTextPic),
    isCalendarPic = ifelse(is.na(isCalendarPic), "NULL", isCalendarPic),
    isDiagramPic = ifelse(is.na(isDiagramPic), "NULL", isDiagramPic),
    isLogoPic = ifelse(is.na(isLogoPic), "NULL", isLogoPic),
    isShapePic = ifelse(is.na(isShapePic), "NULL", isShapePic),
    contains_youtube = factor(contains_youtube)
  ) %>%
  
  # creating bins for number of words and sentence counters in 5 categories
  mutate(
    binned_num_words = cut(
      num_words,
      breaks = 5,
      labels = c("VShort", "Short", "Med", "Long", "VLong")
    ),
    flag_num_words_long = ifelse(num_words > mean(num_words), 1, 0),
    binned_sentence_counter = cut(
      sentence_counter,
      breaks = 5,
      labels = c("VShort", "Short", "Med", "Long", "VLong")
    )
  ) %>%
  
  # converting category_parent to factor
  mutate(
    category_parent = ifelse(is.na(category_parent),  'other', tolower(category_parent)),
    category_parent = as.factor(category_parent)
  ) %>%
  
  # calculating freq of each category
  group_by(category_name) %>%
  mutate(category_name_freq = n()) %>%
  ungroup() %>%
  
  # finding projects with less than 500
  mutate(
    category_name = ifelse(
      category_name_freq < 500,
      tolower(category_parent),
      tolower(category_name)
    ),
    category_name = as.factor(category_name)
  ) %>%
  
  # replacing missing numerical values by 0
  mutate(
    numfaces_project = ifelse(is.na(numfaces_project), 0, numfaces_project),
    numfaces_creator = ifelse(is.na(numfaces_creator), 0, numfaces_creator),
    male_project = ifelse(is.na(male_project), 0, male_project),
    male_creator = ifelse(is.na(male_creator), 0, male_creator),
    female_project = ifelse(is.na(female_project), 0, female_project),
    female_creator = ifelse(is.na(female_creator), 0, female_creator)
  ) %>%
  
  # replacing missing numerical values by mean value
  # and also treating outliers alongside formatting of date
  mutate(
    smiling_project = ifelse(
      is.na(smiling_project),
      mean(smiling_project, na.rm = TRUE),
      smiling_project
    ),
    smiling_project = ifelse(smiling_project > 100, 100, smiling_project),
    smiling_creator = ifelse(
      is.na(smiling_creator),
      mean(smiling_creator, na.rm = TRUE),
      smiling_creator
    ),
    smiling_creator = ifelse(smiling_creator > 100, 100, smiling_creator),
    minage_project = ifelse(
      is.na(minage_project),
      mean(minage_project, na.rm = TRUE),
      minage_project
    ),
    minage_creator = ifelse(
      is.na(minage_creator),
      mean(minage_creator, na.rm = TRUE),
      minage_creator
    ),
    name = (str_to_lower(ifelse(
      is.na(name), "Name Not Available", name
    ))),
    creator_name = (str_to_lower(
      ifelse(is.na(creator_name), "Name Not Available", creator_name)
    )),
    blurb = str_to_lower(ifelse(
      is.na(blurb), "Description Not Available", blurb
    )),
    deadline = as.Date(deadline, format = "%Y-%m-%d"),
    deadline_year = as.numeric(format(deadline, "%Y"))
  ) %>%
  
  # separating creators name with firstname and lastname
  separate(
    creator_name,
    into = c("firstname", "lastname"),
    extra = "drop",
    remove = FALSE
  ) %>%
  
  # modifying the names to same cases i.e. upper case to work with the data ahead
  mutate(lastname = toupper(lastname),
         firstname = toupper(firstname)) %>%
  
  # joining SSA Dataset on firstname column
  left_join(names, by = "firstname") %>%
  
  # determining gender of creators using the data from SSA
  mutate(gender = case_when(
    flag_female == 1 ~ "F",
    flag_female == 0 ~ "M",
    is.na(flag_female) ~  "U"
  )) %>%
  
  # determining the race of the creators based on the lastname and firstname if former is missing
  mutate(
    race = predict_race(lastname, surname = TRUE)[3],
    race = as.character(unlist(race)),
    race = ifelse(is.na(race), "None", race)
  )

# creating 4 bins for deadline date as 4 seasons - Winter, Sping, Summer, Fall
# also separting the location column
total_train <- total_train %>%
  mutate(
    deadline_month = as.numeric(format(deadline, "%m")),
    deadline_year = as.factor(deadline_year),
    deadline_season = case_when(
      deadline_month %in% c(12, 1, 2) ~ 'Winter',
      deadline_month %in% c(3, 4, 5) ~ 'Spring',
      deadline_month %in% c(6, 7, 8) ~ 'Summer',
      deadline_month %in% c(9, 10, 11) ~ 'Fall',
      TRUE ~ 'unknown'
    ),
    deadline_season = as.factor(deadline_season),
    created_at = as.Date(created_at),
    launched_at = as.Date(launched_at),
    region = as.factor(region),
    state = as.factor(toupper(stri_sub(location_slug,-2))),
    location_slug = stri_sub(location_slug, 0,-4),
    location_slug = gsub('-', ' ', location_slug)
  ) %>%
  
  # replacing missing numerical values by "NONE" OR mean values based
  mutate(
    isbwImg1 = as.factor(ifelse(is.na(isbwImg1), 'NONE', isbwImg1)),
    color_foreground = as.factor(ifelse(
      is.na(color_foreground), 'NONE', color_foreground
    )),
    color_background = as.factor(ifelse(
      is.na(color_background), 'NONE', color_background
    )),
    accent_color = ifelse(is.na(accent_color), 'NONE', accent_color),
    captions = ifelse(is.na(captions), 'NONE', captions),
    maxage_project = ifelse(
      is.na(maxage_project),
      mean(maxage_project, na.rm = TRUE),
      maxage_project
    ),
    maxage_creator = ifelse(
      is.na(maxage_creator),
      mean(maxage_creator, na.rm = TRUE),
      maxage_creator
    )) %>% 
  
  # creating flag for foreground and background colors 
  # also determining days of work between launch date, project creation date and deadlines
  mutate(
    flag_color_foreground = ifelse(color_foreground %in% c('Black', 'White', 'Grey'), 1, 0),
    flag_color_background = ifelse(color_background %in% c('Black', 'White', 'Grey'), 1, 0),
    dead_creat = deadline - created_at,
    launch_creat = launched_at - created_at,
    launch_deadline = deadline - launched_at) %>% 
  
  # creating bins for goal and also flag for pos affin
  # finding rate for for adverbs, nouns, verbs etc.
  mutate(
    goal_bin4 = ntile(goal, 4),
    goal_bin10 = ntile(goal, 10),
    flag_overall_pos = ifelse(afinn_pos >= afinn_neg, 1, 0),
    rate_adv = ifelse(num_words > 0, ADV / num_words, 0),
    rate_noun = ifelse(num_words > 0, NOUN / num_words, 0),
    rate_adp = ifelse(num_words > 0, ADP / num_words, 0),
    rate_prt = ifelse(num_words > 0, PRT / num_words, 0),
    rate_det = ifelse(num_words > 0, DET / num_words, 0),
    rate_pron = ifelse(num_words > 0, PRON / num_words, 0),
    rate_verb = ifelse(num_words > 0, VERB / num_words, 0),
    rate_num = ifelse(num_words > 0, NUM / num_words, 0),
    rate_conj = ifelse(num_words > 0, CONJ / num_words, 0),
    rate_adj = ifelse(num_words > 0, ADJ / num_words, 0)
  ) %>%
  
  # replacing missing numerical values by "NONE"
  mutate(tag_names = (ifelse(is.na(tag_names), 'NONE', tag_names))) %>%
  
  # separating tag names
  separate(tag_names, into = ("tag_names"), extra = "drop") %>%
  mutate(tag_names = as.factor(tag_names)) %>%
  
  # grouping creators to find top 25% of creators
  group_by(creator_name) %>%
  mutate(count_creator_id = n()) %>%
  ungroup() %>%
  mutate(
    quartile_rank_creator_id = ntile(count_creator_id, 4),
    ntile_rank_creator_id = ntile(count_creator_id, 10),
    top_creator_ids = as.factor(ifelse(ntile_rank_creator_id == 10, "YES", "NO"))
  ) %>%
  
  # working with rewards columns to find max, min and count of rewards
  mutate(
    reward_amounts = ifelse(is.na(reward_amounts), 0, reward_amounts),
    all_rewards = str_split(reward_amounts, ','),
    count_rewards = lengths(all_rewards),
    min_reward = ifelse(
      count_rewards > 1,
      map(all_rewards, pluck, 1) %>% reduce(c),
      reward_amounts
    ),
    min_reward = gsub("[^0-9.-]", "0", min_reward),
    min_reward = as.numeric(min_reward),
    max_reward = ifelse(
      count_rewards > 1,
      map(all_rewards, pluck, last) %>% reduce(c),
      reward_amounts
    ),
    max_reward = gsub("[^0-9.-]", "0", max_reward),
    max_reward = as.numeric(max_reward)
  ) %>%
  
  # dropping needless variables
  select(
    -c(
      all_rewards,
      reward_amounts,
      category_name_freq,
      creator_id,
      name,
      creator_name,
      blurb,
      deadline,
      created_at,
      launched_at,
      location_slug,
      color_foreground,
      color_background,
      accent_color,
      captions,
      tag_names,
      reward_descriptions,
      firstname,
      lastname,
      state,
      first_quartile,
      third_quartile,
      iqr
    )
  )

# rescaling the numerical variables in range of 0 - 1 
total_train <- total_train %>%
  mutate(
    goal = rescale(goal),
    minage_creator = rescale(minage_creator),
    maxage_creator = rescale(maxage_creator),
    minage_project = rescale(minage_project),
    maxage_project = rescale(maxage_project),
    num_words = rescale(num_words),
    sentence_counter = rescale(sentence_counter),
    avgsentencelength = rescale(avgsentencelength)
  )

### Feature Engineering ends 
#########################################

# loading library need for classification and regression training
library(caret)
library(rpart)
library(e1071)
library(rpart.plot)
library(Metrics)

#create a one-hot encoding of all of the factor variables
dummy <- dummyVars( ~ . , data = total_train)
one_hot_kickstarter <-
  data.frame(predict(dummy, newdata = total_train))

# dropping the baseline factor variables
input_data <- one_hot_kickstarter %>%
  select(
    -c(
      location_typeCounty,
      region.ENCentral,
      category_parent.art,
      category_name.art,
      isbwImg1.FALSE,
      isTextPic0,
      isLogoPic0,
      isCalendarPic0,
      isDiagramPic0,
      isShapePic0,
      contains_youtube.0,
      binned_num_words.VShort,
      binned_sentence_counter.VShort,
      top_creator_ids.NO,
      genderF,
      raceNone,
      flag_female,
      ADV,
      NOUN,
      ADP,
      PRT,
      DET,
      PRON,
      VERB,
      NUM,
      CONJ,
      ADJ
    )
  )

# choosing the features to train the model
input_data <-
  input_data %>% select(
    c(
      goal,
      location_typeSuburb,
      region.MidAtl,
      region.NewEng,
      region.Pacific,
      category_parent.dance,
      category_parent.design,
      category_parent.fashion,
      category_parent.film...video,
      category_parent.games,
      category_parent.music,
      category_parent.publishing,
      category_parent.technology,
      category_parent.theater,
      numfaces_project,
      male_project,
      female_project,
      num_words,
      avg_wordlengths,
      contains_youtube.1,
      sentence_counter,
      avgsentencelength,
      avgsyls,
      afinn_pos,
      binned_num_words.Short,
      binned_num_words.Med,
      binned_num_words.Long,
      genderM,
      raceamerican_indian,
      racewhite,
      deadline_season.Fall,
      deadline_season.Summer,
      dead_creat,
      launch_creat,
      goal_bin10,
      rate_noun,
      rate_verb,
      count_rewards,
      max_reward,
      testxNO,
      testxYES,
      id
    )
  )

# separating the total train data back to training and testing data
train_x <- input_data %>%
  filter(testxNO == 1) %>%
  select(-c(testxYES, testxNO))
test_x <- input_data %>%
  filter(testxYES == 1) %>%
  select(-c(testxYES, testxNO))

####################### AFTER CLEANING FINAL VARIABLE train_x -train

#join the training y to the training x file
#also turn two of the target variables into factors
train <- train_x %>%
  left_join(train_y, by = "id")

# keeping backers_count and removing columns big_hit and success
train <- train %>%
  select(-c(big_hit, id, success))

# splitting the data training and validation in  80%-20% 
set.seed(1)
va_inst <- sample(nrow(train), .2 * nrow(train))
new_valid <- train[va_inst,]
new_train <- train[-va_inst,]

############################################################################################################################

################################## 1. Linear Regression ##################################
# performing Linear Regression
lr = lm(backers_count ~ ., data = new_valid)
summary(lr)
preds_backers <- predict(lr, newdata = new_valid)
new_valid$lr_pred <- preds_backers

################################## 2 & 3.Ridge and Lasso Regression ##################################

## Doing 10-Fold CV for the train data. We will fit a lasso and a ridge regression model

# Ridge (alpha = 0) and Lasso (alpha = 1)
it_train <- new_train %>%
  select(-c("backers_count"))
dtm_train <- data.matrix(it_train)

it_valid <- new_valid %>% select(-c("lr_pred","backers_count"))
dtm_valid <- data.matrix(it_valid)

# setting grid
grid <- 10 ^ seq(7,-7, length = 100)
k <- 10

library(glmnet)
# Training the model
cv_ridge <-
  cv.glmnet(
    dtm_train,
    new_train$backers_count,
    alpha = 0,
    family = "gaussian",
    lambda = grid,
    nfolds = k
  )
plot(cv_ridge)

cv_lasso <-
  cv.glmnet(
    dtm_train,
    new_train$backers_count,
    alpha = 1,
    family = "gaussian",
    lambda = grid,
    nfolds = k
  )
plot(cv_lasso)

# best lambda
ridge.optimal_lambda <- cv_ridge$lambda.min
lasso.optimal_lambda <- cv_lasso$lambda.min

# prediction on training data
ridge.predictions_train <-
  predict(cv_ridge, s = ridge.optimal_lambda, newx = dtm_train)
lasso.predictions_train <-
  predict(cv_lasso, s = lasso.optimal_lambda, newx = dtm_train)

# rmse
rmse(new_train$backers_count, ridge.predictions_train)
rmse(new_train$backers_count, lasso.predictions_train)

# prediction on testing data
ridge.preds_backers <-
  predict(cv_ridge, s = ridge.optimal_lambda, newx = dtm_valid)
lasso.preds_backers <-
  predict(cv_lasso, s = lasso.optimal_lambda, newx = dtm_valid)

# modifying negative predictions to 0
new_valid$ridge.predictions <- ridge.preds_backers
new_valid <- new_valid %>%
  mutate(ridge.predictions = ifelse(ridge.predictions < 0, 0, ridge.predictions))

new_valid$lasso.predictions <- lasso.preds_backers
new_valid <- new_valid %>%
  mutate(lasso.predictions = ifelse(lasso.predictions < 0, 0, lasso.predictions))

################################## 4. Random Forest ##################################
library(randomForest)

## Doing 5-Fold CV for the train data. We will fit a Random Forest model
control <- trainControl(method = "cv", number = 5)

# Random forest model
rf_model <-
  train(
    backers_count ~ .,
    data = new_train,
    method = "rf",
    trControl = control
  )
print(rf_model)
plot(rf_model)
saveRDS(rf_model, "rf_model.rds")

# Random forest model by tuning hyper parameter
forest_model <-
  randomForest(backers_count ~ .,
               data = new_train,
               mtry = 2,
               ntree = 1000)
print(forest_model)
saveRDS(forest_model, "rf_backers_count.rds")

# load rf-model (run if not training)
rf_backers_count_model <- readRDS("rf_backers_count.rds")

summary(rf_backers_count_model)
varImpPlot(rf_backers_count_model)

rf_pred <-
  predict(rf_backers_count_model, newdata = new_valid, type = "response")
new_valid$rf_pred <- rf_pred

################################## 5. Boosted Random Forest ##################################
# loading library for performing boosting
library(gbm)

hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    formula = backers_count ~ .,
    distribution = "gaussian",
    data = new_train,
    n.trees = 5000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

booston_boost = gbm(
  formula = backers_count ~ .,
  distribution = "gaussian",
  data = new_train,
  n.trees = 2300,
  interaction.depth = 5,
  shrinkage = 0.1,
  n.minobsinnode = 15,
  bag.fraction = .65, 
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE,
  cv.folds = 5
)  

plot(hyper_grid$optimal_trees, hyper_grid$min_RMSE)

par(mar = c(5, 11.5, 1, 1))
summary(
  booston_boost, 
  cBars = 10,
  method = relative.influence,
  las = 2,
)

# creating feature importance list
par(mar = c(5.1, 4.1, 4.1, 2.1))
vip::vip(booston_boost, cBars = 10)
min_MSE <- which.min(booston_boost$cv.error)
sqrt(booston_boost$cv.error[min_MSE])
gbm.perf(booston_boost, method = "cv")

boston_boost_tst_pred = predict(booston_boost, newdata = new_valid, n.trees = 201)
new_valid$boost <- boston_boost_tst_pred


################################## 6. Ensemble modeling ##################################

testy <- new_valid %>%
  mutate(
    lr_pred = ifelse(lr_pred < 0, 0, lr_pred),
    rf_pred = ifelse(rf_pred < 0, 0, rf_pred),
    ridge.predictions = ifelse(ridge.predictions < 0, 0, ridge.predictions),
    lasso.predictions = ifelse(lasso.predictions < 0, 0, lasso.predictions),
    boost = ifelse(boost < 0, 0, boost),
    avgpred = (lr_pred + rf_pred + ridge.predictions + boost) / 4,
    avg3pred = (lr_pred + rf_pred + boost) / 3,
    avg3pred2 = (lr_pred + rf_pred + ridge.predictions) / 3,
    weightavg = lr_pred * 0.20 + boost * 0.20 + rf_pred * 0.45 + ridge.predictions *
      0.10 + lasso.predictions * 0.05,
    med_votes = (rf_pred + lr_pred) / 2,
    max_votes = ifelse(rf_pred > lr_pred, rf_pred, lr_pred),
    min_votes = ifelse(rf_pred > boost, rf_pred, boost),
  )

preds_backers <- testy$weightavg
write.table(preds_backers, "backers_count_group4.csv", row.names = FALSE)

# comparing rmses for all different models
library(Metrics)
rmse(testy$backers_count, testy$lr_pred)
rmse(testy$backers_count, testy$ridge.predictions)
rmse(testy$backers_count, testy$lasso.predictions)
rmse(testy$backers_count, testy$rf_pred)
rmse(testy$backers_count, testy$boost)
rmse(testy$backers_count, testy$avgpred)
rmse(testy$backers_count, testy$weightavg)
rmse(testy$backers_count, testy$max_votes)
rmse(testy$backers_count, testy$min_votes)
rmse(testy$backers_count, testy$med_votes)
rmse(testy$backers_count, testy$avg3pred)
rmse(testy$backers_count, testy$avg3pred2)