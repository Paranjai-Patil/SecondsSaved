library(tidyverse)
library(dplyr)
library(randomForest)
library(caret)



allvoronoi <- rbind(week_4_run_voronoi, week_5_run_voronoi, week_6_run_voronoi,
                    week_7_run_voronoi, week_8_run_voronoi)

alltracking <- rbind(week_4_tracking, week_5_tracking, week_6_tracking,
                     week_7_tracking, week_8_tracking)



#FORMAT VORONOI DATA SO THAT WE CAN BUILD MODEL 
#REPEAT FOR RUN AND PASS

vor_calc <- left_join(allvoronoi, plays %>% select(gameId, playId, possessionTeam),
                    by = c("gameId", "playId")) %>%
  mutate(offTeam = ifelse(club == possessionTeam, 1, 0)) %>%
  filter(offTeam == 1)

vor_calc <- vor_calc %>%
  mutate(across(-event, ~ifelse(is.na(.), 2, .)))

vor_calc <- vor_calc %>%
  group_by(gameId, playId, frameId) %>%
  summarize(across(starts_with("vor_"), sum, na.rm = TRUE)) %>%
  ungroup()


vor_calc <- left_join(tester2, tester %>% select(gameId, playId, frameId, tackleFrame, handoffFrame),
                     by = c("gameId", "playId", "frameId")) %>%
  mutate(timeToTackle = (tackleFrame-frameId)/10) %>%
  mutate(frameSinceHandoff = frameId-handoffFrame) %>%
  distinct(gameId, playId, frameId, .keep_all = TRUE) %>%
  #ENSURE WHEN THE BOX IS OUT OF BOUNDS NEITHER TEAM HAS VORONOI ADVANTAGE
  mutate_at(vars(starts_with("vor_")), ~ifelse(. > 1.5, 0.5, .))


#FILTER INTO TESTING AND TRAINING DATA


training_dataset <- vor_calc %>%
  filter(week == 4)

testing_dataset <- vor_calc %>%
  filter(week != 4)

trainingsubset <- training_dataset %>% 
  sample_n(10000)



input_vars <- c(
  "vor_1", "vor_2", "vor_3", "vor_4", "vor_5", "vor_6", "vor_7", "vor_8", "vor_9", "vor_10",
  "vor_11", "vor_12", "vor_13", "vor_14", "vor_15", "vor_16", "vor_17", "vor_18", "vor_19", "vor_20",
  "vor_21", "vor_22", "vor_23", "vor_24", "vor_25", "vor_26", "vor_27", "vor_28", "vor_29", "vor_30",
  "vor_31", "vor_32", "vor_33", "vor_34", "vor_35", "vor_36", "vor_37", "vor_38", "vor_39", "vor_40",
  "vor_41", "vor_42", "vor_43", "vor_44", "vor_45", "vor_46", "vor_47", "vor_48", "vor_49", "vor_50",
  "vor_51", "vor_52", "vor_53", "vor_54", "vor_55", "vor_56", "vor_57", "vor_58", "vor_59", "vor_60",
  "vor_61", "vor_62", "vor_63", "vor_64", "vor_65", "vor_66", "vor_67", "vor_68", "vor_69", "vor_70",
  "vor_71", "vor_72", "vor_73", "vor_74", "vor_75", "vor_76", "vor_77", "vor_78", "vor_79", "vor_80",
  "vor_81", "vor_82", "vor_83", "vor_84", "vor_85", "vor_86", "vor_87", "vor_88", "vor_89", "vor_90",
  "vor_91", "vor_92", "vor_93", "vor_94", "vor_95", "vor_96", "vor_97", "vor_98", "vor_99", "vor_100",
  "vor_101", "vor_102", "vor_103", "vor_104", "vor_105", "vor_106", "vor_107", "vor_108", "vor_109", "vor_110",
  "vor_111", "vor_112", "vor_113", "vor_114", "vor_115", "vor_116", "vor_117", "vor_118", "vor_119", "vor_120",
  "frameId", "frameSinceHandoff", "timeToTackle", "x_std", "y_std", "o_std", "dir_std", "s", "a", "pastLOS",
  )

trainingsubset <- subset(trainingsubset, select = input_vars)
ctrl <- trainControl(method = "cv", number = 5)


#TRAIN RANDOM FOREST MODEL
rf_model <- train(timeToTackle ~ ., data = trainingsubset, method = "rf", trControl = ctrl, ntree = 50)


prediction <- predict(rf_model, testing_dataset[, input_vars])
accuracy <- (mean((prediction - testing_dataset$timeToTackle)^2))
#ENSURE ACCURACY
cat("Mean Squared Error (MSE):", accuracy, "\n")

#MSE OF AROUND 0.7 FOR BOTH MODELS
#APPLY PREDICTIONS
testing_dataset$timeToTacklePred <- prediction



expected_VOR <- rbind(exp_week_7_voronoi, exp_week_8_voronoi)

#APPLY PREDICTIONS TO VORONOI MODEL AS WELL
prediction_vor <- predict(rf_model, expected_VOR[, input_vars])
expected_VOR$timeToTacklePred <- prediction_vor




#FORMAT BOTH DATASETS WITH PREDICTIONS

SecondsSaved <- left_join(expected_VOR, testing_dataset %>% select(gameId, playId, frameId, timeToTacklePred), by = c("gameId", "playId", "frameId")) %>%
  mutate(secondsSaved = timeToTacklePred.y - timeToTacklePred.x) %>%
  select(gameId, playId, frameId, pred_player, secondsSaved)


SecondsSaved <- SecondsSaved %>%
  group_by(gameId, playId, pred_player) %>%
  summarize(
    total_secondsSaved = sum(secondsSaved),
    count = n_distinct(pred_player) 
  )






