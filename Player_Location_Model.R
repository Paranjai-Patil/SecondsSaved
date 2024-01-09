library(tidyverse)
library(dplyr)
library(tidyr)
library(randomForest)
library(caret)

#REPEAT FOR PASS AND RUN

importantframes <- alltracking  %>%
  group_by(gameId, playId) %>%
  mutate(handoffFrame = ifelse(event == "handoff", frameId, NA)) %>%
  fill(handoffFrame, .direction = "downup") %>%
  mutate(tackleFrame = ifelse(event == "tackle", frameId, NA)) %>%
  fill(tackleFrame, .direction = "downup") %>%
  ungroup() %>%
  filter(!is.na(handoffFrame)) %>%
  filter(frameId >= handoffFrame) %>%
  filter(tackleFrame >= frameId)



importantframes_run$offTeam <- 0
for (i in 1:nrow(plays)) {
  current_gameId <- plays[i, "gameId"]
  current_playId <- plays[i, "playId"]
  current_offTeam <- plays[i, "possessionTeam"]
  
  matching_rows <- which(importantframes_run$gameId == current_gameId & 
                           importantframes_run$playId == current_playId &
                           importantframes_run$club == current_offTeam)
  
  if (length(matching_rows) > 0) {
    importantframes_run$offTeam[matching_rows] <- 1
  }
}


bcframes <- importantframes_run %>%
  filter(ballCarrier == 1)

blockerframes <- importantframes_run %>%
  filter(offTeam == 1) %>%
  filter(ballCarrier == 0)

defenderframes <- importantframes_run %>%
  filter(offTeam == 0) %>%
  left_join(players %>% distinct(displayName, position), by = "displayName") %>%
  group_by(displayName) %>%
  mutate(position = first(position)) %>%
  ungroup() %>%
  filter(position %in% c("OLB", "CB", "NT", "FS", "ILB", "DE", "DT", "SS", "MLB")) %>%
  mutate(position = ifelse(position %in% c("ILB", "MLB"), "LB",
                           ifelse(position %in% c("FS", "SS"), "SAF",
                                  ifelse(position %in% c("OLB", "DE"), "EDGE",
                                         ifelse(position %in% c("DT", "NT"), "IDL", position)))))


defensiveframes <- merge(defenderframes, bcframes, by = c("gameId", "playId", "frameId")) %>%
  mutate(dist1 = 0)



#FEATURE ENGINEER TO CREATE VARIABLES
#FIND DISTANCES OF BLOCKERS FROM DEFENDERS, AND DEFENDERS FROM BALL CARRIER 

defendframes <- defensiveframes %>%
  mutate(dist1 = NA_character_, dist2 = NA_character_, dist3 = NA_character_, dist4 = NA_character_, dist5 = NA_character_, 
         dist6 = NA_character_, dist7 = NA_character_, dist8 = NA_character_, dist9 = NA_character_, dist10 = NA_character_, dist11 = NA_character_)



defendframes <- defendframes %>%
  mutate(distance = sqrt((X_std.x - X_std.y)^2 + (Y_std.x - Y_std.y)^2))


defendframes <- defendframes %>%
  group_by(gameId, playId, frameId) %>%
  mutate(rank = rank(distance))

defendframes <- defendframes %>%
  group_by(gameId, playId, frameId) %>%
  arrange(rank) %>%
  mutate(across(starts_with("dist"), ~ ifelse(row_number() == parse_number(str_extract(cur_column(), "\\d+")), displayName.x, NA_character_))) %>%
  ungroup()

defendframes <- defendframes %>%
  group_by(gameId, playId, frameId) %>%
  mutate(across(starts_with("dist"), ~ifelse(any(!is.na(.)), na.omit(.), NA))) %>%
  ungroup()


defendframes <- defendframes %>%
  mutate(locX1 = NA_character_, locX2 = NA_character_, locX3 = NA_character_, locX4 = NA_character_)


defendframes <- defendframes %>%
  group_by(gameId, playId, frameId) %>%
  arrange(rank) %>%
  mutate(
    locX1 = case_when(rank == 1 ~ X_std.x,
                      TRUE ~ NA_real_),
    locX2 = case_when(rank == 2 ~ X_std.x,
                      TRUE ~ NA_real_),
    locX3 = case_when(rank == 3 ~ X_std.x,
                      TRUE ~ NA_real_),
    locX4 = case_when(rank == 4 ~ X_std.x,
                      TRUE ~ NA_real_)
  ) %>%
  ungroup()


defendframes <- defendframes %>%
  group_by(gameId, playId, frameId) %>%
  mutate(
    across(starts_with("locX"), ~ifelse(any(!is.na(.)), na.omit(.), NA_real_))
  ) %>%
  ungroup()



defendframes <- defendframes %>%
  mutate(locY1 = NA_character_, locY2 = NA_character_, locY3 = NA_character_, locY4 = NA_character_)

defendframes <- defendframes %>%
  group_by(gameId, playId, frameId) %>%
  arrange(rank) %>%
  mutate(
    locY1 = case_when(rank == 1 ~ Y_std.x, TRUE ~ NA_real_),
    locY2 = case_when(rank == 2 ~ Y_std.x, TRUE ~ NA_real_),
    locY3 = case_when(rank == 3 ~ Y_std.x, TRUE ~ NA_real_),
    locY4 = case_when(rank == 4 ~ Y_std.x, TRUE ~ NA_real_)
  ) %>% ungroup()

defendframes <- defendframes %>%
  group_by(gameId, playId, frameId) %>%
  mutate(
    across(starts_with("locY"), ~ifelse(any(!is.na(.)), na.omit(.), NA_real_))
  ) %>%
  ungroup()


defendframes <- defendframes %>%
  mutate(
    x1 = case_when(
      rank == 1 ~ locX2,
      TRUE ~ locX1
    ),
    x2 = case_when(
      rank == 1 ~ locX3,
      rank == 2 ~ locX3,
      TRUE ~ locX2
    ),
    x3 = case_when(
      rank == 1 ~ locX4,
      rank == 2 ~ locX4,
      rank == 3 ~ locX4,
      TRUE ~ locX3
    ),
    y1 = case_when(
      rank == 1 ~ locY2,
      TRUE ~ locY1
    ),
    y2 = case_when(
      rank == 1 ~ locY3,
      rank == 2 ~ locY3,
      TRUE ~ locY2
    ),
    y3 = case_when(
      rank == 1 ~ locY4,
      rank == 2 ~ locY4,
      rank == 3 ~ locY4,
      TRUE ~ locY3
    )
  )


defendframes <- defendframes %>%
  mutate(
    x1_previous = ifelse(frameId != handoffFrame.x, lag(x1), NA_real_),
    x2_previous = ifelse(frameId != handoffFrame.x, lag(x2), NA_real_),
    x3_previous = ifelse(frameId != handoffFrame.x, lag(x3), NA_real_),
    y1_previous = ifelse(frameId != handoffFrame.x, lag(y1), NA_real_),
    y2_previous = ifelse(frameId != handoffFrame.x, lag(y2), NA_real_),
    y3_previous = ifelse(frameId != handoffFrame.x, lag(y3), NA_real_)
  )


defendframes <- defendframes %>%
  mutate(
    x1_2previous = ifelse(frameId -1 > handoffFrame.x, lag(x1_previous), NA_real_),
    x2_2previous = ifelse(frameId -1 > handoffFrame.x, lag(x2_previous), NA_real_),
    x3_2previous = ifelse(frameId -1 > handoffFrame.x, lag(x3_previous), NA_real_),
    y1_2previous = ifelse(frameId -1 > handoffFrame.x, lag(y1_previous), NA_real_),
    y2_2previous = ifelse(frameId -1 > handoffFrame.x, lag(y2_previous), NA_real_),
    y3_2previous = ifelse(frameId -1 > handoffFrame.x, lag(y3_previous), NA_real_)
  )


defendframes <- defendframes %>%
  mutate(
    BCx_previous = ifelse(frameId != handoffFrame.x, lag(X_std.y), NA_real_),
    BCy_previous = ifelse(frameId != handoffFrame.x, lag(Y_std.y), NA_real_),
    BCs_previous = ifelse(frameId != handoffFrame.x, lag(s.y), NA_real_),
    BCa_previous = ifelse(frameId != handoffFrame.x, lag(a.y), NA_real_),
    BCo_previous = ifelse(frameId != handoffFrame.x, lag(O_std.y), NA_real_),
    BCdir_previous = ifelse(frameId != handoffFrame.x, lag(Dir_std.y), NA_real_),
  )

defendframes <- defendframes %>%
  mutate(
    BCx_2previous = ifelse(frameId-1 > handoffFrame.x, lag(BCx_previous), NA_real_),
    BCy_2previous = ifelse(frameId-1 > handoffFrame.x, lag(BCy_previous), NA_real_),
    BCs_2previous = ifelse(frameId-1 > handoffFrame.x, lag(BCs_previous), NA_real_),
    BCa_2previous = ifelse(frameId-1 > handoffFrame.x, lag(BCa_previous), NA_real_),
    BCo_2previous = ifelse(frameId-1 > handoffFrame.x, lag(BCo_previous), NA_real_),
    BCdir_2previous = ifelse(frameId-1 > handoffFrame.x, lag(BCdir_previous), NA_real_),
  )


defendframes <- defendframes %>%
  mutate(distnum = case_when(
    displayName.x == dist1 ~ 1,
    displayName.x == dist2 ~ 2,
    displayName.x == dist3 ~ 3,
    displayName.x == dist4 ~ 4,
    displayName.x == dist5 ~ 5,
    displayName.x == dist6 ~ 6,
    displayName.x == dist7 ~ 7,
    displayName.x == dist8 ~ 8,
    displayName.x == dist9 ~ 9,
    displayName.x == dist10 ~ 10,
    displayName.x == dist11 ~ 11,
    TRUE ~ NA_integer_
  ))


defendframes <- defendframes %>%
  mutate(
    DEFx_previous = ifelse(frameId != handoffFrame.x, lag(X_std.x), NA_real_),
    DEFy_previous = ifelse(frameId != handoffFrame.x, lag(Y_std.x), NA_real_),
    DEFs_previous = ifelse(frameId != handoffFrame.x, lag(s.x), NA_real_),
    DEFa_previous = ifelse(frameId != handoffFrame.x, lag(a.x), NA_real_),
    DEFo_previous = ifelse(frameId != handoffFrame.x, lag(O_std.x), NA_real_),
    DEFdir_previous = ifelse(frameId != handoffFrame.x, lag(Dir_std.x), NA_real_),
  )

defendframes <- defendframes %>%
  mutate(
    DEFx_2previous = ifelse(frameId-1 > handoffFrame.x, lag(DEFx_previous), NA_real_),
    DEFy_2previous = ifelse(frameId-1 > handoffFrame.x, lag(DEFy_previous), NA_real_),
    DEFs_2previous = ifelse(frameId-1 > handoffFrame.x, lag(DEFs_previous), NA_real_),
    DEFa_2previous = ifelse(frameId-1 > handoffFrame.x, lag(DEFa_previous), NA_real_),
    DEFo_2previous = ifelse(frameId-1 > handoffFrame.x, lag(DEFo_previous), NA_real_),
    DEFdir_2previous = ifelse(frameId-1 > handoffFrame.x, lag(DEFdir_previous), NA_real_),
  )




modelfactorstest <- defendframes %>%
  mutate(week = 2) %>%
  select(gameId, playId, frameId, week, nflId.x, displayName.x, club.x, X_std.x, Y_std.x,
         s.x, a.x, O_std.x, Dir_std.x, event.x, handoffFrame.x, tackleFrame.x,
         position, distnum, BCx_previous, BCy_previous, BCs_previous, BCa_previous,
         BCo_previous, BCdir_previous, BCx_2previous, BCy_2previous, BCs_2previous,
         BCa_2previous, BCo_2previous, BCdir_2previous, x1_previous, x2_previous,
         x3_previous, y1_previous, y2_previous, y3_previous, x1_2previous, 
         x2_2previous, x3_2previous, y1_2previous, y2_2previous, y3_2previous, 
         DEFx_previous, DEFy_previous, DEFs_previous, DEFa_previous,
         DEFo_previous, DEFdir_previous, DEFx_2previous, DEFy_2previous,
         DEFs_2previous, DEFa_2previous, DEFo_2previous, DEFdir_2previous) %>%
  mutate(displayName = displayName.x, club = club.x, nflId = nflId.x, x = X_std.x, y = Y_std.x, s = s.x, a = a.x, 
         o = O_std.x, dir = Dir_std.x, event = event.x, handoffFrame = handoffFrame.x,
         tackleFrame = tackleFrame.x) %>%
  select(-nflId.x, -X_std.x, -Y_std.x, -s.x, -a.x, -O_std.x, -Dir_std.x, -event.x,
         -handoffFrame.x, -tackleFrame.x, -displayName.x, -club.x) %>%
  select(displayName, everything())








unique_keys <- union(select(defenderframes, gameId, playId, frameId),
                     select(blockerframes, gameId, playId, frameId))

combined_frames <- unique_keys %>%
  full_join(defenderframes, by = c("gameId", "playId", "frameId")) %>%
  full_join(blockerframes, by = c("gameId", "playId", "frameId"),
            suffix = c(".defender", ".blocker"))

combined_frames <- combined_frames %>%
  filter(!(is.na(displayName.defender) & !is.na(displayName.blocker)))


combinedframes <- combined_frames %>%
  mutate(dist1 = NA_character_, dist2 = NA_character_, dist3 = NA_character_, dist4 = NA_character_, dist5 = NA_character_, 
         dist6 = NA_character_, dist7 = NA_character_, dist8 = NA_character_, dist9 = NA_character_, dist10 = NA_character_)



combinedframes <- combinedframes %>%
  mutate(distance = sqrt((X_std.defender - X_std.blocker)^2 + (Y_std.defender - Y_std.blocker)^2))


combinedframes <- combinedframes %>%
  group_by(gameId, playId, frameId, nflId.defender) %>%
  mutate(rank = rank(distance))

combinedframes <- combinedframes %>%
  group_by(gameId, playId, frameId, nflId.defender) %>%
  arrange(rank) %>%
  mutate(across(starts_with("dist"), ~ ifelse(row_number() == parse_number(str_extract(cur_column(), "\\d+")), displayName.blocker, NA_character_))) %>%
  ungroup()

combinedframes <- combinedframes %>%
  group_by(gameId, playId, frameId, nflId.defender) %>%
  mutate(across(starts_with("dist"), ~ifelse(any(!is.na(.)), na.omit(.), NA))) %>%
  ungroup()



combinedframes <- combinedframes %>%
  mutate(locX1 = NA_character_, locX2 = NA_character_, locX3 = NA_character_, 
         locY1 = NA_character_, locY2 = NA_character_, locY3 = NA_character_)



combinedframes <- combinedframes %>%
  mutate(locX1 = ifelse(rank == 1, X_std.blocker, NA_character_),
         locX2 = ifelse(rank == 2, X_std.blocker, NA_character_),
         locX3 = ifelse(rank == 3, X_std.blocker, NA_character_),
         locY1 = ifelse(rank == 1, Y_std.blocker, NA_character_),
         locY2 = ifelse(rank == 2, Y_std.blocker, NA_character_),
         locY3 = ifelse(rank == 3, Y_std.blocker, NA_character_),)



combinedframes <- combinedframes %>%
  group_by(gameId, playId, frameId, nflId.defender) %>%
  mutate(
    across(starts_with("locX"), ~ ifelse(any(!is.na(.)), as.character(na.omit(.)), NA_character_), .names = "new_{.col}")
  ) %>%
  ungroup()



combinedframes <- combinedframes %>%
  group_by(gameId, playId, frameId, nflId.defender) %>%
  mutate(
    across(starts_with("locY"), ~ ifelse(any(!is.na(.)), as.character(na.omit(.)), NA_character_), .names = "new_{.col}")
  ) %>%
  ungroup()

combinedframes <- combinedframes %>%
  filter(rank == 1)

combinedframes <- combinedframes %>%
  mutate(displayName = displayName.defender)


result <- modelfactorstest %>%
  left_join(
    combinedframes %>%
      select(gameId, playId, frameId, displayName, new_locX1, new_locX2,
             new_locX3, new_locY1,new_locY2, new_locY3),
    by = c("gameId", "playId", "frameId", "displayName")
  ) %>%
  rename(olX1 = new_locX1, olX2 = new_locX2,olX3 = new_locX3,olY1 = new_locY1,
         olY2 = new_locY2,olY3 = new_locY3)


result <- result %>%
  mutate(
    olx1_previous = ifelse(frameId != handoffFrame, lag(olX1), NA_real_),
    olx2_previous = ifelse(frameId != handoffFrame, lag(olX2), NA_real_),
    olx3_previous = ifelse(frameId != handoffFrame, lag(olX3), NA_real_),
    oly1_previous = ifelse(frameId != handoffFrame, lag(olY1), NA_real_),
    oly2_previous = ifelse(frameId != handoffFrame, lag(olY2), NA_real_),
    oly3_previous = ifelse(frameId != handoffFrame, lag(olY3), NA_real_)
  )


result <- result %>%
  mutate(
    olx1_2previous = ifelse(frameId -1 > handoffFrame, lag(olx1_previous), NA_real_),
    olx2_2previous = ifelse(frameId -1 > handoffFrame, lag(olx2_previous), NA_real_),
    olx3_2previous = ifelse(frameId -1 > handoffFrame, lag(olx3_previous), NA_real_),
    oly1_2previous = ifelse(frameId -1 > handoffFrame, lag(oly1_previous), NA_real_),
    oly2_2previous = ifelse(frameId -1 > handoffFrame, lag(oly2_previous), NA_real_),
    oly3_2previous = ifelse(frameId -1 > handoffFrame, lag(oly3_previous), NA_real_)
  )


#CREATE FINAL FEATURES

finalmodelstats <- result %>%
  mutate(XrelBC = DEFx_previous - BCx_previous, YrelBC = DEFy_previous - BCy_previous,
         XrelBC2 = DEFx_2previous - BCx_previous, YrelBC2 = DEFy_2previous - BCy_2previous,
         XrelLOS = DEFx_previous - losX, sinceHandoff = frameId-handoffFrame,
         XrelOL1 = DEFx_previous - olx1_previous, YrelOL1 = DEFy_previous - oly1_previous,
         XrelOL2 = DEFx_previous - olx2_previous, YrelOL2 = DEFy_previous - oly2_previous,
         XrelOL3 = DEFx_previous - olx3_previous, YrelOL3 = DEFy_previous - oly3_previous,
         bcXrelLOS = BCx_previous - losX, 
         Def1XrelBC = x1_previous - BCx_previous, Def1YrelBC = y1_previous - BCy_previous,
         Def2XrelBC = x2_previous - BCx_previous, Def2YrelBC = y1_previous - BCy_previous,
         Def3XrelBC = x3_previous - BCx_previous, Def3YrelBC = y1_previous - BCy_previous,
         chngX = BCx_previous- BCx_2previous, chngY = BCy_previous- BCy_2previous,
         chngS = BCs_previous- BCs_2previous, chngA = BCa_previous- BCa_2previous,
         chngO = BCo_previous- BCo_2previous, chngDir = BCdir_previous- BCdir_2previous,
         changeInX = x-DEFx_previous, changeInY = y-DEFy_previous) %>%
  filter(frameId-handoffFrame > 1)



#FILTER INTO TRAINING AND TESTING

training_data <- finalmodelstats %>%
  filter(week == 1)

testing_data <- finalmodelstats %>%
  filter(!(week == 1))


#REPEAT FOR EVERY POSITION

POSdata <- training_data %>%
  filter(position == pos)


POSfinaldata <- testing_data %>%
  filter(position == pos)



trainingsubset <- POSdata %>% 
  sample_n(15000) %>%
  select(-event) %>%
  na.omit()

set.seed(120)
input_vars <- c("DEFx_previous", "DEFy_previous", "BCx_previous", "BCy_previous", "BCs_previous",
                "BCa_previous", "BCo_previous", "BCdir_previous", "XrelBC", "XrelBC2", "YrelBC",
                "YrelBC2", "XrelLOS", "distnum", "bcXrelLOS", "frameId", "sinceHandoff", "XrelOL1",
                "XrelOL2", "XrelOL3", "YrelOL1", "YrelOL2", "YrelOL3", "Def1XrelBC", "Def1YrelBC",
                "Def2XrelBC", "Def2YrelBC", "Def3XrelBC", "Def3YrelBC", "chngX", "chngY", "chngS",
                "chngA", "chngO", "chngDir", "changeInX")

trainingsubset <- subset(trainingsubset, select = input_vars)
ctrl <- trainControl(method = "cv", number = 5)

#CREATE THE RANDOM FOREST MODEL
rf_model <- train(changeInX ~ ., data = trainingsubset, method = "rf", trControl = ctrl, ntree = 50)


prediction <- predict(rf_model, POSfinaldata[, input_vars])
accuracy <- (mean((prediction - POSfinaldata$changeInX)^2))
cat("Mean Squared Error (RMSE):", accuracy, "\n")

#ENSURE MODEL ACCURACY
#MAJORITY OF MODELS HAVE MSE OF 0.01 TO 0.06


#APPLY PREDICTIONS

POSfinaldata$predChangeInX <- prediction



#REPEAT FOR ALL POSITIONS X AND Y

