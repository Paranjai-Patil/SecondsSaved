library(tidyverse)
library(dplyr)
library(utils)

#DOWNLOAD FILES
csv_files <- c("games.csv", "players.csv", "plays.csv", "tackles.csv", 
               paste0("tracking_week_", 1:8, ".csv"))

plays <- plays %>%
  filter(playNullifiedByPenalty == "N")

tracking_weeks <- list(tracking_week_1, tracking_week_2, tracking_week_3, tracking_week_4,
                       tracking_week_5, tracking_week_6, tracking_week_7, tracking_week_8)



#NORMALIZE AND CLEAN DATA
for (i in seq_along(tracking_weeks)) {
  tracking_weeks[[i]] <- tracking_weeks[[i]] %>%
    mutate(X_std = ifelse(playDirection == "left", 120 - x, x),
           Y_std = ifelse(playDirection == "left", 53.3 - y, y),
           O_std = ifelse(playDirection == "left", (o + 180) %% 360, o),
           Dir_std = ifelse(playDirection == "left", (dir + 180) %% 360, dir))
}

tracking_week_1 <- tracking_weeks[[1]]
tracking_week_2 <- tracking_weeks[[2]]
tracking_week_3 <- tracking_weeks[[3]]
tracking_week_4 <- tracking_weeks[[4]]
tracking_week_5 <- tracking_weeks[[5]]
tracking_week_6 <- tracking_weeks[[6]]
tracking_week_7 <- tracking_weeks[[7]]
tracking_week_8 <- tracking_weeks[[8]]



update_tracking <- function(tracking_week, tackles, plays) {
  tracking_week$madeTackle <- 0
  tracking_week$ballCarrier <- 0
  
  for (i in 1:nrow(tackles)) {
    current_gameId <- tackles[i, "gameId"]
    current_playId <- tackles[i, "playId"]
    current_nflId <- tackles[i, "nflId"]
    
    matching_rows <- which(tracking_week$gameId == current_gameId & 
                             tracking_week$playId == current_playId & 
                             tracking_week$nflId == current_nflId)
    
    if (length(matching_rows) > 0) {
      tracking_week$madeTackle[matching_rows] <- 1
    }
  }
  
  for (i in 1:nrow(plays)) {
    current_gameId <- plays[i, "gameId"]
    current_playId <- plays[i, "playId"]
    current_nflId <- plays[i, "ballCarrierId"]
    
    matching_rows <- which(tracking_week$gameId == current_gameId & 
                             tracking_week$playId == current_playId & 
                             tracking_week$nflId == current_nflId)
    
    if (length(matching_rows) > 0) {
      tracking_week$ballCarrier[matching_rows] <- 1
    }
  }
  
  return(tracking_week)
}

tracking_week_1 <- update_tracking(tracking_week_1, tackles, plays)
tracking_week_2 <- update_tracking(tracking_week_2, tackles, plays)
tracking_week_3 <- update_tracking(tracking_week_3, tackles, plays)
tracking_week_4 <- update_tracking(tracking_week_4, tackles, plays)
tracking_week_5 <- update_tracking(tracking_week_5, tackles, plays)
tracking_week_6 <- update_tracking(tracking_week_6, tackles, plays)
tracking_week_7 <- update_tracking(tracking_week_7, tackles, plays)
tracking_week_8 <- update_tracking(tracking_week_8, tackles, plays)


tackler_data <- rbind(
  tracking_week_1[tracking_week_1$madeTackle == 1, ],
  tracking_week_2[tracking_week_2$madeTackle == 1, ],
  tracking_week_3[tracking_week_3$madeTackle == 1, ],
  tracking_week_4[tracking_week_4$madeTackle == 1, ],
  tracking_week_5[tracking_week_5$madeTackle == 1, ],
  tracking_week_6[tracking_week_6$madeTackle == 1, ],
  tracking_week_7[tracking_week_7$madeTackle == 1, ],
  tracking_week_8[tracking_week_8$madeTackle == 1, ]
)

ball_carrier_data <- rbind(
  tracking_week_1[tracking_week_1$ballCarrier == 1, ],
  tracking_week_2[tracking_week_2$ballCarrier == 1, ],
  tracking_week_3[tracking_week_3$ballCarrier == 1, ],
  tracking_week_4[tracking_week_4$ballCarrier == 1, ],
  tracking_week_5[tracking_week_5$ballCarrier == 1, ],
  tracking_week_6[tracking_week_6$ballCarrier == 1, ],
  tracking_week_7[tracking_week_7$ballCarrier == 1, ],
  tracking_week_8[tracking_week_8$ballCarrier == 1, ]
)

tacklers_and_carriers <- merge(ball_carrier_data, tackler_data, by = c("gameId", "playId", "frameId"), all.x = TRUE) %>%
  mutate(netX = X_std.y - X_std.x,
         netY = Y_std.y)






#FIND THE RUN AND PASS VORONOI BOX

matched_run <- merge(ball_carrier_data, tackler_data, by = c("gameId", "playId", "frameId"))

matched_run <- matched_run %>%
  group_by(gameId, playId) %>%
  mutate(handoffFrame = ifelse(event.x == "handoff", frameId, NA)) %>%
  fill(handoffFrame, .direction = "downup") %>%
  mutate(tackleFrame = ifelse(event.x == "tackle", frameId, NA)) %>%
  fill(tackleFrame, .direction = "downup") %>%
  ungroup()

run_coordinates <- matched_run %>%
  filter(!is.na(handoffFrame)) %>%
  filter(frameId >= handoffFrame) %>%
  filter(tackleFrame >= frameId) %>%
  mutate(netX = X_std.y - X_std.x,
         netY = Y_std.y - Y_std.x) %>%
  select(netX, netY, week)


run_coordinates_train <- run_coordinates %>%
  filter(week == 1 | week == 2)
  
run_coordinates_test <- run_coordinates %>%
  filter(week != 1 & week != 2)

column_name <- "netX"

#REPEAT FOR X, Y, IN BOTH NEGATIVE AND POSITIVE DIRECTIONS, AND FOR RUN AND PASS
percentile_90 <- quantile(run_coordinates_train[[column_name]], 0.9)

print(percentile_90)


#FINAL COORDINATES WITH 90% OF DATA

#(-3 to 9 in X, -5 to 5 in Y) for run
#(-4 to 9 in X, -6 to 6 in Y) for run



#ENSURE THIS WORKS ON REST OF DATA
percentile_value <- quantile(run_coordinates_test$netX, probs = c(-5), na.rm = TRUE)
print(percentile_value)
