install.packages("tidyverse")
install.packages("deldir")
install.packages("polyclip")
library(tidyverse)
library(deldir)
library(polyclip)
library(utils)

# VARIABLES

# Results from Voronoi Box, the area around the ball carrier
x_pos_run <- 9
x_neg_run <- 3
y_run <- 5

x_pos_pass <- 9
x_neg_pass <- 4
y_pass <- 6





# CALCULATE VORONOI AREA

voronoi_area <- function(field, play_type) {
  field_df <- as.data.frame(field[1:22, ])
  
  x_pos <- get(paste0("x_pos_", play_type))
  x_neg <- get(paste0("x_neg_", play_type))
  y_s <- get(paste0("y_", play_type))
  
  field_df$defender <- 0
  defense_team <- plays[plays$gameId == field_df[1, "gameId"] & 
                          plays$playId == field_df[1, "playId"], "defensiveTeam"]
  defenders <- which(field_df$club == defense_team)
  num_defenders <- length(defenders)
  field_df$defender[defenders] <- 1
  
  tess <- deldir(field_df$x_std, field_df$y_std, rw=c(-20,140,-10,63.3))
  
  ball_carrier <- field_df[field_df$ballCarrier == 1, ]
  x_min <- ball_carrier[1, "x_std"] - x_neg
  x_max <- ball_carrier[1, "x_std"] + x_pos
  y_min <- ball_carrier[1, "y_std"] - y_s
  y_max <- ball_carrier[1, "y_std"] + y_s
  
  post_clip <- tile.list(tess, clipp=list(x=c(x_min, x_max, x_max, x_min),
                                          y=c(y_min, y_min, y_max, y_max)))
  
  point_list <- lapply(post_clip, "[[", "ptNum")
  point_chart <- vector(length=22)
  for (p in seq_along(point_list)) {
    point_chart[point_list[[p]]] <- p
  }
  
  field_df <- field_df %>%
    filter(row_number() %in% point_list)
  
  index <- 1
  for (x in seq(x_min, x_max-1)) {
    for (y in seq(y_min, y_max-1)) {
      current_vor <- paste0("vor_", index)
      field_df[ , current_vor] <- 0
      
      if (x > 120 || x+1 < 0 || y > 53.3 || y+1 < 0) {
        field_df[ , current_vor] <- NA
        break
      }
      
      box <- tile.list(tess, 
                       clipp=list(x=c(x, x+1, x+1, x),
                                  y=c(y, y, y+1, y+1)))
      
      pt <- sapply(box, "[[", "ptNum")
      ar <- sapply(box, "[[", "area")
      field_df[point_chart[pt], current_vor] <- ar
      
      index <- index + 1
    }
  }
  
  return(field_df)
}



voronoi_area_shell <- function(field, play_type) {
  field_df <- as.data.frame(field[1:22, ])
  
  x_pos <- get(paste0("x_pos_", play_type))
  x_neg <- get(paste0("x_neg_", play_type))
  y_s <- get(paste0("y_", play_type))
  
  field_df$defender <- 0
  defense_team <- plays[plays$gameId == field_df[1, "gameId"] & 
                          plays$playId == field_df[1, "playId"], "defensiveTeam"]
  defenders <- which(field_df$club == defense_team)
  num_defenders <- length(defenders)
  field_df$defender[defenders] <- 1
  
  tess <- deldir(field_df$x_std, field_df$y_std, rw=c(-20,140,-10,63.3))
  
  ball_carrier <- field_df[field_df$ballCarrier == 1, ]
  x_min <- ball_carrier[1, "x_std"] - x_neg
  x_max <- ball_carrier[1, "x_std"] + x_pos
  y_min <- ball_carrier[1, "y_std"] - y_s
  y_max <- ball_carrier[1, "y_std"] + y_s
  
  post_clip <- tile.list(tess, clipp=list(x=c(x_min, x_max, x_max, x_min),
                                          y=c(y_min, y_min, y_max, y_max)))
  
  point_list <- lapply(post_clip, "[[", "ptNum")
  point_chart <- vector(length=22)
  for (p in seq_along(point_list)) {
    point_chart[point_list[[p]]] <- p
  }
  
  field_df <- field_df %>%
    filter(row_number() %in% point_list)
  
  return(field_df)
}






# TESTING


create_run_voronoi <- function(tracking_week) {
  ntr <- tracking_week %>%
    group_by(gameId, playId) %>%
    filter(any(ballCarrier == 1)) %>%
    ungroup() %>%
    group_by(gameId, playId) %>%
    mutate(handoffFrame = ifelse(event == "handoff", frameId, NA)) %>%
    fill(handoffFrame, .direction = "downup") %>%
    mutate(tackleFrame = ifelse(event == "tackle", frameId, NA)) %>%
    fill(tackleFrame, .direction = "downup") %>%
    ungroup() %>%
    filter(!is.na(handoffFrame)) %>%
    filter(frameId >= handoffFrame) %>%
    filter(tackleFrame >= frameId) %>%
    group_by(gameId, playId, frameId) %>%
    do(voronoi_area(., "run"))
  return(ntr)
}

create_pass_voronoi <- function(tracking_week) {
  ntp <- tracking_week %>%
    group_by(gameId, playId) %>%
    filter(any(ballCarrier == 1)) %>%
    ungroup() %>%
    group_by(gameId, playId) %>%
    mutate(passFrame = ifelse(event == "pass_outcome_caught", frameId, NA)) %>%
    fill(passFrame, .direction = "downup") %>%
    mutate(tackleFrame = ifelse(event == "tackle", frameId, NA)) %>%
    fill(tackleFrame, .direction = "downup") %>%
    ungroup() %>%
    filter(!is.na(passFrame)) %>%
    filter(frameId >= passFrame) %>%
    filter(tackleFrame >= frameId) %>%
    group_by(gameId, playId, frameId) %>%
    do(voronoi_area(., "pass"))
  return(ntp)
}


# test_play <- create_pass_voronoi(tracking_week_1[c(1:506), ])


# Run these separately
week_8_run_voronoi <- create_run_voronoi(tracking_week_8)
week_7_run_voronoi <- create_run_voronoi(tracking_week_7)
week_6_run_voronoi <- create_run_voronoi(tracking_week_6)
week_5_run_voronoi <- create_run_voronoi(tracking_week_5)
week_4_run_voronoi <- create_run_voronoi(tracking_week_4)
week_3_run_voronoi <- create_run_voronoi(tracking_week_3)
week_2_run_voronoi <- create_run_voronoi(tracking_week_2)
week_1_run_voronoi <- create_run_voronoi(tracking_week_1)

week_8_pass_voronoi <- create_pass_voronoi(tracking_week_8)
week_7_pass_voronoi <- create_pass_voronoi(tracking_week_7)
week_6_pass_voronoi <- create_pass_voronoi(tracking_week_6)
week_5_pass_voronoi <- create_pass_voronoi(tracking_week_5)
week_4_pass_voronoi <- create_pass_voronoi(tracking_week_4)
week_3_pass_voronoi <- create_pass_voronoi(tracking_week_3)
week_2_pass_voronoi <- create_pass_voronoi(tracking_week_2)
week_1_pass_voronoi <- create_pass_voronoi(tracking_week_1)

write.csv()
