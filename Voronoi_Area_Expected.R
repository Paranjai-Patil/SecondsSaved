install.packages("tidyverse", repos = "http://cran.us.r-project.org")
install.packages("deldir", repos = "http://cran.us.r-project.org")
install.packages("polyclip", repos = "http://cran.us.r-project.org")
install.packages("parallel", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(deldir)
library(polyclip)
library(parallel)
library(utils)

# VARIABLES

# Results from Voronoi Box, the area around the ball carrier
x_pos_run <- 9
x_neg_run <- 3
y_run <- 5

x_pos_pass <- 9
x_neg_pass <- 4
y_pass <- 6

# Replace with week number you want to run
week_number <- 8
# Number of cores you want your machine to use (only for Mac and Linux machines)
core_count <- 5





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





create_run_voronoi <- function(i) {
  field <- tracking_week_run %>%
    filter(gameId == final_run_data[i, "gameId"] &
             playId == final_run_data[i, "playId"] &
             frameId == final_run_data[i, "frameId"]) %>%
    mutate(pred_player = final_run_data[i, "nflId"])
  ppi <- which(field$nflId == final_run_data[i, "nflId"])
  field[ppi, "x_std"] = final_run_data[i, "predX"]
  field[ppi, "y_std"] = final_run_data[i, "predY"]
  return(voronoi_area(field, "run"))
}

create_pass_voronoi <- function(i) {
  field <- tracking_week_pass %>%
    filter(gameId == final_pass_data[i, "gameId"] &
             playId == final_pass_data[i, "playId"] &
             frameId == final_pass_data[i, "frameId"]) %>%
    mutate(pred_player = final_pass_data[i, "nflId"])
  ppi <- which(field$nflId == final_pass_data[i, "nflId"])
  field[ppi, "x_std"] = final_pass_data[i, "predX"]
  field[ppi, "y_std"] = final_pass_data[i, "predY"]
  return(voronoi_area(field, "pass"))
}



plays <- read.csv("/Users/aparep7474/Documents/BDB2024/nfl-big-data-bowl-2024/plays.csv")

tracking_week <- read.csv(paste0("/Users/aparep7474/Documents/BDB2024/UpdatedTrackingData/tracking_week_", week_number, ".csv"))
final_run_data <- read.csv("/Users/aparep7474/Documents/BDB2024/UpdatedTrackingData/finalRunData.csv")
final_run_data <- final_run_data %>%
  filter(week == week_number)

tracking_week_run <- tracking_week %>%
  group_by(gameId, playId) %>%
  mutate(handoffFrame = ifelse(event == "handoff", frameId, NA)) %>%
  fill(handoffFrame, .direction = "downup") %>%
  mutate(tackleFrame = ifelse(event == "tackle", frameId, NA)) %>%
  fill(tackleFrame, .direction = "downup") %>%
  ungroup() %>%
  filter(!is.na(handoffFrame)) %>%
  filter(frameId >= handoffFrame) %>%
  filter(tackleFrame >= frameId) %>%
  filter(club != "football")
rm(tracking_week)

if (Sys.info()['sysname'] == "Darwin" || Sys.info()['sysname'] == "Linux") {
  # CODE FOR MAC/LINUX (different paralellization: mclapply(), fork based)
  # Change mc.cores based on how many cores machine has
  # testing: final_run_data_voronoi <- mclapply(1:1000, create_run_voronoi, mc.cores=core_count)
  final_run_data_voronoi <- mclapply(1:nrow(final_run_data), create_run_voronoi, mc.cores=core_count)
  final_run_data_voronoi <- bind_rows(final_run_data_voronoi)
} else {
  # CODE FOR WINDOWS (different paralellization: parLapply(), socket based)
  cl <- makeCluster(detectCores())
  clusterEvalQ(cl, c(library(tidyverse), library(deldir), library(polyclip)))
  clusterExport(cl, c('x_neg_pass', 'x_neg_run', 'x_pos_pass', 'x_pos_run', 'y_pass', 'y_run'))
  clusterExport(cl, c('voronoi_area', 'tracking_week_run', 'final_run_data', 'plays'))
  final_run_data_voronoi <- parLapply(cl, 1:nrow(final_run_data), create_run_voronoi)
  final_run_data_voronoi <- bind_rows(final_run_data_voronoi)
}

write.csv(final_run_data_voronoi, paste0("/Users/aparep7474/Documents/BDB2024/VoronoiAreas/final_run_data_week_", week_number, "_voronoi_p20.csv"), row.names=FALSE)
rm(final_run_data_voronoi)
gc()
print("Run Complete")



tracking_week <- read.csv(paste0("/Users/aparep7474/Documents/BDB2024/UpdatedTrackingData/tracking_week_", week_number, ".csv"))
final_pass_data <- read.csv("/Users/aparep7474/Documents/BDB2024/UpdatedTrackingData/finalPassData.csv")
final_pass_data <- final_pass_data %>%
  filter(week == week_number)

tracking_week_pass <- tracking_week %>%
  group_by(gameId, playId) %>%
  mutate(passFrame = ifelse(event == "pass_outcome_caught", frameId, NA)) %>%
  fill(passFrame, .direction = "downup") %>%
  mutate(tackleFrame = ifelse(event == "tackle", frameId, NA)) %>%
  fill(tackleFrame, .direction = "downup") %>%
  ungroup() %>%
  filter(!is.na(passFrame)) %>%
  filter(frameId >= passFrame) %>%
  filter(tackleFrame >= frameId) %>%
  filter(club != "football")
rm(tracking_week)

if (Sys.info()['sysname'] == "Darwin" || Sys.info()['sysname'] == "Linux") {
  # CODE FOR MAC/LINUX (different paralellization: mclapply(), fork based)
  # Change mc.cores based on how many cores machine has
  # testing: final_pass_data_voronoi <- mclapply(1:1000, create_pass_voronoi, mc.cores=core_count)
  final_pass_data_voronoi <- mclapply(1:nrow(final_pass_data), create_pass_voronoi, mc.cores=core_count)
  final_pass_data_voronoi <- bind_rows(final_pass_data_voronoi)
} else {
  # CODE FOR WINDOWS (different paralellization: parLapply(), socket based)
  cl <- makeCluster(detectCores())
  clusterEvalQ(cl, c(library(tidyverse), library(deldir), library(polyclip)))
  clusterExport(cl, c('x_neg_pass', 'x_neg_run', 'x_pos_pass', 'x_pos_run', 'y_pass', 'y_run'))
  clusterExport(cl, c('voronoi_area', 'tracking_week_pass', 'final_pass_data', 'plays'))
  final_pass_data_voronoi <- parLapply(cl, 1:nrow(final_pass_data), create_pass_voronoi)
  final_pass_data_voronoi <- bind_rows(final_pass_data_voronoi)
}

write.csv(final_pass_data_voronoi, paste0("/Users/aparep7474/Documents/BDB2024/VoronoiAreas/final_pass_data_week_", week_number, "_voronoi.csv"), row.names=FALSE)
print("Pass Complete")
