library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(extrafont)
library(deldir)
library(polyclip)
library(gganimate)
library(utils)


#REPEAT FOR PASS HEATMAP

ggplot(run_coordinates, aes(x = net_y, y = net_x)) +
  geom_bin2d(binwidth = c(1, 1)) +
  scale_fill_gradient(trans = "log", limits = c(20, 30000), breaks = c(50, 400, 3000), low = "blue", high = "red") +
  geom_point(x = 0, y = 0, size = 5, shape = 21, fill = "black") +  # Filled black circle at (0, 0)
  labs(title = "Tackler Heatmap on Runs", x = "\"Y\" coordinate relative to ball carrier", y = "\"X\" coordinate relative to ball carrier") +
  theme_minimal() +
  coord_equal(ratio = 1) +
  coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 20)) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, family = "serif"),
    axis.text = element_text(size = 14, family = "serif"),
    axis.title = element_text(size = 16, family = "serif"),
    legend.text = element_text(size = 12, family = "serif"),  
    legend.title = element_text(size = 14, family = "serif")
  ) +
  geom_rect(xmin = -5, xmax = 5, ymin = -3, ymax = 9, color = "green", fill = NA)






#LINEBACKERS GRAPH


LBgraph <- SecondsSaved %>%
  filter(position == "LB") %>%
  mutate(SSperplay = secondsSaved/count) %>%
  filter(count > 30)

ggplot(LBgraph, aes(x = average_dist, y = SSperplay, label = displayName, color = hex_code)) +
  geom_point() +
  geom_text_repel(size = 3, hjust = 0, vjust = 0, family = "serif") +
  labs(title = "Linebackers Seconds Saved vs Distance from Expected Position",
       x = "Average distance from expected position",
       y = "Seconds Saved per Play") +
  scale_color_identity() +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(size = 18, hjust = 0.5)) 



#TABLE FOR BEST EDGE RUSHERS

TopEdgeRushers <- SecondsSaved %>%
  filter(count >= 30)

Top20 <- TopEdgeRushers %>%
  arrange(desc(secondsSavedperplay)) %>%
  slice(1:20)

Top20$Rank <- seq(1, 20)



#BIN IMPORTANCE GRAPH FOR RUN

importance <- varImp(rf_model)

importance$factorName <- as.numeric(gsub("vor_", "", importance$factorName))

importance$RelativeLogScaledImportance <- log(importance$Importance + 1)

ggplot(importance, aes(x = factorName %% 10, y = factorName %/% 10, fill = RelativeLogScaledImportance)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(x = "X-axis (vor_1 to vor_10)", y = "Y-axis (vor_1 to vor_12)", title = "Importance Visualization") +
  coord_fixed() +
  theme_minimal(base_family = "serif") +  
  theme(legend.title = element_text(face = "italic")) 



# Animated Voronoi and TT Graphs
# Animated Expected Coords

# Results from Voronoi Box, the area around the ball carrier
x_pos_run <- 9
x_neg_run <- 3
y_run <- 5

x_pos_pass <- 9
x_neg_pass <- 4
y_pass <- 6

WINDOWX1 <- 0
WINDOWX2 <- 0





voronoi_visual <- function(play, play_type) {
  x_pos <- get(paste0("x_pos_", play_type))
  x_neg <- get(paste0("x_neg_", play_type))
  y_s <- get(paste0("y_", play_type))
  
  play$type <- play$club
  play$type[play$ballCarrier == 1] = "Ball Carrier"
  
  play$poly_id <- 1:nrow(play)
  
  frames <- split(play, play$frameId)
  updated_frames <- list()
  for (f in seq_along(frames)) {
    field <- frames[[f]][1:22, ]
    
    ball_carrier <- field[field$ballCarrier == 1, ]
    field$x_min <- ball_carrier[1, "x_std"] - x_neg
    field$x_max <- ball_carrier[1, "x_std"] + x_pos
    field$y_min <- ball_carrier[1, "y_std"] - y_s
    field$y_max <- ball_carrier[1, "y_std"] + y_s
    
    tess <- deldir(field$x_std, field$y_std, rw=c(-20, 140, -10, 63.3))
    tiles <- tile.list(tess)
    
    updated_field <- list()
    for (i in 1:22) {
      poly_x = as.numeric(tiles[[i]]$x)
      poly_y = as.numeric(tiles[[i]]$y)
      updated_field[[i]] <- field[i, ] %>%
        merge(as.data.frame(poly_x)) %>%
        mutate(poly_y = poly_y)
    }
    bind_rows(updated_field)
    
    updated_frames[[f]] <- updated_field
  }
  updated_play <- bind_rows(updated_frames)
  
  plot <- ggplot() +
    geom_polygon(data = updated_play, aes(x = poly_y,
                                          y = poly_x,
                                          group = poly_id,
                                          fill = type),
                 color = "black") +
    scale_fill_manual(values = c("Ball Carrier" = "green", "TeamName1" = "TeamColor1", "TeamName2" = "TeamColor2"))
  
  plot <- plot +
    geom_rect(data = updated_play, aes(xmin = y_min, xmax = y_max, ymin = x_min, ymax = x_max),
              fill = "transparent", color = "green")
  
  plot <- plot + 
    geom_point(data = play, aes(y_std, x_std), size = 1, color='white') +
    scale_x_reverse() +
    # CHANGE HARD CODED X AND Y WHEN CHANGING PLAY
    coord_fixed(xlim = c(53.3, 0), ylim = c(WINDOWX1, WINDOWX2)) +
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  plot <- plot + 
    transition_time(frameId)
  
  plot_anim <- animate(plot, fps = 10)
  
  return(plot_anim)
}

future_player_visual <- function(play, fut_play) {
  play$type <- play$club
  play$type[play$ballCarrier == 1] = "Ball Carrier"
  play$type[play$nflId == fut_play$nflId[1]] = "Player"
  
  plot <- ggplot(play, aes(x = y_std, y = x_std)) + 
    geom_point(data = play, aes(x = y_std, y = x_std, color = type), size = 2) +
    geom_point(data = fut_play, aes(x = predY, y = predX), size = 2, color = "TeamColor2", shape = 21) +
    scale_color_manual(values = c("Player" = "TeamColor2", "Ball Carrier" = "green")) +
    scale_x_reverse() +
    # CHANGE HARD CODED X AND Y WHEN CHANGING PLAY
    coord_fixed(xlim = c(53.3, 0), ylim = c(WINDOWX1, WINDOWX2)) +
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  plot <- plot + 
    transition_time(frameId)
  
  plot_anim <- animate(plot, fps = 10)
  
  return(plot_anim)
}

ttt_visual <- function(play) {
  plot <- ggplot(play, aes(x = frameId, y = ttt)) +
    geom_line() + 
    xlim(0, play[nrow(play), "frameId"]) +
    xlab("Frames After Motion") +
    ylab("Time to Tackle Prediction")
  
  plot <- plot +
    transition_reveal(frameId)
  
  plot_anim <- animate(plot, start_pause = play[1, "frameId"]-1, fps = 10)
  
  return(plot_anim)
}

player_id <- player_id
game_id <- game_id
play_id <- play_id

plot_fut_play <- final_run_data %>%
  filter(gameId == game_id & playId == play_id & nflId == player_id) %>%
  arrange(frameId)

plot_play <- tracking_week_8 %>%
  filter(gameId == game_id & playId == play_id) %>%
  filter(club != "football") %>%
  arrange(frameId) %>%
  filter(frameId <= plot_fut_play$tackleFrame[1])

plot_play_ttt <- ttt_run_pred %>%
  filter(gameId == game_id & playId == play_id)
plot_play_ttt <- plot_play %>%
  filter(frameId %in% plot_play_ttt$frameId) %>%
  filter(nflId == player_id) %>%
  mutate(ttt = plot_play_ttt$timeToTacklePred)

plot_i <- future_player_visual(plot_play, plot_fut_play)
plot_ii <- voronoi_visual(plot_play, play_type)
plot_iv <- ttt_visual(plot_play_ttt)
plot_i
plot_ii
plot_iv




# Static Voronoi and TT Graphs

static_frame <- 0

voronoi_visual_static <- function(play, play_type, highlight) {
  x_pos <- get(paste0("x_pos_", play_type))
  x_neg <- get(paste0("x_neg_", play_type))
  y_s <- get(paste0("y_", play_type))
  
  play$type <- play$club
  play$type[play$ballCarrier == 1] = "Ball Carrier"
  play$type[play$nflId == highlight] = "Player"
  
  play$poly_id <- 1:nrow(play)
  
  frames <- split(play, play$frameId)
  updated_frames <- list()
  for (f in seq_along(frames)) {
    field <- frames[[f]][1:22, ]
    
    ball_carrier <- field[field$ballCarrier == 1, ]
    field$x_min <- ball_carrier[1, "x_std"] - x_neg
    field$x_max <- ball_carrier[1, "x_std"] + x_pos
    field$y_min <- ball_carrier[1, "y_std"] - y_s
    field$y_max <- ball_carrier[1, "y_std"] + y_s
    
    tess <- deldir(field$x_std, field$y_std, rw=c(-20, 140, -10, 63.3))
    tiles <- tile.list(tess)
    
    updated_field <- list()
    for (i in 1:22) {
      poly_x = as.numeric(tiles[[i]]$x)
      poly_y = as.numeric(tiles[[i]]$y)
      updated_field[[i]] <- field[i, ] %>%
        merge(as.data.frame(poly_x)) %>%
        mutate(poly_y = poly_y)
    }
    bind_rows(updated_field)
    
    updated_frames[[f]] <- updated_field
  }
  updated_play <- bind_rows(updated_frames)
  
  play <- play %>%
    filter(frameId == static_frame)
  updated_play <- updated_play %>%
    filter(frameId == static_frame)
  
  plot <- ggplot() +
    geom_polygon(data = updated_play, aes(x = poly_y,
                                          y = poly_x,
                                          group = poly_id,
                                          fill = type),
                 color = "black") +
    scale_fill_manual(values = c("Player" = "#6ea2ff", "Ball Carrier" = "green", "TeamName1" = "TeamColor1", "TeamName2" = "TeamColor2"))
  
  plot <- plot +
    geom_rect(data = updated_play, aes(xmin = y_min, xmax = y_max, ymin = x_min, ymax = x_max),
              fill = "transparent", color = "green")
  
  plot <- plot + 
    geom_point(data = play, aes(y_std, x_std), size = 1, color='white') +
    scale_x_reverse() +
    # CHANGE HARD CODED X AND Y WHEN CHANGING PLAY
    coord_fixed(xlim = c(53.3, 0), ylim = c(WINDOWX, WINDOWY)) +
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    ggtitle("Frame #")
  
  return(plot)
}

ttt_visual_static <- function(play) {
  play <- play %>%
    mutate(frame = ifelse(frameId == static_frame, 1, 0))
  
  plot <- ggplot(play, aes(x = frameId, y = ttt, color = frame)) +
    geom_line() + 
    geom_point(size = 3) +
    theme(legend.position="none") +
    xlab("Frames After Motion") +
    ylab("Time to Tackle Prediction")
  
  return(plot)
}

player_id <- player_id
game_id <- game_id
play_id <- play_id

plot_fut_play <- final_run_data %>%
  filter(gameId == game_id & playId == play_id & nflId == player_id) %>%
  arrange(frameId)

plot_play <- tracking_week_8 %>%
  filter(gameId == game_id & playId == play_id) %>%
  filter(club != "football") %>%
  arrange(frameId) %>%
  filter(frameId <= plot_fut_play$tackleFrame[1])

plot_play_r <- plot_play %>%
  filter(frameId %in% plot_fut_play$frameId) %>%
  filter(nflId == player_id) %>%
  mutate(x_std = plot_fut_play$predX) %>%
  mutate(y_std = plot_fut_play$predY)
plot_play_r <- rows_update(plot_play, plot_play_r, by = c("nflId", "frameId"))

plot_play_ttt <- ttt_run_pred %>%
  filter(gameId == game_id & playId == play_id)
plot_play_ttt <- plot_play %>%
  filter(frameId %in% plot_play_ttt$frameId) %>%
  filter(nflId == player_id) %>%
  mutate(ttt = plot_play_ttt$timeToTacklePred)

plot_play_ttt_exp <- ttt_run_pred_exp %>%
  filter(gameId == game_id & playId == play_id & pred_player == player_id)
plot_play_ttt_exp <- plot_play %>%
  filter(frameId %in% plot_play_ttt_exp$frameId) %>%
  filter(nflId == player_id) %>%
  mutate(ttt = plot_play_ttt_exp$timeToTacklePred)

plot_ii <- voronoi_visual_static(plot_play, play_type, player_id)
plot_iii <- voronoi_visual_static(plot_play_r, play_type, player_id)
plot_iv <- ttt_visual_static(plot_play_ttt)
plot_v <- ttt_visual_static(plot_play_ttt_exp)
plot_ii
plot_iii
plot_iv
plot_v
 
