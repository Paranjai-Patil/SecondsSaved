library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(extrafont)


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







