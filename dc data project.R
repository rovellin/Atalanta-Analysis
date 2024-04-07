install.packages("httpuv")
install.packages("remotes")
remotes::install_version("SDMTools", "1.1-221")
devtools::install_github("statsbomb/StatsBombR")
install.packages("StatsBombR")
library(StatsBombR)
library(tidyverse)
library(readr)
library(ggplot2)
library(paletteer)
library(devtools)
devtools::install_github("FCrSTATS/SBpitch")
library(SBpitch)
library(cowplot)
remotes::install_github("statsbomb/StatsBombR")
library(extrafont)

comps <- FreeCompetitions() %>% 
  filter(competition_id == 12 & season_name == "2015/2016")

matches <- FreeMatches(comps)

atalanta_mask <- (matches['home_team.home_team_name'] == 'Atalanta') | (matches['away_team.away_team_name'] == 'Atalanta')

atalanta <- matches[atalanta_mask, ]

starting_11 <- StatsBombData %>% 
  filter(type.name == "Starting XI")

formation <- starting_11 %>% 
  filter(tactics.formation == 4231 & team.name == "Atalanta")

analyze_matches <- atalanta[c(1, 6, 11, 12, 13, 15, 16), ]

wins <- analyze_matches[c(2, 3, 5, 7), ]

drop <- analyze_matches[c(1, 4, 6), ]

atalanta_wins <- free_allevents(MatchesDF = wins, Parallel = T)

atalanta_wins[c("location_x", "location_y")] <- 
  str_split_fixed(atalanta_wins$location, ",", 2)

atalanta_wins$location_x <- 
  str_remove(atalanta_wins$location_x, "c")

atalanta_wins$location_x <- 
  str_remove(atalanta_wins$location_x, "\\(")

atalanta_wins$location_y <- 
  str_remove(atalanta_wins$location_y, "\\)")

atalanta_wins$location_x <- as.numeric(atalanta_wins$location_x)
atalanta_wins$location_y <- as.numeric(atalanta_wins$location_y)

atalanta_wins[c("pass_location_x", "pass_location_y")] <- 
  str_split_fixed(atalanta_wins$pass.end_location, ",", 2)

atalanta_wins$pass_location_x <- 
  str_remove(atalanta_wins$pass_location_x, "c")

atalanta_wins$pass_location_x <- 
  str_remove(atalanta_wins$pass_location_x, "\\(")

atalanta_wins$pass_location_y <- 
  str_remove(atalanta_wins$pass_location_y, "\\)")

atalanta_wins$pass_location_x <- as.numeric(atalanta_wins$pass_location_x)
atalanta_wins$pass_location_y <- as.numeric(atalanta_wins$pass_location_y)

atalanta_wins$pass_vert <- 
  atalanta_wins$pass_location_x - atalanta_wins$location_x

atalanta_wins$prog_pass <- ifelse(atalanta_wins$pass_vert < -3, "Negative", 
                                  ifelse(atalanta_wins$pass_vert > 3, "Positive", "Neutral"))

atalanta_wins$position <- ifelse(atalanta_wins$position.name %in% c("Left Back", "Right Back", "Left Wing Back", "Right Wing Back"), "Full Back", 
                                 ifelse(atalanta_wins$position.name %in% c("Right Center Back", "Left Center Back", "Center Back"), "Center Back", 
                                        ifelse(atalanta_wins$position.name %in% c("Center Defensive Midfield", "Left Defensive Midfield", "Right Defensive Midfield"), "Defensive Midfield", 
                                               ifelse(atalanta_wins$position.name %in% c("Center Attacking Midfield", "Left Midfield", "Right Midfield"), "Attacking Midfield", 
                                                      ifelse(atalanta_wins$position.name %in% c("Left Wing", "Right Wing"), "Winger", 
                                                             ifelse(atalanta_wins$position.name %in% c("Center Forward", "Left Center Forward", "Right Center Forward"), "Center Forward", 
                                                                    ifelse(atalanta_wins$position.name %in% c("Left Center Midfield", "Right Center Midfield"), "Center Midfield", "Goalkeeper")))))))


atalanta_wins$quarter <- ifelse(atalanta_wins$minute < 23, 1, 
                                ifelse(atalanta_wins$minute > 22 & atalanta_wins$period == 1, 2, 
                                       ifelse(atalanta_wins$minute > 44 & atalanta_wins$minute < 78 & atalanta_wins$period == 2, 3, 4)))

atalanta_drop <- free_allevents(MatchesDF = drop, Parallel = T)

atalanta_drop[c("location_x", "location_y")] <- 
  str_split_fixed(atalanta_drop$location, ",", 2)

atalanta_drop$location_x <- 
  str_remove(atalanta_drop$location_x, "c")

atalanta_drop$location_x <- 
  str_remove(atalanta_drop$location_x, "\\(")

atalanta_drop$location_y <- 
  str_remove(atalanta_drop$location_y, "\\)")

atalanta_drop$location_x <- as.numeric(atalanta_drop$location_x)
atalanta_drop$location_y <- as.numeric(atalanta_drop$location_y)

atalanta_drop[c("pass_location_x", "pass_location_y")] <- 
  str_split_fixed(atalanta_drop$pass.end_location, ",", 2)

atalanta_drop$pass_location_x <- 
  str_remove(atalanta_drop$pass_location_x, "c")

atalanta_drop$pass_location_x <- 
  str_remove(atalanta_drop$pass_location_x, "\\(")

atalanta_drop$pass_location_y <- 
  str_remove(atalanta_drop$pass_location_y, "\\)")

atalanta_drop$pass_location_x <- as.numeric(atalanta_drop$pass_location_x)
atalanta_drop$pass_location_y <- as.numeric(atalanta_drop$pass_location_y)

atalanta_drop$pass_vert <- 
  atalanta_drop$pass_location_x - atalanta_drop$location_x

atalanta_drop$prog_pass <- ifelse(atalanta_drop$pass_vert < -3, "Negative", 
                                  ifelse(atalanta_drop$pass_vert > 3, "Positive", "Neutral"))

atalanta_drop$position <- ifelse(atalanta_drop$position.name %in% c("Left Back", "Right Back", "Left Wing Back", "Right Wing Back"), "Full Back", 
                                 ifelse(atalanta_drop$position.name %in% c("Right Center Back", "Left Center Back", "Center Back"), "Center Back", 
                                        ifelse(atalanta_drop$position.name %in% c("Center Defensive Midfield", "Left Defensive Midfield", "Right Defensive Midfield"), "Defensive Midfield", 
                                               ifelse(atalanta_drop$position.name %in% c("Center Attacking Midfield", "Left Midfield", "Right Midfield"), "Attacking Midfield", 
                                                      ifelse(atalanta_drop$position.name %in% c("Left Wing", "Right Wing"), "Winger", 
                                                             ifelse(atalanta_drop$position.name %in% c("Center Forward", "Left Center Forward", "Right Center Forward"), "Center Forward", 
                                                                    ifelse(atalanta_drop$position.name %in% c("Left Center Midfield", "Right Center Midfield"), "Center Midfield", "Goalkeeper")))))))


atalanta_drop$quarter <- ifelse(atalanta_drop$minute < 23, 1, 
                                ifelse(atalanta_drop$minute > 22 & atalanta_drop$period == 1, 2, 
                                       ifelse(atalanta_drop$minute > 44 & atalanta_drop$minute < 78 & atalanta_drop$period == 2, 3, 4)))

counter_win <- atalanta_wins %>% 
  filter(counterpress == "TRUE")

atalanta_counter_win <- counter_win %>%
  filter(team.name == "Atalanta")

opp_counter_win <- counter_win %>% 
  filter(team.name != "Atalanta")

counter_drop <- atalanta_drop %>% 
  filter(counterpress == "TRUE")

atalanta_counter_drop <- counter_drop %>% 
  filter(team.name == "Atalanta")

opp_counter_drop <- counter_drop %>% 
  filter(team.name != "Atalanta")

palette <- paletteer_d("RColorBrewer::YlOrRd", direction = 1)

atalanta_counter_win_map <- create_Pitch(grass_colour = "gray15", 
                                         background_colour = "gray15", 
                              line_colour = "white") + 
  geom_density_2d_filled(data = atalanta_counter_win, 
                         aes(x = location_x, y = location_y, fill = ..level..,), 
                         alpha = 0.4, contour_var = "ndensity", 
                         breaks = seq(0.1, 1.0, length.out = 10)) + 
  theme(legend.position = "none") + 
  scale_x_continuous(limits = c(0, 120)) + 
  scale_y_continuous(limits = c(0, 80)) + 
  scale_fill_manual(values = c(palette), aesthetics = c("fill", "color")) + 
  theme(legend.position = "none", 
        plot.background = element_rect(colour = "gray15", fill = "gray15"), 
        plot.title = element_text(color = "white", hjust = .5, size = 22, 
                                  family = "Comic Sons MS", face = "bold", 
                                  vjust =  -1)) + 
  labs(title = "Atalanta Counter Press in Wins")

ggdraw(atalanta_counter_win_map) + theme(plot.background = element_rect(fill = "gray15", color = NA))

opp_counter_win_map <- create_Pitch(grass_colour = "gray15", 
                                         background_colour = "gray15", 
                                         line_colour = "white") + 
  geom_density_2d_filled(data = opp_counter_win, 
                         aes(x = location_x, y = location_y, fill = ..level..,), 
                         alpha = 0.4, contour_var = "ndensity", 
                         breaks = seq(0.1, 1.0, length.out = 10)) + 
  theme(legend.position = "none") + 
  scale_x_continuous(limits = c(0, 120)) + 
  scale_y_continuous(limits = c(0, 80)) + 
  scale_fill_manual(values = c(palette), aesthetics = c("fill", "color")) + 
  theme(legend.position = "none", 
        plot.background = element_rect(colour = "gray15", fill = "gray15"), 
        plot.title = element_text(color = "white", hjust = .5, size = 22, 
                                  family = "Comic Sons MS", face = "bold", 
                                  vjust =  -1)) + 
  labs(title = "Opponent Counter Press in Atalanta Wins")

ggdraw(opp_counter_win_map) + theme(plot.background = element_rect(fill = "gray15", color = NA))

atalanta_counter_drop_map <- create_Pitch(grass_colour = "gray15", 
                                         background_colour = "gray15", 
                                         line_colour = "white") + 
  geom_density_2d_filled(data = atalanta_counter_drop, 
                         aes(x = location_x, y = location_y, fill = ..level..,), 
                         alpha = 0.4, contour_var = "ndensity", 
                         breaks = seq(0.1, 1.0, length.out = 10)) + 
  theme(legend.position = "none") + 
  scale_x_continuous(limits = c(0, 120)) + 
  scale_y_continuous(limits = c(0, 80)) + 
  scale_fill_manual(values = c(palette), aesthetics = c("fill", "color")) + 
  theme(legend.position = "none", 
        plot.background = element_rect(colour = "gray15", fill = "gray15"), 
        plot.title = element_text(color = "white", hjust = .5, size = 22, 
                                  family = "Comic Sons MS", face = "bold", 
                                  vjust =  -1)) + 
  labs(title = "Atalanta Counter Press in Non-Wins")

ggdraw(atalanta_counter_drop_map) + theme(plot.background = element_rect(fill = "gray15", color = NA))

opp_counter_drop_map <- create_Pitch(grass_colour = "gray15", 
                                          background_colour = "gray15", 
                                          line_colour = "white") + 
  geom_density_2d_filled(data = opp_counter_drop, 
                         aes(x = location_x, y = location_y, fill = ..level..,), 
                         alpha = 0.4, contour_var = "ndensity", 
                         breaks = seq(0.1, 1.0, length.out = 10)) + 
  theme(legend.position = "none") + 
  scale_x_continuous(limits = c(0, 120)) + 
  scale_y_continuous(limits = c(0, 80)) + 
  scale_fill_manual(values = c(palette), aesthetics = c("fill", "color")) + 
  theme(legend.position = "none", 
        plot.background = element_rect(colour = "gray15", fill = "gray15"), 
        plot.title = element_text(color = "white", hjust = .5, size = 22, 
                                  family = "Comic Sons MS", face = "bold", 
                                  vjust =  -1)) + 
  labs(title = "Opponent Counter Press in Atalanta Non-Wins")

ggdraw(opp_counter_drop_map) + theme(plot.background = element_rect(fill = "gray15", color = NA))

ggplot(atalanta_counter_win, aes(fill = quarter, position, group = quarter)) + 
  geom_bar(position = "dodge", stat = "count") + 
  xlab("Positon") + 
  ylab("Count") + 
  labs(fill = "Quarter") + 
  ggtitle("Counter Pressures by Atalanta in Wins") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(opp_counter_win, aes(fill = quarter, position, group = quarter)) + 
  geom_bar(position = "dodge", stat = "count") + 
  xlab("Positon") + 
  ylab("Count") + 
  labs(fill = "Quarter") + 
  ggtitle("Counter Pressures by Opponents in Atalanta Wins") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(atalanta_counter_drop, aes(fill = quarter, position, group = quarter)) + 
  geom_bar(position = "dodge", stat = "count") + 
  xlab("Positon") + 
  ylab("Count") + 
  labs(fill = "Quarter") + 
  ggtitle("Counter Pressures by Atalanta in Non-Wins") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(opp_counter_drop, aes(fill = quarter, position, group = quarter)) + 
  geom_bar(position = "dodge", stat = "count") + 
  xlab("Positon") + 
  ylab("Count") + 
  labs(fill = "Quarter") + 
  ggtitle("Counter Pressures by Opponents in Atalanta Non-Wins") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) 

atalanta_counter_win_defmid <- atalanta_counter_win %>% 
  filter(position == "Defensive Midfield")

atalanta_counter_drop_defmid <- atalanta_counter_drop %>% 
  filter(position == "Defensive Midfield")

defmid_counter_win <- create_Pitch(grass_colour = "gray15", 
                                     background_colour = "gray15", 
                                     line_colour = "white") + 
  geom_density_2d_filled(data = atalanta_counter_win_defmid, 
                         aes(x = location_x, y = location_y, fill = ..level..,), 
                         alpha = 0.4, contour_var = "ndensity", 
                         breaks = seq(0.1, 1.0, length.out = 10)) + 
  theme(legend.position = "none") + 
  scale_x_continuous(limits = c(0, 120)) + 
  scale_y_continuous(limits = c(0, 80)) + 
  scale_fill_manual(values = c(palette), aesthetics = c("fill", "color")) + 
  theme(legend.position = "none", 
        plot.background = element_rect(colour = "gray15", fill = "gray15"), 
        plot.title = element_text(color = "white", hjust = .5, size = 22, 
                                  family = "Comic Sons MS", face = "bold", 
                                  vjust =  -1)) + 
  labs(title = "Atalanta Defensive Midfielders Counter Press in Wins")

ggdraw(defmid_counter_win) + theme(plot.background = element_rect(fill = "gray15", color = NA))

defmid_counter_drop <- create_Pitch(grass_colour = "gray15", 
                                   background_colour = "gray15", 
                                   line_colour = "white") + 
  geom_density_2d_filled(data = atalanta_counter_drop_defmid, 
                         aes(x = location_x, y = location_y, fill = ..level..,), 
                         alpha = 0.4, contour_var = "ndensity", 
                         breaks = seq(0.1, 1.0, length.out = 10)) + 
  theme(legend.position = "none") + 
  scale_x_continuous(limits = c(0, 120)) + 
  scale_y_continuous(limits = c(0, 80)) + 
  scale_fill_manual(values = c(palette), aesthetics = c("fill", "color")) + 
  theme(legend.position = "none", 
        plot.background = element_rect(colour = "gray15", fill = "gray15"), 
        plot.title = element_text(color = "white", hjust = .5, size = 22, 
                                  family = "Comic Sons MS", face = "bold", 
                                  vjust =  -1)) + 
  labs(title = "Atalanta Defensive Midfielders Counter Press in Non-Wins")

ggdraw(defmid_counter_drop) + theme(plot.background = element_rect(fill = "gray15", color = NA))

win_shot <- atalanta_wins %>% filter(type.name == "Shot")

win_shot <- win_shot %>% 
  mutate(
    goal = case_when(
      shot.outcome.name == "Goal" ~ "True", 
      shot.outcome.name != "Goal" ~ "False",
    )
  )

atalanta_win_shot <- win_shot %>% 
  filter(possession_team.name == "Atalanta")

opp_win_shot <- win_shot %>% 
  filter(possession_team.name != "Atalanta")

create_Pitch(grass_colour = "gray15", 
             background_colour = "gray15", 
             line_colour = "gray40") + 
  geom_point(atalanta_win_shot, mapping = aes(x = location_x, y = location_y, fill = goal, 
                                    size = shot.statsbomb_xg), color = "gray60", 
             pch = 21) + 
  scale_size_continuous(limits = c(0, 1), breaks = c(.25, .5, .75, 1), 
                        labels = c(".25", ".5", ".75", "1")) + 
  scale_fill_manual(breaks = c("True", "False"), values = c("green3", "gray15"), 
                    labels = c("Goal", "No Goal")) + 
  theme(plot.background = element_rect(colour = "gray15", fill = "gray15"), 
        plot.title = element_text(color = "white", hjust = 0.5, size = 20, 
                                  family = "Comic Sons MS", face = "bold", 
                                  vjust = -1), 
        legend.position = c(.5, .2), 
        legend.key = element_rect(fill = "transparent", color = "transparent"), 
        legend.background = element_rect(fill = "gray20", 
                                         colour = "transparent"), 
        legend.title = element_text(hjust = .4, vjust = .5, size = 10, 
                                    family = "Comic Sons MS", face = "bold", 
                                    colour = "white"), 
        legend.text = element_text(hjust = .4, vjust = .5, size = 8, 
                                   family = "Comic Sons MS", face = "bold", 
                                   colour = "white"), 
        legend.direction = "horizontal", 
        legend.box = "vertical", 
        legend.box.just = "center", 
        legend.margin = margin(t = .1, b = .1, l = .1, unit = "cm")) + 
  labs(title = "Atalanta Shots in Wins", 
       fill = "Outcome", 
       size = "xG") + 
  coord_flip(xlim = c(60, 120), ylim = c(0, 80)) + 
  guides(fill = guide_legend(order = 1))

create_Pitch(grass_colour = "gray15", 
             background_colour = "gray15", 
             line_colour = "gray40") + 
  geom_point(opp_win_shot, mapping = aes(x = location_x, y = location_y, fill = goal, 
                                              size = shot.statsbomb_xg), color = "gray60", 
             pch = 21) + 
  scale_size_continuous(limits = c(0, 1), breaks = c(.25, .5, .75, 1), 
                        labels = c(".25", ".5", ".75", "1")) + 
  scale_fill_manual(breaks = c("True", "False"), values = c("green3", "gray15"), 
                    labels = c("Goal", "No Goal")) + 
  theme(plot.background = element_rect(colour = "gray15", fill = "gray15"), 
        plot.title = element_text(color = "white", hjust = 0.5, size = 20, 
                                  family = "Comic Sons MS", face = "bold", 
                                  vjust = -1), 
        legend.position = c(.5, .2), 
        legend.key = element_rect(fill = "transparent", color = "transparent"), 
        legend.background = element_rect(fill = "gray20", 
                                         colour = "transparent"), 
        legend.title = element_text(hjust = .4, vjust = .5, size = 10, 
                                    family = "Comic Sons MS", face = "bold", 
                                    colour = "white"), 
        legend.text = element_text(hjust = .4, vjust = .5, size = 8, 
                                   family = "Comic Sons MS", face = "bold", 
                                   colour = "white"), 
        legend.direction = "horizontal", 
        legend.box = "vertical", 
        legend.box.just = "center", 
        legend.margin = margin(t = .1, b = .1, l = .1, unit = "cm")) + 
  labs(title = "Opponent Shots in Atalanta Wins", 
       fill = "Outcome", 
       size = "xG") + 
  coord_flip(xlim = c(60, 120), ylim = c(0, 80)) + 
  guides(fill = guide_legend(order = 1))

drop_shot <- atalanta_drop %>% filter(type.name == "Shot")

drop_shot <- drop_shot %>% 
  mutate(
    goal = case_when(
      shot.outcome.name == "Goal" ~ "True", 
      shot.outcome.name != "Goal" ~ "False",
    )
  )

atalanta_drop_shot <- drop_shot %>% 
  filter(possession_team.name == "Atalanta")

opp_drop_shot <- drop_shot %>% 
  filter(possession_team.name != "Atalanta")

create_Pitch(grass_colour = "gray15", 
             background_colour = "gray15", 
             line_colour = "gray40") + 
  geom_point(atalanta_drop_shot, mapping = aes(x = location_x, y = location_y, fill = goal, 
                                              size = shot.statsbomb_xg), color = "gray60", 
             pch = 21) + 
  scale_size_continuous(limits = c(0, 1), breaks = c(.25, .5, .75, 1), 
                        labels = c(".25", ".5", ".75", "1")) + 
  scale_fill_manual(breaks = c("True", "False"), values = c("green3", "gray15"), 
                    labels = c("Goal", "No Goal")) + 
  theme(plot.background = element_rect(colour = "gray15", fill = "gray15"), 
        plot.title = element_text(color = "white", hjust = 0.5, size = 20, 
                                  family = "Comic Sons MS", face = "bold", 
                                  vjust = -1), 
        legend.position = c(.5, .2), 
        legend.key = element_rect(fill = "transparent", color = "transparent"), 
        legend.background = element_rect(fill = "gray20", 
                                         colour = "transparent"), 
        legend.title = element_text(hjust = .4, vjust = .5, size = 10, 
                                    family = "Comic Sons MS", face = "bold", 
                                    colour = "white"), 
        legend.text = element_text(hjust = .4, vjust = .5, size = 8, 
                                   family = "Comic Sons MS", face = "bold", 
                                   colour = "white"), 
        legend.direction = "horizontal", 
        legend.box = "vertical", 
        legend.box.just = "center", 
        legend.margin = margin(t = .1, b = .1, l = .1, unit = "cm")) + 
  labs(title = "Atalanta Shots in Non-Wins", 
       fill = "Outcome", 
       size = "xG") + 
  coord_flip(xlim = c(60, 120), ylim = c(0, 80)) + 
  guides(fill = guide_legend(order = 1))

create_Pitch(grass_colour = "gray15", 
             background_colour = "gray15", 
             line_colour = "gray40") + 
  geom_point(opp_drop_shot, mapping = aes(x = location_x, y = location_y, fill = goal, 
                                         size = shot.statsbomb_xg), color = "gray60", 
             pch = 21) + 
  scale_size_continuous(limits = c(0, 1), breaks = c(.25, .5, .75, 1), 
                        labels = c(".25", ".5", ".75", "1")) + 
  scale_fill_manual(breaks = c("True", "False"), values = c("green3", "gray15"), 
                    labels = c("Goal", "No Goal")) + 
  theme(plot.background = element_rect(colour = "gray15", fill = "gray15"), 
        plot.title = element_text(color = "white", hjust = 0.5, size = 20, 
                                  family = "Comic Sons MS", face = "bold", 
                                  vjust = -1), 
        legend.position = c(.5, .2), 
        legend.key = element_rect(fill = "transparent", color = "transparent"), 
        legend.background = element_rect(fill = "gray20", 
                                         colour = "transparent"), 
        legend.title = element_text(hjust = .4, vjust = .5, size = 10, 
                                    family = "Comic Sons MS", face = "bold", 
                                    colour = "white"), 
        legend.text = element_text(hjust = .4, vjust = .5, size = 8, 
                                   family = "Comic Sons MS", face = "bold", 
                                   colour = "white"), 
        legend.direction = "horizontal", 
        legend.box = "vertical", 
        legend.box.just = "center", 
        legend.margin = margin(t = .1, b = .1, l = .1, unit = "cm")) + 
  labs(title = "Opponent Shots in Atalanta Non-Wins", 
       fill = "Outcome", 
       size = "xG") + 
  coord_flip(xlim = c(60, 120), ylim = c(0, 80)) + 
  guides(fill = guide_legend(order = 1))

pass_win <- atalanta_wins %>% 
  filter(type.name == "Pass")

atalanta_pass_win <- pass_win %>% 
  filter(possession_team.name == "Atalanta")

opp_pass_win <- pass_win %>% 
  filter(possession_team.name != "Atalanta")

pass_drop <- atalanta_drop %>% 
  filter(type.name == "Pass")

atalanta_pass_drop <- pass_drop %>% 
  filter(possession_team.name == "Atalanta")

opp_pass_drop <- pass_drop %>% 
  filter(possession_team.name != "Atalanta")

ggplot(atalanta_pass_win, aes(fill = prog_pass, position, group = prog_pass)) + 
  geom_bar(position = "dodge", stat = "count") + 
  xlab("Positon") + 
  ylab("Count") + 
  labs(fill = "Pass Type") + 
  ggtitle("Progressive Passing by Atalanta in Wins") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(opp_pass_win, aes(fill = prog_pass, position, group = prog_pass)) + 
  geom_bar(position = "dodge", stat = "count") + 
  xlab("Positon") + 
  ylab("Count") + 
  labs(fill = "Pass Type") + 
  ggtitle("Progressive Passing by Opponent in Atalanta Wins") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(atalanta_pass_drop, aes(fill = prog_pass, position, group = prog_pass)) + 
  geom_bar(position = "dodge", stat = "count") + 
  xlab("Positon") + 
  ylab("Count") + 
  labs(fill = "Pass Type") + 
  ggtitle("Progressive Passing by Atalanta in Non-Wins") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(opp_pass_drop, aes(fill = prog_pass, position, group = prog_pass)) + 
  geom_bar(position = "dodge", stat = "count") + 
  xlab("Positon") + 
  ylab("Count") + 
  labs(fill = "Pass Type") + 
  ggtitle("Progressive Passing by Opponent in Atalanta Non-Wins") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

cb_pass_win <- atalanta_pass_win %>% 
  filter(position == "Center Back")

cb_pass_drop <- atalanta_pass_drop %>% 
  filter(position == 'Center Back')

cb_pass_win_map <- create_Pitch(grass_colour = "gray15", 
                                    background_colour = "gray15", 
                                    line_colour = "white") + 
  geom_density_2d_filled(data = cb_pass_win, 
                         aes(x = location_x, y = location_y, fill = ..level..,), 
                         alpha = 0.4, contour_var = "ndensity", 
                         breaks = seq(0.1, 1.0, length.out = 10)) + 
  theme(legend.position = "none") + 
  scale_x_continuous(limits = c(0, 120)) + 
  scale_y_continuous(limits = c(0, 80)) + 
  scale_fill_manual(values = c(palette), aesthetics = c("fill", "color")) + 
  theme(legend.position = "none", 
        plot.background = element_rect(colour = "gray15", fill = "gray15"), 
        plot.title = element_text(color = "white", hjust = .5, size = 22, 
                                  family = "Comic Sons MS", face = "bold", 
                                  vjust =  -1)) + 
  labs(title = "Atalanta Center Backs Passing in Wins")

ggdraw(cb_pass_win_map) + theme(plot.background = element_rect(fill = "gray15", color = NA))

cb_pass_drop_map <- create_Pitch(grass_colour = "gray15", 
                            background_colour = "gray15", 
                            line_colour = "white") + 
  geom_density_2d_filled(data = cb_pass_drop, 
                         aes(x = location_x, y = location_y, fill = ..level..,), 
                         alpha = 0.4, contour_var = "ndensity", 
                         breaks = seq(0.1, 1.0, length.out = 10)) + 
  theme(legend.position = "none") + 
  scale_x_continuous(limits = c(0, 120)) + 
  scale_y_continuous(limits = c(0, 80)) + 
  scale_fill_manual(values = c(palette), aesthetics = c("fill", "color")) + 
  theme(legend.position = "none", 
        plot.background = element_rect(colour = "gray15", fill = "gray15"), 
        plot.title = element_text(color = "white", hjust = .5, size = 22, 
                                  family = "Comic Sons MS", face = "bold", 
                                  vjust =  -1)) + 
  labs(title = "Atalanta Center Backs Passing in Non-Wins")

ggdraw(cb_pass_drop_map) + theme(plot.background = element_rect(fill = "gray15", color = NA))

ggplot(atalanta_pass_win, aes("", pass.height.name, fill = pass.height.name)) + 
         geom_bar(stat = "identity", width = 1) + 
         coord_polar("y", start = 0) + 
         theme_void() + 
         labs(fill = "Pass Height") + 
         ggtitle("Atalanta Pass Height in Wins") + 
         theme(plot.title = element_text(hjust = 0.5, size = 20))

ggplot(opp_pass_win, aes("", pass.height.name, fill = pass.height.name)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) + 
  theme_void() + 
  labs(fill = "Pass Height") + 
  ggtitle("Opponent Pass Height in Atalanta Wins") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20))

ggplot(atalanta_pass_drop, aes("", pass.height.name, fill = pass.height.name)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) + 
  theme_void() + 
  labs(fill = "Pass Height") + 
  ggtitle("Atalanta Pass Height in Non-Wins") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20))

ggplot(opp_pass_drop, aes("", pass.height.name, fill = pass.height.name)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) + 
  theme_void() + 
  labs(fill = "Pass Height") + 
  ggtitle("Opponent Pass Height in Atalanta Non-Wins") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20))

