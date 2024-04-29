library(hockeyR)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpath)
library(gt)
library(gtExtras)
library(ggtext)

shots <- read_csv("shots_2023.csv") 

shots <- shots %>%
  filter(isPlayoffGame == 0) %>%
  select(game_id, period, pos_team = teamCode, xg = xGoal, goal)

regulation <- shots %>%
  filter(period <= 3) %>%
  group_by(game_id, pos_team) %>%
  summarize(xg = sum(xg), goals = sum(goal)) %>%
  ungroup() %>%
  group_by(game_id) %>%
  mutate(team_num = row_number())

regulation <- pivot_wider(regulation, names_from = team_num, values_from = c(pos_team, xg, goals), names_glue = "{.value}_{team_num}")

probs <- function(xg_1, xg_2) {
  win_prob_1 <- 0
  win_prob_2 <- 0
  draw_prob <- 0
  
  for (goals_1 in 0:15) {
    for (goals_2 in 0:15) {
      if (goals_1 > goals_2) {
        win_prob_1 <- win_prob_1 + dpois(goals_1, xg_1) * dpois(goals_2, xg_2)
      } else if (goals_2 > goals_1) {
        win_prob_2 <- win_prob_2 + dpois(goals_1, xg_1) * dpois(goals_2, xg_2)
      } else {
        draw_prob <- draw_prob + dpois(goals_1, xg_1) * dpois(goals_2, xg_2)
      }
    }
  }
  
  return(data.frame(win_prob_1, win_prob_2, draw_prob))
}

regulation_probs <- probs(regulation$xg_1, regulation$xg_2)


overtime <- shots %>%
  filter(period == 4) %>%
  group_by(game_id, pos_team) %>%
  summarize(ot_xg = sum(xg), ot_goals = sum(goal)) %>%
  ungroup() %>%
  group_by(game_id) %>%
  mutate(team_num = row_number())

overtime <- pivot_wider(overtime, names_from = team_num, values_from = c(pos_team, ot_xg, ot_goals), names_glue = "{.value}_{team_num}")

overtime$ot_xg_2[which(is.na(overtime$ot_xg_2))] <- 0
overtime$ot_goals_2[which(is.na(overtime$ot_goals_2))] <- 0

overtime <- overtime %>% select(-pos_team_1, -pos_team_2)

overtime_probs <- probs(overtime$ot_xg_1, overtime$ot_xg_2)

overtime_probs <- overtime_probs %>% select(ot_win_prob_1 = win_prob_1, ot_win_prob_2 = win_prob_2, ot_draw_prob = draw_prob)

regulation <- cbind(regulation, regulation_probs)
overtime <- cbind(overtime, overtime_probs)

games <- left_join(regulation, overtime, by = "game_id")

# some of the goal values in games are not perfectly right if penalty shots occurred and therefore should not be considered, goals are just to see theyre not required in analysis

# some assumptions will be made
# 1. for games that didn't go to overtime, for the draw probability, we assume that the team will have the same win, draw, and loss probabilities for OT with a draw in overtime leading to a penalty shootout
# 2. if it does go to a penalty shootout, a 50% probability is given to both teams to win it

games <- games %>%
  mutate(xpoints_1 = ifelse(is.na(ot_xg_1), (2 * (win_prob_1 + draw_prob * win_prob_1 + draw_prob * draw_prob * 0.5) + 1 * (draw_prob + draw_prob * win_prob_2 + draw_prob * draw_prob * 0.5)), (2 * (win_prob_1 + draw_prob * ot_win_prob_1 + draw_prob * ot_draw_prob * 0.5) + 1 * (draw_prob + draw_prob * ot_win_prob_2 + draw_prob * ot_draw_prob * 0.5))), xpoints_2 = ifelse(is.na(ot_xg_1), (2 * (win_prob_2 + draw_prob * win_prob_2 + draw_prob * draw_prob * 0.5) + 1 * (draw_prob + draw_prob * win_prob_1 + draw_prob * draw_prob * 0.5)), (2 * (win_prob_2 + draw_prob * ot_win_prob_2 * draw_prob * ot_draw_prob * 0.5) + 1 * (draw_prob + draw_prob * ot_win_prob_1 * draw_prob * ot_draw_prob * 0.5))))

condensed <- games %>% select(game_id, pos_team_1, pos_team_2, xpoints_1, xpoints_2)

indiv_exp <- condensed %>% pivot_longer(cols = starts_with("pos_team_"), names_to = "team_pos", values_to = "team") %>%
  mutate(xpoints = ifelse(team_pos == "pos_team_1", xpoints_1, xpoints_2)) %>%
  select(game_id, team, xpoints) %>%
  group_by(team) %>%
  mutate(game_num = row_number()) %>%
  ungroup()

# https://shanemcd.org/2023/08/23/2023-24-nhl-schedule-and-results-in-excel-xlsx-and-csv-formats/

results <- read_csv("nhl-202324-asplayed.csv")

results <- results %>%
  rename(team_1 = Visitor, score_1 = Score...5, team_2 = Home, score_2 = Score...7) %>%
  mutate(points_1 = ifelse(score_1 > score_2, 2, ifelse(score_1 < score_2 & Status != "Regulation", 1, 0)), points_2 = ifelse(score_2 > score_1, 2, ifelse(score_2 < score_1 & Status != "Regulation", 1, 0))) %>%
  select(team_1, points_1, team_2, points_2)

names <- team_logos_colors %>%
  select(abbr = team_abbr, team = full_team_name)

results <- left_join(results, names, by = c("team_1"="team")) %>% rename(abbr_1 = abbr)
results <- left_join(results, names, by = c("team_2"="team")) %>% rename(abbr_2 = abbr)

results <- results %>% select(-team_1, -team_2)

results <- results %>%
  pivot_longer(cols = starts_with("abbr"), names_to = "team", values_to = "abbr") %>%
  mutate(points = ifelse(team == "abbr_1", points_1, points_2)) %>%
  select(team = abbr, points) %>%
  group_by(team) %>%
  mutate(game_num = row_number())

indiv <- left_join(indiv_exp, results, by = c("team", "game_num"))

totals <- indiv %>%
  group_by(team) %>%
  summarize(xpoints = sum(xpoints), points = sum(points))

totals$xpoints <- round(totals$xpoints)

totals <- totals %>% mutate(points_over_exp = points - xpoints)

gt_align_caption <- function(left, right) {
  caption <- paste0(
    '<span style="float: left;">', left, '</span>',
    '<span style="float: right;">', right, '</span>'
  )
  return(caption)
}

caption = gt_align_caption("Data from <b>MoneyPuck</b>", "Amrit Vignesh | <b>@avsportsanalyst</b>")

logos <- team_logos_colors

logos <- logos %>%
  select(team = team_abbr, name = full_team_name, logo = team_logo_espn)

totals <- left_join(totals, logos, by = "team") %>%
  arrange(-xpoints) %>%
  select(logo, name, xpoints, points, points_over_exp)

total_table <- totals %>% gt() %>% 
  gt_img_rows(columns = logo) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(logo, name, xpoints, points, points_over_exp)
  ) %>%
  gt_hulk_col_numeric(c(xpoints, points, points_over_exp)) %>%
  cols_label(
    logo = md(""),
    name = md("**Team**"),
    xpoints = md("**Exp. Points**"),
    points = md("**Points**"),
    points_over_exp = md("**Points Over Exp.**")
  ) %>%
  tab_header(
    title = md("**2023/24 NHL Expected Points Table**"),
    subtitle = md("*Based on **xG** Generation*")
  ) %>%
  opt_align_table_header(align = "center") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(name, xpoints, points, points_over_exp)
    )
  ) %>%
  tab_source_note(html(caption)) 

gtsave(total_table, "total_table.png", vwidth = 5000, vheight = 4000)

total_plot <- totals %>%
  ggplot(aes(x = xpoints, y = points)) + 
  geom_hline(yintercept = mean(totals$points), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(totals$xpoints), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_smooth(method = "lm") + 
  geom_from_path(aes(path = logo), height = 0.05) +
  labs(x = "Expected Points", 
       y = "Points",
       title = "2023/24 NHL Expected vs Actual Points",
       caption = "Data from **MoneyPuck** | Amrit Vignesh | **@avsportsanalyst**") + 
  theme(plot.title = element_markdown(size=14, hjust=0.5, face="bold"), axis.title = element_text(face="bold"), plot.caption = element_markdown(hjust = 0.5))

ggsave("total_plot.png", total_plot)

indiv <- indiv %>%
  group_by(team) %>%
  mutate(cumulative_xpoints = cumsum(xpoints),
         cumulative_points = cumsum(points)) %>%
  ungroup()

subfolder_path <- "indivs/"
dir.create(subfolder_path, showWarnings = FALSE)

for (sel_team in unique(indiv$team)) {
  indiv_team <- indiv %>% filter(team == sel_team)
  
  indiv_image <- indiv_team %>% 
    slice_max(order_by = game_num, n = 1)
  
  indiv_plot <- indiv_team %>%
    ggplot(aes(game_num)) +
    geom_line(aes(y = cumulative_xpoints, color = "xPoints")) +
    geom_line(aes(y = cumulative_points, color = "Points")) +
    scale_color_manual(values = c("xPoints" = "red", "Points" = "blue"), name = "") +
    geom_image(data = indiv_image, aes(x = game_num, y = cumulative_points, image = logos$logo[which(logos$team == sel_team)]), size = 0.05) + 
    labs(title = paste0("2023/24 Points vs xPoints Progression: ", sel_team),
         x = "Game Number",
         y = "Points") + 
    theme(plot.title = element_markdown(size=14, hjust=0.5, face="bold"), axis.title = element_text(face="bold"), plot.caption = element_markdown(hjust = 0.5))
  
  ggsave(file.path(subfolder_path, paste0(sel_team, ".png")), indiv_plot)
}






