
# INSTALL THESE PACKAGES IF YOUDONT HAVE THEM
library(nflreadr)
library(car)
library(MASS)
library(lmtest)
library(sandwich)
library(tidyverse)

# RESEARCH QUESTION:
#Research Question: To what extent can an NFL team's total seasonal offensive output (Points Scored) 
#be predicted by their play-by-play passing and rushing efficiency?


# Null (H0)
# There is no statistically significant linear relationship 
#between Pass EPA  or Rush EPA  and Total Points Scored.

# Alternate (HA)
#At least one efficiency metric Pass EPA , or Rush EPA has a 
#significant linear relationship with Total Points Scored.




# GENERATING DATASET FROM NFL VERSE PACKAGE IN R


# 2025 season raw data

pbp <- load_pbp(2025) |>  #pbp = play by play
  filter(season_type == "REG", !is.na(posteam))

schedules <- load_schedules(2025) |> 
  filter(game_type == "REG", !is.na(home_score))

# total points scored
home_pts <- schedules |> 
  group_by(team = home_team) |> 
  summarize(pts = sum(home_score))


# away points
away_pts <- schedules |> 
  group_by(team = away_team) |> 
  summarize(pts = sum(away_score))


# totalpoints
total_points_df <- bind_rows(home_pts, away_pts) |> 
  group_by(team) |> 
  summarize(total_points = sum(pts))

# DV play by play metrics

offense_metrics <- pbp |>
  group_by(team = posteam) |>
  summarize(
    pass_epa_play = mean(epa[pass == 1], na.rm = TRUE),
    rush_epa_play = mean(epa[rush == 1], na.rm = TRUE),
    early_down_success = mean(success[down %in% c(1, 2)], na.rm = TRUE),
    late_down_success = mean(success[down %in% c(3, 4)], na.rm = TRUE),
    proe = mean(pass_oe, na.rm = TRUE),
    
    #offensive penalties
    penalty_yards = sum(penalty_yards[penalty_team == posteam], na.rm = TRUE),
    
    # giveaways per drive
    total_turnovers = sum(interception == 1 | fumble_lost == 1, na.rm = TRUE),
    total_drives = n_distinct(game_id, drive),
    turnover_rate = total_turnovers / total_drives,
    .groups = "drop"
  )

# mean field start position

field_position <- pbp |>
  filter(!is.na(drive), !is.na(yardline_100)) |>
  group_by(team = posteam, game_id, drive) |>
  slice(1) |> # Grabs only the first play of every drive
  ungroup() |>
  group_by(team = posteam) |>
  summarize(avg_start_position = mean(yardline_100, na.rm = TRUE))

# join all data
nfl_model_data <- total_points_df |>
  inner_join(offense_metrics, by = "team") |>
  inner_join(field_position, by = "team") |>
  select(-total_turnovers, -total_drives) # Drop raw counting stats used for rate formulas


head(nfl_model_data)


# EDA


par(mfrow = c(1, 2))

# Histogram: Check for shape and modality
hist(nfl_model_data$total_points, 
     breaks = 10, col = "lightblue", border = "white",
     main = "Distribution of Total Points",
     xlab = "Total Points Scored")

# Boxplot: Check for spread and identifying outliers (the dots)
boxplot(nfl_model_data$total_points, 
        col = "orange", main = "Boxplot of Total Points",
        ylab = "Points")

# Reset grid
par(mfrow = c(1, 1))


# bivariate
# Scatter plot with a regression line
plot(nfl_model_data$pass_epa_play, nfl_model_data$total_points,
     pch = 19, col = "darkblue",
     main = "Pass EPA vs. Total Points",
     xlab = "Pass EPA per Play", ylab = "Total Points")

# fitted line
abline(lm(total_points ~ pass_epa_play, data = nfl_model_data), 
       col = "red", lwd = 2)


# this code adds the team labels to the points
# atlanta falcons is somewhere in the model you can see
text(nfl_model_data$pass_epa_play, nfl_model_data$total_points, 
     labels = nfl_model_data$team, pos = 3, cex = 0.7)



# correlatoin matrix

numeric_cols <- nfl_model_data[, sapply(nfl_model_data, is.numeric)]

# Pairs Plot: A visual grid of every relationship
pairs(numeric_cols, 
      main = "NFL Model Data: Pairs Plot",
      pch = 19, col = "darkred", cex = 0.7)


print(round(cor(numeric_cols), 2))

#_________________________________________________
# CRUDE MODEL




# BASELINE MODEL
baseline_model <- lm(total_points ~ pass_epa_play + rush_epa_play + 
                       early_down_success + late_down_success + proe + 
                       penalty_yards + turnover_rate + avg_start_position, 
                     data = nfl_model_data)

print(summary(baseline_model))

# colinearity cehck
vif_values <- vif(baseline_model)
print(vif_values)

# Vif plot
barplot(vif_values, main = "VIF Scores by Variable", horiz = TRUE, col = "steelblue", las = 1)
abline(v = 5, col = "red", lwd = 2, lty = 2) # Adding a red threshold line at VIF = 5



# ADJUSTED MODEL

# Section 3: The Adjusted Model and Diagnostics


adjusted_model <- lm(total_points ~ pass_epa_play + rush_epa_play + 
                       late_down_success + proe, 
                     data = nfl_model_data)



print(summary(adjusted_model))

# VIF test
print(vif(adjusted_model))


par(mfrow = c(1, 2))

# Plot 1: Residuals vs Fitted 

plot(adjusted_model, which = 1, 
     col = "darkblue", pch = 16, 
     main = "Residuals vs Fitted")

# Plot 2: Normal Q-Q 

plot(adjusted_model, which = 2, 
     col = "darkred", pch = 16, 
     main = "Normal Q-Q")

par(mfrow = c(1, 1))



# Cook's D test

cooks_d <- cooks.distance(adjusted_model)

# define threshhold
threshold <- 4 / nrow(nfl_model_data)

# cook's d plot
plot(cooks_d, pch = 16, col = "darkblue", type = "h",
     main = "Cook's Distance: Identifying Influential Teams",
     ylab = "Cook's Distance", xlab = "Team Index")
abline(h = threshold, col = "red", lty = 2) # Add threshold line

#lebl
text(x = 1:length(cooks_d), y = cooks_d, 
     labels = ifelse(cooks_d > threshold, as.character(1:length(cooks_d)), ""), 
     pos = 3, cex = 0.8, col = "darkred")


cat("\n--- HIGH LEVERAGE TEAMS 15,12,28 ---\n")
influential_teams <- nfl_model_data[c(12, 15, 28), ]
print(influential_teams |> select(team, total_points, pass_epa_play, rush_epa_play))





# box cox testing




bc_test <- boxcox(adjusted_model, lambda = seq(-2, 2, by = 0.1), plotit = TRUE)


optimal_lambda <- bc_test$x[which.max(bc_test$y)]


print(optimal_lambda)




# final adjusted model




base_model <- lm(total_points ~ pass_epa_play + rush_epa_play + 
                   late_down_success + proe, 
                 data = nfl_model_data)


print(summary(base_model))


robust_summary <- coeftest(base_model, vcov = vcovHC(base_model, type = "HC3"))
print(robust_summary)




# Final Attempt


final_model <- lm(total_points ~ pass_epa_play + rush_epa_play, 
                  data = nfl_model_data)


final_robust_summary <- coeftest(final_model, vcov = vcovHC(final_model, type = "HC3"))
print(final_robust_summary)

# 3. Generate the Diagnostic Plots
par(mfrow = c(1, 2))


plot(final_model, which = 1, 
     col = "darkblue", pch = 16, 
     main = "Residuals vs Fitted (Final Model)")
plot(final_model, which = 2, 
     col = "darkred", pch = 16, 
     main = "Normal Q-Q (Final Model)")

par(mfrow = c(1, 1))



# Adjusted Model (Log transformation)

# log model
log_model <- lm(log(total_points) ~ pass_epa_play + rush_epa_play, 
                data = nfl_model_data)


print(summary(log_model))

par(mfrow = c(1, 2))

plot(log_model, which = 1, 
     col = "darkblue", pch = 16, 
     main = "Log-Model Residuals vs Fitted")

plot(log_model, which = 2, 
     col = "darkred", pch = 16, 
     main = "Log-Model Normal Q-Q")

par(mfrow = c(1, 1))


# Square Root test

# 1. Fit the Square Root Transformed Model
# We apply the sqrt() function directly to the dependent variable
sqrt_model <- lm(sqrt(total_points) ~ pass_epa_play + rush_epa_play, 
                 data = nfl_model_data)


print(summary(sqrt_model))


par(mfrow = c(1, 2))

plot(sqrt_model, which = 1, 
     col = "darkblue", pch = 16, 
     main = "Sqrt-Model Residuals vs Fitted")

plot(sqrt_model, which = 2, 
     col = "darkred", pch = 16, 
     main = "Sqrt-Model Normal Q-Q")

par(mfrow = c(1, 1))



# AI coded this
# Weighted Least Squares:

# residuals
resid_abs <- abs(residuals(final_model))


var_model <- lm(resid_abs ~ pass_epa_play + rush_epa_play, data = nfl_model_data)


#weights
w_weights <- 1 / (predict(var_model)^2)


# model
wls_model <- lm(total_points ~ pass_epa_play + rush_epa_play, 
                data = nfl_model_data, 
                weights = w_weights)

#Diagnostic Plots
par(mfrow = c(1, 2))

plot(wls_model, which = 1, col = "darkblue", pch = 16, main = "WLS Residuals vs Fitted")
plot(wls_model, which = 2, col = "darkred", pch = 16, main = "WLS Normal Q-Q")
par(mfrow = c(1, 1))

print(summary(wls_model))

