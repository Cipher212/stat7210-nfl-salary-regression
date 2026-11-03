
# REQUIRED PACKAGES 
library(tidyverse)
library(car)
library(ggcorrplot)
library(lmtest)
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(effects)
library("tinytex")


# EDA SECTION
# ==========================================

# QUARTERBACK EDA
qb_data <- read.csv("data/qb_data_final.csv")
qb_data <- qb_data |>
  filter(!is.na(interceptions), !is.na(age_at_contract), 
         !is.na(cpoe), !is.na(bmi))

# EDA CONDUCTED
hist(qb_data$apy_adj_2026, 
     main = "Distribution of Quarterback Adjusted APY", 
     xlab = "Projected 2026 Adjusted APY ($)", 
     col = "steelblue",      # Professional blue fill
     border = "white",       # Clean white borders between bars
     breaks = 12)            # Adjusts the number of bins

# Set up a 1 row by 3 column grid for the boxplots to save space
par(mfrow = c(1, 3))

# Boxplot 1: Passing TDs
boxplot(qb_data$passing_tds, 
        main = "Scoring Volume", 
        ylab = "Passing Touchdowns", 
        col = "#ff9999",     # Light red fill
        border = "darkred",  # Dark red outline
        pch = 16)            # Solid dots for outliers

# Boxplot 2: Completion Rate
boxplot(qb_data$comp_rate, 
        main = "Passing Efficiency", 
        ylab = "Completion Rate (%)", 
        col = "#99ccff",     # Light blue fill
        border = "darkblue", # Dark blue outline
        pch = 16)

# Boxplot 3: Rushing Yards
boxplot(qb_data$rushing_yards, 
        main = "Dual-Threat Mobility", 
        ylab = "Rushing Yards", 
        col = "#99ff99",     # Light green fill
        border = "darkgreen",# Dark green outline
        pch = 16)

par(mfrow = c(1, 1))

# subset numeric columns

numeric_cols <- qb_data |> 
  select(where(is.numeric)) |>
  select(-any_of(c("player_id", "prior_year", "birth_year", "cap_value")))

numeric_cols <- numeric_cols[, colSums(is.na(numeric_cols)) < nrow(numeric_cols)]
cor_matrix <- cor(numeric_cols, use = "pairwise.complete.obs")
cor_matrix[is.na(cor_matrix)] <- 0

cat("\nTop Predictors for Adjusted APY (N =", nrow(qb_data), "):\n")
print(sort(cor_matrix[,"apy_adj_2026"], decreasing = TRUE))


ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 2, 
           method="square", colors = c("#E46764", "white", "#6D9EC1"), 
           title="Correlation Matrix: QB Performance vs Salary", ggtheme=theme_minimal())


#################################################
# WIDE RECEIVCER EDA 

wr_data <- read.csv("data/wr_data_final.csv")

# Players below 3 million dollar salary are cutoff

wr_data <- wr_data |>
  filter(apy_adj_2026 >= 3.0) |>
  mutate(
    yards_per_target = receiving_yards / targets,
    adot = receiving_air_yards / targets,
    catch_rate = receptions / targets
  ) |>
  
  drop_na(targets, yards_per_target, adot, apy_adj_2026)

# TARGET VARIABLE HISTOGRAM

hist(wr_data$apy_adj_2026, 
     main = "Distribution of Wide Receiver Adjusted APY", 
     xlab = "Projected 2026 Adjusted APY ($)", 
     col = "steelblue",      # Professional blue fill
     border = "white",       # Clean white borders between bars
     breaks = 15)            # Slightly more bins for the log-normal tail


# 1x3 EDA boxplots

par(mfrow = c(1, 3))

#  Targets (Volume)
boxplot(wr_data$targets, 
        main = "Target Volume", 
        ylab = "Total Targets", 
        col = "#ff9999",     
        border = "darkred",  
        pch = 16)            

# Yards Per Target (Efficiency)
boxplot(wr_data$yards_per_target, 
        main = "Overall Efficiency", 
        ylab = "Yards Per Target", 
        col = "#99ccff",     
        border = "darkblue", 
        pch = 16)

#  aDOT (Route Depth)
boxplot(wr_data$adot, 
        main = "Route Depth", 
        ylab = "Average Depth of Target (Yards)", 
        col = "#99ff99",     
        border = "darkgreen",
        pch = 16)


par(mfrow = c(1, 1))


# Correlatin Matrix

numeric_cols_wr <- wr_data |> 
  select(where(is.numeric)) |>
  select(-any_of(c("player_id", "prior_year", "birth_year", "cap_value")))


numeric_cols_wr <- numeric_cols_wr[, colSums(is.na(numeric_cols_wr)) < nrow(numeric_cols_wr)]
cor_matrix_wr <- cor(numeric_cols_wr, use = "pairwise.complete.obs")
cor_matrix_wr[is.na(cor_matrix_wr)] <- 0


cat("\nTop Predictors for WR Adjusted APY (N =", nrow(wr_data), "):\n")
print(sort(cor_matrix_wr[,"apy_adj_2026"], decreasing = TRUE))

# Heatmap ggplot2
ggcorrplot(cor_matrix_wr, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 2, 
           method="square", colors = c("#E46764", "white", "#6D9EC1"), 
           title="Correlation Matrix: WR Performance vs Salary", ggtheme=theme_minimal())




##################################################################################
# MODELING SECTION 



# QUARTERBACKS
# FULL UANDJUSTED MODEL
# DO NOT USE

model_full_qb <- lm(apy_adj_2026 ~ passing_tds + comp_rate + rushing_yards + 
                      interceptions + age_at_contract + cpoe + bmi, 
                    data = qb_data)
summary(model_full_qb)
par(mfrow=c(2,2))
plot(model_full_qb)
par(mfrow=c(1,1))


# FINAL MODEL

# inflation adjusted APY vs Passing Touchdowns + Completion Rate + Rushing Yards + erors

model_final_qb <- lm(apy_adj_2026 ~ passing_tds + comp_rate + rushing_yards, data = qb_data)
summary(model_final_qb)
par(mfrow=c(2,2))
plot(model_final_qb)
par(mfrow=c(1,1))

# tests
anova(model_final_qb, model_full_qb)


# VIF Test
print(vif(model_final_qb))
# Shapiro Wilke Test for normality 
print(shapiro.test(residuals(model_final_qb)))
# Breusch Pagan Test for hetereoscedasticity
print(bptest(model_final_qb))

######################################################################

# Diagnostic plots 2x2
par(mfrow = c(2, 2))
plot(model_final_qb) 
# Cook's D test
cooks_d <- cooks.distance(model_final_qb)
top_outliers <- sort(cooks_d, decreasing = TRUE)[1:3]
print(top_outliers)
# Cook's D Plot
par(mfrow = c(1, 1))
plot(model_final_qb, which = 4, 
     main = "Cook's Distance: Influential Quarterbacks",
     col = "darkred", lwd = 2)







###################################

# WIDE RECEIVER REGRESSION MODELING
###################################

wr_data <- read.csv("data/wr_data_final.csv")

# 1. Apply Filters, Engineer Variables, and Drop NAs
wr_data <- wr_data |>
  filter(apy_adj_2026 >= 3.0) |>
  mutate(
    catch_rate = receptions / targets,
    adot = receiving_air_yards / targets,
    yac_per_reception = yards_after_catch / receptions,
    yards_per_target = receiving_yards / targets,
    td_per_target = receiving_tds / targets
  ) |>
  drop_na(targets, receptions, receiving_yards, receiving_tds, 
          yards_after_catch, receiving_air_yards, receiving_epa, 
          age_at_contract, bmi, catch_rate, adot, yac_per_reception, 
          yards_per_target, td_per_target, apy_adj_2026)

# Full Model
model_full_log <- lm(log(apy_adj_2026) ~ targets + receptions + receiving_yards + 
                       receiving_tds + yards_after_catch + receiving_air_yards + 
                       receiving_epa + age_at_contract + bmi + catch_rate + adot + 
                       yac_per_reception + yards_per_target + td_per_target, 
                     data = wr_data)

# Final Reduced Model
model_reduced_log <- lm(log(apy_adj_2026) ~ targets + yards_per_target + age_at_contract, 
                        data = wr_data)

summary(model_reduced_log)
par(mfrow=c(2,2))
plot(model_reduced_log)
par(mfrow=c(1,1))


# PArtial F-Test
anova(model_reduced_log, model_full_log)


####################################################################333

# POST-HOC TESTS 

# VIF Test
print(vif(model_reduced_log))
# Shapiro Wilke 
print(shapiro.test(residuals(model_reduced_log)))
# Breusch Pagan 
print(bptest(model_reduced_log))
# Diagnostic plots
par(mfrow = c(2, 2))
plot(model_reduced_log) 
# Cook's D test
cooks_d_wr <- cooks.distance(model_reduced_log)
top_outliers_wr <- sort(cooks_d_wr, decreasing = TRUE)[1:3]
print(top_outliers_wr)
# Cook's D Plot
par(mfrow = c(1, 1))
plot(model_reduced_log, which = 4, 
     main = "Cook's Distance: Influential Wide Receivers",
     col = "darkred", lwd = 2)
# DURBIN WATSON TEST
durbinWatsonTest(model_reduced_log)





#############################################################################################33
# POSTER VISUALISATIONS 
#=======================================

# This whole section was made with Gemini AI as its just ggplot2
# Gemini is really good for making on the fly adjustments for your plots it knows all the settings for
# geom line colours and hex codes. We don't need to do this manually

# Just visualisations, but feel 
# free to check the code if you notice any mistake

# --- 1. RESIDUALS VS FITTED WITH SHADED ERROR REGIONS ---

# QB Residuals Plot
qb_res_data <- data.frame(Fitted = fitted(model_final_qb), Residuals = residuals(model_final_qb))
plot_qb_res <- ggplot(qb_res_data, aes(x = Fitted, y = Residuals)) +
  geom_point(color = "#2c3e50", alpha = 0.7, size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_smooth(method = "loess", se = TRUE, color = "#e74c3c", fill = "#ffbaba", alpha = 0.4) +
  labs(title = "QB Model: Residuals vs. Fitted",
       subtitle = "Shaded region indicates 95% confidence interval of the variance",
       x = "Fitted Adjusted APY ($)", y = "Residuals") +
  theme_minimal(base_size = 14)

# WR Residuals Plot
wr_res_data <- data.frame(Fitted = fitted(model_reduced_log), Residuals = residuals(model_reduced_log))
plot_wr_res <- ggplot(wr_res_data, aes(x = Fitted, y = Residuals)) +
  geom_point(color = "#2c3e50", alpha = 0.7, size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_smooth(method = "loess", se = TRUE, color = "#e74c3c", fill = "#ffbaba", alpha = 0.4) +
  labs(title = "WR Model: Residuals vs. Fitted",
       subtitle = "Shaded region indicates 95% confidence interval of the variance",
       x = "Fitted Log(Adjusted APY)", y = "Residuals") +
  theme_minimal(base_size = 14)

print(plot_qb_res)
print(plot_wr_res)


# PREDICTED VS ACTUAL (accuracy plot)

# QB Pred vs Actual
pred_data_qb <- data.frame(Actual = qb_data$apy_adj_2026, Predicted = fitted(model_final_qb))
plot_qb_pred <- ggplot(pred_data_qb, aes(x = Predicted, y = Actual)) +
  geom_point(color = "#2c3e50", alpha = 0.7, size = 3) +
  geom_abline(intercept = 0, slope = 1, color = "#e74c3c", linetype = "dashed", linewidth = 1) +
  labs(title = "QB Model: Predicted vs. Actual APY",
       subtitle = "Dashed line represents perfect prediction",
       x = "Predicted Adjusted APY ($)", y = "Actual Adjusted APY ($)") +
  theme_minimal(base_size = 14)

# WR Pred vs Actual (transofmred back to real dollars too)
pred_data_wr <- data.frame(Actual = wr_data$apy_adj_2026, Predicted = exp(fitted(model_reduced_log)))
plot_wr_pred <- ggplot(pred_data_wr, aes(x = Predicted, y = Actual)) +
  geom_point(color = "#2c3e50", alpha = 0.7, size = 3) +
  geom_abline(intercept = 0, slope = 1, color = "#e74c3c", linetype = "dashed", linewidth = 1) +
  labs(title = "WR Model: Predicted vs. Actual APY",
       subtitle = "Dashed line represents perfect prediction in real dollars",
       x = "Predicted Adjusted APY ($)", y = "Actual Adjusted APY ($)") +
  theme_minimal(base_size = 14)

print(plot_qb_pred)
print(plot_wr_pred)



# Age Tax negative line graph for Wide Receivers

age_effect <- effect("age_at_contract", model_reduced_log)
age_effect_df <- as.data.frame(age_effect)
# back transform  to get it in USD again
age_effect_df$fit_dollars <- exp(age_effect_df$fit)
age_effect_df$lower_dollars <- exp(age_effect_df$lower)
age_effect_df$upper_dollars <- exp(age_effect_df$upper)

# Visualisation
ggplot(age_effect_df, aes(x = age_at_contract, y = fit_dollars)) +
  geom_line(color = "#c0392b", linewidth = 1.5) +
  geom_ribbon(aes(ymin = lower_dollars, ymax = upper_dollars), alpha = 0.2, fill = "#e74c3c") +
  labs(title = "The Age Tax: WR Market Depreciation",
       subtitle = "Expected APY decline holding volume and efficiency constant",
       x = "Age at Contract Signing",
       y = "Expected Adjusted APY ($ Millions)") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1, suffix = "M")) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))




##############################################################################################################3

# SANDBOX (Discarded Code)


# # Coefficient Forest plots (unused)
# 
# # QB Forest Plot
# coefs_qb <- as.data.frame(summary(model_final_qb)$coefficients)
# ci_qb <- as.data.frame(confint(model_final_qb))
# forest_data_qb <- cbind(coefs_qb, ci_qb)[-1, ] 
# forest_data_qb$Term <- c("Passing TDs", "Completion Rate", "Rushing Yards")
# 
# plot_qb_forest <- ggplot(forest_data_qb, aes(x = Estimate, y = reorder(Term, Estimate))) +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
#   geom_errorbarh(aes(xmin = `2.5 %`, xmax = `97.5 %`), height = 0.2, color = "#2980b9", linewidth = 1) +
#   geom_point(size = 4, color = "#c0392b") +
#   labs(title = "QB Effect Sizes (Beta Coefficients)",
#        subtitle = "Error bars represent 95% Confidence Intervals",
#        x = "Estimated Impact on Adjusted APY ($)", y = "") +
#   theme_minimal(base_size = 14)
# 
# # WR Forest Plot

# coefs_wr <- as.data.frame(summary(model_reduced_log)$coefficients)
# ci_wr <- as.data.frame(confint(model_reduced_log))
# forest_data_wr <- cbind(coefs_wr, ci_wr)[-1, ] 
# forest_data_wr$Term <- c("Targets", "Targets Squared", "Yards Per Target")
# 
# plot_wr_forest <- ggplot(forest_data_wr, aes(x = Estimate, y = reorder(Term, Estimate))) +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
#   geom_errorbarh(aes(xmin = `2.5 %`, xmax = `97.5 %`), height = 0.1, color = "#2980b9", linewidth = 1) +
#   geom_point(size = 4, color = "#c0392b") +
#   labs(title = "WR Effect Sizes (Log-Linear Coefficients)",
#        subtitle = "Impact on the Natural Log of Adjusted APY",
#        x = "Estimated Beta Coefficient", y = "") +
#   theme_minimal(base_size = 14)
# 
# print(plot_qb_forest)
# print(plot_wr_forest)
# 
# 

# model_test_1 <- lm(log(apy_adj_2026) ~ log(targets) + yards_per_target, data = wr_data)
# autoplot(model_test_1)
# model_test_2 <- lm(log(apy_adj_2026) ~ targets + I(targets^2) + yards_per_target, data = wr_data)
# autoplot(model_test_2)
# model_test_3 <- lm(log(apy_adj_2026) ~ targets * yards_per_target, data = wr_data)
# autoplot(model_test_3)

# 
# model_log_log <- lm(log(apy_adj_2026) ~ log(targets) + yards_per_target, data = wr_data)
# summary(model_log_log)
# autoplot(model_log_log)
# 
# 
# model_interaction <- lm(log(apy_adj_2026) ~ targets * yards_per_target, data = wr_data)
# summary(model_interaction)
# autoplot(model_interaction)
# 
# 
# model_test_age <- lm(log(apy_adj_2026) ~ targets + yards_per_target + age_at_contract, data = wr_data)
# summary(model_test_age)
# autoplot(model_test_age, which = 1)
# 
# 



# durbinWatsonTest(model_final_qb)
# 
# # Pull the residual for Observation 27 (Lamar Jackson)
# qb_residuals <- residuals(model_final_qb)
# print(qb_residuals[27])
# 
# 
# 
# 
# # WR sqr rt model
# model_balance <- lm(log(apy_adj_2026) ~ sqrt(targets) + yards_per_target + age_at_contract, 
#                        data = wr_data)
# 

# summary(model_balance)

# gold_res_data <- data.frame(
#   Fitted = fitted(model_balance), 
#   Residuals = residuals(model_balance)
# )
# 

# library(ggplot2)
# ggplot(gold_res_data, aes(x = Fitted, y = Residuals)) +
#   geom_point(color = "#2c3e50", alpha = 0.7, size = 3) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
#   geom_smooth(method = "loess", se = TRUE, color = "#e74c3c", fill = "#ffbaba", alpha = 0.4) +
#   labs(title = "WR 'Goldilocks' Model: Residuals vs. Fitted",
#        subtitle = "Testing Sqrt(Targets) for optimal parsimony",
#        x = "Fitted Log(Adjusted APY)", 
#        y = "Residuals") +
#   theme_minimal(base_size = 14)
# 

# plot(model_goldilocks, which = 1,2)
# 
# 
# 
# 
# 
# # Spline attempts
# library(splines)
# library(car)
# 
# # Spline Model Test (failed)
# # ====================================================================
# cat("\n--- VARIATION 1: Target Spline (df = 3) ---\n")
# model_spline_1 <- lm(log(apy_adj_2026) ~ ns(targets, df = 3) + yards_per_target + age_at_contract, 
#                      data = wr_data)
# 
# print(summary(model_spline_1))
# print(vif(model_spline_1))
# 
# par(mfrow = c(2, 2))
# plot(model_spline_1, main = "Var 1: Targets df=3")
# par(mfrow = c(1, 1))
# 


