library(ggplot2)

basketball <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/nba_data_cleaned.csv")

lm <- lm(basketball$average_points_per_game ~ basketball$average_turnovers_per_game)
aug <- augment(lm)


rvf_current <- function(data) {
  data %>% ggplot(aes(x = .fitted, y = .resid)) +
    geom_point() +
    stat_smooth(method = "loess", se = FALSE, n = 250)
}



data1 <- sample_n(basketball, 250)

lm1 <- lm(data1$average_points_per_game ~ data1$average_turnovers_per_game)
aug1 <- augment(lm1)

# Use the existing linear model to predict on new data
data1$Predicted <- predict(lm1, newdata = data1)


# Create a new ggplot object for the original data and the smoothed line
plot_original <- ggplot(aug1, aes(x = .fitted, y = .resid)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  stat_smooth(col="red", method = "loess", se=FALSE, linewidth = 0.5, n = 250) +
  labs(title = "Residuals vs. Fitted",
       x = "Fitted values",
       y = "Residuals")

# Extract the model information
model_info1 <- ggplot_build(plot_original)$data[[3]][, c("x","y")]

# Extract the x values and y values for the smoothed line
x_values1 <- model_info1$x
y_values1 <- model_info1$y

# Create a new ggplot object for the original data
plot_empty <- ggplot(aug, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0) +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  labs(title = "Residuals vs. Fitted",
       x = "Fitted values",
       y = "Residuals")


# Add the smoothed line from the original plot to the new plot
plot_new <- plot_empty +
  geom_line(data = model_info1, aes(x = x, y = y), color = "red", size = 0.5)


data2 <- sample_n(basketball, 250)
lm2 <- lm(data2$average_points_per_game ~ data2$average_turnovers_per_game)
aug2 <- augment(lm2)
#
new_line <- ggplot_build(rvf_current(aug2))$data[[2]][, c("x", "y")]


data3 <- sample_n(basketball, 250)
lm3 <- lm(data3$average_points_per_game ~ data3$average_turnovers_per_game)
aug3 <- augment(lm3)
#
new_line1 <- ggplot_build(rvf_current(aug3))$data[[2]][, c("x", "y")]



history_df <- data.frame(x = numeric(), y = numeric())

model_info1$group <- nrow(history_df) / 250 + 1


history_df <- rbind(history_df, model_info1)

new_line$group <- nrow(history_df) / 250 + 1
new_line$is_recent <- "TRUE"

history_df$is_recent[1:nrow(history_df)] <- "FALSE"

history_df <- rbind(history_df, new_line)









# Display the new plot
# print(plot_new)

# 
# fit <- lm(basketball$Average.Points.per.Game ~ basketball$Age)
# aug_test <- augment(fit)
# 
# test_plot <- ggplot(data = aug_test, aes(x = .fitted, y = .resid)) + 
#   geom_point(alpha = 0) +
#   geom_hline(yintercept = 0, linetype = 2, col = "black") +
#   geom_line(data = model_info, aes(x = x, y = y), color = "red", size = 0.5)


# qq_plot <- aug %>% ggplot(aes(sample = .std.resid)) +
#   stat_qq_line() +
#   stat_qq(geom = "path")
# 
# qq_build <- ggplot_build(qq_plot)$data[[2]]["x"]

# qq_new_line$group <- nrow(qq_history_data$history) / sample_size() + 1

# nrow(qq_build)

test_lm <- lm(average_points_per_game ~ average_points_per_game, data = data1)

test_lm2 <- lm(average_blocks_per_game ~ average_points_per_game, data = data1)


data1 %>%
  ggplot(aes(x = average_points_per_game, y = average_points_per_game)) +
  geom_point(shape = 1) +
  labs(title = "Data Fit") +
  stat_smooth(method = "lm", se = FALSE, col="blue", linewidth = 0.5)



test_rbf <- augment(test_lm) %>% ggplot(aes(x = .fitted, y = .resid)) +
  geom_point(shape = 1) +
  # geom_hline(yintercept = 0, linetype = 2, color = "black") +
  stat_smooth(col="red", method = "loess", se=FALSE, linewidth = 0.5, n = 250) +
  labs(title = "Residuals vs. Fitted",
       x = "Fitted values",
       y = "Residuals")
