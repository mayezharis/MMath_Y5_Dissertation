library(ggplot2)

basketball <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/nba_data_cleaned.csv")

data <- sample_n(basketball, 250)

lm <- lm(data$Average.Points.per.Game ~ data$Age)
aug <- augment(lm)

# Use the existing linear model to predict on new data
data$Predicted <- predict(lm, newdata = data)


# Create a new ggplot object for the original data and the smoothed line
plot_original <- ggplot(aug, aes(x = .fitted, y = .resid)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  stat_smooth(col="red", method = "loess", se=FALSE, linewidth = 0.5, n = 250) +
  labs(title = "Residuals vs. Fitted",
       x = "Fitted values",
       y = "Residuals")

# Extract the model information
model_info <- ggplot_build(plot_original)$data[[3]][, c("x","y")]

# Extract the x values and y values for the smoothed line
x_values <- model_info$x
y_values <- model_info$y

# Create a new ggplot object for the original data
plot_new <- ggplot(aug, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0) +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  labs(title = "Residuals vs. Fitted",
       x = "Fitted values",
       y = "Residuals")


# Add the smoothed line from the original plot to the new plot
plot_new <- plot_new +
  geom_line(data = model_info, aes(x = x, y = y), color = "red", size = 0.5)

# Display the new plot
print(plot_new)