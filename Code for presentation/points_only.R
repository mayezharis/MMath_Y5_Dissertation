library(dplyr)
library(latex2exp)


x <- runif(500)
y <- x + rnorm(500, sd = 0.25)
data <- data.frame(y=y, x=x)

data <- data.frame(x = x, y = y)

# Fit linear regression model
lm_model <- lm(y ~ x, data = data)

aug <- augment(lm_model)

# Predicted values from the linear model
predicted <- predict(lm_model)

points <- aug %>% ggplot(aes(x = .fitted, y = .resid)) +
  # geom_segment(aes(x = x, y = y, xend = x, yend = predicted), color = "red",linetype = "dashed") +   # Add vertical lines
  geom_point(size = 3, color = "black", alpha = 0.5) +                                        # Scatterplot of data points
  # geom_smooth(method = "lm", se = FALSE, color = "red") +   # Add linear regression line without confidence interval
  labs(
    title = "Example of Simple Linear Regression",
    x = TeX("Explanatory variable, $x$"), 
    y = TeX("Response variable, $y$")
  ) +
  theme(plot.title = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent') #transparent legend pane
  )
ggsave('Code for presentation/points.png', points, bg='transparent')


points_faded <- aug %>% ggplot(aes(x = .fitted, y = .resid)) +
  # geom_segment(aes(x = x, y = y, xend = x, yend = predicted), color = "red",linetype = "dashed") +   # Add vertical lines
  geom_point(size = 3, color = "black", alpha = 0.1) +                                        # Scatterplot of data points
  # geom_smooth(method = "lm", se = FALSE, color = "red") +   # Add linear regression line without confidence interval
  labs(
    title = "Example of Simple Linear Regression",
    x = TeX("Explanatory variable, $x$"), 
    y = TeX("Response variable, $y$")
  ) +
  theme(plot.title = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent') #transparent legend pane
  )
ggsave('Code for presentation/points_faded.png', points_faded, bg='transparent')
