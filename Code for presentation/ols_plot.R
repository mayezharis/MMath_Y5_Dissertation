library(dplyr)
library(latex2exp)


x <- 1:20
y <- (2*x + 130) + 8*rnorm(20)

data <- data.frame(x = x, y = y)

# Fit linear regression model
lm_model <- lm(y ~ x, data = data)

# Predicted values from the linear model
predicted <- predict(lm_model)

# Create ggplot
ols_presentation <- ggplot(data, aes(x = x, y = y)) +
  geom_segment(aes(x = x, y = y, xend = x, yend = predicted), color = "black") +   # Add vertical lines
  geom_point(size = 3, color = "black") +                                        # Scatterplot of data points
  geom_smooth(method = "lm", se = FALSE, color = "black") +   # Add linear regression line without confidence interval
  labs(
    title = "Example of Simple Linear Regression",
    x = TeX("Explanatory variable, $x$"), 
    y = TeX("Response variable, $y$")
  ) +
  theme(plot.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        # panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent') #transparent legend pane
  )
ggsave('Code for presentation/ols_example_pres.png', ols_presentation, bg='transparent')
