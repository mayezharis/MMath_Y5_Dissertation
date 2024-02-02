library(ggplot2)
library(latex2exp)


x <- runif(500, min = -1, max = 1)
y <- x^2  + rnorm(500, sd = 0.05)
plot(x,y)

fit <- lm(y ~ x)
aug <- augment(fit)
rvf <- aug %>% 
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 1) +
  stat_smooth(method = "loess", se = !TRUE, col = "red", n = length(x)) +
  labs(x = "Fitted Values",
       y = "Residuals") +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20))

rvf


sl <- aug %>% 
  ggplot(aes(x = .fitted, y = sqrt(abs(.std.resid)))) +
  geom_point(alpha = 0.6) +
  stat_smooth(method = "loess", se = !TRUE, col = "red", n = length(x)) +
  labs(x = "Fitted Values",
       y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$")) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20))

sl

qq <- aug %>%
  ggplot(aes(sample = .std.resid)) +
  stat_qq_line() +
  stat_qq() +
  labs(x = "N(0, 1) quantiles", 
       y = "Standardized residuals") +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20))

qq
