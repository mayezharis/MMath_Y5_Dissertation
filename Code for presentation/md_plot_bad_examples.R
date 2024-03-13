library(dplyr)
library(ggplot2)
library(broom)
library(latex2exp)

set.seed(12345)
x <- runif(500, min = -1, max = 1)
y <- x^3  + rnorm(500, sd = 0.3)

fit <- lm(y ~ x)
aug <- augment(fit)
rvf <- aug %>% 
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 1) +
  stat_smooth(method = "loess", se = !TRUE, col = "red", n = length(x)) +
  labs(x = "Fitted Values",
       y = "Residuals") +
  theme(axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent'))

rvf
ggsave('Code for presentation/rvf_example_bad.png', rvf, bg='transparent')


set.seed(12345)
x <- runif(500, min = -1, max = 1)
y <- x^3  + rnorm(500, sd = 0.075)
fit <- lm(y ~ x)
aug <- augment(fit)

sl <- aug %>% 
  ggplot(aes(x = .fitted, y = sqrt(abs(.std.resid)))) +
  geom_point(alpha = 0.6) +
  stat_smooth(method = "loess", se = !TRUE, col = "red", n = length(x)) +
  labs(x = "Fitted Values",
       y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$")) +
  theme(axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent'))
sl
ggsave('Code for presentation/sl_example_bad.png', sl, bg='transparent')


set.seed(12345)
x <- runif(500, min = -1, max = 1)
y <- x^2  + rnorm(500, sd = 0.075)
fit <- lm(y ~ x)
aug <- augment(fit)

qq <- aug %>%
  ggplot(aes(sample = .std.resid)) +
  stat_qq_line() +
  stat_qq() +
  labs(x = "N(0, 1) quantiles", 
       y = "Standardized residuals") +
  theme(axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent'))

qq
ggsave('Code for presentation/qq_example_bad.png', qq, bg='transparent')
