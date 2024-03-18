library(nullabor)
library(tidyverse)
library(latex2exp)
library(broom)


set.seed(234)
x <- runif(250, min = -2, max = 2)
y <- x^2 + rnorm(250, sd = 2)
samp <- data.frame(x=x, y=y)

samp_mod <- y ~ x
samp_fit <- lm(samp_mod, data = samp)


aug_samp <- augment(samp_fit)
lineup_resids <- lineup(null_lm(samp_mod, method = "boot"), true = aug_samp, pos=9)

lineup_example <- lineup_resids %>%
  ggplot(aes(x = (.fitted-mean(.fitted))/sd(.fitted), y = (.resid-mean(.resid))/sd(.resid))) +
  geom_hline(yintercept = 0, linetype = 2, color = "gray3") +
  geom_point(shape = 16, alpha=0.5) +
  geom_smooth(col="red", method = "loess", se=FALSE, linewidth = 1.25) +
  facet_wrap(~ .sample) +
  labs(x ="Fitted values", y = "Residuals") +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent'))
ggsave('Code for presentation/lineup_example.png', lineup_example, bg='transparent')


set.seed(500)

samp <- data[sample(nrow(data), 250), ] %>% 
  filter(bmi>18 | bmi < 35) %>%
  select(all_of(c("bmi", "age")))

samp_mod <- bmi ~ age
samp_fit <- lm(samp_mod, data = samp)
aug_samp <- augment(samp_fit)

rorschach_resids <- rorschach(null_lm(samp_mod, method = "boot"), true = aug_samp, p=0.1)
rorschach_example <- rorschach(null_lm(samp_mod, method = "boot"), true = aug_samp, p = 0)
lineup_rorschach_example <- lineup(null_lm(samp_mod, method = "boot"), true = aug_samp, pos = 17)

rorschach_example <- rorschach_example %>%
  group_by(.sample) %>%
  mutate(.std.resid_null = .resid/sd(.resid), 
         .scale_null = sqrt(abs(.std.resid_null))) %>%
  ggplot(aes(x =(.fitted-mean(.fitted))/sd(.fitted), y = (.scale_null-mean(.scale_null))/sd(.scale_null))) +
  geom_point(shape = 16, alpha=0.5) +
  geom_smooth(col="red", method = "loess", se=FALSE, linewidth = 1.25) +
  facet_wrap(~ .sample) +
  labs(x ="Fitted values", y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$")) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent'))
ggsave('Code for presentation/rorschach_example.png', rorschach_example, bg='transparent')
