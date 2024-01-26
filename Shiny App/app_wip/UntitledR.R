data <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/cric_bat_data.csv")
fit <- lm(no_of_runs_scored ~ minutes_batted, data = data)
aug <- augment(fit)

aug_test <- aug %>%
  mutate(.new = sqrt(abs(.std.resid)))

aug_minmax <- aug %>% 
  filter(if_any(everything(), ~.x == max(.x) | .x == min(.x)))


for (col in 1:length(colnames(aug))) {
  print(c(max(aug[[col]]), min(aug[[col]])))
}

