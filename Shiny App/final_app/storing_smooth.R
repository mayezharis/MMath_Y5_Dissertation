library(ggplot2)

pp<-ggplot(mtcars, aes(x=hp, y=wt)) +   geom_point() +   geom_smooth()

plot_info <- ggplot_build(pp)
