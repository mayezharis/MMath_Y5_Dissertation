dframe <- data.frame(x = rnorm(150))
library(ggplot2)
# three histograms of normally distributed values
ggplot(
  data=rorschach(method=null_dist("x", "norm"), n = 5, true=dframe)
) +
  geom_histogram(aes(x=x, y=..density..), binwidth=0.25) +
  facet_grid(.~.sample) +
  geom_density(aes(x=x), colour="steelblue", size=1)

# uniform distributions are not as easy to recognize as such
dframe$x = runif(150)
ggplot(
  data=lineup(method=null_dist("x", "uniform",
                                  params=list(min=0, max=1)),
                 n = 5, true=dframe)) +
  geom_histogram(aes(x=x, y=..density..), binwidth=0.1) +
  facet_grid(.~.sample) +
  geom_density(aes(x=x), colour="steelblue", size=1)


data(mtcars)
library(ggplot2)
ggplot(data=lineup(method=null_permute("mpg"), n = 20, true=mtcars)) +
  geom_boxplot(aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +facet_wrap(.~.sample) +
  theme(legend.position="none", aspect.ratio=1)