library(tidyverse)

iris

as_tibble(iris)            #kjører datasettet iris
iris <- as_tibble(iris)

# Make a plot to show how `Petal.Length` varies between species.

iris %>% 
  select(Petal.Length, Species) %>%
  ggplot(aes(x = Species, y = Petal.Length)) + geom_boxplot()

# trykk her om du vil se det fine boxplottet mitt

# Find the variance of `Petal.Length` for each species.
iris %>%
  group_by(Species) %>%
  summarise(variance_petal.length = var(Petal.Length))

# Answer:
#   Species          variance_petal.length
#   <fct>                      <dbl>
#  1 setosa                    0.0302
#  2 versicolor                0.221 
#  3 virginica                 0.305 


# Fit an anova using `lm` between `Petal.Length` and species and examine the diagnostic plots.
mod <- lm(Petal.Length ~ Species, data = iris)

# anova
anova(mod)
summary(mod)

# examine diagnostics plot, funnet på 300b forelesing
par(mfrow=c(2,2))
plot(mod)

# Fit a `gls` for the same model. Have the coefficients changed?

library(nlme)

mod <- gls(Petal.Length ~ Species, data = iris)













