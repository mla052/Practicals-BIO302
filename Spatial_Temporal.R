# 1.	Simulate a 100-observation autocorrelated timeseries with arima.sim, with a first order autoregressive coefficient of 0.5. Also make a time vector of 1:100
library("tidyverse")
set.seed(42)
df <- data.frame(x = 1:100, y = arima.sim(list(ar=.5), n = 100)

det som står på powerpoint                
x1<-arima.sim(list(ar=.5),n=100)
x <- 1:100

# 2.	Plot the data.
plot(x1)

# 3.	Regress the timeseries against time with an OLS model. Does the model appear to be statistically significant?
y1 <- lm(x1 ~ time(x1))
summary(y1)

# Answer:
# no it is not significantly significant

# 4.	Plot the model diagnostics, including an acf and pacf of the residuals.
pacf(resid(y1))

acf(resid(y1))

# 5.	Use the Durbin-Watson test to test the residuals for autocorrelation.
library("lmtest")
dwtest(y1)

# DW = 0.97573 this is a positive autocorrelation as it is smaller than 2
# p-value is 6.532e-08

# 6.	Fit a gls with an appropriate correlation structure. Is this a better model? How have the p-value and effect size changed?
library("nlme")
year <- time(x1)
fit.gls <- gls(x1 ~ year)
summary(fit.gls)
anova(fit.gls)

fit2.gls<-gls(x1~year, corr=corAR1())
summary(fit2.gls)
anova(fit2.gls, fit.gls)

# df er høyere på fit2.gls enn fit.gls og AIC er lavere på fit2.gls enn på fit.gls så fit2.gls er en bedre modell en fit.gls

# 7.	Repeat the above 1000 times and find how autocorrelation affects the distribution of the p-value and the effect size.
library("broom")

library("broom.mixed")

mod1000 <- rerun(.n = 1000, tibble(x = 1:100, y = arima.sim(list(ar=0.5), n = 100))) %>%
  map(~gls(y ~ x, corr = corAR1(), data = .)) %>%
  map_df(tidy) %>%
  filter(term == "x")

ggplot(mod1000, aes(x = p.value)) + geom_histogram()


#effect size
mod_n <- 
    rerun(.n = 1000, data_frame(x = 1:100, y = arima.sim(list(ar=.5), n = 100))) %>% 
      map(~gls(y ~ x, corr = corAR1(), data = .)) %>% 
      map_df(tidy) %>%
  filter(term == "x")

mod_n %>%
  mutate(sig = p.value < 0.05) %>%
  ggplot(aes(x = p.value, fill = sig)) + geom_histogram()

## REAL DATA

# 1.	The built-in dataset LakeHuron has annual lake level data from 1885 to 1972 Load the data with the command data(LakeHuron)

data(LakeHuron)

# 2.	Plot the data.
plot(LakeHuron)
