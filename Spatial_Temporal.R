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

# 3.	Regress the LakeHuron lake level against year using a linear model. Is there a significant trend?

year<-time(LakeHuron)
fit.lm<-lm(LakeHuron~year)
summary(fit.lm)

# p-value is 3.545e-08 so yes there is a significant trend

# 4.	Plot the autocorrelation and partial autocorrelation functions for the residuals from this regression. Interpret them.
acf(resid(fit.lm))

pacf(resid(fit.lm))

# because the acf follows the weight matrix (approximately) and the pacf does not go from 1 to only 0's we can interpret that this has correlation.


# 5.	Fit an autoregressive models to the residuals. Compare the results with your interpretation of the PACF plot.

ar(x = resid(fit.lm))

# the coefficients shows the rho values
# this looks similar to the PACF plot
# something about termes that I do not understand


# 6.	Fit a gls model using a corAR1 correlation structure. Test if the correlation structure is necessary. Is the trend significant?
year<-time(LakeHuron)
fit3.gls<-gls(LakeHuron~year)
summary(fit3.gls)

fit4.gls<-gls(LakeHuron~year, corr=corAR1())
summary(fit4.gls)
anova(fit3.gls, fit4.gls)

# Answer the df is lower in fit3.gls and it is higher at AIC in fit3.gls meaning that fit4.gls is the better model. p-value is <.0001 therefore it significant.


# 7.	Fit a gls model using a corARMA correlation structure with two AR terms. Is this model an improvement?
fit5.gls <- gls(LakeHuron ~ year, corr = corARMA(p = 2))
summary(fit5.gls)

# p-value is 0.0223 so this means this model is an improvement. 

# the ar function and the corARMA gives almost the same coefficients, this is only because one is built later than the other one. 

