

# Pacakges
library(dplyr)
library(ggplot2)
library(brms)
library(tidyverse)
library(tidybayes)
library(LaplacesDemon)

##################################
# Read Data
##################################

diabetes <- read.csv('diabetic_data.filtered.csv')

x_values <- seq(-15,15, length.out = 1000)
data.frame(x_values) %>%
  ggplot(aes(x_values))+
  stat_function(fun=dst, args=list(nu=3,mu=0,sigma=2.5))


m1priors <- c(
  prior(student_t(3, 0, 2.5), class = "Intercept"),
  prior(student_t(3, 0, 2.5), class = "b")
)

isFALSE <-
  function (x) 
  {
    identical(FALSE, x)
  }

m1 <- brm(
  readmitted~insulin,
  data = new_diab,
  prior = m1priors,
  family = "bernoulli",
  seed = 123 # Adding a seed makes results reproducible.
) 





