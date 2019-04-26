
library(ggplot2)
library(dplyr)

# load data 
df <- read.csv('dataset_diabetes/diabetic_data.csv')
map <- read.csv('dataset_diabetes/IDs_mapping.csv')
df[df == '?'] = NA
df[df == 'None'] = NA

# remove columns with NAs
#df <- df[,colSums(is.na(df)) == 0]

# relabel readmitted
df$readmitted = ifelse(df$readmitted=='NO',0,1)

# turn treatment into binomial
df[22:47] <- ifelse(df[22:47] == 'No', 0, 1)

# process insulin dataframe
df.insulin <- df[c('insulin','age')] #subset dataframe
df.insulin$insulin <- ifelse(df.insulin$insulin == 0, 'No', 'Yes') #relabel
df.insulin$insulin <- as.factor(df.insulin$insulin) 

# edit age labels
df.insulin$age <- gsub("[^0-9-]","",df.insulin$age) # remove [ )
df.insulin$age <- gsub("-","_",df.insulin$age)
df.insulin$age <- paste("age" , df.insulin$age, sep='_')

#plot histogram
ggplot(df.insulin, aes(x=insulin))+
    stat_count(width = 0.5)+
    facet_wrap(~age,nrow = 2)+
    xlab('Insulin Usage')+
    geom_text(stat='count', aes(label=..count..), vjust=-0.2)+
    theme_bw()

#split dataset by age group
df.insulin.split <- split(df.insulin, df.insulin$age)

table(df.insulin.split$age_0_10)

# summarize splits
for (i in 1: length(df.insulin.split)){
    #df.insulin.split[[i]] <- table(df.insulin.split[[i]])
    a <- table(df.insulin.split[[i]])[1]
    b <- table(df.insulin.split[[i]])[2]
    n <- a + b
    theta <- theta <- a/(a + b)
}

install.packages('bang')

library(bang)

df.new <- df[c('insulin','age')]
df.new$age <- as.numeric(factor(df.new$age))

df.new$age <- as.numeric(factor(df.new$age))
df.new$insulin <- as.numeric(df.new$insulin)

colnames(df.new) <- c('y','n')

df.new$y[df.new$y == 0] <- 2

df.new$n <- df.new$n + 10

success.df <- df.new %>% filter(y == 1) %>% group_by(n) %>% summarise(success=n())

count.df <- df.new %>% group_by(n) %>% summarise(count=n())

summary.df <- merge(success.df, count.df)

summary.2.df <- summary.df[,2:3]


rat_res <- hef(model = "beta_binom", data = rat, n = 10000)

plot(rat_res, ru_scale = TRUE)

acf(rat_res$sim_vals[,2])

plot(1:10000,rat_res$sim_vals[,1], type='l')

#rat <- as.data.frame(rat)
y <- summary.2.df$success
n <- summary.2.df$count
J <- length(y)

log.prior <- function(alpha,beta) {
  {-2.5}*log(alpha + beta)
}

draw.thetas <- function(alpha,beta) {
  return(rbeta(J,alpha+y,beta+n-y))
}

draw.alpha <- function(alpha,beta,theta,prop.sd) {
  alpha.star <- rnorm(1,alpha,prop.sd)
  num <- J*(lgamma(alpha.star+beta) - lgamma(alpha.star)) +
    alpha.star*sum(log(theta)) + log.prior(alpha.star,beta)
  den <- J*(lgamma(alpha+beta)      - lgamma(alpha)) +
    alpha     *sum(log(theta)) + log.prior(alpha,beta)
# print(c(alpha,alpha.star,num,den))
  acc <- ifelse((log(runif(1))<=num - den)&&(alpha.star>0),1,0)
  alpha.acc <<- alpha.acc + acc
  return(ifelse(acc,alpha.star,alpha))
}

draw.beta <- function(alpha,beta,theta,prop.sd) {
  beta.star <- rnorm(1,beta,prop.sd)
  num <- J*(lgamma(alpha+beta.star) - lgamma(beta.star)) +
    beta.star*sum(log(1-theta)) + log.prior(alpha,beta.star)
  den <- J*(lgamma(alpha+beta)      - lgamma(beta)) +
    beta     *sum(log(1-theta)) + log.prior(alpha,beta)
# print(c(beta,beta.star,num,den))
  acc <- ifelse((log(runif(1))<=num - den)&&(beta.star>0),1,0)
  beta.acc <<- beta.acc + acc

  return(ifelse(acc,beta.star,beta))
}

B <- 20000
M <- 200000

MM <- B + M

alpha <- matrix(NA,MM)
beta <- alpha
theta <- matrix(NA,nrow=MM,ncol=J)

# Metropolis tuning parameters
alpha.prop.sd <-  0.6
beta.prop.sd <-   3.2

# Initial values for the chain
alpha[1] <- 1
beta[1] <- 1
#theta[1,] <- draw.thetas(alpha[1],beta[1]) # or 
theta[1,] <- (y+.5/(n+.5))

# Monitor acceptance frequency
alpha.acc <- 0
beta.acc <- 0

# MCMC simulation
for (m in 2:MM) {
  alpha[m] <- draw.alpha(alpha[m-1],beta[m-1],theta[m-1,],alpha.prop.sd)
  beta[m] <- draw.beta(alpha[m],beta[m-1],theta[m-1,],beta.prop.sd)
  theta[m,] <- draw.thetas(alpha[m],beta[m])
}

good <- (B+1):MM

alpha.mcmc <- alpha[good]
beta.mcmc <- beta[good]
theta.mcmc <- theta[good,]


par(mfrow=c(2,2))
plot(alpha.mcmc,type="l")
plot(beta.mcmc,type="l")
acf(alpha.mcmc,100)
acf(beta.mcmc,100)

print(round(c(alpha.rate=alpha.acc/MM,beta.rate=beta.acc/MM),2))


library(MASS)

par(mfrow=c(3,2))

plot(density(alpha))
plot(density(beta))

contour(kde2d(alpha,beta),xlab="alpha",ylab="beta")
contour(kde2d(log(alpha/beta),log(alpha+beta)),
        xlab="alpha/beta",ylab="log(alpha+beta)")

persp(kde2d(alpha,beta),theta=45,phi=45)
persp(kde2d(log(alpha/beta),log(alpha+beta)),theta=45,phi=45)

theta
