

##################################
# Read Data
##################################

diabetes <- read.csv('diabetic_data.csv')


### weakly inf o rma t i v e p r i o r s
nu0<-1 ; s20<-100
eta0<-1 ; t20<-100
mu0<-50 ; g20<-25
###
### Starting values
m<-length(unique(Y[, 1]))
n<-sv<-ybar<-rep (NA,m)
for(j in 1:m)
{
  ybar[j]<-mean(Y[Y[,1]==j,2])
  sv[j ]<-var(Y[Y[,1]==j,2])
  n[j]<-sum(Y[,1]== j )
  
}
theta<-ybar ; sigma2<-mean(sv)
mu<-mean(theta) ; tau2<-var(theta)
###
### setup MCMC
set.seed(1)
S<-5000
THETA<-matrix( nrow=S , ncol=m)
SMT<-matrix( nrow=S , ncol=3)
###
### MCMC algorithm
for(s in 1:S)
{
  # sample new va lue s o f the the t a s
  for( j in 1:m)
  {
    vtheta <-1/(n[j]/ sigma2+1/tau2 )
    etheta<-vtheta*( ybar[j] * n[j] / sigma2+mu/ tau2 )
    theta[j]<-rnorm( 1 , etheta ,sqrt( vtheta ) )
  }
  #sample new value o f sigma2
  nun<-nu0+sum(n)
  ss<-nu*s20
  for( j in 1:m){ ss<-ss+sum( (Y[Y[,1]==j ,2] - theta[j])^2 ) }
  sigma2<-1/rgamma( 1 , nun/2 , ss/2)
  #sample a new value o f mu
  vmu<- 1/(m/ tau2+1/g20 )
  emu<- vmu*(m*mean( theta )/ tau2 + mu0/g20 )
  mu<-rnorm( 1 , emu , sqrt(vmu) )
  # sample a new value o f tau2
  etam<-eta0+m
  ss<- eta0 * t20 + sum((theta-mu)^2 )
  
  tau2<-1/rgamma( 1 , etam/2 , ss/2)
  #store results
  THETA[s,]<-theta
  SMT[s,]<-c( sigma2 ,mu, tau2 )
}
###