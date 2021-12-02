#ST793 Blog
rm(list = ls())
library(tseries)
# Plots used in the introduction part
set.seed(1234)
obs = 200
burnin = 50
# Generate stationary data from AR(1) process 
x1 = arima.sim(model = list(order = c(1, 0, 0), ar = 0.4), n = obs)
x1 = x1[burnin: obs]
plot(x1, type = 'l', main = 'Y(t) = 0.4Y(t-1) + a(t)')
# Generate data from AR(1) process with a unit root
x2 = arima.sim(model = list(order = c(1, 0, 0), ar = 0.99), n = obs)
x2 = x2[burnin: obs]
plot(x2, type = 'l', main = 'Y(t) = Y(t-1) + a(t)')
# Generate data from AR(1) process with drift
time = 1:obs
x3 = 1 * time + arima.sim(model = list(order = c(1, 0, 0), ar = 0.99), n = obs)
x3 = x3[burnin: obs]
plot(x3, type = 'l', main = 'Y(t) = 1 + Y(t-1) + a(t)')
# Generate data with a deterministic trend only (Y = 2 + t + a(t))
noise = rnorm(obs)
x4 = 2 + time + noise
x4 = x4[burnin: obs]
plot(x4, type = 'l', main = 'Y(t) = 2 + t + a(t)')

adf.test(x1)
adf.test(x2)
adf.test(x3)
adf.test(x4)


# Tests
library(astsa)
library(Hmisc)
set.seed(1234)
# Generate observations from the AR(1) model 1000 times.
n=seq(20,100,by=20)
N=1000
ar1=matrix(data=rep(1,N*length(n)), nrow=N)
#xmean=c(1)
pvalue=matrix(data=rep(1, N*length(n)), nrow=N)

for (i in 1:N) {
  size = n[i]
  for (j in 1:length(n)){
    x = arima.sim(model = list(order = c(1, 0, 0), ar = .9), n = size)
    fit=sarima(x, 1, 0, 0,details = FALSE)
    ar1[i,j]=fit$ttable[1,1]
    #xmean[i]=fit$ttable[2,1]
    pvalue[i,j]=fit$ttable[1,4]
  }
}



#Find the average of estimate and percentages of p-values that are significant
library(plotrix)
rhohat=colMeans(ar1)  # Returns a vector
serhohat=apply(ar1,2,sd)

ppercentage=c(1)
for (k in 1:length(n)){
  ppercentage[k]=sum(pvalue[,k]<0.05)/N
 
}
ppercentage
#alphat=mean(xmean)
#sealphat=std.error(xmean)

print(cat("Estimated rho=",rhohat, "with standrad error",serhohat))
## Estimated rho= 0.8617414 with standrad error 0.001666163NULL
print(cat("The percentage of rejection rate is:",ppercentage))
## The percentage of rejection rate is: 1NULL
#print(cat("Estimated alpha=",alphat, "with standrad error",sealphat))
#Visualize the distribution of estimate and p-values. All p-values are significant.

for (i in 1:length(n)){
  hist(ar1[,i],probability = TRUE,main =expression(paste("Histogram of ",hat(rho)," when ",rho,"=0.9")) ,xlab = bquote(hat(rho)~" when Sample Size is"~.(n[i])))
  abline(v=0.9,col="red")
  
  #plot(pvalue[,i],main ="plot of p-values" ,xlab = "p-value",type = "p")
}

library(reshape2)
library(ggplot2)
library(tidyr)
ar1 %>% gather() %>% head()
ggplot(gather(ar1), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')+geom_vline(xintercept=0.9, linetype="dashed", 
                                                color = "red", size=2)+ggtitle(expression(paste("Histogram of ",hat(rho)," when ",rho,"=0.9"))) 
