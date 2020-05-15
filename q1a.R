#data preparation
data_q1a<-data[(data$country=='South Africa' & data$variety=='Sauvignon Blanc')|(data$country=='Chile' & data$variety=='Chardonnay'),] 
data_q1a<-data_q1a[data_q1a$price==15,] %>% select(variety,points)
data_q1a<-droplevels(data_q1a)

#EDA
ggplot(data_q1a) + geom_boxplot(aes(variety, points, fill = variety)) + geom_jitter(aes(variety,points, shape = variety))

#key stats
tapply(data_q1a$points, data_q1a$variety, length)
tapply(data_q1a$points, data_q1a$variety, mean)
tapply(data_q1a$points, data_q1a$variety, median)
tapply(data_q1a$points, data_q1a$variety, sd)

#t-test to compare means
t.test(points ~ variety, data=data_q1a, var.equal = TRUE)

#bayesian model
compare_2_gibbs <- function(y, ind, mu0 = 86, tau0 = 1/400, del0 = 5, gamma0 = 1/400, 
                            a0 = 1, b0 = 50, maxiter = 5000)
{
  y1 <- y[ind == 'Sauvignon Blanc']
  y2 <- y[ind == 'Chardonnay']
  
  n1 <- length(y1) 
  n2 <- length(y2)

  ##### starting values
  mu <- (mean(y1) + mean(y2)) / 2
  del <- (mean(y1) - mean(y2)) / 2
  
  mat_store <- matrix(0, nrow = maxiter, ncol = 3)
  #####
  
  ##### Gibbs sampler
  an <- a0 + (n1 + n2)/2
  
  for(s in 1 : maxiter) 
  {
    
    ##update tau
    bn <- b0 + 0.5 * (sum((y1 - mu - del) ^ 2) + sum((y2 - mu + del) ^ 2))
    tau <- rgamma(1, an, bn)
    ##
    
    ##update mu
    taun <-  tau0 + tau * (n1 + n2)
    mun <- (tau0 * mu0 + tau * (sum(y1 - del) + sum(y2 + del))) / taun
    mu <- rnorm(1, mun, sqrt(1/taun))
    ##
    
    ##update del
    gamman <-  tau0 + tau*(n1 + n2)
    deln <- ( del0 * tau0 + tau * (sum(y1 - mu) - sum(y2 - mu))) / gamman
    del<-rnorm(1, deln, sqrt(1/gamman))
    ##
    
    ## store parameter values
    mat_store[s, ] <- c(mu, del, tau)
  }
  colnames(mat_store) <- c("mu", "del", "tau")
  return(mat_store)
}

#fitting the model
library(MCMCpack)
fit <- compare_2_gibbs(data_q1a$points, as.factor(data_q1a$variety))
plot(as.mcmc(fit),smooth=0)

#basic stats of posterior
raftery.diag(as.mcmc(fit))
apply(fit, 2, mean)
apply(fit, 2, sd)
mean(1/sqrt(fit[, 3]))
sd(1/sqrt(fit[, 3]))

#simulating the two classes
y1_sim <- rnorm(5000, fit[, 1] + fit[, 2], sd = 1/sqrt(fit[, 3]))
y2_sim <- rnorm(5000, fit[, 1] - fit[, 2], sd = 1/sqrt(fit[, 3]))

#looking at the differences in points
ggplot(data.frame(y_sim_diff = y1_sim - y2_sim)) + stat_bin(aes(y_sim_diff),binwidth=1)

hist_top <- ggplot()+geom_histogram(aes(y1_sim))
empty <- ggplot()+geom_point(aes(1,1), colour="white")+
  theme(axis.ticks=element_blank(), 
        panel.background=element_blank(), 
        axis.text.x=element_blank(), axis.text.y=element_blank(),           
        axis.title.x=element_blank(), axis.title.y=element_blank())

scatter <- ggplot(data.frame(y1_sim, y2_sim)) + geom_point(aes(y1_sim, y2_sim), alpha = 0.3) + geom_abline(slope = 1, intercept = 0)
hist_right <- ggplot()+geom_histogram(aes(y2_sim))+coord_flip()
grid.arrange(hist_top, empty, scatter, hist_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))

#getting the probability
mean(y1_sim > y2_sim)
