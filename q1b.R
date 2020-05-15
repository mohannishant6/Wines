#preparing the data
data_q1b<-data[data$country=='Italy' & data$price<=20,]
counts<-aggregate(data_q1b$region_1,list(data_q1b$region_1),length)
counts<-droplevels(counts[counts$x>3,'Group.1'])
data_q1b<-droplevels(data_q1b[data_q1b$region_1 %in% counts,c('region_1','points')])
nlevels(data_q1b$region_encoded)

#mapping the factor name for future use
mapping<-data.frame(region_1=unique(data_q1b$region_1))
mapping$region_encoded=as.factor(as.integer(mapping$region_1))
data_q1b<-left_join(data_q1b,mapping,by='region_1')[,c('region_encoded','points')]

#EDA
p<-ggplot(data_q1b) + geom_boxplot(aes(x = reorder(region_encoded, points, median), points, 
                               fill = reorder(region_encoded, points, median)), show.legend=FALSE)
p+theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7))

p<-ggplot(data_q1b, aes(x = reorder(region_encoded,region_encoded, length))) + stat_count()
p+theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7))

ggplot(data_q1b, aes(points)) + stat_bin(binwidth=1)

ggplot(data.frame(size = tapply(data_q1b$points, data_q1b$region_encoded, length), 
                  mean_score = tapply(data_q1b$points, data_q1b$region_encoded, mean)), aes(size, mean_score)) + 
  geom_point() + xlab("Region Sample size") + ylab("Mean Points") + 
  ggtitle(" ")


#function for sampling
compare_m_gibbs <- function(y, ind, mu0 = 86, tau0 = 1/50, 
                     a0 = 1/50, b0 = 1/500, alpha0 =1, beta0 = 1/500, maxiter = 20000)
{
  #adding small random noise to avoid error due to zero variance within a region
  y<-y+rnorm(length(y), 1 ,1)/10000
  
  ### starting values
  m <- nlevels(ind)
  ybar <- theta <- tapply(y, ind, mean)
  tau_w <- mean(1 / tapply(y, ind, var)) ##within group precision
  mu <- mean(theta)
  tau_b <-var(theta) ##between group precision
  n_m <- tapply(y, ind, length)
  alphan <- alpha0 + sum(n_m)/2
  ###
  
  ### setup MCMC
  theta_mat <- matrix(0, nrow=maxiter, ncol=m)
  mat_store <- matrix(0, nrow=maxiter, ncol=3)
  ###
  
  ### MCMC algorithm
  for(s in 1:maxiter) 
  {
    
    # sample new values of the thetas
    for(j in 1:m) 
    {
      taun <- n_m[j] * tau_w + tau_b
      thetan <- (ybar[j] * n_m[j] * tau_w + mu * tau_b) / taun
      theta[j]<-rnorm(1, thetan, 1/sqrt(taun))
    }
    
    #sample new value of tau_w
    ss <- 0
    for(j in 1:m){
      ss <- ss + sum((y[ind == j] - theta[j])^2)
    }
    betan <- beta0 + ss/2
    tau_w <- rgamma(1, alphan, betan)
    
    #sample a new value of mu
    taum <- m * tau_b + tau0
    mum <- (mean(theta) * m * tau_b + mu0 * tau0) / taum
    mu <- rnorm(1, mum, 1/ sqrt(taum)) 
    
    # sample a new value of tau_b
    am <- a0 + m/2
    bm <- b0 + sum((theta - mu)^2) / 2
    tau_b <- rgamma(1, am, bm)
    
    #store results
    theta_mat[s,] <- theta
    mat_store[s, ] <- c(mu, tau_w, tau_b)
  }
  colnames(mat_store) <- c("mu", "tau_w", "tau_b")
  return(list(params = mat_store, theta = theta_mat))
}

fit2 <- compare_m_gibbs(data_q1b$points, data_q1b$region_encoded)
apply(fit2$params, 2, mean)
apply(fit2$params, 2, sd)
## within region standard variation
mean(1/sqrt(fit2$params[, 2]))
sd(1/sqrt(fit2$params[, 2]))
## between school standard variation
mean(1/sqrt(fit2$params[, 3]))
sd(1/sqrt(fit2$params[, 3]))
 
# get basic posterior summary
theta_hat <- apply(fit2$theta, 2, mean) 
names(theta_hat) <- 1:174 # keep track of different regions
sort(theta_hat, decreasing = TRUE) ## which regions did best and worst?

theta_ci <- apply(fit2$theta, 2, quantile, prob = c(0.025, .975)) ## upper/lower bounds for thetas
df_error <- data.frame(lower = theta_ci[1, ], upper = theta_ci[2, ], mean = theta_hat, 
                        region_1 = factor(1:174))
ggplot(df_error, aes(x = reorder(region_1, mean), mean)) + geom_errorbar(aes(ymin = lower, ymax = upper))

# reformat samples for ggplot
theta_df <- data.frame(samples = as.numeric(fit2$theta), 
                       region_1 = rep(1:ncol(fit2$theta), each = nrow(fit2$theta))) 

ggplot(theta_df) + geom_boxplot(aes(x = reorder(region_1, samples, median), samples, 
                                    fill = reorder(region_1, samples, median)), show.legend=FALSE)

ggplot(data.frame(size = tapply(data_q1b$points, data_q1b$region_encoded, length), theta_hat = theta_hat), 
       aes(size, theta_hat)) + geom_point()

ggplot(data.frame(ybar = tapply(data_q1b$points, data_q1b$region_encoded, mean), theta_hat = theta_hat), 
       aes(ybar, theta_hat)) + geom_point()

# checking differnce
mean(data_q1b$points)
mean(theta_hat)

# identify better than average regions
prediction_decoded<-subset(merge(data.frame(theta_hat),mapping,by.x=0,by.y="region_encoded"),select=-c(Row.names))
higher<-prediction_decoded[theta_hat>mean(data_q1b$points),]
higher
