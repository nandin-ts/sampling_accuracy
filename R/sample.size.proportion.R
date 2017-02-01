sample.size.proportion<-function(alpha, proportion, ME){
  ###binomial distribution based on foody et al 2009 and Cochran 1944, formula 4.2.
  #alpha is desired confidence level of the estimate, for 95% confidence, it is 0.05
  zstar<-qnorm(1-alpha/2)
  #proportion is planned proportion estimate
  #ME is allowed margin of error, for 5% margin of error, it is 0.05
  n<-zstar^2*proportion*(1-proportion)/ME^2 
  return(n)
}
#example
#sample.size.proportion(0.05, 0.8, 0.05)
