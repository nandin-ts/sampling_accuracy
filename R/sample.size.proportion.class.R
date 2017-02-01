sample.size.proportion.class<-function(alpha, proportion, RE){
  ### Cochran 1944, formula 4.2. to calculate n when estimating the total number
  ### NP of units in class C, there we use relative errror instead of absolute error. 
  #alpha is desired confidence level of the estimate, for 95% confidence, it is 0.05
  zstar<-qnorm(1-alpha/2)
  #proportion is planned proportion estimate
  #ME is allowed margin of error, for 5% margin of error, it is 0.05
  n<-zstar^2*(1-proportion)/(RE^2 *proportion)
  return(n)
}#example
#sample.size.proportion.class(0.05, 0.8, 0.1)
