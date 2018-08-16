poissonProcessSim <- function(i, l, t, lambda){
  x = numeric(); #initialise X
  x[1] = i; #initial state of the process
  tl = numeric(); #initialise arrival times
 
  #Generate arrival times : t0 = 0, t1 = e1, t2 = e1 + e2, ..., tl = e1 + ... + el
  tl = c(0, cumsum(rexp(l-1,lambda)))
  
  for (j in 1:length(t)) {
    for (k in 2:length(tl)) {
      if(t[j]>=tl[k-1] & t[j]<=tl[k]){
        x[j+1] = x[1] + (k-1);
      }
    }
  }
  
  #return simulated poisson process
  return(x);
  
}

#example
arrivalTimes = seq(from=0, to=300, by=1);
poissonSim = poissonProcessSim(i = 2, l = 25, t = arrivalTimes, lambda = 0.3);

plot(poissonSim, type = 's', ylab = 'X(t)', xlab = 't', main = 'Simulated Poisson Process')

