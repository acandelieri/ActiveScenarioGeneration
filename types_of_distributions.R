demand.from.Gaussian <- function(N,mu,sd) {
  demand = round(rnorm(N,mu,sd))
  demand[which(demand<0)] = 0
  demand.from.Gaussian = demand
}

demand.from.GM_k2 <- function(N,mu1,sd1,mu2,sd2,alpha=0.5) {
  U = runif(N)
  I = as.numeric(U<alpha)
  demand = round(I * rnorm(N,mu1,sd1) + (1-I)*rnorm(N,mu2,sd2))
  demand[which(demand<0)] = 0
  demand.from.GM_k2 = demand
}

demand.from.tail.distr <- function(N,mu,sd,alpha=0.3,tail.on="both") {
  stopifnot( tail.on %in% c("left","right","both") )
  U = runif(N)
  I = as.numeric(U<alpha)
  demand = round( (1-I) * rnorm(N,mu,sd) )
  
  n = length(which(I==1))
  if( tail.on=="both" ) {
    nl = round(n/2)
    nr = n-nl
  } else {
    if( tail.on=="right" ){
      nr = n
      nl = 0
    } else {
      nr = 0
      nl = n
    }
  }
  
  left.tail = rnorm( nl,mu-6*sd,3*sd )
  left.tail[which(left.tail<0)] = 0
  right.tail = rnorm( nr,mu+6*sd,3*sd )

  demand[which(I==1)] = c(left.tail,right.tail)
  
  demand[which(demand<0)] = 0
  demand.from.tail.distr = demand
}
