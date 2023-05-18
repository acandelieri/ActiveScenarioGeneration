library(DiceKriging)
library(lpSolve)



# Sample Average Approximation value
SAA <- function( x, scenarios, M=100, s=1.2, r=0.8, c=1) {
  y = apply(cbind(rep(x,length(scenarios)),scenarios),1,min)
  SAA = -c*x + mean(s*y+r*(x-y))
}


# Sample Average Approximation value (multi-variate case)
SAA.multi <- function( x, scenarios, Ms=rep(100,ncol(scenarios)),
                       ss=rep(1.2,ncol(scenarios)), r=0.8, cs=rep(1,ncol(scenarios)) ) {
  
  X = matrix(rep(x,nrow(scenarios)),nrow(scenarios),byrow=T)
  Y = matrix(NA,nrow(X),ncol(X))
  for( i in 1:length(X) )
    Y[i] = min(X[i],scenarios[i])
  SAA = -sum(cs*x) + mean( apply(matrix(rep(ss,nrow(Y)),nrow(Y),byrow=T)*Y+r*(X-Y),1,sum) )
}


# solving the news vendor problem:
# 'probs' must be a vector providing the probability for each scenario
# if the Deterministic Equivalent is used
# otherwise (i.e., 'probs' == NULL) SAA is used as objective function
solvingNewsVendor <- function( scenarios, probs=NULL, M=100, s=1.2, r=0.8, c=1 ) {
  
  if(is.null(probs) )
    probs = rep(1/length(scenarios),length(scenarios))
  
  N = length(scenarios)
  
  A1 = cbind( numeric(N), diag(N) )     # y[i] >= 0
  A2 = cbind( numeric(N), diag(N) )     # y[i] <= csi[i]
  A3 = cbind( rep(1,N), -diag(N) )      # x >= y[i] 
  A4 = cbind( rep(1,N), matrix(0,N,N) ) # x >= 0
  A5 = cbind( rep(1,N), matrix(0,N,N) ) # x <= M
  A = rbind( A1, A2, A3, A4, A5 )
  
  # make free memory...
  rm(A1); rm(A2); rm(A3); rm(A4); rm(A5); gc()
  
  
  res = lp(direction="max",
           objective.in=c( sum(probs)*r-c, probs*rep(s-r,N) ),
           const.mat=A,
           const.dir=c( rep(">=",N), rep("<=",N), rep(">=",N), rep(">=",N), rep("<=",N) ),
           const.rhs=c( numeric(N), scenarios, numeric(N), numeric(N), rep(M,N) ),
           all.int=T,
           presolve=1 )
  
  solvingNewsVendor = res 
}



# solving the multi-dimensional news vendor problem:
# 'probs' must be a vector providing the probability for each scenario
# if the Deterministic Equivalent is used
# otherwise (i.e., 'probs' == NULL) SAA is used as objective function
solvingNewsVendorMulti <- function( scenarios, probs=NULL, Ms=rep(100,ncol(scenarios)),
                               ss=rep(1.2,ncol(scenarios)), r=0.8, cs=rep(1,ncol(scenarios)) ) {

  if( is.null(probs) )
    probs = rep(1/nrow(scenarios),nrow(scenarios))

  N = nrow(scenarios)
  d = ncol(scenarios)

  A1 = cbind( matrix(0,N*d,d), diag(N*d) )                                      # y[i] >= 0
  A2 = cbind( matrix(0,N*d,d), diag(N*d) )                                      # y[i] <= csi[i]
  A3 = cbind( matrix(matrix(rep(diag(d),N)),N*d,byrow=T), -diag(N*d) )          # x >= y[i]
  A4 = cbind( matrix(matrix(rep(diag(d),N)),N*d,byrow=T), matrix(0,N*d,N*d) )   # x >= 0
  A5 = cbind( matrix(matrix(rep(diag(d),N)),N*d,byrow=T), matrix(0,N*d,N*d) )   # x <= M
  A = rbind( A1, A2, A3, A4, A5 )

  # make free memory...
  rm(A1); rm(A2); rm(A3); rm(A4); rm(A5); gc()



  res = lp(direction="max",
           objective.in=c( sum(probs)*r-cs, probs*rep(ss-r,N) ),
           const.mat=A,
           const.dir=c( rep(">=",N*d), rep("<=",N*d), rep(">=",N*d), rep(">=",N*d), rep("<=",N*d) ),
           const.rhs=c( numeric(N*d), as.numeric(t(scenarios)), numeric(N*d), numeric(N*d), rep(Ms,N) ),
           all.int=T,
           presolve=1 )

  solvingNewsVendor = res
}


# Objective function on a set of scenarios, separately
obj.on.scenarios <- function( x, scenarios, M=100, s=1.2, r=0.8, c=1 ) {
  y = apply(cbind(rep(x,length(scenarios)),scenarios),1,min)
  SAA.vals = -c*x + (s*y+r*(x-y))
}

# Objective function on a set of scenarios, separately, for the multivariate case
obj.on.scenarios.multi <- function( x, scenarios, Ms=rep(100,ncol(scenarios)),
                                    ss=rep(1.2,ncol(scenarios)), r=0.8, cs=rep(1,ncol(scenarios)) ) {
  X = matrix(rep(x,nrow(scenarios)),nrow(scenarios),byrow=T)
  Y = matrix(NA,nrow(X),ncol(X))
  for( i in 1:length(X) )
    Y[i] = min(X[i],scenarios[i])
  SAA.vals = -sum(cs*x) +apply(matrix(rep(ss,nrow(Y)),nrow(Y),byrow=T)*Y+r*(X-Y),1,sum)
}