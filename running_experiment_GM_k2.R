rm(list=ls());graphics.off();cat("\014")

source("core.R")
source("types_of_distributions.R")


# - experiment's parameters -------------------------------------------------------

seed = 30 # "seed", it also denotes the identifier of the independent run.

M = 1000  # max quantity of news paper to buy (1st phase decision)
c = 1.0   # unitary cost
s = 1.2   # unitary sell price (i.e., s>c)
r = 0.5   # unitary 'recourse' sell price (i.e., r<c)
stopifnot( s>c & r<c )

N.val = 1000 # number of out-of-sample scenarios
N0 = 5      # min number of generated scenarios
N.max = 50  # max number of generated scenarios


# Gaussian-mixture's parameters for 'actual' demand
alpha = 0.5; mu1 = 50; sd1 = 10; mu2 = 200; sd2 = 10  

# BO's hyper-parameters
BO.init = 5   # initialization for each BO task
BO.iter = 25  # sequential iteration of BO (excluding BO.init)
ker = "exp"   # Gaussian Process's kernel function (i.e., "exp" is recommended)
beta = 3      # uncertainty multiplier in the GP-UCB acquisition function (i.e., 3 is suggested)




# - Initialization ----------------------------------------------------------------

# generating an Out-Of-Sample (OOS) scenarios set (the same for all the independent runs)
set.seed(42)
OOS = demand.from.GM_k2(N.val,mu1,sd1,mu2,sd2,alpha)

# setting the seed (i.e., the run.id)
set.seed(seed)

# probability density function estimated from the OOS
pdf = density(OOS,from=0)


#******************************************************************
#* Patched!
#******************************************************************
# # initial scenario set for BO, sampled uniformly at random
# S.BO = round(runif(N0,0,M))
#******************************************************************

# with seed==10 ans seed==18 the following while-loop becomes infinite...
if( seed!=10 & seed!=18 ) {
  S.BO = round(sample(pdf$x,N0,replace=T,prob=pdf$y))
} else {
  S.BO = round(sample(pdf$x,N0*100,replace=T,prob=pdf$y))
  S.BO = unique(S.BO)[1:N0]
}
S.BO[which(S.BO<0)] = 0

retry = T
while(retry) {
  cat("(Preliminaries): Initializing the design for BO...\n")
  # init design...
  S.init = round(runif(BO.init,0,M))
  oos.saa.init = numeric(BO.init)
  for( i in 1:length(S.init) ) {
    S.tmp = c(S.BO,S.init[i])
    res = solvingNewsVendor(scenarios=S.tmp,probs=NULL,M=M,s=s,r=r,c=c)
    x = res$solution[1]
    oos.saa.init[i] = SAA(x=x,scenarios=OOS,M=M,s=s,r=r,c=c)
  }
  retry = (is.na(sd(oos.saa.init)) | sd(oos.saa.init)<10^-8)
}




# - MAIN --------------------------------------------------------------------------

# df0 - data frame storing results for "sampling from actual distribution" (i.e., benchmark)
# df_ - data frame storing results for "sampling from approximated distribution"
# df.r - data frame storing results for "sampling uniformly at random"
# df.bo - data frame storing results for "BO-based scenario generation"
df0 = data.frame( s.size = numeric(),
                  scenarios = character(),
                  x = numeric(),
                  SAA = numeric(),
                  oos.SAA = numeric(),
                  stringsAsFactors=F )

df_ = df.r = df.bo = df0


# Repeat for different sizes of the generated scenario set
for( n in N0:N.max ) {
  
  cat("[ Scenario set with size",n,"]\n\n")
  
  
  # Sampling from actual ----
  cat("> Generating scenarios by sampling from actual distribution (i.e., benchmark)...\n")
  S = demand.from.GM_k2(n,mu1,sd1,mu2,sd2,alpha)
  # solving w.r.t SAA
  res = solvingNewsVendor(scenarios=S,probs=NULL,M=M,s=s,r=r,c=c)
  x = res$solution[1]
  oos.SAA = SAA(x=x,scenarios=OOS,M=M,s=s,r=r,c=c)
  
  df0 = rbind( df0, data.frame( s.size = n,
                                scenarios = toString(S),
                                x = x,
                                SAA = res$objval,
                                oos.SAA = oos.SAA,
                                stringsAsFactors=F ))
  # ----

  
  # Sampling from approximated ----
  cat("> Generating scenarios by sampling from approximated distribution...\n")
  # Sampling from approximated
  S = round(sample(pdf$x,n,replace=T,prob=pdf$y))
  S[which(S<0)]=0
  # solving w.r.t SAA
  res = solvingNewsVendor(scenarios=S,probs=NULL,M=M,s=s,r=r,c=c)
  x = res$solution[1]
  oos.SAA = SAA(x=x,scenarios=OOS,M=M,s=s,r=r,c=c)
  
  df_ = rbind( df_, data.frame( s.size = n,
                                scenarios = toString(S),
                                x = x,
                                SAA = res$objval,
                                oos.SAA = oos.SAA,
                                stringsAsFactors=F ))
  # ----
  
  
  # Sampling uniformly at random ----
  cat("> Generating scenarios by sampling uniformly at random in [0,M]...\n")
  # Sampling uniformly at random
  S = round(runif(n,0,M))
  # solving w.r.t SAA
  res = solvingNewsVendor(scenarios=S,probs=NULL,M=M,s=s,r=r,c=c)
  x = res$solution[1]
  oos.SAA = SAA(x=x,scenarios=OOS,M=M,s=s,r=r,c=c)
  
  df.r = rbind( df.r, data.frame( s.size = n,
                                  scenarios = toString(S),
                                  x = x,
                                  SAA = res$objval,
                                  oos.SAA = oos.SAA,
                                  stringsAsFactors=F ))
  # ----
  
  
  # Active Sampling via BO ----
  cat("> Generating scenarios through BO-based active sampling...\n")
  if( n>N0 ) {
    
    while( length(S.BO)<n ) {
      
      cat("--> Searching for the most promising scenario to be added!\n")
      # sequential optimization (iteratively add the most promising scenario)
      S = S.init
      oos.saa.BO = oos.saa.init
      while( length(S)<BO.init+BO.iter ) {
        cat(".")
        gp = NULL
        while( is.null(gp) ) {
          try( expr=(gp = km( design=data.frame(csi=S),response=oos.saa.BO,
                              covtype=ker, nugget.estim=T,
                              control=list(trace=0) )),
               silent = T )
          if(is.null(gp))
            cat("[?]")
        }
        # easy UCB...
        ss = 0:M
        zz = predict( gp, data.frame(csi=ss), "UK" )
        ix = which.max( zz$mean+beta*zz$sd )
        s.new = ss[ix]
  
        S.tmp = c(S.BO,s.new)
        res = solvingNewsVendor(scenarios=S.tmp,probs=NULL,M=M,s=s,r=r,c=c)
        x = res$solution[1]
        oos.saa.BO = c(oos.saa.BO,SAA(x=x,scenarios=OOS,M=M,s=s,r=r,c=c))
        S = c(S,s.new)
      }
      
      # update the Scenario set for BO
      S.BO = c(S.BO,S[which.max(oos.saa.BO)])
      cat("added!\n")
      
    }
  }
  
  # evaluate SAA on OOS if each candidate is added to the current Scenario set
  # solving w.r.t SAA
  res = solvingNewsVendor(scenarios=S.BO,probs=NULL,M=M,s=s,r=r,c=c)
  x = res$solution[1]
  oos.SAA = SAA(x=x,scenarios=OOS,M=M,s=s,r=r,c=c)
  df.bo = rbind( df.bo, data.frame( s.size = n,
                                    scenarios = toString(S.BO),
                                    x = x,
                                    SAA = res$objval,
                                    oos.SAA = oos.SAA,
                                    stringsAsFactors=F ))
  # ----
  
  cat("\n")
  
}


# Saving results...
if( !dir.exists("experiments") )
  dir.create("experiments")
if( !dir.exists(paste0("experiments/Gaussian_Mixture_OOS_",N.val,"_Size_",N.max)) )
    dir.create(paste0("experiments/Gaussian_Mixture_OOS_",N.val,"_Size_",N.max))
saveRDS( list(M=M,c=c,s=s,r=r,N.val=N.val,N.max=N.max,N0=N0,
              alpha=alpha,mu1=mu1,sd1=sd1,mu2=mu2,sd2=sd2,OOS=OOS,
              S.BO=S.BO,BO.init=BO.init,BO.iter=BO.iter,ker=ker,beta=beta,
              df0=df0,df_=df_,df.r=df.r,df.bo=df.bo),
         paste0("experiments/Gaussian_Mixture_OOS_",N.val,"_Size_",N.max,"/GM_OOS_",N.val,"_Size_",N.max,"_Seed_",seed,".RDS") )
