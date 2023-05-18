rm(list=ls());graphics.off();cat("\014")

source("core.R")
source("types_of_distributions.R")


# - experiment's parameters -------------------------------------------------------

seed = 22 # "seed", it also denotes the identifier of the independent run.

Ms = c(1000,500,200)  # max quantity of (different) news papers to buy (1st phase decision)
cs = c(1.0,1.0,1.2)   # unitary costs
ss = c(1.2,1.1,1.3)   # unitary sell prices (i.e., s>c)
r = 0.5   # unitary 'recourse' sell price (i.e., r<c)
stopifnot( all(ss>cs) & all(r<cs) )

N.val = 1000 # number of out-of-sample scenarios
N0 = 5      # min number of generated scenarios
N.max = 50  # max number of generated scenarios


# Gaussian-mixture's parameters for 'actual' demand
alpha1 = 0.5; mu1.1 = 50; sd1.1 = 10; mu2.1 = 200; sd2.1 = 10
alpha2 = 0.5; mu1.2 = 25; sd1.2 = 5; mu2.2 = 100; sd2.2 = 5
alpha3 = 0.5; mu1.3 = 15; sd1.3 = 5; mu2.3 = 50; sd2.3 = 5

# BO's hyper-parameters
BO.init = 5   # initialization for each BO task
BO.iter = 25  # sequential iteration of BO (excluding BO.init)
ker = "exp"   # Gaussian Process's kernel function (i.e., "exp" is recommended)
beta = 3      # uncertainty multiplier in the GP-UCB acquisition function (i.e., 3 is suggested)


ucb <- function(x,gp,beta=1) {
  pred = predict(gp,data.frame(t(x)),"UK",checkNames=F)
  ucb = pred$mean + beta*pred$sd
}



# - Initialization ----------------------------------------------------------------

# generating an Out-Of-Sample (OOS) scenarios set (the same for all the independent runs)
set.seed(42)
d1 = demand.from.GM_k2(N.val,mu1.1,sd1.1,mu2.1,sd2.1,alpha1)
d2 = demand.from.GM_k2(N.val,mu1.2,sd1.2,mu2.2,sd2.2,alpha2)
d3 = demand.from.GM_k2(N.val,mu1.3,sd1.3,mu2.3,sd2.3,alpha3)
OOS = cbind(d1,d2,d3)

# setting the seed (i.e., the run.id)
set.seed(seed)

# probability density function estimated from the OOS
pdf1 = density(d1,from=0)
pdf2 = density(d2,from=0)
pdf3 = density(d3,from=0)


#******************************************************************
#* Patched!
#******************************************************************
# # initial scenario set for BO, sampled uniformly at random
# # (slightly different in this case, just to avoid endless loop)
# S.BO = cbind( round(runif(100*N0,0,Ms[1])),
#               round(runif(100*N0,0,Ms[2])), 
#               round(runif(100*N0,0,Ms[3])) ) 
# S.BO = unique(S.BO)
# S.BO = S.BO[1:N0,]
#******************************************************************

# initial scenario set for BO: sampling few scenarios from OOS-based PDF
d1 = round(sample(pdf1$x,N0,replace=T,prob=pdf1$y))
d2 = round(sample(pdf2$x,N0,replace=T,prob=pdf2$y))
d3 = round(sample(pdf3$x,N0,replace=T,prob=pdf3$y))
S.BO = cbind(d1,d2,d3)
S.BO[which(S.BO<0)]=0


retry = T
while(retry) {
  cat("(Preliminaries): Initializing the design for BO...\n")
  # init design...
  S.init = cbind( round(runif(BO.init,0,Ms[1])),
                  round(runif(BO.init,0,Ms[2])),
                  round(runif(BO.init,0,Ms[3]))  )
  oos.saa.init = numeric(BO.init)
  for( i in 1:nrow(S.init) ) {
    S.tmp = rbind(S.BO,S.init[i,])
    res = solvingNewsVendorMulti(scenarios=S.tmp,probs=NULL,Ms=Ms,ss=ss,r=r,cs=cs)
    x = res$solution[1:3]
    oos.saa.init[i] = SAA.multi(x=x,scenarios=OOS,Ms=Ms,ss=ss,r=r,cs=cs)
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
  d1 = demand.from.GM_k2(n,mu1.1,sd1.1,mu2.1,sd2.1,alpha1)
  d2 = demand.from.GM_k2(n,mu1.2,sd1.2,mu2.2,sd2.2,alpha2)
  d3 = demand.from.GM_k2(n,mu1.3,sd1.3,mu2.3,sd2.3,alpha3)
  S = cbind(d1,d2,d3)
  
  # solving w.r.t SAA
  res = solvingNewsVendorMulti(scenarios=S,probs=NULL,Ms=Ms,ss=ss,r=r,cs=cs)
  x = res$solution[1:3]
  oos.SAA = SAA.multi(x=x,scenarios=OOS,Ms=Ms,ss=ss,r=r,cs=cs)
  
  df0 = rbind( df0, data.frame( s.size = n,
                                scenarios = toString(S),
                                x = toString(x),
                                SAA = res$objval,
                                oos.SAA = oos.SAA,
                                stringsAsFactors=F ))
  # ----

  
  # Sampling from approximated ----
  cat("> Generating scenarios by sampling from approximated distribution...\n")
  # Sampling from approximated
  d1 = round(sample(pdf1$x,n,replace=T,prob=pdf1$y))
  d2 = round(sample(pdf2$x,n,replace=T,prob=pdf2$y))
  d3 = round(sample(pdf3$x,n,replace=T,prob=pdf3$y))
  S = cbind(d1,d2,d3)
  S[which(S<0)]=0
  # solving w.r.t SAA
  res = solvingNewsVendorMulti(scenarios=S,probs=NULL,Ms=Ms,ss=ss,r=r,cs=cs)
  x = res$solution[1:3]
  oos.SAA = SAA.multi(x=x,scenarios=OOS,Ms=Ms,ss=ss,r=r,cs=cs)
  
  df_ = rbind( df_, data.frame( s.size = n,
                                scenarios = toString(S),
                                x = toString(x),
                                SAA = res$objval,
                                oos.SAA = oos.SAA,
                                stringsAsFactors=F ))
  # ----
  
  
  # Sampling uniformly at random ----
  cat("> Generating scenarios by sampling uniformly at random in [0,M]...\n")
  # Sampling uniformly at random
  d1 = round(runif(n,0,Ms[1]))
  d2 = round(runif(n,0,Ms[2]))
  d3 = round(runif(n,0,Ms[3]))
  S = cbind(d1,d2,d3)
  # solving w.r.t SAA
  res = solvingNewsVendorMulti(scenarios=S,probs=NULL,Ms=Ms,ss=ss,r=r,cs=cs)
  x = res$solution[1:3]
  oos.SAA = SAA.multi(x=x,scenarios=OOS,Ms=Ms,ss=ss,r=r,cs=cs)
  
  df.r = rbind( df.r, data.frame( s.size = n,
                                  scenarios = toString(S),
                                  x = toString(x),
                                  SAA = res$objval,
                                  oos.SAA = oos.SAA,
                                  stringsAsFactors=F ))
  # ----
  
  
  # Active Sampling via BO ----
  cat("> Generating scenarios through BO-based active sampling...\n")
  if( n>N0 ) {
    
    while( nrow(S.BO)<n ) {
      
      cat("--> Searching for the most promising scenario to be added!\n")
      # sequential optimization (iteratively add the most promising scenario)
      S = S.init
      oos.saa.BO = oos.saa.init
      while( nrow(S)<BO.init+BO.iter ) {
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
        
        
        # optimizing UCB
        pars = cbind( round(runif(100,0,Ms[1])),
                      round(runif(100,0,Ms[2])),
                      round(runif(100,0,Ms[3])) )
        yyy = NULL
        for( ii in 1:nrow(pars) ) {
          tmp = ucb(x=pars[ii,],gp=gp,beta=1)
          yyy = c(yyy,tmp)
        }
        pars = pars[order(yyy,decreasing=T),]
        pars = pars[1:5,]
        
        best.par = NA; best.yyy = -Inf
        for( ii in 1:nrow(pars) ) {
          opt = optim(par=pars[ii,],fn=ucb,gr=NULL,method="L-BFGS-B",lower=0,upper=Ms,
                      control=list(trace=0,fnscale=-1),gp=gp,beta=1)
          if( opt$value>best.yyy ) {
            best.par = round(opt$par)
            best.yyy = opt$value
          }
        }
        
        S.tmp = rbind(S.BO,best.par)
        res = solvingNewsVendorMulti(scenarios=S.tmp,probs=NULL,Ms=Ms,ss=ss,r=r,cs=cs)
        x = res$solution[1:3]
        oos.saa.BO = c(oos.saa.BO,SAA.multi(x=x,scenarios=OOS,Ms=Ms,ss=ss,r=r,cs=cs))
        S = rbind(S,best.par)
      }
      
      # update the Scenario set for BO
      S.BO = rbind(S.BO,S[which.max(oos.saa.BO),])
      cat("added!\n")
      
    }
  }
  
  # evaluate SAA on OOS if each candidate is added to the current Scenario set
  # solving w.r.t SAA
  res = solvingNewsVendorMulti(scenarios=S.BO,probs=NULL,Ms=Ms,ss=ss,r=r,cs=cs)
  x = res$solution[1:3]
  oos.SAA = SAA.multi(x=x,scenarios=OOS,Ms=Ms,ss=ss,r=r,cs=cs)
  df.bo = rbind( df.bo, data.frame( s.size = n,
                                    scenarios = toString(S.BO),
                                    x = toString(x),
                                    SAA = res$objval,
                                    oos.SAA = oos.SAA,
                                    stringsAsFactors=F ))
  # ----
  
  cat("\n")
  
}

# Saving results...
if( !dir.exists("experiments_multi") )
  dir.create("experiments_multi")
if( !dir.exists(paste0("experiments_multi/Gaussian_Mixture_OOS_",N.val,"_Size_",N.max)) )
    dir.create(paste0("experiments_multi/Gaussian_Mixture_OOS_",N.val,"_Size_",N.max))
saveRDS( list(Ms=Ms,cs=cs,ss=ss,r=r,N.val=N.val,N.max=N.max,N0=N0,
              alpha1=alpha1,alpha2=alpha2,alpha3=alpha3,
              mu1.1=mu1.1,sd1.1=sd1.1,mu2.1=mu2.1,sd2.1=sd2.1,
              mu1.2=mu1.2,sd1.2=sd1.2,mu2.2=mu2.2,sd2.2=sd2.2,
              mu1.3=mu1.3,sd1.3=sd1.3,mu2.3=mu2.3,sd2.3=sd2.3,
              OOS=OOS,S.BO=S.BO,BO.init=BO.init,BO.iter=BO.iter,ker=ker,beta=beta,
              df0=df0,df_=df_,df.r=df.r,df.bo=df.bo),
         paste0("experiments_multi/Gaussian_Mixture_OOS_",N.val,"_Size_",N.max,"/GM_OOS_",N.val,"_Size_",N.max,"_Seed_",seed,".RDS") )
