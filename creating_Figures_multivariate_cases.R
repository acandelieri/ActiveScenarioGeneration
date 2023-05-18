rm(list=ls()); graphics.off(); cat("\014")
source("core.R")
source("types_of_distributions.R")


#**********************************************
# uncomment the block associated to the
# univariate case you want to plot
#**********************************************

# OOS
# files = list.files("experiments_multi//Gaussian_OOS_1000_Size_50",full.names=T)
# xmin=55000; xmax=60000
# bw=100

# files = list.files("experiments_multi/Gaussian_Mixture_OOS_1000_Size_50",full.names=T)
# xmin=6000; xmax=12000
# bw=100

# files = list.files("experiments_multi/TailOn_both_OOS_1000_Size_50",full.names=T)
# xmin=40000; xmax=45000
# bw=100

# files = list.files("experiments_multi/TailOn_left_OOS_1000_Size_50",full.names=T)
# xmin=25000; xmax=32000
# bw=100

files = list.files("experiments_multi/TailOn_right_OOS_1000_Size_50",full.names=T)
xmin=51000; xmax=61000
bw=100



saa0 = saa_ = saa.r =saa.bo = NULL

for( f in files ) {
  res = readRDS(f)
  # saa0 = rbind(saa0,res$df0$oos.SAA)
  # saa_ = rbind(saa_,res$df_$oos.SAA)
  # saa.r = rbind(saa.r,res$df.r$oos.SAA)
  saa.bo = rbind(saa.bo,res$df.bo$oos.SAA)
}

N0=res$N0; N.max=res$N.max


first.best.ixs = apply(saa.bo,1,which.max)



n.breaks = 10


df = data.frame( file=character(),
                 group=character(),
                 values=numeric(),
                 stringsAsFactors=F )


for( f in files ) {

  cat("> Reading file",f,"...")
  res = readRDS(f)
  
  # size 5
  S0 = matrix(as.numeric(unlist(strsplit(res$df0$scenarios[1],", "))),ncol=3,byrow=F)
  x = solvingNewsVendorMulti(scenarios=S0,probs=NULL,Ms=res$Ms,ss=res$ss,r=res$r,cs=res$cs)
  x = x$solution[1:3]
  oos.saa0 = obj.on.scenarios.multi(x=x,scenarios=res$OOS,Ms=res$Ms,ss=res$ss,r=res$r,cs=res$cs)
  

  df = rbind( df, data.frame( file=f,
                              group="actual_05",
                              values=sum(oos.saa0),
                              stringsAsFactors=F ) )
  
  # size 25
  S0 = matrix(as.numeric(unlist(strsplit(res$df0$scenarios[21],", "))),ncol=3,byrow=F)
  x = solvingNewsVendorMulti(scenarios=S0,probs=NULL,Ms=res$Ms,ss=res$ss,r=res$r,cs=res$cs)
  x = x$solution[1:3]
  oos.saa0 = obj.on.scenarios.multi(x=x,scenarios=res$OOS,Ms=res$Ms,ss=res$ss,r=res$r,cs=res$cs)
  
  
  df = rbind( df, data.frame( file=f,
                              group="actual_25",
                              values=sum(oos.saa0),
                              stringsAsFactors=F ) )
  
  # size 50
  S0 = matrix(as.numeric(unlist(strsplit(res$df0$scenarios[46],", "))),ncol=3,byrow=F)
  x = solvingNewsVendorMulti(scenarios=S0,probs=NULL,Ms=res$Ms,ss=res$ss,r=res$r,cs=res$cs)
  x = x$solution[1:3]
  oos.saa0 = obj.on.scenarios.multi(x=x,scenarios=res$OOS,Ms=res$Ms,ss=res$ss,r=res$r,cs=res$cs)
  
  
  df = rbind( df, data.frame( file=f,
                              group="actual_50",
                              values=sum(oos.saa0),
                              stringsAsFactors=F ) )
  
  
  
  # size 5
  S_ = matrix(as.numeric(unlist(strsplit(res$df_$scenarios[1],", "))),ncol=3,byrow=F)
  x = solvingNewsVendorMulti(scenarios=S_,probs=NULL,Ms=res$Ms,ss=res$ss,r=res$r,cs=res$cs)
  x = x$solution[1:3]
  oos.saa_ = obj.on.scenarios.multi(x=x,scenarios=res$OOS,Ms=res$Ms,ss=res$ss,r=res$r,cs=res$cs)
  
  df = rbind( df, data.frame( file=f,
                              group="OOS_05",
                              values=sum(oos.saa_),
                              stringsAsFactors=F ) )
  
  # size 25
  S_ = matrix(as.numeric(unlist(strsplit(res$df_$scenarios[21],", "))),ncol=3,byrow=F)
  x = solvingNewsVendorMulti(scenarios=S_,probs=NULL,Ms=res$Ms,ss=res$ss,r=res$r,cs=res$cs)
  x = x$solution[1:3]
  oos.saa_ = obj.on.scenarios.multi(x=x,scenarios=res$OOS,Ms=res$Ms,ss=res$ss,r=res$r,cs=res$cs)
  
  df = rbind( df, data.frame( file=f,
                              group="OOS_25",
                              values=sum(oos.saa_),
                              stringsAsFactors=F ) )

  
  # size 50
  S_ = matrix(as.numeric(unlist(strsplit(res$df_$scenarios[46],", "))),ncol=3,byrow=F)
  x = solvingNewsVendorMulti(scenarios=S_,probs=NULL,Ms=res$Ms,ss=res$ss,r=res$r,cs=res$cs)
  x = x$solution[1:3]
  oos.saa_ = obj.on.scenarios.multi(x=x,scenarios=res$OOS,Ms=res$Ms,ss=res$ss,r=res$r,cs=res$cs)
  
  
  df = rbind( df, data.frame( file=f,
                              group="OOS_50",
                              values=sum(oos.saa_),
                              stringsAsFactors=F ) )

  
  
  # first.max.ix = which.max(res$df.bo$oos.SAA)
  first.max.ix = first.best.ixs[which(files==f)]

  S.bo = matrix(as.numeric(unlist(strsplit(res$df.bo$scenarios[first.max.ix],", "))),ncol=3,byrow=F)
  x = solvingNewsVendorMulti(scenarios=S.bo,probs=NULL,Ms=res$Ms,ss=res$ss,r=res$r,cs=res$cs)
  x = x$solution[1:3]
  oos.saa.bo = obj.on.scenarios.multi(x=x,scenarios=res$OOS,Ms=res$Ms,ss=res$ss,r=res$r,cs=res$cs)
  
  
  df = rbind( df, data.frame( file=f,
                              group="proposed",
                              values=sum(oos.saa.bo),
                              stringsAsFactors=F ) )
  
  
  cat("done!\n")
  
}


par(mar=c(3.1,4.6,1.1,1.1))
boxplot( values~group, df, col=c(rep("pink3",3),rep("skyblue3",3),"green3"),
         # at=c(1:3,5:7,9),
         ylab="Sum of profits on OOS scenarios [€]", xlab=NA,
         names=c("size=5","size=25","size=50",
                 "size=5","size=25","size=50",
                 "optimal size"), 
         cex.axis=1.3, cex.lab=1.3 )
legend("bottomright", col=c("pink3","skyblue3","green3"), pch=15, cex=1.3,
       legend=c("Sampling from actual","Sampling from OOS-based pdf","Active Learning"))

# plot(density(df$values[which(df$group=="actual_50")],bw=bw,from=xmin,to=xmax),
#      col="blue",lwd=4, ylim=c(0,0.003))
# lines(density(df$values[which(df$group=="OOS_50")],bw=bw,from=xmin,to=xmax),
#       col="skyblue",lwd=4)
# lines(density(df$values[which(df$group=="proposed")],bw=bw,from=xmin,to=xmax),
#       col="green3",lwd=4)


d1 = density(df$values[which(df$group=="actual_50")],bw=bw,from=xmin,to=xmax)
d2 = density(df$values[which(df$group=="OOS_50")],bw=bw,from=xmin,to=xmax)
d3 = density(df$values[which(df$group=="proposed")],bw=bw,from=xmin,to=xmax)

par(mar=c(4.1,4.6,1.1,1.1))

plot( d1$x, d1$y, type="l", cex.lab=1.3, cex.axis=1.3,
      xlab="Sum of profits on OOS scenarios [€]", ylab="Density",
      ylim=c(0,0.003))
polygon( c(d1$x,rev(d1$x)), c(d1$y,numeric(length(d1$x))),
         col=adjustcolor("pink3",alpha.f=0.3), border="pink3", lwd=3 )
polygon( c(d2$x,rev(d2$x)), c(d2$y,numeric(length(d2$x))),
         col=adjustcolor("skyblue3",alpha.f=0.3), border="skyblue3", lwd=3 )
polygon( c(d3$x,rev(d3$x)), c(d3$y,numeric(length(d3$x))),
         col=adjustcolor("green3",alpha.f=0.2), border="green3", lwd=3 )

legend("top", col=c("pink3","skyblue3","green3"), pch=15, cex=1.3,
       legend=c("Sampling from actual","Sampling from OOS-based pdf","Active Learning"))

