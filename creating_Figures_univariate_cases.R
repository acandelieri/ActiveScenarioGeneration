rm(list=ls()); graphics.off(); cat("\014")
source("core.R")
source("types_of_distributions.R")


#**********************************************
# uncomment the block associated to the
# multivariate case you want to plot
#**********************************************


# OOS
# files = list.files("experiments/Gaussian_OOS_1000_Size_50",full.names=T)
# xmin=44500; xmax=48500
# bw=100

# files = list.files("experiments/Gaussian_Mixture_OOS_1000_Size_50",full.names=T)
# xmin=5000; xmax=9000
# bw=100

# files = list.files("experiments/TailOn_both_OOS_1000_Size_50",full.names=T)
# xmin=70000; xmax=75000
# bw=100

# files = list.files("experiments/TailOn_left_OOS_1000_Size_50",full.names=T)
# xmin=52000; xmax=62000
# bw=100

files = list.files("experiments/TailOn_right_OOS_1000_Size_50",full.names=T)
xmin=85000; xmax=90000
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
  S0 = as.numeric(unlist(strsplit(res$df0$scenarios[1],", ")))
  x = solvingNewsVendor(scenarios=S0,probs=NULL,M=res$M,s=res$s,r=res$r,c=res$c)
  x = x$solution[1]
  oos.saa0 = obj.on.scenarios(x=x,scenarios=res$OOS,M=res$M,s=res$s,r=res$r,c=res$c)
  
  df = rbind( df, data.frame( file=f,
                              group="actual_05",
                              values=sum(oos.saa0),
                              stringsAsFactors=F ) )
  
  # size 25
  S0 = as.numeric(unlist(strsplit(res$df0$scenarios[21],", ")))
  x = solvingNewsVendor(scenarios=S0,probs=NULL,M=res$M,s=res$s,r=res$r,c=res$c)
  x = x$solution[1]
  oos.saa0 = obj.on.scenarios(x=x,scenarios=res$OOS,M=res$M,s=res$s,r=res$r,c=res$c)
  
  df = rbind( df, data.frame( file=f,
                              group="actual_25",
                              values=sum(oos.saa0),
                              stringsAsFactors=F ) )
  
  # size 50
  S0 = as.numeric(unlist(strsplit(res$df0$scenarios[46],", ")))
  x = solvingNewsVendor(scenarios=S0,probs=NULL,M=res$M,s=res$s,r=res$r,c=res$c)
  x = x$solution[1]
  oos.saa0 = obj.on.scenarios(x=x,scenarios=res$OOS,M=res$M,s=res$s,r=res$r,c=res$c)
  
  df = rbind( df, data.frame( file=f,
                              group="actual_50",
                              values=sum(oos.saa0),
                              stringsAsFactors=F ) )
  
  
  
  # size 5
  S_ = as.numeric(unlist(strsplit(res$df_$scenarios[1],", ")))
  x = solvingNewsVendor(scenarios=S_,probs=NULL,M=res$M,s=res$s,r=res$r,c=res$c)
  x = x$solution[1]
  oos.saa_ = obj.on.scenarios(x=x,scenarios=res$OOS,M=res$M,s=res$s,r=res$r,c=res$c)

  df = rbind( df, data.frame( file=f,
                              group="OOS_05",
                              values=sum(oos.saa_),
                              stringsAsFactors=F ) )
  
  # size 25
  S_ = as.numeric(unlist(strsplit(res$df_$scenarios[21],", ")))
  x = solvingNewsVendor(scenarios=S_,probs=NULL,M=res$M,s=res$s,r=res$r,c=res$c)
  x = x$solution[1]
  oos.saa_ = obj.on.scenarios(x=x,scenarios=res$OOS,M=res$M,s=res$s,r=res$r,c=res$c)
  
  df = rbind( df, data.frame( file=f,
                              group="OOS_25",
                              values=sum(oos.saa_),
                              stringsAsFactors=F ) )

  
  # size 50
  S_ = as.numeric(unlist(strsplit(res$df_$scenarios[46],", ")))
  x = solvingNewsVendor(scenarios=S_,probs=NULL,M=res$M,s=res$s,r=res$r,c=res$c)
  x = x$solution[1]
  oos.saa_ = obj.on.scenarios(x=x,scenarios=res$OOS,M=res$M,s=res$s,r=res$r,c=res$c)
  
  df = rbind( df, data.frame( file=f,
                              group="OOS_50",
                              values=sum(oos.saa_),
                              stringsAsFactors=F ) )

  
  
  # # size = 5
  # S.r = as.numeric(unlist(strsplit(res$df.r$scenarios[1],", ")))
  # x = solvingNewsVendor(scenarios=S.r,probs=NULL,M=res$M,s=res$s,r=res$r,c=res$c)
  # x = x$solution[1]
  # oos.saa.r = obj.on.scenarios(x=x,scenarios=res$OOS,M=res$M,s=res$s,r=res$r,c=res$c)
  # 
  # df = rbind( df, data.frame( file=f,
  #                             group="Unif. Rand. 05",
  #                             values=sum(oos.saa.r),
  #                             stringsAsFactors=F ) )
  # 
  # # size = 25
  # S.r = as.numeric(unlist(strsplit(res$df.r$scenarios[21],", ")))
  # x = solvingNewsVendor(scenarios=S.r,probs=NULL,M=res$M,s=res$s,r=res$r,c=res$c)
  # x = x$solution[1]
  # oos.saa.r = obj.on.scenarios(x=x,scenarios=res$OOS,M=res$M,s=res$s,r=res$r,c=res$c)
  # 
  # df = rbind( df, data.frame( file=f,
  #                             group="Unif. Rand. 25",
  #                             values=sum(oos.saa.r),
  #                             stringsAsFactors=F ) )
  # 
  # # size = 50
  # S.r = as.numeric(unlist(strsplit(res$df.r$scenarios[46],", ")))
  # x = solvingNewsVendor(scenarios=S.r,probs=NULL,M=res$M,s=res$s,r=res$r,c=res$c)
  # x = x$solution[1]
  # oos.saa.r = obj.on.scenarios(x=x,scenarios=res$OOS,M=res$M,s=res$s,r=res$r,c=res$c)
  # 
  # df = rbind( df, data.frame( file=f,
  #                             group="Unif. Rand. 50",
  #                             values=sum(oos.saa.r),
  #                             stringsAsFactors=F ) )
  
  # first.max.ix = which.max(res$df.bo$oos.SAA)
  first.max.ix = first.best.ixs[which(files==f)]

  S.bo = as.numeric(unlist(strsplit(res$df.bo$scenarios[first.max.ix],", ")))
  x = solvingNewsVendor(scenarios=S.bo,probs=NULL,M=res$M,s=res$s,r=res$r,c=res$c)
  x = x$solution[1]
  oos.saa.bo = obj.on.scenarios(x=x,scenarios=res$OOS,M=res$M,s=res$s,r=res$r,c=res$c)

  
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
legend("right", col=c("pink3","skyblue3","green3"), pch=15, cex=1.3,
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
      ylim=c(0,0.0035))
polygon( c(d1$x,rev(d1$x)), c(d1$y,numeric(length(d1$x))),
         col=adjustcolor("pink3",alpha.f=0.3), border="pink3", lwd=3 )
polygon( c(d2$x,rev(d2$x)), c(d2$y,numeric(length(d2$x))),
         col=adjustcolor("skyblue3",alpha.f=0.3), border="skyblue3", lwd=3 )
polygon( c(d3$x,rev(d3$x)), c(d3$y,numeric(length(d3$x))),
         col=adjustcolor("green3",alpha.f=0.2), border="green3", lwd=3 )

legend("top", col=c("pink3","skyblue3","green3"), pch=15, cex=1.3,
       legend=c("Sampling from actual","Sampling from OOS-based pdf","Active Learning"))
