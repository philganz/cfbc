library(reshape2)

#load and reassign data
load("results_gam.RData")
results_gam <- results 
load("results_main.RData")
results_main <- results 
load("results_sig.RData")
results_sig <- results 

#reshape arrays into long data frames
results_gam_df <- melt(results_gam)
results_main_df <- melt(results_main)
results_sig_df <- melt(results_sig)

#Plot pdfs of parameter estimates
par(mfrow=c(1,3))
hist(results[,"gamma"],breaks=20,col="grey",main="")
abline(v=mean(results[,"gamma"]),col=2,lty=2,lwd=2)
abline(v=gamma,lty=2,lwd=2)
hist(results[,"alpha"],breaks=20,col="grey",main="")
abline(v=mean(results[,"alpha"]),col=2,lty=2,lwd=2)
abline(v=alpha,lty=2,lwd=2)
hist(results[,"rho"],breaks=20,col="grey",main="")
abline(v=mean(results[,"rho"]),col=2,lty=2,lwd=2)
abline(v=rho,lty=2,lwd=2)
mtext("Red=mean(Estimated)  Black=True", side = 3, line = -2, cex=1.5, outer = TRUE)