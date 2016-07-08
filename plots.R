#load required packages
library(ggplot2)
library(gridExtra)

#load and reassign data
load("results_gam.RData")
results_gam  <- results 

load("results_main.RData")
results_main <- results

load("results_sig.RData")
results_sig  <- results 

#remove last array
rm(results)

#stack results from separate cases on top of each other
results_gam  <- rbind(results_gam[,,"0.1",],results_gam[,,"0.3",],results_gam[,,"0.7",],results_gam[,,"0.9",])
results_main <- as.matrix(results_main[,,1,1])
results_sig  <- rbind(results_sig[,,,"0.1"],results_sig[,,,"0.3"],results_sig[,,,"0.7"],results_sig[,,,"0.9"])

#convert to data frames
results_gam  <- data.frame(results_gam)
results_main <- data.frame(results_main)
results_sig  <- data.frame(results_sig)

#create one data frame for all results
results <- rbind(results_gam,results_main,results_sig)

#add main results to gamma and sigma cases
results_gam <- rbind(results_gam,results_main)
results_sig <- rbind(results_sig,results_main)

#add residuals column for parameters of interest
results_gam$gamma_resids <- results_gam$gamma_est - results_gam$gamma_true
results_sig$sigma_eps_resids <- results_sig$sigma_eps_est - results_sig$sigma_eps_true

#plots

#save default margins
d.mar <- c(5.1,4.1,4.1,2.1)

#histograms of parameter estimates
#initialize plotting function
pdf("params_hists.pdf",7,5)
#create plots
par(mfrow=c(2,2),mar=d.mar-c(0,0,1,0))
hist(results_main$gamma_est,breaks=20,col="grey",main=expression(gamma),xlab="",cex.main=2)
abline(v=results_main$gamma_true,lwd=2)
abline(v=mean(results_main$gamma_est),lty=2,lwd=2)
abline(v=median(results_main$gamma_est),lty=3,lwd=2)
hist(results_main$alpha_est,breaks=20,col="grey",main=expression(alpha),xlab="",cex.main=2)
abline(v=results_main$alpha_true,lwd=2)
abline(v=mean(results_main$alpha_est),lty=2,lwd=2)
abline(v=median(results_main$alpha_est),lty=3,lwd=2)
hist(results_main$rho_est,breaks=20,col="grey",main=expression(rho),xlab="",cex.main=2)
abline(v=results_main$rho_true,lwd=2)
abline(v=mean(results_main$rho_est),lty=2,lwd=2)
abline(v=median(results_main$rho_est),lty=3,lwd=2)
hist(results_main$sigma_eps_est,breaks=20,col="grey",main=expression(sigma[epsilon]),xlab="",cex.main=2)
abline(v=results_main$sigma_eps_true,lwd=2)
abline(v=mean(results_main$sigma_eps_est),lty=2,lwd=2)
abline(v=median(results_main$sigma_eps_est),lty=3,lwd=2)
#turn plotting function off
dev.off()

#boxplots of gamma and sigma_eps residuals
#initialize plotting function
pdf("gam_sig_resids.pdf",8,4)
#create plots
gb <- ggplot(results_gam,aes(factor(gamma_true),gamma_resids))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab("True value")+
  ylab("Residuals")+
  labs(title=expression(gamma))+
  coord_flip()
sb <- ggplot(results_sig,aes(factor(sigma_eps_true),sigma_eps_resids))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab("")+
  scale_x_discrete(breaks=NULL)+
  ylab("Residuals")+
  labs(title=expression(sigma[epsilon]))+
  coord_flip()
#arrange plots
grid.arrange(gb,sb,ncol=2)
#turn plotting function off
dev.off()