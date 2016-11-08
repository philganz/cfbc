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

load("results_fixed_gam.RData")
results_fixed_gam <- results

#remove last array
rm(results)

#stack results from separate cases on top of each other
results_gam  <- rbind(results_gam[,,"0.1",],results_gam[,,"0.3",],results_gam[,,"0.7",],results_gam[,,"0.9",])
results_main <- as.matrix(results_main[,,1,1])
results_sig  <- rbind(results_sig[,,,"0.1"],results_sig[,,,"0.3"],results_sig[,,,"0.7"],results_sig[,,,"0.9"])
results_fixed_gam  <- rbind(results_fixed_gam[,,"0.1",],results_fixed_gam[,,"0.3",],results_fixed_gam[,,"0.5",],results_fixed_gam[,,"0.7",],results_fixed_gam[,,"0.9",],results_fixed_gam[,,"1",])

#convert to data frames
results_gam  <- data.frame(results_gam)
results_main <- data.frame(results_main)
results_sig  <- data.frame(results_sig)
results_fixed_gam  <- data.frame(results_fixed_gam)
  
#create one data frame for all results (except fixed case)
results <- rbind(results_gam,results_main,results_sig)

#add main results to gamma and sigma cases
results_gam <- rbind(results_gam,results_main)
results_sig <- rbind(results_sig,results_main)

#add residuals column for parameters of interest
results_gam$gamma_resids           <- results_gam$gamma_est - results_gam$gamma_true
results_gam$sigma_eps_resids       <- results_gam$sigma_eps_est - results_gam$sigma_eps_true
results_sig$gamma_resids           <- results_sig$gamma_est - results_sig$gamma_true
results_sig$sigma_eps_resids       <- results_sig$sigma_eps_est - results_sig$sigma_eps_true
results_fixed_gam$alpha_resids     <- results_fixed_gam$alpha_est - results_fixed_gam$alpha_true
results_fixed_gam$rho_resids       <- results_fixed_gam$rho_est - results_fixed_gam$rho_true
results_fixed_gam$sigma_eps_resids <- results_fixed_gam$sigma_eps_est - results_fixed_gam$sigma_eps_true

#plots

#save default margins
d.mar <- c(5.1,4.1,4.1,2.1)

#Figure 6. Probability distributions of parameter estimates (converged runs only)
#initialize plotting function
pdf("params_hists_converged.pdf",7,5)
#create plots
par(mfrow=c(2,2),mar=d.mar-c(0,0,1,0))
hist(subset(results_main$gamma_est,results_main$convergence==1),breaks=20,col="grey",main=expression(gamma),xlab="",cex.main=2)
abline(v=results_main$gamma_true,lwd=2)
abline(v=mean(subset(results_main$gamma_est,results_main$convergence==1)),lty=2,lwd=2)
abline(v=median(subset(results_main$gamma_est,results_main$convergence==1)),lty=3,lwd=2)
hist(subset(results_main$alpha_est,results_main$convergence==1),breaks=20,col="grey",main=expression(alpha),xlab="",cex.main=2)
abline(v=results_main$alpha_true,lwd=2)
abline(v=mean(subset(results_main$alpha_est,results_main$convergence==1)),lty=2,lwd=2)
abline(v=median(subset(results_main$alpha_est,results_main$convergence==1)),lty=3,lwd=2)
hist(subset(results_main$rho_est,results_main$convergence==1),breaks=20,col="grey",main=expression(rho),xlab="",cex.main=2)
abline(v=results_main$rho_true,lwd=2)
abline(v=mean(subset(results_main$rho_est,results_main$convergence==1)),lty=2,lwd=2)
abline(v=median(subset(results_main$rho_est,results_main$convergence==1)),lty=3,lwd=2)
hist(subset(results_main$sigma_eps_est,results_main$convergence==1),breaks=20,col="grey",main=expression(sigma[epsilon]),xlab="",cex.main=2)
abline(v=results_main$sigma_eps_true,lwd=2)
abline(v=mean(subset(results_main$sigma_eps_est,results_main$convergence==1)),lty=2,lwd=2)
abline(v=median(subset(results_main$sigma_eps_est,results_main$convergence==1)),lty=3,lwd=2)
#turn plotting function off
dev.off()

#Figure 7. Relative residuals for gamma sensitivity simulation (converged runs only)
#initialize plotting function
pdf("gam_case_all_relative_converged.pdf",8,4)
#create plots
gac <- ggplot(subset(results_gam,results_gam$convergence==1),aes(factor(gamma_true),(alpha_est-alpha_true)/alpha_true))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab(expression("True"~gamma))+
  ylab("Relative residuals")+
  labs(title=expression(alpha))+
  coord_flip()
ggc <- ggplot(subset(results_gam,results_gam$convergence==1),aes(factor(gamma_true),gamma_resids/gamma_true))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab("")+
  scale_x_discrete(breaks=NULL)+
  ylab("Relative residuals")+
  labs(title=expression(gamma))+
  coord_flip()
grc <- ggplot(subset(results_gam,results_gam$convergence==1),aes(factor(gamma_true),(rho_est-rho_true)/rho_true))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab(expression("True"~gamma))+
  ylab("Relative residuals")+
  labs(title=expression(rho))+
  coord_flip()
gsc <- ggplot(subset(results_gam,results_gam$convergence==1),aes(factor(gamma_true),sigma_eps_resids/sigma_eps_true))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab("")+
  scale_x_discrete(breaks=NULL)+
  ylab("Relative residuals")+
  labs(title=expression(sigma[epsilon]))+
  coord_flip()
#arrange plots
grid.arrange(gac,ggc,grc,gsc,ncol=2,nrow=2)
#turn plotting function off
dev.off()

#Figure 8. Relative residuals for sigma sensitivity simulation (converged runs only)
#initialize plotting function
pdf("sig_case_all_relative_converged.pdf",8,4)
#create plots
sac <- ggplot(subset(results_sig,results_sig$convergence==1),aes(factor(sigma_eps_true),(alpha_est-alpha_true)/alpha_true))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab(expression("True"~sigma[epsilon]))+
  ylab("Relative residuals")+
  labs(title=expression(alpha))+
  coord_flip()
sgc <- ggplot(subset(results_sig,results_sig$convergence==1),aes(factor(sigma_eps_true),gamma_resids/gamma_true))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab("")+
  scale_x_discrete(breaks=NULL)+
  ylab("Relative residuals")+
  labs(title=expression(gamma))+
  coord_flip()
src <- ggplot(subset(results_sig,results_sig$convergence==1),aes(factor(sigma_eps_true),(rho_est-rho_true)/rho_true))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab(expression("True"~sigma[epsilon]))+
  ylab("Relative residuals")+
  labs(title=expression(rho))+
  coord_flip()
ssc <- ggplot(subset(results_sig,results_sig$convergence==1),aes(factor(sigma_eps_true),sigma_eps_resids/sigma_eps_true))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab("")+
  scale_x_discrete(breaks=NULL)+
  ylab("Relative residuals")+
  labs(title=expression(sigma[epsilon]))+
  coord_flip()
#arrange plots
grid.arrange(sac,sgc,src,ssc,ncol=2,nrow=2)
#turn plotting function off
dev.off()

#fixed gamma exercise
#reassign true gammas to objects
gamma_true <-  c(0.1, 0.3, 0.5, 0.7, 0.9, 1)
#set up labels
xlab <- c(expression(gamma==0.1),expression(gamma==0.3),expression(gamma==0.5),expression(gamma==0.7),expression(gamma==0.9),expression(gamma==1))

#Figure 9. Residuals for fixed gamma cases > 0.3 (converged runs only)
#initialize plotting function
pdf("fixed_high_gam_resids_converged.pdf",10,4)
#create plots
fab2c <- ggplot(subset(results_fixed_gam,results_fixed_gam$gamma_true>0.3 & results_fixed_gam$convergence==1),aes(factor(gamma_true),alpha_resids))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab(expression("True"~gamma))+
  ylab("Residuals")+
  labs(title=expression(alpha))+
  coord_flip()
frb2c <- ggplot(subset(results_fixed_gam,results_fixed_gam$gamma_true>0.3 & results_fixed_gam$convergence==1),aes(factor(gamma_true),rho_resids))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab("")+
  scale_x_discrete(breaks=NULL)+
  ylab("Residuals")+
  labs(title=expression(rho))+
  coord_flip()
fsb2c <- ggplot(subset(results_fixed_gam,results_fixed_gam$gamma_true>0.3 & results_fixed_gam$convergence==1),aes(factor(gamma_true),sigma_eps_resids))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab("")+
  scale_x_discrete(breaks=NULL)+
  ylab("Residuals")+
  labs(title=expression(sigma[epsilon]))+
  coord_flip()
#arrange plots
grid.arrange(fab2c,frb2c,fsb2c,ncol=3)
#turn plotting function off
dev.off()
