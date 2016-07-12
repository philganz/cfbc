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

#boxplots of gamma and sigma_eps residuals individually 
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

#boxplots of gamma and sigma_eps residuals 

#gamma cases
#initialize plotting function
pdf("gam_case_resids.pdf",8,4)
#create plots
gg <- ggplot(results_gam,aes(factor(gamma_true),gamma_resids))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab(expression("True"~gamma))+
  ylab("Residuals")+
  labs(title=expression(gamma))+
  coord_flip()
gs <- ggplot(results_gam,aes(factor(gamma_true),sigma_eps_resids))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab("")+
  scale_x_discrete(breaks=NULL)+
  ylab("Residuals")+
  labs(title=expression(sigma[epsilon]))+
  coord_flip()
#arrange plots
grid.arrange(gg,gs,ncol=2)
#turn plotting function off
dev.off()

#sigma_eps cases
#initialize plotting function
pdf("sig_case_resids.pdf",8,4)
#create plots
sg <- ggplot(results_sig,aes(factor(sigma_eps_true),gamma_resids))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab(expression("True"~sigma[epsilon]))+
  ylab("Residuals")+
  labs(title=expression(gamma))+
  coord_flip()
ss <- ggplot(results_sig,aes(factor(sigma_eps_true),sigma_eps_resids))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab("")+
  scale_x_discrete(breaks=NULL)+
  ylab("Residuals")+
  labs(title=expression(sigma[epsilon]))+
  coord_flip()
#arrange plots
grid.arrange(sg,ss,ncol=2)
#turn plotting function off
dev.off()

#boxplots of gamma and sigma_eps residuals 

#gamma cases
#initialize plotting function
pdf("gam_case_rel_resids.pdf",8,4)
#create plots
gg <- ggplot(results_gam,aes(factor(gamma_true),gamma_resids/gamma_true))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab(expression("True"~gamma))+
  ylab("Relative residuals")+
  labs(title=expression(gamma))+
  coord_flip()
gs <- ggplot(results_gam,aes(factor(gamma_true),sigma_eps_resids/sigma_eps_true))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab("")+
  scale_x_discrete(breaks=NULL)+
  ylab("Relative residuals")+
  labs(title=expression(sigma[epsilon]))+
  coord_flip()
#arrange plots
grid.arrange(gg,gs,ncol=2)
#turn plotting function off
dev.off()

#sigma_eps cases
#initialize plotting function
pdf("sig_case_rel_resids.pdf",8,4)
#create plots
sg <- ggplot(results_sig,aes(factor(sigma_eps_true),gamma_resids/gamma_true))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab(expression("True"~sigma[epsilon]))+
  ylab("Relative residuals")+
  labs(title=expression(gamma))+
  coord_flip()
ss <- ggplot(results_sig,aes(factor(sigma_eps_true),sigma_eps_resids/sigma_eps_true))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab("")+
  scale_x_discrete(breaks=NULL)+
  ylab("Relative residuals")+
  labs(title=expression(sigma[epsilon]))+
  coord_flip()
#arrange plots
grid.arrange(sg,ss,ncol=2)
#turn plotting function off
dev.off()

#fixed gamma histograms
#reassign true gammas to objects
gamma_true <-  c(0.1, 0.3, 0.5, 0.7, 0.9, 1)
#set up labels
xlab <- c(expression(gamma==0.1),expression(gamma==0.3),expression(gamma==0.5),expression(gamma==0.7),expression(gamma==0.9),expression(gamma==1))
#initialize plotting function
pdf("fixed_gam_hists.pdf",10,4)
#create plots
par(mfcol=c(3,6),mar=d.mar-c(1,2,3,0))
for (j in 1:length(gamma_true)){
hist(subset(results_fixed_gam$alpha_est,results_fixed_gam$gamma_true==gamma_true[j]),breaks=20,col="grey",main=expression(alpha),xlab="",cex.main=1.8)
abline(v=results_fixed_gam$alpha_true, lwd=1.3)
abline(v=mean(subset(results_fixed_gam$alpha_est,results_fixed_gam$gamma_true==gamma_true[j])),lty=2, lwd=1.3)
abline(v=median(subset(results_fixed_gam$alpha_est,results_fixed_gam$gamma_true==gamma_true[j])),lty=3, lwd=1.3)
hist(subset(results_fixed_gam$rho_est,results_fixed_gam$gamma_true==gamma_true[j]),breaks=20,col="grey",main=expression(rho),xlab="",cex.main=1.8)
abline(v=results_fixed_gam$rho_true, lwd=1.3)
abline(v=mean(subset(results_fixed_gam$rho_est,results_fixed_gam$gamma_true==gamma_true[j])),lty=2, lwd=1.3)
abline(v=median(subset(results_fixed_gam$rho_est,results_fixed_gam$gamma_true==gamma_true[j])),lty=3, lwd=1.3)
hist(subset(results_fixed_gam$sigma_eps_est,results_fixed_gam$gamma_true==gamma_true[j]),breaks=20,col="grey",main=expression(sigma[epsilon]),xlab=xlab[j],cex.main=1.8,cex.lab=1.5)
abline(v=results_fixed_gam$sigma_eps_true, lwd=1.3)
abline(v=mean(subset(results_fixed_gam$sigma_eps_est,results_fixed_gam$gamma_true==gamma_true[j])),lty=2, lwd=1.3)
abline(v=median(subset(results_fixed_gam$sigma_eps_est,results_fixed_gam$gamma_true==gamma_true[j])),lty=3, lwd=1.3)}
#turn plotting function off
dev.off()

#fixed gamma boxplots

#all values of gamma_true residuals
#initialize plotting function
pdf("fixed_all_gam_resids.pdf",10,4)
#create plots
fab <- ggplot(results_fixed_gam,aes(factor(gamma_true),alpha_resids))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab(expression("True"~gamma))+
  ylab("Residuals")+
  scale_y_continuous(breaks=c(0,25,50,75,100))+
  labs(title=expression(alpha))+
  coord_flip()
frb <- ggplot(results_fixed_gam,aes(factor(gamma_true),rho_resids))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab("")+
  scale_x_discrete(breaks=NULL)+
  ylab("Residuals")+
  labs(title=expression(rho))+
  coord_flip()
fsb <- ggplot(results_fixed_gam,aes(factor(gamma_true),sigma_eps_resids))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab("")+
  scale_x_discrete(breaks=NULL)+
  ylab("Residuals")+
  labs(title=expression(sigma[epsilon]))+
  coord_flip()
#arrange plots
grid.arrange(fab,frb,fsb,ncol=3)
#turn plotting function off
dev.off()

#values of gamma_true > 0.3 residuals
#initialize plotting function
pdf("fixed_high_gam_resids.pdf",10,4)
#create plots
fab2 <- ggplot(subset(results_fixed_gam,results_fixed_gam$gamma_true>0.3),aes(factor(gamma_true),alpha_resids))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab(expression("True"~gamma))+
  ylab("Residuals")+
  labs(title=expression(alpha))+
  coord_flip()
frb2 <- ggplot(subset(results_fixed_gam,results_fixed_gam$gamma_true>0.3),aes(factor(gamma_true),rho_resids))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab("")+
  scale_x_discrete(breaks=NULL)+
  ylab("Residuals")+
  labs(title=expression(rho))+
  coord_flip()
fsb2 <- ggplot(subset(results_fixed_gam,results_fixed_gam$gamma_true>0.3),aes(factor(gamma_true),sigma_eps_resids))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab("")+
  scale_x_discrete(breaks=NULL)+
  ylab("Residuals")+
  labs(title=expression(sigma[epsilon]))+
  coord_flip()
#arrange plots
grid.arrange(fab2,frb2,fsb2,ncol=3)
#turn plotting function off
dev.off()

#all values of gamma_true residuals
#initialize plotting function
pdf("fixed_all_gam_rel_resids.pdf",10,4)
#create plots
fabr <- ggplot(results_fixed_gam,aes(factor(gamma_true),alpha_resids/alpha_true))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab(expression("True"~gamma))+
  ylab("Relative residuals")+
  scale_y_continuous(breaks=c(0,25,50,75,100))+
  labs(title=expression(alpha))+
  coord_flip()
frbr <- ggplot(results_fixed_gam,aes(factor(gamma_true),rho_resids/rho_true))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab("")+
  scale_x_discrete(breaks=NULL)+
  ylab("Relative residuals")+
  labs(title=expression(rho))+
  coord_flip()
fsbr <- ggplot(results_fixed_gam,aes(factor(gamma_true),sigma_eps_resids/sigma_eps_true))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab("")+
  scale_x_discrete(breaks=NULL)+
  ylab("Relative residuals")+
  labs(title=expression(sigma[epsilon]))+
  coord_flip()
#arrange plots
grid.arrange(fabr,frbr,fsbr,ncol=3)
#turn plotting function off
dev.off()

#values of gamma_true > 0.3 residuals
#initialize plotting function
pdf("fixed_high_gam_rel_resids.pdf",10,4)
#create plots
fabr2 <- ggplot(subset(results_fixed_gam,results_fixed_gam$gamma_true>0.3),aes(factor(gamma_true),alpha_resids/alpha_true))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab(expression("True"~gamma))+
  ylab("Relative residuals")+
  labs(title=expression(alpha))+
  coord_flip()
frbr2 <- ggplot(subset(results_fixed_gam,results_fixed_gam$gamma_true>0.3),aes(factor(gamma_true),rho_resids/rho_true))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab("")+
  scale_x_discrete(breaks=NULL)+
  ylab("Relative residuals")+
  labs(title=expression(rho))+
  coord_flip()
fsbr2 <- ggplot(subset(results_fixed_gam,results_fixed_gam$gamma_true>0.3),aes(factor(gamma_true),sigma_eps_resids/sigma_eps_true))+
  geom_boxplot()+
  geom_hline(yintercept=0)+
  theme_classic(base_size=16)+
  xlab("")+
  scale_x_discrete(breaks=NULL)+
  ylab("Relative residuals")+
  labs(title=expression(sigma[epsilon]))+
  coord_flip()
#arrange plots
grid.arrange(fabr2,frbr2,fsbr2,ncol=3)
#turn plotting function off
dev.off()