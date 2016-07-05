#Combining the Cohen-Fishman growth increment model 
#with a Box-Cox transformation: flexibility and uncertainty
#
#Terrance J. Quinn II and Richard B. Deriso
#
#Example with Pacific halibut mark-recapture data
#Simulation implemented in R by Phil Ganz

#set seed for reproducibility
set.seed(269)

#read in real mark-recapture data for Pacific halibut
dat <- read.table("cfbc.dat",skip=3,col.names=c("Y1","Y2","Delt"))

#MLE parameter estimates
gamma <- 0.407856120493
alpha <- 0.958360876960
rho <- 0.945290125594

#sigma_epsilon
sigma_eps_sq <- 0.101212

#number of observations
nobs <- length(dat$Y1)

#number of simulation replicates
R <- 1000

#set up table for saving estimates
results <- data.frame(gamma=NA,
	                  alpha=NA,
	                  rho=NA,
	                  sigma_eps_sq=NA,
	                  alpha_tilde=NA,
	                  k=NA,
	                  Y_inf=NA,
	                  obj_fun=NA)

#build admb model
shell("admb cfbc")

#run simulation
for (i in 1:R){
#apply Box-Cox transform to Y1s to get x1s
x1 <- (dat$Y1^gamma-1)/gamma

#simulate x2s from MLE parameter estimates (treated as "known" parameter values)
x2.sim <- alpha*(1-rho^dat$Delt)/(1-rho)+rho^dat$Delt*x1
#add error
x2.sim <- x2.sim + sqrt(sigma_eps_sq * (1-rho^(2*dat$Delt))/(1-rho^2)) * rnorm(nobs,0,1)

#back-transform x2s to simulate Y2s
Y2.sim <- (gamma*x2.sim+1)^(1/gamma)
Y2.sim <- round(Y2.sim)

#format simulated data for text file
dat.sim <- matrix(nrow=323,ncol=3)
colnames(dat.sim) <- c("#Number","of","observations")
dat.sim[1,] <- c(nobs,"","")
dat.sim[2,] <- c("","","")
dat.sim[3,] <- c("#Y1","Y2","Delt")
dat.sim[-c(1:3),1] <- dat$Y1
dat.sim[-c(1:3),2] <- Y2.sim
dat.sim[-c(1:3),3] <- dat$Delt
#write simulated data to text file
write.table(dat.sim,file=paste(getwd(),"/cfbc.sim.DAT",sep=""),quote=FALSE,row.names=FALSE)

#run estimation model
shell("cfbc")

#read in parameter estimates from .par file
results[i,c(1:3)] <- t(read.delim("cfbc.par",comment.char = "#",header=FALSE,sep=" "))

#read in other values of interest from .rep file
rep <- read.delim("cfbc.rep",colClasses = "character")
results[i,"sigma_eps_sq"] <- as.numeric(rep[1,])
results[i,"alpha_tilde"]  <- as.numeric(rep[3,])
results[i,"k"]            <- as.numeric(rep[5,])
results[i,"Y_inf"]        <- as.numeric(rep[7,])
results[i,"obj_fun"]      <- as.numeric(rep[9,])
}

#save results as .csv
#write.csv(results,"results.csv")

#read in results from .csv
#results <- read.csv("results.csv")[,-1]

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
