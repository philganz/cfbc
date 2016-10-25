#Combining the Cohen-Fishman growth increment model 
#with a Box-Cox transformation
#
#Terrance J. Quinn II, Richard B. Deriso, and Philip D. Ganz
#
#Example with Pacific halibut mark-recapture data
#Simulation implemented in R by P.D. Ganz

#set seed for reproducibility
set.seed(269)

#get required package(s)
library(xlsx)

#read in real mark-recapture data for Pacific halibut
dat <- read.table("cfbc.dat",skip=3,col.names=c("Y1","Y2","Delt"))

#"true" parameter values for simulation  
gamma_true <-  c(0.1, 0.3, 0.5, 0.7, 0.9, 1) #0.5 #c(0.1, 0.3, 0.7, 0.9)
alpha_true <- 1
rho_true   <- 0.95

#sigma_epsilon
sigma_eps_true <- 0.5 #c(0.1, 0.3, 0.7, 0.9)

#number of observations
nobs <- length(dat$Y1)

#number of simulation replicates
R <- 1000

#set up an array for saving estimates
to_record <- c("gamma_true", "gamma_est","alpha_true","alpha_est","rho_true","rho_est","sigma_eps_true","sigma_eps_est","alpha_tilde","k","Y_inf","obj_fun","maxgrad","convergence")
results   <- array(NA,dim=c(R,length(to_record),length(gamma_true),length(sigma_eps_true)),dimnames=list(NULL,to_record,gamma_true,sigma_eps_true))

#set up file name for saving results later
if(length(gamma_true)==4) {file_name <- "results_gam.xlsx"
} else if (length(sigma_eps_true)==4) {file_name <- "results_sig.xlsx"
} else if (length(gamma_true)==6) {file_name <- "results_fixed_gam.xlsx"
} else {file_name <- "results_main.xlsx"}
write.xlsx(R,file=file_name,sheetName="Replicates")

#build admb model
shell("admb cfbc")

#run simulation
#i <- 1; j <- 1; k <- 1
for (j in 1:length(gamma_true)){
for (k in 1:length(sigma_eps_true)){
for (i in 1:R){
#apply Box-Cox transform to Y1s to get x1s
x1 <- (dat$Y1^gamma_true[j]-1)/gamma_true[j]

#simulate x2s from MLE parameter estimates (treated as "known" parameter values)
x2.sim <- alpha_true*(1-rho_true^dat$Delt)/(1-rho_true)+rho_true^dat$Delt*x1
#add error
x2.sim <- x2.sim + sigma_eps_true[k] * sqrt((1-rho_true^(2*dat$Delt))/(1-rho_true^2)) * rnorm(nobs,0,1)

#back-transform x2s to simulate Y2s
Y2.sim <- (gamma_true[j]*x2.sim+1)^(1/gamma_true[j])

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

#keep track of true values
results[i,c("gamma_true","alpha_true","rho_true","sigma_eps_true"),j,k] <- c(gamma_true[j],alpha_true,rho_true,sigma_eps_true[k])

#read in parameter estimates from .par file
results[i,c("gamma_est","alpha_est","rho_est"),j,k] <- t(read.delim("cfbc.par",comment.char = "#",header=FALSE,sep=" "))

#read in other values of interest from .rep file
rep <- read.delim("cfbc.rep",colClasses = "character")
results[i,"sigma_eps_est",j,k] <- sqrt(as.numeric(rep[1,]))
results[i,"alpha_tilde",j,k]   <- as.numeric(rep[3,])
results[i,"k",j,k]             <- as.numeric(rep[5,])
results[i,"Y_inf",j,k]         <- as.numeric(rep[7,])
results[i,"obj_fun",j,k]       <- as.numeric(rep[9,])
results[i,"maxgrad",j,k]       <- as.numeric(rep[11,])
results[i,"convergence",j,k]   <- results[i,"maxgrad",j,k]<0.0001

#end replicate loop
}

#save results in excel
sheet_name <- paste("gam",gamma_true[j],"_sig",sigma_eps_true[k],sep="")
write.xlsx(results[,,j,k], file=file_name, sheetName=sheet_name,append=TRUE)

#end gamma and sigma_eps case loops
}}

#save results as RData
save(results,file=paste(substr(file_name,1,nchar(file_name)-4),"RData",sep=""))