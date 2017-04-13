#' Does modified joint regression analysis for variety-by-environment data (P.W. Lane & K. Ryder).
#'
#' Equivalent to RJOINT function in Genstat. Function taken from code included in agridat() package help menu which was not turned into a function (for some reason)
#' @param genotypes vector containing genotype (or option) ID
#' @param environments vector containing environment (or context) ID
#' @param outcome vector containing numeric outcome values
#' @param tolerance tolerance level for convergence
#' @param cex.label size of labels on plot
#' @param ... other arguments to plot()
#' @keywords Sensitivity
#' @export
#' @examples
#' library(agricolae)
#' data(plrv)
#' sensitivity(plrv$Genotype,plrv$Locality,plrv$Yield)


sensitivity<-function(genotypes,environments,outcome,tolerance=.0001,cex.label=0.8,...){
  require(doBy)
  dat<-data.frame(outcome=outcome,gen=genotypes,env=environments)
  dat$gen<-as.factor(dat$gen)
  dat$env<-as.factor(dat$env)
  n.gen <- nlevels(dat$gen)
  n.env <- nlevels(dat$env)
  
  unadjmeans<-tapply(dat$outcome,dat$gen,mean,na.rm=T)
  
  # Estimate theta (env eff)
  m0 <- lm(outcome ~ -1 + env + gen, dat)
  thetas <- coef(m0)[1:n.env]
  thetas <- thetas-mean(thetas) 
  # center env effects
  # Add env effects to the data
  dat$theta <- thetas[match(paste("env",dat$env,sep=""), names(thetas))]
  
  # Initialize beta (gen slopes) at 1
  betas <- rep(1, n.gen)
  
  done <- FALSE
  counter<-0
  while(!done){
    
    betas0 <- betas
    
    # M1: Fix thetas (env effects), estimate beta (gen slope)
    m1 <- lm(outcome ~ -1 + gen + gen:theta, data=dat)
    betas <- coef(m1)[-c(1:n.gen)]
    dat$beta <- betas[match(paste("gen",dat$gen,":theta",sep=""), names(betas))]
    # print(betas)
    
    # M2: Fix betas (gen slopes), estimate theta (env slope)
    m2 <- lm(outcome ~ env:beta + gen -1, data=dat)
    
    thetas <- coef(m2)[-c(1:n.gen)]
    thetas[is.na(thetas)] <- 0  # Change last coefficient from NA to 0
    dat$theta <- thetas[match(paste("env",dat$env,":beta",sep=""), names(thetas))]
    #print(thetas)
    
    
    # Check convergence
    chg <- sum(((betas-betas0)/betas0)^2)
    counter<-counter+1
    cat("Iteration",counter,": Relative change in betas",chg,"\n")
    if(chg < tolerance){ done <- TRUE
    adjmeans<-data.frame(LSmeans(m1,"gen",at=list(theta=mean(thetas)))$coef)
    betas.se<-summary(m1)$coef[,2][-c(1:n.gen)]
    env.means<-data.frame(LSmeans(m2,"env")$coef)
    }
    
  }
  Geno<-data.frame(genotype=levels(dat$gen),mean=unadjmeans,adjustedmean=adjmeans$estimate,adjustedmean.se=adjmeans$se,Sensitivity=betas,Sensitivity.se=betas.se)
  rownames(Geno)<-NULL               
  Envi<-data.frame(environment=levels(dat$env),Effect=thetas-mean(thetas),adjustedmean=env.means$estimate)
  rownames(Envi)<-NULL
  
  plot(Geno$adjustedmean,Geno$Sensitivity,xlab="Adusted Mean",ylab="Sensitivity",pch="",...)  
  text(Geno$adjustedmean,Geno$Sensitivity,Geno$genotype,cex=cex.label)  
  title("Sensitivity")
  
  return(list(gen=Geno,env=Envi))
}