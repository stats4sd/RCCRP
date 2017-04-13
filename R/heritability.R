#' Calculates heritability statistic(s) from breeding trial. syntax largely stolen from Ric Coe.
#' @param genotypes vector containing genotype (or option) ID
#' @param environments vector containing environment (or context) ID
#' @param outcome vector containing numeric outcome values
#' @param rep rep variable - must be a unique identifier
#' @keywords heritability
#' @export
#' @examples
#' library(agricolae)
#' data(plrv)
#' heritability(plrv$Genotype,plrv$Locality,plrv$Yield,paste(plrv$Locality,plrv$Rep))

heritability<-function(genotypes, environments=NULL, outcome, rep){
  

  require(lme4)
  
  if(length(environments)>0){
    data<-data.frame(variety=genotypes, environment=environments,y=outcome, rep=rep)
    mod1<-lmer(y~(1|environment)+(1|environment:rep)+(1|variety)+(1|variety:environment),data)
    
    
    
    #extracting variance compnents and calculating heritability
    vars<-as.data.frame(VarCorr(mod1))
    vg<-vars$vcov[vars$grp=="variety"]
    vge<-vars$vcov[vars$grp=="variety:environment"]
    verror<-vars$vcov[vars$grp=="Residual"]
    h2<-vg/(vg+vge+verror)
    vt<-vars$vcov[vars$grp=="environment"]
    vtr<-vars$vcov[vars$grp=="environment:rep"]
    
    interaction_prop<-vge/(vg+vge+vt+vtr+verror)
    return(list(h2=h2,GxE=interaction_prop,model=mod1))
    
  }
  else{
    data<-data.frame(variety=genotypes, y=outcome, rep=rep)
    mod1<-lmer(y~(1+rep)+(1|variety),data)
    
    
    
    #extracting variance compnents and calculating heritability
    vars<-as.data.frame(VarCorr(mod1))
    vg<-vars$vcov[vars$grp=="variety"]
    verror<-vars$vcov[vars$grp=="Residual"]
    h2<-vg/(vg+verror)
    return(list(h2=h2,model=mod1))
  }
}
