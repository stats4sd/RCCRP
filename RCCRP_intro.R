##Installation of package - requires reliable internet as a few things need to be downloaded here

#first install devtools to load direct from github
#install.packages("devtools")
library(devtools)

#then load the RCCRP package from github
#install_github("stats4sd/RCCRP")

library(RCCRP)

#now to go through each of the functions within the package:

#This is not meant to be a comprehensive guide to everything needed for an OxC analysis
#These are some functions to simplify the processing of some common and important tasks 

#I am going to use the MET data called plrv from the agricolae package as an example dataset
library(agricolae)
data(plrv)

#for a summary of the dataset go to the help menu using ?
?plrv

#for a more numerical summary use summary:
summary(plrv)

#One problem we will run into later - we need a unique plot ID.
#Rep is repeated across locations so this can be made be combining rep and locality columns
plrv$plotID<-paste(plrv$Locality,plrv$Rep,sep="")

##lets do some summary graphs - obviously boxplots are nice:

boxplot(Yield~Genotype,data=plrv)
boxplot(Yield~Genotype+Locality,data=plrv)

##means with error bars are a little bit trickier to get off the ground though - so I have written something to help that works in exactly the same way

mean_errorbar(Yield~Genotype,data=plrv)
mean_errorbar(Yield~Genotype+Locality,data=plrv)
#looks kind of horrible with so many combos but it works

#but I also let you extract the data going into this plot if you want to play with it somewhere else

means_errors<-mean_errorbar(Yield~Genotype+Locality,data=plrv,output = TRUE)
means_errors

#although these can all be done to look much nicer if you want to play with ggplot2 through a bit of practice
library(ggplot2)
ggplot(data=plrv,aes(y=Yield,x=Genotype))+geom_boxplot()+facet_wrap(~Locality)+coord_flip()

ggplot(data=plrv,aes(y=Yield,x=Genotype,group=Locality))+
  stat_summary(fun.y = mean,geom = "point")+
  stat_summary(fun.data = mean_se,geom = "errorbar",fun.args = list(mult=1.96))+
  facet_wrap(~Locality)+coord_flip()



#OK - lets look at some pairwise treatment comparisons
#Lets look at Unica and 141.28 since they caught my eye in the boxplots for some reason
compare_trt(outcome=plrv$Yield,
            plotID=plrv$plotID,
            trtID=plrv$Genotype,
            trt1="Unica",
            trt2="141.28")

#The blue line fits a trend  - this will maybe help to explain if the OxC interaction with these two varieties follows a nice easy pattern
#(e.g. Var X better in low yielding envs, var Y better in high yielding envs)
#This doesn't really show us much in this case - if you want to remove the blue line you can add in an option:
compare_trt(outcome=plrv$Yield,
            plotID=plrv$plotID,
            trtID=plrv$Genotype,
            trt1="Unica",
            trt2="141.28",
            smoother = FALSE)


#black line is y=x and red line shows average difference - so in this case average is very similar between the two
#again can remove this line if you want 
compare_trt(outcome=plrv$Yield,
            plotID=plrv$plotID,
            trtID=plrv$Genotype,
            trt1="Unica",
            trt2="141.28",
            smoother = FALSE,
            meanline = FALSE)

#so far it looks like no clear difference between the two vars - but what about GxE information? We can split points by site
compare_trt(outcome=plrv$Yield,
            plotID=plrv$plotID,
            trtID=plrv$Genotype,
            grpID=plrv$Locality,
            trt1="Unica",
            trt2="141.28",
            smoother = FALSE,
            meanline = FALSE)

#So we can get a nice picture -
#in some places no real difference as points straddle the middle line (e.g. SR-03)
#in some places Unica is always a winner (e.g. LM-02)
#in some places 141.28 is a clear winner (e.g. LM-03)


#######OK - graph number 2, presenting same data in different way using cumulative frequency plots:
riskdiff(outcome=plrv$Yield,
         plotID=plrv$plotID,
         trtID=plrv$Genotype,
         trt1="Unica",
         trt2="141.28",
         main="Cumulative Frequency Plot of Unica vs 141.28")

#######OK - graph number 2, presenting same data in different way:
riskdiff(outcome=plrv$Yield,
         plotID=plrv$plotID,
         trtID=plrv$Genotype,
         grpID=plrv$Locality,
         trt1="Unica",
         trt2="141.28",
         main="Cumulative Frequency Plot of Unica vs 141.28")

#A little bit messier (note to self - make this more beautiful by ggplot-ifying) but shows the same things
#could also do something similar with riskplot to show actual values rather than differences but need to do some subsetting first
plrv_unica<-subset(plrv,Genotype=="Unica")

riskplot(y=plrv_unica$Yield,
         splitvar=plrv_unica$Locality,
         main="Cumulative Frequency Plot of Unica vs 141.28")


#how about a biplot?
biplot_from_data(genotypes = plrv$Genotype,
                 environments = plrv$Locality,
                 outcome = plrv$Yield)

#This will launch GGE Biplot in a new window & you can play around endlessly
#Note it may confuse you slightly if you have a non-complete set of genotypes and environments.
#Defaults to all environments included but only the genotypes present in all environments


##heritability for breeders:

heritability(genotypes = plrv$Genotype,
             environments = plrv$Locality,
             outcome = plrv$Yield,
             rep=plrv$plotID)

#h2 is the overall heritability
#GxE is the genotype by environment heritability
#the variance component values are shown in the model part of it


#sensitivity -  modified joint regression analysis for variety-by-environment data (P.W. Lane & K. Ryder)
#This is RJOINT in genstat
sensitivity(genotypes = plrv$Genotype,
             environments = plrv$Locality,
             outcome = plrv$Yield)

#superiority -   Calculates stability coefficients for genotype-by-environment data (R.W. Payne).
#Equivalent to GESTABILITY function in Genstat
superiority(genotypes = plrv$Genotype,
            environments = plrv$Locality,
            outcome = plrv$Yield)







