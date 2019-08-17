################################################################################
######                  Psychometrics Project Analysis                  ########
################################################################################

#clear the environment
rm(list = ls())

#set your working directory
setwd("~/Dropbox/Graduate Courses/Psychometrics/Final Project/Project Data Files")

#load necessary packages
library(Hmisc)
library(xlsx)
library(dplyr)
library(psych)
library(lessR)

#read in data
all_content = readLines("FULLDATA_CLEAN.csv")
skip_second = all_content[-2]
dat1 = read.csv(textConnection(skip_second), header = T, stringsAsFactors = T)

#subset education
edu <- subset(as.data.frame(dat1$EDU, header = T))
     edu <- `colnames<-`(edu, "Education")

#subset political view
pol <- subset(as.data.frame(dat1$POL, header = T))
     pol <- `colnames<-`(pol, "Political Affliation")
     
#create a function that calcuates the percentage for each factor level
percent <- function(x, y) {
      x <- table(x) #provides the number of individuals in each factor - list
      y <- 381 #total number of observations
      return(x/y)
}

list.edu <- sapply(edu, percent) #table of education by percentage
list.pol <- sapply(pol, percent) #table of political affli. by percentage


#move to the original dataset
dat1$POL <- dat1$EDU <- NULL #get rid of the extra Edu column in original dataset

#need to recode factor strings into numbers for entire dataset
#SQL helps when responses have different anchors but not perinent here
unfactorise <- function(x) {
      case_when(x %in% c("Strongly disagree") ~ 1,
                x %in% c("Somewhat disagree") ~ 2,
                x %in% c("Neither agree nor disagree") ~ 3,
                x %in% c("Somewhat agree") ~ 4,
                x %in% c("Strongly agree") ~ 5)
}

new.dat <- data.frame(sapply(dat1, unfactorise)) #unfactorise dataset 
     sum(is.na(new.dat)) #no missing values!

#Recode appropriate indicators
recoded.dat <- Recode(c(HUM2_R, HUM3_R, HUM6_R, HUM7_R, HUM8_R, HUM9_R, HUM11_R,
                    HUM13_R, HUM14_R), old = 1:5, new = 5:1, data = new.dat)

#cleaned dataset with recoded values and ready for analysis
dat2 <- recoded.dat
rm(d, recoded.dat, new.dat) #keep a (semi) clean space

##Subset constructs##
#Note to self, figure out how to use grep function!
dat.HUM <- dat2[, c(2,3,7:9,11,14:16, 19, 24, 27:29, 31:32)]
dat.SA <- dat2[, c(1,4,5,10,13,17,18,21:23,26)]
dat.LS <- dat2[, c(6,12,20,25,30)]

################################################################################
#######                  Exploratory Factor Analysis                   ########
################################################################################
library(GPArotation)
#library(ltm)

#conduct a parallel analysis to obtain a scree plot
para.HUM <- fa.parallel(dat.HUM) #only items wanted for factor analysis
     para.HUM #look at that!

##Note to self, eigenvalues for EFA will generally be lower than PCA because the
##latter places a 1 in the diagonal, while the former creates communality estimates

para.HUM$fa.values #EFA eigenvalues
summary(para.HUM)
#para.HUM$pc.values is for PCA eigenvalues

#request oblique rotation for 1 factor model
obliq1 <- fa(dat.HUM,nfactors=1,rotate="oblimin",fm="pa")
     print(obliq1, sort=TRUE) #26% of the variance is explained >:(
     obliq1$values  #another method to call for eigenvalues to report from EFA
     obliq1$loadings

#request oblique rotation for 2 factor model - though unnecessary
obliq2 <- fa(dat.HUM,nfactors=2,rotate="oblimin",fm="pa")
     print(obliq2, sort=TRUE)
     #two factors most likely explained by negative correlation of reversed items
     obliq2$values  # this is the EFA eigenvalues
     obliq2$loadings


#draws diagrams of factor structure and loadings
fa.diagram(obliq1, cut = .25)
fa.diagram(obliq2, cut = .25)


factor.congruence(list(obliq1$loadings,obliq2$loadings)) #compare the conguent statistics
round(cor(obliq1$loadings, obliq2$loadings),2) #last number is the number of decimals

################################################################################
########                  Internal Consistency                      ############
################################################################################

alpha(dat.HUM[, -c(4,5,12)], check.keys = T) #.85 with removed items
#not sure why check.keys has to be used even after reverse coding?
alpha(dat.SA) #.88 cronbach alpha
alpha(dat.LS) # .89 cronbach alpha


################################################################################
########                  Descriptive Statistics                      ##########
################################################################################

#get rid of items HUM5 (.14) and HUM12 (.15) factor loadings
SS.HUM <- rowMeans(subset(dat.HUM, select = -c(HUM5, HUM12)))
SS.SA <- rowMeans(dat.SA)
SS.LS <- rowMeans(dat.LS)

#dataframe consisting of all scale scores
ScaleScores <- cbind.data.frame(SS.HUM, SS.SA, SS.LS)
     SS.mean_sd <- describe(ScaleScores) #needed for mean and sd
     
ScaleScores.corr <- rcorr(as.matrix(ScaleScores))
DescriptiveStats <- cbind(SS.mean_sd$mean, SS.mean_sd$sd, ScaleScores.corr$r)
     DescriptiveStats <- `colnames<-`(DescriptiveStats, c("Mean", "SD", "HUM",
                                                          "SA", "LS"))
#write out the correlation table
write.csv(DescriptiveStats, 
          "~/Dropbox/Graduate Courses/Psychometrics/Final Project/Project Data Files/Construct Correlations.csv")

#clean up everything
rm(list = ls())