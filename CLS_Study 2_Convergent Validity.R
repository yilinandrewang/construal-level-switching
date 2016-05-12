### Test ####
# Set working directory
cvg <- read.csv("cvg.csv", TRUE, ",")
names(cvg)
library(lavaan)
library(ggplot2)

## Switching Cost (SC) ####

cvg$stroop_sc_1 <- (cvg$stroop_incongruent1 - cvg$stroop_congruent1)/cvg$stroop_allsd1
cvg$stroop_sc_2 <- (cvg$stroop_incongruent2 - cvg$stroop_congruent2)/cvg$stroop_allsd2
cvg$catexe_sc_1 <- (cvg$catexe_Switch1 - cvg$catexe_NoSwitch1)/cvg$catexe_allsd1
cvg$catexe_sc_2 <- (cvg$catexe_Switch2 - cvg$catexe_NoSwitch2)/cvg$catexe_allsd2
cvg$navon_sc_1  <- (cvg$navon_Switch1 - cvg$navon_NoSwitch1)/cvg$navon_allsd1
cvg$navon_sc_2  <- (cvg$navon_Switch2 - cvg$navon_NoSwitch2)/cvg$navon_allsd2
cor(subset(cvg[,36:41]), use="pairwise.complete.obs")
total <- subset(cvg[,36:41])

# Plot the distributions
qplot(x=c(cvg$stroop_sc_1,cvg$stroop_sc_2,cvg$catexe_sc_1,cvg$catexe_sc_2,cvg$navon_sc_1,cvg$navon_sc_2),
      data=stack(total),geom="density",xlim=c(-1.5,1.5), fill=ind, 
      alpha=I(.5),xlab="Standardized Scores", main="Distributions of Switching Asymmetry and Delay")

# Model 1 ####
HS.model_sc1 <- 'catexe_navon_sc =~ catexe_sc_1+catexe_sc_2+navon_sc_1+navon_sc_2'
fit1 <- cfa(HS.model_sc1, data = cvg)
summary(fit1, fit.measures = TRUE)

# Model 2 ####
HS.model_sc2 <- '
catexe_sc =~ catexe_sc_1+catexe_sc_2
navon_sc =~ navon_sc_1+navon_sc_2'
fit2 <- cfa(HS.model_sc2, data = cvg)
summary(fit2, fit.measures = TRUE)

# Model 3 ####
HS.model_sc3 <- '
catexe_sc =~ catexe_sc_1+catexe_sc_2
navon_sc =~ navon_sc_1+navon_sc_2
stroop_sc =~ stroop_sc_1+stroop_sc_2
'
fit3 <- cfa(HS.model_sc3, data = cvg)
summary(fit3, fit.measures = TRUE)

# Model 4a ####
HS.model_sc4a <- '
catexe_navon =~ navon_sc_1+catexe_sc_1+catexe_sc_2+navon_sc_2
stroop_sc =~ stroop_sc_1+stroop_sc_2
gen =~ catexe_navon + stroop_sc
'
fit4a <- cfa(HS.model_sc4a, data = cvg, std.lv=FALSE)
summary(fit4a, fit.measures = TRUE)

# Model 4b ####
HS.model_sc4b <- '
catexe_navon =~ navon_sc_1+catexe_sc_1+catexe_sc_2+navon_sc_2
stroop_sc =~ stroop_sc_1+stroop_sc_2
'
fit4b <- cfa(HS.model_sc4b, data = cvg, std.lv=TRUE)
summary(fit4b, fit.measures = TRUE)

# Model 5 ####
HS.model_sc5 <- '
catexe_navon_stroop =~ stroop_sc_1+catexe_sc_1+catexe_sc_2+navon_sc_1+navon_sc_2+stroop_sc_2'
fit5 <- cfa(HS.model_sc5, data = cvg)
summary(fit5, fit.measures = TRUE)


## Switching Asymmetry (SA) ####
cvg$navon_sa_1 <- ((cvg$navon_HiLo1-cvg$navon_LoHi1)-(cvg$navon_LoLo1-cvg$navon_HiHi1))/cvg$navon_allsd1
cvg$navon_sa_2 <- ((cvg$navon_HiLo2-cvg$navon_LoHi2)-(cvg$navon_LoLo2-cvg$navon_HiHi2))/cvg$navon_allsd2
cvg$catexe_sa_1 <- ((cvg$catexe_HiLo1-cvg$catexe_LoHi1)-(cvg$catexe_LoLo1-cvg$catexe_HiHi1))/cvg$catexe_allsd1
cvg$catexe_sa_2 <- ((cvg$catexe_HiLo2-cvg$catexe_LoHi2)-(cvg$catexe_LoLo2-cvg$catexe_HiHi2))/cvg$catexe_allsd2
total2 <- subset(cvg[,42:45])
qplot(x=c(cvg$navon_sa_1,cvg$navon_sa_2,cvg$catexe_sa_1,cvg$catexe_sa_2),
      data=stack(total2),geom="density",xlim=c(-1.5,1.5), fill=ind,
      alpha=I(.5),xlab="Standardized Scores", main="Distributions of Switching Asymmetry and Delay")
cor(subset(cvg[,42:45]), use="pairwise.complete.obs")

# Model 6 ####
HS.model_sa1 <- 'catexe_navon_sa =~ catexe_sa_2 + catexe_sa_1 + navon_sa_1 + navon_sa_2'
fit6 <- cfa(HS.model_sa1, data = cvg)
summary(fit6, fit.measures = TRUE)

# Model 7 ####
HS.model_sa2 <- '
catexe_sa =~ catexe_sa_1+catexe_sa_2
navon_sa =~ navon_sa_1+navon_sa_2'
fit7 <- cfa(HS.model_sa2, data = cvg)
summary(fit7, fit.measures = TRUE)


# LAVAAN Example ####
example(cfa)
HS.model <- 'visual =~x1+x2+x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9'
fit <- cfa(HS.model, data = HolzingerSwineford1939)
summary(fit, fit.measures = TRUE)
View(HolzingerSwineford1939)

### Merge Data ####

# Stroop

setwd("/Users/andrewang/Documents/iLibrary/UC DAVIS/My Research/Construal Level Switching/Study 2/Data/Formatted Data/Main/stroop")
library(base)
data <- Sys.glob("*.csv")
for (fileName in data) {
  dat <- read.csv(fileName, TRUE, ",")
  dat <- dat[-c(1:10,35),] # Delete instruction and practice RTs
  dat$cong <- "incongruent" # Identify whether stimulus is congruent or not
  dat$cong[c(1:3, 8:10, 15:17, 22:27, 32:34, 39:41, 46:48)] <- "congruent"
  dat <- dat[order(dat$Order),] # Sort by the order in which participants saw stimuli
  dat <- dat[!dat$RT %in% boxplot.stats(dat$RT)$out,] # Exclude outliers based on boxplot
  
  # create new data based on contents of original file:
  stroopnew <- data.frame(
    File = fileName,
    congruent1 <- mean(dat$RT[which(dat$cong=="congruent" & dat$Order%%2==0)]),
    congruent2 <- mean(dat$RT[which(dat$cong=="congruent" & dat$Order%%2==1)]),
    incongruent1 <- mean(dat$RT[which(dat$cong=="incongruent" & dat$Order%%2==0)]),
    incongruent2 <- mean(dat$RT[which(dat$cong=="incongruent" & dat$Order%%2==1)]),
    allsd1 <- sd(dat$RT[dat$Order%%2==0]),
    allsd2 <- sd(dat$RT[dat$Order%%2==1])
  )
  # write new data to separate file:
  write.table(stroopnew,
              "stroopnew.csv",
              append = TRUE,
              sep = ",",
              row.names = FALSE,
              col.names = FALSE)}

# Navon
setwd("/Users/andrewang/Documents/iLibrary/UC DAVIS/My Research/Construal Level Switching/Study 2/Data/Formatted Data/Main/navon")
library(base)
data <- Sys.glob("*.csv")
for (fileName in data) {
  dat <- read.csv(fileName, TRUE, ",")
  dat <- dat[-c(57,114),] # Delete instruction RTs
  dat$level <- "Low" # Identify target level of the stimulus on the current trial
  dat$level[which(dat$Stim==c("HF.bmp","HT.bmp","LF.bmp","LT.bmp"))] <- "High"
  dat <- dat[order(dat$Order),] # Sort by the order in which participants saw stimuli
  levelpre <- c(NA, dat$level) # Identify target level of the stimulus from the trial before
  length(levelpre) <- 112
  dat$levelpre <- c(levelpre)
  dat$match <- rep("Different",112)
  dat$match[which(dat$level==dat$levelpre)] <- "Same"
  dat <- dat[-c(1:8,57:64),] # Delete practice trials (first eight in each 56-trial block)
  dat <- dat[!dat$RT %in% boxplot.stats(dat$RT)$out,] # Exclude outliers based on boxplot
  dat <- dat[which(dat$Correct=="True"),] # Exclude incorrect responses
  
  # create new data based on contents of original file:
  allRT <- data.frame(
    File = fileName,
    HiLo1 = mean(dat$RT[which(dat$level=="Low" & dat$levelpre=="High" & dat$Order%%2==0)]),
    LoHi1 = mean(dat$RT[which(dat$level=="High" & dat$levelpre=="Low" & dat$Order%%2==0)]),
    LoLo1 = mean(dat$RT[which(dat$level=="Low" & dat$levelpre=="Low" & dat$Order%%2==0)]),
    HiHi1 = mean(dat$RT[which(dat$level=="High" & dat$levelpre=="High" & dat$Order%%2==0)]),
    HiLo2 = mean(dat$RT[which(dat$level=="Low" & dat$levelpre=="High" & dat$Order%%2==1)]),
    LoHi2 = mean(dat$RT[which(dat$level=="High" & dat$levelpre=="Low" & dat$Order%%2==1)]),
    LoLo2 = mean(dat$RT[which(dat$level=="Low" & dat$levelpre=="Low" & dat$Order%%2==1)]),
    HiHi2 = mean(dat$RT[which(dat$level=="High" & dat$levelpre=="High" & dat$Order%%2==1)]),
    NoSwitch1 = mean(dat$RT[which(dat$level==dat$levelpre & dat$Order%%2==0)]),
    Switch1 = mean(dat$RT[which(dat$level!=dat$levelpre & dat$Order%%2==0)]),
    NoSwitch2 = mean(dat$RT[which(dat$level==dat$levelpre & dat$Order%%2==1)]),
    Switch2 = mean(dat$RT[which(dat$level!=dat$levelpre & dat$Order%%2==1)]),
    allsd1 = sd(dat$RT[dat$Order%%2==0]),
    allsd2 = sd(dat$RT[dat$Order%%2==1])
  )
  # write new data to separate file:
  write.table(allRT,
              "navonnew.csv",
              append = TRUE,
              sep = ",",
              row.names = FALSE,
              col.names = FALSE)}

# Catexe
setwd("/Users/andrewang/Documents/iLibrary/UC DAVIS/My Research/Construal Level Switching/Study 2/Data/Formatted Data/Main/cat-exe")
catexe <- read.csv("catexe.csv", TRUE, ",")
for(x in 1:200){ # N = 200
  # For each SubjectID: timing data are variables 126:159, 163:168, 169:202, 206:211
  RT <- stack(catexe[x,c(126:159, 163:168, 169:202, 206:211)])
  RT$ind <- substring(RT$ind,2)
  RT$SubjectID <- as.factor(rep(catexe$Subject[x],80))
  RT$Randomized <- t(catexe[x,c(221:300)])
  RT$Order <- match(RT$ind,RT$Randomized)
  RT <- RT[,c(3,4,2,1,5)]
  RT <- RT[order(RT$Order),]
  RT$level <- substring(RT$ind,1,3)
  levelpre <- c(NA, RT$level)
  length(levelpre) <- 80
  RT$levelpre <- c(levelpre)
  RT$match <- rep("Different",80)
  RT$match[which(RT$level==RT$levelpre)] <- "Same"
  
  allRT <- data.frame(
    SubjectID = catexe$Subject[x],
    HiLo1 = mean(RT$values[which(RT$level=="exa" & RT$levelpre=="cat" & RT$Order%%2==0)]),
    LoHi1 = mean(RT$values[which(RT$level=="cat" & RT$levelpre=="exa" & RT$Order%%2==0)]),
    LoLo1 = mean(RT$values[which(RT$level=="exa" & RT$levelpre=="exa" & RT$Order%%2==0)]),
    HiHi1 = mean(RT$values[which(RT$level=="cat" & RT$levelpre=="cat" & RT$Order%%2==0)]),
    NoSwitch1 = mean(RT$values[which(RT$level==RT$levelpre & RT$Order%%2==0)]),
    Switch1 = mean(RT$values[which(RT$level!=RT$levelpre & RT$Order%%2==0)]),
    allsd1 = sd(RT$values[which(RT$Order%%2==0)]),
    HiLo2 = mean(RT$values[which(RT$level=="exa" & RT$levelpre=="cat" & RT$Order%%2==1)]),
    LoHi2 = mean(RT$values[which(RT$level=="cat" & RT$levelpre=="exa" & RT$Order%%2==1)]),
    LoLo2 = mean(RT$values[which(RT$level=="exa" & RT$levelpre=="exa" & RT$Order%%2==1)]),
    HiHi2 = mean(RT$values[which(RT$level=="cat" & RT$levelpre=="cat" & RT$Order%%2==1)]),
    NoSwitch2 = mean(RT$values[which(RT$level==RT$levelpre & RT$Order%%2==1)]),
    Switch2 = mean(RT$values[which(RT$level!=RT$levelpre & RT$Order%%2==1)]),
    allsd2 = sd(RT$values[which(RT$Order%%2==1)])
  )
  
  write.table(allRT, 
              "catexenew.csv",
              append = TRUE,
              sep = ",",
              row.names = FALSE,
              col.names = FALSE)
}