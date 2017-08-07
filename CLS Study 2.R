# Construal Level Switching Study 2
# Last updated: 08/06/17

#### Prep: Navon ####

# Set working directory to data folder
setwd(paste("/Users/andrewang/Documents/iLibrary/UC DAVIS/My Research/",
            "Construal Level Switching/Study 2/Data/Formatted Data/Main/navon",
            sep = ""))

# Pull each participant's data file from the folder
data <- Sys.glob("*.csv")

# Set a dataframe for storing data
navon <- as.data.frame(matrix(NA, nrow = 0, ncol = 0))

# Loop
for (i in data) {
  dat <- read.csv(i, TRUE, ",")
  
  # 1. Delete instructions
  dat <- dat[-c(57, 114), ]
  
  # 2. Identify target level of the stimulus on the current trial
  dat$level <- "Low"
  dat$level[which(dat$Stim == 
                    c("HF.bmp", "HT.bmp", "LF.bmp", "LT.bmp"))] <- "High"
  
  # 3. Sort trials by the order in which participants saw them
  dat <- dat[order(dat$Order), ]
  
  # 4. Identify what the level of the stimulus on the previous trial is on
  levelpre <- c(NA, dat$level)
  length(levelpre) <- dim(dat)[1]
  dat$levelpre <- c(levelpre)
  
  # 5. Delete practice trials: first 8 (# of stim) in each 56-trial block
  dat <- dat[-c(1:8, 57:64), ]
  
  # 6. Calculate error rate
  error <- mean(dat$Correct == "False")
  
  # 7. Exclude outlier trials (RT) within each participant's data (> +-2 SD)
  outl <- length(which(abs(dat$RT - mean(dat$RT))/sd(dat$RT) >= 2))/96
  dat <- dat[-which(abs(dat$RT - mean(dat$RT))/sd(dat$RT) >= 2), ]
  
  # 8. Exclude the rest of the trials with incorrect responses
  dat <- dat[which(dat$Correct == "True"), ]
  
  # 9. Parcel
  if (dim(dat)[1]%%4 == 0) {dat$parID <- rep(1:4, dim(dat)[1]/4)
  } else {dat$parID <- c(rep(1:4, dim(dat)[1]/4), 1:(dim(dat)[1]%%4))}
  
  # 10. Calculate mean RTs
  navon[i, "SubjectID"] <- substring(i, 1, nchar(i)-4) # Participant ID
  
  # Overall
  navon[i, "n_HiLo"] <- mean(dat$RT[which(dat$levelpre == "High" & 
                                            dat$level == "Low")])
  navon[i, "n_LoHi"] <- mean(dat$RT[which(dat$levelpre == "Low" & 
                                            dat$level == "High")])
  navon[i, "n_LoLo"] <- mean(dat$RT[which(dat$levelpre == "Low" & 
                                            dat$level == "Low")])
  navon[i, "n_HiHi"] <- mean(dat$RT[which(dat$levelpre == "High" &
                                            dat$level == "High")])
  navon[i, "n_NoSwitch"] <- mean(dat$RT[which(dat$level == dat$levelpre)])
  navon[i, "n_Switch"] <- mean(dat$RT[which(dat$level != dat$levelpre)])
  navon[i, "n_Error"] <- error
  navon[i, "n_outl"] <- outl
  navon[i, "n_allsd"] <- sd(dat$RT)
  
  # Parcel 1
  navon[i, "n_HiLo_p1"] <- mean(dat$RT[which(
    dat$levelpre == "High" & dat$level == "Low" & dat$parID == 1)])
  navon[i, "n_LoHi_p1"] <- mean(dat$RT[which(
    dat$levelpre == "Low" & dat$level == "High" & dat$parID == 1)])
  navon[i, "n_LoLo_p1"] <- mean(dat$RT[which(
    dat$levelpre == "Low" & dat$level == "Low" & dat$parID == 1)])
  navon[i, "n_HiHi_p1"] <- mean(dat$RT[which(
    dat$levelpre == "High" & dat$level == "High" & dat$parID == 1)])
  navon[i, "n_NoSwitch_p1"] <- mean(dat$RT[which(
    dat$level == dat$levelpre & dat$parID == 1)])
  navon[i, "n_Switch_p1"] <- mean(dat$RT[which(
    dat$level != dat$levelpre & dat$parID == 1)])
  navon[i, "n_allsd_p1"] <- sd(dat$RT[which(dat$parID == 1)])
  
  # Parcel 2
  navon[i, "n_HiLo_p2"] <- mean(dat$RT[which(
    dat$levelpre == "High" & dat$level == "Low" & dat$parID == 2)])
  navon[i, "n_LoHi_p2"] <- mean(dat$RT[which(
    dat$levelpre == "Low" & dat$level == "High" & dat$parID == 2)])
  navon[i, "n_LoLo_p2"] <- mean(dat$RT[which(
    dat$levelpre == "Low" & dat$level == "Low" & dat$parID == 2)])
  navon[i, "n_HiHi_p2"] <- mean(dat$RT[which(
    dat$levelpre == "High" & dat$level == "High" & dat$parID == 2)])
  navon[i, "n_NoSwitch_p2"] <- mean(dat$RT[which(
    dat$level == dat$levelpre & dat$parID == 2)])
  navon[i, "n_Switch_p2"] <- mean(dat$RT[which(
    dat$level != dat$levelpre & dat$parID == 2)])
  navon[i, "n_allsd_p2"] <- sd(dat$RT[which(dat$parID == 2)])
  
  # Parcel 3
  navon[i, "n_HiLo_p3"] <- mean(dat$RT[which(
    dat$levelpre == "High" & dat$level == "Low" & dat$parID == 3)])
  navon[i, "n_LoHi_p3"] <- mean(dat$RT[which(
    dat$levelpre == "Low" & dat$level == "High" & dat$parID == 3)])
  navon[i, "n_LoLo_p3"] <- mean(dat$RT[which(
    dat$levelpre == "Low" & dat$level == "Low" & dat$parID == 3)])
  navon[i, "n_HiHi_p3"] <- mean(dat$RT[which(
    dat$levelpre == "High" & dat$level == "High" & dat$parID == 3)])
  navon[i, "n_NoSwitch_p3"] <- mean(dat$RT[which(
    dat$level == dat$levelpre & dat$parID == 3)])
  navon[i, "n_Switch_p3"] <- mean(dat$RT[which(
    dat$level != dat$levelpre & dat$parID == 3)])
  navon[i, "n_allsd_p3"] <- sd(dat$RT[which(dat$parID == 3)])
  
  # Parcel 4
  navon[i, "n_HiLo_p4"] <- mean(dat$RT[which(
    dat$levelpre == "High" & dat$level == "Low" & dat$parID == 4)])
  navon[i, "n_LoHi_p4"] <- mean(dat$RT[which(
    dat$levelpre == "Low" & dat$level == "High" & dat$parID == 4)])
  navon[i, "n_LoLo_p4"] <- mean(dat$RT[which(
    dat$levelpre == "Low" & dat$level == "Low" & dat$parID == 4)])
  navon[i, "n_HiHi_p4"] <- mean(dat$RT[which(
    dat$levelpre == "High" & dat$level == "High" & dat$parID == 4)])
  navon[i, "n_NoSwitch_p4"] <- mean(dat$RT[which(
    dat$level == dat$levelpre & dat$parID == 4)])
  navon[i, "n_Switch_p4"] <- mean(dat$RT[which(
    dat$level != dat$levelpre & dat$parID == 4)])
  navon[i, "n_allsd_p4"] <- sd(dat$RT[which(dat$parID == 4)])
}
rownames(navon) <- 1:length(data)

# 10. Visually check for outliers: Across participants

# Histograms
par(mfrow = c(2, 2))
hist(navon$n_HiLo, breaks = 100); hist(navon$n_LoHi, breaks = 100)
hist(navon$n_LoLo, breaks = 100); hist(navon$n_HiHi, breaks = 100)

# Boxplots
library(car)
par(mfrow = c(1, 4))
Boxplot(navon$n_HiLo); Boxplot(navon$n_LoHi) # Note that the IDs are just row #
Boxplot(navon$n_LoLo); Boxplot(navon$n_HiHi)

# Which rows contain the outliers? (Note that row number != Subject ID)
which(navon$n_HiLo < 500) # Rows 28 131 161
which(navon$n_LoHi < 500) # Rows 28 131 161
which(navon$n_LoLo < 400) # Rows 28
which(navon$n_HiHi < 400) # Rows 161
# We exclude Rows 28 131 161

which(navon$n_HiLo > 1500) # Rows 106 190
which(navon$n_LoHi > 1500) # Rows 106 190
which(navon$n_LoLo > 1300) # Rows 106 150 157 190
which(navon$n_HiHi > 1200) # Rows 81 106 157
# We exclude Rows 106 190 157

# Create n_Exclude variable
navon$n_Exclude <- 0
navon$n_Exclude[c(28, 131, 161, 106, 190, 157)] <- 1

# 11. Exclude participants with error rate > 20%
par(mfrow = c(1, 1))
hist(navon$n_Error, breaks = 300)
navon$n_Exclude[which(navon$n_Error > 0.2)] <- 2

mean(navon$n_Error) # Overall error rate 6%
mean(navon$n_outl) # Overall outlier rate 5%
length(which(navon$n_Error > 0.2)) # n = 12

#### Level-Switching Cost: Navon ####

navon1 <- navon[which(navon$n_Exclude == 0), 1:5]
navontest <- stack(navon1, select = c(n_HiLo, n_LoHi, n_LoLo, n_HiHi))
navontest <- cbind(rep(navon1$SubjectID, 4), navontest)
navontest$current <- as.factor(substring(navontest$ind, 5))
navontest$switch <- as.factor(c(rep("Switch", length(navon1$SubjectID) * 2), 
                                rep("NoSwitch", length(navon1$SubjectID) * 2)))
colnames(navontest)[1:3] <- c("SubjectID", "RT", "condition")

library(ez)
# no missing data
ezDesign(data = navontest, y = SubjectID, x = current, col = switch)
ezANOVA(data = navontest, dv = RT, wid = SubjectID, within = .(current, switch),
        type = 3)
round(mean(navontest$RT[which(navontest$current == "Hi")]), 0)
round(sd(navontest$RT[which(navontest$current == "Hi")]), 0)
round(mean(navontest$RT[which(navontest$current == "Lo")]), 0)
round(sd(navontest$RT[which(navontest$current == "Lo")]), 0)

round(mean(navontest$RT[which(navontest$switch == "Switch")]), 0)
round(sd(navontest$RT[which(navontest$switch == "Switch")]), 0)
round(mean(navontest$RT[which(navontest$switch == "NoSwitch")]), 0)
round(sd(navontest$RT[which(navontest$switch == "NoSwitch")]), 0)

#### Prep: Catexe ####

# Set working directory to data folder
setwd(paste("/Users/andrewang/Documents/iLibrary/UC DAVIS/My Research/",
            "Construal Level Switching/Study 2/Data/Formatted Data/Main/cat-exe",
            sep = ""))

# Read compiled data file (see Study 2 Notes)
catexe <- read.csv("catexe.csv", TRUE, ",")
names(catexe)

# Set a dataframe for storing data
ce <- as.data.frame(matrix(NA, nrow = 0, ncol = 0))

# Loop
for(i in 1:dim(catexe)[1]) {
  
  # 1. Transpose timing data into long form: 126:159, 163:168, 169:202, 206:211
  dat <- stack(catexe[i, c(126:159, 163:168, 
                           169:202, 206:211)]) # Transpose data
  dat$SubjectID <- as.factor(rep(catexe$Subject[i], 80)) # Attach subject ID
  dat$ind <- substring(dat$ind, 2) # Attach stimulus name
  
  # 2. Identify the order in which a participant saw the trials
  dat$Randomized <- t(catexe[i, c(221:300)]) # Load order of stimuli
  dat$Order <- match(dat$ind, dat$Randomized) # Assign order value to each trial
  dat <- dat[, c(3, 4, 2, 1, 5)] # Re-order columns
  
  # 3. Identify target level of the stimulus on the current trial
  dat$level <- substring(dat$ind, 1, 3)
  
  # 4. Sort timing data by the order of trials
  dat <- dat[order(dat$Order), ]
  
  # 5. Identify what the level of the stimulus on the previous trial is on
  levelpre <- c(NA, dat$level)
  length(levelpre) <- 80
  dat$levelpre <- c(levelpre)
  
  # 6. Delete practice trials: first 8 (# of stim) in each 40-trial block
  dat <- dat[-c(1:8, 41:48), ]
  
  # 7. Exclude outlier trials (RT) within each participant's data (> +-2 SD)
  outl <- length(which(abs(dat$values - 
                             mean(dat$values))/sd(dat$values) >= 2))/64
  dat <- dat[-which(abs(dat$values - mean(dat$values))/sd(dat$values) >= 2), ]
  
  # 8. Parcel
  if (dim(dat)[1]%%4 == 0) {dat$parID <- rep(1:4, dim(dat)[1]/4)
  } else {dat$parID <- c(rep(1:4, dim(dat)[1]/4), 1:(dim(dat)[1]%%4))}
  
  # 9. Calculate mean dats
  ce[i, "SubjectID"] <- catexe$Subject[i]
  
  # Overall
  ce[i, "c_HiLo"] <- mean(dat$values[which(dat$levelpre == "cat" &
                                             dat$level == "exa")])
  ce[i, "c_LoHi"] <- mean(dat$values[which(dat$levelpre == "exa" &
                                             dat$level == "cat")])
  ce[i, "c_LoLo"] <- mean(dat$values[which(dat$levelpre == "exa" &
                                             dat$level == "exa")])
  ce[i, "c_HiHi"] <- mean(dat$values[which(dat$levelpre == "cat" &
                                             dat$level == "cat")])
  ce[i, "c_NoSwitch"] <- mean(dat$values[which(dat$level == dat$levelpre)])
  ce[i, "c_Switch"] <- mean(dat$values[which(dat$level != dat$levelpre)])
  ce[i, "c_allsd"] <- sd(dat$values)
  ce[i, "c_outl"] <- outl
  
  # Parcel 1
  ce[i, "c_HiLo_p1"] <- mean(dat$values[which(
    dat$levelpre == "cat" & dat$level == "exa" & dat$parID == 1)])
  ce[i, "c_LoHi_p1"] <- mean(dat$values[which(
    dat$levelpre == "exa" & dat$level == "cat" & dat$parID == 1)])
  ce[i, "c_LoLo_p1"] <- mean(dat$values[which(
    dat$levelpre == "exa" & dat$level == "exa" & dat$parID == 1)])
  ce[i, "c_HiHi_p1"] <- mean(dat$values[which(
    dat$levelpre == "cat" & dat$level == "cat" & dat$parID == 1)])
  ce[i, "c_NoSwitch_p1"] <- mean(dat$values[which(
    dat$level == dat$levelpre & dat$parID == 1)])
  ce[i, "c_Switch_p1"] <- mean(dat$values[which(
    dat$level != dat$levelpre & dat$parID == 1)])
  ce[i, "c_allsd_p1"] <- sd(dat$values[which(dat$parID == 1)])
  
  # Parcel 2
  ce[i, "c_HiLo_p2"] <- mean(dat$values[which(
    dat$levelpre == "cat" & dat$level == "exa" & dat$parID == 2)])
  ce[i, "c_LoHi_p2"] <- mean(dat$values[which(
    dat$levelpre == "exa" & dat$level == "cat" & dat$parID == 2)])
  ce[i, "c_LoLo_p2"] <- mean(dat$values[which(
    dat$levelpre == "exa" & dat$level == "exa" & dat$parID == 2)])
  ce[i, "c_HiHi_p2"] <- mean(dat$values[which(
    dat$levelpre == "cat" & dat$level == "cat" & dat$parID == 2)])
  ce[i, "c_NoSwitch_p2"] <- mean(dat$values[which(
    dat$level == dat$levelpre & dat$parID == 2)])
  ce[i, "c_Switch_p2"] <- mean(dat$values[which(
    dat$level != dat$levelpre & dat$parID == 2)])
  ce[i, "c_allsd_p2"] <- sd(dat$values[which(dat$parID == 2)])
  
  # Parcel 3
  ce[i, "c_HiLo_p3"] <- mean(dat$values[which(
    dat$levelpre == "cat" & dat$level == "exa" & dat$parID == 3)])
  ce[i, "c_LoHi_p3"] <- mean(dat$values[which(
    dat$levelpre == "exa" & dat$level == "cat" & dat$parID == 3)])
  ce[i, "c_LoLo_p3"] <- mean(dat$values[which(
    dat$levelpre == "exa" & dat$level == "exa" & dat$parID == 3)])
  ce[i, "c_HiHi_p3"] <- mean(dat$values[which(
    dat$levelpre == "cat" & dat$level == "cat" & dat$parID == 3)])
  ce[i, "c_NoSwitch_p3"] <- mean(dat$values[which(
    dat$level == dat$levelpre & dat$parID == 3)])
  ce[i, "c_Switch_p3"] <- mean(dat$values[which(
    dat$level != dat$levelpre & dat$parID == 3)])
  ce[i, "c_allsd_p3"] <- sd(dat$values[which(dat$parID == 3)])
  
  # Parcel 4
  ce[i, "c_HiLo_p4"] <- mean(dat$values[which(
    dat$levelpre == "cat" & dat$level == "exa" & dat$parID == 4)])
  ce[i, "c_LoHi_p4"] <- mean(dat$values[which(
    dat$levelpre == "exa" & dat$level == "cat" & dat$parID == 4)])
  ce[i, "c_LoLo_p4"] <- mean(dat$values[which(
    dat$levelpre == "exa" & dat$level == "exa" & dat$parID == 4)])
  ce[i, "c_HiHi_p4"] <- mean(dat$values[which(
    dat$levelpre == "cat" & dat$level == "cat" & dat$parID == 4)])
  ce[i, "c_NoSwitch_p4"] <- mean(dat$values[which(
    dat$level == dat$levelpre & dat$parID == 4)])
  ce[i, "c_Switch_p4"] <- mean(dat$values[which(
    dat$level != dat$levelpre & dat$parID == 4)])
  ce[i, "c_allsd_p4"] <- sd(dat$values[which(dat$parID == 4)])
}

# 10. Visually check for outliers: Across participants

# Histograms
par(mfrow = c(2, 2))
hist(ce$c_HiLo, breaks = 200); hist(ce$c_LoHi, breaks = 200)
hist(ce$c_LoLo, breaks = 200); hist(ce$c_HiHi, breaks = 200)

# Boxplots
library(car)
par(mfrow = c(1, 4))
Boxplot(ce$c_HiLo); Boxplot(ce$c_LoHi)
Boxplot(ce$c_LoLo); Boxplot(ce$c_HiHi)

# Which rows contain the outliers? (Note that row number != Subject ID)
which(ce$c_HiLo > 10000) # Rows 92 118
which(ce$c_LoHi > 8000) # Rows 22  91  92  93 109 118 125 137 180 199
which(ce$c_LoLo > 10000) # Rows 92 118
which(ce$c_HiHi > 10000) # Rows 92
# We exclude Rows 92 118

# Create c_Exclude variable
ce$c_Exclude <- 0
ce$c_Exclude[c(92, 118)] <- 1

mean(ce$c_outl) # Overall outlier rate 5%

#### Level-Switching Cost: Catexe ####
catexe1 <- ce[which(ce$c_Exclude == 0), c(1:5)]
catexetest <- stack(catexe1, select = c(c_HiLo, c_LoHi, c_LoLo, c_HiHi))
catexetest <- cbind(rep(catexe1$SubjectID, 4), catexetest)
catexetest$current <- as.factor(substring(catexetest$ind, 5))
catexetest$switch <- as.factor(c(rep("Switch", length(catexe1$SubjectID) * 2), 
                              rep("NoSwitch", length(catexe1$SubjectID) * 2)))
colnames(catexetest)[1:3] <- c("SubjectID", "RT", "condition")

library(ez)
# no missing data
ezDesign(data = catexetest, y = SubjectID, x = current, col = switch)
ezANOVA(data = catexetest, dv = RT, wid = SubjectID, 
        within = .(current, switch), type = 3)
round(mean(catexetest$RT[which(catexetest$current == "Hi")]), 0)
round(sd(catexetest$RT[which(catexetest$current == "Hi")]), 0)
round(mean(catexetest$RT[which(catexetest$current == "Lo")]), 0)
round(sd(catexetest$RT[which(catexetest$current == "Lo")]), 0)

round(mean(catexetest$RT[which(catexetest$switch == "Switch")]), 0)
round(sd(catexetest$RT[which(catexetest$switch == "Switch")]), 0)
round(mean(catexetest$RT[which(catexetest$switch == "NoSwitch")]), 0)
round(sd(catexetest$RT[which(catexetest$switch == "NoSwitch")]), 0)

ezStats(catexetest, dv = RT, wid = SubjectID, within = .(current, switch), 
        type = 3)
ezPlot(catexetest, dv = RT, wid = SubjectID, within = .(current, switch),
       x = switch, split = current, type = 3)


#### Prep: Stroop ####

# Set working directory to data folder
setwd(paste("/Users/andrewang/Documents/iLibrary/UC DAVIS/My Research/",
            "Construal Level Switching/Study 2/Data/Formatted Data/Main/stroop",
            sep = ""))

# Pull each participant's data file from the folder
data <- Sys.glob("*.csv")

# Set a dataframe for storing data
stroop <- as.data.frame(matrix(NA, nrow = 0, ncol = 0))

# Loop
for (i in data) {
  dat <- read.csv(i, TRUE, ",")
  
  # 1. Delete instructions and demo block
  dat <- dat[-c(1:10, 35), ]
  
  # 2. Identify whether stimulus is congruent or not
  dat$cong <- "incongruent"
  dat$cong[which(dat$Stim == "bb.bmp")] <- "congruent"
  dat$cong[which(dat$Stim == "gg.bmp")] <- "congruent"
  dat$cong[which(dat$Stim == "rr.bmp")] <- "congruent"
  dat$cong[which(dat$Stim == "yy.bmp")] <- "congruent"
  
  # 3. Sort trials by the order in which participants saw them
  dat <- dat[order(dat$Order), ]
  
  # 4. Delete practice trials: first 4 (# of stimuli) in each 24-trial block
  dat <- dat[-c(1:4, 25:28), ]
  
  # 5. Calculate overall error rate
  error <- mean(dat$Correct == "False")
  
  # 6. Exclude outlier trials (RT) within each participant's data (> +-2 SD)
  outl <- length(which(abs(dat$RT - mean(dat$RT))/sd(dat$RT) >= 2))/40
  dat <- dat[-which(abs(dat$RT - mean(dat$RT))/sd(dat$RT) >= 2), ]
  
  # 7. Exclude the rest of the trials with incorrect responses
  dat <- dat[which(dat$Correct == "True"), ]
  
  # 8. Parcel
  if (dim(dat)[1]%%4 == 0) {dat$parID <- rep(1:4, dim(dat)[1]/4)
  } else {dat$parID <- c(rep(1:4, dim(dat)[1]/4), 1:(dim(dat)[1]%%4))}
  
  # 9. Calculate mean RTs
  stroop[i, "SubjectID"] <- substring(i, 1, nchar(i)-4)
  
  # Overall
  stroop[i, "s_congruent"] <- mean(dat$RT[which(dat$cong == "congruent")])
  stroop[i, "s_incongruent"] <- mean(dat$RT[which(dat$cong == "incongruent")])
  stroop[i, "s_Error"] <- error
  stroop[i, "s_outl"] <- outl
  stroop[i, "s_allsd"] <- sd(dat$RT)
  
  # Parcel 1
  stroop[i, "s_congruent_p1"] <- mean(dat$RT[which(dat$cong == "congruent" &
                                                     dat$parID == 1)])
  stroop[i, "s_incongruent_p1"] <- mean(dat$RT[which(dat$cong == "incongruent" &
                                                       dat$parID == 1)])
  stroop[i, "s_allsd_p1"] <- sd(dat$RT[which(dat$parID == 1)])
  
  # Parcel 2
  stroop[i, "s_congruent_p2"] <- mean(dat$RT[which(dat$cong == "congruent" &
                                                     dat$parID == 2)])
  stroop[i, "s_incongruent_p2"] <- mean(dat$RT[which(dat$cong == "incongruent" &
                                                       dat$parID == 2)])
  stroop[i, "s_allsd_p2"] <- sd(dat$RT[which(dat$parID == 2)])
  
  # Parcel 3
  stroop[i, "s_congruent_p3"] <- mean(dat$RT[which(dat$cong == "congruent" &
                                                     dat$parID == 3)])
  stroop[i, "s_incongruent_p3"] <- mean(dat$RT[which(dat$cong == "incongruent" &
                                                       dat$parID == 3)])
  stroop[i, "s_allsd_p3"] <- sd(dat$RT[which(dat$parID == 3)])
  
  # Parcel 4
  stroop[i, "s_congruent_p4"] <- mean(dat$RT[which(dat$cong == "congruent" &
                                                     dat$parID == 4)])
  stroop[i, "s_incongruent_p4"] <- mean(dat$RT[which(dat$cong == "incongruent" &
                                                       dat$parID == 4)])
  stroop[i, "s_allsd_p4"] <- sd(dat$RT[which(dat$parID == 4)])
}
rownames(stroop) <- 1:length(data)

# 10. Visually check for outliers: Across participants

# Histograms
par(mfrow = c(2, 1))
hist(stroop$s_congruent, xlim = c(200, 2000), breaks = 200)
hist(stroop$s_incongruent, xlim = c(200, 2000), breaks = 200)

# Boxplots
library(car)
par(mfrow = c(1, 2))
Boxplot(stroop$s_congruent); Boxplot(stroop$s_incongruent)
# Note that the IDs are just row numbers

# Which rows contain the outliers? (Note that row number != Subject ID)
which(stroop$s_congruent < 400) # 155
which(stroop$s_incongruent < 500) # Rows 155 175
# We exclude Rows 155 175

which(stroop$s_congruent > 1500) # Rows 105 156 189
which(stroop$s_incongruent > 1500) # Rows 5  74 105 156 189
# We exclude Rows 105 156 189

# Create s_Exclude variable
stroop$s_Exclude <- 0
stroop$s_Exclude[c(155, 175, 105, 156, 189)] <- 1

# 11. Exclude participants with error rate > 20%
par(mfrow = c(1, 1))
hist(stroop$s_Error, breaks = 300)
stroop$s_Exclude[which(stroop$s_Error > 0.2)] <- 2

mean(stroop$s_Error) # Overall error rate 6%
mean(stroop$s_outl) # Overall outlier rate 5%
length(which(stroop$s_Error > 0.2)) # n = 12

# Stroop interference ####
strooptest <- stroop[which(stroop$s_Exclude == 0), ]
t.test(strooptest$s_incongruent, strooptest$s_congruent, paired = T)
round(mean(strooptest$s_incongruent, na.rm = T))
round(sd(strooptest$s_incongruent, na.rm = T))
round(mean(strooptest$s_congruent, na.rm = T))
round(sd(strooptest$s_congruent, na.rm = T))
library(effsize)
cohen.d(strooptest$s_incongruent, strooptest$s_congruent, na.rm = T, paired = T)


#### Data Merger ####

# Assign NAs to data that should be excluded for each task
colnames(navon)
navon[which(navon$n_Exclude != 0), 2:31] <- NA
colnames(stroop)
stroop[which(stroop$s_Exclude != 0), 2:18] <- NA
colnames(ce)
ce[which(ce$c_Exclude != 0), 2:37] <- NA

# Merge three dataframes into one
cvg <- merge(ce, navon, all.x = TRUE)
cvg <- merge(cvg, stroop, all.x = TRUE)

# Attach participant and exploratory variables
colnames(catexe)[1] <- "SubjectID"
cvg <- merge(cvg, catexe[, c(1, 3, 5, 6, 9:11, 56:60, 62:65)], all.x = TRUE)

# Cleaning
unique(cvg$dAge)
cvg$dAge[which(cvg$dAge == "99")] <- NA
cvg$dAge <- as.numeric(cvg$dAge)

# Gender and Age
length(which(cvg$dGender == 2)) # 167 female
round(mean(cvg$dAge, na.rm = T), 1) # mean age = 19.5
round(sd(cvg$dAge, na.rm = T), 1) # SD age = 1.4
range(cvg$dAge, na.rm = T) # age range 18 - 24

### Convergent and Discriminant Validity Tests ####

# Raw correlations
colnames(cvg)
cvg$s_sc <- (cvg$s_congruent - cvg$s_incongruent)/cvg$s_allsd
cvg$n_sc  <- (cvg$n_Switch - cvg$n_NoSwitch)/cvg$n_allsd
cvg$c_sc  <- (cvg$c_Switch - cvg$c_NoSwitch)/cvg$c_allsd
cor.test(cvg$c_sc, cvg$n_sc)
cor.test(cvg$s_sc, cvg$n_sc)
cor.test(cvg$s_sc, cvg$c_sc)

library(lavaan); library(ggplot2); library(corrplot)
library(lmerTest); library(psych); library(semPlot); library(psy)

# Scores by parcel

# Navon
colnames(navon)
cvg$n_sc_p1  <- (cvg$n_Switch_p1 - cvg$n_NoSwitch_p1)/cvg$n_allsd_p1
cvg$n_sc_p2  <- (cvg$n_Switch_p2 - cvg$n_NoSwitch_p2)/cvg$n_allsd_p2
cvg$n_sc_p3  <- (cvg$n_Switch_p3 - cvg$n_NoSwitch_p3)/cvg$n_allsd_p3
cvg$n_sc_p4  <- (cvg$n_Switch_p4 - cvg$n_NoSwitch_p4)/cvg$n_allsd_p4

# Catexe
cvg$c_sc_p1 <- (cvg$c_Switch_p1 - cvg$c_NoSwitch_p1)/cvg$c_allsd_p1
cvg$c_sc_p2 <- (cvg$c_Switch_p2 - cvg$c_NoSwitch_p2)/cvg$c_allsd_p2
cvg$c_sc_p3 <- (cvg$c_Switch_p3 - cvg$c_NoSwitch_p3)/cvg$c_allsd_p3
cvg$c_sc_p4 <- (cvg$c_Switch_p4 - cvg$c_NoSwitch_p4)/cvg$c_allsd_p4

# Stroop
cvg$s_sc_p1 <- (cvg$s_incongruent_p1 - cvg$s_congruent_p1)/cvg$s_allsd_p1
cvg$s_sc_p2 <- (cvg$s_incongruent_p2 - cvg$s_congruent_p2)/cvg$s_allsd_p2
cvg$s_sc_p3 <- (cvg$s_incongruent_p3 - cvg$s_congruent_p3)/cvg$s_allsd_p3
cvg$s_sc_p4 <- (cvg$s_incongruent_p4 - cvg$s_congruent_p4)/cvg$s_allsd_p4

# Correlation table of measurement scores
colnames(cvg)
corrplot(cor(cvg[, 98:109], use = "pairwise.complete.obs"), method = "square",
         shade.col = NULL, tl.col = "black", tl.srt = 45, tl.cex = 1)
round(cor(cvg[, 98:109], use = "pairwise.complete.obs"), 2)
round(var(cvg[, 98:109], use = "pairwise.complete.obs"), 2)

# Model 1: Original
mod1 <- "
c_sc =~ c_sc_p1 + c_sc_p2 + c_sc_p3 + c_sc_p4
n_sc =~ n_sc_p1 + n_sc_p2 + n_sc_p3 + n_sc_p4
s_sc =~ s_sc_p1 + s_sc_p2 + s_sc_p3 + s_sc_p4
"
fit1 <- cfa(mod1, data = cvg, std.lv = TRUE) # Not converged
