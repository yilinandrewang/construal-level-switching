# Construal Level Switching Study 4
# Last updated: 07/30/17

#### Prep: Navon ####

# Set working directory to data folder
setwd(paste("/Users/andrewang/Documents/iLibrary/UC DAVIS/My Research/",
            "Construal Level Switching/Study 4/Data/Navon",
            sep = ""))

# Pull each participant's data file from the folder
data <- Sys.glob("*.csv")

# Set a dataframe for storing data
navon <- as.data.frame(matrix(NA, nrow = 0, ncol = 0))

# Loop
for (i in data) {
  dat <- read.csv(i, TRUE, ",")
  
  # 1. Delete instructions
  dat <- dat[-c(49, 98, 147, 196), ]
  
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

  # 5. Delete practice trials: first 8 (# of stimuli) in each 48-trial block
  dat <- dat[-c(1:8, 49:56, 97:104, 145:152), ]
  
  # 6. Calculate error rate
  error <- mean(dat$Correct == "False")
  
  # 7. Exclude trials with incorrect responses
  dat <- dat[which(dat$Correct == "True"), ]
  
  # 8. Exclude outlier trials (RT) within each participant's data (> +-2 SD)
  outl <- length(which(abs(dat$RT - mean(dat$RT))/sd(dat$RT) >= 2))/160
  dat <- dat[-which(abs(dat$RT - mean(dat$RT))/sd(dat$RT) >= 2), ]
  
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
  
  # Block 1
  navon[i, "n_HiLo_b1"] <- mean(dat$RT[which(dat$levelpre == "High" & 
                                        dat$level == "Low" & dat$Block == 2)])
  navon[i, "n_LoHi_b1"] <- mean(dat$RT[which(dat$levelpre == "Low" & 
                                        dat$level == "High" & dat$Block == 2)])
  navon[i, "n_LoLo_b1"] <- mean(dat$RT[which(dat$levelpre == "Low" & 
                                        dat$level == "Low" & dat$Block == 2)])
  navon[i, "n_HiHi_b1"] <- mean(dat$RT[which(dat$levelpre == "High" &
                                        dat$level == "High" & dat$Block == 2)])
  navon[i, "n_NoSwitch_b1"] <- mean(dat$RT[which(dat$level == dat$levelpre &
                                            dat$Block == 2)])
  navon[i, "n_Switch_b1"] <- mean(dat$RT[which(dat$level != dat$levelpre & 
                                            dat$Block == 2)])
  navon[i, "n_allsd_b1"] <- sd(dat$RT[which(dat$Block == 2)])
  
  # Block 2
  navon[i, "n_HiLo_b2"] <- mean(dat$RT[which(dat$levelpre == "High" & 
                                        dat$level == "Low" & dat$Block == 4)])
  navon[i, "n_LoHi_b2"] <- mean(dat$RT[which(dat$levelpre == "Low" & 
                                        dat$level == "High" & dat$Block == 4)])
  navon[i, "n_LoLo_b2"] <- mean(dat$RT[which(dat$levelpre == "Low" & 
                                        dat$level == "Low" & dat$Block == 4)])
  navon[i, "n_HiHi_b2"] <- mean(dat$RT[which(dat$levelpre == "High" & 
                                        dat$level == "High" & dat$Block == 4)])
  navon[i, "n_NoSwitch_b2"] <- mean(dat$RT[which(dat$level == dat$levelpre &
                                            dat$Block == 4)])
  navon[i, "n_Switch_b2"] <- mean(dat$RT[which(dat$level != dat$levelpre &
                                            dat$Block == 4)])
  navon[i, "n_allsd_b2"] <- sd(dat$RT[which(dat$Block == 4)])
  
  # Block 3
  navon[i, "n_HiLo_b3"] <- mean(dat$RT[which(dat$levelpre == "High" & 
                                        dat$level == "Low" & dat$Block == 6)])
  navon[i, "n_LoHi_b3"] <- mean(dat$RT[which(dat$levelpre == "Low" & 
                                        dat$level == "High" & dat$Block == 6)])
  navon[i, "n_LoLo_b3"] <- mean(dat$RT[which(dat$levelpre == "Low" & 
                                        dat$level == "Low" & dat$Block == 6)])
  navon[i, "n_HiHi_b3"] <- mean(dat$RT[which(dat$levelpre == "High" & 
                                        dat$level == "High" & dat$Block == 6)])
  navon[i, "n_NoSwitch_b3"] <- mean(dat$RT[which(dat$level == dat$levelpre &
                                            dat$Block == 6)])
  navon[i, "n_Switch_b3"] <- mean(dat$RT[which(dat$level != dat$levelpre &
                                            dat$Block == 6)])
  navon[i, "n_allsd_b3"] <- sd(dat$RT[which(dat$Block == 6)])
  
  # Block 4
  navon[i, "n_HiLo_b4"] <- mean(dat$RT[which(dat$levelpre == "High" & 
                                        dat$level == "Low" & dat$Block == 8)])
  navon[i, "n_LoHi_b4"] <- mean(dat$RT[which(dat$levelpre == "Low" & 
                                        dat$level == "High" & dat$Block == 8)])
  navon[i, "n_LoLo_b4"] <- mean(dat$RT[which(dat$levelpre == "Low" & 
                                        dat$level == "Low" & dat$Block == 8)])
  navon[i, "n_HiHi_b4"] <- mean(dat$RT[which(dat$levelpre == "High" & 
                                        dat$level == "High" & dat$Block == 8)])
  navon[i, "n_NoSwitch_b4"] <- mean(dat$RT[which(dat$level == dat$levelpre &
                                             dat$Block == 8)])
  navon[i, "n_Switch_b4"] <- mean(dat$RT[which(dat$level != dat$levelpre &
                                             dat$Block == 8)])
  navon[i, "n_allsd_b4"] <- sd(dat$RT[which(dat$Block == 8)])
  
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
hist(navon$n_HiLo, breaks = 300); hist(navon$n_LoHi, breaks = 300)
hist(navon$n_LoLo, breaks = 300); hist(navon$n_HiHi, breaks = 300)

# Boxplots
library(car)
par(mfrow = c(1, 4))
Boxplot(navon$n_HiLo); Boxplot(navon$n_LoHi) # Note that the IDs are just row #
Boxplot(navon$n_LoLo); Boxplot(navon$n_HiHi)

# Which rows contain the outliers? (Note that row number != Subject ID)
which(navon$n_HiLo < 450) # Rows 89 233 240 268
which(navon$n_LoHi < 450) # Rows 89 233 240 268
which(navon$n_LoLo < 450) # Rows 89 240 268
which(navon$n_HiHi < 400) # Rows 89 240 268
# We exclude Rows 89 233 240 268

which(navon$n_HiLo > 1150) # Rows 91 172 182 194 195 204 205
which(navon$n_LoHi > 1200) # Row 182 205
which(navon$n_LoLo > 1000) # Rows 2 172 182 205 226
which(navon$n_HiHi > 1000) # Rows  2  40  91 194 204 205 208
# We exclude Rows 2 91 172 182 194 204 205

# Create n_Exclude variable
navon$n_Exclude <- 0
navon$n_Exclude[c(89, 233, 240, 268, 2, 91, 172, 182, 194, 204, 205)] <- 1

# 11. Exclude participants with error rate > 20%
par(mfrow = c(1, 1))
hist(navon$n_Error, breaks = 300)
navon$n_Exclude[which(navon$n_Error > 0.2)] <- 2

mean(navon$n_Error) # Overall error rate 5%
mean(navon$n_outl) # Overall outlier rate 4%
length(which(navon$n_Error > 0.2)) # n = 10

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
round(mean(navontest$RT[which(navontest$current == "Hi")]), 2)
round(sd(navontest$RT[which(navontest$current == "Hi")]), 2)
round(mean(navontest$RT[which(navontest$current == "Lo")]), 2)
round(sd(navontest$RT[which(navontest$current == "Lo")]), 2)

round(mean(navontest$RT[which(navontest$switch == "Switch")]), 2)
round(sd(navontest$RT[which(navontest$switch == "Switch")]), 2)
round(mean(navontest$RT[which(navontest$switch == "NoSwitch")]), 2)
round(sd(navontest$RT[which(navontest$switch == "NoSwitch")]), 2)

#### Prep: Stroop ####

# Set working directory to data folder
setwd(paste("/Users/andrewang/Documents/iLibrary/UC DAVIS/My Research/",
            "Construal Level Switching/Study 4/Data/Stroop",
            sep = ""))

# Pull each participant's data file from the folder
data <- Sys.glob("*.csv")

# Set a dataframe for storing data
stroop <- as.data.frame(matrix(NA, nrow = 0, ncol = 0))

# Loop
for (i in data) {
  dat <- read.csv(i, TRUE, ",")
  
  # 1. Delete instructions and demo block
  dat <- dat[-c(1:10, 59, 108, 157, 206), ]
  
  # 2. Identify whether stimulus is congruent or not
  dat$cong <- "incongruent"
  dat$cong[which(dat$Stim == "bb.bmp")] <- "congruent"
  dat$cong[which(dat$Stim == "gg.bmp")] <- "congruent"
  dat$cong[which(dat$Stim == "rr.bmp")] <- "congruent"
  dat$cong[which(dat$Stim == "yy.bmp")] <- "congruent"
  
  # 3. Sort trials by the order in which participants saw them
  dat <- dat[order(dat$Order), ]
  
  # 4. Delete practice trials: first 8 (# of stimuli) in each 48-trial block
  dat <- dat[-c(1:8, 49:56, 97:104, 145:152), ]
  
  # 5. Calculate overall error rate
  error <- mean(dat$Correct == "False")
  
  # 6. Exclude the trials with incorrect responses
  dat <- dat[which(dat$Correct == "True"), ]
  
  # 7. Exclude outlier trials (RT) within each participant's data (> +-2 SD)
  outl <- length(which(abs(dat$RT - mean(dat$RT))/sd(dat$RT) >= 2))/160
  dat <- dat[-which(abs(dat$RT - mean(dat$RT))/sd(dat$RT) >= 2), ]
  
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
  
  # Block 1
  stroop[i, "s_congruent_b1"] <- mean(dat$RT[which(dat$cong == "congruent" &
                                                    dat$Block == 4)])
  stroop[i, "s_incongruent_b1"] <- mean(dat$RT[which(dat$cong == "incongruent" &
                                                    dat$Block == 4)])
  stroop[i, "s_allsd_b1"] <- sd(dat$RT[which(dat$Block == 4)])
  
  # Block 2
  stroop[i, "s_congruent_b2"] <- mean(dat$RT[which(dat$cong == "congruent" &
                                                    dat$Block == 6)])
  stroop[i, "s_incongruent_b2"] <- mean(dat$RT[which(dat$cong == "incongruent" &
                                                    dat$Block == 6)])
  stroop[i, "s_allsd_b2"] <- sd(dat$RT[which(dat$Block == 6)])
  
  # Block 3
  stroop[i, "s_congruent_b3"] <- mean(dat$RT[which(dat$cong == "congruent" &
                                                    dat$Block == 8)])
  stroop[i, "s_incongruent_b3"] <- mean(dat$RT[which(dat$cong == "incongruent" &
                                                    dat$Block == 8)])
  stroop[i, "s_allsd_b3"] <- sd(dat$RT[which(dat$Block == 8)])
  
  # Block 4
  stroop[i, "s_congruent_b4"] <- mean(dat$RT[which(dat$cong == "congruent" &
                                                    dat$Block == 10)])
  stroop[i, "s_incongruent_b4"] <- mean(dat$RT[which(dat$cong == "incongruent" &
                                                    dat$Block == 10)])
  stroop[i, "s_allsd_b4"] <- sd(dat$RT[which(dat$Block == 10)])
  
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
hist(stroop$s_congruent, xlim = c(200, 2000), breaks = 100)
hist(stroop$s_incongruent, xlim = c(200, 2000), breaks = 100)

# Boxplots
library(car)
par(mfrow = c(1, 2))
Boxplot(stroop$s_congruent); Boxplot(stroop$s_incongruent)
# Note that the IDs are just row numbers

# Which rows contain the outliers? (Note that row number != Subject ID)
which(stroop$s_congruent < 400) # None
which(stroop$s_incongruent < 500) # Rows 5 267
# We exclude Rows 5 267

which(stroop$s_congruent > 1200) # Rows 187 204
which(stroop$s_incongruent > 1350) # Rows 55 187 204 261
# We exclude Rows 55 187 204 261

# Create n_Exclude variable
stroop$s_Exclude <- 0
stroop$s_Exclude[c(5, 267, 55, 187, 204, 261)] <- 1

# 11. Exclude participants with error rate > 20%
par(mfrow = c(1, 1))
hist(stroop$s_Error, breaks = 300)
stroop$s_Exclude[which(stroop$s_Error > 0.2)] <- 2

mean(stroop$s_Error) # Overall error rate 5%
mean(stroop$s_outl) # Overall outlier rate 4%
length(which(stroop$s_Error > 0.2)) # n = 8

# Stroop interference ####
t.test(stroop$s_incongruent, stroop$s_congruent, paired = T)
mean(stroop$s_incongruent, na.rm = T); sd(stroop$s_incongruent, na.rm = T)
mean(stroop$s_congruent); sd(stroop$s_congruent)
library(effsize)
cohen.d(stroop$s_incongruent, stroop$s_congruent, na.rm = T, paired = T)

#### Prep: Catexe ####

# Set working directory to data folder
setwd(paste("/Users/andrewang/Documents/iLibrary/UC DAVIS/My Research/",
            "Construal Level Switching/Study 4/Data/Catexe",
            sep = ""))

# Pull each participant's data file from the folder
data <- Sys.glob("*.csv")

# Set a dataframe for storing data
catexe <- as.data.frame(matrix(NA, nrow = 0, ncol = 0))

# Loop
for (i in data) {
  dat <- read.csv(i, TRUE, ",")
  
  # 1. Delete instructions and practice block
  dat <- dat[-c(1:25, 74, 123, 172, 221), ]
  
  # 2. Identify target level of the stimulus on the current trial
  dat$level <- substring(dat$Stim, 1, 3)
  
  # 3. Sort trials by the order in which participants saw them
  dat <- dat[order(dat$Order), ]

  # 4. Identify what the level of the stimulus on the previous trial is on
  levelpre <- c(NA, dat$level)
  length(levelpre) <- dim(dat)[1]
  dat$levelpre <- c(levelpre)
  
  # 5. Delete practice trials: first 8 (# of stimuli) in each 48-trial block
  dat <- dat[-c(1:8, 49:56, 97:104, 145:152), ]
  
  # 6. Calculate error rate
  error <- mean(dat$Correct == "False")
  
  # 7. Exclude the rest of the trials with incorrect responses
  dat <- dat[which(dat$Correct == "True"), ]
  
  # 8. Exclude outlier trials (RT) within each participant's data (> +-2 SD)
  outl <- length(which(abs(dat$RT - mean(dat$RT))/sd(dat$RT) >= 2))/160
  dat <- dat[-which(abs(dat$RT - mean(dat$RT))/sd(dat$RT) >= 2), ]
  
  # 9. Parcel
  if (dim(dat)[1]%%4 == 0) {dat$parID <- rep(1:4, dim(dat)[1]/4)
  } else {dat$parID <- c(rep(1:4, dim(dat)[1]/4), 1:(dim(dat)[1]%%4))}
  
  # 10. Calculate mean RTs
  catexe[i, "SubjectID"] <- substring(i, 1, nchar(i)-4) # ID participants
  
  # Overall
  catexe[i, "c_HiLo"] <- mean(dat$RT[which(dat$levelpre == "cat" & 
                                            dat$level == "exa")])
  catexe[i, "c_LoHi"] <- mean(dat$RT[which(dat$levelpre == "exa" & 
                                            dat$level == "cat")])
  catexe[i, "c_LoLo"] <- mean(dat$RT[which(dat$levelpre == "exa" & 
                                            dat$level == "exa")])
  catexe[i, "c_HiHi"] <- mean(dat$RT[which(dat$levelpre == "cat" &
                                            dat$level == "cat")])
  catexe[i, "c_NoSwitch"] <- mean(dat$RT[which(dat$level == dat$levelpre)])
  catexe[i, "c_Switch"] <- mean(dat$RT[which(dat$level != dat$levelpre)])
  catexe[i, "c_Error"] <- error
  catexe[i, "c_outl"] <- outl
  catexe[i, "c_allsd"] <- sd(dat$RT)
  
  # Block 1
  catexe[i, "c_HiLo_b1"] <- mean(dat$RT[which(dat$levelpre == "cat" & 
                                        dat$level == "exa" & dat$Block == 4)])
  catexe[i, "c_LoHi_b1"] <- mean(dat$RT[which(dat$levelpre == "exa" & 
                                        dat$level == "cat" & dat$Block == 4)])
  catexe[i, "c_LoLo_b1"] <- mean(dat$RT[which(dat$levelpre == "exa" & 
                                        dat$level == "exa" & dat$Block == 4)])
  catexe[i, "c_HiHi_b1"] <- mean(dat$RT[which(dat$levelpre == "cat" &
                                        dat$level == "cat" & dat$Block == 4)])
  catexe[i, "c_NoSwitch_b1"] <- mean(dat$RT[which(dat$level == dat$levelpre &
                                        dat$Block == 4)])
  catexe[i, "c_Switch_b1"] <- mean(dat$RT[which(dat$level != dat$levelpre & 
                                        dat$Block == 4)])
  catexe[i, "c_allsd_b1"] <- sd(dat$RT[which(dat$Block == 4)])
  
  # Block 2
  catexe[i, "c_HiLo_b2"] <- mean(dat$RT[which(dat$levelpre == "cat" & 
                                        dat$level == "exa" & dat$Block == 6)])
  catexe[i, "c_LoHi_b2"] <- mean(dat$RT[which(dat$levelpre == "exa" & 
                                        dat$level == "cat" & dat$Block == 6)])
  catexe[i, "c_LoLo_b2"] <- mean(dat$RT[which(dat$levelpre == "exa" & 
                                        dat$level == "exa" & dat$Block == 6)])
  catexe[i, "c_HiHi_b2"] <- mean(dat$RT[which(dat$levelpre == "cat" & 
                                        dat$level == "cat" & dat$Block == 6)])
  catexe[i, "c_NoSwitch_b2"] <- mean(dat$RT[which(dat$level == dat$levelpre &
                                        dat$Block == 6)])
  catexe[i, "c_Switch_b2"] <- mean(dat$RT[which(dat$level != dat$levelpre &
                                        dat$Block == 6)])
  catexe[i, "c_allsd_b2"] <- sd(dat$RT[which(dat$Block == 6)])
  
  # Block 3
  catexe[i, "c_HiLo_b3"] <- mean(dat$RT[which(dat$levelpre == "cat" & 
                                        dat$level == "exa" & dat$Block == 8)])
  catexe[i, "c_LoHi_b3"] <- mean(dat$RT[which(dat$levelpre == "exa" & 
                                        dat$level == "cat" & dat$Block == 8)])
  catexe[i, "c_LoLo_b3"] <- mean(dat$RT[which(dat$levelpre == "exa" & 
                                        dat$level == "exa" & dat$Block == 8)])
  catexe[i, "c_HiHi_b3"] <- mean(dat$RT[which(dat$levelpre == "cat" & 
                                        dat$level == "cat" & dat$Block == 8)])
  catexe[i, "c_NoSwitch_b3"] <- mean(dat$RT[which(dat$level == dat$levelpre &
                                            dat$Block == 8)])
  catexe[i, "c_Switch_b3"] <- mean(dat$RT[which(dat$level != dat$levelpre &
                                          dat$Block == 8)])
  catexe[i, "c_allsd_b3"] <- sd(dat$RT[which(dat$Block == 8)])
  
  # Block 4
  catexe[i, "c_HiLo_b4"] <- mean(dat$RT[which(dat$levelpre == "cat" & 
                                        dat$level == "exa" & dat$Block == 10)])
  catexe[i, "c_LoHi_b4"] <- mean(dat$RT[which(dat$levelpre == "exa" & 
                                        dat$level == "cat" & dat$Block == 10)])
  catexe[i, "c_LoLo_b4"] <- mean(dat$RT[which(dat$levelpre == "exa" & 
                                        dat$level == "exa" & dat$Block == 10)])
  catexe[i, "c_HiHi_b4"] <- mean(dat$RT[which(dat$levelpre == "cat" & 
                                        dat$level == "cat" & dat$Block == 10)])
  catexe[i, "c_NoSwitch_b4"] <- mean(dat$RT[which(dat$level == dat$levelpre &
                                        dat$Block == 10)])
  catexe[i, "c_Switch_b4"] <- mean(dat$RT[which(dat$level != dat$levelpre &
                                        dat$Block == 10)])
  catexe[i, "c_allsd_b4"] <- sd(dat$RT[which(dat$Block == 10)])
  
  # Parcel 1
  catexe[i, "c_HiLo_p1"] <- mean(dat$RT[which(
    dat$levelpre == "cat" & dat$level == "exa" & dat$parID == 1)])
  catexe[i, "c_LoHi_p1"] <- mean(dat$RT[which(
    dat$levelpre == "exa" & dat$level == "cat" & dat$parID == 1)])
  catexe[i, "c_LoLo_p1"] <- mean(dat$RT[which(
    dat$levelpre == "exa" & dat$level == "exa" & dat$parID == 1)])
  catexe[i, "c_HiHi_p1"] <- mean(dat$RT[which(
    dat$levelpre == "cat" & dat$level == "cat" & dat$parID == 1)])
  catexe[i, "c_NoSwitch_p1"] <- mean(dat$RT[which(
    dat$level == dat$levelpre & dat$parID == 1)])
  catexe[i, "c_Switch_p1"] <- mean(dat$RT[which(
    dat$level != dat$levelpre & dat$parID == 1)])
  catexe[i, "c_allsd_p1"] <- sd(dat$RT[which(dat$parID == 1)])
  
  # Parcel 2
  catexe[i, "c_HiLo_p2"] <- mean(dat$RT[which(
    dat$levelpre == "cat" & dat$level == "exa" & dat$parID == 2)])
  catexe[i, "c_LoHi_p2"] <- mean(dat$RT[which(
    dat$levelpre == "exa" & dat$level == "cat" & dat$parID == 2)])
  catexe[i, "c_LoLo_p2"] <- mean(dat$RT[which(
    dat$levelpre == "exa" & dat$level == "exa" & dat$parID == 2)])
  catexe[i, "c_HiHi_p2"] <- mean(dat$RT[which(
    dat$levelpre == "cat" & dat$level == "cat" & dat$parID == 2)])
  catexe[i, "c_NoSwitch_p2"] <- mean(dat$RT[which(
    dat$level == dat$levelpre & dat$parID == 2)])
  catexe[i, "c_Switch_p2"] <- mean(dat$RT[which(
    dat$level != dat$levelpre & dat$parID == 2)])
  catexe[i, "c_allsd_p2"] <- sd(dat$RT[which(dat$parID == 2)])
  
  # Parcel 3
  catexe[i, "c_HiLo_p3"] <- mean(dat$RT[which(
    dat$levelpre == "cat" & dat$level == "exa" & dat$parID == 3)])
  catexe[i, "c_LoHi_p3"] <- mean(dat$RT[which(
    dat$levelpre == "exa" & dat$level == "cat" & dat$parID == 3)])
  catexe[i, "c_LoLo_p3"] <- mean(dat$RT[which(
    dat$levelpre == "exa" & dat$level == "exa" & dat$parID == 3)])
  catexe[i, "c_HiHi_p3"] <- mean(dat$RT[which(
    dat$levelpre == "cat" & dat$level == "cat" & dat$parID == 3)])
  catexe[i, "c_NoSwitch_p3"] <- mean(dat$RT[which(
    dat$level == dat$levelpre & dat$parID == 3)])
  catexe[i, "c_Switch_p3"] <- mean(dat$RT[which(
    dat$level != dat$levelpre & dat$parID == 3)])
  catexe[i, "c_allsd_p3"] <- sd(dat$RT[which(dat$parID == 3)])
  
  # Parcel 4
  catexe[i, "c_HiLo_p4"] <- mean(dat$RT[which(
    dat$levelpre == "cat" & dat$level == "exa" & dat$parID == 4)])
  catexe[i, "c_LoHi_p4"] <- mean(dat$RT[which(
    dat$levelpre == "exa" & dat$level == "cat" & dat$parID == 4)])
  catexe[i, "c_LoLo_p4"] <- mean(dat$RT[which(
    dat$levelpre == "exa" & dat$level == "exa" & dat$parID == 4)])
  catexe[i, "c_HiHi_p4"] <- mean(dat$RT[which(
    dat$levelpre == "cat" & dat$level == "cat" & dat$parID == 4)])
  catexe[i, "c_NoSwitch_p4"] <- mean(dat$RT[which(
    dat$level == dat$levelpre & dat$parID == 4)])
  catexe[i, "c_Switch_p4"] <- mean(dat$RT[which(
    dat$level != dat$levelpre & dat$parID == 4)])
  catexe[i, "c_allsd_p4"] <- sd(dat$RT[which(dat$parID == 4)])
}
rownames(catexe) <- 1:length(data)

# 11. Visually check for outliers: Across participants

# Histograms
par(mfrow = c(2, 2))
hist(catexe$c_HiLo, xlim = c(100, 3000), breaks = 300)
hist(catexe$c_LoHi, xlim = c(100, 3000), breaks = 300)
hist(catexe$c_LoLo, xlim = c(100, 3000), breaks = 300)
hist(catexe$c_HiHi, xlim = c(100, 3000), breaks = 300)

# Boxplots
library(car)
par(mfrow = c(1, 4))
Boxplot(catexe$c_HiLo); Boxplot(catexe$c_LoHi) # Note that the IDs are just row
Boxplot(catexe$c_LoLo); Boxplot(catexe$c_HiHi)

# Which rows contain the outliers? (Note that row number != Subject ID)
which(catexe$c_HiLo < 700) # Rows 36 236 243 272
which(catexe$c_LoHi < 700) # Rows 36 236 272
which(catexe$c_LoLo < 700) # Rows 36 236 272
which(catexe$c_HiHi < 700) # Rows 36 236 272
# We exclude Rows 36 236 272

which(catexe$c_HiLo > 2500) # Rows 8 233 245
which(catexe$c_LoHi > 2500) # Rows 8  42  61 233 245
which(catexe$c_LoLo > 2200) # Rows 8  61 233 245
which(catexe$c_HiHi > 2500) # Rows 8  61 233 245
# We exclude Rows 8  61 233 245

# Create c_Exclude variable
catexe$c_Exclude <- 0
catexe$c_Exclude[c(36, 236, 272, 8, 61, 233, 245)] <- 1

# 12. Exclude participants with error rate > 20%
par(mfrow = c(1, 1))
hist(catexe$c_Error, breaks = 300)
catexe$c_Exclude[which(catexe$c_Error > 0.2)] <- 2

mean(catexe$c_Error) # Overall error rate 9%
mean(catexe$c_outl) # Overall outlier rate 8%
length(which(catexe$c_Error > 0.2)) # n = 12

#### Level-Switching Cost: Catexe ####
catexe1 <- catexe[which(catexe$c_Exclude == 0), c(1:5, 10)]
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
round(mean(catexetest$RT[which(catexetest$current == "Hi")]), 2)
round(sd(catexetest$RT[which(catexetest$current == "Hi")]), 2)
round(mean(catexetest$RT[which(catexetest$current == "Lo")]), 2)
round(sd(catexetest$RT[which(catexetest$current == "Lo")]), 2)

round(mean(catexetest$RT[which(catexetest$switch == "Switch")]), 2)
round(sd(catexetest$RT[which(catexetest$switch == "Switch")]), 2)
round(mean(catexetest$RT[which(catexetest$switch == "NoSwitch")]), 2)
round(sd(catexetest$RT[which(catexetest$switch == "NoSwitch")]), 2)

ezStats(catexetest, dv = RT, wid = SubjectID, within = .(current, switch), 
        type = 3)
ezPlot(catexetest, dv = RT, wid = SubjectID, within = .(current, switch),
       x = switch, split = current, type = 3)

# I tried to alculate simple main effects in SPSS
setwd(paste("/Users/andrewang/Documents/iLibrary/UC DAVIS/My Research/",
            "Construal Level Switching/Study 4/Data",
            sep = ""))
write.table(catexe1, sep = ",", row.names = F, "Study 4 Catexe.csv")

# Using individual scores
catexe1$sa <- ((catexe1$c_HiLo - catexe1$c_LoHi) - 
               (catexe1$c_LoLo - catexe1$c_HiHi))
t.test(catexe1$sa)


#### Data Merger ####

# Assign NAs to data that should be excluded for each task
colnames(navon)
navon[which(navon$n_Exclude != 0), 2:66] <- NA
colnames(stroop)
stroop[which(stroop$s_Exclude != 0), 2:30] <- NA
colnames(catexe)
catexe[which(catexe$c_Exclude != 0), 2:66] <- NA

# Merge three dataframes into one
cvg <- merge(catexe, navon, all.x = TRUE)
cvg <- merge(cvg, stroop, all.x = TRUE)


### Convergent and Discriminant Validity Tests ####

# Raw correlations
colnames(cvg)
cvg$s_sc <- (cvg$s_congruent - cvg$s_incongruent)/cvg$s_allsd
cvg$n_sc  <- (cvg$n_Switch - cvg$n_NoSwitch)/cvg$n_allsd
cvg$c_sc  <- (cvg$c_Switch - cvg$c_NoSwitch)/cvg$c_allsd
cor.test(cvg$c_sc, cvg$n_sc)
cor.test(cvg$s_sc, cvg$n_sc)
cor.test(cvg$s_sc, cvg$c_sc)

library(lavaan);library(ggplot2);library(corrplot)
library(lmerTest);library(psych);library(semPlot)

# Scores by parcel

# Navon
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
corrplot(cor(cvg[, 167:178], use = "pairwise.complete.obs"), method = "square",
         shade.col = NULL, tl.col = "black", tl.srt = 45, tl.cex = 1)
round(cor(cvg[, 167:178], use = "pairwise.complete.obs"), 2)


# Model 1
sc_mod1 <- "
c_sc =~ c_sc_p1 + c_sc_p2 + c_sc_p3 + c_sc_p4
n_sc =~ n_sc_p1 + n_sc_p2 + n_sc_p3 + n_sc_p4
s_sc =~ s_sc_p1 + s_sc_p2 + s_sc_p3 + s_sc_p4
"
sc_fit1 <- cfa(sc_mod1, data = cvg, std.lv = TRUE)
summary(sc_fit1, fit.measures = TRUE, standardized = TRUE)
semPaths(sc_fit1, "std", fade = F, style = "lisrel",
         edge.color = 'black', esize = T, nodeLabels = labels,
         edge.label.cex = 0.8, sizeMan = 6, sizeLat = 8)

# Model 2: Missing Data
sc_mod2 <- "
c_sc =~ c_sc_p1 + c_sc_p2 + c_sc_p3 + c_sc_p4
n_sc =~ n_sc_p1 + n_sc_p2 + n_sc_p3 + n_sc_p4
s_sc =~ s_sc_p1 + s_sc_p2 + s_sc_p3 + s_sc_p4
"
sc_fit2 <- cfa(sc_mod2, data = cvg, std.lv = TRUE, missing = "FIML")
summary(sc_fit2, fit.measures = TRUE, standardized = TRUE)
semPaths(sc_fit2, "std", fade = F, style = "lisrel", intercepts = F,
         edge.color = 'black', esize = T, nodeLabels = labels,
         edge.label.cex = 0.8, sizeMan = 6, sizeLat = 8)

# Graph the correlations
sc_fit2_label <- c("Catexe~~Navon", "Catexe~~Stroop", "Navon~~Stroop")
sc_fit2_mean  <- c(0.634, 0.162, 0.290)
sc_fit2_lower <- c(0.634-0.149, 0.162-0.130, 0.290-0.121)
sc_fit2_upper <- c(0.634+0.149, 0.162+0.130, 0.290+0.121)
sc_fit2_graph <- data.frame(sc_fit2_label, sc_fit2_mean, sc_fit2_lower, sc_fit2_upper)
sc_fit2_graph$sc_fit2_label <- factor(sc_fit2_graph$sc_fit2_label, 
                                levels = rev(sc_fit2_graph$sc_fit2_label))
ggplot(sc_fit2_graph, aes(x = sc_fit2_label, y = sc_fit2_mean, 
                       ymin = sc_fit2_lower, ymax = sc_fit2_upper)) +
  geom_pointrange(fatten = 5) + coord_flip() + 
  xlab("Correlations") + ylab("Estimate (±1 SE)")

### Data Analysis: Switching Asymmetry ####

# Switching Asymmetry, by parcel
cvg$c_sa_p1 <- (cvg$c_HiLo_p1 - cvg$c_LoHi_p1 - 
                  (cvg$c_LoLo_p1 - cvg$c_HiHi_p1))/cvg$c_allsd_p1
cvg$c_sa_p2 <- (cvg$c_HiLo_p2 - cvg$c_LoHi_p2 - 
                  (cvg$c_LoLo_p2 - cvg$c_HiHi_p2))/cvg$c_allsd_p2
cvg$c_sa_p3 <- (cvg$c_HiLo_p3 - cvg$c_LoHi_p3 - 
                  (cvg$c_LoLo_p3 - cvg$c_HiHi_p3))/cvg$c_allsd_p3
cvg$c_sa_p4 <- (cvg$c_HiLo_p4 - cvg$c_LoHi_p4 - 
                  (cvg$c_LoLo_p4 - cvg$c_HiHi_p4))/cvg$c_allsd_p4

cvg$n_sa_p1 <- (cvg$n_HiLo_p1 - cvg$n_LoHi_p1 - 
                  (cvg$n_LoLo_p1 - cvg$n_HiHi_p1))/cvg$n_allsd_p1
cvg$n_sa_p2 <- (cvg$n_HiLo_p2 - cvg$n_LoHi_p2 - 
                  (cvg$n_LoLo_p2 - cvg$n_HiHi_p2))/cvg$n_allsd_p2
cvg$n_sa_p3 <- (cvg$n_HiLo_p3 - cvg$n_LoHi_p3 - 
                  (cvg$n_LoLo_p3 - cvg$n_HiHi_p3))/cvg$n_allsd_p3
cvg$n_sa_p4 <- (cvg$n_HiLo_p4 - cvg$n_LoHi_p4 - 
                  (cvg$n_LoLo_p4 - cvg$n_HiHi_p4))/cvg$n_allsd_p4

# Switching Asymmetry, Overall
cvg$c_sa <- (cvg$c_HiLo - cvg$c_LoHi - 
                  (cvg$c_LoLo - cvg$c_HiHi))/cvg$c_allsd
cvg$n_sa <- (cvg$n_HiLo - cvg$n_LoHi - 
               (cvg$n_LoLo - cvg$n_HiHi))/cvg$n_allsd
cor(cvg[181], cvg[182], use = "pairwise.complete.obs")


# Visualize SA for the 4 * 2 = 8 parcels
par(mfrow = c(2, 4))
hist(cvg$n_sa_p1); hist(cvg$n_sa_p2); hist(cvg$n_sa_p3); hist(cvg$n_sa_p4)
hist(cvg$c_sa_p1); hist(cvg$c_sa_p2); hist(cvg$c_sa_p3); hist(cvg$c_sa_p4)

# Correlation table of SA scores
colnames(cvg)
sa <- subset(cvg[, 173:180])
cor(sa, use = "pairwise.complete.obs")
par(mfrow = c(1, 1))
corrplot(cor(sa, use = "pairwise.complete.obs"), method = "square",
         shade.col = NULL, tl.col = "black", tl.srt = 45, tl.cex = 1)

# Model 1
sa_mod1 <- "
n_sa =~ n_sa_p1 + n_sa_p2 + n_sa_p3 + n_sa_p4
c_sa =~ c_sa_p1 + c_sa_p2 + c_sa_p3 + c_sa_p4
"
sa_fit1 <- cfa(sa_mod1, data = cvg, std.lv = TRUE)
summary(sa_fit1, fit.measures = TRUE, standardized = TRUE)
semPaths(sa_fit1, "std", fade = F, style = "lisrel",
         edge.color = 'black', esize = T, nodeLabels = labels,
         edge.label.cex = 0.8, sizeMan = 6, sizeLat = 8)

# Model 2
sa_mod2 <- "
c_sa =~ c_sa_p1 + c_sa_p2 + c_sa_p3 + c_sa_p4
"
sa_fit2 <- cfa(sa_mod2, data = cvg, std.lv = TRUE)
summary(sa_fit2, fit.measures = TRUE, standardized = TRUE)
semPaths(sa_fit2, "std", fade = F, style = "lisrel",
         edge.color = 'black', esize = T, nodeLabels = labels,
         edge.label.cex = 0.8, sizeMan = 6, sizeLat = 8)
