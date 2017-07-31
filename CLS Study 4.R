# Construal Level Switching Study 4
# Last updated: 04/12/17

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
  
  # 7. Exclude outlier trials (RT) within each participant's data (> +-2 SD)
  dat <- dat[-which(abs(dat$RT - mean(dat$RT))/sd(dat$RT) >= 2), ]
  
  # 8. Exclude the rest of the trials with incorrect responses
  dat <- dat[which(dat$Correct == "True"), ]
  
  # 9. Parcel
  if (dim(dat)[1]%%4 == 0) {dat$parID <- rep(1:4, dim(dat)[1]/4)
  } else {dat$parID <- c(rep(1:4, dim(dat)[1]/4), 1:(dim(dat)[1]%%4))}
  
  # 10. Calculate mean RTs
  navon[i, "File"] <- substring(i, 1, nchar(i)-4) # Participant ID
  
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
which(navon$n_HiLo < 400) # Rows 89 240 268
which(navon$n_LoHi < 400) # Rows 233 240 268
which(navon$n_LoLo < 400) # Rows 240 268
which(navon$n_HiHi < 400) # Rows 89 233 240 268
# We exclude Rows 240 268

which(navon$n_HiLo > 1200) # Rows 91 182 195 204 205
which(navon$n_LoHi > 1100) # Row 91 182 194 195 204
which(navon$n_LoLo > 1000) # Rows 2 172 182 205 226
which(navon$n_HiHi > 1000) # Rows 40  91 194 205 208
# We exclude Rows 91 182 194 205

# Create n_Exclude variable
navon$n_Exclude <- 0
navon$n_Exclude[c(240, 268, 91, 182, 194, 205)] <- 1

# 11. Exclude participants with error rate > 20%
par(mfrow = c(1, 1))
hist(navon$n_Error, breaks = 300)
navon$n_Exclude[which(navon$n_Error > 0.2)] <- 2

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
  
  # 6. Exclude outlier trials (RT) within each participant's data (> +-2 SD)
  dat <- dat[-which(abs(dat$RT - mean(dat$RT))/sd(dat$RT) >= 2), ]
  
  # 7. Exclude the rest of the trials with incorrect responses
  dat <- dat[which(dat$Correct == "True"), ]
  
  # 8. Parcel
  if (dim(dat)[1]%%4 == 0) {dat$parID <- rep(1:4, dim(dat)[1]/4)
  } else {dat$parID <- c(rep(1:4, dim(dat)[1]/4), 1:(dim(dat)[1]%%4))}
  
  # 9. Calculate mean RTs
  stroop[i, "File"] <- substring(i, 1, nchar(i)-4)
  
  # Overall
  stroop[i, "s_congruent"] <- mean(dat$RT[which(dat$cong == "congruent")])
  stroop[i, "s_incongruent"] <- mean(dat$RT[which(dat$cong == "incongruent")])
  stroop[i, "s_Error"] <- error
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
par(mfrow = c(1, 2))
hist(stroop$s_congruent, xlim = c(200, 2000), breaks = 300)
hist(stroop$s_incongruent, xlim = c(200, 2000), breaks = 300)

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
which(stroop$s_incongruent > 1400) # Rows 55 187 204
# We exclude Rows 187 204

# Create n_Exclude variable
stroop$s_Exclude <- 0
stroop$s_Exclude[c(5, 267, 187, 204)] <- 1

# 11. Exclude participants with error rate > 20%
par(mfrow = c(1, 1))
hist(stroop$s_Error, breaks = 300)
stroop$s_Exclude[which(stroop$s_Error > 0.2)] <- 2


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
  
  # 7. Exclude outlier trials (RT) within each participant's data (> +-2 SD)
  dat <- dat[-which(abs(dat$RT - mean(dat$RT))/sd(dat$RT) >= 2), ]
  
  # 8. Exclude the rest of the trials with incorrect responses
  dat <- dat[which(dat$Correct == "True"), ]
  
  # 9. Parcel
  if (dim(dat)[1]%%4 == 0) {dat$parID <- rep(1:4, dim(dat)[1]/4)
  } else {dat$parID <- c(rep(1:4, dim(dat)[1]/4), 1:(dim(dat)[1]%%4))}
  
  # 10. Calculate mean RTs
  catexe[i, "File"] <- substring(i, 1, nchar(i)-4) # ID participants
  
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
which(catexe$c_HiLo < 600) # Rows 36 236 272
which(catexe$c_LoHi < 600) # Rows 36 236 272
which(catexe$c_LoLo < 600) # Rows 36 236 272
which(catexe$c_HiHi < 600) # Rows 36 236 272
# We exclude Rows 36 236 272

which(catexe$c_HiLo > 2300) # Rows 8  61 233 249
which(catexe$c_LoHi > 2500) # Rows 8  61 233
which(catexe$c_LoLo > 2200) # Rows 8  61 233
which(catexe$c_HiHi > 2500) # Rows 8  61 233
# We exclude Rows 8  61 233

# Create c_Exclude variable
catexe$c_Exclude <- 0
catexe$c_Exclude[c(36, 236, 272, 8, 61, 233)] <- 1

# 12. Exclude participants with error rate > 20%
par(mfrow = c(1, 1))
hist(catexe$c_Error, breaks = 300)
catexe$c_Exclude[which(catexe$c_Error > 0.2)] <- 2



#### Data Merger ####

# Assign NAs to data that should be excluded for each task
colnames(navon)
navon[which(navon$n_Exclude != 0), 2:65] <- NA
colnames(stroop)
stroop[which(stroop$s_Exclude != 0), 2:29] <- NA
colnames(catexe)
catexe[which(catexe$c_Exclude != 0), 2:65] <- NA

# Merge three dataframes into one
cvg <- merge(catexe, navon, all.x = TRUE)
cvg <- merge(cvg, stroop, all.x = TRUE)

# Test means
View(cvg)
mean(cvg$n_HiLo, na.rm = T)
mean(cvg$n_LoHi, na.rm = T)
mean(cvg$n_LoLo, na.rm = T)
mean(cvg$n_HiHi, na.rm = T)

# Individual Scores (To be updated 170305) ####
View(cvg)
str(cvg)
t.test(cvg$c_NoSwitch, cvg$c_Switch)
library(plotrix)
library(effsize)
cohen.d(cvg$c_NoSwitch, cvg$c_Switch, na.rm = T)
std.error(cvg$c_Switch)
std.error(cvg$c_NoSwitch)

t.test(cvg$n_NoSwitch, cvg$n_Switch)
library(plotrix)
library(effsize)
cohen.d(cvg$n_NoSwitch, cvg$n_Switch, na.rm = T)
std.error(cvg$n_Switch)
std.error(cvg$n_NoSwitch)


### Data Analysis: Switching Cost ####

library(lavaan);library(ggplot2);library(corrplot)
library(lmerTest);library(psych);library(semPlot)

# Switching Cost (SC) and Switching Interference (SI), by parcel
cvg$s_sc_p1 <- (cvg$s_incongruent_p1 - cvg$s_congruent_p1)/cvg$s_allsd_p1
cvg$s_sc_p2 <- (cvg$s_incongruent_p2 - cvg$s_congruent_p2)/cvg$s_allsd_p2
cvg$s_sc_p3 <- (cvg$s_incongruent_p3 - cvg$s_congruent_p3)/cvg$s_allsd_p3
cvg$s_sc_p4 <- (cvg$s_incongruent_p4 - cvg$s_congruent_p4)/cvg$s_allsd_p4
cvg$c_sc_p1 <- (cvg$c_Switch_p1 - cvg$c_NoSwitch_p1)/cvg$c_allsd_p1
cvg$c_sc_p2 <- (cvg$c_Switch_p2 - cvg$c_NoSwitch_p2)/cvg$c_allsd_p2
cvg$c_sc_p3 <- (cvg$c_Switch_p3 - cvg$c_NoSwitch_p3)/cvg$c_allsd_p3
cvg$c_sc_p4 <- (cvg$c_Switch_p4 - cvg$c_NoSwitch_p4)/cvg$c_allsd_p4
cvg$n_sc_p1  <- (cvg$n_Switch_p1 - cvg$n_NoSwitch_p1)/cvg$n_allsd_p1
cvg$n_sc_p2  <- (cvg$n_Switch_p2 - cvg$n_NoSwitch_p2)/cvg$n_allsd_p2
cvg$n_sc_p3  <- (cvg$n_Switch_p3 - cvg$n_NoSwitch_p3)/cvg$n_allsd_p3
cvg$n_sc_p4  <- (cvg$n_Switch_p4 - cvg$n_NoSwitch_p4)/cvg$n_allsd_p4

# Visualize SC for the 4 * 3 = 12 parcels
par(mfrow = c(3, 4))
hist(cvg$s_sc_p1); hist(cvg$s_sc_p2); hist(cvg$s_sc_p3); hist(cvg$s_sc_p4)
hist(cvg$n_sc_p1); hist(cvg$n_sc_p2); hist(cvg$n_sc_p3); hist(cvg$n_sc_p4)
hist(cvg$c_sc_p1); hist(cvg$c_sc_p2); hist(cvg$c_sc_p3); hist(cvg$c_sc_p4)

# Correlation table of SC and SI scores
colnames(cvg)
sc <- subset(cvg[, 161:172])
cor(sc, use = "pairwise.complete.obs")
par(mfrow = c(1, 1))
corrplot(cor(sc, use = "pairwise.complete.obs"), method = "square",
         shade.col = NULL, tl.col = "black", tl.srt = 45, tl.cex = 1)

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


# Sandbox ####
library(lavaan); library(corrplot)
navon.test <- navon[which(navon$n_Exclude == 0), ]

n_sc_1  <- (navon.test$n_Switch_p1 - navon.test$n_NoSwitch_p1)/navon.test$n_allsd_p1
n_sc_2  <- (navon.test$n_Switch_p2 - navon.test$n_NoSwitch_p2)/navon.test$n_allsd_p2
n_sc_3  <- (navon.test$n_Switch_p3 - navon.test$n_NoSwitch_p3)/navon.test$n_allsd_p3
n_sc_4  <- (navon.test$n_Switch_p4 - navon.test$n_NoSwitch_p4)/navon.test$n_allsd_p4
sc <- data.frame(n_sc_1, n_sc_2, n_sc_3, n_sc_4)
View(sc)
corrplot(cor(sc, use = "pairwise.complete.obs"), method = "number",
         shade.col = NULL, tl.col = "black", tl.srt = 45, tl.cex = 1)
