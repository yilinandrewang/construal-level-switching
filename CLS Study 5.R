# CLS Study 5
# Last Update: 07/30/17

#### Prep: Number-Letter Task ####

# Set working directory to data folder
setwd(paste("/Users/andrewang/Documents/iLibrary/UC DAVIS/My Research/",
            "Construal Level Switching/Study 5/Data/Number-Letter",
            sep = ""))

# Pull each participant's data file from the folder
data <- Sys.glob("*.txt")

# Set a dataframe for storing data
nl <- as.data.frame(matrix(NA, nrow = 0, ncol = 0))

# Loop
for (i in data) {
  dat <- read.table(i) # read each data file
  
  # 1. Label column names
  colnames(dat) <- c("blockname", "sti_position", "tasktype", "sti_letter", 
                      "sti_number", "blocktype", "switch", "status", "RT", "TT")
    # blockname: block name
    # sti_position: position of stimulus 1, 2, 3, 4
      # (top left, top right, bottom right, bottom left)
    # tasktype: 1 or 2
    # sti_letter: the letter stimulus
    # sti_number: the number stimulus
    # blocktype: type of block (1 = just task 1; 2 = just task 2; 0 = both)
    # switch: 1 = task switch, 0 = task repeat
    # status: 1 = correct, 2 = error, 3 = too slow
    # RT: response time (ms)
    # TT: total time (response time + button release time)
  
  # 2. Number the order of trials
  dat$order <- 1:240
  
  # 3. Identify experimental trials
  # (first 8 trials of each mixed block of 48 trials are practice trials)
  dat$exp <- 1
  dat$exp[c(1:48, 49:56, 97:104, 145:152, 193:200)] <- 0
  
  # 4. Identify trials that immediately follow error trials
  dat$follow <- 0
  if (length(which(dat$status == 2)) == 0) {
    dat <- dat
    error <- 0
    followe <- 0
  } else {
    # Create follow variable: 0 = trials do not follow an error trial; 1 = do
    dat$follow[which(dat$status == 2 & dat$order != 240) + 1] <- 1
    # Percentage of error trials in the experimental trials
    error <- length(which(dat$status == 2 & 
                            dat$exp == 1))/length(which(dat$exp == 1))
    # Percentage of correct trials that follow error trials in the exp trials
    followe <- length(which(dat$follow == 1 &
                              dat$exp == 1))/length(which(dat$exp == 1))
  }
  
  # 5. Keep only the experimental trials
  dat <- dat[which(dat$exp == 1), ]
  
  # 6. Delete error trials and trials following an error
  dat <- dat[which(dat$status == 1), ] # Delete error trials
  dat <- dat[which(dat$follow == 0), ] # Delete trials following error
  
  # 7. Delete outliers (+- 2.5 MAD)
  outl <- length(which(abs(dat$RT - median(dat$RT))/mad(dat$RT) >= 2.5))/160
  if (max(abs((dat$RT - median(dat$RT))/mad(dat$RT))) < 2.5) {
    dat <- dat
  } else {
    dat <- dat[-which(
      abs(dat$RT - median(dat$RT))/mad(dat$RT) >= 2.5), ]
  }
  
  # 8. Delete trials with RT less than 100 ms
  if (length(which(dat$RT < 100)) == 0) {
    dat <- dat
  } else {dat <- dat[-c(which(dat$RT < 100)), ]}
  
  # 9. Parcel
  if (dim(dat)[1]%%4 == 0) {
    dat$parID <- rep(1:4, dim(dat)[1]/4)
  } else {
    dat$parID <- c(rep(1:4, dim(dat)[1]/4), 1:(dim(dat)[1]%%4))
  }
  
  # 10. Calculate mean RTs
  nl[i, "File"] <- i
  
  # Overall
  nl[i, "l_switch"] <- mean(dat$RT[which(dat$switch == 1)])
  nl[i, "l_noswitch"] <- mean(dat$RT[which(dat$switch == 0)])
  nl[i, "l_allsd"] <- sd(dat$RT)
  nl[i, "l_error"] <- error
  nl[i, "l_followe"] <- followe
  nl[i, "l_outl"] <- outl
  
  # Parcel 1
  nl[i, "l_switch_p1"] <- mean(dat$RT[which(dat$switch == 1 & dat$parID == 1)])
  nl[i, "l_noswitch_p1"] <- mean(dat$RT[which(dat$switch == 0 & dat$parID == 1)])
  nl[i, "l_allsd_p1"] <- sd(dat$RT[which(dat$parID == 1)])
  
  # Parcel 2
  nl[i, "l_switch_p2"] <- mean(dat$RT[which(dat$switch == 1 & dat$parID == 2)])
  nl[i, "l_noswitch_p2"] <- mean(dat$RT[which(dat$switch == 0 & dat$parID == 2)])
  nl[i, "l_allsd_p2"] <- sd(dat$RT[which(dat$parID == 2)])
  
  # Parcel 3
  nl[i, "l_switch_p3"] <- mean(dat$RT[which(dat$switch == 1 & dat$parID == 3)])
  nl[i, "l_noswitch_p3"] <- mean(dat$RT[which(dat$switch == 0 & dat$parID == 3)])
  nl[i, "l_allsd_p3"] <- sd(dat$RT[which(dat$parID == 3)])
  
  # Parcel 4
  nl[i, "l_switch_p4"] <- mean(dat$RT[which(dat$switch == 1 & dat$parID == 4)])
  nl[i, "l_noswitch_p4"] <- mean(dat$RT[which(dat$switch == 0 & dat$parID == 4)])
  nl[i, "l_allsd_p4"] <- sd(dat$RT[which(dat$parID == 4)])
}

nlid <- read.csv("data.csv")
nlid <- nlid[, 1:2]
colnames(nlid) <- c("SubjectID", "File")
NL <- merge(nlid, nl)
NL <- NL[order(NL$SubjectID), ]

# 11. Visually check for outliers: Across participants

# Histograms
par(mfrow = c(2, 1))
hist(NL$l_switch, xlim = c(100, 2500), breaks = 300)
hist(NL$l_noswitch, xlim = c(100, 1500), breaks = 300)

# Boxplots
library(car)
par(mfrow = c(1, 2))
Boxplot(NL$l_switch); Boxplot(NL$l_noswitch)
# Note that the IDs are just row numbers

# Which rows contain the outliers? (Note that row number != Subject ID)
which(NL$l_switch < 400) # Row 111
which(NL$l_noswitch < 400) # Row 111
# We exclude Row 111

which(NL$l_switch > 1700) # Rows 85 107 273 282 385
which(NL$l_noswitch > 1300) # Rows 153 353
# We exclude Rows 85 107 273 282 385 153 353

# Create l_Exclude variable
NL$l_Exclude <- 0
NL$l_Exclude[c(111, 85, 107, 273, 282, 385, 153, 353)] <- 1

# 12. Exclude participants with error rate > 20%
par(mfrow = c(1, 1))
hist(NL$l_error, breaks = 300)
NL$l_Exclude[which(NL$l_error > 0.2)] <- 2

mean(NL$l_error) # Overall error rate 8%
mean(NL$l_followe, na.rm = T) # Overall rate of trials following error 
mean(NL$l_outl) # Overall outlier rate 9%
length(which(NL$l_error > 0.2)) # n = 26

#### Task Switching Cost: Number-Letter Task ####
t.test(NL$l_switch[which(NL$l_Exclude == 0)], 
       NL$l_noswitch[which(NL$l_Exclude == 0)], paired = T)
round(mean(NL$l_switch[which(NL$l_Exclude == 0)]), 2)
round(sd(NL$l_switch[which(NL$l_Exclude == 0)]), 2)
round(mean(NL$l_noswitch[which(NL$l_Exclude == 0)]), 2)
round(sd(NL$l_noswitch[which(NL$l_Exclude == 0)]), 2)
library(effsize)
cohen.d(NL$l_switch, NL$l_noswitch, na.rm = T, paired = T)

#### Prep: Navon ####

# Set working directory to data folder
setwd(paste("/Users/andrewang/Documents/iLibrary/UC DAVIS/My Research/",
            "Construal Level Switching/Study 5/Data/Navon",
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
  
  # 8. Delete outliers (+- 2.5 MAD)
  outl <- length(which(abs(dat$RT - median(dat$RT))/mad(dat$RT) >= 2.5))/160
  if (max(abs((dat$RT - median(dat$RT))/mad(dat$RT))) < 2.5) {
    dat <- dat
  } else {
    dat <- dat[-which(
      abs(dat$RT - median(dat$RT))/mad(dat$RT) >= 2.5), ]
  }
  
  # 9. Parcel
  if (dim(dat)[1]%%4 == 0) {
    dat$parID <- rep(1:4, dim(dat)[1]/4)
  } else {
    dat$parID <- c(rep(1:4, dim(dat)[1]/4), 1:(dim(dat)[1]%%4))
    }
  
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
hist(navon$n_HiLo, breaks = 100, xlim = c(0, 1500))
hist(navon$n_LoHi, breaks = 100, xlim = c(0, 1500))
hist(navon$n_LoLo, breaks = 100, xlim = c(0, 1500))
hist(navon$n_HiHi, breaks = 100, xlim = c(0, 1500))

# Boxplots
library(car)
par(mfrow = c(1, 4))
Boxplot(navon$n_HiLo); Boxplot(navon$n_LoHi) # Note that the IDs are just row #
Boxplot(navon$n_LoLo); Boxplot(navon$n_HiHi)

# Which rows contain the outliers? (Note that row number != Subject ID)
which(navon$n_HiLo < 400) # Rows 25 291 370
which(navon$n_LoHi < 350) # Rows 25 291 370
which(navon$n_LoLo < 400) # Rows 25 291 370
which(navon$n_HiHi < 380) # Rows 25 291 370
# We exclude Rows 25 291 370

which(navon$n_HiLo > 1300) # Row /
which(navon$n_LoHi > 1300) # Row 373
which(navon$n_LoLo > 1200) # Row 228
which(navon$n_HiHi > 1100) # Row /
# We exclude Rows 228

# Create n_Exclude variable
navon$n_Exclude <- 0
navon$n_Exclude[c(25, 291, 370, 228)] <- 1

# 11. Exclude participants with error rate > 20%
par(mfrow = c(1, 1))
hist(navon$n_Error, breaks = 300)
navon$n_Exclude[which(navon$n_Error > 0.2)] <- 2

mean(navon$n_Error) # Overall error rate 5%
mean(navon$n_outl) # Overall outlier rate 8%
length(which(navon$n_Error > 0.2)) # n = 15

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

#### Prep: Catexe ####

# Set working directory to data folder
setwd(paste("/Users/andrewang/Documents/iLibrary/UC DAVIS/My Research/",
            "Construal Level Switching/Study 5/Data/Catexe",
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
  
  # 7. Exclude trials with incorrect responses
  dat <- dat[which(dat$Correct == "True"), ]
  
  # 8. Delete outliers (+- 2.5 MAD)
  outl <- length(which(abs(dat$RT - median(dat$RT))/mad(dat$RT) >= 2.5))/160
  if (max(abs((dat$RT - median(dat$RT))/mad(dat$RT))) < 2.5) {
    dat <- dat
  } else {
    dat <- dat[-which(
      abs(dat$RT - median(dat$RT))/mad(dat$RT) >= 2.5), ]
  }
  
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
hist(catexe$c_HiLo, xlim = c(100, 3000), breaks = 200)
hist(catexe$c_LoHi, xlim = c(100, 3000), breaks = 200)
hist(catexe$c_LoLo, xlim = c(100, 3000), breaks = 200)
hist(catexe$c_HiHi, xlim = c(100, 3000), breaks = 200)

# Boxplots
library(car)
par(mfrow = c(1, 4))
Boxplot(catexe$c_HiLo); Boxplot(catexe$c_LoHi) # Note that the IDs are just row
Boxplot(catexe$c_LoLo); Boxplot(catexe$c_HiHi)

# Which rows contain the outliers? (Note that row number != Subject ID)
which(catexe$c_HiLo < 500) # Rows 42 221 223 277 318 368 406
which(catexe$c_LoHi < 500) # Rows 221 223 277 318 368 406
which(catexe$c_LoLo < 500) # Rows 42 221 223 277 318 368 406
which(catexe$c_HiHi < 500) # Rows 42 221 223 277 318 368 406
# We exclude Rows 42 221 223 277 318 368 406

which(catexe$c_HiLo > 2500) # Rows 20 213
which(catexe$c_LoHi > 2500) # Rows 17  20 213
which(catexe$c_LoLo > 2500) # Rows 20 213
which(catexe$c_HiHi > 2500) # Rows 20 213
# We exclude Rows 20 213

# Create c_Exclude variable
catexe$c_Exclude <- 0
catexe$c_Exclude[c(42, 221, 223, 277, 318, 368, 406, 20, 213)] <- 1

# 12. Exclude participants with error rate > 20%
par(mfrow = c(1, 1))
hist(catexe$c_Error, breaks = 300)
catexe$c_Exclude[which(catexe$c_Error > 0.2)] <- 2

mean(catexe$c_Error) # Overall error rate 9%
mean(catexe$c_outl) # Overall outlier rate 8%
length(which(catexe$c_Error > 0.2)) # n = 33


#### Level-Switching Cost: Catexe ####
catexe1 <- catexe[which(catexe$c_Exclude == 0), 1:5]
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


#### Data Merger ####

# Assign NAs to data that should be excluded for each task

colnames(NL)
NL[which(NL$l_Exclude != 0), c(1, 3:20)] <- NA
colnames(navon)
navon[which(navon$n_Exclude != 0), 2:38] <- NA
colnames(catexe)
catexe[which(catexe$c_Exclude != 0), 2:38] <- NA

# Merge three dataframes into one
cvg <- merge(catexe, navon, all.x = TRUE)
cvg <- merge(cvg, NL, all.x = TRUE)
View(cvg)
colnames(cvg)


### Convergent Validity Tests ####

# Raw correlations
cvg$l_sc <- (cvg$l_switch - cvg$l_noswitch)/cvg$l_allsd
cvg$n_sc  <- (cvg$n_Switch - cvg$n_NoSwitch)/cvg$n_allsd
cvg$c_sc  <- (cvg$c_Switch - cvg$c_NoSwitch)/cvg$c_allsd
cor.test(cvg$c_sc, cvg$n_sc)
cor.test(cvg$l_sc, cvg$n_sc)
cor.test(cvg$l_sc, cvg$c_sc)

# CFA

library(lavaan);library(ggplot2);library(corrplot)
library(lmerTest);library(psych);library(semPlot)

# Switching cost by parcel

# Number-Letter Task
cvg$l_sc_p1 <- (cvg$l_switch_p1 - cvg$l_noswitch_p1)/cvg$l_allsd_p1
cvg$l_sc_p2 <- (cvg$l_switch_p2 - cvg$l_noswitch_p2)/cvg$l_allsd_p2
cvg$l_sc_p3 <- (cvg$l_switch_p3 - cvg$l_noswitch_p3)/cvg$l_allsd_p3
cvg$l_sc_p4 <- (cvg$l_switch_p4 - cvg$l_noswitch_p4)/cvg$l_allsd_p4

# Navon
cvg$n_sc_p1  <- (cvg$n_Switch_p1 - cvg$n_NoSwitch_p1)/cvg$n_allsd_p1
cvg$n_sc_p2  <- (cvg$n_Switch_p2 - cvg$n_NoSwitch_p2)/cvg$n_allsd_p2
cvg$n_sc_p3  <- (cvg$n_Switch_p3 - cvg$n_NoSwitch_p3)/cvg$n_allsd_p3
cvg$n_sc_p4  <- (cvg$n_Switch_p4 - cvg$n_NoSwitch_p4)/cvg$n_allsd_p4

# Catexe
cvg$c_sc_p1  <- (cvg$c_Switch_p1 - cvg$c_NoSwitch_p1)/cvg$c_allsd_p1
cvg$c_sc_p2  <- (cvg$c_Switch_p2 - cvg$c_NoSwitch_p2)/cvg$c_allsd_p2
cvg$c_sc_p3  <- (cvg$c_Switch_p3 - cvg$c_NoSwitch_p3)/cvg$c_allsd_p3
cvg$c_sc_p4  <- (cvg$c_Switch_p4 - cvg$c_NoSwitch_p4)/cvg$c_allsd_p4

# Together
colnames(cvg)
corrplot(cor(cvg[, 101:112], use = "pairwise.complete.obs"), method = "square",
         shade.col = NULL, tl.col = "black", tl.srt = 45, tl.cex = 1)
round(cor(cvg[, 101:112], use = "pairwise.complete.obs"), 2)


# Model 1
sc_mod1 <- "
l_sc =~ l_sc_p1 + l_sc_p2 + l_sc_p3 + l_sc_p4
n_sc =~ n_sc_p1 + n_sc_p2 + n_sc_p3 + n_sc_p4
c_sc =~ c_sc_p1 + c_sc_p2 + c_sc_p3 + c_sc_p4
"
sc_fit1 <- cfa(sc_mod1, data = cvg, std.lv = TRUE)
summary(sc_fit1, fit.measures = TRUE, standardized = TRUE)
labels <- c("NL1", "NL2", "NL3", "NL4", "Navon1", "Navon2", "Navon3", "Navon4",
            "Catexe1", "Catexe2", "Catexe3", "Catexe4", "NL", "Navon", "Catexe")
semPaths(sc_fit1, "std", fade = F, style = "lisrel",
         edge.color = 'black', esize = T, nodeLabels = labels,
         edge.label.cex = 0.8, sizeMan = 6, sizeLat = 8)

# Model 2
sc_mod2 <- "
l_sc =~ l_sc_p1 + l_sc_p2 + l_sc_p3 + l_sc_p4
n_sc =~ n_sc_p1 + n_sc_p2 + n_sc_p3 + n_sc_p4
c_sc =~ c_sc_p1 + c_sc_p2 + c_sc_p3 + c_sc_p4

l_sc ~~ v1*n_sc
l_sc ~~ v1*c_sc
c_sc ~~ v1*n_sc
"
sc_fit2 <- cfa(sc_mod2, data = cvg, std.lv = TRUE)
summary(sc_fit2, fit.measures = TRUE, standardized = TRUE)
lavTestLRT(sc_fit1, sc_fit2) # Model 1 fits significantly better than Model 2

# Model 3
sc_mod3 <- "
l_sc =~ l_sc_p1 + l_sc_p2 + l_sc_p3 + l_sc_p4
n_sc =~ n_sc_p1 + n_sc_p2 + n_sc_p3 + n_sc_p4
c_sc =~ c_sc_p1 + c_sc_p2 + c_sc_p3 + c_sc_p4

l_sc ~~ v1*n_sc
l_sc ~~ v1*c_sc
c_sc ~~ v2*n_sc
"
sc_fit3 <- cfa(sc_mod3, data = cvg, std.lv = TRUE)
summary(sc_fit3, fit.measures = TRUE, standardized = TRUE)
lavTestLRT(sc_fit1, sc_fit3) # Model 1 fits significantly better than Model 3
lavTestLRT(sc_fit2, sc_fit3) # Model 2 and 3 are similar
