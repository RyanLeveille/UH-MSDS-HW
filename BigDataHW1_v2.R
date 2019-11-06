#setwd("C:/Users/steven/Dropbox/UH Study/A Summer 2019/Big Data Analytics/HW1")
setwd("E:/E_Downloads/big_data_hw1/PAMAP2_Dataset/Protocol")
getwd()

#load packages 
library(bit)
library(ff)
library(ffbase)

#Path for the temp dir.
options(fftempdir = "E:/E_Downloads/ffdf")

#========================================================================== 
# P1-Solution 1. ff package
#========================================================================== 

subject1 = read.table.ffdf(file="subject101.dat", 
                                          VERBOSE=TRUE, 
                                          header=F, next.rows=100000, colClasses=NA)
subject2 = read.table.ffdf(file="subject102.dat", 
                                          VERBOSE=TRUE, 
                                          header=F, next.rows=100000, colClasses=NA)
subject3 = read.table.ffdf(file="subject103.dat", 
                                          VERBOSE=TRUE, 
                                          header=F, next.rows=100000, colClasses=NA)
subject4 = read.table.ffdf(file="subject104.dat", 
                                          VERBOSE=TRUE, 
                                          header=F, next.rows=100000, colClasses=NA)
subject5 = read.table.ffdf(file="subject105.dat", 
                                          VERBOSE=TRUE, 
                                          header=F, next.rows=100000, colClasses=NA)
subject6 = read.table.ffdf(file="subject106.dat", 
                                          VERBOSE=TRUE, 
                                          header=F, next.rows=100000, colClasses=NA)
subject7 = read.table.ffdf(file="subject107.dat", 
                                          VERBOSE=TRUE, 
                                          header=F, next.rows=100000, colClasses=NA)
subject8 = read.table.ffdf(file="subject108.dat", 
                                          VERBOSE=TRUE, 
                                          header=F, next.rows=100000, colClasses=NA)
subject9 = read.table.ffdf(file="subject109.dat", 
                                          VERBOSE=TRUE, 
                                          header=F, next.rows=100000, colClasses=NA)

# size of the object
object.size(subject1)

str(subject1)
dim(subject1)
dimnames(subject1)

# Merge together (only works for data frame type????? too large)
# subject1 = as.data.frame(subject1)
# subject2 = as.data.frame(subject2)
# df = merge(subject1, subject2, by=intersect(names(subject1), names(subject2)), 
#            all.x=T, all.y=T) # ?????????????????
# merge others...

###########################
###### Preprocessing ######
###########################

# Each of the data-files contains 54 columns per row, the columns contain the following data:
# – 1 timestamp (s)
# – 2 activityID (see II.2. for the mapping to the activities)
# – 3 heart rate (bpm)
# – 4-20 IMU hand
# – 21-37 IMU chest
# – 38-54 IMU ankle
# The IMU sensory data contains the following columns:
# – 1 temperature (°C)
# – 2-4 3D-acceleration data (ms-2), scale: ±16g, resolution: 13-bit (X, Y, Z)
# – 5-7 3D-acceleration data (ms-2), scale: ±6g, resolution: 13-bit*
# – 8-10 3D-gyroscope data (rad/s)
# – 11-13 3D-magnetometer data (μT)
# – 14-17 orientation (invalid in this data collection)

# hand X, Y, Z:  v14, v15, v16
# chest X, Y, Z: v31, v32, v33
# ankle X, Y, Z: v48, v49, v50

# Renaming col names
colnames(subject1)[1] = "timestamp"
colnames(subject1)[2] = "activityID"
colnames(subject1)[3] = "hr"

colnames(subject1)[5] = "hand_x"
colnames(subject1)[6] = "hand_y"
colnames(subject1)[7] = "hand_z"

colnames(subject1)[22] = "chest_x"
colnames(subject1)[23] = "chest_y"
colnames(subject1)[24] = "chest_z"

colnames(subject1)[39] = "ankle_x"
colnames(subject1)[40] = "ankle_y"
colnames(subject1)[41] = "ankle_z"

colnames(subject1)

# Save dat to csv

# write.csv.ffdf(subject1, "subject1.csv", VERBOSE = TRUE)

write.table(subject1, "subject1.csv", sep = ",", 
            row.names = FALSE, col.names = TRUE)
write.table(subject2, "subject2.csv", sep = ",", 
            row.names = FALSE, col.names = TRUE)
write.table(subject3, "subject3.csv", sep = ",", 
            row.names = FALSE, col.names = TRUE)
write.table(subject4, "subject4.csv", sep = ",", 
            row.names = FALSE, col.names = TRUE)
write.table(subject5, "subject5.csv", sep = ",", 
            row.names = FALSE, col.names = TRUE)
write.table(subject6, "subject6.csv", sep = ",", 
            row.names = FALSE, col.names = TRUE)
write.table(subject7, "subject7.csv", sep = ",", 
            row.names = FALSE, col.names = TRUE)
write.table(subject8, "subject8.csv", sep = ",", 
            row.names = FALSE, col.names = TRUE)
write.table(subject9, "subject9.csv", sep = ",", 
            row.names = FALSE, col.names = TRUE)

#========================================================================== 
# P1-Solution 2. bigmemory
#========================================================================== 

library(bigmemory)

subject.mat1 = read.big.matrix("subject1.csv", header = T, sep = ",", 
                                type = "double" )
subject.mat2 = read.big.matrix("subject2.csv", header = T, sep = ",",
                                type = "double" )
subject.mat3 = read.big.matrix("subject3.csv", header = T, sep = ",",
                                type = "double" )
subject.mat4 = read.big.matrix("subject4.csv", header = T, sep = ",",
                                type = "double" )
subject.mat5 = read.big.matrix("subject5.csv", header = T, sep = ",",
                                type = "double" )
subject.mat6 = read.big.matrix("subject6.csv", header = T, sep = ",",
                                type = "double" )
subject.mat7 = read.big.matrix("subject7.csv", header = T, sep = ",",
                                type = "double" )
subject.mat8 = read.big.matrix("subject8.csv", header = T, sep = ",",
                                type = "double" )
subject.mat9 = read.big.matrix("subject9.csv", header = T, sep = ",",
                                type = "double" )

object.size(subject.mat1)
dim(subject.mat1) 

#========================================================================== 
# P1-Solution 3. parallel  # ????????????????????????
#========================================================================== 
library(snow)
library(parallel)


numOfProcessors = detectCores(logical = TRUE) 
# logical = TRUE --> detects number of logical CPUs/cores
# logical = FALSE --> detects number of physical CPU/cores

# use these processors as nodes in a cluster for parallel processing   
cl = makeCluster(numOfProcessors-1, type = "SOCK") 
# SOCK is one of the two types of cluster communication 
#Generally, it is advisable to create a cluster with n number of nodes,
#where n = detectCores() - 1, This approach allows us to benefit from multi-threading, 
#without putting an excessive pressure on other processes or applications that may be run in parallel.
cl


#The parallel package allows the execution of apply() operations on each node 
#in the cluster through the clusterApply() function and parallelized implementations 
#of the apply() family of functions parLapply(), parSapply(), and parApply().

subject1.df = as.data.frame(subject1)

system.time(meanbig = clusterApply(cl, subject1.df, fun=mean, na.rm=TRUE))
meanbig

system.time(meanbig2 = parSapply(cl, subject1.df, FUN = mean, na.rm=TRUE))
meanbig2

system.time(meanbig3 = mclapply(subject1.df, FUN=mean, na.rm=TRUE, mc.cores=1))
#which is a parallelized version of lapply() relying on forking 
#(not available on Windows unless mc.cores = 1).

meanbig3

#Once the parallel jobs are complete it is a good habit to close all connections with the 
#following statement:
stopCluster(cl)

dim(subject1.df)

head(subject1.df)

#========================================================================== 
# P2-Solution 1. Signals 
#========================================================================== 

# Apply equation
subject1$e_hand = sqrt(subject1$hand_x^2 + subject1$hand_y^2 + 
                         subject1$hand_z^2)
subject1$e_chest = sqrt(subject1$chest_x^2 + subject1$chest_y^2 + 
                          subject1$chest_z^2)
subject1$e_ankle = sqrt(subject1$ankle_x^2 + subject1$ankle_y^2 + 
                          subject1$ankle_z^2)

#========================================================================== 
# P2-Solution 2. Signals 
#========================================================================== 

# Apply equation
# Need to add new columns
add.cols(subject.mat1, 1, column.names="newcol")  #???????

subject.mat1[, "e_hand"] = sqrt(subject.mat1[, "hand_x"]^2 + subject.mat1[, "hand_y"]^2 + 
                             subject.mat1[, "hand_z"]^2)
subject.mat1[, "e_chest"] = sqrt(subject.mat1[, "chest_x"]^2 + subject.mat1[, "chest_y"]^2 + 
                                  subject.mat1[, "chest_z"]^2)
subject.mat1[, "e_ankle"] = sqrt(subject.mat1[, "ankle_x"]^2 + subject.mat1[, "ankle_y"]^2 + 
                                  subject.mat1[, "ankle_z"]^2)

#========================================================================== 
# P3-Solution 1. Correlation matrix 
#==========================================================================

cor(subject1[, c("e_hand", "e_chest", "e_ankle", "hr")], use = "complete.obs")

# e_hand vs e_chest:  +0.62, strong positive correlation.
# e_hand vs e_ankle:  +0.52, mid-strength positive correlation.
# e_hand vs hr:       -0.08, weak negative correlation.
# e_chest vs e_ankle: +0.58, mid-strength positive correlation.
# e_chest vs hr:      -0.12, weak negative correlation.
# e_ankle vs hr:      -0.11, weak negative correlation.

#========================================================================== 
# P3-Solution 2. Correlation matrix 
#==========================================================================

cor(subject.mat1[, c("e_hand", "e_chest", "e_ankle", "hr")], use="complete.obs")

# e_hand vs e_chest:  +0.68, strong positive correlation.
# e_hand vs e_ankle:  +0.52, mid-strength positive correlation.
# e_hand vs hr:       -0.08, weak negative correlation.
# e_chest vs e_ankle: +0.58, mid-strength positive correlation.
# e_chest vs hr:      -0.12, weak negative correlation.
# e_ankle vs hr:      -0.11, weak negative correlation.

#========================================================================== 
# P4-Solution 1. Multiple linear regression
#==========================================================================

library(biglm)
library(biganalytics)
regress1 = bigglm.big.matrix(activityID ~ e_hand + e_chest + e_ankle + hr,
                              data = subject1)

summary(regress1)

#========================================================================== 
# P4-Solution 2. Multiple linear regression
#==========================================================================

library(biglm)
library(biganalytics)
regress1 = bigglm.big.matrix(activityID ~ e_hand + e_chest + e_ankle + hr,
                              data = subject.mat1)

summary(regress1)













