library("reshape2")
library("tidyverse")
library("psych")
#Load data set
womenshealth <- read.delim("~/Desktop/Uni/Master/1. Semester/Advanced Scientific Methods/Assignments/womenshealth.txt")
View(womenshealth)
#Drop missing data
womenshealth = womenshealth %>% 
  drop_na()
#Assign vectors
subno <- cbind(womenshealth$subno) 
timedrs <- cbind(womenshealth$timedrs)
attdrug <- cbind(womenshealth$attdrug)
atthouse <- cbind(womenshealth$atthouse)
income <- cbind (womenshealth$income)
emplmnt <- cbind(womenshealth$emplmnt)
mstatus <- cbind(womenshealth$mstatus)
race <- cbind(womenshealth$race)
log10.ltimedrs <- cbind(womenshealth$log10.ltimedrs)
#Create matrix
matrix_wom <- cbind(subno, timedrs,attdrug, atthouse, income, emplmnt, mstatus, race, log10.ltimedrs)
k <-ncol(matrix_wom)
n <- nrow(matrix_wom)
#Create matrix of means
matrix_mean <- matrix(data=1, nrow=n) %*% 
  colMeans(matrix_wom)
#Create difference matrix
diff_wom <- matrix_wom - matrix_mean
#Covariance matrix
cov_wom <- (n-1)^-1 * t(diff_wom) %*% diff_wom
#Correlation matrix
var_vector_wom <-diag(cov_wom)^(-1/2) 
diag(var_vector_wom)%*% cov_wom %*% diag(var_vector_wom)


