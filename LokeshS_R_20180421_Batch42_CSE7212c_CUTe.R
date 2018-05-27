rm(list=ls(all.names=TRUE))

## 1. Create a vector that contains a sequence of numbers from 1000 to 10000 
#     and filter out only those elements which are divisible by 7 and 19

vect <- c(1000:10000)

vecDiv7 <- vect[vect %% 7 == 0]
vecDiv7

vecDiv19 <- vect[vect %% 19 == 0]
vecDiv19

## 2.Create an R function that takes an integer "n" 
#    as the input and returns the nth number in the fibonacci sequence


## function to print the nth number of the fibonacci sequence
fib <- function(n){
  fibvals <- numeric(n)
  fibvals[1] <- 1
  fibvals[2] <- 1
  for (i in 3:n) { 
    fibvals[i] <- fibvals[i-1]+fibvals[i-2]
  } 
  fibvals[n]
}


fib(15)

### Questions using the datasets

## 3. Read the files relating to top 50 students from all the 5 colleges  

col1 <- read.csv("D:/INSOFE/CUTe01/data/COL1.csv")
col2 <- read.csv("D:/INSOFE/CUTe01/data/COL2.csv")
col3 <- read.csv("D:/INSOFE/CUTe01/data/COL3.csv")
col4 <- read.csv("D:/INSOFE/CUTe01/data/COL4.csv")
col5 <- read.csv("D:/INSOFE/CUTe01/data/COL5.csv")

## 4.  Observe the data distribution of each column in all the 5 dataframes. 
#       For each college

### a. Report if the summary of the variable

summary(col1)

summary(col2)

summary(col3) # Acad_score average and overall score avg is greater compared to other colleges

summary(col4) 

summary(col5)

### b. Also report if any one category in categorical attribute has dominance over other categories. 
#   Hint: Can this be answered by observing the counts of each level

str(col1)

str(col2)

str(col3)

str(col4)

str(col5)

#Note :  Every college CollegeID is have only one level so this can be dominant over other categories


## c. Find attributes that have missing value

colSums(is.na(col1))
colSums(is.na(col2))
colSums(is.na(col3))
colSums(is.na(col4))
colSums(is.na(col5))


## d. Report how many missing values are present in each file

sum(is.na(col1)) # 9 missing values are present in col1 file

sum(is.na(col2)) # 6 missing values are present in col2 file

sum(is.na(col3)) # 19 missing values are present in col3 file

sum(is.na(col4)) # 9 missing values are present in the col4 file

sum(is.na(col5)) # 9 missing values are present in col5 files


## 5. In each of the files fill missing values and explain the imputation strategy 

library(DMwR)

# Here Acad_Score is numeric attribute and Behaviour_type is factorial attribute

## 6. Combine all those data frames into a single, consolidated data frame, 
#     and name it as "consolidated_data

consolidated_data <- rbind(col1, col2, col3, col4, col5)

# Every null values are replaced using the central imputation strategy
consolidated_data <- centralImputation(consolidated_data)
consolidated_data

## 7.  Do range normalisation of the numeric variable

consolidated_data_NumAtr <- subset(consolidated_data, select = 
                                     c("Acad_Score", "Overall_Score"))

library(vegan)

Range_Norm <- decostand(consolidated_data_NumAtr,
                                "range")

Range_Norm
 