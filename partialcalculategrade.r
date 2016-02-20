## This for partial grades
# Get grades per section
grades <- read.csv("grades038.csv") # for the other section
# if additional class is needed, do grades_1 <- read.csv(...) and so on, then row
# bind them to make the variable grades.
# replace all NA with 0 except comments column
comments <- grades[,ncol(grades)]
grades <- grades[,-ncol(grades)]
library(dplyr) # load dplyr
Personalinfo <- grades[,c("LastName", "FirstName", "Sctn_Code", "Pid")]
projs <- select(grades, contains("lab")) # there is a function called labs
webwork <- select(grades, contains("HW")) # all hw
exams <- select(grades, contains("ex"))
exams[is.na(exams)] <- 0 # put 0 in place of NA
final <- select(grades, contains("final"))
final[is.na(final)] <- 0 # put 0 in place of NA

# See the first column with all NA! that tells you how many labs were done
m <- which(apply(is.na(projs[,1:ncol(projs)]), 2, all))[1] # first col with all NA
if(is.null(m) | is.na(m)){
  m = 12
}
projs <- projs[,1:(m-1)] # retain the labs that were completed
projs[is.na(projs)] <- 0

# order each row of labs from largest to smallest.
# note that after this lab1 is no longer lab1 etc...
for(i in 1:nrow(projs)){
  projs[i,] <- projs[i,order(projs[i,], decreasing = TRUE)]  
}


# remove lowest 2 grades
projs <- projs[,-c(ncol(projs), ncol(projs) - 1)]
PLS <- 10*ncol(projs) # Possible Lab Score... change this to get partial grades
projs <- mutate(projs, lScore = rowSums(projs), ldScore = lScore/PLS) # ldScore: lab decimal score
PES <- 100 # Possible Exam Score, assuming each is worth 100
exams <- mutate(exams, edScore1 = Ex1/PES, edScore2 = Ex2/PES) # edScore: exam decimal score
PFS <- 100 # Possible Final Score
final <- mutate(final, fdScore = FINAL/PFS)

Windex <- which(grades$LastName == "MAX") # index for max values per hw
# b/c max will be 100 and we have to check column with all 0
wtemp <- webwork[-Windex,] # remove the max out
# See the first column with all NA! that tells you how many HW were due
m <- which(apply(wtemp[,1:ncol(wtemp)] == 0, 2, all))[1] # first col with all 0
if(!is.na(m)){
webwork <- webwork[,1:(m-1)] # retain the hw that were due
}

Vec_max <- webwork[Windex[1],] # The max per hw
PWS <- ncol(webwork) # Possible maximum webwork score... obsoete now

for(i in 1:nrow(webwork)){
  webwork[i,] <- webwork[i,]/Vec_max # get proportion per hw.
}
webwork <- mutate(webwork, wwork = rowSums(webwork)/PWS)

##### classGrades
cGrades <- cbind(Personalinfo, webwork, projs, exams, final) %>% 
  mutate(total = (15*wwork + 15*ldScore + 20*edScore1 + 20*edScore2 + 30*fdScore), 
         GPA = ifelse(total >= 90, 4.0, ifelse(total >= 85, 3.5, 
                                               ifelse(total >= 79, 3.0, 
                                                      ifelse(total >= 73, 2.5,
                                                             ifelse(total >= 65, 2.0,
                                                                    ifelse(total >= 60, 1.5, 
                                                                           ifelse(total >= 55, 1.0, 0.0))))))),
         LGrade = ifelse(GPA == 4.0, "A", ifelse(GPA == 3.5, "B+",
                                                 ifelse(GPA == 3.0, "B", 
                                                        ifelse(GPA == 2.5, "C+",
                                                               ifelse(GPA == 2.0, "C",
                                                                      ifelse(GPA == 1.5, "D+",
                                                                             ifelse(GPA == 1.0, "D", "F"))))))))
### An experiment with full grades works

gd <- function(x,y){
  a <- filter(cGrades, LastName == x) %>% select(c(wwork, ldScore, Ex1, Ex2, total)) %>% 
    mutate(percentagenow = 100*total/70, Need_final = y - total, 
           Need_final_percent = 100*Need_final/30)
  return(a)
}