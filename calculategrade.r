# Get grades per section
grades <- read.csv("grades038.csv") # for the other section
# if additional class is needed, do grades_1 <- read.csv(...) and so on, then row
# bind them to make the variable grades.
# replace all NA with 0 except comments column
comments <- grades[,ncol(grades)]
grades <- grades[,-ncol(grades)]
grades[is.na(grades)] <- 0
library(dplyr) # load dplyr
Personalinfo <- grades[,c("LastName", "FirstName", "Sctn_Code", "Pid")]
projs <- select(grades, contains("lab")) # there is a function called labs
webwork <- select(grades, contains("HW")) # all hw
exams <- select(grades, contains("ex"))
final <- select(grades, contains("final"))

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
PFS <- 200 # Possible Final Score
final <- mutate(final, fdScore = FINAL/PFS)

Windex <- which(grades$LastName == "MAX") # index for max values per hw

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
# x is the lastname
gd_final <- function(x){
  return(filter(cGrades, LastName == x) %>% select(c(wwork, ldScore, Ex1, Ex2, FINAL, total, GPA,
                                                     LGrade)))
}