load('continue_random.RData')
if(length(vec) < 1){
  vec <- readLines("MTH124_038.txt") ## do not run this next time, load workspace and run rest 
}

while(readline("Space or Enter | any other key to quit") == ""){
  a <- sample(length(vec), 1) # pick a number between 1 and nber of students
  print(vec[a]) # print student name
  vec <- vec[-a] # remove it
}

save(vec, file = "continue_random.RData")