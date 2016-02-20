# A function to randomize my list of student into groups of 4 or 3
# if size of class mod 4 is 1 then take away 9 random people and sample so that
# we get 3 groups of three. then the rest must be even.
# If size mod 4 is 2 then simply take away 6 random people and have 2 groups of 3
# if size mod 4 is 3 then we don't really need anything to do. just take out 3 
# random people. If size mod 4 is zero then we are good to go. so long as Size 
# is at least 6 or 9 we are ok

# For simplicity we will assume that no names are the same. Otherwise we
# can just quickly modify the names to keep our code the same

randomizeClass <- function(vec){
  set.seed(2015)
  Size <- length(vec)
  if(Size %% 4 == 0){
    df <- data.frame(matrix(nrow = 4, ncol = (Size/4)))
    for(i in 1:(Size/4)){
      names(df)[i] <- paste("Group_", i, sep = "")
      a <- sample(vec, 4)
      vec <- setdiff(vec, a)
      df[[i]] <- a
    }
  } else if(Size %% 4 == 1){
    a <- sample(vec, 9)
    vec <- setdiff(vec, a)
    G1 <- c(sample(a, 3), NA)
    G2 <- c(sample(setdiff(a,G1[1:3]), 3), NA)
    G3 <- c(setdiff(a, union(G1[1:3], G2[1:3])), NA)
    
    df <- data.frame(matrix(nrow = 4, ncol = (((Size - 9)/4) + 3)))
    names(df)[1:3] <- c("Group_1", "Group_2", "Group_3")
    df[[1]] <- G1; df[[2]] <- G2; df[[3]] <- G3
    for(i in 4:(((Size - 9)/4) + 3)){
      names(df)[i] <- paste("Group_", i, sep = "")
      a <- sample(vec, 4)
      vec <- setdiff(vec, a)
      df[[i]] <- a
    }
  } else if(Size %% 4 == 2){
    a <- sample(vec, 6)
    vec <- setdiff(vec, a)
    G1 <- c(sample(a, 3), NA)
    G2 <- c(setdiff(a,G1[1:3]), NA)
    
    df <- data.frame(matrix(nrow = 4, ncol = (((Size - 6)/4) + 2)))
    names(df)[1:2] <- c("Group_1", "Group_2")
    df[[1]] <- G1; df[[2]] <- G2
    for(i in 3:(((Size - 6)/4) + 2)){
      names(df)[i] <- paste("Group_", i, sep = "")
      a <- sample(vec, 4)
      vec <- setdiff(vec, a)
      df[[i]] <- a
    }
  } else {
    a <- sample(vec, 3)
    vec <- setdiff(vec, a)
    G1 <- c(a, NA)
    df <- data.frame(matrix(nrow = 4, ncol = (((Size - 3)/4) + 1)))
    names(df)[1] <- "Group_1"
    df[[1]] <- G1
    for(i in 2:(((Size - 3)/4) + 1)){
      names(df)[i] <- paste("Group_", i, sep = "")
      a <- sample(vec, 4)
      vec <- setdiff(vec, a)
      df[[i]] <- a
    }
  }
  
  return(df)
}

vec <- readLines("MTH012.txt")
Class_Groups <- randomizeClass(vec)