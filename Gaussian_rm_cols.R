# This Function Simplifies The Coefficients of A Matrix by row
simp_mat <- function(D){
  if(!is.matrix(D)){
    stop("Matrices Only: Your Input Is Not A Matrix")
  }
  for(i in 1:nrow(D)){
    div <- D[i, D[i,] != 0][1] # Divisor: First Non-Zero Term
    # In The Row ... This Is Just To Make Sure That My Gaussian
    # Elmination Returns 1 As Leading Coef. I Could Do min(...)
    
    D[i,] <- D[i,] / div
  }
  return(D)
}

# This checks whether two vectors are linerarily independent
Linear_check <- function(x,y){
  if(!is.vector(x) || !is.vector(y)){
    return("Please Enter Vectors Only")
  } else {
    if(all(x==0) || all(y==0)){
      return(FALSE) # Just Ignore Zero Vectors
    } else {
      k <- min(x[x!=0]) # Smallest non zero coef of x
      n <- min(y[y!=0]) # Smallest non zero coef of y
      x <- x/k # Simplify x to lowest coefs
      y <- y/n # Simplify y to lowest coefs
      return(all(x==y))
    }
  }
}

# This function removes zero rows and columns of a matrix
Zero_elim <- function(B){
  if(!is.matrix(B)){
    stop("Matrices Only: Your Input Is Not A Matrix")
  }
  m_rows <- nrow(B) # Matrix nber of rows
  m_cols <- ncol(B) # Matriz nber of columns
  Z_rows <- NULL # Zero Rows.
  Z_cols <- NULL # Zero Cols.
  
  # Get and Remove All Zero Rows
  for(i in 1:m_rows){
    if(all(B[i,] == 0)){
      Z_rows <- c(Z_rows, i)
    }
  }
  if(!is.null(Z_rows)){
    B <- B[-1 * Z_rows, ] # Remove the zero rows
  }
  
  # Get and remove All zero Columns
  for(j in 1:m_cols){
    if(all(B[,j] == 0)){
      Z_cols <- c(Z_cols, j)
    }
  }
  if(!is.null(Z_cols)){
    B <- B[, -1 * Z_cols] # Remove the zero columns
  }
  
  return(B)
}

# My Gaussian Elimination

Gaussian_rref <- function(A, simplify = FALSE){
  if(!is.matrix(A)){
    stop("Matrices Only: Your Input Is Not A Matrix")
  }
  # There should be not be more rows than columns.
  m_rows <- nrow(A) # Matrix nber of rows
  m_cols <- ncol(A) # Matriz nber of columns
  
  B <- Zero_elim(A) # Remove all zero rows
  
  new_m_rows <- nrow(B) # New nber of rows after removing 0 rows
  new_m_cols <- ncol(B)
  
  # Check if any two rows are linearly dependent
  # If so, Remove duplicates
  for(i in 1:(new_m_rows - 1)){
    for(j in (i + 1):new_m_rows){
      if(Linear_check(B[i,], B[j,])){
        B[j,] <- rep(0, new_m_cols) # Set duplicates to 0 vectors
      }
    }
  }
  
  # Now removes all zero rows... i.e. duplicate rows
  B <- Zero_elim(B)
  new_m_rows <- nrow(B) # New nber of rows after removing 0 rows
  new_mcols <- ncol(B) # New nber of cols after removing 0 cols
  
  if(is.null(new_m_rows)){
    stop("All Rows Are The Same: Pick Some Variables And
           Solve For The Last One")
  }
  
  if(new_m_rows < 2){
    message("Many Rows Seem To Be Linearly Dependent")
    return(B)
  }
  
  # After linearly dependent rows are taken out.
  if(new_m_rows >= new_m_cols){
    stop("Please See That There Are More Columns Than Rows:
         I took away duplicate rows and still face issues")
  }
  
  # Now Let's Focus On The Elimination
  for(i in 1:(new_m_rows - 1)){
    # Get lowest coefficient for cuurent variable
    ind <- which(B[,i] == min(B[B[,i] != 0, i]))[1]
    temp <- B[i,] # Save first row as temporary
    B[i,] <- B[ind,] # Set first row to have lowest coef
    B[ind,] <- temp # Complete the swap of rows
    
    pivot <- B[i,i] # Get leading coef of pivot row
    
    for(j in (i+1):new_m_rows){
      mul = B[j,i] # Get leading coef of current row
      # Update current row to eliminate leading coef
      B[j,] <- mul*B[i,] - pivot*B[j,]
    }
  } 
  
  # Simplify Coefficients : This Can Be Turned On Or Off
  if(simplify) {
    return(simp_mat(B))
  } else {
    return(B)
  }
}

## Compute Determinant of a matrix

# I will use the row opertations to get the determinant
# For this I will readjust the Gaussian_rref function

Gaussian_rref_adj <- function(A){
  # We mus not simplify
  # Adjust so that we keep track of any swap of rows
  # Adjust so that we only subtract or add the pivot row
  # Remove unecessary lines
  # At this I just realized that this does not guarantee
  # That the matrix comes out as Upper triangular
  m_rows <- nrow(A) # Matrix nber of rows
  m_cols <- ncol(A) # Matrix nber of columns
  B <- A # Just assign A to B. This keeps me from changing all
  # instances of B in the code
  # Now Let's Focus On The Elimination
  count <- 0 # This will determine how many time we swap rows
  for(i in 1:(m_rows - 1)){
    # Get lowest coefficient for cuurent variable
    ind <- which(B[,i] == min(B[B[,i] != 0, i]))[1]
    # This determines if we have to swap rows
    if(ind != i){
      # I just realized that this if statement can save time
      # I will do the same to the previous Gaussian_rref
      count <- count + 1
      temp <- B[i,] # Save first row as temporary
      B[i,] <- B[ind,] # Set first row to have lowest coef
      B[ind,] <- temp # Complete the swap of rows
    }
    
    pivot <- B[i,i] # Get leading coef of pivot row
    
    for(j in (i+1):m_rows){
      mul = B[j,i] # Get leading coef of current row
      # Update current row to eliminate leading coef
      B[j,] <- B[j,]  - (mul/pivot)*B[i,]
    }
  } 
  
  return(B)
}

Rref_det <- function(A){
  if(!is.matrix(A)){
    stop("Matrices Only!")
  }
  m_rows <- nrow(A) # Matrix rows
  m_cols <- ncol(A) # Matrix columns
  # No Determinant for non-square matrices
  if(m_rows != m_cols){
    stop("Cannot Perform This Operation On A Non-Sqaure Matrix")
  }
  # If There are any zero rows or columns then return 0
  # m_rows = m_cols so we can check both at once
  for(i in 1:m_rows){
    if(all(A[i,] == 0) || all(A[,i] == 0)) {
      return(0)
    }
  }
  # If There are any 2 linearly dependent rows or columns => 0
  for(i in 1:(m_rows - 1)){
    for(j in (i + 1):m_rows){
      if(Linear_check(A[i,], A[j,]) || 
           Linear_check(A[,i], A[,j])){
        return(0)    
      }
    }
  }
  # Now we can perform the adjusted rref function
}