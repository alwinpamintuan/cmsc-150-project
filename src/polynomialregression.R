options(digits=5)

PartialPivotingPR <- function(augcoeffmatrix,i){
  maxElement = max(abs(augcoeffmatrix[i:nrow(augcoeffmatrix),i])) #get pivot element
  pivotRow = match(maxElement,augcoeffmatrix[,i]) #get index of pivot element to determine pivot row
  
  #since abs() was used on maxElement, check for -maxElement if maxElement is not found
  if(is.na(pivotRow)){
    maxElement = maxElement*-1
    pivotRow = match((maxElement),augcoeffmatrix[,i])
  }
  
  if(augcoeffmatrix[pivotRow,i]==0) return(NA) #no unique solution exists
  
  #Swap ith column with pivot row
  temp = augcoeffmatrix[i,]
  augcoeffmatrix[i,] = augcoeffmatrix[pivotRow,]
  augcoeffmatrix[pivotRow,] = temp
  return(augcoeffmatrix)
}

GaussJordanEliminationPR <- function(augcoeffmatrix){
  solutionSet = c()
  
  for(i in 1:nrow(augcoeffmatrix)){
    if(i != nrow(augcoeffmatrix)) augcoeffmatrix = PartialPivotingPR(augcoeffmatrix,i) #partial pivoting
    augcoeffmatrix[i,] = (augcoeffmatrix[i,])/(augcoeffmatrix[i,i])                  #find a[i,]
    
    for(j in 1:nrow(augcoeffmatrix)){
      if(i==j){next}
      normalRow = augcoeffmatrix[j,i] * augcoeffmatrix[i,]  #find normalized row
      augcoeffmatrix[j,] = augcoeffmatrix[j,] - normalRow   #find a[j,]: a[j,] - NORMALIZED ROW
    }
  }
  
  RHS = augcoeffmatrix[,ncol(augcoeffmatrix)]
  for(i in 1:nrow(augcoeffmatrix)) solutionSet[i] = RHS[i] 
  
  return(solutionSet)
}

#set-ups the augmented coefficient matrix given the degree of the
#regression and the vectors of independent and dependent variables
setupACM <- function(indep,dep,degree){
  n = degree
  acm = matrix(c(0), nrow = n+1, ncol = n+2) #creates matrix of n+1 rows and n+2 cols

  #assign values for augmented coefficient matrix
  for(i in 1:(n+1)){
    for(j in 0:n){
      acm[i,(j+1)] = sum(indep^((i-1)+j))
    }
    acm[i,(n+2)] = sum(indep^(i-1) * dep)
  }
  
  return(acm)
}

PolynomialRegression <- function(df,degree){
  indep = df[,1]
  dep = df[,2]
  
  #if degree<1, regression not valid
  #if length(x_vector)!=length(y_vector), not valid
  if(degree >= length(unique(indep))) degree = length(unique(indep))-1
  if(degree<1 || length(indep)!=length(dep)) return(NA)

  acm = setupACM(indep,dep,degree) #set-up the augmented coefficient matrix
  solution = GaussJordanEliminationPR(acm) #solve augmented coefficient matrix using gauss-jordan elimination
  
  #complete polynomial regression string
  str_function = "function(x)"
  for(i in 1:(degree+1)){
    var = gsub(" ","",paste("x^",i-1))
    term = paste(round(solution[i],digits = 4),"*",var)
    str_function = paste(str_function,term)
    if(i!=(degree+1)) str_function = paste(str_function,"+")
  }

  #converts polynomial string to a function and evaluates said function
  fxn <- function(x){
    eval(parse(text = str_function))
  }

  return(list(fxn=fxn(x),strfxn = str_function))
}