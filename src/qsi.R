options(digits = 5)

PartialPivoting <- function(augcoeffmatrix,i){
  maxElement = max(abs(augcoeffmatrix[i:nrow(augcoeffmatrix),i])) #get pivot element
  pivotRow = match(maxElement,augcoeffmatrix[,i]) #get index of pivot element to determine pivot row

  #since abs() was used on maxElement, check for -maxElement if maxElement is not found
  if(is.na(pivotRow)){
    maxElement = maxElement*-1
    pivotRow = match((maxElement),augcoeffmatrix[,i])
  }

  if(augcoeffmatrix[pivotRow,i]==0) return(NA) #no unique solution exists

  #swap
  temp = augcoeffmatrix[i,]
  augcoeffmatrix[i,] = augcoeffmatrix[pivotRow,]
  augcoeffmatrix[pivotRow,] = temp
  return(augcoeffmatrix)
}


GaussJordanElimination <- function(augcoeffmatrix){
  solutionSet = c()

  for(i in 1:nrow(augcoeffmatrix)){
    if(i != nrow(augcoeffmatrix)) augcoeffmatrix = PartialPivoting(augcoeffmatrix,i) #partial pivoting
    augcoeffmatrix[i,] = (augcoeffmatrix[i,])/(augcoeffmatrix[i,i])                  #find a[i,]

    for(j in 1:nrow(augcoeffmatrix)){
      if(i==j) next
      normalRow = augcoeffmatrix[j,i] * augcoeffmatrix[i,]  #find normalized row
      augcoeffmatrix[j,] = augcoeffmatrix[j,] - normalRow   #find a[j,]: a[j,] - NORMALIZED ROW
    }
  }
  
  RHS = augcoeffmatrix[,ncol(augcoeffmatrix)]
  for(i in 1:nrow(augcoeffmatrix)) solutionSet[i] = RHS[i] 
  
  return(list("GaussJordanEliminatedMatrix"=augcoeffmatrix,"SolutionSet"=solutionSet))
}

Condition1 <- function(mat, x, fx, no.intervals){
  a.index = 0
  b.index = 1
  c.index = 2
  row.index = 1
  
  for(i in 2:no.intervals){
      a = x[i]^2
      b = x[i]
      rhs = fx[i]

      mat[row.index,a.index] = a
      mat[row.index, b.index] = b
      mat[row.index, c.index] = 1
      mat[row.index, ncol(mat)] = rhs

      row.index = row.index + 1

      a.index = a.index + 3
      b.index = b.index + 3
      c.index = c.index + 3
      
      mat[row.index, a.index] = a
      mat[row.index, b.index] = b
      mat[row.index, c.index] = 1
      mat[row.index, ncol(mat)] = rhs

      row.index = row.index + 1
  }

  return(list(Matrix=mat, Index = row.index))
}

Condition2 <- function(mat, x, fx, row.index){
  mat[row.index, 0] = x[1]^2
  mat[row.index, 1] = x[1]
  mat[row.index, 2] = 1
  mat[row.index, ncol(mat)] = fx[1]

  row.index = row.index + 1
  index = nrow(mat)
  mat[row.index, (index-2)] = x[length(x)]^2
  mat[row.index, (index-1)] = x[length(x)]
  mat[row.index, index] = 1
  mat[row.index, ncol(mat)] = fx[length(x)]

  row.index = row.index + 1
  return(list(Matrix = mat, Index = row.index))
}

Condition3 <- function(mat, x, fx, row.index, no.intervals){
  a.index = 0
  b.index = 1
  
  for(i in 2:no.intervals){
    mat[row.index, a.index] = 2*x[i]
    mat[row.index, b.index] = 1

    a.index = a.index + 3
    b.index = b.index + 3

    mat[row.index, a.index] = -2*x[i]
    mat[row.index, b.index] = -1

    row.index = row.index + 1
  }

  return(mat)
}

GenerateFunctions <- function(solutionset){
  functionList = c()
  j = 1
  for(i in 1:(length(solutionset)/3)){
    functionList = c(functionList, paste("function(x)",solutionset[j],"* x^2 +",solutionset[j+1],"* x +",solutionset[j+2]))
    j = j + 3
  }

  return(functionList)
}

getQueryInterval <- function(x, query){
  no.intervals = length(x)-1

  for(i in 1:no.intervals){
    if(query >= x[i] && query <= x[(i+1)]) return(i)
  }
  return(0)
}

QuadraticSplineInterpolation <- function(df, query){
  x = df[,1]
  fx = df[,2]

  no.intervals = length(x) - 1
  matrix.size = (no.intervals*3)-1
  str.functions = c()
  mat = matrix(c(0), nrow = matrix.size, ncol = matrix.size+1)

  cond1 = Condition1(mat,x,fx,no.intervals)
  mat = cond1$Matrix
  row.index = cond1$Index

  cond2 = Condition2(mat,x,fx,row.index)
  mat = cond2$Matrix
  row.index = cond2$Index

  mat = Condition3(mat,x,fx,row.index,no.intervals)
  solutionset = GaussJordanElimination(mat)$SolutionSet

  functionList = GenerateFunctions(c(0,solutionset))

  index = getQueryInterval(x, query)

  fxn <- function(x){
    eval(parse(text = functionList[index]))
  }

  return(list(FunctionList = functionList, fxn = fxn(x), index = index))
}