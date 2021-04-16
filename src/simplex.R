# Simplex Method Implementation
# @author: John Alwin Pamintuan

#Checks if negative data exists in objective function row
withNegative <- function(Z){
  for(i in Z) if(i<0) return(TRUE)
  return(FALSE)
}

#Returns index of the column with smallest negative integer in the objective function row
getPivotColumn <- function(Z){
  min = min(Z[Z<0])
  return( which(Z==min)[1] )
}

#Returns index of the minimum positive test ratio
#Returns NA if no test ratio is positive (Infeasible Solution)
getMinTR <- function(TR){
  min = min(TR[TR>0])
  if(min==Inf || is.na(min)) return(NA)
  return(which(TR==min)[1])
}

updateTR <- function(TR, Solution, PivotColumn){
  for(i in 1:(length(TR)-1)){
    if(PivotColumn[i]==0) TR[i] = Inf
    else TR[i] = Solution[i]/PivotColumn[i] 
  }
  return(TR)
}

simplex <- function(df){
  options(digits = 4)
  
  itercount = 1
  iterlist = list(df)
  basicsolution = list()
  zRow.index = nrow(df)
  trRows.index = nrow(df)-1
  solution.index = ncol(df)-1
  
  while(withNegative(df[zRow.index,])){

    pivot.column = getPivotColumn(df[zRow.index,1:(solution.index-1)])

    df[,"TR"] = updateTR(df[,"TR"], df[,"Solution"], df[, pivot.column])
    pivot.row = getMinTR(df[,"TR"])
    
    pivot.element = df[pivot.row, pivot.column]
    if(is.na(pivot.row)) return(list(Dataframe = df, Solution ="No feasible Solution"))

    df[pivot.row, 1:solution.index] = df[pivot.row, 1:solution.index]/pivot.element

    for(i in 1:nrow(df)){
      if(i==pivot.row) next

      normalRow = df[i, pivot.column] * df[pivot.row,]
      df[i,1:(ncol(df)-1)] = df[i,1:solution.index] - normalRow[1:solution.index]
    }
    
    itercount = itercount + 1
    iterlist[itercount] = list(df)
    basicsolution[itercount] = list(df[nrow(df),9:23])
  }
  return(list(Dataframe = iterlist, BasicSolution = basicsolution, MinCost = df[zRow.index, solution.index]))
}

setUpInitialTableau <- function(dataframe){
  supply = dataframe[,"Supply"]
  demand = dataframe["Demand",]
  cost = as.vector(t(dataframe[1:3,1:5]))
  tableau = data.frame(matrix(0, nrow = 9, ncol = 16))
  
  l = 1
  u = 5
  
  #Setup supply constraints
  for(i in 1:3){
    tableau[i,l:u] = -1 #Convert to >=
    l = l + 5
    u = u + 5

    tableau[i,ncol(tableau)] = -supply[i]
  }

  #Setup demand constraints
  a = 1
  for(i in 4:8){
    tableau[i,ncol(tableau)] = demand[(i-3)]
    tableau[i,seq(a,15,5)] = 1 #Surplus variable
    a = a + 1
  }

  for(i in 1:length(cost)) tableau[nrow(tableau),i] = cost[i]
  
  return(t(tableau))
}

addSlackToInitialTableau <- function(tableau){
  Solution = tableau[,ncol(tableau)]
  tableau = tableau[,-ncol(tableau)]
  
  #Adds slack variables
  for(i in 1:16){
    S = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    S[i] = 1
    tableau = cbind(tableau, S)
  }

  TR = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  tableau = cbind(tableau, Solution, TR)
  tableau[nrow(tableau),1:9] = -1 * tableau[nrow(tableau),1:9]

  rownames(tableau) = c('A1','A2','A3','A4','A5','B1','B2','B3','B4','B5','C1','C2','C3','C4','C5','Z')
  colnames(tableau) = c('S1','S2','S3','S4','S5','S6','S7','S8','A1','A2','A3','A4','A5','B1','B2','B3','B4','B5','C1','C2','C3','C4','C5','Z','Solution','TR')
  
  return(tableau)
}

getMinimumCost <- function(df){
  df = setUpInitialTableau(df)
  df = addSlackToInitialTableau(df)
  return(simplex(df))
}

getInitialTableau <- function(df){
  df = setUpInitialTableau(df)
  df = addSlackToInitialTableau(df)
}

#TEST CASE 1
demand = c(180,80,200,160,220,NA)
supply = c(310,260,280)
cost = matrix(c(10,8,6,5,4,6,5,4,3,6,3,4,5,5,9), nrow=3,ncol=5,byrow=TRUE)
df = data.frame(cost, Supply = supply)
df = rbind(df, Demand = demand)
rownames(df) = c("A Denver","B Phoenix","C Dallas","Demand")
colnames(df) = c("1 Sacramento", "2 Salt Lake City", "3 Albuquerque", "4 Chicago", "5 New York City", "Supply")