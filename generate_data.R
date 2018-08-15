#Reading .csv file and saving the data in a variable
##############################################################################

path1 = file.path("C:","geology.csv")
dataGeo = read.table(path1, header = TRUE, sep = ",", stringsAsFactor = FALSE)

path2 = file.path("C:","complete.csv")
dataNet = read.table(path2, header = TRUE, sep = ",", stringsAsFactor = FALSE)

# Package for 10-fold cross validation
##############################################################################

library(caret)

# Creating folds for the database without geological age

foldsNet=createFolds(dataNet$SBTn,k=10,list=TRUE,returnTrain=FALSE)

# Creating folds for the database with geological age

foldsGeo=createFolds(dataGeo$MSBTn,k=10,list=TRUE,returnTrain=FALSE)

# Proportion check
##############################################################################

for(j in 1:10){
  
  # Selecting fold
  
  cat("Fold",j,"\n\n")
  
  testNet=dataNet[foldsNet[[j]],]
  testGeo=dataGeo[foldsGeo[[j]],]
  
  # Percentage of elements by class for Robertson (1991)
  
  cat("Robertson (1991)\n\n")
  
  for(i in sort(unique(dataNet$SBTn))){
    cat("Class", i, ":", 100*nrow(testNet[testNet$SBTn==i,])/nrow(dataNet[dataNet$SBTn==i,]),"\n")
  }
  
  cat("\n")
  
  # Percentage of elements by class for Robertson (2016)
  
  cat("Robertson (2016)\n\n")
  
  for(i in sort(unique(dataNet$MSBTn))){
    cat("Class", i, ":", 100*nrow(testNet[testNet$MSBTn==i,])/nrow(dataNet[dataNet$MSBTn==i,]),"\n")
  }
  
  cat("\n")
  
  # Percentage of elements by class for Robertson (2016) with geological age
  
  cat("Robertson (2016) with geological age\n\n")
  
  for(i in sort(unique(dataGeo$MSBTn))){
    cat("Class", i, ":", 100*nrow(testGeo[testGeo$MSBTn==i,])/nrow(dataGeo[dataGeo$MSBTn==i,]),"\n")
  }
  
  cat("\n")
  
  # Percentage of elements by Strutuctural Classification (EST)
  
  cat("Structural Classification\n\n")
  
  for(i in sort(unique(dataGeo$EST))){
    cat("Class", i, ":", 100*nrow(testGeo[testGeo$EST==i,])/nrow(dataGeo[dataGeo$EST==i,]),"\n")
  }
  
  cat("\n\n")
  
}

# Defining validation sets for each test stage without replacement
##############################################################################

# Vector with the chosen folds in sequence

validFolds=c()

# Vector with the picking options

pick=1:10

for(j in 1:10){
  
  # Selecting test fold
  
  cat("Test fold:",j,"\n\n")
  
  # Setting seed for sampling
  
  set.seed( as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31) )
  
  # Sampling a fold for validation
  
  if(j<10){
    
    # Choosing some unchosen fold
    
    f=sample(pick[pick!=j],1)
    
    # Excluding chosen fold from the picking options
    
    pick=pick[pick!=f]
    
    # Keeping the fold identification
    
    validFolds=append(validFolds,f)
    
    # Print the choice
    
    cat("-> Validation fold:",f,"\n\n")
    
  }else{
    
    # Choosing the only unchosen fold remaining
    
    f=pick[1]
    
    # Excluding the last fold from the picking options
    
    pick=pick[pick!=f]
    
    # Keeping the fold identification
    
    validFolds=append(validFolds,f)
    
    # Print the last fold
    
    cat("-> Validation fold:",f,"\n\n")
    
  }
}

# Show the chosen folds in sequence

show(validFolds)