### Query Document Database
### Author: MAGHRABI, MAHVASH
### Course: CS5200
### Term: FALL 2022

# assumes that you have set up the database structure by running CreateFStruct.R

# Query Parameters (normally done via a user interface)

quarter <- "Q2"
year <- "2021"
customer <- "Medix"

# write code below

# Question 6
# returns the generated report's full path
genReportPath <- function(customer, year, quarter){
  path = paste("docDB", "reports", customer, year, quarter, sep ="/")
  return(path)
}
print(genReportPath)

# Question 7
# returns the generated report's file name
genReportReportFileName <- function(customer, year, quarter){
  fileName = paste(customer, year, quarter, "pdf", sep = ".")
  return(fileName)
}
print(genReportReportFileName)

# Question 8
# Creates the lock file in the required folder on certain if/else conditions
# if folder path does not exist - return null
# if lock file created - return 0
# if if lock exits - return -1
setLock <- function(customer, year, quarter) {
  folderPath = genReportPath(customer, year, quarter)

  if (!dir.exists(folderPath)){
    return()
  }
# Creating lock file if it doesn't exist
  lockFileName = paste(folderPath, ".lock", sep = "/")
  if(!file.exists(lockFileName) && file.create(lockFileName)){
    return(0)
  }
  return(-1)
}

#Question 9
storeReport <- function(customer, year, quarter){
  lockSet = setLock(customer, year, quarter)
  if(is.null(lockSet)){
    return("Folder does not exist")
  }
  if(lockSet == -1){
    return("folder is locked")
  }
  fileName = genReportReportFileName(customer, year, quarter)
  filePath = paste(genReportPath(customer, year, quarter),
                   fileName, sep = "/")
  returnMsg = ""
  
  # Copying the file
  if(file.copy(fileName, filePath)){
    return(returnMsg)
  }else{
    return("No file to copy")
  }
}
#Question 10
relLock <- function(customer, year, quarter){
  folderPath = genReportPath(customer, year, quarter)
  
  if(!dir.exists(folderPath)){
    return("not a valid path")
  }
  lockFilePath = paste(folderPath, ".lock", sep = "/")
  if(file.exists(lockFilePath)){
    file.remove(lockFilePath)
    return("lock removed")
  } else {
    return("no lock")
  }
}
