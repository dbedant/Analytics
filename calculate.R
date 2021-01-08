GetSelectedCol <- function(input, finalList) {
  tHeaderList <- as.list(names(finalList))
  selectTreeNames <- get_selected(input$tree1234)
  index <- 1
  for(treeGroup in finalList){
    for ( i in treeGroup)
    {
      if(i %in% selectTreeNames){
        return(tHeaderList[[index]])
      }
    }
    index <- index + 1 
  }
  ""
}
#--------------------------Filter table-------------
ParseTable <- function(dtTree,input, finalList1, bRemove = TRUE) {
  
  filterTable <- data.frame()
  tHeaderList <- as.list(names(finalList1))
  
  index <- 1
  selectList <- list()
  for(treeGroup in finalList1){
    bSelect = FALSE
    for ( i in treeGroup)
    {
      if(i %in% get_selected(input$tree1234)){
        filterTable <- bind_rows(dtTree %>% filter(get(tHeaderList[[index]]) == i), 
                                 filterTable)
        bSelect = TRUE
      }
    }
    if(!bSelect)
    {
      selectList <- c(selectList, index)
    }
    index <- index + 1 
  }
  if(!bRemove)
  {
    return(filterTable)
  }
  filterTable <- filterTable[unlist(selectList)]
  emptyCol = list()
  for(index in 1:ncol(filterTable)){
    uniqueList <- unique(filterTable[,index])
    if(!(length(uniqueList) == 1 && uniqueList[[1]] == ""))
      emptyCol <- c(emptyCol, index)
  }
  filterTable <- filterTable[unlist(emptyCol)]
  filterTable
}

#--------------------------Tree creation-------------
GetTreeList <- function(dtTree) {
  finalList <- list()
  for(index in 1:ncol(dtTree)){
    
    newelem <- list()
    for(index1 in 1:nrow(dtTree)){
      if(dtTree[index1,index] != ""){
        if(match( dtTree[index1,index], newelem, nomatch = 0) == 0)
          newelem <- append(newelem, dtTree[index1,index])
      }
    }
    names(newelem) <- newelem
    tempNewelem <- list(index = newelem )
    finalList <- c(finalList, tempNewelem)
  }
  
  names(finalList) = names(dtTree)
  finalList
}


#-----------------------------------Read-------------------
ReadLogFile <- function() {
  fileList =list.files(path = "covid/", pattern = NULL, all.files = FALSE,
                       full.names = TRUE, recursive = FALSE,
                       ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  
  tMatrix <- matrix(,nrow = 0, ncol=0)
  tMatrix <- cbind(tMatrix, c("")) 
  tMatrix <- cbind(tMatrix, c("")) 
  
  newHeaderName = list("duration", "year")
  for ( fileName in fileList) {
    conn <- file(fileName, open="r")
    linn <- readLines(conn) 
    bFirst <- TRUE
    for (i in 1:length(linn)){
      if(bFirst)
      {
        bFirst = FALSE
        tempData <- str_split(linn[i], ",",simplify = TRUE)
        newHeaderName[1] = tempData[1]
      }
      else
      {
        tempData <- str_split(linn[i], ",",simplify = TRUE)
        lastIndex <- 2
        tempList <- list()
        for (index in 1:length(newHeaderName)){
          tempList <- append(tempList, "")
        }
        for (index in 1:length(tempData)){
          if(index == 1)
          {
            tempList[1] <- tempData[index]
          }
          else
          {
            if(index %% 2 == 0)
            {
              tempList[lastIndex] <- tempData[index]
            }
            else
            {
              lastIndex <- match( tempData[index], newHeaderName, nomatch = 0)
              if(lastIndex == 0)
              {
                tMatrix <- cbind(tMatrix, c(""))
                newHeaderName <- append(newHeaderName, tempData[index])
              }
              lastIndex <- match( tempData[index], newHeaderName, nomatch = 0)
            }
          }
        }
        tMatrix <- rbind(tMatrix, unlist(tempList)) 
      }
    }
  }
  colnames(tMatrix) <- newHeaderName
  tMatrix <- data.frame(tMatrix)
  tMatrix
}

#----------------------
ApplyYear <- function(input, dtTree, bRemoveYear = TRUE) {
  
  dtTree <- dtTree %>% filter(year <= input$slider1[2] & year>= input$slider1[1])
 
  if(bRemoveYear)
  {
    dtTree <- dtTree[, colnames(dtTree) != firstColNames]
    dtTree <- dtTree[, colnames(dtTree) != "year"]
  }
  emptyCol = list()
  for(index in 1:ncol(dtTree)){
    uniqueList <- unique(dtTree[,index])
    if(!(length(uniqueList) == 1 && uniqueList[[1]] == ""))
      emptyCol <- c(emptyCol, index)
  }
  dtTree <- dtTree[unlist(emptyCol)]
  dtTree
}

