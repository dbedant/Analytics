source("calculate.R")
#--------------------------Check box-------------
GetCheckBox <- function(finalList1,input) {
  selectList <- list()
  nonSelectList <- list()
  
  tHeaderList <- as.list(names(finalList1))
  index <- 1
  for(treeGroup in finalList1){
    
    bSelect = FALSE
    for ( i in treeGroup)
    {
      if(i %in% get_selected(input$tree1234)){
        bSelect = TRUE
        break
      }
    }
    if(bSelect){
      selectList <- c(selectList, tHeaderList[index])
    }
    else{
      nonSelectList <- c(nonSelectList, tHeaderList[index])
    }
    index <- index + 1 
  }
  list(selectList, nonSelectList)
}

#--------------------------Possible col------------
GetPossibleOption <- function(dtTree,input, tMatrix) {
  asdsa <- GetTreeList(ApplyYear(input, tMatrix, TRUE))
  
  newTable <- ParseTable(dtTree,input, asdsa, TRUE)
  
  colList <- as.list(names(newTable))
  for(index in 1: length(colList)){
    if(colList[[index]] == input$selectgroup){
      return(unique(newTable[,index]))
    }
      
  }
}

#------------------------Plot matrix ----------
GetPlotTree <- function(dtTree, input) {
  ParseTable(dtTree, input, GetTreeList(ApplyYear(input, tMatrix, TRUE)), FALSE)
}