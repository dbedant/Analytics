library(shiny)
library(ggplot2)
library(shinyTree)
library(tidyverse)
library(plotly)
source("calculate.R")
source("server.R")
#--------------------------Read files------------------------------------------
tMatrix <- ReadLogFile()
firstColNames <- names(tMatrix)[1]
maxValue = 0
minValue = 5000
for(input in as.integer(tMatrix[,2]))
{
  if(input > maxValue)
    maxValue = input
  if(input < minValue)
    minValue = input
}
#--------------------------Fluid------------------------------------------
ui <- fluidPage(
    title = "Analytics Report",
	headerPanel(h3("Analytics Report")),
	sliderInput("slider1", label = h3("Year"), min = minValue, 
	            max = maxValue, value = c(minValue, maxValue), step =1 ),
	sidebarPanel(width = 3,
                 shinyTree("tree1234", checkbox = TRUE, search=TRUE, searchtime = 1000),
      uiOutput("checkGroup"),
      uiOutput("filterGroup"),
      checkboxInput("checkbox1", label = "Filter By Year", value = TRUE)
     ),
	mainPanel(plotOutput("plot2"))
)


#-------------------------Server-------------------------------------------
server <- shinyServer(function(input, output, session) {
  
  
  #-------------------------Tree-------------------------------------------
  output$tree1234 <- renderTree({
    if(is.null(input$slider1)){
      return(NULL)
    }
    GetTreeList(ApplyYear(input, tMatrix, TRUE))
  })
 
  output$checkGroup<- renderUI({ 
    result <- GetCheckBox(GetTreeList(ApplyYear(input, tMatrix, TRUE)),input)
    
    if(length(result[[1]]) > 0 && length(result[[2]]) > 0 )
    {
      selectInput("selectgroup","Select column",choices = 
                    result[[2]],selected="",multiple = FALSE)
    }
    else
    {
      return(NULL)
    }
  })
  
  output$filterGroup<-renderUI({
    if(is.null(input$selectgroup)){
      return(NULL)
    }
    else if(input$selectgroup==""){
      return(NULL)
    }
    else{
      selectList <- GetPossibleOption(ApplyYear(input, tMatrix, TRUE),input, tMatrix)
      
      selectInput("selectgroup1","Select reference",choices = 
                    selectList,selected="",multiple = TRUE)
    }
  })
  
  output$plot2<-renderPlot({
    input$selectgroup1
    if(is.null(input$selectgroup)){
      return(NULL)
    }
    else if(input$selectgroup==""){
      return(NULL)
    }
    inputList <- GetTreeList(ApplyYear(input, tMatrix, TRUE))
    dt <- ParseTable(tMatrix, input, inputList, FALSE)
    dt <- dt %>% filter(get(input$selectgroup) != "")
    
    filterTable <- data.frame()
    for(dat in input$selectgroup1)
    {
      filterTable <- bind_rows(dt %>% filter(get(input$selectgroup) == dat), 
                               filterTable)
    }
    if(length(input$selectgroup1) > 0)
      dt <- filterTable
    
    rowX <- GetSelectedCol(input, inputList)
    targetX <- rowX
    if(input$checkbox1 == TRUE)
    {
      dt <- mutate(dt, tempx = paste(year, get(rowX)))
      targetX <- "tempx"
    }
    
    red.bold.italic.text <- element_text(face = "bold.italic", color = "red", size = 20)
    blue.bold.italic.16.text <- element_text(face = "bold.italic", color = "blue", size = 16)
    ggplot(dt) + 
      geom_col(aes_string(x=targetX,y=firstColNames,fill=input$selectgroup)) +
      theme(title = red.bold.italic.text, axis.title = red.bold.italic.text,  axis.text.x = blue.bold.italic.16.text)+
      ggtitle("Result")
    }) 
  
})

shinyApp(ui=ui, server=server)
