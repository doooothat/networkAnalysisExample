library(shiny)
library(FSelector)\
library(visNetwork)

#?Լ?: ????ǥ ????
CT_inforGain <- function(data, fileName)
{
  ##### information gain phase ##########
  test_data <- data
  m <- matrix(0,ncol = length(test_data),nrow=length(test_data)) # make data form (matrix) because of drawing network graph
  m <- data.frame(m) # tranform matrix to data frame
  for (i in 1:length(test_data)){
    tmp_data <- test_data[,i]
    tmp <- information.gain(tmp_data ~ ., data = test_data)
    for(j in 1:length(tmp[,1])){
      if(i != j){
        m[i,j] <- tmp[j,1]
      }
      else{
        m[i,j] <- 0
      }
    }
    rm(tmp)
    rm(tmp_data)
  }
  names(m)<- names(test_data)
  write.csv(m, fileName, row.names = FALSE)
  return (m)
}


shinyServer(function(input, output, session) {
  
  #????: ?????? ???Ϸκ??? ?????͸? ?ҷ??? ???̺??? ????
  output$dataView <- renderTable({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header=input$header1, sep=input$sep1)
  })
  
  
  #????: ?????? ???Ϸκ??? ?????͸? ?ҷ??? ???????̺? ?????ϰ? ?????? ???????̺? ????
  output$crossView <- renderTable({
    CrossTB()
  })
  
  
  #????: ?????? ???????̺??? ?ִ? ????�� ?ҷ??? ??Ʈ??ũ ????
  output$networkView <- renderVisNetwork({ #renderPlot  #reactivePlot({
    net_Plot()
  })
  
  
  #"ǥ??ǥ ????" Button Click
  CrossTB <- eventReactive(input$createT, {
    if(isTRUE(input$crossT)){
      inFile <- input$file
      if (is.null(inFile))
        return(NULL)
      m <- read.csv(inFile$datapath, header=TRUE, sep=',')
      write.csv(m, "infor_gain.csv", row.names = FALSE)
      m
    }
    else{
      inFile <- input$file
      if (is.null(inFile))
        return(NULL)
      m <- read.csv(inFile$datapath, header=TRUE, sep=',')
      CT_inforGain(m, "infor_gain.csv")
    }
  })
  
  
  #action Button��?κ??? ?̺?Ʈ ?߻?
  net_Plot <- eventReactive(input$createNet, {
    from_value <-list()
    to_value <-list()
    label_value <-list()
    edge_value<-list()
    
    m <- read.csv("./infor_gain.csv", header=TRUE, sep=',')
    minV <- min(m)
    maxV <- max(m)
    perV <- maxV - minV
    for (i in 1:length(m)){
      for (j in 1:length(m[,1])){
        if (((m[i,j]-minV)/perV) > (1/input$cutting)){
          from_value <- append(from_value,i) # insert edge from value
          to_value <- append(to_value,j) # insert edge to value
          label_value <- append(label_value, m[i,j]) #insert information gain value
          edge_value <- append(edge_value, floor((m[i,j]-minV)/perV*10))
        }
      }
    }
    
    from_value <-as.numeric(from_value) #change data type (list to numeric)
    to_value <-as.numeric(to_value) #change data type (list to numeric)
    
    nodes <- data.frame(id = 1:length(m), label = paste(names(m)), size = 15, physics = FALSE)
    edges <- data.frame(from = from_value, to = to_value, label = paste(label_value), length = 200, width = paste(edge_value))
    
    visNetwork(nodes, edges)%>%
      visEdges(arrow = 'from', scaling = list(min = 10, max = 30))%>% 
      visInteraction(multiselect = TRUE) 
    
  })
})










