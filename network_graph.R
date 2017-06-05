#install.pacakges("FSelector") #calculate information gain
#install.packages("qgraph") # draw network graph
#install.packages("visnetwork") #draw network graph advanced

#test_data <-read.csv("~/credit/test_data_2.csv") # data import (read data)

network_graph<-function(data)
{
  
  library(FSelector)
  library(visNetwork)
  
  #library(qgraph)
  
  
  ##### information gain phase ##########
  
  
  test_data <- data
  
  m <- matrix(0,ncol = length(test_data),nrow=length(test_data)) # make data form (matrix) because of drawing network graph
  m <- data.frame(m) # tranform matrix to data frame
  
  
  for (i in 1:length(test_data))
  {
    tmp_data <- test_data[,i]
    tmp <- information.gain(tmp_data ~ ., data = test_data)
    
    for(j in 1:length(tmp[,1]))
    {
      if(i != j)
      {
        m[i,j] <- tmp[j,1]
      }
      else
      {
        m[i,j] <- 0
      }
    }
    rm(tmp)
    rm(tmp_data)
  }
  names(m)<- names(test_data)
  
  write.csv(m,'information_gain.csv',row.names = FALSE)
  
  
  ##### visnetwork phase ###########
  
  from_value <-list()
  to_value <-list()
  label_value <-list()
  edge_value<-list()
  
  for (i in 1:length(m)){
    
    for (j in 1:length(m[,1])){
      
      if (m[i,j] != 0){
        
        from_value <- append(from_value,i) # insert edge from value
        to_value <- append(to_value,j) # insert edge to value
        label_value <-append(label_value, m[i,j]) #insert information gain value
        edge_value <-append(edge_value, floor(m[i,j]*10)) #edge width setting
      }
    }
  }
  
  
  from_value <-as.numeric(from_value) #change data type (list to numeric)
  to_value <-as.numeric(to_value) #change data type (list to numeric)
  
  edge <-data.frame(from = from_value, to = to_value,label = paste(label_value),length = 700, width = paste(edge_value)) # set edge
  
  node <- data.frame(id = 1:length(m), label = paste(names(m)),physics = FALSE) #set node ##!!encoding issue(korean)!!##
  
  output<- visNetwork(node,edge)%>%  #draw network graph base
    visEdges(arrow = 'from', scaling = list(min = 2, max = 2))%>% #
    visInteraction(multiselect = TRUE) #
  
  #output <-qgraph(m, esize=4, gray =TRUE, edge.labels=TRUE, title="information gain draw Network Graph",dig=TRUE) #draw network graph
  
  return (output)
  
}
