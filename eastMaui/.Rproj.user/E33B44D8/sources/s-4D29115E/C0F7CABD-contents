# convert node and input dataframes to matrices

convertMatrix = function(nodeInput, calcInput){
  nodes.mat = as.matrix(nodeInput[ ,-2])
  row.names(nodes.mat)<-unlist(nodeInput[ ,1])
  nodes.mat[ , c(3:11)] = as.numeric(nodes.mat[ , c(3:11)] )

  input.mat = as.matrix(calcInput)
  row.names(input.mat)<- unlist(calcInput[,1])

  outList<- list('nodes' = nodes.mat, 'inputs' = input.mat)
  return(outList)
}
