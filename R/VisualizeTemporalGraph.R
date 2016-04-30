
#   Copyright (c) 2016-2017, Khushnood Abbas (khushnood.abbas@gmail.com), UESTC China.
#  
#  All rights reserved.
# 
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
# 
#  * Redistributions of source code must retain the above copyright
#  notice, this list of conditions and the following disclaimer.
#  
#  * Redistributions in binary form must reproduce the above copyright
#  notice, this list of conditions and the following disclaimer in the
#  documentation and/or other materials provided with the
#  distribution.
#    
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
#  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
#  COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
#  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
#  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
#  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
#  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
#  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
#  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
#  OF THE POSSIBILITY OF SUCH DAMAGE.
 

#' @param filePath network data file CSV.
#' @param targetPath out put image directory path
#' @param nodeFromInex The column index in the file for first node, default is 1.
#' @param nodeToIndex The column index in the file for second node, default is 2.
#' @param timeIndex The column index in the file for timstamp, default is 3.
#' @param unixTimeStamp the time stamp is unix time stamp, default is true.
#' @param plotType there are so many packages and methods to plot the network image, here we have considered only two(1,2). One can extend it according to their need.
#' @details Take snap shot of the network on random time step and then make a dynamic file to visualize its evolution.

#' @export 
#' @examples
#' plotImageFile(filePath=paste0("./inputFile.txt"),targetPath="./images/")

plotImageFile <-
  function(filePath = paste0("./inputFile.txt"),targetPath = "./images/",nodeFromInex =
  1,nodeToIndex = 2,timeIndex = 3,unixTimeStamp = TRUE,plotType = 1) {
  library(igraph)
  library(network)
  library(parallel)
  library(qgraph)
  temporalData = read.csv(file = paste0(filePath),sep = "\t")
  temporalData = cbind(temporalData[,nodeFromInex],temporalData[,nodeToIndex],temporalData[,timeIndex])
  ttime = temporalData[,timeIndex]
  temporalData = temporalData[order(ttime),]
  ttime = ttime[order(ttime)]
  temporalData = na.omit(temporalData)
  if (Sys.info()[['sysname']] == "Windows") {
  do.call(ttime, mclapply(seq(10), function(temporalDataVar,ttimeVar,targetPathVar,nodeFromInexVar,nodeToIndexVar,timeIndexVar,unixTimeStampVar,plotTypeVar)
  plotImage(
  temporalData,sample(ttime,10),targetPath,nodeFromInex,nodeToIndex,timeIndex,unixTimeStamp,plotType
  ),mc.cores = 1))
  
  shell(paste(
  "convert -delay 40", paste0(file.path(targetPath,""),"*.png"), paste0(file.path(targetPath,""),"outputgif.gif")
  ),intern = TRUE)
  ##  #convert -delay 40 *.png allInfectious.gif # if from the code it is unable to make png you can use system shell to convert it using this command
  
  }
  else{
  tncores = detectCores(all.tests = FALSE, logical = FALSE)
  ncores = as.integer(tncores * 3 / 4)
  do.call(ttime, mclapply(seq(10), function(temporalDataVar,ttimeVar,targetPathVar,nodeFromInexVar,nodeToIndexVar,timeIndexVar,unixTimeStampVar,plotTypeVar)
  plotImage(
  temporalData,sample(ttime,10),targetPath,nodeFromInex,nodeToIndex,timeIndex,unixTimeStamp,plotType
  ),mc.cores = ncores))
  
  shell(paste(
  "convert -delay 40", paste0(file.path(targetPath,""),"*.png"), paste0(file.path(targetPath,""),"outputgif.gif")
  ),intern = TRUE)
  
  #convert -delay 40 *.png allInfectious.gif #
  
  
  }
  }
  
  #' @details Local method to plot utilized for parallel plotting.
  plotImage <-
  function(temporalDataVar,ttimeVar,targetPathVar,nodeFromInexVar = 1,nodeToIndexVar =
  2,timeIndexVar = 3,unixTimeStampVar = TRUE,plotTypeVar = 1) {
  for (t in ttimeVar) {
  tmdData = temporalDataVar[temporalDataVar[,timeIndexVar] <= t,]
  if (length(tmdData) < 1) {
  next()
  }
  sourceUsr = tmdData[,nodeFromInexVar]
  targetUsr = tmdData[,nodeToIndexVar]
  networkData <- data.frame(sourceUsr, targetUsr)

  if (!file.exists(targetPathVar)) {
  dir.create(targetPathVar)
  }
  png(file = paste0(targetPathVar,t,".png"))
  
  if (plotTypeVar == 1) {
      adj = get.adjacency(graph.edgelist(as.matrix(networkData, directed =
  FALSE)))
    netwrk = network(adj);
  degreeOFNodes = degree(netwrk)
  qgraph(
  netwrk,color = "green",edge.color = 2,label.color = 3,usePCH = 3,vsize =
  scale(degreeOFNodes)
  )
  }
  if (plotTypeVar == 2) {
  gplot(networkData,vertex.cex = scale(degreeOFNodes))
  
  }
  
  if (unixTimeStampVar) {
  title(main = paste(structure(t, class = c(
  "POSIXct", "POSIXt"
  ))))
  }
  else{
  title(main = paste("At :" ,t))
  }
  
  dev.off()
  
  
  }
  }
