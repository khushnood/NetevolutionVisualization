# NetevolutionVisualization
This R project is for visualizing time evolving network. Any one finds any mistack can update the code.
 @param filePath network data file CSV.
 @param targetPath out put image directory path
 @param nodeFromInex The column index in the file for first node, default is 1.
 @param nodeToIndex The column index in the file for second node, default is 2.
 @param timeIndex The column index in the file for timstamp, default is 3.
 @param unixTimeStamp the time stamp is unix time stamp, default is true.
 @param plotType there are so many packages and methods to plot the network image, here we have considered only two(1,2). One can extend it according to their need.
#details Take snap shot of the network on random time step and then make a dynamic file to visualize its evolution.

#export 
#examples
#plotImageFile(filePath=paste0("./inputFile.txt"),targetPath="./images/")


