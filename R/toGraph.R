# Importing static file for graphing initial peaks
## Imported file is always of same format
## hence this script is static and does not take any arguments (For Now).
### Using library: dplyr, ggplot2, ggthemes, reshape2, ggthemes, viridis, lattice
graph_Valley<-function(file_csv,graphFolder,graphType)
{
  #windowTemp
  if(!require(ggplot2)){
    install.packages("ggplot2");
  }
  if(!require("plyr")){
    install.packages("plyr");
  }
  if(!require("dplyr")){
    install.packages("dplyr");
  }
  if(!require("reshape2")){
    install.packages("reshape2");
  }
  if(!require("lattice")){
    install.packages("lattice");
  }
  if(!require("gridExtra")){
    install.packages("gridExtra");
  }
  if(!require("viridis")){
    install.packages("viridis");
  }
  if(!require("ggthemes")){
    install.packages("ggthemes");
  }

  library(ggplot2);
  library("plyr");
  library("dplyr");
  library('reshape2');
  library(lattice);
  library(gridExtra);
  library(viridis);
  library(ggthemes);

  #graphPeaks file should have files in absolute path and header as "files"
  graphFiles <- read.csv(paste( file_csv, sep="/"), header=TRUE);
  fileList <- as.character(graphFiles$files);
  as.character(fileList[1][1])
  cat("Graphing... \n");
  for (i in 1:length(fileList)) {
    ## close rendering loop for Unique Peaks interval
    cat(paste('\n Graphing File: ', i, ' of ', length(fileList), '\n ', sep = ''));
    cat(fileList[i]);
    singleGraphData<-read.delim2(as.character(fileList[i]), header = TRUE)

    ## ----------------------------------------------------------------
    ### Stating graph function constructors
    #  M2<-melt(as.data.frame(singleGraphData), id="index", measure=colnames("signalIn","Vlog3", "Vqual1","Vqual2","Vqual3","Vqual4","Vqual5"));
    keyWords <- colnames(singleGraphData)[!colnames(singleGraphData) %in% c('index')];
    M2<-melt(as.data.frame(singleGraphData), id="index", measure=keyWords);
    ### Fix data configuration coercing double to char for "value" for a decent ploting.
    M2[,"value"]<-(as.double(as.character(M2[,"value"])));
    ggplot(data=M2, aes(x=index, y=value, group=variable, color=variable))+geom_line();

    ### Setting multiwindow plot
    selectedCol<-keyWords[grepl("^v.*", keyWords)];
    pA<-ggplot(data=M2, aes(x=index, y=value, colour=variable))+geom_line()+ theme(legend.text=element_text(size=5.5)) + theme(plot.title=element_text(hjust=0))+theme(axis.text=element_text(size=7));
    l1<-cbind(singleGraphData["index"],singleGraphData[selectedCol[1]]); colnames(l1)<-c('index', 'Level'); l1$window<-unlist(strsplit(selectedCol[1], '.', fixed=TRUE))[2]
    l2<-cbind(singleGraphData["index"],singleGraphData[selectedCol[2]]); colnames(l2)<-c('index', 'Level'); l2$window<-unlist(strsplit(selectedCol[2], '.', fixed=TRUE))[2]
    l3<-cbind(singleGraphData["index"],singleGraphData[selectedCol[3]]); colnames(l3)<-c('index', 'Level'); l3$window<-unlist(strsplit(selectedCol[3], '.', fixed=TRUE))[2]
    l4<-cbind(singleGraphData["index"],singleGraphData[selectedCol[4]]); colnames(l4)<-c('index', 'Level'); l4$window<-unlist(strsplit(selectedCol[4], '.', fixed=TRUE))[2]
    l5<-cbind(singleGraphData["index"],singleGraphData[selectedCol[5]]); colnames(l5)<-c('index', 'Level'); l5$window<-unlist(strsplit(selectedCol[5], '.', fixed=TRUE))[2]

    if(!is.factor(l1['Level'][,1])){
      l1['Level'][,1] <- factor(l1['Level'][,1]);
    }
    if(!is.factor(l2['Level'][,1])){
      l2['Level'][,1] <- factor(l2['Level'][,1]);
    }
    if(!is.factor(l3['Level'][,1])){
      l3['Level'][,1] <- factor(l3['Level'][,1]);
    }
    if(!is.factor(l4['Level'][,1])){
      l4['Level'][,1] <- factor(l4['Level'][,1]);
    }
    if(!is.factor(l5['Level'][,1])){
      l5['Level'][,1] <- factor(l5['Level'][,1]);
    }
    M3 <- bind_rows(list(l1,l2,l3,l4,l5));
    M3<-as.data.frame(M3[with(M3, order(Level, index)), ]);
    M3[,"Level"]<-(as.double(as.character(M3[,"Level"])));
    pB<-ggplot(M3, aes(x=index, y=window))
    pB<-pB +geom_tile(aes(fill=Level))
    pB<-pB + scale_fill_viridis(option="B", begin=0, end=1, name="# VQuality")
    pB<-pB + labs(x=NULL, y=NULL, title="Vquals per window & index")
    pB<-pB + theme_tufte(base_family="Helvetica")
    pB<-pB + theme(plot.title=element_text(hjust=0))
    pB<-pB + theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())
    pB<-pB + theme(axis.text=element_text(size=8.5))
    pB<-pB + theme(legend.title=element_text(size=8))
    pB<-pB + theme(legend.text=element_text(size=7))
    pB


    g<-arrangeGrob(pA,pB);
    originalFileName<-unlist(strsplit(fileList[i], '/', fixed=TRUE));
    originalFileName<-originalFileName[length(originalFileName)];
    fileBase<- unlist(strsplit(originalFileName, '.', fixed=TRUE))[1]
    fileName<-paste(graphFolder,fileBase, sep = '/');
    if(graphType == 'png'){
      pngTitle = paste0(fileName,'_S.png');
      ggsave(file=pngTitle, g);
    }
    if(graphType == 'pdf'){
      pdfTitle = paste0(fileName,'_S.pdf');
      ggsave(file=pdfTitle, g, width = 65, height = 20, units = "cm");
    }


  }
}
