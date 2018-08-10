# Libraries ####
# install.packages("circlize")
# install.packages("superheat")
# install.packages("plotly")
# install.packages("tidyverse")
# install.packages("FactoMineR")
# install.packages("factoextra")
# install.packages("readxl")
# install.packages("corrplot")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("ggrepel")
# install.packages("RColorBrewer")
# source("http://bioconductor.org/biocLite.R")
# biocLite("ComplexHeatmap")

# Load Libraries ####
rm(list=ls())
library(plotly)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(readxl)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(RColorBrewer)
library(ComplexHeatmap)
library(circlize)
library(superheat)

# Set WD ####

setwd("C:/Users/AAAA/Desktop/")

# Main Function Code ####


CorrHeatPCA <- function(excel, sheet, uniqueList = "none", PCA = T, Heat = T, Correlations = T, kCenters = 5, heatWidth = 2000, heatHeight = 4000, useGroup = (rep(c("Fb", "Fg", "Sb", "Sg"), each = 4))){
  
  # Path/Folder Creation ####
  createdPath = paste0("Output - ", substring(timestamp(), 14, 19))
  dir.create(path = createdPath)
  
  # Read in Data ####
  a <- read_excel(path = excel, sheet = sheet, trim_ws = T)
  
  a.df <- as.data.frame(a)
  
  a.df.list <- if(uniqueList == "none") {
    a.df.list <- a.df
  } else {
    b <- read_excel(path = excel, sheet = uniqueList, trim_ws = T)
    a.df.list <- a.df[a.df$`Probe Name` %in% b$`Probe Name`,]
    a.df.list
  }
  

  # Filter and tidy data ####
  
  
  # a.df.list <- a.df[a.df$`Probe Name` %in% b$`Probe Name`,]
  
  rownames(a.df.list) <- a.df.list$`Probe Name`
  
  a.df.list$`Probe Name` <- NULL
  
  a.df.list <- as.data.frame(a.df.list)
  dim(a.df.list)
  head(a.df.list)
  typeof(a.df.list)
  
  a.df.t <- t(a.df.list)
  a.df.t <- as.matrix(a.df.t)
  group <- useGroup
  a.bind <- cbind.data.frame(a.df.t, group)
  
  dim(a.bind)
  a.bind[, dim(a.bind)[2]]
  
  # PCA ####
  if(PCA == T) {
    # Initial PCA function data ####
    res.pca <- PCA(a.bind[,-(dim(a.bind)[2])], graph = T, scale.unit = T)

    # Contribution Saving ####    
    fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
    ggsave(
      filename = paste0(
        createdPath,"/",
        "PCA1 - ", 
        sheet, 
        " - ",
        substring(timestamp(), 14, 22),".",
        substring(timestamp(), 24, 25),
        ".png"),
      width = 150, 
      height = 150, units = "mm")
    
    fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
    ggsave(
     filename = paste0(
        createdPath,"/",
        "PCA2 - ", 
        sheet, 
        " - ",
        substring(timestamp(), 14, 22),".",
        substring(timestamp(), 24, 25),
        ".png"),
      width = 150, 
      height = 150, units = "mm")
    
    fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)
    ggsave(
      filename = paste0(
        createdPath,"/",
        "PCA3 - ", 
        sheet, 
        " - ",
        substring(timestamp(), 14, 22),".",
        substring(timestamp(), 24, 25),
        ".png"),
      width = 150, 
      height = 150, units = "mm")
    # PCA - Biplot ####
    fviz_pca_biplot(res.pca, alpha.var="contrib", col.var = "gray72") + 
      theme_minimal()
    ggsave(
      filename = paste0(
        createdPath,"/",
        "PCA - Biplot - ", 
        sheet, 
        " - ",
        substring(timestamp(), 14, 22),".",
        substring(timestamp(), 24, 25),
        ".png"),
      width = 200, 
      height = 150, units = "mm")
    
    # Scree Plot ####
    fviz_eig(res.pca, addlabels =T, 
             barfill="green", 
             barcolor ="darkblue",
             linecolor ="red") + labs(
               title = "Variances - PCA",
               x = "Principal Components",
               y = "% of variances")
    
    ggsave(
      filename = paste0(
        createdPath,"/",
        "PCA - Scree plot - ", 
        sheet, 
        " - ",
        substring(timestamp(), 14, 22),".",
        substring(timestamp(), 24, 25),
        ".png"),
      width = 200, 
      height = 150, units = "mm")
    
    # Var Corr ####
    png(
      filename = paste0(
        createdPath,"/",
        "Corr-Var-", 
        sheet, 
        " - ",
        substring(timestamp(), 14, 22),".",
        substring(timestamp(), 24, 25),
        ".png"), 
      width = 750, 
      height = 2500, units = "px")
    
    var <- get_pca_var(res.pca)
    corrplot(var$cos2, is.corr=FALSE)
    dev.off()
    
    
    # PCA Individual Plot ####
    g1 <- fviz_pca_ind(res.pca, habillage = a.bind$group, pointsize = 2, repel = T, addEllipses = T, show.centroids = T)
    
    g1 + scale_shape_manual(values=c(15:22))
    ggsave(
      filename = paste0(
        createdPath,"/",
        "PCA - Individual -", 
        sheet, 
        " - ",
        substring(timestamp(), 14, 22),".",
        substring(timestamp(), 24, 25),
        ".png"),
      width = 200, 
      height = 150, units = "mm")
    # PCA Individual Plot - No label ####
    g1 <- fviz_pca_ind(res.pca, habillage = a.bind$group, pointsize = 2, repel = F, addEllipses = T, show.centroids = T, label = "none")
    
    g1 + scale_shape_manual(values=c(15:22))
    ggsave(
      filename = paste0(
        createdPath,"/",
        "PCA - Individual - No Label - ", 
        sheet, 
        " - ",
        substring(timestamp(), 14, 22),".",
        substring(timestamp(), 24, 25),
        ".png"),
      width = 200, 
      height = 150, units = "mm")
    # PCA 3D pre ####
    pca <- prcomp(a.bind[,-(dim(a.bind)[2])], scale. = T)
    
    
    data2 <- as.data.frame(pca$x)
    f <- list(
      family = "Arial",
      size = 23,
      color = "black")
    m <- list(l=200, r=200, b=200, t=200)
    data2$groups <- group
    eig.val <- get_eigenvalue(pca)
    
    # Plotly Labels ####
    p <- plot_ly(data2, 
                 x = ~pca$x[,1], 
                 y = ~pca$x[,2], 
                 z = ~pca$x[,3], 
                 color = ~data2$groups, 
                 colors = c("#ff0000", "#3366cc", "#ff66cc", "#3399ff"),
                 marker=list(size=12, opacity=1),  
                 hoverinfo = 'text', 
                 text = ~paste('</br> PCA1: ',
                               pca$x[,1], '</br> PCA2: ',
                               pca$x[,2], '</br> PCA3: ',
                               pca$x[,3], '</br> </br> Group: ', 
                               data2$groups)) %>% 
      layout(scene = list(
        xaxis = list(title = paste0(
          'PCA1 ', 
          as.character(round(eig.val[1,1], 1)), "%"),
          titlefont = f,
          gridwidth = 2,
          gridcolor = "black",
          opacity = 0.7), 
        yaxis = list(title = paste0(
          'PCA2 ', 
          as.character(round(eig.val[2,1], 1)), "%"),
          titlefont = f,
          gridwidth = 2,
          gridcolor = "black"),
        zaxis = list(title = paste0(
          'PCA3 ', 
          as.character(round(eig.val[3,1], 1)), "%"),
          titlefont = f,
          gridwidth = 2,
          gridcolor = "black")),
        font = list(
          family = "Arial", 
          size = 15, 
          color = "black"
        ),
        legend = list(
          # xanchor = "left",
          # yanchor = "bottom",
          borderwidth = 2,
          bordercolor = "gray86",
          bgcolor = "gray20",
          font = list(
            family = "Arial", 
            size = 17, 
            color = "black"
          )
        )
      )

    # p
    htmlwidgets::saveWidget(as_widget(p), file = (
      paste0("index - ", 
             excel, " - ", 
             sheet, " - ",
             substring(timestamp(), 10, 19),
             ".html")))
    
    # Plotly No Labels ####
    p2 <- plot_ly(data2, 
                 x = ~pca$x[,1], 
                 y = ~pca$x[,2], 
                 z = ~pca$x[,3], 
                 color = ~data2$groups, 
                 colors = c("#ff0000", "#3366cc", "#ff66cc", "#3399ff"),
                 marker=list(size=12, opacity=1),  
                 hoverinfo = 'text', 
                 text = ~paste('</br> PCA1: ',
                               pca$x[,1], '</br> PCA2: ',
                               pca$x[,2], '</br> PCA3: ',
                               pca$x[,3], '</br> </br> Group: ', 
                               data2$groups)) %>% 
      layout(scene = list(
        xaxis = list(title = "",
          gridwidth = 2,
          gridcolor = "black",
          opacity = 0.7), 
        yaxis = list(title = "",
          gridwidth = 2,
          gridcolor = "black"),
        zaxis = list(title = "",
          gridwidth = 2,
          gridcolor = "black")),
        font = list(
          family = "Arial", 
          size = 15, 
          color = "black"
        ),
        legend = list(
          # xanchor = "left",
          # yanchor = "bottom",
          borderwidth = 2,
          bordercolor = "gray86",
          bgcolor = "gray20",
          font = list(
            family = "Arial", 
            size = 17, 
            color = "black"
          )
        )
      )
    
    # p
    htmlwidgets::saveWidget(as_widget(p2), file = (
      paste0("index - No Label - ", 
             excel, " - ", 
             sheet, " - ",
             substring(timestamp(), 10, 19),
             ".html")))
    # PCA Done ####
    message("PCA done!")
  }
  
  # Heatmap ####
  if(Heat == T){
    j <- (a.bind[,-(dim(a.bind)[2])])
    x <- data.matrix(j)
    
    x1 <- scale(as.matrix(x))
    heatmap(as.matrix(x1))
    
    # km.res <- kmeans(t(x1), centers = 2)
    # fviz_nbclust(x1, kmeans, method = "gap_stat")
    # fviz_nbclust(x1, kmeans, method = "wss")
    # fviz_nbclust(x1, kmeans, method = "silhouette")
    # 
    # write.csv(km.res$cluster, file = "clusters.csv")
    # z1 <- read.csv(file = "clusters.csv", stringsAsFactors = F)
    # 
    # whatiwant <- structure(as.integer(z1$x), names = as.character(x = z1$X))
    # whatiwant
    # 
    # whatiwant2 <- whatiwant[match(names(x = km.res$cluster), names(whatiwant))]
    
    t2 <-Heatmap(t(j), 
                 # split = whatiwant2,
                 # cell_fun = cell_fun2,
                 cluster_rows = TRUE, 
                 cluster_columns = TRUE,
                 col = colorRamp2(c(-.4, 0, .4), c("green", "gray18", "red")),
                 clustering_distance_columns = "spearman",
                 clustering_distance_rows = "spearman",
                 show_row_names = T,
                 row_dend_reorder = F,
                 km = kCenters,
                 show_column_names = T)
    tiff(
      filename = paste0(
        createdPath,"/",
        "Heat-", 
        sheet, 
        " - ",
        substring(timestamp(), 14, 22),".",
        substring(timestamp(), 24, 25),
        ".tiff"), 
      width = heatWidth, 
      height = heatHeight, res = 300)
    
    draw(t2)
    
    dev.off()
    # Heatmap No split ####
    t2 <-Heatmap(t((j)), 
                 # split = whatiwant2,
                 # cell_fun = cell_fun2,
                 cluster_rows = TRUE, 
                 cluster_columns = TRUE,
                 col = colorRamp2(c(-.4, 0, .4), c("green", "gray18", "red")),
                 clustering_distance_columns = "spearman",
                 clustering_distance_rows = "spearman",
                 show_row_names = F,
                 show_column_names = T)
    tiff(
      filename = paste0(
        createdPath,"/",
        "Heat No Split -", 
        sheet, 
        " - ",
        substring(timestamp(), 14, 22),".",
        substring(timestamp(), 24, 25),
        ".tiff"), 
      width = heatWidth, 
      height = heatHeight, res = 300)
    
    draw(t2)
    
    dev.off()
    # Heat Done ####
    message("Heat done!")
  }
  
  # Correlations ####
  if(Correlations == T){
  
    # Correlation split Data #### 
    j <- (a.bind[,-(dim(a.bind)[2])])
    
    # First Correlation ####
    png(filename = paste0(createdPath,"/",
      "Corr-r-", 
      sheet, 
      " - ",
      substring(timestamp(), 14, 22),".",
      substring(timestamp(), 24, 25), 
      ".png"), 
      width = 750, 
      height = 750)
    
    corrplot(cor(t(j)), 
             method = "color", 
             order = "hclust")
    
    dev.off()
    
    # Second Correlation ####
    png(
      filename = paste0(
        createdPath,"/",
        "Corr-c-", 
        sheet, 
        " - ",
        substring(timestamp(), 14, 22),".",
        substring(timestamp(), 24, 25),
        ".png"), 
        width = 750, 
        height = 750)
    
    corrplot(cor(j), 
             method = "color", 
             order = "hclust",tl.pos = F,tl.cex = 1)
    
    dev.off()
    
    # Done Correlations ####
    message("Corr done!")
  }
  
}

VolcanoMake <- function(excel, textRepel = T, colorPresent = T, ylims = c(0,10), xlims = c(-5,5), pValueMin = 0.0549, repelForce = 1, textSize = 5, colorUp = "red", colorDown = "green", colorLow = "gray", overrideTitle = F, newTitleAdd){
  
  checkList <- c(
    "FgSg" = "Fg vs. Sg",
    "FbFg" = "Fb vs. Fg",
    "SbSg" = "Sb vs. Sg",
    "FgSb" = "Fg vs. Sb",
    "FbSg" = "Fb vs. Sg",
    "FbSb" = "Fb vs. Sb"
  )
  
    
  for (i in 1:6) {
    message(names(checkList)[i])
    message(checkList[[i]])
    # data1 <- read.csv(paste0(names(checkList)[i],".Both.NP.HIPP.csv"))
    data1 <- read_excel(path = excel, 
      sheet = (names(checkList)[i]), trim_ws = T)
    
    title1 <- checkList[[i]]
    
    title1 <- paste0(title1, ifelse(overrideTitle == T, newTitleAdd," - AutoGen"))
    dim(data1)
    head(data1)
    
    message("Max FC: ", max(data1$log2FC))
    message("Min FC: ", min(data1$log2FC))
    
    message("Max logP: ", max(data1$Neglog10p))
    message("Min logP: ", min(data1$Neglog10p))
    
    data1$threshold = as.factor(data1$Pvalue < pValueMin)
    data1$Significant <- ifelse(data1$Pvalue < pValueMin, "p-value < 0.05", "Not Sig")
    

    
    set.seed(6)
    
    # First Pass Graphing ###
    breaks <- c(-0.5,0,0.5)
    
    dataP=subset(data1, 
                 Pvalue < pValueMin)
    dataUp = subset(dataP, 
                    log2FC > 0.0)
    dataDown = subset(dataP, 
                      log2FC < 0.0)
    
    
    g1 <- ggplot(data=data1, aes(x=data1$log2FC, y =data1$Neglog10p)) +
      geom_point(aes(color=data1$log2FC), size = 3) +
      xlim(xlims) +
      ylim(ylims) +
      xlab("log 2 fold change") + ylab("-log 10 p-value") +
      theme_bw() +
      theme(legend.position="none") + ggtitle(title1)  +
      theme(plot.title = element_text(face="bold", size=21, hjust=0.5)) +
      geom_vline(aes(xintercept=0.5), 
                 colour="black", 
                 linetype="dashed") + 
      geom_vline(aes(xintercept=-0.5), 
                 colour="black", 
                 linetype="dashed") +
      geom_vline(aes(xintercept=0.0), 
                 colour="black") +
      geom_hline(aes(yintercept=0.0), 
                 colour = "black") +
      geom_hline(aes(yintercept=1.3), 
                 colour = "gray", linetype = "dashed", size = 1) +
      geom_hline(aes(yintercept=2.3),
                 colour = "black", 
                 linetype = "dashed", 
                 size = 1.05) + scale_colour_gradientn(
                   colours = c(colorLow,colorLow,colorLow), 
                   breaks = breaks, 
                   labels = format(breaks)) + 
      geom_point(data = dataUp, aes(x=log2FC,y=Neglog10p), 
                 col = ifelse(colorPresent == T, colorUp, "gray"), size =3) +
      geom_point(data = dataDown, aes(x=log2FC,y=Neglog10p), 
                 col = ifelse(colorPresent == T, colorDown, "gray"), size = 3) +
      annotate("text", 
               x = xlims[1], 
               y = 1.4, 
               label = "p = 0.05", 
               size = 5, 
               hjust = 0, 
               vjust = 0,  
               parse = FALSE, 
               fontface = 'italic') +
      annotate("text", 
               x = xlims[1], 
               y = 2.4, 
               label = "p = 0.005", 
               size = 5, 
               hjust = 0, 
               vjust = 0,  
               parse = FALSE, 
               fontface = 'italic') +
      theme(plot.title = element_text(hjust = 0.5)) + 
      theme(
        axis.text=element_text(size=12), 
        axis.title=element_text(size=17,face="bold")) + 
      theme(
        axis.text.x = element_text(face="bold", color="black", size=16, angle=0),
        axis.text.y = element_text(face="bold", color="black", size=16, angle=0))
    
    g1
    
    ifelse(textRepel == T, 
           g1 + geom_text_repel(data=dataP, aes(log2FC,
                                                Neglog10p,
                                                label=Probe.Name),
                                force = repelForce, 
                                size =textSize), 
           g1)
    
    ggsave(filename = paste0(
      substring(timestamp(), 14, 19), "-", substring(title1, 0, last = 2),
      substring(title1, 8, 9),"-", substring(timestamp(), 14, 22),".",
      substring(timestamp(), 24, 25), 
      ifelse(colorPresent == T, " - color", ""),
      ifelse(textRepel == T, " - labels",""),
      "-p-values-Autogen.tiff"), 
      width = 25, height = 25, units = "cm")
    
  }

  # End ####
  message("Volcanos are done!")
}
  





# Session Info output to file ####
writeLines(capture.output(sessionInfo()), paste0(substring(timestamp(), 14, 19)," - test", " - session.txt"))



CorrHeatPCA(excel = "RandomData1.xlsx", sheet = "Centered", uniqueList = "NamesCheck")

VolcanoMake(excel = "RandomData1.xlsx", textRepel = T, ylims = c(0,4), xlims = c(-1.25,1.25), repelForce = 2,pValueMin = 0.0149, colorDown = "blue")
