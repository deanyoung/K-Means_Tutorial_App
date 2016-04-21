library(dplyr)
library(ggplot2)
library(shiny)

current.iter <- 0
current.step <- "a"
counter <- 1


df.check <- function(df){
  
  if(TRUE %in% is.na(df)){
    stop("Dataframe contains missing values.")
  }
  
  if(ncol(df) > 2){
    stop("Dataframe has more than 2 variables. Principal component decomposition currently not supported.")
  }
  
  for(x in sapply(df,class)){
    
    if(x != "numeric"){
      stop("Dataframe contains 1 or more non-numeric type columns.")
      
    }
    
  }
  
}

km.init <- function(df,k,seed=76){
  
  samp.vec <- c(seq(1,k,1))
  
  set.seed(seed)
  df.mod <- df %>% mutate(cluster=as.factor(
    sample(samp.vec,nrow(df),replace=TRUE)
  ))
  
  return(df.mod)
  
  
}

km.ggplot <- function(df, xlab, ylab, reg.size=5){
  
  p <- ggplot(data=df, aes(x=x1,y=x2,col=cluster)) + geom_point(size=reg.size) +
    xlab(xlab) + ylab(ylab) + ggtitle("K-Means")
  
  return(p)
  
}

km.centroid <- function(df){
  
  
  centroid <- df %>% group_by(cluster) %>% summarise(x1.c = mean(x1), x2.c = mean(x2))
  
  return(centroid)
}

km.euclid <- function(df,centroid){
  
  
  for(i in 1:nrow(centroid)){
    
    i.str <- as.character(i)
    
    df[,i.str] <- sqrt(((df$x1 - centroid[[i,2]])^2) + ((df$x2 - centroid[[i,3]])^2))
  }
  
  return(df)
  
}

km.reassign <- function(df){
  
  new.cluster <- c(rep(NA,nrow(df)))
  temp.df <- df %>% select(-x1,-x2,-cluster)
  
  for(i in 1:nrow(temp.df)){
    
    min <- Inf
    
    for(j in 1:ncol(temp.df)){
      
      if(temp.df[i,j] < min){
        min <- temp.df[i,j]
        new.cluster[i] <- j
        
      }
      
    }
    
  }
  
  df[["cluster"]] <- as.factor(new.cluster)
  
  return(df)
  
}

km.iterate <- function(df, k){
  
  df.check(df)
  
  glist <- list()
  df <- km.init(df,k)
  
  orig.colnames <- colnames(df)
  colnames(df) <- c("x1","x2","cluster") # reassign variable names to be abstract enough for functions to recognize
  
  base <- km.ggplot(df,orig.colnames[1],orig.colnames[2])
  glist[[length(glist)+1]] <- base
  
  prev.cluster <- c()
  
  while (isTRUE(all.equal(prev.cluster,df$cluster)) == FALSE){
    
    prev.cluster <- df$cluster # store previous cluster
    
    centroid <- km.centroid(df) # compute centroid
    gg.cent <- geom_point(data=centroid, aes(x=x1.c,y=x2.c,fill=cluster), size=20, shape=13) # layer for centroid
    
    base.cent <- base + gg.cent # plot clusters + new centroid
    
    glist[[length(glist)+1]] <- base.cent # add to ggplot list
    
    df <- km.euclid(df,centroid) %>% km.reassign() # compute euclidean dist. and reassign clusters
    
    base <- km.ggplot(df,orig.colnames[1],orig.colnames[2]) # reassigned clusters, new base
    base.cent <- base + gg.cent # plot with old centroid
    
    glist[[length(glist)+1]] <- base.cent # add to ggplot list
    
  }
  
  return(glist)
  
}

