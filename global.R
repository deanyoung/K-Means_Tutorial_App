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

km.init <- function(df,k,seed){
  
  samp.vec <- c(seq(1,k,1))
  
  set.seed(seed)
  df.mod <- df %>% mutate(cluster=as.factor(
    sample(samp.vec,nrow(df),replace=TRUE)
  ))
  
  return(df.mod)
  
  
}

km.ggplot <- function(df, xlab, ylab, reg.size=5){
  
  p <- ggplot(data=df, aes(x=x1,y=x2,col=cluster)) + geom_point(size=reg.size) +
    xlab(xlab) + ylab(ylab) + ggtitle("K-Means") +
    scale_x_continuous(breaks=seq(-3,3,1)) +
    scale_y_continuous(breaks=seq(-3,3,1)) +
    coord_cartesian(xlim = c(-3, 3), ylim=c(-3,3)) +
    theme(aspect.ratio = 1)
  
  
  return(p)
  
}

km.centroid <- function(df){
  
  
  centroid <- df %>% group_by(cluster) %>% summarise(x1.c = mean(x1), x2.c = mean(x2))
  
  return(centroid)
}

km.euclid <- function(df,centroid){
  
  df <- df %>% select(x1,x2,cluster)
  
  for(x in centroid$cluster){
    
    x.str <- as.character(x)
    
    df[,x.str] <- sqrt(((df$x1 - as.numeric(centroid[centroid$cluster==x.str,2]))^2) + 
                         ((df$x2 - as.numeric(centroid[centroid$cluster==x.str,3]))^2))
  }
  
  return(df)
  
}

km.reassign <- function(df){
  
  new.cluster <- c(rep(NA,nrow(df)))
  temp.df <- df %>% select(-x1,-x2,-cluster)
  
  for(i in 1:nrow(temp.df)){
    
    min <- Inf
    
    for(x in colnames(temp.df)){
      
      if(temp.df[i,x] < min){
        min <- temp.df[i,x]
        new.cluster[i] <- x
        
      }
      
    }
    
  }
  
  df[["cluster"]] <- as.factor(new.cluster)
  
  return(df)
  
}

km.iterate <- function(df, k, seed){
  
  df.check(df)
  
  df <- scale(df) %>% as.data.frame() # standardize variables to z-score
  
  glist <- list()
  df <- km.init(df,k,seed)
  
  orig.colnames <- colnames(df)
  colnames(df) <- c("x1","x2","cluster") # reassign variable names to be abstract enough for functions to recognize
  
  base <- km.ggplot(df,orig.colnames[1],orig.colnames[2])
  glist[[length(glist)+1]] <- base
  
  count <- 0
  
  prev.cluster <- c()
  
  vec <- c()
  
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
    
    count <- count + 1
    
    # check if a centroid has been "isolated" from all data points
    if(base$data$cluster %>% levels() %>% length() < gg.cent$data$cluster %>% levels() %>% length()){
      
      vec <- append(vec,count) # take note of step if so
      
    }
    
  }
  
  final.list <- list(glist,vec)
  
  return(final.list)
  
}

