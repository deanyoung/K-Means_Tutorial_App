library(dplyr)
library(ggplot2)

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
        xlab(xlab) + ylab(ylab)
  
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

km.interate <- function(df, k){
  
  glist <- list()
  df <- km.init(df,k)
  
  orig.colnames <- colnames(df)
  colnames(df) <- c("x1","x2","cluster")
  
  base <- km.ggplot(df,orig.colnames[1],orig.colnames[2])
  glist[[length(glist)+1]] <- base
  
  prev.cluster <- c()
  
  #while (isTRUE(all.equal(prev.cluster,df$cluster)) == FALSE){
    
    centroid <- km.centroid(df)
    
    base.cent <- base + geom_point(data=centroid, aes(x=x1.c,y=x2.c,fill=cluster), size=20, shape=13)
    
    glist[[length(glist)+1]] <- base.cent
    
    eu <- km.euclid(df,centroid)
    
    
  #}
    
  return(eu)

}

x1 <- c(1,1,0,5,6,4)
x2 <- c(4,3,4,1,2,0)

t <- data.frame(x1,x2)
glist <- c()

g <- km.interate(t,3)



