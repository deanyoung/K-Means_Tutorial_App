library(dplyr)
library(ggplot2)

x1 <- c(1,1,0,5,6,4)
x2 <- c(4,3,4,1,2,0)

df <- data.frame(x1,x2)

#a)
ggplot(df,aes(x=x1,y=x2)) + geom_point(size=5)

#b)
set.seed(76)
df <- df %>% mutate(cluster=sample(c(1,2),nrow(df),replace=TRUE))
prev.cluster <- c()
print(df$cluster)

#e)
while (isTRUE(all.equal(prev.cluster,df$cluster)) == FALSE){
  
  prev.cluster <- df$cluster
  
#c)

  centroid <- df %>% group_by(cluster) %>% summarise(x1.c = mean(x1), x2.c = mean(x2))
  centroid <- centroid %>% as.matrix()
  
#d)
  df <- df %>% mutate(euclid.1 = ((x1 - centroid[[1,2]])^2) + ((x2 - centroid[[1,3]])^2), 
                      euclid.2 = ((x1 - centroid[[2,2]])^2) + ((x2 - centroid[[2,3]])^2))
  
  df <- df %>% mutate(cluster = ifelse(euclid.1 < euclid.2, 1, 2))
  print(df$cluster)

}

#f)

df$cluster <- as.factor(df$cluster)
ggplot(df, aes(x=x1,y=x2,col=cluster)) + geom_point(size=5)

