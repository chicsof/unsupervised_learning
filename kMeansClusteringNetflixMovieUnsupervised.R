movie_metadata <- read.csv(file="movie_metadata.csv",head = TRUE, sep = ",")
head(movie_metadata)

#remove data that can confuse algorithm
#trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#movie_metadata$gross <- trim(movie_metadata$gross)

#make into a matrix since we only need numerics
movie <- data.matrix(movie_metadata)
movie <- na.omit(movie)

#make thinks quick...for now
smple <- movie[sample(nrow(movie),500),]

#clastering columns
smple_short <- smple[,c(9,23)]
smple_matrix <- data.matrix(smple_short)

head(smple_matrix)

#elbow method for choosing k clusters
wss <- (nrow(smple_matrix)-1)*sum(apply(smple_matrix,2,var))
for (i in 2:15) wss[i]<-sum(kmeans(smple_matrix,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")
#from plot k=3

#clustering
cl <- kmeans(smple_matrix,3,nstart=25)
plot(smple_matrix, col =(cl$cluster +1) , main="k-means result with 2 clusters", pch=1, cex=1, las=1)
points(cl$centers, col = "black", pch = 17, cex = 2)

#measuring performance
as.numeric(cl$betweenss)/as.numeric(cl$totss)

#examine cluster: shows centroid coordinates
cl$centers

#examine how cluster affects fb likes
dSet <- aggregate(smple,by=list(cl$cluster),FUN=mean)
 c(as.character(dSet[1]),dSet$movie_facebook_likes)



