df1 <- as.data.frame(iris)

df <- createDataFrame(iris)

class(df1)

class(df)

head(select(df, df$Sepal_Length, df$Species))

head(select(df1, df1$Sepal_Length, df$Species))
#there is an error function. because there is no inherited method for this function.

head(filter(df, df$Sepal_Length>5.5))

head(filter(df1, df1$Sepal_Length>5.5))
#the same as last one, showing me there is an error in function.

head(filter(select(df, df$Sepal_Length, df$Species), df$Sepal_Length>5.5))

head(summarize(groupBy(df, df$Species), mean=mean(df$Sepal_Length), count=n(df$Sepal_Length)))

df2 <- head(summarize(groupBy(df, df$Species), mean=mean(df$Sepal_Length), count=n(df$Sepal_Length)))

head(arrange(df2, asc(df2$Species)))
