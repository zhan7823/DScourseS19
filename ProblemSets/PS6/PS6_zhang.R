
#
df <- data_from_oklahoma_county

attach(df)
summary(df)

names(df)

df1 <- df[43:63, ]

summary(df1)

G <- df1$`Number of Establishments`
hist(G)

density(G)

G.den <- density(G)
plot(G.den)
boxplot(G)
