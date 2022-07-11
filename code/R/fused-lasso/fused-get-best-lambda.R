a <- readRDS("results/CV-lasso/small-fused-cv-1k-stand-updated//err-mat-plots/err-line-plot.rds")
a
# a + stat_summary(aes(group=x),fun=mean)
b <- a + geom_smooth(aes(color = factor(fold)), method="loess")
b
# b + stat_summary(aes(y=),fun=mean)
c <- ggplot_build(b)
dat <- c$data
df1 <- dat[[1]]
df1 %>% arrange(x) -> df1
head(df1)
df2 <- dat[[2]]
head(df2)


# test %>% group_by(group) %>% summarize(m=mean(y)) -> q
# test %>% group_by(x) %>% summarize(mean_at_x=mean(y)) -> p

# chec overlapping 
df2 %>% mutate(rx=round(x, 2)) -> df2
head(df2)
range(df2$rx)
unique(df2$rx)
g1 <- df2 %>% filter(group==1)
plot(g1$x, g1$y)
ap1 <- approx(g1$x, g1$y, n=3000)
m <- loess(g1$y ~ g1$x)
plot(m)
pr <- predict(m, newdata = seq(7,11, by=0.0001))
plot(pr)
g1 %>% mutate(int_x = approx(x,y))



df2 %>% mutate(int_x = approx(x,y)$x, int_y = approx(x,y)$y) %>% summarise(kk = mean(int_y)) -> aha
ggplot(aha, aes(x=rx, y=kk)) + geom_line()


ggplot(data = p, aes(x=x, y=mean_at_x)) + geom_line()

approx(c(1,2,3,5),c(2,4,6,10))

test %>% group_by(x) %>% summarize(mean_at_x=mean(y)) %>% ungroup() -> test2
head(test2)
plot(test2$x, test2$mean_at_x)
ggplot(data = test2, aes(x=x, y=mean_at_x)) + geom_point()

# df2 contains lines
# now for each x compute mean of y
df2$x <- round(df2$x, 4)
df2 %>% group_by(x) %>% summarize(mean_at_x = mean(y)) %>% ungroup() -> test
head(test)
test %>% select(x, mean_at_x) %>% distinct -> test2
head(test2)
plot(x=test2$x, y=test2$mean_at_x)
ggplot(test2, aes(x=x, y=mean_at_x)) + geom_point()

# consider the range for each line
df2 %>% group_by(group) %>% summarise(range(x))
