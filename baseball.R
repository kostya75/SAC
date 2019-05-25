library(plyr)
library(ggplot2)
baseball

#rbi= runs batten in
#ab= at bat, a number of times a player faced pitcher

baberuth<-subset(baseball, id == "ruthba01",select=c(id,year,rbi,ab))
baberuth<-transform(baberuth,cyear=year-min(year)+1)
baberuth

baseball<-subset(baseball,ab>=25,select=c(id,year,rbi,ab))
baseball<-ddply(baseball,.(id),transform,cyear=year-min(year)+1)

#baberuth plot
ggplot(baberuth,aes(x=cyear,y=rbi/ab))+geom_line()+theme_bw()


#all
xlim<-range(baseball$cyear,na.rm = T)
ylim<-range(baseball$rbi / baseball$ab, na.rm=TRUE)

plotpattern<-function(df){
  ggplot(df,aes(x=cyear,y=rbi/ab))+geom_line()+xlim(xlim)+ylim(ylim)
}

pdf("paths.pdf", width = 8, height = 4)
d_ply(baseball, .(reorder(id, rbi / ab)), failwith(NA, plotpattern), .print = TRUE)
dev.off()





pdf("paths.pdf", width = 8, height = 4)
d_ply(baseball, .(reorder(id, rbi / ab)), failwith(NA, plotpattern),.print = TRUE)
dev.off()


bmodel<-function(df) lm(rbi/ab~cyear,data=df)

bmodels<-dlply(baseball,.(id),bmodel)

tt<-bmodel(baberuth)
rsq<-function(x) summary(x)$r.squared
bcoefs<-ldply(bmodels,function(x) c(coef(x),rsquare=rsq(x)))
names(bcoefs)[2:3]<-c("intercept","slope")

baseballcoef<-merge(baseball,bcoefs,by="id")
subset(baseballcoef,rsquare>=0.99)$id

ggplot(bcoefs, aes(slope, intercept)) + 
  geom_point(aes(size = rsquare), alpha = 0.5) +
  geom_vline(xintercept = 0, size=0.5, colour="grey50") + 
  geom_hline(yintercept = 0, size = 0.5, colour="grey50")
last_plot()+xlim(c(-.01,.01))

