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
d_ply(baseball, .(reorder(id, rbi / ab)), failwith(NA, plotpattern),.print = TRUE)
dev.off()
