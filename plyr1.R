library(plyr)
# http://www.biostat.jhsph.edu/~iruczins/teaching/jf/ch13.pdf
library(MASS)
library(ggplot2)

one <- ozone[1, 1, ]

month<-ordered(rep(1:12,length=72))

model <- rlm(one ~ month - 1)
model_lm<-lm(one~month-1)

deseas <- resid(model)

deseas_lm <- resid(model_lm)

ggplot()+geom_line(aes(x=deseas,y=one,group=month,color=month),size=2)

deseasf <- function(value) rlm(value ~ month - 1)
deseasf
# models : array to lists
models<-alply(ozone, 1:2, deseasf)
# list of models to array
deseas<-laply(models, resid)
str(models[1])
failed<-laply(models,function(x) !x$converged) 

seqmon<-function(x) {
  temp<-x %% 12
  if(temp!=0) temp else 12
}

osonedf<-matrix(0,ncol=5,nrow=41472,dimnames=list(seq_len(41472),c("lat","long","value","time","month")))
inc<-1
for (z in 1:72){
  for (i in 1:24){
    for (j in 1:24){
      osonedf[inc,1]<-attr(ozone,"dimnames")[["lat"]][i]
      osonedf[inc,2]<-attr(ozone,"dimnames")[["long"]][j]
      osonedf[inc,3]<-ozone[i,j,z]
      osonedf[inc,4]<-attr(ozone,"dimnames")[["time"]][z]
      osonedf[inc,5]<-seqmon(z)
      inc<-inc+1
    }
  }
}

osonedf<-as.data.frame(osonedf)
osonedf[]<-lapply(osonedf,as.character)
osonedf[]<-lapply(osonedf,as.numeric)


#split
locs<-with(osonedf,list(osonedf$long,osonedf$lat))
pieces<-split(osonedf,locs)
#create func
deseasef_df<-function(df) { rle(value~month-1,data=df)}
models<-lapply(pieces, deseasef_df)
deseas_base<-mapply(function(model,df){ cbind(df[rep(1, 72), c("lat", "long")],resid(model))},models,pieces)
tt<-do.call("rbind",deseas_base)


lapply(models,function(model) resid(model))
