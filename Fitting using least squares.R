# FITTING OF STRAIGHT LINE
sample_data<-data.frame(x=0:5,y=c(5,7,9,11,13,15))
linear_model1<-lm(y~x,data=sample_data)
summary(linear_model1)
plot(sample_data$x,sample_data$y)
x_axis<-seq(0,5,length=6)
lines(x_axis,predict(linear_model1,data.frame(x=x_axis)),col='green')


# FITTING OF PARABOLA
sample_data<-data.frame(x=c(1,1.5,2,2.5,3,3.5,4,4.5),y=c(2,18.5,29,42,57.5,75.5,96,119))
linear_model2<-lm(y~poly(x,2,raw=TRUE),data=sample_data)
summary(linear_model2)
plot(sample_data$x,sample_data$y)
lines(sample_data$x,predict(linear_model2,data.frame(x=x_axis)),col='red')


# FITTING OF POWER CURVE
x<-c(1,2,3,4,5)
y<-c(3,12,27,48,75)
plot(x,y)
ds<-data.frame(x,y)
m<-lm(y~x,data=ds)
plot(y~x,main="known cubic, with noise")
lines(x,y,col='green')


# FITTING OF EXPONENTIAL CURVE
Time=c(1,2,3,4,5,6,7,8,9)
Counts=c(20,30,52,77,135,211,326,550,1052)
A<-data.frame(Time,Counts)
names(A)
exponential.model<-lm(log(Counts)~Time)
summary(exponential.model)
timevalues<-seq(1:9)
Counts.exponential2<-exp(predict(exponential.model,list(Time=timevalues)))
plot(Time,Counts,pch=16)
lines(timevalues,Counts.exponential2,lwd=2,col='red',xlab="Time",ylab="Counts")