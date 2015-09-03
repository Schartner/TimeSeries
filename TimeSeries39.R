data<-read.csv("TimeSEriesRhine.csv", sep=";")
ts<-ts(data$TotN_conc,start=data$Year[1],frequency = 12)
plot(ts, type="l")
plot(stl(ts, s.window=time(ts)))

#need to change the x axis still
model.linear<-lm(ts~time(ts))
summary(model.linear)
#is significant
resi<-rstudent(model.linear)
acf(resi)

month.<-season(ts)
#ummy variable construct, compares to january, even if not 
#significant, might be only difference is too small to january
m.season<-lm(ts~month. + time(ts))
summary(m.season)
plot(ts)
lines(as.vector(fitted(m.season)),col="red")
plot(fitted(m.season),type="l") 
#works like this but not over the plot
#need to fix
resi.season<-rstudent(m.season)
plot(resi.season, x=time(ts), type="l")
points(y=resi.season,x=time(ts), pch=as.vector(season(ts)))
acf(resi.season)
qqplot(time(ts),resi.season)

#e
data(JJ)
plot(JJ, type="l")

#need to change the x axis still
model.linear.JJ<-lm(JJ~time(JJ))
summary(model.linear.JJ)
abline(model.linear.JJ, col="red")

#is significant
resi.JJ<-rstudent(model.linear)
acf(resi.JJ)

month.JJ<-season(JJ)
#ummy variable construct, compares to january, even if not 
#significant, might be only difference is too small to january
m.season<-lm(JJ~month.JJ + time(JJ))
summary(m.season)
plot(JJ)
lines(as.vector(fitted(m.season)),x=time(JJ),col="red")
plot(fitted(m.season),type="l") 
#works like this but not over the plot
#need to fix
resi.JJ.season<-rstudent(m.season)
plot(resi.JJ.season, x=as.time(time(JJ)), type="l")
points(y=resi.JJ.season,x=time(JJ), pch=as.vector(season(JJ)))
acf(resi.JJ.season)
qqplot(time(JJ),resi.JJ.season)


#3
data<-read.csv("Silver.csv", sep=";")
data$EURO<-as.numeric(data$EURO)
plot(x=data$Date,y=data$EURO)
#heuristic to leave out the first observation
plot(x=data$Date[-1],y=diff(log(data$EURO)))
acf(diff(log(data$EURO)))
qqplot(diff(log(data$EURO)),x=data$Date)

BoxCox.ar(data$EURO)
plot(x=data$Date[-1],y=diff(data$EURO^0.75))
qqplot(x=data$Date,y=diff(data$EURO^0.75))

plot(x=data$Date[-1],y=diff(data$EURO))
qqplot(x=data$Date,y=diff(data$EURO))

plot(x=data$Date[-1],y=diff(data$EURO^0.5))
qqplot(x=data$Date,y=diff(data$EURO^0.5))