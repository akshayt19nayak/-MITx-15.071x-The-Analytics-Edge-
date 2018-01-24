IBM<-read.csv(file.choose())
CocaCola<-read.csv(file.choose())
Boeing<-read.csv(file.choose())
ProcterGamble<-read.csv(file.choose())
GE<-read.csv(file.choose())


IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

median(Boeing$StockPrice)
max(CocaCola$StockPrice)
sd(ProcterGamble$StockPrice)

plot(x=CocaCola$Date,y=CocaCola$StockPrice,type='l')


plot(x=ProcterGamble$Date[333:335],y=ProcterGamble$StockPrice[333:335],type='l',ylim=c(0,210))
abline(v=as.Date(c("2000-03-01")), lwd=2)
lines(CocaCola$Date,CocaCola$StockPrice,col='red')



abline(h=mean(CocaCola$StockPrice),lwd=1)
abline(h=mean(ProcterGamble$StockPrice),lwd=1)

Boeing$StockPrice[333:335]

nrow(CocaCola[CocaCola$StockPrice<mean(CocaCola$StockPrice),])
nrow(ProcterGamble[ProcterGamble$StockPrice<mean(ProcterGamble$StockPrice),])

max(tapply(GE$StockPrice,months(GE$Date),mean))

