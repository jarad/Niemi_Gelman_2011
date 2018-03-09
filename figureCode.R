# Load Zimbabwe confirmed case data
zimbabwe <-read.csv("ZimbabweMeaslesDataWithProvinces.csv")
zimbabwe$date <- as.Date(as.character(zimbabwe$date))
zimbabwe.fixed <- subset(zimbabwe, date != '2010-01-24', select=c(district, province, date, total)) # new.cases column is unreliable

# Dates of mass vaccination campaign
vac.start <- as.Date("2010/05/24")
vac.end  <- as.Date("2010/06/02")

# Confirmed to suspected ratio based on WHO data
p <- 693/13783 

# Change to .05 to make axes easier to read
p <- 0.05

############################ Extract data for difference districts/provinces #######################


harare <- subset(zimbabwe.fixed, district=="harare")

bulawayo <- subset(zimbabwe.fixed, province=="bulawayo")

tmp      <- subset(zimbabwe.fixed, province %in% c("mashonaland central", "mashonaland east", "mashonaland west"), select=c(date, total))
mashonaland <- aggregate(tmp$total, list(tmp$date), sum)
names(mashonaland) <- c("date","total")

############################ Default R figure ######################################################

png("RDefaultFigure.png")
plot(harare$date, harare$total)
dev.off()

############################# Improved figure functions ##############################################

create.figure <- function(dat,header,simple=F,...) {
  attach(dat)
  new <- diff(c(0,total))
  new[new<0] <- 0         # Force the number of new cases to be non-negative
  new[new==0] <- .3   # Make the zero points more readable
  
  plot(date, new, type='p', pch=19, axes=F, frame.plot=T, xlab='', ylab='', xlim=c(min(date)-15, as.Date("2011/1/1")), ylim=c(0,40), xaxs="i", yaxs="i", bty="l", main=header)
  lines(smooth.spline(date, new), col='red') 
  lines(smooth.spline(date, new*3), col='red', lty=2) 
  lines(smooth.spline(date, new/3), col='red', lty=2)
  polygon(c(vac.start,vac.start,vac.end,vac.end), c(-10,50,50,-10), col=rgb(46/256,139/256,87/256,.5), border=NA)
  detect.start <- as.Date(c("2009/11/01"))
  detect.end <- as.Date(c("2009/11/03"))
  polygon(c(detect.start,detect.start,detect.end,detect.end), c(-10,50,50,-10), col=rgb(46/256,139/256,87/256,.5), border=NA)
    axis(1, seq(as.Date("2009/10/1"), as.Date("2011/1/1"), "3 months"), labels=F) 
  
  if (!simple){
    title(ylab='New confirmed cases each week')
    axis(2, seq(0,40,by=10))
    axis(1, seq(as.Date("2009/11/15"), as.Date("2011/1/15"), "3 months"), tick=F, label=c("Q4","Q1","Q2","Q3","Q4"), line=-.5)
    axis(1, as.Date("2010/7/1"), labels="2010", line=.7, tick=F)
    axis(1, as.Date("2010/1/1"), labels=F, tck=-.12)
    est <- seq(0, by=200, length=5)
    axis(4, est*p, est, col.ticks='red', col.axis='red')
    axis(4, 400*p, "Estimated total cases per week", tick=F, col.axis='red', line=1.5)
    arrows(as.Date("2009/12/01"), 30 ,detect.end, 28, length=0.1, col='seagreen')
    text(as.Date("2009/12/01"), 30, "Outbreak\ndetected", pos=4, col='seagreen')
    arrows(as.Date("2010/07/02"), 30,as.Date("2010/05/28"), 28, length=0.1, col='seagreen')
    text(as.Date("2010/07/02"), 30, "Mass vaccination campaign\n(30 weeks after detection)", pos=4, col='seagreen')
    legend(as.Date("2010/08/10"), 18, c("Estimated total cases\n(see axis on right)","95% interval"), col='red', lty=1:2)
  }
  
  detach(dat)
}


##################################################################################################

png("RImprovedFigure.png", height=400, width=600)
par(mar=c(3,3,3,3.5), mgp=c(2,.7,0), tck=-.02)
create.figure(harare, "Progress of measles outbreak in Zimbabwe,\nbefore and after the vaccination campaign")
dev.off()


############################# Small multiples #############################################


png("smallMultiplesSimple.png", height=700, width=500)
par(mar=c(3,4.5,2,0), mfrow=c(3,1))
create.figure(harare,"",      simple=T)
    title(ylab='New confirmed cases each week', cex.lab=1.5)
    axis(2, seq(0,40,by=10), cex.axis=1.5)
    mtext ("Harare                   ", 3, cex=1.5)
create.figure(bulawayo,"",    simple=T)
    title(ylab='New confirmed cases each week', cex.lab=1.5)
    axis(2, seq(0,40,by=10), cex.axis=1.5)
    mtext ("Bulawayo                       ", 3, cex=1.5)
create.figure(mashonaland,"", simple=T)
    title(ylab='New confirmed cases each week', cex.lab=1.5)
    axis(2, seq(0,40,by=10), cex.axis=1.5)
    mtext ("Mashonaland                           ", 3, cex=1.5)
    axis(1, seq(as.Date("2009/11/15"), as.Date("2011/1/15"), "3 months"), tick=F, label=c("Q4","Q1","Q2","Q3","Q4"), line=-.5, cex.axis=1.5)
    axis(1, as.Date("2010/7/1"), labels="2010", line=.7, tick=F, cex.axis=1.5)
    axis(1, as.Date("2010/1/1"), labels=F, tck=-.12, cex.axis=1.5)
dev.off()

