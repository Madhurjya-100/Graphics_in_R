#histogram
population<- c(200,300,150,100,100,250,250,300,100,300,150,100)
hist(population,
     xlab="population",
     ylab="frequency",
     col="green",
     border="black",
     main= "Population",
     )

# Simple Dotplot
dotchart(population,labels=row.names(population),cex=.7,
         main="Population",
         xlab="Population")

# Add a Normal Curve (Thanks to Peter Dalgaard)
x <- b1$AGE
h<-hist(x, breaks=10, col="red", xlab="AGE",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

# Sort by LC, group and color by AGE
x <- b1[order(b1$AGE),] # sort by mpg
x$LC <- factor(x$LC) # it must be a factor
x$color[x$LC==9] <- "red"
x$color[x$LC==6] <- "blue"
x$color[x$LC==7] <- "darkgreen"
dotchart(x$AGE,labels=row.names(x),cex=.7,groups= x$LC,
         main="AGE by LC",
         xlab="LC", gcolor="black", color=x$color)

# Simple Pie Chart
slices <- c(10, 12,4, 16, 8)
lbls <- c("Assam", "New Delhi", "Rajasthan", "West Bengal", "Tamil Nadu")
pie(slices, labels = lbls, main="Pie Chart of Indian States")

# Pie Chart with Percentages
slices <- c(10, 12, 4, 16, 8)
lbls <- c("Assam", "New Delhi", "Rajasthan", "West Bengal", "Tamil Nadu")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of States")

# 3D Exploded Pie Chart
library(plotrix)
slices <- c(10, 12, 4, 16, 8)
lbls <- c("Assam", "New Delhi", "Rajasthan", "West Bengal", "Tamil Nadu")
pie3D(slices,labels=lbls,explode=0.1,
      main="Pie Chart of states ")

#boxplot
boxplot(AGE~LC,data=b1, main="Boxplot of LC",
         xlab="LC", ylab="AGE")

# boxes colored for ease of interpretation
boxplot(AGE~LC, data=b1, notch=TRUE,
        col=(c("gold","darkgreen")),
        main="LC", xlab="LC")



# Example of a Bagplot
library(aplpack)
attach(b1)
bagplot(AGE,LC, xlab="LC", ylab="AGE",
        main="Bagplot Example")

# Violin Plots
library(vioplot)
x1 <- b1$AGE[b1$LC==9]
x2 <- b1$AGE[b1$LC==7]
x3 <- b1$AGE[b1$LC==6]
vioplot(x1, x2, x3, names=c("9 LC", "7 LC", "6 LC"), ylab= "Frequency",
        col="gold", main="Violin Plots of LC")




