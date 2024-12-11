#answer 3a
data1 <- read.csv("ApplianceShipments.csv")

library(forecast)
Shipments.ts <- ts(data1$Shipments, start = c(1985, 1), end = c(1989, 4), freq = 4)
plot(Shipments.ts, xlab = "Year", ylab = "Quarterly Shipments (Million $)", main = "Quarterly Shipments of household appliances",
     ylim = c(3500, 5000))

#3b

Shipments.2yrs <- window(Shipments.ts, start = c(1985,1), end = c(1986,4))
plot(Shipments.2yrs, xlab = "Year", ylab = "Quarterly Shipments (Million $)", main = "Quarterly Shipments Zoomed In", ylim = c(3500, 5000))

#3c

years <- 1985:1989

Q1 <- data.frame(data1[c(1,5,9,13,17),c("Shipments")])

Q2 <- data.frame(data1[c(2,6,10,14,18),c("Shipments")])

Q3 <- data.frame(data1[c(3,7,11,15,19),c("Shipments")])

Q4 <- data.frame(data1[c(4,8,12,16,20),c("Shipments")])

matplot(years, cbind(Q1, Q2, Q3, Q4), type = "l", lty = 1, 
        col = c("red", "blue", "green", "purple"), xlab = "Year", 
        ylab = "Shipments (Millions of $)", main = "Quarter Wise Shipments")
legend("bottomright", legend = c("Q1", "Q2", "Q3","Q4"), 
       col = c("red", "blue", "green", "purple"), 
       lty = 1)

#3d 
annual.shipments.ts <- aggregate(Shipments.ts, FUN = sum)
plot(annual.shipments.ts, xlab = "Year", ylab = "Total Shipments (Millions of $)",main = "Total Yearly Shipments",
     ylim = c(15000, 19000))

#Answer 4

data2 <- read.csv("RidingMowers.csv")

par(mfcol = c(1,1), xpd=TRUE) # allow legend to be displayed outside of plot area
plot(data2$Lot_Size ~ data2$Income, ylab = "Lot Size (1000 sq ft)", xlab = "Income ($1000s)",
     col = ifelse(data2$Ownership == "Owner", "orange", "green"))
# add legend outside of plotting area
# In legend() use argument inset =  to control the location of the legend relative
# to the plot.
legend("topleft", inset=c(0, -0.1), 
       legend = c("Ownership= Owner", "Ownership = Nonowner"), col = c("orange", "green"), 
       pch = 1, cex = 0.5)

#Answer 5
# 5a

data3 <- read.csv("LaptopSalesJanuary2008.csv")

barplot1 <- aggregate(data3$Retail.Price, by = list(data3$Store.Postcode), FUN = mean)

names(barplot1) <- c("Store", "AvgPrice")
graph5 <- barplot(barplot1$AvgPrice,  names.arg = barplot1$Store, 
        xlab = "Store", ylab = "Avg.Price", ylim=c(0,500),col=rainbow(25), las=2,border = 0, cex.lab=1, cex.axis=1)
text(x = graph5, y = barplot1$AvgPrice, 
     label = round(barplot1$AvgPrice, 2), pos = 3, 
     cex = 0.8, col = "black")
max(barplot1$AvgPrice)
min(barplot1$AvgPrice)

#5b
## side-by-side boxplots

par(mar = c(8, 5, 4, 2)) 
boxplot(data3$Retail.Price ~ data3$Store.Postcode, names.arg = barplot1$Store, 
        xlab = "Store", ylab = "Avg Price",col = rainbow(25),    las = 2, cex.lab = 1, cex.axis = 0.8)

