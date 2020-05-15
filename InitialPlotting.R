# Shows a scatter plot of all the wines above a minimum rating and their price. Colored by country
MinPointPlots <- function(table, minPoint) {
  high = which(table$points>=minPoint)
  HighRatingsA = table[high,]
  highnna = which(!is.na(HighRatingsA$price))
  HighRatings = HighRatingsA[highnna,]
  
  len = length(HighRatings$points)
  average = mean(HighRatings$price)
  Cap = paste("Points greater than or equal to", minPoint, "// Average =", round(average, 2))
  
  p1 = ggplot(HighRatings) + 
    geom_point(aes(x=seq(1:len), y=price, color = HighRatings$country)) +
    geom_hline(yintercept = average) + 
    xlab("") +  ylab("Price per Bottle") + labs(color = "Countries") +
    labs(caption = Cap)
  
  
  return(p1)
}

# Shows the number of bottles each country has at certain price points. Above a minimum rating
FreqAtPriceBarChart <- function(table, minPoint) {
  high = which(table$points>=minPoint)
  HighRatingsA = table[high,]
  highnna = which(!is.na(HighRatingsA$price))
  HighRatings = HighRatingsA[highnna,]
  
  roundPrice = round_any(HighRatings$price, 100)
  freqTable = xtabs(~roundPrice+HighRatings$country)
  table2 = as.data.frame(as.matrix(freqTable))
  
  Cap = paste("For points greater than or equal to", minPoint)
  
  p1 = ggplot(table2, aes(fill=HighRatings.country, y=Freq, x=roundPrice, lege)) + 
    geom_bar(position="stack", stat="identity") +
    guides(fill=guide_legend(title="Countries")) +
    ggtitle(paste("Number of bottles at different price points")) + xlab("Price for a bottle") + 
    ylab("Number of bottles") + labs(caption = Cap)
  
  return(p1)
}

# Shows the average price and a confidence interval for each rating above a minimum rating.
# The first creates the data frame needed for the plotting
AveragesDataFrame <- function(table, minPoint) {
  ratings = seq(minPoint,100)
  priceAvg = c()
  ciUp = c()
  ciDown = c()
  for(i in 1:length(ratings))
  {
    nums = which(table$points == ratings[i])
    ratePricesAll = table[nums,5]
    ratePrices = ratePricesAll[which(!is.na(ratePricesAll))]
    
    priceMean = mean(ratePrices)
    priceSD = sd(ratePrices)
    priceAvg = c(priceAvg, priceMean)
    
    error = qnorm(0.975)*priceSD/sqrt(length(ratePrices))
    ciUp = c(ciUp, priceMean + error)
    ciDown = c(ciDown, priceMean - error)
  }
  
  df = as.data.frame(ratings)
  df = cbind(df, priceAvg, ciUp, ciDown)
  return(df)
}

# This creates the plot for the above data frame
AveragesPlot <- function(dataFrame, minPoint) {
  df = dataFrame
  ratings = seq(minPoint,100)
  p1 = ggplot(df) + 
    geom_point(aes(x=ratings, y=priceAvg)) + 
    geom_line(aes(x=ratings, y=ciUp)) + geom_line(aes(x=ratings, y=ciDown)) +
    geom_ribbon(aes(x=ratings,ymin=ciDown,ymax=ciUp), fill="blue", alpha = .25) +
    scale_x_continuous(breaks=ratings) + xlab("Ratings") + ylab("Price")
  
  return(p1)
}
