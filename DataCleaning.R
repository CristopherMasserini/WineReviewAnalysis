# Loads some necessary packages and reads the data set
Wine130 <- function() {
  library(plyr)
  library(dplyr)
  library(readr)
  winemag_data_130k_v2 <- read_csv("Documents/R/wine-reviews/winemag-data-130k-v2.csv")
}

# Looks at the title of the wine and extracts the vintages out of there.
# Creates a new column for the Vintages
WineVintages <- function(data) {
  library(stringr)
  # Problems with vintages under 1990, so ignoring data under that year
  dfVintages = data
  n = length(dfVintages$title)
  vintage = rep(0,n)
  dfVintages = cbind(dfVintages, vintage)
  years = seq(1990, 2020)
  yearsStr = unlist(lapply(years, toString))
  for (i in 1:n)
  {
    descrip = dfVintages$title[i]
    rows = str_detect(descrip, yearsStr)
    if (TRUE %in% rows)
    {
      row = which(rows)
      vint = years[row]
      dfVintages$vintage[i] = vint
    }
    print(i)
  }
  dfVintages = dfVintages[-which(dfVintages$vintage == 0),]
  return(dfVintages)
}
data = Wine130()
wineVint = WineVintages(data)

# Removes the redundent column of row number
# Also removes twitter handle
wine = wineVint[,-c(1,11)]
wine
