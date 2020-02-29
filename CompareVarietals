# Compares a varietal between two countries
VarietalComparePerCountries <- function(varietal, CountryA, CountryB){
  ratings = seq(80, 100)
  
  # Creates the table for information to be stored
  VarTable = matrix(NA,ncol = 4, nrow = 21,byrow=TRUE)
  colnames(VarTable) = c(paste(CountryA, "Count"), paste(CountryB, "Count"),
                         paste(CountryA, "Average Price"), paste(CountryB, "Average Price"))
  rownames(VarTable) = ratings
  VarTable = as.table(VarTable)
  
  # Creates data for the two different countries to be used
  CountryAData = wine[which(wine$country == CountryA), ]
  CountryAData = CountryAData[which(CountryAData$variety == varietal), ]
  
  CountryBData = wine[which(wine$country == CountryB), ]
  CountryBData = CountryBData[which(CountryBData$variety == varietal), ]
  
  # Gets the count and average price for the varietal for each country at each rating
  for (i in seq(80:100))
  {
    CounARows = which(CountryAData$points == ratings[i])
    CounBRows = which(CountryBData$points == ratings[i])
    
    # Number of times that varietal occurs for each country
    VarTable[i,1] = length(CounARows)
    VarTable[i,2] = length(CounBRows)
    
    CountryADataRating = CountryAData[CounARows,]
    CountryBDataRating = CountryBData[CounBRows,]
    
    NewRowsA = which(!is.na(CountryADataRating$price))
    NewRowsB = which(!is.na(CountryBDataRating$price))
    
    # Average price for each for the varietal for each country
    VarTable[i,3] = signif(mean(CountryADataRating[NewRowsA, ]$price), 4)
    VarTable[i,4] = signif(mean(CountryBDataRating[NewRowsB, ]$price), 4)
  }
  return(VarTable)
}

# Comparing two seperate varietals
VarietalCompare <- function(varietalA, varietalB){
  ratings = seq(80, 100)
  
  # Creates the table for information to be stored
  VarTable = matrix(NA,ncol = 4, nrow = 21,byrow=TRUE)
  colnames(VarTable) = c(paste(varietalA, "Count"), paste(varietalB, "Count"),
                         paste(varietalA, "Average Price"), paste(varietalB, "Average Price"))
  rownames(VarTable) = ratings
  VarTable = as.table(VarTable)
  
  # Creates data for the two different countries to be used
  varietalAData = wine[which(wine$variety == varietalA), ]
  varietalBData = wine[which(wine$variety == varietalB), ]
  
   # Gets the count and average price for each varietal at each rating
  for (i in seq(80:100))
  {
    VarARows = which(varietalAData$points == ratings[i])
    VarBRows = which(varietalBData$points == ratings[i])
    
    # Number of times that each varietal occurs
    VarTable[i,1] = length(VarARows)
    VarTable[i,2] = length(VarBRows)
    
    varietalADataRating = varietalAData[VarARows,]
    varietalBDataRating = varietalBData[VarBRows,]
    
    NewRowsA = which(!is.na(varietalADataRating$price))
    NewRowsB = which(!is.na(varietalBDataRating$price))
    
    # Average price for each for varietal
    VarTable[i,3] = signif(mean(varietalADataRating[NewRowsA, ]$price), 4)
    VarTable[i,4] = signif(mean(varietalBDataRating[NewRowsB, ]$price), 4)
  }
  return(VarTable)
}
