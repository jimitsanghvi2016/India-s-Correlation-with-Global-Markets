# Data Preprocessing: Getting the dataset ready for analysis.
setwd('C:/Users/Jimit/Desktop/Github/India-s-Correlation-with-Global-Markets')

df <- read.csv("data.csv", header = T)

attach(df)

summary(df)
# As we see in the summary there are lot of NAs. So the dataset needs cleaning and processing.

n <- ncol(df)
dateCols <- seq(1,n-1,2) # column number for date columns  
indexCols <- seq(2,n,2) # column number for index columns  

# Let us check type of date and indexes
typeof(df$Date)
typeof(df$Date.1)
# So, the date columns for each corresponding index are not in proper format.

# Let us do the formatting for all date columns.
formatDate <- function(x){
  return(as.Date(as.character(x), format = "%m/%d/%Y"))
}

for (i in dateCols) {
  df[,i] <- formatDate(df[,i])
}
typeof(df$Date)
typeof(df$Date.1)
# So, the date columns are now in proper format.

# Let us check type of all index columns
typeof(df$NIFTY.Index)
typeof(df$SPX.Index)
# They are in proper format.

# Now we will merge the indexes based on their corresponding dates.
# First we separate each index with their corresponding date as dataframe.
# So, we separate index and store it in list data1 and do some data cleaning.
data1 <- NULL
j <- 1
for(i in 1:16){
    data1[i] <- list(df[,c(j,j+1)])
    names(data1[[i]])[1] <- "Date"
    data1[[i]] <- na.omit(data1[[i]])
    j <- j+2
}

# Merging using dplyr package
library(dplyr)
library(zoo)
dataFinal <- data1[[1]]
# We perform left join as Nifty.Index is our Response Variable and we want to retain all of the values of Nifty.Index
for(i in 2:16){
  dataFinal <- left_join(dataFinal, data1[[i]])
}
dataFinal <- dataFinal[-c(1,2),]
head(dataFinal)
summary(dataFinal)
# As we can see in the summary there are lot of NAs in the dataFinal.
# This is due to public holidays in various countries during a business day(Mon to Fri).
# But, we just cannot delete the rows with NAs, as this will delete some valuable information.
# Instead, we will replace NAs with the previous day price.

dataFinal <- dataFinal %>% 
  do(na.locf(.))

dataFinal$Date <- as.Date(dataFinal$Date)
for(i in 2:16){
  dataFinal[,i] <- as.double(dataFinal[,i])
}

summary(dataFinal)
# The summary shows no NAs nor any inconsistencies in the data. 
# Finally, we have a clean data for further analysis.

save(dataFinal,file="dataFinal.Rda")
