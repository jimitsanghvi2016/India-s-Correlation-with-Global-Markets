# Load Data
load("dataFinal.Rda")
dataPrice <- dataFinal
# Return of an asset is a complete and scale-free summary of an investment. Therefore, most financial studies involves returns, instead of prices or assets.
# For this project we will take continously compounded return (log return) on daily basis given as: rt = ln(Pt/Pt-1) = ln(Pt) - ln(Pt-1).
# Applying the scale-free log return on each index
dataReturn <- dataFinal
logReturn <- function(x){
  return(diff(log(x), lag=1))
}
# lag = 1 gives the daily return.
# Applying the above function on the dataset.
for (i in 2:17) {
  x <- logReturn(dataReturn[,i])
  x <- c(0,x)
  dataReturn[,i] <- x
}
dataReturn <- dataReturn[-1,]
save(dataReturn, file="dataReturn.Rda")