source("packages.R")
source("database.R")
source("IChart.R")

spcInterpretations<- function(values = NULL, MAXVALUE = NULL, MINVALUE= NULL){
  values<- as.numeric(values)
  Mean <- round(mean(values), 2)
  Moving_Range <- abs(diff(values))
  Moving_Range_Mean <- round(mean(Moving_Range),2)
  Sigma <- round(Moving_Range_Mean/1.128, 2) 
  UCL <- round(Mean + (3 * Sigma),2)
  LCL <- round(Mean - (3 * Sigma),2)
  actual_UCL<- unique(MAXVALUE)
  print("actual_UCL")
  print(actual_UCL)
  actual_LCL<- unique(MINVALUE)
  print("actual_LCL")
  print(actual_LCL)
  Median<- median(values)
  CP <- round((as.numeric(UCL) - as.numeric(LCL)) / (6 * Sigma), 2)
  CPU <- round((as.numeric(UCL) - Mean) / (3 * Sigma), 2)
  CPL <- round((Mean - as.numeric(LCL)) / (3 * Sigma), 2)
  CPK <- round(min(CPU, CPL), 2)
  valData <- data.frame(Measures = c(paste('Mean'), paste('Sigma'), paste('Calculated UCL'), paste('Calculated LCL'), 
                                     paste('Actual UCL'), paste('Actual LCL'),
                                     paste('CP'), paste('CPU'), 'CPL', 'Cpk'),
                        Values = c(Mean, Sigma, UCL, LCL, actual_UCL, actual_LCL, CP, CPU, CPL, CPK))
  return(valData)
}

getmode <- function(valx = NULL) {
  uniqv <- unique(valx)
  uniqv[which.max(tabulate(match(valx, uniqv)))]
}

calculations<- function(data= NULL, values = NULL, MAXVALUE = NULL, MINVALUE= NULL){
  values<- as.numeric(values)
  Mean <- round(mean(values), 2)
  sigma_overall <- sd(values)
  temp_sigma = c()
  for (value in values) {
    if(value >= (Mean - sigma_overall) && value <= (Mean + sigma_overall)){
      temp_sigma <- c(temp_sigma, value)
    }
  }
  sigma_percent <- round((sum(length(temp_sigma))/ nrow(data)) * 100,2 )
  Median<- round(median(values), 2)
  Mode<- round(getmode(valx = values), 2)
  spc_data <- data.frame(Measures = c(paste('Mean'), paste('sigma_percent'), paste('Median'), paste('Mode')),
                        Values = c(Mean, sigma_percent, Median, Mode))
  return(spc_data)
}
