freqTableOfVector = function(x){
  freqTable = data.frame(table(x))
  freqTable = freqTable[order(freqTable$Freq, decreasing=T),]
  return(freqTable)
}

freqTableToString = function(x){
  return(paste(x[,1], x[,2], sep=':', collapse='___'))
}

getDataProfile = function(Data, maxNumTopLevelsToShowInFreqTable = 10, minNumLevelsToBeConsideredAsNotFactor = 10, missing_value=NA)
{
  numVars = ncol(Data)
  numRows = nrow(Data)
  summaryColumns = c('DataType', 'CountUnique', 'CountMissingNA', 'PercentMissingNA','Cat_CountLevels', 'Cat_FreqTable',
                     'Num_Mean', 'Num_StD', 'Num_25thPerc', 'Num_Median', 'Num_75thPerc', 'Comment')
  numSummaries =  length(summaryColumns)

  profileSummaryDF = data.frame(matrix(nrow=numVars, ncol=numSummaries+1))

  if(ncol(profileSummaryDF) != (length(summaryColumns)+1)) stop('Error: Number of column names provided does not match the number of summary columns being created.')

  #maxNumTopLevelsToShowInFreqTable = 10 #Set this to a value less than or equal to 0 to show frequence of all levels
  #minNumLevelsToBeConsideredAsNotFactor = 10

  # ------ Creating Empty Vectors to Store Results ------
  dTypeVec = rep(NA,numVars)
  numUniqueVec = rep(NA,numVars)
  numMissingVec = rep(NA,numVars)
  percMissingVec = rep(NA,numVars)
  numLevelsVec = rep(NA,numVars)
  freqLevelsVec = rep(NA,numVars)
  meanVec = rep(NA,numVars)
  stdVec = rep(NA,numVars)
  p25Vec = rep(NA,numVars)
  p75Vec = rep(NA,numVars)
  medianVec = rep(NA,numVars)
  commentVec = rep(NA,numVars)
  # ======================================================

  for(i in 1:numVars)
  {
    varName = names(Data)[i]
    print(paste('varNum = ', i,'  Processing variable: ', varName))
    colData = Data[,i]
    if(all(is.na(colData)))
    {
      commentVec[i] = 'Completely missing'
      next
    }

    dtype = typeof(colData)
    if(dtype=='integer' & is.factor(colData)) dtype='factor'
    dTypeVec[i] = dtype
    numUniqueVec[i] = length(unique(colData))
    if(is.na(missing_value)) numMissingVec[i] = sum(is.na(colData)) else numMissingVec[i] = sum(colData==missing_value)
    percMissingVec[i] = round(100*numMissingVec[i]/numRows,2)

    #Handle Factors
    numLevels = NA
    if(is.factor(Data[,varName]))
    {
      numLevels = length(levels(colData))
      numLevelsVec[i] = numLevels
      freqTable = freqTableOfVector(colData)
      if(maxNumTopLevelsToShowInFreqTable <= 0) maxNumTopLevelsToShowInFreqTable = numRows
      numTopLevelsToSelect = min(numLevels, maxNumTopLevelsToShowInFreqTable)
      freqTable = freqTable[1:numTopLevelsToSelect,]
      freqLevelsVec[i] = freqTableToString(freqTable)
      next
    }

    #Handle Numerics
    if(is.numeric(colData))
    {
      meanVec[i] = mean(colData, na.rm=T)
      stdVec[i] = sd(colData, na.rm=T)
      quant = quantile(colData, c(0.25, 0.5, 0.75), na.rm=T)
      p25Vec[i] = quant[[1]]
      medianVec[i] = quant[[2]]
      p75Vec[i] = quant[[3]]
    }

    #Need to be coded as factor
    if(numUniqueVec[i] < minNumLevelsToBeConsideredAsNotFactor & !is.factor(colData))
    {
      #print(paste('Getting freq table for var: ',varName))
      commentVec[i] = 'Consider recoding as factor'
      freqTable = freqTableOfVector(colData)
      #print(freqTable)
      numTopLevelsToSelect = min(numUniqueVec[i], maxNumTopLevelsToShowInFreqTable)
      freqTable = freqTable[1:numTopLevelsToSelect,]
      freqLevelsVec[i] = freqTableToString(freqTable)
    }

    #TODO:  Handle Date and Other Data Types
  }
  profileSummaryDF = data.frame(names(Data), dTypeVec, numUniqueVec, numMissingVec, percMissingVec, numLevelsVec, freqLevelsVec,
                                meanVec, stdVec, p25Vec, medianVec, p75Vec, commentVec)
  colnames(profileSummaryDF) = c('VarName', summaryColumns)

  return(profileSummaryDF)

}

#summaryDF = getDataProfile(data.frame(iris))
#print(summaryDF)