#' @title Variable Selection for Linear Regression
#'
#'@description This package incldes function which accepts dataframe,vectors of predictors and
#'targetvariable and gives table of Rsquared,adjRSquared and P-value for combination of predictors
#'which helps in Variable selection of Linear Regression.
#
#'@param symbol
#'
#'@return dataframe with 4 columns of  predictorCombinations,RSquared,adjrsquared and pvalue
#
#'@example VariableSelection(mtcars,c('disp','wt','hp'),'mpg')
#
#'@export  VariableSelection
#

VariableSelection<-function(datafr,predlist,target)
{
  data=datafr
  tar=target
  pred=predlist
  #we create subset of predictors in datatable
  #V1   V2   V3
  #1: disp <NA> <NA>
  #2:   wt <NA> <NA>
  #3:   hp <NA> <NA>
  #4: disp   wt <NA>
  #5: disp   hp <NA>
  #6:   wt   hp <NA>
  #7: disp   wt   hp
  df=data.table::rbindlist(sapply(1:length(pred),function(i) as.data.frame(t(combn(pred,i)))),fill=TRUE)
  dframe=data.frame()
  predictors=vector()
  for (i in 1:dim(df)[1])
  {
    predvals=vector()
    ResultVector=vector()
    for (j in 1:dim(df)[2])
    {
      if(!is.na(df[i][[j]]))
      {
        val=as.character(df[i][[j]])
        predvals=append(predvals,val)
      }
    }
    #to concatenate subset eg.disp+wt+hp

    ab=c(paste(predvals,collapse='+'))
    predictors[i]=ab
    predvals <- c(predvals,tar)

    #create a temp data wit predictors and target

    temp_data <- data[,c(predvals)]
    names(temp_data) <- c(names(temp_data)[1:length(names(temp_data))-1],'y')

    #fit the model

    mod=lm(y~.,data=temp_data)

    #extract required info from summary table

    ResultVector=append(ResultVector,summary(mod)$r.squared)
    ResultVector=append(ResultVector,summary(mod)$adj.r.squared)
    ResultVector=append(ResultVector,summary(mod)$coefficients[2,4])
    dframe=rbind(dframe,ResultVector)
    colnames(dframe)=c('Rsquared','adjrsquared','pvalue')
    modelsum = data.frame(predictors,dframe)
  }
  return(modelsum)
}

