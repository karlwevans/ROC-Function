# Function to give ROC
#
# INPUT
# Two vectors:
#     obs: truth 0 for failure, 1 for success
#     pred: probability of success
# OUTPUT
# A tibble with 3 columns: threshold, specificity, and sensitivity.
get_ROC <- function(obs, pred) {
  stopifnot(is.numeric(obs),                                # filter non numeric
            is.numeric(pred),                               # filter non numeric
            length(obs)==length(pred),                      # filter no missing data
            sapply(obs, function(x) any(x==0)||any(x==1)),  # filter obs not boolean
            sapply(pred, function(x) any(x<=1)&&any(x>=0))) # filter preds not probability
  df <- tibble(obs, pred)
  df <- df[order(-pred),]                 # decreasing order for calcs
  Threshold<-unique(df$pred)              # find thresholds
  n=sum(obs==0)                           # total -ve
  p=sum(obs==1)                           # total +ve
  l=length(Threshold)                     # No. thresholds
  TN<-c()                                 #
  TP<-c()                                 # initiate 
  for (i in 1:l){
    df$class<-ifelse(df$pred>=Threshold[i], 1,-1)  # create class: above / below threshold 
    TN[i]=sum(df$obs==0&df$class==-1)       # sum TNs
    Sensitivity=TP/p                        # calc sensitivity
    TP[i]=sum(df$obs==1&df$class==1)        # sum TPs
    Specificity=TN/n                        # calc specificity
  }
  Sensitivity<-append(Sensitivity, 1)       # Manually add 1st sensitivity
  Threshold<-cbind(Threshold,Specificity)   #
  Threshold<-cbind(Threshold,Sensitivity)   #
  ROC<- as.data.frame(Threshold)            # collect into dataframe
  ROC<-aggregate(.~Threshold, ROC, sum)     # aggregate for common threshold
  ROC<-rbind(c(-Inf, 0, 1),ROC)             #
  ROC<-rbind(ROC,c(Inf, 1, 0))              # manually add inf values
  ROC<-tibble(ROC)                          # return a tibble
  return(ROC)
}