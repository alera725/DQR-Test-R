#My name is NEO
# Function example
getRate <- function(Data){     

  # Calling necessary Libraries
  suppressMessages(library(dplyr))
  suppressMessages(library(summarytools))
  
  
  require(reshape2)
  
  # Import data
  Data <- as.data.frame(Data)
  
  
  # NAs Count 
  na_count <- as.data.frame(sapply(Data, function(y) sum(length(which(is.na(y)))))) # NAs Count
  colnames(na_count) <- c('NAs')
  
  # Type of Variable
  Type <- sapply(Data,class)
  
  # Unique values
  Unique_values <- apply(Data, 2, function(x) length(unique(x)))
  
  
  # Max or Min Categoric variables
  
  Data %>% mutate_if(is.factor, as.character) -> Data
  
  j <- 0
  Maxi <- vector()
  Mini <- vector()
  pal_max <- vector()
  pal_min <- vector()
  
  for (i in 1:length(Type)){
    
    if (Type[i]!='numeric' && Type[i]!='integer'){
      
      j = j+1
      
      uniques <- unique(Data[,i])
      
      Maxi[j] <- max(nchar(uniques))
      pal_max[j] <- uniques[which(nchar(uniques)==Maxi[j])][1]
      
      Mini[j] <- min(nchar(uniques))
      pal_min[j] <- uniques[which(nchar(uniques)==Mini[j])][1]
      
    }else{
      
      j = j
      
    }
  }
  
  
  na_count <- t(na_count)
  df <- data.frame(matrix(NA, nrow = 3, ncol = ncol(na_count)))
  colnames(df) <- colnames(na_count)
  rownames(df) <- c('NAs','Variable_Type','Unique_Values')
  df[1,] <- na_count
  df[2,] <- Type
  df[3,] <- Unique_values
  
  #df<-rbind(Type,na_count,Unique_values)
  df <- as.data.frame(t(df))
  Variable <- rownames(df)
  df$Variable <- Variable
  #df <- as.data.frame(df)
    
  # Creating Report
  #summarytools::descr(Data)
  mydata <- summarytools::descr(Data)
  transpose_mydata <- t(mydata)
  transpose_mydata_df <- as.data.frame(transpose_mydata)
  Variable <- rownames(transpose_mydata_df)
  transpose_mydata_df <- cbind(Variable,transpose_mydata_df)
  
  #df2 <- rbind(transpose_mydata_df,df)
  df <- as.data.frame(lapply(df, unlist))
  rownames(df) <- NULL
  transpose_mydata_df <- as.data.frame(lapply(transpose_mydata_df, unlist))

  df2 <- merge(x=df,y=transpose_mydata_df,by=c("Variable"),all.x=TRUE)
  df3 <- transpose_mydata_df %>% full_join(df, by = c("Variable")) %>% select(Variable, everything())
  
  #df2[is.na(df2)] <- "-"
  
  rownames(df2) <- df2[,1]
  
  return(df2) 
  
}


