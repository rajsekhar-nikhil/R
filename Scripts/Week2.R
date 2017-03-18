pollutantmean <- function(directory,pollutant,id =1:332){
  filelist <- list.files(path = directory,pattern = ".csv",full.names = T)
  mydata <- numeric()
  for(i in id){
    file_data <- read.csv(filelist[i])
    mydata <- c(mydata,file_data[[pollutant]])
  }
  mean(mydata,na.rm = T)
}

complete <- function(directory,id = 1:332){
  filelist <- list.files(path = directory,pattern = ".csv",full.names = T)
  my_df <- data.frame(id = numeric(), nobs = numeric())
  x<-1
  for (i in id){
    file_data <- read.csv(filelist[i])
    m <- sum(complete.cases(file_data))
    my_df[x,] <- c(i,m)
    x <- x+1
  }
  my_df
}

corr <- function (directory, threshold  = 0){
  complete_data <- complete("specdata")
  id <- c(complete_data$id[complete_data$nobs>threshold])
  filelist <- list.files(path = directory,pattern = ".csv",full.names = T)
  my_vector <- numeric()
  for(i in id){
    file_data <- read.csv(filelist[i])
    my_vector <- c(my_vector,cor(file_data$sulfate,file_data$nitrate,use = "complete.obs"))
  }
  my_vector
}

