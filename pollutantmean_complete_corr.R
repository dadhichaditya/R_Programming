my_final_table <- function(directory, id=1:332){
  final_table <-NULL
  for(i in id){
    x <- str_pad(i, 3, pad='0')
    file_path <- file.path(directory, paste0(x, '.csv'))
    # print(file_path)
    myfile <- read.csv(file_path)
    # View(myfile)
    final_table <- rbind(final_table, myfile)
  }
  final_table
  # View(final_table)
}

pollutantmean <- function(directory, pollutant, id = 1:332){
  final_table <- my_final_table(directory, id)
  mean(final_table[,pollutant], na.rm = TRUE)
}

complete <- function(directory, id = 1:332){
  complete_result <- matrix(ncol = 2)
  colnames(complete_result) <- c('id', 'nobs')
  for(i in id){
    x <- str_pad(i, 3, pad='0')
    file_path <- file.path(directory, paste0(x, '.csv'))
    # print(file_path)
    myfile <- read.csv(file_path)
    complete_result <- rbind(complete_result, c(i, sum(complete.cases(myfile))))
    # View(myfile)
  }
  complete_result[complete.cases(complete_result),]
}

corr <- function(directory, threshold = 0){
  files <- (Sys.glob("specdata//*.csv"));
  
  correlations <- c()
  
  for (file in files) {
    file_data <- read.csv(file, sep = ",");
    complete_cases <- file_data[complete.cases(file_data),];
    if (nrow(complete_cases) > threshold) {
      correlations <- c(correlations, cor(complete_cases$sulfate, complete_cases$nitrate))
    }
  }
  
  return(correlations)
}