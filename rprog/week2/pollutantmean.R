

pollutantmean <- function(directory, pollutant, id=1:332){
  file_list <- list.files(directory, full.names = TRUE)
  datfile <- NULL
  for(i in id){
    datfile <- rbind(datfile, read.csv(file_list[i]))
  }
  pol_mean <- mean(datfile[,pollutant], na.rm = TRUE)
  pol_mean
}

directory <- "~/coursera/datascience/rprog/week2/specdata/"

# pollutantmean(directory, "nitrate", 5:50)


