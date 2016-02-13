complete <- function(directory, id = 1:332){
      
      file_list <- list.files(directory, full.names = TRUE)
      
      completedf <- data.frame()
      
      for(i in id){
            datfile <- read.csv(file_list[i])
            compl_obs <- subset(datfile, sulfate != "NA" & nitrate != "NA")
            obs <- c(i, nrow(compl_obs))
            completedf <- rbind(completedf, obs)
      }
      names(completedf)[1] <- paste("id")
      names(completedf)[2] <- paste("nobs")
      completedf
}

datdir <- "~/coursera/datascience/rprog/week2/specdata/"

test <- complete(datdir, 5:20)
test