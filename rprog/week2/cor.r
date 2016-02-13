

corr <- function(directory, threshold = 0){
      corvec <- NULL
      
      datfile <- NULL
      
      file_list <- list.files(directory, full.names = TRUE)
      
      completedf <- data.frame()
      
      thres <- c()
      
      id <- c(1:length(file_list))
      ##### code from prog assignment 1-2
      for(i in id){
            obsfile <- read.csv(file_list[i])
            obstemp <- subset(obsfile, sulfate != "NA" & nitrate != "NA")
            obs <- c(i, nrow(obstemp))
            completedf <- rbind(completedf, obs)
      }
      names(completedf)[1] <- paste("id")
      names(completedf)[2] <- paste("nobs")
      #####
      
      # test if any file have complete obs greater than or equal to threshold, if not return empty vector thres
      if(threshold < max(completedf$nobs)){
            thres <- completedf$id[completedf$nobs > threshold]
            for(t in thres){
                  datfile <- read.csv(file_list[t])
                  cordat <- subset(datfile, sulfate != "NA" & nitrate != "NA")
                  corvec <- c(corvec, cor(x = cordat$sulfate, y = cordat$nitrate, use = "complete.obs"))
            }
            corvec
      } else { thres }
}


datdir <- "~/coursera/datascience/rprog/week2/specdata/"




threshold = 500



file_list <- list.files(datdir, full.names = TRUE)

completedf <- data.frame()

id <- 1:25

for(i in id){
      obsfile <- read.csv(file_list[i])
      obstemp <- subset(obsfile, sulfate != "NA" & nitrate != "NA")
      obs <- c(i, nrow(obstemp))
      completedf <- rbind(completedf, obs)
}
names(completedf)[1] <- paste("id")
names(completedf)[2] <- paste("nobs")


if(threshold < max(completedf$nobs)){
      thres <- completedf$id[completedf$nobs >= threshold]
      corvec = NULL
      datfile = NULL
      for(t in thres){
            datfile <- read.csv(file_list[t])
            cordat <- subset(datfile, sulfate != "NA" & nitrate != "NA")
            corvec <- rbind(corvec, cor(x = cordat$sulfate, y = cordat$nitrate, use = "complete.obs"))
      }
      corvec
} else { thres }



compl_obs <- subset(datfile)

head(datfile)


