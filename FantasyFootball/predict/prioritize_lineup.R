library(dplyr)
library(grid)
library(gridExtra)
# Prioritize all player lineup. 
# Currently hardcoded for week one. 
prioritize_lineup <- function(week.number){
      
      setwd("~/Desktop/FantasyFootball/")
      
      #Get all the prediction files for that current week. 
      prediction_files <- list.files(paste("predictions/week-", week.number, sep =""), full.names = TRUE)
      
      
      predictions_all <- do.call(what = rbind, lapply(prediction_files, read.csv)) %>% 
            arrange(desc(Predictions))
      
      
      write.csv(predictions_all, "predictions/week-1/all.csv", row.names = FALSE)
      #write pdf file of output
      #pdf(file = "output/week-1/predictions.pdf")
      #grid.newpage()
      #grid.table(predictions_all)
      #dev.off()
      
}
prioritize_lineup(week.number = 1)