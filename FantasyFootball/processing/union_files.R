# After scraping player data, in the folder '/data' there are subdirectories for each poisition, and 
# Further subdirectories for each year. Finally there is a .csv file for each week of that year. 
# This script merges all the files in each position subdirectory so there is one file for each position for all the years.

union_files <- function(){
      # Extract all the position subdirectories
      position_directories <- list.dirs(path = "./data/player_data/", full.names = TRUE, recursive = FALSE)
      
      # Iterate though each of the position subdirectories
      for (i in 1:length(position_directories)){
            # Get the name of the position. This is the lowest level subdirectory. 
            position_name = basename(position_directories[i])
            #Get all the files in the position subdirectory
            position_files <- paste("./data/player_data/", position_name, "/", list.files(path = position_directories[i], recursive = TRUE), sep="")
            # Merge all the files in this subdirectory into one big dataframe
            big_df <- do.call(rbind, lapply(position_files, read.csv))
            # Write this dataframe to a new file
            dir.create(file.path("./processed_data/player_data/"), showWarnings = FALSE)
            dir.create(file.path(paste("./processed_data/player_data/", position_name, "/", sep = "")), showWarnings = FALSE)
            
            write.csv(x = big_df, file = paste("./processed_data/player_data/", position_name,"/", position_name, ".csv", sep=""))
            
      }
}

union_files()