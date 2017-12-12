library(tidyr)
library(stringr)
library(devtools)
library(dplyr)

# After scraping, the schedules are not in tidy form. 
# The first column is a team name, the rest of the columns are the week number, and the each value is the match up at that week. 
# This function converts each of the files in /data/schedules to tidy format and stores the processed versions in /processed_data/schedules
clean_schedules <- function(){
      
      # Create a dataframe which we add all the schedules to
      df_schedules <- data.frame(matrix(ncol = 5, nrow=0))
      colnames(df_schedules) <- c( "team", "Week", "Opponent", "Home", "year")
      
      # loop through the files in the /data/schedules directory to process each of the schedules one at the time. 
      files <- list.files(path = "data/schedules/")
      for (i in 1:length(files)){
            #store the file name for later
            file_name <- files[i]
            #store just the year (without the .csv extension)
            year <- str_extract(file_name, '20[0-9]{2}(?=\\.csv)')
            
            # Read the data into a dataframe
            df_messy <- read.csv(file = paste("data/schedules/",file_name, sep=""))
            # Perform the following operations on the dataframe: 
            # 
            # 1. Convert to long format.
            # 2. When a team has a 'bye week', they do not play any game. Right now the opponent is hard coded as 'BYE' for these weeks. Replace with NA.
            # 3. If there is an '@' sign in the Opponent value, then team played away. Set this in a new column. 
            # 4. Remove the '@' sign from values in the Opponent column. 
            # 5. After melting in step 1, values are stored in "Week" column as string values in form 'week.2'. Convert to numeric values.
            # 6. Add a column for the year
            df_tidy <-  df_messy %>%
                  gather(Week, Opponent, week.1:week.17) %>%
                  mutate(Opponent = replace(Opponent, Opponent == "BYE", NA)) %>%
                  mutate(Home = ifelse(str_detect(Opponent, "@"), 0, 1)) %>%
                  mutate(Opponent = str_replace(Opponent, "@", "")) %>%
                  mutate(Week = str_replace(Week, "week\\.(?=[0-9]{1,2})", "")) %>%
                  mutate(year = as.integer(year))

            # Merge this schedule with previous schedules
            df_schedules <- rbind(df_schedules, df_tidy)
      }
      # Write tidy schedules into new files
      dir.create(file.path("./processed_data"), showWarnings = FALSE)
      write.csv(df_schedules, "./processed_data/schedules.csv")
}

clean_schedules()

