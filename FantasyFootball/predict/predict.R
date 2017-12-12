#Import libraries
suppressWarnings(suppressMessages(library(devtools)))
suppressWarnings(suppressMessages(library(dplyr)))

# Predict next week's fantasy football point output for Tight Ends, and write to .csv file
predict_te <- function(){

	#Change directory in order to reference files appropriately. 
	setwd("~/Desktop/FantasyFootball")

	#Open the tight end data. It is assumed that this has been updated with the previous week's data. 
	df_te <- read.csv("processed_data/player_data/TE/TE.csv")

	#Join tight end table with Opponent table in order to incorportate next week's opponent
	df_schedules <- read.csv("processed_data/schedules.csv")

	df_te <- df_te %>%  #Add next week's opponent
	      left_join(
	            df_schedules %>%
	                  mutate(previous.week = Week - 1)  %>% #Store the previous week number
	                  select(team, year, previous.week, Opponent) %>% # keep only the columns I'll want to join in
	                  rename(Next.Opp = Opponent) %>% # rename the next week's opponent column
	                  group_by(team, year, previous.week) %>%
	                  summarise(Next.Opp = first(Next.Opp)),
	            by = c("Team" = "team", "year" = "year", "week" = "previous.week")
	      )

	#Store a series of shift statistics: 
	# - Next week's fantasy points earned (this will be the response variable in the analysis to follow)
	# - Previous Week's target passes
	# - Previous week's receptions
	# - Previous week's yards
	df_te <- df_te %>%
	    group_by(Player, year) %>% # group by player and year to ensure stats stay within the current season
	    mutate(
            Next.Fantasy.Points = lead(n = 1, Fantasy.Points), #Add the next week's points
            Previous.Targets = lag(n = 1, Targets), # Previous week's targets
            Previous.Rec = lag(n = 1, Rec), # Previous week's receptions
            Previous.Yds = lag(n = 1, Yds) # Previous week's yards
	           
        ) %>% 
	    arrange(Player,year,week) # Sort table to see effect of windowing

	# Add the average fantasy points a player scores in the current seasosn. 
	df_te <- df_te %>%
	      group_by(Player, year) %>%
	      mutate(Avg.Fantasy.Points = mean(Fantasy.Points), # average fantasy points for that year
	             Avg.Targets = mean(Targets), # Average targets for that year
	             Avg.Rec = mean(Rec), # Average receptions for that year
	             Avg.Yds = mean(Yds) # Average yards for that year
	      )  

	# Add the average points, targets, receptions etc that the player has scored in the previous season
	df_te <-  df_te %>%
	      left_join(
	            df_te %>% 
	                  mutate(next.year = year +1) %>% # Add column for next year
	                  group_by(Player, year) %>% 
	                  mutate(
	                              Previous.Avg.Fantasy.Points = first(Avg.Fantasy.Points), #Add average fantasy points
	                              Previous.Avg.Targets = first(Avg.Targets), # Add average targets
	                              Previous.Avg.Rec = first(Avg.Rec), # Add average receptions
	                              Previous.Avg.Yds = first(Avg.Yds)
	                        ) %>%
	                  filter(row_number() == 1) %>%  #Only keep one row per player/year group to avoid duplication upon join
	                  select(Player, next.year, Previous.Avg.Fantasy.Points, Previous.Avg.Targets, Previous.Avg.Rec, Previous.Avg.Yds)
	                  , 
	            by = c("Player" = "Player", "year" = "next.year")
	            ) %>%
	            ungroup()

	# Next, compute a series of cumulative average statistics. 
	# These will be the average number of fantasy ponts, targets, receptions, and yards for a player within that season up to (and including) a certain week.
	df_te <- df_te %>%
      group_by(Player, year) %>%  #Look at players one season at a time
      mutate(
            Cum.Avg.Fantasy.Points = cummean(Fantasy.Points), #cumualtive average number of fantasy points per game
            Cum.Avg.Targets = cummean(Targets), #cumulative average number of targets per game
            Cum.Avg.Rec = cummean(Rec), #cumulative average number of receptions per game
            Cum.Avg.Yds = cummean(Yds), #cumulative average number of yards per game
            Cum.Avg.TD = cummean(TD) #cumalative average number of TDs
      ) %>% 
      ungroup()

 	# Now, I'd like to add some features that show how the opposing team does against this player. 
	# First, I'll add the average number of fantasy points let up by the opposing team in 2016 and 2015 to players of the TE position.
	df_te <- df_te %>%
      left_join( #add the average points let up by opponent in previous year
            df_te %>%
                  group_by(Opp, year) %>% 
                  summarise(Avg.Points.Allowed.Last.Year = mean(Fantasy.Points)) %>% #average fantasy points allowed by opponent last year 
                  ungroup() %>%
                  mutate(next.year = year + 1) #will be used to join with year column to get last year's avg points allowed
      ,
      by = c("Next.Opp" = "Opp", "year" = "next.year")
      ) %>%
      left_join( #add the average points let up by next week's opponent two years ago
            df_te %>%
                  group_by(Opp, year) %>% 
                  summarise(Avg.Points.Allowed.Last.Last.Year = mean(Fantasy.Points)) %>% #average fantasy points allowed by opponent last year 
                  ungroup() %>%
                  mutate(next.next.year = year + 2) #will be used to join with year column to get last year's avg points allowed
      ,
      by = c("Next.Opp" = "Opp", "year" = "next.next.year")
      )


     # When Next.Opp is NA, it means that there is no team to be played next week because the player will be on a bye week. 
	# This will cause the columns 'Avg.Points.Allowed.Last.Year' and 'Avg.Points.Allowed.Last.Last.Year' to be NA as well. 
	# Since I want to use these columns for my model, I'll fill the with something reasonable - the average amount of points allowed. 
	df_te[is.na(df_te$Avg.Points.Allowed.Last.Year), "Avg.Points.Allowed.Last.Year"] <- mean(df_te$Avg.Points.Allowed.Last.Year, na.rm = TRUE)
	df_te[is.na(df_te$Avg.Points.Allowed.Last.Last.Year), "Avg.Points.Allowed.Last.Last.Year"] <- mean(df_te$Avg.Points.Allowed.Last.Last.Year, na.rm = TRUE)

    # I use many lagged statistics from previous years.
    # This has two problems.
    # First, the state of Football has changed in recent times - offenses are more productive than they used to be. It may not make sense to a model to data from too far back.
    # Second, many of the features I extract rely on looking back to performances of previous years. This results in NA's if there are no years to look back to in the datast. 

	# As such, I will remove the two earliest years in the dataset - 2011 and 2012. 
	df_te <- df_te %>% filter(year > 2012)

	# As I will be incorporating previous weeks' performances into my model, I don't want to take into account breakaway weeks when fitting my regresssion parameters. 
	# So as a simple heuristic, I'll ignore weeks that uncharactaristic of the past 3 years of data for each player. 

	# To determine if a performance is "uncharactaristic", I'll use the boxplot (Tukey) outlier method. 
	test <- 
	df_te %>% #Join to get the upper and lower Tukey thresholds for a player's fantasy points, based on data since 2014.
	      left_join(
	                df_te %>%
	                      group_by(Player) %>% #finding average for each player
	                      summarize(
	                            Q25.Fantasy.Points = quantile(Fantasy.Points, probs = .25), #25th quantile of fantasy points earned in games since 2014
	                            Q75.Fantasy.Points = quantile(Fantasy.Points, probs = .75) #75th quantile of fantasy points earned in games since 2014
	                      ) %>%
	                      mutate( IQR.Fantasy.Points = Q75.Fantasy.Points - Q25.Fantasy.Points) %>% #Get the IQR of fantasy points earned since 2014
	                      mutate( 
	                            Lower.Limit.Fantasy.Points = Q25.Fantasy.Points - 3*IQR.Fantasy.Points, 
	                            Upper.Limit.Fantasy.Points = Q75.Fantasy.Points + 3*IQR.Fantasy.Points #calculate lower and upper tukey limits
	                      ) %>%
	                      select(Player, Lower.Limit.Fantasy.Points, Upper.Limit.Fantasy.Points) #keep only columns I want to bring over in join
	                , 
	                by = "Player"
	      ) %>%
	      filter( Fantasy.Points >= Lower.Limit.Fantasy.Points & Fantasy.Points <= Upper.Limit.Fantasy.Points)

	# Fill NA values
	test[is.na(test)] <- 0

	# Fit the model
	model <- lm(data = test, Next.Fantasy.Points ~ Cum.Avg.Rec + Cum.Avg.Targets + Cum.Avg.Yds + Cum.Avg.TD + Previous.Avg.Targets + Avg.Points.Allowed.Last.Year ,  na.action = na.exclude )	# Store predictions
	test$Predictions = predict(model)


	# Prioritize tight ends for line up
	predictions <- test %>% 
            mutate(max_year = max(year)) %>% 
            filter(year == max_year) %>% #keep only most recent year's predictions
            group_by(Player) %>% #group by player to get the most recent week the player has played
            mutate(max_week = max(week)) %>% 
            filter(week == max_week) %>% #Only keep each player's last week predictions
            select(Player, Team, Pos, year, week, Previous.Avg.Fantasy.Points, Predictions) %>% # Keep only columns I'm interested in writing to file
            arrange(desc(Predictions)) %>%
            ungroup() %>%
            top_n(20) 

    # Store the week (for the file name)
    year <- predictions$year[1]
    week <- predictions$week[1] + 1
    # If the year is 2016, it means it is the draft prediction and there is no data for 2017 yet. In this case set the week to one. 
    if(year < 2017) {
    	week <- 1
    }


    # Write to file
    dir.create(file.path("./predictions"), showWarnings = FALSE)
    dir.create(file.path(paste("./predictions/week-", week, "/", sep = "")), showWarnings = FALSE)
    write.csv(x = predictions, file = paste("./predictions/week-", week,"/", "TE.csv", sep=""))

}

# Predict which quarterback to add to lineup
predict_qb <- function(){

	#Change directory in order to reference files appropriately. 
	setwd("~/Desktop/FantasyFootball")

	#Open Quarter Back Data
	df_qb <- read.csv("processed_data/player_data/QB/QB.csv")

	# Join Table with schedle table to get next week's opponent
	df_schedules <- read.csv("processed_data/schedules.csv")

	df_qb <- df_qb %>%  #Add next week's opponent
	      left_join(
	            df_schedules %>%
	                  mutate(previous.week = Week - 1)  %>% #Store the previous week number
	                  select(team, year, previous.week, Opponent) %>% # keep only the columns I'll want to join in
	                  rename(Next.Opp = Opponent) %>% # rename the next week's opponent column
	                  group_by(team, year, previous.week) %>%
	                  summarise(Next.Opp = first(Next.Opp)),
	            by = c("Team" = "team", "year" = "year", "week" = "previous.week")
	    )

	# Compute a series of shift statstics from the previous week 
	df_qb <- df_qb %>%
      group_by(Player, year) %>% # group by player and year to ensure stats stay within the current season
      mutate(
            Next.Fantasy.Points = lead(n = 1, Fantasy.Points), #Add the next week's points
            Previous.Att = lag(n = 1, Att), #Add previous week's attempts
            Previous.Comp = lag(n = 1, Comp), #Add previous week's completions
            Previous.Yds = lag(n = 1, Yds), # Previous week's yards
            Previous.TD = lag(n = 1, TD)
           
            ) %>% 
      arrange(Player,year,week) # Sort table to see effect of windowing


    #add the average fanatasy points, attempts, yards, Yards/Attempt, TD and Interceptions from previous year
    # Add the average fantasy points in that season.
	df_qb <- df_qb %>%
	      group_by(Player, year) %>%
	      mutate(Avg.Fantasy.Points = mean(Fantasy.Points), # average fantasy points for that year
	             Avg.Att = mean(Att), # Average targets for that year
	             Avg.Yds = mean(Yds), # Average yards for that year
	             Avg.Yds.Att = mean(Yds.Att), #Average yards/attempt for that year
	             Avg.TD = mean(TD), #Average TD's per game for that year
	             Avg.Int = mean(Int) #Average number of interceptions per game for that year
	      )  

	# Add columns for these averages from the previous season.
	df_qb <-  df_qb %>%
	      left_join(
	            df_qb %>% 
	                  mutate(next.year = year +1) %>% # Add column for next year
	                  group_by(Player, year) %>% 
	                  mutate(
	                              Previous.Avg.Fantasy.Points = first(Avg.Fantasy.Points), #Add average fantasy points
	                              Previous.Avg.Att = first(Avg.Att), # Add average attempts/game
	                              Previous.Avg.Yds = first(Avg.Yds), # Add average yards/game
	                              Previous.Avg.Yds.Att = first(Avg.Yds.Att), #Add average yards/attempt/game
	                              Previous.Avg.TD = first(Avg.TD), #Add average TDs/game
	                              Previous.Avg.Int = first(Avg.Int) #Add average interceptions per game
	                        ) %>%
	                  filter(row_number() == 1) %>%  #Only keep one row per player/year group to avoid duplication upon join
	                  select(Player, next.year, Previous.Avg.Fantasy.Points, Previous.Avg.Att, Previous.Avg.Yds, Previous.Avg.Yds.Att, Previous.Avg.TD, Previous.Avg.Int)
	                  , 
	            by = c("Player" = "Player", "year" = "next.year")
	            ) %>%
	            ungroup()


	# Next, compute a series of cumulative average statistics.
	# These will be the average number of fantasy ponts, attempts, completions, yards, yards/attempt, TDs, and interceptions
	# for a player within that season up to (and including) a certain week. 
	df_qb <- df_qb %>%
      group_by(Player, year) %>%  #Look at players one season at a time
      mutate(
            Cum.Avg.Fantasy.Points = cummean(Fantasy.Points), #cumualtive average number of fantasy points per game
            Cum.Avg.Att = cummean(Att), #cumulative average number of attempts per game
            Cum.Avg.Yds = cummean(Yds), #cumulative average number of yards per game
            Cum.Avg.Yds.Att = cummean(Yds.Att), #cumulative average number of yards/attempt per game
            Cum.Avg.TD = cummean(TD), #cumalative average number of TDs
            Cum.Avg.Int = cummean(Int)
      ) %>% 
      ungroup()

    # Now, see if the player has increased in performance from previous week to current week. 
    # This may suggest that player is on short term "hot streak."
    df_qb <- df_qb %>%
      mutate(
            Att.Increase = Att - Previous.Att, #See the increase/decrease in targets from previous week
            Yds.Increase = Yds - Previous.Yds, 
            Comp.Increase = Comp - Previous.Comp
      )

 	# Now, I'd like to add some features that show how the opposing team does against this player. 
	# First, I'll add the average number of fantasy points let up by the opponent to players of the position in quetion in the previous two seasons. 
	df_qb <- df_qb %>%
      left_join( #add the average points let up by opponent in previous year
            df_qb %>%
                  group_by(Opp, year) %>% 
                  summarise(Avg.Points.Allowed.Last.Year = mean(Fantasy.Points)) %>% #average fantasy points allowed by opponent last year 
                  ungroup() %>%
                  mutate(next.year = year + 1) #will be used to join with year column to get last year's avg points allowed
      ,
      by = c("Next.Opp" = "Opp", "year" = "next.year")
      ) %>%
      left_join( #add the average points let up by next week's opponent two years ago
            df_qb %>%
                  group_by(Opp, year) %>% 
                  summarise(Avg.Points.Allowed.Last.Last.Year = mean(Fantasy.Points)) %>% #average fantasy points allowed by opponent last year 
                  ungroup() %>%
                  mutate(next.next.year = year + 2) #will be used to join with year column to get last year's avg points allowed
      ,
      by = c("Next.Opp" = "Opp", "year" = "next.next.year")
      )


	# When Next.Opp is NA, it means that there is no team to be played next week because the player will be on a bye week. 
	# This will cause the columns 'Avg.Points.Allowed.Last.Year' and 'Avg.Points.Allowed.Last.Last.Year' to be NA as well. 
	# Since I want to use these columns for my model, I'll fill the with something reasonable - the average amount of points allowed. 
	df_qb[is.na(df_qb$Avg.Points.Allowed.Last.Year), "Avg.Points.Allowed.Last.Year"] <- mean(df_qb$Avg.Points.Allowed.Last.Year, na.rm = TRUE)
	df_qb[is.na(df_qb$Avg.Points.Allowed.Last.Last.Year), "Avg.Points.Allowed.Last.Last.Year"] <- mean(df_qb$Avg.Points.Allowed.Last.Last.Year, na.rm = TRUE)

	# I use many lagged statistics from previous years.
	# This has two problems. First, the state of Football has changed 
	# in recent times - offenses are more productive than they used to be. 
	# It may not make sense to a model to data from too far back. 
	# Second, many of the features I extract rely on looking back to performances of previous years. 
	# This results in NA's if there are no years to look back to in the datast. 

	# As such, I will remove the two earliest years in the dataset - 2011 and 2012. 
	df_qb <- df_qb %>% filter(year > 2012)

	# As I will be incorporating previous weeks' performances into my model, I don't want to take into account breakaway weeks when fitting my regresssion parameters. 
	# So as a simple heuristic, I'll ignore weeks that uncharactaristic of the past 3 years of data for each player. 

	# To determine if a performance is "uncharactaristic", I'll use the boxplot (Tukey) outlier method. 
	test <- 
	df_qb %>% #Join to get the upper and lower Tukey thresholds for a player's fantasy points, based on data since 2014.
	      left_join(
	                df_qb %>%
	                      group_by(Player) %>% #finding average for each player
	                      summarize(
	                            Q25.Fantasy.Points = quantile(Fantasy.Points, probs = .25), #25th quantile of fantasy points earned in games since 2014
	                            Q75.Fantasy.Points = quantile(Fantasy.Points, probs = .75) #75th quantile of fantasy points earned in games since 2014
	                      ) %>%
	                      mutate( IQR.Fantasy.Points = Q75.Fantasy.Points - Q25.Fantasy.Points) %>% #Get the IQR of fantasy points earned since 2014
	                      mutate( 
	                            Lower.Limit.Fantasy.Points = Q25.Fantasy.Points - 3*IQR.Fantasy.Points, 
	                            Upper.Limit.Fantasy.Points = Q75.Fantasy.Points + 3*IQR.Fantasy.Points #calculate lower and upper tukey limits
	                      ) %>%
	                      select(Player, Lower.Limit.Fantasy.Points, Upper.Limit.Fantasy.Points) #keep only columns I want to bring over in join
	                , 
	                by = "Player"
	      ) %>%
	      filter( Fantasy.Points >= Lower.Limit.Fantasy.Points & Fantasy.Points <= Upper.Limit.Fantasy.Points)

	# Fill NA values in 
	test[is.na(test)] <- 0

	# Create a dummy model
	model <- lm(data = test, Next.Fantasy.Points ~ Fantasy.Points  + Previous.Avg.Yds + Previous.Avg.Int +   Cum.Avg.Att + Cum.Avg.TD +  Cum.Avg.Yds +  Avg.Points.Allowed.Last.Year,
	  na.action = na.exclude )

	# Add predictions
	test$Predictions = predict(model)

	# Prioritize players for lineup 
	predictions <- test %>% 
		mutate(max_year = max(year)) %>% 
		filter(year == max_year) %>% #keep only most recent year's predictions
		group_by(Player) %>% #group by player to get the most recent week the player has played
		mutate(max_week = max(week)) %>% 
		filter(week == max_week) %>% #Only keep each player's last week predictions
		select (Player, Team, Pos, year, week, Previous.Avg.Fantasy.Points, Predictions) %>% #Information I'd like to see when chosing lineup
		arrange(desc(Predictions)) %>%
		ungroup() %>%
		top_n(20)


	# Store the week (for the file name)
    year <- predictions$year[1]
    week <- predictions$week[1] + 1
    # If the year is 2016, it means it is the draft prediction and there is no data for 2017 yet. In this case set the week to one. 
    if(year < 2017) {
    	week <- 1
    }


    # Write to file
    dir.create(file.path("./predictions"), showWarnings = FALSE)
    dir.create(file.path(paste("./predictions/week-", week, "/", sep = "")), showWarnings = FALSE)
    write.csv(x = predictions, file = paste("./predictions/week-", week,"/", "QB.csv", sep=""))

}

# Predict which running back to add to lineup
predict_rb <- function(){

	#Change directory in order to reference files appropriately. 
	setwd("~/Desktop/FantasyFootball")

	#Open Running Back Data
	df_rb <- read.csv("processed_data/player_data/RB/RB.csv")

	# Join Table with schedle table to get next week's opponent
	df_schedules <- read.csv("processed_data/schedules.csv")

	# Join with schedules to get next week's opponent
	df_rb <- df_rb %>%  #Add next week's opponent
	      left_join(
	            df_schedules %>%
	                  mutate(previous.week = Week - 1)  %>% #Store the previous week number
	                  select(team, year, previous.week, Opponent) %>% # keep only the columns I'll want to join in
	                  rename(Next.Opp = Opponent) %>% # rename the next week's opponent column
	                  group_by(team, year, previous.week) %>%
	                  summarise(Next.Opp = first(Next.Opp)),
	            by = c("Team" = "team", "year" = "year", "week" = "previous.week")
	      )


	# Store a series of shift statistics: 
	# - Next week's fantasy points earned (this will be the response variable in the analysis to follow)
	# - Previous week's attempts
	# - Previous week's yards
	# - Previous week's yards/attempt
	# - Previous week's TDs
	# - Previous week's Fumbles
	# - Previous week's Receptions
	df_rb <- df_rb %>%
      group_by(Player, year) %>% # group by player and year to ensure stats stay within the current season
      mutate(
            Next.Fantasy.Points = lead(n = 1, Fantasy.Points), #Add the next week's points
            Previous.Att = lag(n = 1, Att), #Add previous week's attempts
            Previous.Yds = lag(n = 1, Yds), #Add previous week's Yards
            Previous.Yds.Att = lag(n = 1, Yds.Att), #Add previous week's Yds/Attempt
            Previous.TD = lag(n = 1, TD), # Previous week's Touchdowns
            Previous.Fum = lag(n = 1, Fum), #Get the previous week's fumbles
            Prevoius.Rec = lag(n = 1, Rec) # Previous week's receptions
            ) %>%
      	ungroup()

	# Add the average fantasy points in that season.
	df_rb <- df_rb %>%
	      group_by(Player, year) %>%
	      mutate(Avg.Fantasy.Points = mean(Fantasy.Points), # average fantasy points for that year
	             Avg.Att = mean(Att), # Average targets for that year
	             Avg.Yds = mean(Yds), # Average yards for that year
	             Avg.Yds.Att = mean(Yds.Att), #Average yards/attempt for that year
	             Avg.TD = mean(TD), #Average TD's per game for that year
	             Avg.Fum = mean(Fum), #Average number of fumbles per game for that year
	             Avg.Rec = mean(Rec) #Average number of receptions per game for that year
	      )  

	# Add columns for these averages from the previous season.
	df_rb <-  df_rb %>%
	      left_join(
	            df_rb %>% 
	                  mutate(next.year = year +1) %>% # Add column for next year
	                  group_by(Player, year) %>% 
	                  mutate(
	                              Previous.Avg.Fantasy.Points = first(Avg.Fantasy.Points), #Add average fantasy points
	                              Previous.Avg.Att = first(Avg.Att), # Add average attempts/game
	                              Previous.Avg.Yds = first(Avg.Yds), # Add average yards/game
	                              Previous.Avg.Yds.Att = first(Avg.Yds.Att), #Add average yards/attempt/game
	                              Previous.Avg.TD = first(Avg.TD), #Add average TDs/game
	                              Previous.Avg.Fum = first(Avg.Fum), #Add average interceptions per game
	                              Previous.Avg.Rec = first(Avg.Rec) #Add average receptions per game
	                        ) %>%
	                  filter(row_number() == 1) %>%  #Only keep one row per player/year group to avoid duplication upon join
	                  select(Player, next.year, Previous.Avg.Fantasy.Points, Previous.Avg.Att, Previous.Avg.Yds, Previous.Avg.Yds.Att, Previous.Avg.TD, Previous.Avg.Fum, Previous.Avg.Rec)
	                  , 
	            by = c("Player" = "Player", "year" = "next.year")
	            ) %>%
	            ungroup()

	# Next, compute a series of cumulative average statistics.
	# These will be the average number of fantasy ponts, attempts, yards, yards/attempt, TDs, fumbles and receptions
	# for a player within that season up to (and including) a certain week. 
	df_rb <- df_rb %>%
      group_by(Player, year) %>%  #Look at players one season at a time
      mutate(
            Cum.Avg.Fantasy.Points = cummean(Fantasy.Points), #cumualtive average number of fantasy points per game
            Cum.Avg.Att = cummean(Att), #cumulative average number of attempts per game
            Cum.Avg.Yds = cummean(Yds), #cumulative average number of yards per game
            Cum.Avg.Yds.Att = cummean(Yds.Att), #cumulative average number of yards/attempt per game
            Cum.Avg.TD = cummean(TD), #cumalative average number of TDs
            Cum.Avg.Fum = cummean(Fum), #cumulative average number of fumbles
            Cum.Avg.Rec = cummean(Rec) #cumulative average number of receptions
      ) %>% 
      ungroup()

	# I use many lagged statistics from previous years. 
	# This has two problems. First, the state of Football has changed in recent times - 
	# offenses are more productive than they used to be. It may not make sense to a model to data from too far back. 
	# Second, many of the features I extract rely on looking back to performances of previous years. 
	# This results in NA's if there are no years to look back to in the datast. 

	# As such, I will remove the two earliest years in the dataset - 2011 and 2012. 
	df_rb <- df_rb %>% filter(year > 2012)

	# As I will be incorporating previous weeks' performances into my model, I don't want to take into account breakaway weeks when fitting my regresssion parameters.
	# So as a simple heuristic, I'll ignore weeks that uncharactaristic of the past 3 years of data for each player. 

	# To determine if a performance is "uncharactaristic", I'll use the boxplot (Tukey) outlier method. 
	test <- 
	df_rb %>% #Join to get the upper and lower Tukey thresholds for a player's fantasy points, based on data since 2014.
	      left_join(
	                df_rb %>%
	                      group_by(Player) %>% #finding average for each player
	                      summarize(
	                            Q25.Fantasy.Points = quantile(Fantasy.Points, probs = .25), #25th quantile of fantasy points earned in games since 2014
	                            Q75.Fantasy.Points = quantile(Fantasy.Points, probs = .75) #75th quantile of fantasy points earned in games since 2014
	                      ) %>%
	                      mutate( IQR.Fantasy.Points = Q75.Fantasy.Points - Q25.Fantasy.Points) %>% #Get the IQR of fantasy points earned since 2014
	                      mutate( 
	                            Lower.Limit.Fantasy.Points = Q25.Fantasy.Points - 3*IQR.Fantasy.Points, 
	                            Upper.Limit.Fantasy.Points = Q75.Fantasy.Points + 3*IQR.Fantasy.Points #calculate lower and upper tukey limits
	                      ) %>%
	                      select(Player, Lower.Limit.Fantasy.Points, Upper.Limit.Fantasy.Points) #keep only columns I want to bring over in join
	                , 
	                by = "Player"
	      ) %>%
	      filter( Fantasy.Points >= Lower.Limit.Fantasy.Points & Fantasy.Points <= Upper.Limit.Fantasy.Points)

	#Fill NA values in test
	test[is.na(test)] <- 0

	#Fit a dummy model
	model <- lm(data = test, Next.Fantasy.Points ~ Fantasy.Points + Previous.Avg.Yds + Cum.Avg.Yds + Cum.Avg.Rec + Cum.Avg.Yds.Att ,  na.action = na.exclude )

	#Add predictions
	test$Predictions = predict(model)

	#Prioritize player lineup
	predictions <- test %>% 
            mutate(max_year = max(year)) %>% 
            filter(year == max_year) %>% #keep only most recent year's predictions
            group_by(Player) %>% #group by player to get the most recent week the player has played
            mutate(max_week = max(week)) %>% 
            filter(week == max_week) %>% #Only keep each player's last week predictions
            select (Player, Team, Pos, year, week, Previous.Avg.Fantasy.Points, Predictions) %>% #Information I'd like to see when chosing lineup
            arrange(desc(Predictions)) %>%
            ungroup() %>%
            top_n(20)


	# Store the week (for the file name)
    year <- predictions$year[1]
    week <- predictions$week[1] + 1
    # If the year is 2016, it means it is the draft prediction and there is no data for 2017 yet. In this case set the week to one. 
    if(year < 2017) {
    	week <- 1
    }


    # Write to file
    dir.create(file.path("./predictions"), showWarnings = FALSE)
    dir.create(file.path(paste("./predictions/week-", week, "/", sep = "")), showWarnings = FALSE)
    write.csv(x = predictions, file = paste("./predictions/week-", week,"/", "RB.csv", sep=""))

}

# Predict which WR to join
predict_wr <- function(){

	#Change directory in order to reference files appropriately. 
	setwd("~/Desktop/FantasyFootball")

	#Open Wide reciever data
	df_wr <- read.csv("processed_data/player_data/WR/WR.csv")

	# Join Table with schedle table to get next week's opponent
	df_schedules <- read.csv("processed_data/schedules.csv")

	#Join Table with schedule table to get next week's oponent. 

	df_wr <- df_wr %>%  #Add next week's opponent
      left_join(
            df_schedules %>%
                  mutate(previous.week = Week - 1)  %>% #Store the previous week number
                  select(team, year, previous.week, Opponent) %>% # keep only the columns I'll want to join in
                  rename(Next.Opp = Opponent) %>% # rename the next week's opponent column
                  group_by(team, year, previous.week) %>%
                  summarise(Next.Opp = first(Next.Opp)),
            by = c("Team" = "team", "year" = "year", "week" = "previous.week")
      )

	# Store a series of shift statistics: 
	# - Next week's fantasy points earned (this will be the response variable in the analysis to follow)
	# - Previous week's targets
	# - Previous week's yards
	# - Previous week's yards/targets
	# - Previous week's TDs
	# - Previous week's Fumbles
	# - Previous week's Receptions

	df_wr <- df_wr %>%
      group_by(Player, year) %>% # group by player and year to ensure stats stay within the current season
      mutate(
            Next.Fantasy.Points = lead(n = 1, Fantasy.Points), #Add the next week's points
            Previous.Targets = lag(n = 1, Targets), #Add previous week's targets
            Previous.Yds = lag(n = 1, Yds), #Add previous week's Yards
            Previous.Yds.Targets = lag(n = 1, Yds.Target), #Add previous week's Yds/Targets
            Previous.TD = lag(n = 1, TD), # Previous week's Touchdowns
            Prevoius.Rec = lag(n = 1, Rec) # Previous week's receptions
            ) %>% 
            ungroup() 


    # Add the average fantasy points in that season.
	df_wr <- df_wr %>%
	      group_by(Player, year) %>%
	      mutate(Avg.Fantasy.Points = mean(Fantasy.Points), # average fantasy points for that year
	             Avg.Targets = mean(Targets), # Average targets for that year
	             Avg.Yds = mean(Yds), # Average yards for that year
	             Avg.Yds.Target = mean(Yds.Target), #Average yards/attempt for that year
	             Avg.TD = mean(TD), #Average TD's per game for that year
	             Avg.Rec = mean(Rec) #Average number of receptions per game for that year
	      )  %>% ungroup()

	# Add columns for these averages from the previous season.
	df_wr <-  df_wr %>%
	      left_join(
	            df_wr %>% 
	                  mutate(next.year = year +1) %>% # Add column for next year
	                  group_by(Player, year) %>% 
	                  mutate(
	                              Previous.Avg.Fantasy.Points = first(Avg.Fantasy.Points), #Add average fantasy points
	                              Previous.Avg.Targets = first(Avg.Targets), # Add average attempts/game
	                              Previous.Avg.Yds = first(Avg.Yds), # Add average yards/game
	                              Previous.Avg.Yds.Target = first(Avg.Yds.Target), #Add average yards/attempt/game
	                              Previous.Avg.TD = first(Avg.TD), #Add average TDs/game
	                              Previous.Avg.Rec = first(Avg.Rec) #Add average receptions per game
	                        ) %>%
	                  filter(row_number() == 1) %>%  #Only keep one row per player/year group to avoid duplication upon join
	                  select(Player, next.year, Previous.Avg.Fantasy.Points, Previous.Avg.Targets, Previous.Avg.Yds, Previous.Avg.Yds.Target, Previous.Avg.TD, Previous.Avg.Rec)
	                  , 
	            by = c("Player" = "Player", "year" = "next.year")
	            ) %>%
	            ungroup()

	# Next, compute a series of cumulative average statistics. 
	# These will be the average number of fantasy ponts, attempts, yards, yards/attempt, TDs, fumbles and receptions 
	# for a player within that season up to (and including) a certain week. 
	df_wr <- df_wr %>%
      group_by(Player, year) %>%  #Look at players one season at a time
      mutate(
            Cum.Avg.Fantasy.Points = cummean(Fantasy.Points), #cumualtive average number of fantasy points per game
            Cum.Avg.Targets = cummean(Targets), #cumulative average number of attempts per game
            Cum.Avg.Yds = cummean(Yds), #cumulative average number of yards per game
            Cum.Avg.Yds.Target = cummean(Yds.Target), #cumulative average number of yards/attempt per game
            Cum.Avg.TD = cummean(TD), #cumalative average number of TDs
            Cum.Avg.Rec = cummean(Rec) #cumulative average number of receptions
      ) %>% 
      ungroup()

	# Now, I'd like to add some features that show how the opposing team does against this player. 
	# First, I'll add the average number of fantasy points let up by the opponent to players of the position in quetion in the previous two seasons. 
	df_wr <- df_wr %>%
      left_join( #add the average points let up by opponent in previous year
            df_wr %>%
                  group_by(Opp, year) %>% 
                  summarise(Avg.Points.Allowed.Last.Year = mean(Fantasy.Points)) %>% #average fantasy points allowed by opponent last year 
                  ungroup() %>%
                  mutate(next.year = year + 1) #will be used to join with year column to get last year's avg points allowed
      ,
      by = c("Next.Opp" = "Opp", "year" = "next.year")
      ) %>%
      left_join( #add the average points let up by next week's opponent two years ago
            df_wr %>%
                  group_by(Opp, year) %>% 
                  summarise(Avg.Points.Allowed.Last.Last.Year = mean(Fantasy.Points)) %>% #average fantasy points allowed by opponent last year 
                  ungroup() %>%
                  mutate(next.next.year = year + 2) #will be used to join with year column to get last year's avg points allowed
      ,
      by = c("Next.Opp" = "Opp", "year" = "next.next.year")
      )


	# When Next.Opp is NA, it means that there is no team to be played next week because the player will be on a bye week. 
	# This will cause the columns 'Avg.Points.Allowed.Last.Year' and 'Avg.Points.Allowed.Last.Last.Year' to be NA as well. 
	# Since I want to use these columns for my model, I'll fill the with something reasonable - the average amount of points allowed. 
	df_wr[is.na(df_wr$Avg.Points.Allowed.Last.Year), "Avg.Points.Allowed.Last.Year"] <- mean(df_wr$Avg.Points.Allowed.Last.Year, na.rm = TRUE)
	df_wr[is.na(df_wr$Avg.Points.Allowed.Last.Last.Year), "Avg.Points.Allowed.Last.Last.Year"] <- mean(df_wr$Avg.Points.Allowed.Last.Last.Year, na.rm = TRUE)

	# Keep data from 2011 and 2012 only. 
	df_wr <- df_wr %>% filter(year > 2012)

	# As I will be incorporating previous weeks' performances into my model, I don't want to take into account breakaway weeks when fitting my regresssion parameters. So as a simple heuristic, I'll ignore weeks that uncharactaristic of the past 3 years of data for each player. 

	# To determine if a performance is "uncharactaristic", I'll use the boxplot (Tukey) outlier method. 
	test <- 
	df_wr %>% #Join to get the upper and lower Tukey thresholds for a player's fantasy points, based on data since 2014.
	      left_join(
	                df_wr %>%
	                      group_by(Player) %>% #finding average for each player
	                      summarize(
	                            Q25.Fantasy.Points = quantile(Fantasy.Points, probs = .25), #25th quantile of fantasy points earned in games since 2014
	                            Q75.Fantasy.Points = quantile(Fantasy.Points, probs = .75) #75th quantile of fantasy points earned in games since 2014
	                      ) %>%
	                      mutate( IQR.Fantasy.Points = Q75.Fantasy.Points - Q25.Fantasy.Points) %>% #Get the IQR of fantasy points earned since 2014
	                      mutate( 
	                            Lower.Limit.Fantasy.Points = Q25.Fantasy.Points - 3*IQR.Fantasy.Points, 
	                            Upper.Limit.Fantasy.Points = Q75.Fantasy.Points + 3*IQR.Fantasy.Points #calculate lower and upper tukey limits
	                      ) %>%
	                      select(Player, Lower.Limit.Fantasy.Points, Upper.Limit.Fantasy.Points) #keep only columns I want to bring over in join
	                , 
	                by = "Player"
	      ) %>%
	      filter( Fantasy.Points >= Lower.Limit.Fantasy.Points & Fantasy.Points <= Upper.Limit.Fantasy.Points)

	# Fill NA values in test
	test[is.na(test)] <- 0
	# Fit dummy model
	model <- lm(data = test, Next.Fantasy.Points ~ Cum.Avg.Targets + Cum.Avg.Yds +  Previous.Avg.Targets    ,  na.action = na.exclude )

	# Add predictions
	test$Predictions = predict(model)

	# Prioritize players for lineup
	predictions <- test %>% 
            mutate(max_year = max(year)) %>% 
            filter(year == max_year) %>% #keep only most recent year's predictions
            group_by(Player) %>% #group by player to get the most recent week the player has played
            mutate(max_week = max(week)) %>% 
            filter(week == max_week) %>% #Only keep each player's last week predictions
            select (Player, Team, Pos, year, week, Previous.Avg.Fantasy.Points, Predictions) %>% #Information I'd like to see when chosing lineup
            arrange(desc(Predictions)) %>%
            ungroup() %>%
            top_n(20)


    # Store the week (for the file name)
    year <- predictions$year[1]
    week <- predictions$week[1] + 1
    # If the year is 2016, it means it is the draft prediction and there is no data for 2017 yet. In this case set the week to one. 
    if(year < 2017) {
    	week <- 1
    }


    # Write to file
    dir.create(file.path("./predictions"), showWarnings = FALSE)
    dir.create(file.path(paste("./predictions/week-", week, "/", sep = "")), showWarnings = FALSE)
    write.csv(x = predictions, file = paste("./predictions/week-", week,"/", "WR.csv", sep=""))
}









# Predict 
predict_te()
predict_qb()
predict_rb()
# predict_wr()


