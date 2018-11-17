

##chapter5

#lahmna names 

lahmanNames %>%  
  # Bind the data frames in lahmanNames
  bind_rows()  %>%
  # Group the result by var
  group_by(var)  %>%
  # Tally the number of appearances
  summarise(n=n()) %>%
  # Filter the data
  filter(n > 1) %>% 
  # Arrange the results
  arrange(desc(n))
  
  ##who are the players
  
  Master

(players <- Master %>% 
  # Return one row for each distinct player
  distinct(playerID, nameFirst, nameLast))
  
  # missing salaries
  
  players %>% 
  # Find all players who do not appear in Salaries
  anti_join(Salaries) %>%
  # Count them
  count()
  
  ## unpaid games
  
  players %>% 
  anti_join(Salaries, by = "playerID") %>% 
  # How many unsalaried players appear in Appearances?
  semi_join(Appearances, by = 'playerID') %>% 
  count()
  
  ## how many games
  
  players %>% 
  # Find all players who do not appear in Salaries
  anti_join(Salaries, by='playerID') %>% 
  # Join them to Appearances
  left_join(Appearances, by='playerID') %>% 
  # Calculate total_games for each player
  group_by(playerID) %>%
  summarise(total_games = sum(G_all, na.rm=TRUE))   %>%
  # Arrange in descending order by total_games
  arrange(desc(total_games))
  
  ##how any at bats
  
  players %>%
  # Find unsalaried players
  anti_join(Salaries, by='playerID')   %>% 
  # Join Batting to the unsalaried players
  left_join(Batting, by = 'playerID') %>% 
  # Group by player
  group_by(playerID) %>% 
  # Sum at-bats for each player
  summarise(total_at_bat = sum(AB, na.rm=TRUE))  %>% 
  # Arrange in descending order
  arrange(desc(total_at_bat))
  
  ## hall of fame nominees
  
  head(HallOfFame,7)

# Find the distinct players that appear in HallOfFame
nominated <- HallOfFame %>% 
  distinct(playerID)

head(nominated, 7)


nominated %>% 
  # Count the number of players in nominated
  count()

head(Master,7)
(nominated_full <- nominated %>% 
  # Join to Master
  left_join(Master, by='playerID') %>% 
  # Return playerID, nameFirst, nameLast
  select(playerID,nameFirst, nameLast))
  
  ## hall of fame inductions
  
  # Find distinct players in HallOfFame with inducted == "Y"
inducted <- HallOfFame %>% 
  filter(inducted=='Y') %>% 
  distinct(playerID)

inducted %>% 
  # Count the number of players in inducted
  count()

inducted_full <- inducted %>% 
  # Join to Master
  left_join(Master, by='playerID')   %>% 
  # Return playerID, nameFirst, nameLast
  select(playerID, nameFirst, nameLast)
  
  ## awards 
  
  
# Tally the number of awards in AwardsPlayers by playerID
nAwards <- AwardsPlayers %>% 
  group_by(playerID)   %>%
  tally()

head(AwardsPlayers)
head(nAwards,7)

nAwards %>% 
  # Filter to just the players in inducted 
  semi_join(inducted, by='playerID') %>% 
  # Calculate the mean number of awards per player
  summarise(avg_n = mean(n, na.rm=TRUE))

nAwards %>% 
  # Filter to just the players in nominated 
  semi_join(nominated, by='playerID' ) %>% 
  # Filter to players NOT in inducted 
  anti_join(inducted, by='playerID') %>% 
  # Calculate the mean number of awards per player
  summarise(avg_n = mean(n, na.rm=TRUE))
  
  ## retirement
  
  str(Appearances)

Appearances %>% 
  # Filter Appearances against nominated
  semi_join(nominated, by='playerID') %>% 
  # Find last year played by player
  group_by(playerID) %>% 
  summarise(last_year = max(yearID)) %>% 
  # Join to full HallOfFame
  left_join(HallOfFame, by='playerID')   %>% 
  # Filter for unusual observations
  filter(last_year >= yearID)
