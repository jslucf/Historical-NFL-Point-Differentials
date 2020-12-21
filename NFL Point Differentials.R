#Reads in season games data off Github
seasons = read.csv('https://raw.githubusercontent.com/leesharpe/nfldata/master/data/games.csv')

#set the current week of the NFL season that was last completed (used for the graphs later on)
current.week = 13

#Gets only seasons since 2002 divisional realignment
df.realign = seasons %>%
   filter(season >= 2002, game_type == 'REG')

#Need to get individual teams on each line, one home and one away
df.home = df.realign %>%
   mutate(team = home_team, 
          opp = away_team, 
          PtsF = home_score, 
          PtsA = away_score) %>%
   select(game_id, season, week, team, opp, PtsF, PtsA)


df.away = df.realign %>%
   mutate(team = away_team, 
          opp = home_team, 
          PtsF = away_score, 
          PtsA = home_score) %>%
   select(game_id, season, week, team, opp, PtsF, PtsA)

df.comb = bind_rows(df.home, df.away) %>%
   mutate(team.id = paste(team, substr(season,3,4), sep="")) %>% #team-season ID
   #Gets the divisions
   mutate(division = case_when(
      team %in% c('NYG', 'DAL', 'PHI', 'WAS', 'WFT') ~ 'NFC East',
      team %in% c('GB', 'DET', 'MIN', 'CHI') ~ 'NFC North',
      team %in% c('ATL', 'CAR', 'NO', 'TB') ~ 'NFC South',
      team %in% c('STL', 'LA', 'ARI', 'SEA', 'SF') ~ 'NFC West',
      team %in% c('BUF', 'MIA', 'NE', 'NYJ') ~ 'AFC East',
      team %in% c('HOU', 'JAC', 'JAX', 'TEN', 'IND') ~ 'AFC South',
      team %in% c('BAL', 'CLE', 'CIN', 'PIT') ~ 'AFC North',
      team %in% c('LV', 'OAK', 'DEN', 'KC', 'LAC', 'SD') ~ 'AFC West')) %>%
   #Division-season ID
   mutate(division.id = paste(division, substr(season,3,4), sep="")) 

#Lookup of team and divisions
team.div.list = unique(df.comb[, c('team', 'division')])

#Joins on the division of the opponent and then checks to see if the game was a divisional game
df.comb = df.comb %>%
   full_join(team.div.list, by=c('opp' = 'team')) %>%
   rename(division = division.x, opp.division = division.y) %>%
   mutate(div.game = ifelse(division == opp.division, 1, 0))

#gets the cumulative pts for and pts against for each team-season combo
df.teams = df.comb %>%
   arrange(team.id, week) %>%
   group_by(team.id, season) %>%
   mutate(PtsF.s = cumsum(PtsF),
          PtsA.s = cumsum(PtsA),
          PD = PtsF.s - PtsA.s) %>%
   filter(week <= current.week) 

#Line chart of the PD by week for all teams in the dataset
ggplot(df.teams, aes(week, PD, fill = team.id)) +
   geom_line(alpha=0.2) +
   #This line highlights teams in the current season, 2020
   geom_line(data=df.teams %>% filter(season==2020),
             aes(week, PD, fill = team.id, alpha=.5), col='blue', size=1, alpha = .5) +
   theme(legend.position = "none") +
   scale_x_continuous(breaks=c(1:17)) +
   #Zooms in only on the same week in completed seasons as current season has also completed
   coord_cartesian(xlim = c(1,current.week)) +
   xlab('Week') +
   ylab('Point Differential') +
   ggtitle('NFL Point Differential by Week (Since 2002 Realignment)')

#Same chart as above but gives overlay to highlight specific teams you'd want to call out
ggplot(df.teams, aes(week, PD, fill = team.id)) +
   geom_line(alpha=0.2) +
   geom_line(data=df.teams %>% filter(team.id %in% c('NYJ20', 'JAX20', 'PIT20', 'KC20')),
             aes(week, PD, fill = team.id, alpha=.5), col='Dark Green', size=2) +
   geom_line(data=df.teams %>% filter(team.id %in% c('CLE17', 'DET08')),
             aes(week, PD, fill = team.id, alpha=.5), col='Yellow', size=2) +
   geom_line(data=df.teams %>% filter(team.id %in% c('STL08')),
             aes(week, PD, fill = team.id, alpha=.5), col='Red', size=2) +
   geom_line(data=df.teams %>% filter(team.id %in% c('NE07')),
             aes(week, PD, fill = team.id, alpha=.5), col='Blue', size=2) +
   theme(legend.position = "none") +
   scale_x_continuous(breaks=c(1:15)) +
   coord_cartesian(xlim = c(1,15), ylim = c(-200,300)) +
   geom_label(data=df.teams %>% filter(team.id %in% c('NYJ20', 'JAX20', 'STL08', 'PIT20', 'NE07', 'CLE17', 'DET08', 'KC20'), 
                                       week==current.week),
              aes(week +1, PD, fill = team.id,
                  label = paste(team.id, PD, sep=': ')),
              position=position_jitter(width=-2,height=10)) +
   xlab('Week') +
   ylab('Point Differential') +
   ggtitle('NFL Point Differential by Week (Since 2002 Realignment)')
              
#This chunk gets the PD by week for each division ID
df.div = df.teams %>%
   #full_join(div.game.lookup, by=c('game_id' = 'game_id')) %>%
   arrange(division.id, week) %>%
   #filter(div.game == 0 ) %>%
   group_by(division.id, season, week) %>%
   summarize(neg.PD = sum(ifelse(PD<0,1,0)),
             teams.active = n(),
             PtsF.s = sum(PtsF),
          PtsA.s = sum(PtsA),
          PD = PtsF.s - PtsA.s,
          ) %>%
   mutate(PD.s = cumsum(PD)) %>%
   filter(week <= current.week) 

#Line chart of every division's PD through the current week
ggplot(df.div, aes(week, PD.s, fill = division.id)) +
   geom_line(alpha=0.2) +
   geom_line(data=df.div %>% filter(season==2020),
             aes(week, PD.s, fill = division.id, alpha=.5), col='blue', size=1, alpha = .5) +
   theme(legend.position = "none") +
   scale_x_continuous(breaks=c(1:17)) +
   coord_cartesian(xlim = c(1,current.week)) +
   xlab('Week') +
   ylab('Point Differential') +
   ggtitle('NFL Point Differential by Week (Since 2002 Realignment)')
