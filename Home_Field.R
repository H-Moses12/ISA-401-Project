# url containing teams win % for 2023
url <- 'https://www.teamrankings.com/nfl/trends/win_trends/'

# scraping the table for NFL team's win % in 2023
overall <- 
  rvest::read_html(url) |>
  rvest::html_element(css = 'table') |>
  rvest::html_table(header = 1)

# checking data types, realized that win percent is stored as a character
str(overall)

# removing the '%' from the win percent values, and making them numeric
overall$`Win %` <- as.numeric(sub("%", "", overall$`Win %`))

# everything looks good now
str(overall)

# removing columns we don't need
overall <- overall[,-2]
overall <- overall[,-3:-4]

# renaming win percentage column so we know its for home & away games
overall <- dplyr::rename(overall, overall_win_pct = `Win %`)

# make sure everything looks good
print(overall)

# now time to grab teams home win %
url_2 <- 'https://www.teamrankings.com/nfl/trends/win_trends/?sc=is_home'

# scraping table with NFL team's win % at home for 2023
home <- 
  rvest::read_html(url_2) |>
  rvest::html_element(css = 'table') |>
  rvest::html_table(header = 1)

# making win % numeric
home$`Win %` <- as.numeric(sub("%", "", home$`Win %`))
str(home)

# renaming the win % column
home <- dplyr::rename(home, home_win_pct = `Win %`)

# removing columns we don't need
home <- home[,-2]
home <- home[,-3:-4]

# making sure everything looks good
print(home)

# url for teams margin of victory at home and away
url_3 <- 'https://www.teamrankings.com/nfl/stat/average-scoring-margin'

#scraping table
mov_2023 <-
  rvest::read_html(url_3) |>
  rvest::html_element(css = 'table') |>
  rvest::html_table(header = 1)

# converting home/away MOV to numbers
mov_2023$Home <- as.numeric(mov_2023$Home)
mov_2023$Away <- as.numeric(mov_2023$Away)

# renaming columns to remove ambiguity
mov_2023 <- dplyr::rename(mov_2023, home_mov = Home)
mov_2023 <- dplyr::rename(mov_2023, away_mov = Away)

# removing columns we don't need
mov_2023 <- mov_2023[,-1]
mov_2023 <- mov_2023[,-2:-4]
mov_2023 <- mov_2023[,-4]


# joing the first two tables based on team name
win_pct_2023_int <- dplyr::inner_join(overall, home, by = "Team")

# joining the last table
win_pct_2023 <- dplyr::inner_join(
  win_pct_2023_int, mov_2023, by = "Team")

# creating a new variable called "home lift". This variable is calculated
# by dividing a teams win percentage at home by their overall win percentage.
# Essentially, it shows how much more likely a team is to win at home compared
# to all of their games
win_pct_2023$home_lift <- 
  round(win_pct_2023$home_win_pct / win_pct_2023$overall_win_pct, 2)

# writing our data frame to an excel document for further modifications
# if(require(openxlsx) == FALSE)install.packages("openxlsx")
# library(openxlsx)
# write.xlsx(win_pct_2023, "win_pct_2023.xlsx")

# In excel we manually entered a column call 'fwmi' which stands for football
# weather missery index and is essentially a number calculated by the weather
# channel for how difficult an NFL cities weather is to play in. higher values
# mean tougher weather, and lower values mean easier weather. Cities with a dome
# have a score of 0 so we made a new column called 'dome' that is true is fwmi
# is 0 and false otherwise. We also added each teams city & state from our own 
# memory so we can create maps in tableau. Next, we calculated a home strength
# of schedule for each team so we can investigate this further. Lastly, we 
# manually input weather each team has turf or grass.

# ------------------------------------------------------------------------------

# url containing each teams schedule for 2023
url_4 <- 'https://www.espn.com/nfl/schedulegrid'

# scraping the table
schedule <-
  rvest::read_html(url_4) |>
  rvest::html_element(css = 'table') |>
  rvest::html_table(header = 3)

# table didn't scrape perfectly so we put it in an excel file to edit it further.
# We will first finish cleaning the data, then we will compute SOS for home games.
# library(openxlsx)
# write.xlsx(schedule, "schedule.xlsx")

# ------------------------------------------------------------------

# htmls for each teams overall record over the last three years
url_5 <- 'https://www.teamrankings.com/nfl/trends/win_trends/?range=yearly_2023'
url_6 <- 'https://www.teamrankings.com/nfl/trends/win_trends/?range=yearly_2022'
url_7 <- 'https://www.teamrankings.com/nfl/trends/win_trends/?range=yearly_2021'

# scraping the tables
all_23 <-
  rvest::read_html(url_5) |>
  rvest::html_element(css = 'table') |>
  rvest::html_table(header = 1)

all_22 <-
  rvest::read_html(url_6) |>
  rvest::html_element(css = 'table') |>
  rvest::html_table(header = 1)

all_21 <-
  rvest::read_html(url_7) |>
  rvest::html_element(css = 'table') |>
  rvest::html_table(header = 1)

# converting win % to a numeric value
all_23$`Win %` <- as.numeric(sub("%", "", all_23$`Win %`))
all_22$`Win %` <- as.numeric(sub("%", "", all_22$`Win %`))
all_21$`Win %` <- as.numeric(sub("%", "", all_21$`Win %`))

# sorting the tables by team name so teams are in the same row in each
# table for future calculations
all_23 <- all_23[order(all_23$Team, decreasing = TRUE), ]
all_22 <- all_22[order(all_22$Team, decreasing = TRUE), ]
all_21 <- all_21[order(all_21$Team, decreasing = TRUE), ]

# urls for each teams record at home over the last three years
url_8 <- 'https://www.teamrankings.com/nfl/trends/win_trends/?range=yearly_2023&sc=is_home'
url_9 <- 'https://www.teamrankings.com/nfl/trends/win_trends/?range=yearly_2022&sc=is_home'
url_10 <- 'https://www.teamrankings.com/nfl/trends/win_trends/?range=yearly_2021&sc=is_home'

# scraping each teams home record
home_23 <-
  rvest::read_html(url_8) |>
  rvest::html_element(css = 'table') |>
  rvest::html_table(header = 1)

home_22 <-
  rvest::read_html(url_9) |>
  rvest::html_element(css = 'table') |>
  rvest::html_table(header = 1)

home_21 <-
  rvest::read_html(url_10) |>
  rvest::html_element(css = 'table') |>
  rvest::html_table(header = 1)

# converting win % to a number
home_23$`Win %` <- as.numeric(sub("%", "", home_23$`Win %`))
home_22$`Win %` <- as.numeric(sub("%", "", home_22$`Win %`))
home_21$`Win %` <- as.numeric(sub("%", "", home_21$`Win %`))

# sorting teams by name again for calculations
home_23 <- home_23[order(home_23$Team, decreasing = TRUE), ]
home_22 <- home_22[order(home_22$Team, decreasing = TRUE), ]
home_21 <- home_21[order(home_21$Team, decreasing = TRUE), ]

# finding each teams avg overall win % for the last 3 years
all_23$avg_win <- round((all_23$`Win %` + all_22$`Win %` + all_21$`Win %`) / 3, 2)

# finding each teams avg home win % for the last three years
home_23$avg_win <- round((home_23$`Win %` + home_22$`Win %` + home_21$`Win %`) / 3, 2)

# dividing each teams home win % by their overall win % to see what teams are
# more or less likely to win at home compared to overall
all_23$home_lift <- round((home_23$avg_win / all_23$avg_win), 1)

# sorting by the value we just calculated in desc. order to see which teams are
# the best at home over the past 3 years
all_23 <- all_23[order(all_23$home_lift, decreasing = TRUE), ]

# removing unnecessary columns for readability
all_23 <- all_23[,-2:-6]

# renaming data frame to reflect the data it now contains
home_lift_historical <- all_23

# writing df to an excel file
# library(openxlsx)
# write.xlsx(home_lift_historical, "home_lift_historical.xlsx")

# ---------------------------------------------------------------------------

# url showing teams turnover margins for home vs. away
url_11 <- 'https://www.teamrankings.com/nfl/stat/turnover-margin-per-game'

turnovers_23 <-
  rvest::read_html(url_11) |>
  rvest::html_element(css = 'table') |>
  rvest::html_table(header = 1)

# data types look good
str(turnovers_23)

# pivoting table so we can visualize it in tableau easier
turnovers_long = turnovers_23 |>
  tidyr::pivot_longer(
    cols = 6:7,
    names_to = 'Home/Away',
    values_to = 'turnover_margin'
  )

print(turnovers_long)

# conversion looks succesful, overwriting the original table
turnovers_23 <- turnovers_long

# removing unnecessary columns
turnovers_23 <- turnovers_23[,-1]
turnovers_23 <- turnovers_23[,-2:-5]

# exporting to excel so we can visualize in tableau
# library(openxlsx)
# write.xlsx(turnovers_23, "turnovers.xlsx")

#----------------------------------------------------------

# downloading updated win_pct_2023 
win_pct_2023_updated <- read.xlsx('data/win_pct_2023.xlsx')

# plotting the missing vals for the 3 final dfs exported to tableau
DataExplorer::plot_missing(win_pct_2023_updated)
DataExplorer::plot_missing(turnovers_23)
DataExplorer::plot_missing(home_lift_historical)

# showing data types - all seem valid
dplyr::glimpse(win_pct_2023_updated)
dplyr::glimpse(turnovers_23)
dplyr::glimpse(home_lift_historical)






