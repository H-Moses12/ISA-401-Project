url <- 'https://www.teamrankings.com/nfl/trends/win_trends/'

overall <- 
  rvest::read_html(url) |>
  rvest::html_element(css = 'table') |>
  rvest::html_table(header = 1)

str(overall)

overall$`Win %` <- as.numeric(sub("%", "", overall$`Win %`))

str(overall)

url_2 <- 'https://www.teamrankings.com/nfl/trends/win_trends/?sc=is_home'

home <- 
  rvest::read_html(url_2) |>
  rvest::html_element(css = 'table') |>
  rvest::html_table(header = 1)

home$`Win %` <- as.numeric(sub("%", "", home$`Win %`))

str(home)

overall <- overall[order(overall$Team, decreasing = TRUE), ]
home <- home[order(home$Team, decreasing = TRUE), ]

overall$home_lift <- (home$`Win %` / overall$`Win %`)

overall[order(overall$home_lift, decreasing = TRUE), ]


url_3 <- 'https://www.teamrankings.com/nfl/trends/ats_trends/?sc=is_home'

home_ats <- 
  rvest::read_html(url_3) |>
  rvest::html_element(css = 'table') |>
  rvest::html_table(header = 1)

home_ats$`Cover %` <- as.numeric(sub("%", "", home_ats$`Cover %`))

print(home_ats)

install.packages("openxlsx")
library(openxlsx)
write.xlsx(home, "home.xlsx")

data <- readxl::read_excel('home_lift.xlsx')

data_no_dome <- subset(data, fwmi != 0)

summary(lm(`Home MOV4`~fwmi, data = data_no_dome))
summary(lm(home_lift~fwmi, data = data_no_dome))



url_3 <- 'https://www.espn.com/nfl/schedulegrid'

schedule <-
  rvest::read_html(url_3) |>
  rvest::html_element(css = 'table') |>
  rvest::html_table(header = 3)

schedule = schedule[-26,]
library(openxlsx)
write.xlsx(schedule, "schedule.xlsx")

data <- readxl::read_excel('home_lift.xlsx')

str(data)
data$Turf <- as.factor(data$Turf)
data$Dome <- as.factor(data$Dome)
data_no_dome <- subset(data, fwmi != 0)

#-----------------------------------------------------------------------------

library(leaps)
home.mov.reg <- regsubsets(`Home MOV4`~Turf + `Home sos` + `avg. attendance` +
                             Dome, data, nbest = 1)
home.mov.reg.2 <- regsubsets(`Home MOV4`~(fwmi + `Home sos` + `avg. attendance`)**2
                             , data_no_dome, nbest = 1)
summ.reg.1 <- summary(home.mov.reg)
summ.reg.2 <- summary(home.mov.reg.2)

max(summ.reg.2$adjr2)

# index where min happens
which.max(summ.reg.2$adjr2)

# accessing the third model
summ.reg.2$outmat[which.max(summ.reg.2$adjr2),]

#--------------------------------------------------------------------------

library(leaps)
home.mov.reg.3 <- regsubsets(home_lift~Turf + `Home sos` + `avg. attendance`, 
                           data, nbest = 1)
home.mov.reg.4 <- regsubsets(home_lift~fwmi + `Home sos` + `avg. attendance`, 
                             data_no_dome, nbest = 1)
summ.reg.3 <- summary(home.mov.reg.3)
summ.reg.4 <- summary(home.mov.reg.4)

max(summ.reg.4$adjr2)

# index where min happens
which.max(summ.reg.4$adjr2)

# accessing the third model
summ.reg.3$outmat[which.max(summ.reg.4$adjr2),]

#-----------------------------------------------------------------------------

# predicting MOV with sos
sos.reg <- lm(`Home MOV4`~`Home sos`, data = data)
summary(sos.reg)

# predicting MOV with fwmi
fwmi.reg <- lm(`Home MOV4`~fwmi, data = data)
summary(fwmi.reg)

# predicting lift with fwmi
fwmi.reg.2 <- lm(home_lift~fwmi, data = data)
summary(fwmi.reg.2)

# predicting home lift with sos
sos.reg.2 <- lm(home_lift~`Home sos`, data = data)
summary(sos.reg.2)

# making the best model
library(leaps)
home.mov.reg.5 <- regsubsets(`Home MOV4`~(Turf + `Home sos` + `avg. attendance`
                             + Dome)**2, data, nbest = 1)
summ.reg.5 <- summary(home.mov.reg.5)

# finding where r2 is maximized
max(summ.reg.5$adjr2)

# index where min happens
which.max(summ.reg.5$adjr2)

# accessing the third model
summ.reg.5$outmat[which.max(summ.reg.5$adjr2),]


# making the best model pt2
library(leaps)
home.mov.reg.6 <- regsubsets(`Home ATS +/-5`~Turf + `Home sos` + `avg. attendance`
                                          + Dome + I(`Home sos`**2), data, nbest = 1)
summ.reg.6 <- summary(home.mov.reg.6)

# finding where r2 is maximized
max(summ.reg.6$adjr2)

# index where min happens
which.max(summ.reg.6$adjr2)

# accessing the third model
summ.reg.6$outmat[which.max(summ.reg.6$adjr2),]


# predicting MOV with sos
sos.reg.3 <- lm(`Home MOV4`~`Home sos` + I(`Home sos`**2), data = data)
summary(sos.reg.3)

# predicting MOV with fwmi
fwmi.reg.3 <- lm(`Home MOV4`~fwmi + I(fwmi**2), data = data_no_dome)
summary(fwmi.reg.3)

sos.fwmi.reg <- lm(`Home MOV4`~fwmi + I(fwmi**2) + `Home sos` + I(`Home sos`**2)
                   + `avg. attendance` + I(`avg. attendance`**2)
                   , data = data_no_dome)
summary(sos.fwmi.reg)

# ------------------------------------------------------------------
url_4 <- 'https://www.teamrankings.com/nfl/trends/win_trends/?range=yearly_2023'
url_5 <- 'https://www.teamrankings.com/nfl/trends/win_trends/?range=yearly_2022'
url_6 <- 'https://www.teamrankings.com/nfl/trends/win_trends/?range=yearly_2021'

all_23 <-
  rvest::read_html(url_4) |>
  rvest::html_element(css = 'table') |>
  rvest::html_table(header = 1)

all_22 <-
  rvest::read_html(url_5) |>
  rvest::html_element(css = 'table') |>
  rvest::html_table(header = 1)

all_21 <-
  rvest::read_html(url_6) |>
  rvest::html_element(css = 'table') |>
  rvest::html_table(header = 1)

all_23$`Win %` <- as.numeric(sub("%", "", all_23$`Win %`))
all_22$`Win %` <- as.numeric(sub("%", "", all_22$`Win %`))
all_21$`Win %` <- as.numeric(sub("%", "", all_21$`Win %`))

all_23 <- all_23[order(all_23$Team, decreasing = TRUE), ]
all_22 <- all_22[order(all_22$Team, decreasing = TRUE), ]
all_21 <- all_21[order(all_21$Team, decreasing = TRUE), ]


url_7 <- 'https://www.teamrankings.com/nfl/trends/win_trends/?range=yearly_2023&sc=is_home'
url_8 <- 'https://www.teamrankings.com/nfl/trends/win_trends/?range=yearly_2022&sc=is_home'
url_9 <- 'https://www.teamrankings.com/nfl/trends/win_trends/?range=yearly_2021&sc=is_home'

home_23 <-
  rvest::read_html(url_7) |>
  rvest::html_element(css = 'table') |>
  rvest::html_table(header = 1)

home_22 <-
  rvest::read_html(url_8) |>
  rvest::html_element(css = 'table') |>
  rvest::html_table(header = 1)

home_21 <-
  rvest::read_html(url_9) |>
  rvest::html_element(css = 'table') |>
  rvest::html_table(header = 1)

home_23$`Win %` <- as.numeric(sub("%", "", home_23$`Win %`))
home_22$`Win %` <- as.numeric(sub("%", "", home_22$`Win %`))
home_21$`Win %` <- as.numeric(sub("%", "", home_21$`Win %`))

home_23 <- home_23[order(home_23$Team, decreasing = TRUE), ]
home_22 <- home_22[order(home_22$Team, decreasing = TRUE), ]
home_21 <- home_21[order(home_21$Team, decreasing = TRUE), ]

all_23$avg_win <- round((all_23$`Win %` + all_22$`Win %` + all_21$`Win %`) / 3, 1)
home_23$avg_win <- round((home_23$`Win %` + home_22$`Win %` + home_21$`Win %`) / 3, 1)
all_23$home_lift <- round((home_23$avg_win / all_23$avg_win), 1)

all_23 <- all_23[order(all_23$home_lift, decreasing = TRUE), ]

