#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#
library(plumber)
library(dplyr)
library(fable)
library(forecast)
library(tsibble)
library(tsibbledata)
library(ggplot2)
library(feasts)
library(tsibble)

#* @apiTitle Forecasting API
#* 
# Globals:
# activeDF             Dataframe of all intervals and skills
# skillsV              Vector of skill in in activeDF
# g_SkillNum           index of active skill_combo in skillsV
# g_SkillName          name of active skill_combo
# activeMonth          Monthly summary
# activeDay            Daily summary
# g_monthly_fit        fitted model
# g_monthly_comps       monthly model decomposition
# dfSun ... dfSat      day of week summaries
# activeMeans          day of week averages (means) 
# startDate
# endDate

# funct:setDateRange
# IN:   eDate, sDate
# OUT:  endDate, startDate
# Purpose: Checks that endDate is after startDate 
#          then places endDate and startDate in global scope.
setDateRange <- function(sDate=NULL, eDate=NULL) {
  if(is.null(eDate)) {
    eDate = Sys.Date()
    print(paste("eDate = ", Sys.Date()))
  }
  if(is.null(sDate)) {
    sDate <- "1970-01-01"
    print(paste("sDate = ", sDate))
  }
  print(paste(sDate, " to ", eDate))
  if(sDate >= eDate) {
    print(paste(sDate, " to ", eDate))
    print("ERROR: startDate is greater than endDate")
  } else {
    endDate <<- eDate
    startDate <<- sDate
  } 
}

# Endpoint: /init/openfile
#* Open a CSV file and extract skills and intervals
#* IN: pathname of CSV file
#* OUT: intervals into activeDF dataframe in global scope
#* OUT: Skills  
#* @param fname
#* @post /init/openfile
function(fname="/sun-country-full.csv2", startDate="2019-11-01", endDate="2022-07-01") {
  df <- read.csv(fname, header=TRUE, na.strings=c(NA,""))
  df <- na.omit(df)
  setDateRange(startDate,endDate)
  activeDF <<- df %>% filter((df$interval >= startDate) & (df$interval < endDate))
  head(activeDF, 20)
  if(dim(activeDF)[1] > 0) {
    # fill in skillsV - a vector of skill_combos
    Skills()
  }
  return(paste("total intervals = ", dim(activeDF)[1]))
}

#* Echo back the input
#* @get /init/date-range
function() {
    return(c("startDate", startDate, "endDate", endDate))
}

#* Get a listing of skills
#* @get /init/skills 
function()  {
  return(skillsV)
}

#* Select the Active skill
#* @param n  index into skill vector
#* @post /init/pick-skill
function(arg) {
  g_skillName <<- skillsV[arg,]
  AggByDate;
  return(g_skillName)
}

#* Get number of calls by month
#* @get /monthly/volume
function()  {
  return(activeMonth)
}

#* Get number of calls by day
#* @get /daily/volume
function()  {
  return(activeDay)
}

#* Plot a histogram of daily averages over last 30 days of time series
#* @serializer png
#* @get /plot/daily-averages
function() {
  plot(activeMeans, xlab='Day Number', main=g_skillName, type='b', col='magenta', sub = "Averages for last 30 days of time series")
}

#* Get monthly call volume plot
#* @serializer png
#* @get /plot/monthly-volume   plot of call volume by month
function() {
  str = paste("Monthly ", startDate, " - ", endDate)
  return(plot(g_monthly_comps$calls, type='b', main=paste("Monthly Calls ",g_skillName), col='red',xlab = str, ylab = "call volume"))
}

#* Fit a monthly model
#* @get /monthly/fit-ETS-model
#* @serializer json
function() {
  df_tsibble <- dfMonth %>% mutate(month = yearmonth(as.character(month))) %>% as_tsibble(index = month)
  g_monthly_fit <<- df_tsibble %>% model(ETS(calls))
  df <- as.data.frame(g_monthly_comps <<- components(g_monthly_fit))
  return(toJSON(df,force=TRUE))
}

#* Get model decomposition as dataframe
#* @get /monthly/decomposition 
#* @serializer json
function() {
  df <- as.data.frame(g_monthly_comps)
  return(toJSON(df,force=TRUE))
}

#* Get monthly model decomposition plot
#* @serializer png
#* @get /monthly/decomposition-plot
function() {
  dfPlot <- autoplot(g_monthly_comps)
  return(plot(dfPlot))
}

#* Make monthly forecast
#* @param number of months in forecast horizon
#* @get /monthly/forecast
function(arg) {
  g_monthly_forecast_plot<<-g_monthly_fit %>% fabletools::forecast(h=arg) %>% autoplot() + xlab(paste("forecast for ", arg))
  g_monthly_forecast_residuals <<- fabletools::accuracy(g_monthly_fit)
  df <- as.data.frame(g_monthly_fit %>% fabletools::forecast(h=arg))
  return(toJSON(df, force = TRUE))
}

#* Make monthly forecast and return residuals
#* @get /monthly/forecast_residuals
function() {
  return(g_monthly_forecast_residuals)
}

#* PLOT monthly forecast
#* @get /plot/monthly-forecast 
#* @serializer png
function() {
  return(plot(g_monthly_forecast_plot))
}

################################# Daily #####################################

#* Plot volume for last 30 days by ( 1=Sunday, 2=Tuesday,...7=Saturday )
#* @param day of the week ( 1=Sunday, 2=Tuesday,...7=Saturday )
#* @serializer png
#* @get /plot/last-thirty-days
function(arg) {
  if(arg == 1) {
    plot(head(dfSun,30), type='b', col='blue', main=g_skillName, xlab="Last 30 Sundays")
  }
  if(arg == 2)
    plot(head(dfMon,30), type='b', col='green', main=g_skillName, xlab="Last 30 Mondays")
  if(arg == 3)
    plot(head(dfTue,30), type='b', col='red', main=g_skillName, xlab="Last 30 Tuesdays")
  if(arg == 4)
    plot(head(dfWed,30), type='b', col='black', main=g_skillName, xlab="Last 30 Wednesdays")
  if(arg == 5)
    plot(head(dfThu,30), type='b', col='magenta', main=g_skillName, xlab="Last 30 Thursdays")
  if(arg == 6)
    plot(head(dfFri,30), type='b', col='cyan', main=g_skillName, xlab="Last 30 Fridays")
  if(arg == 7)
    plot(head(dfSat,30), type='b', col='red', main=g_skillName, xlab="Last 30 Saturdays")
}

#* Get daily call volume plot
#* @serializer png
#* @get /plot/daily-volume   plot of call volume by day
function() {
  str = paste("Daily ", startDate, " - ", endDate)
  return(plot(activeDay$calls,  main=paste("Daily Calls - ", g_skillName), col='blue', xlab = str, ylab ="call volumne"))
}
  
function(numDays) {
  daily_tsibble <- dfDay %>% as_tsibble(index = day) %>% fill_gaps(calls=0)
  thirtyDay_tsibble <- tail(daily_tsibble, numDays)
  g_daily_fit <<- thirtyDay_tsibble %>% model(ETS(calls))
  g_daily_comps <<- components(g_fit)
}    





######################### miscellaneous functions ###########################

#* Funct: Skills()
#* Purpose: Retrieve all the skill_combo values, remove missing, 
#* and store in a vector, skillsV that resides in global scope.
#* Lastly, Initialize globals g_skillNum and g_skillName
#* IN:activeDF
#* Out: skillsV 
#* Return: number of unique skills
Skills <- function() {
  df <- distinct(select(activeDF, skill_combo))
  df <- na.omit(df, skill_combo)
  if(dim(df)[1] <= 0) {
    return(0)
  }
  skillsV <<- as.vector(df)
  g_skillNum <<- 1
  g_skillName <<- skillsV$skill_combo[g_skillNum]
  return(dim(df)[1])
}


#* funct: aggByDate()
#* Purpose: Filters activeDF by g_SkillName then summarize by Month and Day, 
#*          then by day of the week. 
#* Called by pick-skill
#* IN:activeDF
#* IN:g_skillName
#* OUT: activeMonth            - Monthly Summaries
#* OUT: activeDay              - Daily Summaries
#* OUT: dfSun ... dfSat        - Day of the Week summaries
#* OUT: activeMeans            
AggByDate <- function() { 
  df <-  activeDF %>% filter(skill_combo == g_skillName)
  df[,1] = as.Date(df$interval, "%Y-%m-%d %H:%M:%S")
  
  activeMonth <<- df %>%      
    group_by(month = lubridate::floor_date(interval, "month")) %>%
    summarize(calls = sum(calls))
  
  activeDay <<- df %>% 
    group_by(day = lubridate::floor_date(interval, "day")) %>%
    summarize(calls = sum(calls))
  
  dfSun <<- activeDay %>% filter(wday(day) == 1)
  dfMon <<- activeDay %>% filter(wday(day) == 2)
  dfTue <<- activeDay %>% filter(wday(day) == 3)
  dfWed <<- activeDay %>% filter(wday(day) == 4)
  dfThu <<- activeDay %>% filter(wday(day) == 5)
  dfFri <<- activeDay %>% filter(wday(day) == 6)
  dfSat <<- activeDay %>% filter(wday(day) == 7)
  
  activeMeans <- c(mean(dfSun$calls),mean(dfMon$calls), mean(dfTue$calls), +
                mean(dfWed$calls), mean(dfThu$calls), mean(dfFri$calls), +
                mean(dfSat$calls)
  )
}




if(FALSE) {

  plotMonthlyForecast <- function(h=4) {
    # s = g_skillName
    print(paste("skillcombo =", g_skillName))
    df_tsibble <- dfMonth %>% mutate(month = yearmonth(as.character(month))) %>% as_tsibble(index = month)
    g_fit <<- df_tsibble %>% model(ETS(calls))
    g_comps <<- components(g_fit)
    report(g_fit)
    autoplot(g_comps)
    plot(g_comps$calls, type='b', main=paste("Monthly Calls ",g_skillName), col='red',xlab = "monthly 11/2019 - 06/2022", ylab = "call volume")
    # ForecastPlot(4)
    fc<-g_fit %>% fabletools::forecast(h=4) %>% autoplot() + xlab("forecast for 4 months")
    fabletools::accuracy(g_fit)
  }
  
}

plotDailyForecast <- function() {
  s = g_skillName
  print(paste("skillcombo =", s))
  daily_tsibble <- dfDay %>% as_tsibble(index = day) %>% fill_gaps(calls=0)
  thirtyDay_tsibble <- tail(daily_tsibble, 30)
  g_fit <<- thirtyDay_tsibble %>% model(ETS(calls))
  g_comps <<- components(g_fit)
  fabletools::report(g_fit)
  autoplot(g_comps)
  plot(g_comps$calls, type='b', main=paste("Daily Volume  ",g_skillName), col='blue',xlab = "Thirty Day Volume 06/2022", ylab = "call volume")
  forecast(g_fit,h=21) %>% autoplot() + xlab("forecast over 21 days")
  fabletoos::accuracy(g_fit)
}
######################################################### End #######################################