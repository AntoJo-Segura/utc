#'
#'@title toUTC
#'
#'@description Returns UTC time from local time
#'
#'@param localtime POSIXct local time
#'
#'@return utctime POSIXct UTC time
#'
#'@example toUTC(Sys.time())
#'
toUTC <- function(localtime){
  utctime <- as.POSIXct( localtime, format="%Y-%m-%d %H:%M:%S")
  attr(utctime,"tzone") <- "UTC"
  utctime
}

#'
#'@title fromUTC
#'
#'@description Returns local time from UTC time
#'
#'@param utctime POSIXct UTC time
#'
#'@return localtime POSIXct local time
#'
#'@examples  fromUTC( as.POSIXct("1991-06-29 05:27:25", tz = "UTC") )
#'
fromUTC <- function(utctime){
  localtime <- as.POSIXct( utctime, format="%Y-%m-%d %H:%M:%S")
  attr(localtime,"tzone") <- Sys.timezone()
  localtime
}

#'
#'@title hoursUTC
#'
#'@description This function returns time difference between UTC and Local
#'
#'@return number of hours
#'
#'@example  hoursUTC()
#'
hoursUTC <- function(){
  as.numeric(
    as.POSIXct(
      format(
        as.POSIXct( Sys.time(), format = "%Y-%m-%d %H:%M:%S" ),
             "%Y-%m-%d %H:%M:%S"
      )
    ) -
    as.POSIXct(
      format( as.POSIXct( toUTC(Sys.time() ), format = "%Y-%m-%d %H:%M:%S" ),
             "%Y-%m-%d %H:%M:%S"
      )
    )
  )
}

#'
#'@title add hours
#'
#'@description This function adds h hours to a POSIXct
#'
#'@param h number of hours
#'
#'@param datetime POSIXct
#'
#'
#'@return  POSIXct and h added
#'
#'@example 2 %h+% Sys.time()
#'

#'
'%h+%' <- function(h,datetime){
  h * 60 *60 + datetime
}

