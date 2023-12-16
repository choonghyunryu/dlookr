#' Flights data
#' 
#' @description 
#' Sample of on-time data for all flights that departed NYC (i.e. JFK, LGA or EWR) in 2013. 
#' 
#' @format A data frame with 3000 rows and 19 variables. The variables are as follows:
#' \describe{
#'   \item{year, month, day}{Date of departure.}
#'   \item{dep_time, arr_time}{Actual departure and arrival times (format HHMM or HMM), local tz.}
#'   \item{sched_dep_time, sched_arr_time}{Scheduled departure and arrival times (format HHMM or HMM), local tz.}
#'   \item{dep_delay, arr_delay}{Departure and arrival delays, in minutes. Negative times represent early departures/arrivals.}
#'   \item{carrier}{Two letter carrier abbreviation. See airlines to get name.}
#'   \item{flight}{Flight number.}
#'   \item{tailnum}{Plane tail number. See planes for additional metadata.}
#'   \item{origin, dest}{Origin and destination.}
#'   \item{air_time}{Amount of time spent in the air, in minutes.}
#'   \item{distance}{Distance between airports, in miles.}
#'   \item{hour, minute}{Time of scheduled departure broken into hour and minutes.}
#'   \item{time_hour}{Scheduled date and hour of the flight as a POSIXct date.}
#' }
#' @docType data
#' @keywords datasets
#' @name flights
#' @usage data(flights)
#' @source 
#' RITA, Bureau of transportation statistics, <https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236>
#' "Flights data" in nycflights13 package <https://github.com/hadley/nycflights13>, License : CC0(Public Domain)
NULL

# set.seed(1234)
# flights <- nycflights13::flights[sample(NROW(nycflights13::flights), size = 3000), ]
# save(flights, file = "data/flights.rda", version = 2)
