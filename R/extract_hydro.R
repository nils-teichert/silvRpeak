#' @title Creates a table of hydrological predictors from river flow data
#'
#' @description This function uses river flow data to returns a table of hydrological predictors for the migration models.
#'
#' @param date a vector of class "Date" representing calendar dates.
#' @param quse a numeric vector of the daily river discharge (in situ observation or prediction). Must be the same length as the date vector.
#' @param Qlim_class a vector with the 10 ordered  quantiles values of the river discharge, 0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9 and 0.99. Must be specified by the user.
#' @param qlimuse a numeric value reflecting the river discharge beyond which the number of days is cumulated.If \code{qlimuse = “Q70”} (default parameter), the 0.70 discharge quantile value is used.
#' @param start a numeric value between 01 and 12 reflecting the month initiating the migration season (default: \code{09} September).
#'
#' @return A data.frame with the hydrological predictors.
#' \itemize{
#'   \item date : the date,
#'   \item bio_year : the migration season starting from the month \code{start},
#'   \item lunarphase : a factor variable with the 4 lunar phase labels,
#'   \item Q : the daily river discharge,
#'   \item Qlim_class : the class estimate of daily flow between 0 and 10,
#'   \item delta_1d : the daily flow change standardised by the river flow extent,
#'   \item delta_sum3d : the sum daily flow change over 3 days standardised by the river flow extent,
#'   \item delta_sum7d : the sum daily flow change over 7 days standardised by the river flow extent,
#'   \item Nday_above : the cumulative number of days above the flow threshold \code{qlimuse}.
#' }
#'
#' @examples
#' # extracts hydrological predictors for the Elorn River
#' hydro_elorn <- extract_hydro(date = elorn_sub$date, quse = elorn_sub$Q,
#' Qlim_class = c(1.31,1.65,2.06,2.7,3.52,4.63,6.09,8.47,12.7,28.9))
#' head(hydro_elorn)
#'
#' @export

extract_hydro <- function(date, quse, Qlim_class, qlimuse ="Q70", start=09){
  date<-as.Date(date)
  if(qlimuse=="Q70") {qlimuse = Qlim_class[7]}
  stand_val <- c(Qlim_class[9]-Qlim_class[1])
  lunePhase <- lunar::lunar.phase(date, name = 4)
  bio_year<-ifelse(lubridate::month(date)<start, lubridate::year(date)-1, lubridate::year(date))
  delta_1d <- c(NA, diff(quse))
  delta_sum3d <- zoo::rollapply(delta_1d, width=3, FUN=sum,  fill = NA,  align = "right") / stand_val
  delta_sum7d <- zoo::rollapply(delta_1d, width=7, FUN=sum,  fill = NA,  align = "right") / stand_val
  delta_1d_stand_val <- delta_1d / stand_val
  qcalss <- rowSums(sapply(Qlim_class, function(x) ifelse(quse>x, 1, 0)))
  yearid<-unique(bio_year)
  Nday_above<-c()
  start_date <- ifelse(start<=10, paste("0",start, sep=""), as.character(start))
  for(i in 1:length(yearid)){
    pic<-ifelse(quse>qlimuse,1,0)[bio_year==yearid[i]]
    pic[is.na(pic)]<-0
    Nday_above <- c(Nday_above, cumsum(pic))
  }
  data.frame(date, bio_year, lunarphase=lunePhase, Q=quse, qcalss = qcalss, delta_1d = delta_1d_stand_val,
             delta_sum3d=delta_sum3d, delta_sum7d=delta_sum7d, Nday_above)
}
