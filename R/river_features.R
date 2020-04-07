#' @title Hydrological features of the references rivers
#'
#' @description Hydrological features of the references rivers used to adjust the generalized boosted regression models for predicting silver eel migrations.
#'
#' @format A data frame with 12 rows and 13 variables:
#' \itemize{
#'   \item river : the river name,
#'   \item site : the locality,
#'   \item catchment : the catchment area (km²)
#'   \item Q10 to Q99 : the quantiles of river discharge from 0.1 to 0.99 (m3/s).
#'   ...
#' }
#'
#' @examples head(river_features)
#'
#'
#' @source HYDRO eaufrance
#' \url{http://www.hydro.eaufrance.fr/}
#'
#'
"river_features"
