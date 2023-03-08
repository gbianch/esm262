#' Compute Fish Diversity in given data set
#' 
#' This function allows the user to input land area, fertlizer application rate, and precipitation and compute the estimated
#' amount of water into the nearby waterbody and how run off impacts water quality.
#' 
#' @param runoff_coeff
#' @param land_area km^2; size of land where fertilizer is applied
#' @param application_rate kg/ha
#' @param soil_type clay (0.5) or loam (0.2)
#' @param area_type flat or hilly agricultural land 
#' @param precipitaion m/s
#' @param catchment_area km^2
#' 
#'
#' @return runoff concentration
#' @return water quality (high, medium low based on water quality standards) for catchment area

#' 

peak_runoff_discharge = runoff_coeff*catchment_area*rainfall_intensity


