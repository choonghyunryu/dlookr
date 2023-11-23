#' Medical Cost Personal Datasets
#' 
#' @description 
#' A dataset containing the age and other attributes of almost 1400 cases. 
#' 
#' @format A data frame with 1338 rows and 7 variables. The variables are as follows:
#' \describe{
#'   \item{age}{age of primary beneficiary.}
#'   \item{sex}{insurance contractor gender, female, male.}
#'   \item{bmi}{body mass index, providing an understanding of body, weights that are relatively high or low relative to height, objective index of body weight (kg / m ^ 2) using the ratio of height to weight, ideally 18.5 to 24.9.}
#'   \item{children}{number of children covered by health insurance / Number of dependents.}
#'   \item{smoker}{flag of smoking, yes, no.}
#'   \item{region}{the beneficiary's residential area in the US, northeast, southeast, southwest, northwest.}
#'   \item{charges}{individual medical costs billed by health insurance.}
#' }
#' @docType data
#' @keywords datasets
#' @name medicost
#' @usage data(medicost)
#' @source 
#' "Medical Cost Personal Datasets" in Kaggle <https://www.kaggle.com/datasets/mirichoi0218/insurance/>, License : Open Database License
NULL

# library(dplyr)
# 
# medicost <- read.csv("pkg_data/insurance.csv")
# 
# change_factor <- function(x) {
#   as.factor(ifelse(x %in% "", NA, x))
# }
# 
# medicost <- medicost %>%
#   mutate(sex = as.factor(sex),
#          smoker = change_factor(smoker),
#          region = change_factor(region)) 
# 
# save(medicost, file = "data/medicost.rda")
