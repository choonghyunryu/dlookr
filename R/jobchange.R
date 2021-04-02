#' Job Change of Data Scientists
#' 
#' @description 
#' A dataset containing the gender and other attributes of almost 20000 cases. 
#' 
#' @details 
#' This dataset designed to understand the factors that lead a person to leave current job for HR researches too.
#' 
#' @format A data frame with 19158 rows and 14 variables. The variables are as follows:
#' \describe{
#'   \item{enrollee_id}{unique ID for candidate}
#'   \item{city}{city code.}
#'   \item{city_dev_index}{developement index of the city (scaled).}
#'   \item{gender}{gender of candidate.}
#'   \item{relevent_experience}{relevant experience of candidate}
#'   \item{enrolled_university}{type of University course enrolled if any.}
#'   \item{education_level}{education level of candidate.}
#'   \item{major_discipline}{education major discipline of candidate.}
#'   \item{experience}{candidate total experience in years.}
#'   \item{company_size}{number of employees in current employer's company.}
#'   \item{company_type}{type of current employer.}
#'   \item{last_new_job}{difference in years between previous job and current job.}
#'   \item{training_hours}{training hours completed.}
#'   \item{job_chnge}{if looking for a job change (boolean), Yes, No.}
#' }
#' @docType data
#' @keywords datasets
#' @name jobchange
#' @usage data(jobchange)
#' @source 
#' "HR Analytics: Job Change of Data Scientists" in Kaggle <https://www.kaggle.com/arashnic/hr-analytics-job-change-of-data-scientists>, License : CC0(Public Domain
NULL

# library(dplyr)
# 
# jobchange <- read.csv("pkg_data/aug_train.csv")
# 
# change_factor <- function(x) {
#   as.factor(ifelse(x %in% "", NA, x))
# }
# 
# jobchange <- jobchange %>%
#   mutate(enrollee_id = as.character(enrollee_id),
#          city = as.factor(city),
#          gender = change_factor(gender),
#          relevent_experience = change_factor(relevent_experience),
#          enrolled_university = change_factor(enrolled_university),
#          education_level = 
#            ordered(education_level, levels = c("Primary School", "High School", 
#                                                "Graduate", "Masters", "Phd")),
#          major_discipline = change_factor(major_discipline),
#          experience = 
#            ordered(experience, levels = c("<1", 1:20, ">20")),
#          company_size = case_when(company_size %in% "10/49" ~ "10-49", 
#                                   company_size %in% "100-500" ~ "100-499",
#                                   TRUE   ~ company_size),
#          company_size = 
#            ordered(company_size, levels = c("<10", "10-49", "50-99", "100-499",
#                                             "500-999", "1000-4999", "5000-9999",
#                                             "10000+")),
#          company_type = change_factor(company_type),
#          last_new_job = 
#            ordered(last_new_job, levels = c("never", "1", "2", "3", "4", ">4")),        
#          target = ifelse(target == 1, "Yes", "No"),
#          target = change_factor(target)) %>% 
#   rename(city_dev_index = city_development_index,
#          job_chnge = target)
# 
# save(jobchange, file = "data/jobchange.rda")
