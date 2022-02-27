## ----environment, echo = FALSE, message = FALSE, warning=FALSE----------------
knitr::opts_chunk$set(collapse = TRUE, comment = "", out.width = "600px", dpi = 70)
options(tibble.print_min = 4L, tibble.print_max = 4L)

library(dlookr)
library(dplyr)
library(ggplot2)

## ----import_data--------------------------------------------------------------
library(ISLR)
str(Carseats)

## ----missing------------------------------------------------------------------
carseats <- ISLR::Carseats

suppressWarnings(RNGversion("3.5.0"))
set.seed(123)
carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA

suppressWarnings(RNGversion("3.5.0"))
set.seed(456)
carseats[sample(seq(NROW(carseats)), 10), "Urban"] <- NA

## ----imputate_na, fig.align='center', fig.width = 7, fig.height = 5-----------
if (requireNamespace("rpart", quietly = TRUE)) {
  income <- imputate_na(carseats, Income, US, method = "rpart")

  # result of imputation
  income

  # summary of imputation
  summary(income)

  # viz of imputation
  plot(income)
} else {
  cat("If you want to use this feature, you need to install the rpart package.\n")
}

## ----imputate_na2, fig.align='center', fig.width = 7, fig.height = 5----------
library(mice)

urban <- imputate_na(carseats, Urban, US, method = "mice")

# result of imputation
urban

# summary of imputation
summary(urban)

# viz of imputation
plot(urban)

## ----imputate_na3-------------------------------------------------------------
# The mean before and after the imputation of the Income variable
carseats %>%
  mutate(Income_imp = imputate_na(carseats, Income, US, method = "knn")) %>%
  group_by(US) %>%
  summarise(orig = mean(Income, na.rm = TRUE),
            imputation = mean(Income_imp))

## ----imputate_outlier, fig.align='center', fig.width = 7, fig.height = 5------
price <- imputate_outlier(carseats, Price, method = "capping")

# result of imputation
price

# summary of imputation
summary(price)

# viz of imputation
plot(price)

## ----imputate_outlier2--------------------------------------------------------
# The mean before and after the imputation of the Price variable
carseats %>%
  mutate(Price_imp = imputate_outlier(carseats, Price, method = "capping")) %>%
  group_by(US) %>%
  summarise(orig = mean(Price, na.rm = TRUE),
    imputation = mean(Price_imp, na.rm = TRUE))

## ----standardization, fig.align='center', fig.width = 7, fig.height = 5-------
carseats %>% 
  mutate(Income_minmax = transform(carseats$Income, method = "minmax"),
    Sales_minmax = transform(carseats$Sales, method = "minmax")) %>% 
  select(Income_minmax, Sales_minmax) %>% 
  boxplot()

## ----resolving1---------------------------------------------------------------
# find index of skewed variables
find_skewness(carseats)

# find names of skewed variables
find_skewness(carseats, index = FALSE)

# compute the skewness
find_skewness(carseats, value = TRUE)

# compute the skewness & filtering with threshold
find_skewness(carseats, value = TRUE, thres = 0.1)

## ----resolving2, fig.align='center', fig.width = 7, fig.height = 5------------
Advertising_log <- transform(carseats$Advertising, method = "log")

# result of transformation
head(Advertising_log)
# summary of transformation
summary(Advertising_log)
# viz of transformation
plot(Advertising_log)

## ----resolving3, fig.align='center', fig.width = 7, fig.height = 5------------
Advertising_log <- transform(carseats$Advertising, method = "log+1")

# result of transformation
head(Advertising_log)
# summary of transformation
summary(Advertising_log)
# viz of transformation
# plot(Advertising_log)

## ----binning, fig.width = 7, fig.height = 5-----------------------------------
# Binning the carat variable. default type argument is "quantile"
bin <- binning(carseats$Income)
# Print bins class object
bin
# Summarize bins class object
summary(bin)
# Plot bins class object
plot(bin)
# Using labels argument
bin <- binning(carseats$Income, nbins = 4,
              labels = c("LQ1", "UQ1", "LQ3", "UQ3"))
bin
# Using another type argument
binning(carseats$Income, nbins = 5, type = "equal")
binning(carseats$Income, nbins = 5, type = "pretty")

if (requireNamespace("classInt", quietly = TRUE)) {
  binning(carseats$Income, nbins = 5, type = "kmeans")
  binning(carseats$Income, nbins = 5, type = "bclust")
} else {
  cat("If you want to use this feature, you need to install the classInt package.\n")
}

# Extract the binned results
extract(bin)

# -------------------------
# Using pipes & dplyr
# -------------------------
library(dplyr)

carseats %>%
 mutate(Income_bin = binning(carseats$Income) %>% 
                     extract()) %>%
 group_by(ShelveLoc, Income_bin) %>%
 summarise(freq = n()) %>%
 arrange(desc(freq)) %>%
 head(10)

## ----binning_by, fig.width = 7, fig.height = 5--------------------------------
library(dplyr)

# optimal binning using character
bin <- binning_by(carseats, "US", "Advertising")

# optimal binning using name
bin <- binning_by(carseats, US, Advertising)
bin

# summary optimal_bins class
summary(bin)

# performance table
attr(bin, "performance")

# visualize optimal_bins class
plot(bin)

# extract binned results
extract(bin) %>% 
  head(20)

## ----trans_web_report, eval=FALSE---------------------------------------------
#  heartfailure %>%
#    transformation_web_report(target = "death_event", subtitle = "heartfailure",
#                              output_dir = "./", output_file = "transformation.html",
#                              theme = "blue")

## ----trans_web_title, echo=FALSE, out.width='80%', fig.align='center', fig.pos="!h", fig.cap="The part of the report"----
knitr::include_graphics('img/transformation_web_title.jpg')

## ----trans_paged_report, eval=FALSE-------------------------------------------
#  heartfailure %>%
#    transformation_paged_report(target = "death_event", subtitle = "heartfailure",
#                                output_dir = "./", output_file = "transformation.pdf",
#                                theme = "blue")

## ----trans_paged_cover, echo=FALSE, out.width='80%', fig.align='center', fig.pos="!h", fig.cap="The part of the report"----
knitr::include_graphics('img/transformation_paged_cover.jpg')

## ----trans_paged_cntent, echo=FALSE, out.width='80%', fig.align='center', fig.pos="!h", fig.cap="The dynamic contents of the report"----
knitr::include_graphics('img/transformation_paged_content.jpg')

