## ----environment, echo = FALSE, message = FALSE, warning=FALSE-----------
knitr::opts_chunk$set(collapse = TRUE, comment = "")
options(tibble.print_min = 4L, tibble.print_max = 4L)

library(dlookr)
library(dplyr)
library(ggplot2)

## ----import_data---------------------------------------------------------
library(ISLR)
str(Carseats)

## ----missing-------------------------------------------------------------
carseats <- ISLR::Carseats

suppressWarnings(RNGversion("3.5.0"))
set.seed(123)
carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA

suppressWarnings(RNGversion("3.5.0"))
set.seed(456)
carseats[sample(seq(NROW(carseats)), 10), "Urban"] <- NA

## ----imputate_na, fig.width = 7, fig.height = 5--------------------------
income <- imputate_na(carseats, Income, US, method = "rpart")

# result of imputate
income

# summary of imputate
summary(income)

# viz of imputate
plot(income)

## ----imputate_na2, fig.width = 7, fig.height = 5-------------------------
library(mice)

urban <- imputate_na(carseats, Urban, US, method = "mice")

# result of imputate
urban

# summary of imputate
summary(urban)

# viz of imputate
plot(urban)

## ----imputate_na3--------------------------------------------------------
# The mean before and after the imputation of the Income variable
carseats %>%
  mutate(Income_imp = imputate_na(carseats, Income, US, method = "knn")) %>%
  group_by(US) %>%
  summarise(orig = mean(Income, na.rm = TRUE),
    imputation = mean(Income_imp))

## ----imputate_outlier, fig.width = 7, fig.height = 5---------------------
price <- imputate_outlier(carseats, Price, method = "capping")

# result of imputate
price

# summary of imputate
summary(price)

# viz of imputate
plot(price)

## ----imputate_outlier2---------------------------------------------------
# The mean before and after the imputation of the Price variable
carseats %>%
  mutate(Price_imp = imputate_outlier(carseats, Price, method = "capping")) %>%
  group_by(US) %>%
  summarise(orig = mean(Price, na.rm = TRUE),
    imputation = mean(Price_imp, na.rm = TRUE))

## ----standardization, fig.width = 7, fig.height = 5----------------------
carseats %>% 
  mutate(Income_minmax = transform(carseats$Income, method = "minmax"),
    Sales_minmax = transform(carseats$Sales, method = "minmax")) %>% 
  select(Income_minmax, Sales_minmax) %>% 
  boxplot()

## ----resolving1----------------------------------------------------------
# find index of skewed variables
find_skewness(carseats)

# find names of skewed variables
find_skewness(carseats, index = FALSE)

# compute the skewness
find_skewness(carseats, value = TRUE)

# compute the skewness & filtering with threshold
find_skewness(carseats, value = TRUE, thres = 0.1)

## ----resolving2, fig.width = 7, fig.height = 5---------------------------
Advertising_log = transform(carseats$Advertising, method = "log")

# result of transformation
head(Advertising_log)
# summary of transformation
summary(Advertising_log)
# viz of transformation
plot(Advertising_log)

## ----resolving3, fig.width = 7, fig.height = 5---------------------------
Advertising_log <- transform(carseats$Advertising, method = "log+1")

# result of transformation
head(Advertising_log)
# summary of transformation
summary(Advertising_log)
# viz of transformation
plot(Advertising_log)

## ----binning, fig.width = 6, fig.height = 5------------------------------
# Binning the carat variable. default type argument is "quantile"
bin <- binning(carseats$Income)
# Print bins class object
bin
# Summarise bins class object
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
binning(carseats$Income, nbins = 5, type = "kmeans")
binning(carseats$Income, nbins = 5, type = "bclust")

# -------------------------
# Using pipes & dplyr
# -------------------------
library(dplyr)

carseats %>%
 mutate(Income_bin = binning(carseats$Income)) %>%
 group_by(ShelveLoc, Income_bin) %>%
 summarise(freq = n()) %>%
 arrange(desc(freq)) %>%
 head(10)

## ----binning_by, fig.width = 6, fig.height = 5---------------------------
# optimal binning
bin <- binning_by(carseats, "US", "Advertising")
bin

# summary optimal_bins class
summary(bin)

# information value 
attr(bin, "iv")

# information value table
attr(bin, "ivtable")

# visualize optimal_bins class
plot(bin, sub = "bins of Advertising variable")

## ----trans_report, eval=FALSE--------------------------------------------
#  carseats %>%
#    transformation_report(target = US)

## ---- eval=FALSE---------------------------------------------------------
#  carseats %>%
#    transformation_report(target = US, output_format = "html",
#      output_file = "transformation.html")

## ----trans_title_pdf, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="Data transformation report cover"----
knitr::include_graphics('img/trans_title_pdf.png')

## ----trans_agenda_pdf, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="Table of Contents"----
knitr::include_graphics('img/trans_agenda_pdf.png')

## ----trans_content_pdf, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="Data Transformation Report Table and Visualization Example"----
knitr::include_graphics('img/trans_content_pdf.png')

## ----trans_agenda_html, echo=FALSE, out.width='80%', fig.align='center', fig.pos="!h", fig.cap="Data transformation report titles and table of contents"----
knitr::include_graphics('img/trans_agenda_html.png')

## ----trans_table_html, echo=FALSE, out.width='50%', fig.align='center', fig.pos="!h", fig.cap="Report table example (Web)"----
knitr::include_graphics('img/trans_table_html.png')

## ----trans_viz_html, echo=FALSE, out.width='75%', fig.align='center', fig.pos="!h", fig.cap="Data transformation report Binning information (web)"----
knitr::include_graphics('img/trans_viz_html.png')

