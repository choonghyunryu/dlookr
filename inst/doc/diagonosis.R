## ----environment, echo = FALSE, message = FALSE, warning=FALSE----------------
knitr::opts_chunk$set(collapse = TRUE, comment = "", out.width = "600px", dpi = 70)
options(tibble.print_min = 4L, tibble.print_max = 4L)

library(dlookr)
library(dplyr)
library(ggplot2)

## ----import_data, warning=FALSE-----------------------------------------------
library(nycflights13)
dim(flights)
flights

## ----diagnose-----------------------------------------------------------------
diagnose(flights)

## ----diagnoses----------------------------------------------------------------
# Select columns by name
diagnose(flights, year, month, day)
# Select all columns between year and day (include)
diagnose(flights, year:day)
# Select all columns except those from year to day (exclude)
diagnose(flights, -(year:day))

## ----diagnose_pipe------------------------------------------------------------
flights %>%
  diagnose() %>%
  select(-unique_count, -unique_rate) %>% 
  filter(missing_count > 0) %>% 
  arrange(desc(missing_count))

## ----diagnose_pipe_numeric----------------------------------------------------
diagnose_numeric(flights)

## ----diagnose_pipe_numeric_pipe-----------------------------------------------
diagnose_numeric(flights) %>% 
  filter(minus > 0 | zero > 0) 

## ----diagnose_category--------------------------------------------------------
diagnose_category(flights)

## ----diagnose_category_pipe---------------------------------------------------
diagnose_category(flights) %>% 
  filter(is.na(levels))

## ----diagnose_category_pipe2--------------------------------------------------
flights %>%
  diagnose_category(top = 500)  %>%
  filter(ratio <= 0.01)

## ----diagnose_outlier---------------------------------------------------------
diagnose_outlier(flights)

## ----diagnose_outlier_pipe----------------------------------------------------
diagnose_outlier(flights) %>% 
  filter(outliers_cnt > 0) 

## ----diagnose_outlier_pipe2---------------------------------------------------
diagnose_outlier(flights) %>% 
  filter(outliers_ratio > 5) %>% 
  mutate(rate = outliers_mean / with_mean) %>% 
  arrange(desc(rate)) %>% 
  select(-outliers_cnt)

## ----plot_outlier, fig.align='center', fig.width = 7, fig.height = 5----------
flights %>%
  plot_outlier(arr_delay) 

## ----plot_outlier_pipe, fig.align='center', fig.width = 7, fig.height = 5, eval=FALSE----
#  flights %>%
#    plot_outlier(diagnose_outlier(flights) %>%
#                   filter(outliers_ratio >= 5) %>%
#                   select(variables) %>%
#                   unlist())

## ----plot_na_pareto1, fig.align='center', fig.width = 7, fig.height = 5-------
mice::boys %>% 
  plot_na_pareto(col = "blue")

## ----plot_na_pareto2, fig.align='center', fig.width = 7, fig.height = 5, eval=FALSE----
#  mice::boys %>%
#    plot_na_pareto(only_na = TRUE, main = "Pareto Chart for mice::boys")

## ----plot_na_pareto3, fig.align='center', fig.width = 7, fig.height = 5, eval=FALSE----
#  mice::boys %>%
#    plot_na_pareto(grade = list(High = 0.1, Middle = 0.6, Low = 1), relative = TRUE)

## ----plot_na_pareto4, fig.align='center', fig.width = 7, fig.height = 5, eval=FALSE----
#  plot_na_pareto(mice::boys, only_na = TRUE, plot = FALSE)

## ----plot_na_hclust, fig.align='center', fig.width = 7, fig.height = 5--------
mice::boys %>% 
  plot_na_hclust(main = "Distribution of missing value")

## ----plot_na_hclust1, fig.align='center', fig.width = 7, fig.height = 5-------
mice::boys %>% 
  plot_na_intersect()

## ----plot_na_hclust3, fig.align='center', fig.width = 7, fig.height = 5, eval=FALSE----
#  mice::boys %>%
#    plot_na_intersect(n_vars = 5)

## ----plot_na_hclust4, fig.align='center', fig.width = 7, fig.height = 5, eval=FALSE----
#  mice::boys %>%
#    plot_na_intersect(only_na = FALSE, n_intersacts = 7)

## ----diagnose_web_report, eval=FALSE------------------------------------------
#  flights %>%
#    diagnose_web_report(subtitle = "flights", output_dir = "./",
#                        output_file = "Diagn.html", theme = "blue")

## ----diag_web_title, echo=FALSE, out.width='80%', fig.align='center', fig.pos="!h", fig.cap="The part of the report"----
knitr::include_graphics('img/diag_web_title.jpg')

## ----diag_web_content, echo=FALSE, out.width='80%', fig.align='center', fig.pos="!h", fig.cap="The dynamic contents of the report"----
knitr::include_graphics('img/diag_web_content.jpg')

## ----diagnose_paged_report, eval=FALSE----------------------------------------
#  flights %>%
#    diagnose_paged_report(subtitle = "flights", output_dir = "./",
#                          output_file = "Diagn.pdf", theme = "blue")

## ----diag_paged_cover, echo=FALSE, out.width='80%', fig.align='center', fig.pos="!h", fig.cap="The part of the report"----
knitr::include_graphics('img/diag_paged_cover.jpg')

## ----diag_paged_cntent, echo=FALSE, out.width='80%', fig.align='center', fig.pos="!h", fig.cap="The dynamic contents of the report"----
knitr::include_graphics('img/diag_paged_content.jpg')

## ----dbi_table, warning=FALSE, message=FALSE----------------------------------
if (!require(DBI)) install.packages('DBI')
if (!require(RSQLite)) install.packages('RSQLite')
if (!require(dplyr)) install.packages('dplyr')
if (!require(dbplyr)) install.packages('dbplyr')

library(dplyr)

carseats <- ISLR::Carseats
carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA

# connect DBMS
con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

# copy carseats to the DBMS with a table named TB_CARSEATS
copy_to(con_sqlite, carseats, name = "TB_CARSEATS", overwrite = TRUE)

## ----dbi_diag-----------------------------------------------------------------
# Diagnosis of all columns
con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  diagnose()

# Positions values select columns, and In-memory mode
con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  diagnose(1, 3, 8, in_database = FALSE)
  
# Positions values select columns, and In-memory mode and collect size is 200
con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  diagnose(-8, -9, -10, in_database = FALSE, collect_size = 200)

## ----dbi_category-------------------------------------------------------------
# Positions values select variables, and In-memory mode and collect size is 200
con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  diagnose_category(7, in_database = FALSE, collect_size = 200) 
  
# Positions values select variables
con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  diagnose_category(-7)

## ----dbi_numeric--------------------------------------------------------------
# Diagnosis of all numerical variables
con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  diagnose_numeric()
  
# Positive values select variables, and In-memory mode and collect size is 200
con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  diagnose_numeric(Sales, Income, collect_size = 200)

## ----dbi_outlier--------------------------------------------------------------
con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  diagnose_outlier()  %>%
  filter(outliers_ratio > 1)

## ----plot_outlier_dbi, fig.align='center', fig.width = 6, fig.height = 4, eval=FALSE----
#  # Visualization of numerical variables with a ratio of
#  # outliers greater than 1%
#  # the result is same as a data.frame, but not display here. reference above in document.
#  con_sqlite %>%
#    tbl("TB_CARSEATS") %>%
#    plot_outlier(con_sqlite %>%
#                   tbl("TB_CARSEATS") %>%
#                   diagnose_outlier() %>%
#                   filter(outliers_ratio > 1) %>%
#                   select(variables) %>%
#                   pull())

## ----dbi_diag_report, eval=FALSE----------------------------------------------
#  # create web report file.
#  con_sqlite %>%
#    tbl("TB_CARSEATS") %>%
#    diagnose_web_report()
#  
#  # create pdf file. file name is Diagn.pdf, and collect size is 350
#  con_sqlite %>%
#    tbl("TB_CARSEATS") %>%
#    diagnose_paged_report(collect_size = 350, output_file = "Diagn.pdf")

