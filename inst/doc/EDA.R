## ----environment, echo = FALSE, message = FALSE, warning=FALSE----------------
knitr::opts_chunk$set(collapse = TRUE, comment = "", out.width = "600px", dpi = 70)
options(tibble.print_min = 4L, tibble.print_max = 4L)

library(dlookr)
library(dplyr)
library(ggplot2)

## ----import_data, warning=FALSE-----------------------------------------------
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

## ----describe-----------------------------------------------------------------
describe(carseats)

## ----describes2---------------------------------------------------------------
# Select columns by name
describe(carseats, Sales, CompPrice, Income)
# Select all columns between year and day (include)
describe(carseats, Sales:Income)
# Select all columns except those from year to day (exclude)
describe(carseats, -(Sales:Income))

## ----describe_pipe------------------------------------------------------------
carseats %>%
  describe() %>%
  select(variable, skewness, mean, p25, p50, p75) %>% 
  filter(!is.na(skewness)) %>% 
  arrange(desc(abs(skewness)))

## ----diagnose_pipe2-----------------------------------------------------------
carseats %>%
  group_by(US) %>% 
  describe(Sales, Income) 

## ----diagnose_pipe3-----------------------------------------------------------
carseats %>%
  group_by(US, Urban) %>% 
  describe(Sales, Income) 

## ----normality----------------------------------------------------------------
normality(carseats)

## ----normality2---------------------------------------------------------------
# Select columns by name
normality(carseats, Sales, CompPrice, Income)

# Select all columns between year and day (inclusive)
normality(carseats, Sales:Income)

# Select all columns except those from year to day (inclusive)
normality(carseats, -(Sales:Income))

## ----normality_pipe-----------------------------------------------------------
library(dplyr)

carseats %>%
  normality() %>%
  filter(p_value <= 0.01) %>% 
  arrange(abs(p_value))

## ----normality_pipe2----------------------------------------------------------
carseats %>%
  group_by(ShelveLoc, US) %>%
  normality(Income) %>% 
  arrange(desc(p_value))

## ----normality_pipe3----------------------------------------------------------
carseats %>%
  mutate(log_income = log(Income)) %>%
  group_by(ShelveLoc, US) %>%
  normality(log_income) %>%
  filter(p_value > 0.01)

## ----plot_normality, fig.align='center', fig.width = 6, fig.height = 4--------
# Select columns by name
plot_normality(carseats, Sales, CompPrice)

## ----plot_normality2, fig.align='center', fig.width = 6, fig.height = 4-------
carseats %>%
  filter(ShelveLoc == "Good") %>%
  group_by(US) %>%
  plot_normality(Income)

## ----correlate----------------------------------------------------------------
correlate(carseats)

## ----correlate2---------------------------------------------------------------
# Select columns by name
correlate(carseats, Sales, CompPrice, Income)

# Select all columns between year and day (include)
correlate(carseats, Sales:Income)

# Select all columns except those from year to day (exclude)
correlate(carseats, -(Sales:Income))

## ----correlate3---------------------------------------------------------------
carseats %>%
  correlate(Sales:Income) %>%
  filter(as.integer(var1) > as.integer(var2))

## ----correlate4---------------------------------------------------------------
carseats %>%
  filter(ShelveLoc == "Good") %>%
  group_by(Urban, US) %>%
  correlate(Sales) %>%
  filter(abs(coef_corr) > 0.5)

## ----plot_correlate, fig.align='center', fig.width = 6, fig.height = 4--------
plot_correlate(carseats)

## ----plot_correlate2, fig.align='center', fig.width = 6, fig.height = 4-------
# Select columns by name
plot_correlate(carseats, Sales, Price)

## ----plot_correlate3, fig.align='center', fig.width = 6, fig.height = 4, warning=FALSE----
carseats %>%
  filter(ShelveLoc == "Good") %>%
  group_by(Urban, US) %>%
  plot_correlate(Sales)

## ----target_by----------------------------------------------------------------
categ <- target_by(carseats, US)

## ----target_by2---------------------------------------------------------------
# If the variable of interest is a numerical variable
cat_num <- relate(categ, Sales)
cat_num
summary(cat_num)

## ----target_by3, fig.align='center', fig.width = 6, fig.height = 4, warning=FALSE----
plot(cat_num)

## ----target_by4---------------------------------------------------------------
# If the variable of interest is a categorical variable
cat_cat <- relate(categ, ShelveLoc)
cat_cat
summary(cat_cat)

## ----target_by5, fig.align='center', fig.width = 6, fig.height = 4, warning=FALSE----
plot(cat_cat)

## ----target_by6---------------------------------------------------------------
# If the variable of interest is a numerical variable
num <- target_by(carseats, Sales)

## ----target_by7---------------------------------------------------------------
# If the variable of interest is a numerical variable
num_num <- relate(num, Price)
num_num
summary(num_num)

## ----target_by8, fig.align='center', fig.width = 6, fig.height = 4, warning=FALSE----
plot(num_num)

## ----target_by8_2, fig.align='center', fig.width = 6, fig.height = 4, warning=FALSE----
plot(num_num, hex_thres = 350)

## ----target_by9---------------------------------------------------------------
# If the variable of interest is a categorical variable
num_cat <- relate(num, ShelveLoc)
num_cat
summary(num_cat)

## ----target_by10, fig.align='center', fig.width = 6, fig.height = 4, warning=FALSE----
plot(num_cat)

## ----eda_report, eval=FALSE---------------------------------------------------
#  carseats %>%
#    eda_report(target = Sales)

## ---- eval=FALSE--------------------------------------------------------------
#  carseats %>%
#    eda_report(target = Sales, output_format = "html", output_file = "EDA_carseats.html")

## ----eda_title_pdf, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="EDA report cover"----
knitr::include_graphics('img/eda_title_pdf.png')

## ----eda_agenda_pdf, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="EDA Report Contents"----
knitr::include_graphics('img/eda_agenda_pdf.png')

## ----eda_intro_pdf, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="Example EDA report table"----
knitr::include_graphics('img/eda_intro_pdf.png')

## ----eda_normality_pdf, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="Normality test information in EDA reports"----
knitr::include_graphics('img/eda_normality_pdf.png')

## ----eda_correlation_pdf, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="Correlation information in EDA reports"----
knitr::include_graphics('img/eda_correlation_pdf.png')

## ----eda_lm_pdf, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="Linear relationship information in EDA reports"----
knitr::include_graphics('img/eda_lm_pdf.png')

## ----eda_anova_pdf, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="Information about ANOVA in EDA reports"----
knitr::include_graphics('img/eda_anova_pdf.png')

## ----eda_egenda_html, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="EDA report titles and table of contents"----
knitr::include_graphics('img/eda_agenda_html.png')

## ----eda_table_html, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="EDA report table example (Web)"----
knitr::include_graphics('img/eda_table_html.png')

## ----eda_normality_html, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="EDA Report Normality Test Information (Web)"----
knitr::include_graphics('img/eda_normality_html.png')

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

## ----dbi_describe-------------------------------------------------------------
# Positive values select variables
con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  describe(Sales, CompPrice, Income)

# Negative values to drop variables, and In-memory mode and collect size is 200
con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  describe(-Sales, -CompPrice, -Income, collect_size = 200)

# Find the statistic of all numerical variables by 'ShelveLoc' and 'US',
# and extract only those with 'ShelveLoc' variable level is "Good".
con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  group_by(ShelveLoc, US) %>%
  describe() %>%
  filter(ShelveLoc == "Good")

# extract only those with 'Urban' variable level is "Yes",
# and find 'Sales' statistics by 'ShelveLoc' and 'US'
con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  filter(Urban == "Yes") %>%
  group_by(ShelveLoc, US) %>%
  describe(Sales)

## ----dbi_normality------------------------------------------------------------
# Test all numerical variables by 'ShelveLoc' and 'US',
# and extract only those with 'ShelveLoc' variable level is "Good".
con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
 group_by(ShelveLoc, US) %>%
 normality() %>%
 filter(ShelveLoc == "Good")

# extract only those with 'Urban' variable level is "Yes",
# and test 'Sales' by 'ShelveLoc' and 'US'
con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
 filter(Urban == "Yes") %>%
 group_by(ShelveLoc, US) %>%
 normality(Sales)

# Test log(Income) variables by 'ShelveLoc' and 'US',
# and extract only p.value greater than 0.01.

# SQLite extension functions for log transformation
RSQLite::initExtension(con_sqlite)

con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
 mutate(log_income = log(Income)) %>%
 group_by(ShelveLoc, US) %>%
 normality(log_income) %>%
 filter(p_value > 0.01)

## ----plot_normality_dbi, fig.align='center', fig.width = 6, fig.height = 4----
# Plot 'Sales' variable by 'ShelveLoc' and 'US'
con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  group_by(ShelveLoc, US) %>%
  plot_normality(Sales)

# extract only those with 'ShelveLoc' variable level is "Good",
# and plot 'Income' by 'US'
con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  filter(ShelveLoc == "Good") %>%
  group_by(US) %>%
  plot_normality(Income)

## ----dbi_correlation----------------------------------------------------------
# Correlation coefficient
# that eliminates redundant combination of variables
con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  correlate() %>%
  filter(as.integer(var1) > as.integer(var2))

con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  correlate(Sales, Price) %>%
  filter(as.integer(var1) > as.integer(var2))

# Compute the correlation coefficient of Sales variable by 'ShelveLoc'
# and 'US' variables. And extract only those with absolute
# value of correlation coefficient is greater than 0.5
con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  group_by(ShelveLoc, US) %>%
  correlate(Sales) %>%
  filter(abs(coef_corr) >= 0.5)

# extract only those with 'ShelveLoc' variable level is "Good",
# and compute the correlation coefficient of 'Sales' variable
# by 'Urban' and 'US' variables.
# And the correlation coefficient is negative and smaller than 0.5
con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  filter(ShelveLoc == "Good") %>%
  group_by(Urban, US) %>%
  correlate(Sales) %>%
  filter(coef_corr < 0) %>%
  filter(abs(coef_corr) > 0.5)

## ----plot_correlation_dbi, fig.align='center', fig.width = 6, fig.height = 4----
# Extract only those with 'ShelveLoc' variable level is "Good",
# and visualize correlation plot of 'Sales' variable by 'Urban'
# and 'US' variables.
con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  filter(ShelveLoc == "Good") %>%
  group_by(Urban, US) %>%
  plot_correlate(Sales)

## ----dbi_ctarget_by-----------------------------------------------------------
# If the target variable is a categorical variable
categ <- target_by(con_sqlite %>% tbl("TB_CARSEATS") , US)

# If the variable of interest is a numarical variable
cat_num <- relate(categ, Sales)
cat_num
summary(cat_num)

## ----plot_target_by_dbi, fig.align='center', fig.align='center', fig.width = 6, fig.height = 4----
plot(cat_num)

## ----dbi_diag_report, eval=FALSE----------------------------------------------
#  ## target variable is categorical variable
#  # reporting the EDA information
#  # create pdf file. file name is EDA_Report.pdf
#  con_sqlite %>%
#    tbl("TB_CARSEATS") %>%
#    eda_report(US)
#  
#  # create pdf file. file name is EDA_TB_CARSEATS.pdf
#  con_sqlite %>%
#    tbl("TB_CARSEATS") %>%
#    eda_report("US", output_file = "EDA_TB_CARSEATS.pdf")
#  
#  # create html file. file name is EDA_Report.html
#  con_sqlite %>%
#    tbl("TB_CARSEATS") %>%
#    eda_report("US", output_format = "html")
#  
#  # create html file. file name is EDA_TB_CARSEATS.html
#  con_sqlite %>%
#    tbl("TB_CARSEATS") %>%
#    eda_report(US, output_format = "html", output_file = "EDA_TB_CARSEATS.html")
#  
#  ## target variable is numerical variable
#  # reporting the EDA information, and collect size is 350
#  con_sqlite %>%
#    tbl("TB_CARSEATS") %>%
#    eda_report(Sales, collect_size = 350)
#  
#  # create pdf file. file name is EDA2.pdf
#  con_sqlite %>%
#    tbl("TB_CARSEATS") %>%
#    eda_report("Sales", output_file = "EDA2.pdf")
#  
#  # create html file. file name is EDA_Report.html
#  con_sqlite %>%
#    tbl("TB_CARSEATS") %>%
#    eda_report("Sales", output_format = "html")
#  
#  # create html file. file name is EDA2.html
#  con_sqlite %>%
#    tbl("TB_CARSEATS") %>%
#    eda_report(Sales, output_format = "html", output_file = "EDA2.html")
#  
#  ## target variable is null
#  # reporting the EDA information
#  con_sqlite %>%
#    tbl("TB_CARSEATS") %>%
#    eda_report()
#  
#  # create pdf file. file name is EDA2.pdf
#  con_sqlite %>%
#    tbl("TB_CARSEATS") %>%
#    eda_report(output_file = "EDA2.pdf")
#  
#  # create html file. file name is EDA_Report.html
#  con_sqlite %>%
#    tbl("TB_CARSEATS") %>%
#    eda_report(output_format = "html")
#  
#  # create html file. file name is EDA2.html
#  con_sqlite %>%
#    tbl("TB_CARSEATS") %>%
#    eda_report(output_format = "html", output_file = "EDA2.html")

