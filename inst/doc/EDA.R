## ----environment, echo = FALSE, message = FALSE, warning=FALSE----------------
knitr::opts_chunk$set(collapse = TRUE, comment = "", out.width = "600px", dpi = 70)
options(tibble.print_min = 4L, tibble.print_max = 4L)

library(dlookr)
library(dplyr)
library(ggplot2)

## ----import_data, warning=FALSE-----------------------------------------------
str(medicost)

## ----missing------------------------------------------------------------------
medicosts <- medicost

suppressWarnings(RNGversion("3.5.0"))
set.seed(123)
medicosts[sample(seq(NROW(medicosts)), 20), "bmi"] <- NA

suppressWarnings(RNGversion("3.5.0"))
set.seed(456)
medicosts[sample(seq(NROW(medicosts)), 10), "smoker"] <- NA

## ----describe-----------------------------------------------------------------
describe(medicosts)

## ----describes2---------------------------------------------------------------
# Select columns by name
describe(medicosts, bmi, children, charges)
# Select all columns between year and day (include)
describe(medicosts, bmi:charges)
# Select all columns except those from year to day (exclude)
describe(medicosts, -(bmi:charges))

## ----describe_pipe------------------------------------------------------------
medicosts %>%
  describe() %>%
  select(described_variables, skewness, mean, p25, p50, p75) %>% 
  filter(!is.na(skewness)) %>% 
  arrange(desc(abs(skewness)))

## ----describe_pipe2-----------------------------------------------------------
medicosts %>%
  group_by(sex) %>% 
  describe(bmi, charges) 

## ----describe_pipe3-----------------------------------------------------------
medicosts %>%
  group_by(sex, smoker) %>% 
  describe(bmi, charges) 

## ----normality----------------------------------------------------------------
normality(medicosts)

## ----normality2---------------------------------------------------------------
# Select columns by name
normality(medicosts, bmi, children, charges)

# Select all columns between year and day (inclusive)
normality(medicosts, bmi:charges)

# Select all columns except those from year to day (inclusive)
normality(medicosts, -(bmi:charges))

## ----normality_pipe-----------------------------------------------------------
library(dplyr)

medicosts %>%
  normality() %>%
  filter(p_value <= 0.01) %>% 
  arrange(abs(p_value))

## ----normality_pipe2----------------------------------------------------------
medicosts %>%
  filter(!is.na(smoker)) %>% 
  group_by(sex, smoker) %>%
  normality(bmi) %>% 
  arrange(desc(p_value))

## ----normality_pipe3----------------------------------------------------------
medicosts %>%
  filter(!is.na(smoker)) %>%   
  mutate(log_bmi = log(bmi)) %>%
  group_by(sex, smoker) %>%
  normality(log_bmi) %>%
  filter(p_value > 0.05)

## ----plot_normality, fig.align='center', fig.width = 7, fig.height = 5--------
# Select columns by name
plot_normality(medicosts, bmi, charges)

## ----plot_normality2, fig.align='center', fig.width = 7, fig.height = 5, eval=FALSE----
#  medicosts %>%
#    filter(sex == "male") %>%
#    group_by(smoker) %>%
#    plot_normality(charges)

## ----correlate----------------------------------------------------------------
correlate(medicosts)

## ----correlate2---------------------------------------------------------------
# Select columns by name
correlate(medicosts, bmi, children, charges)

# Select all columns between year and day (include)
correlate(medicosts, bmi:charges)

# Select all columns except those from year to day (exclude)
correlate(medicosts, -(bmi:charges))

## ----correlate3---------------------------------------------------------------
medicosts %>%
  correlate(bmi:charges) %>%
  filter(as.integer(var1) > as.integer(var2))

## ----correlate4---------------------------------------------------------------
tab_corr <- medicosts %>%
  filter(region == "northwest") %>%
  group_by(sex, smoker) %>%
  correlate(charges) %>%
  filter(abs(coef_corr) > 0.5)

tab_corr

## ----plot_correlate, fig.align='center', fig.width = 7, fig.height = 5--------
medicosts %>% 
  correlate() %>% 
  plot()

## ----plot_correlate2, fig.align='center', fig.width = 6, fig.height = 4, eval=TRUE----
# Select columns by name
correlate(medicosts, bmi, charges) %>% 
  plot()

## ----plot_correlate3, fig.align='center', fig.width = 6, fig.height = 4, warning=FALSE, eval=TRUE----
medicosts %>%
  filter(region == "northwest") %>%
  group_by(smoker) %>%
  correlate() %>%
  plot() 

## ----target_by----------------------------------------------------------------
categ <- target_by(medicosts, smoker)

## ----target_by2---------------------------------------------------------------
# If the variable of interest is a numerical variable
cat_num <- relate(categ, charges)
cat_num
summary(cat_num)

## ----target_by3, fig.align='center', fig.width = 7, fig.height = 5, warning=FALSE----
plot(cat_num)

## ----target_by4---------------------------------------------------------------
# If the variable of interest is a categorical variable
cat_cat <- relate(categ, sex)
cat_cat
summary(cat_cat)

## ----target_by5, fig.align='center', fig.width = 7, fig.height = 5, warning=FALSE----
plot(cat_cat)

## ----target_by6---------------------------------------------------------------
# If the variable of interest is a numerical variable
num <- medicosts %>% 
  filter(!is.na(bmi)) %>% 
  target_by(charges)

## ----target_by7---------------------------------------------------------------
# If the variable of interest is a numerical variable
num_num <- relate(num, bmi)
num_num
summary(num_num)

## ----target_by8, fig.align='center', fig.width = 7, fig.height = 5, warning=FALSE----
plot(num_num)

## ----target_by8_2, fig.align='center', fig.width = 7, fig.height = 5, warning=FALSE----
plot(num_num, hex_thres = 350)

## ----target_by9---------------------------------------------------------------
# If the variable of interest is a categorical variable
num_cat <- relate(num, smoker)
num_cat
summary(num_cat)

## ----target_by10, fig.align='center', fig.width = 7, fig.height = 5, warning=FALSE----
plot(num_cat)

## ----eda_web_report, eval=FALSE-----------------------------------------------
#  heartfailure %>%
#    eda_web_report(target = "death_event", subtitle = "heartfailure",
#                   output_dir = "./", output_file = "EDA.html", theme = "blue")

## ----eda_web_title, echo=FALSE, out.width='80%', fig.align='center', fig.pos="!h", fig.cap="The part of the report"----
knitr::include_graphics('img/eda_web_title.jpg')

## ----eda_paged_report, eval=FALSE---------------------------------------------
#  heartfailure %>%
#    eda_paged_report(target = "death_event", subtitle = "heartfailure",
#                     output_dir = "./", output_file = "EDA.pdf", theme = "blue")

## ----eda_paged_cover, echo=FALSE, out.width='80%', fig.align='center', fig.pos="!h", fig.cap="The part of the report"----
knitr::include_graphics('img/eda_paged_cover.jpg')

## ----eda_paged_cntent, echo=FALSE, out.width='80%', fig.align='center', fig.pos="!h", fig.cap="The dynamic contents of the report"----
knitr::include_graphics('img/eda_paged_content.jpg')

## ----dbi_table, warning=FALSE, message=FALSE, eval=FALSE----------------------
#  if (!require(DBI)) install.packages('DBI', repos = "http://cran.us.r-project.org")
#  if (!require(RSQLite)) install.packages('RSQLite', repos = "http://cran.us.r-project.org")
#  if (!require(dplyr)) install.packages('dplyr', repos = "http://cran.us.r-project.org")
#  if (!require(dbplyr)) install.packages('dbplyr', repos = "http://cran.us.r-project.org")
#  
#  library(dplyr)
#  
#  medicosts <- medicost
#  
#  # connect DBMS
#  con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#  
#  # copy medicosts to the DBMS with a table named TB_MEDICOST
#  copy_to(con_sqlite, medicosts, name = "TB_MEDICOST", overwrite = TRUE)

## ----dbi_describe, eval=FALSE-------------------------------------------------
#  # Positive values select variables
#  con_sqlite %>%
#    tbl("TB_MEDICOST") %>%
#    describe(age, bmi, charges)
#  
#  # Negative values to drop variables, and In-memory mode and collect size is 200
#  con_sqlite %>%
#    tbl("TB_MEDICOST") %>%
#    describe(-age, -bmi, -charges, collect_size = 200)
#  
#  # Find the statistic of all numerical variables by 'sex' and 'smoker',
#  # and extract only those with 'sex' variable level is "male".
#  con_sqlite %>%
#    tbl("TB_MEDICOST") %>%
#    group_by(sex, smoker) %>%
#    describe() %>%
#    filter(sex == "male")
#  
#  # extract only those with 'region' variable level is "northwest",
#  # and find 'charges' statistics by 'sex' and 'smoker'
#  con_sqlite %>%
#    tbl("TB_MEDICOST") %>%
#    filter(region == "northwest") %>%
#    group_by(sex, smoker) %>%
#    describe(charges)

## ----dbi_normality, eval=FALSE------------------------------------------------
#  # Test all numerical variables by 'sex' and 'smoker',
#  # and extract only those with 'sex' variable level is "male".
#  con_sqlite %>%
#    tbl("TB_MEDICOST") %>%
#    group_by(sex, smoker) %>%
#    normality() %>%
#    filter(sex == "male")
#  
#  # extract only those with 'region' variable level is "northwest",
#  # and test 'charges' by 'sex' and 'smoker'
#  con_sqlite %>%
#    tbl("TB_MEDICOST") %>%
#    filter(region == "northwest") %>%
#    group_by(sex, smoker) %>%
#    normality(charges)
#  
#  # Test log(charges) variables by 'sex' and 'smoker',
#  # and extract only p.value greater than 0.01.
#  
#  # SQLite extension functions for log transformation
#  RSQLite::initExtension(con_sqlite)
#  
#  con_sqlite %>%
#    tbl("TB_MEDICOST") %>%
#    mutate(log_charges = log(charges)) %>%
#    group_by(sex, smoker) %>%
#    normality(log_charges) %>%
#    filter(p_value < 0.01)

## ----plot_normality_dbi, fig.align='center', fig.width = 6, fig.height = 4, eval=FALSE----
#  # extract only those with 'sex' variable level is "male",
#  # and plot 'charges' by 'smoker'
#  # the result is same as a data.frame, but not display here. reference above in document.
#  con_sqlite %>%
#    tbl("TB_MEDICOST") %>%
#    filter(sex == "male") %>%
#    group_by(smoker) %>%
#    plot_normality(charges)

## ----dbi_correlation, eval=FALSE----------------------------------------------
#  # Correlation coefficient
#  # that eliminates redundant combination of variables
#  con_sqlite %>%
#    tbl("TB_MEDICOST") %>%
#    correlate() %>%
#    filter(as.integer(var1) > as.integer(var2))
#  
#  con_sqlite %>%
#    tbl("TB_MEDICOST") %>%
#    correlate(bmi, charges) %>%
#    filter(as.integer(var1) > as.integer(var2))
#  
#  # Compute the correlation coefficient of charges variable by 'sex'
#  # and 'smoker' variables. And extract only those with absolute
#  # value of correlation coefficient is greater than 0.5
#  con_sqlite %>%
#    tbl("TB_MEDICOST") %>%
#    group_by(sex, smoker) %>%
#    correlate(charges) %>%
#    filter(abs(coef_corr) >= 0.5)
#  
#  # extract only those with 'region' variable level is "southeast",
#  # and compute the correlation coefficient of 'charges' variable
#  # by 'sex' and 'smoker' variables.
#  # And the correlation coefficient is negative and smaller than 0.3
#  con_sqlite %>%
#    tbl("TB_MEDICOST") %>%
#    filter(region == "southeast") %>%
#    group_by(sex, smoker) %>%
#    correlate(charges) %>%
#    filter(coef_corr < 0) %>%
#    filter(abs(coef_corr) > 0.5)

## ----plot_correlation_dbi, fig.align='center', fig.width = 6, fig.height = 4, warning=FALSE, eval=FALSE----
#  # Extract only those with 'region' variable level is "southeast",
#  # and visualize correlation plot by 'sex' and 'smoker' variables.
#  con_sqlite %>%
#    tbl("TB_MEDICOST") %>%
#    filter(region == "southeast") %>%
#    group_by(sex, smoker) %>%
#    correlate() %>%
#    plot()

## ----dbi_ctarget_by, eval=FALSE-----------------------------------------------
#  # If the target variable is a categorical variable
#  categ <- target_by(con_sqlite %>% tbl("TB_MEDICOST") , sex)
#  
#  # If the variable of interest is a numerical variable
#  cat_num <- relate(categ, charges)
#  cat_num
#  summary(cat_num)

## ----plot_target_by_dbi, fig.align='center', fig.align='center', fig.width = 6, fig.height = 4, eval=FALSE----
#  # the result is same as a data.frame, but not display here. reference above in document.
#  plot(cat_num)

## ----dbi_eda_report, eval=FALSE, eval=FALSE-----------------------------------
#  # create web report file.
#  con_sqlite %>%
#    tbl("TB_MEDICOST") %>%
#    eda_web_report()
#  
#  # create pdf file. file name is EDA.pdf, and collect size is 350
#  con_sqlite %>%
#    tbl("TB_MEDICOST") %>%
#    eda_paged_report(collect_size = 350, output_file = "EDA.pdf")

