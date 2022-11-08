# dlookr 0.6.1

## BUG FIXES

* Fixed EDA report failing when target variable is number or integer 
  in eda_web_report() and eda_paged_report(). (thanks to neural-oracle, #75) 
      
* Added exception handling logic for errors that occur when the bandwidth 
  estimate of a numeric variable is 0 in plot.relate(). (#76)
      
* Fixed error while performing eda_web_report() without target variable. 
  (thanks to Ashirwad Barnwal, #81, #83)         

## MINOR CHANGES

* Added new arguments to functions that generate pdf reports for use in shinyapps.io (#77)     
    - diagnose_paged_report()
    - eda_paged_report()
    - transformation_paged_report()
        
# dlookr 0.6.0

## BUG FIXES

* Fixed an error in eda_web_report() when `variable` is included in the 
  name of a variable applied to select(). (thanks to Ashirwad Barnwal, #65) 
      
* Fixed a wrong summary in univar_category() when `x` is included in the 
  name of a variable applied to select(). (#69)    
      
* Fixed some wrong case in plot.correlate. (#66)
    - Change the x-axis to 'variable1' and the y-axis to 'variable2'.
    - Rotate the x-axis tick labels to 40 degrees for group_by case.
    - Fixed incorrect title of individual plot for group_by case.
        
* Cases that may cause errors in the binning process are not output in the report. 
  For example, if you want to bin constant value. (thanks to Iqbal Jamal, #68)
      
* Fixed an error in eda_web_report() when there are missing values in 
  categorical data. (#72)      
      
## MINOR CHANGES

* dlookr supports 'tbl_dbi' that can be analyzed in conjunction with DBMS. 
  This is done using the dbplyr package. The related code has been modified 
  to be compatible with the new version 2.2.0 of the dbplyr package. 
  This work was done by Maximilian Girlich. (thanks to @mgirlich, #70) 
      
* Now, possible to implement imputation capping in imputate_outlier() 
  function with user specified values of caps. (thanks to @Tomas Janik, #71)       
      

# dlookr 0.5.6

## BUG FIXES

* Fixed a CRAN Additional issue that is "deprecated wrning" in donttest. 
      
## MAJOR CHANGES

* plot_correlate() was deprecated. Instead, use plot.correlate().
    - plot_correlate.data.frame()
    - plot_correlate.grouped_df()
    - plot_correlate.tbl_dbi()
      
      
      
# dlookr 0.5.5

## BUG FIXES

* Fixed an error in describe() when `variable` is included in the name of 
  a variable applied to group_by(). (thanks to @SchmidtPaul, #59) 
    
* Fixed an error that occurred when the value of the 'marginal' argument 
  of summary.compare_category() was TRUE and a variable with only one level 
  was included among the variables. (#61)
      
* Fix an error in 'normality-list' section of eda_web_report(). (#62)      

* Fix an issue where table and table detail do not match in 
  html_compare_category() under certain conditions . (#63)   
      
* Fixed an error in html_compare_category() when the number of levels of 
  one of the two categorical variables is 1. (#63)         
    
## MAJOR CHANGES

* Added a new binning function called binning_rgr(), plot.infogain_bins().
  This function bins using recursive information gain ratio maximization.
  (thanks to Luca Zavarella, #15)
      
* Implemented the PPS(Predictive Power Score) with pps.data.frame(), 
  pps.target_df(), summary.pps(), plot.pps(). (thanks to Luca Zavarella, #20)      
      
* By fixing bug #59, the variable 'variable' in the describe() result 
  was changed to 'described_variables'. 
      
* Added a new method that is summary.correlate() for summarizing correlation 
  coefficient and plot.correlate() for visualize correlation coefficient.
  The new method is Cramer's V and Theil's U. (#64)
      
## MINOR CHANGES

* Added 'all.combinations' argument to describe() and describe.tbl_dbi(). 
  When used with group_by(), this argument expresses all combinations of 
  group combinations. If the argument value is TRUE, cases that do not 
  exist as actual data are also included in the output. 
  (thanks to @SchmidtPaul, #59)  
      
* For a function that visualizes correlation coefficients, change the 
  direction of the diagonal(top-left to bottom-right) and invert the color 
  of the cells(-1: red, 1: blue) in plot.     
    - plot_correlate.data.frame()
    - plot_correlate.grouped_df()
    - plot_correlate.tbl_dbi()



# dlookr 0.5.4

## BUG FIXES

* Fixed an error that occurred when attaching a package in an offline environment.
    - In the case of an offline environment, the import of Google fonts 
      is canceled and only offline fonts are imported.
    
* Fixed an error that occurred when the number of breaks and 
  the number of labels in plot.compare_category() are different.
      
* Fixed an error that occurred when check in the CRAN in r-devel-windows-x86_64-old
    - "Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
      there is no package called 'stringi'"
    - so append stringi package to "Suggests" section in DESCRIPTION file.

## MINOR CHANGES

* The default font for visualization is "Roboto Condensed" in the online 
  environment and "Liberation Sans Narrow" in the offline environment.
    
  
  
# dlookr 0.5.3

## BUG FIXES
  
* Fixed an error that occurred when generating a paged report as a pdf file 
  when non-ASCII (multi-byte) characters were included in MS-Windows.
    - diagnose_paged_report()
    - eda_paged_report()
    - transformation_paged_report()
      
## MAJOR CHANGES
  
* The size of the vignettes is large and the size of the attached file is large, 
  so it is difficult to keep the package stable on CRAN. So I opted to move 
  the content of vignettes to an external web page and share that URL on vignettes.
  
  
  
# dlookr 0.5.2

## BUG FIXES

* Fixed an error in plot_hist_numeric() when variable names contain spaces. 
      
* Fixed an error that occurred in compare_numeric() when using 
  a tibble object as an argument. 
      
* Fixed an error that occurred in eda_paged_report() and eda_web_report() 
  when using a tibble object as an argument. 
      
* Fixed an error that occurred when generating outliers information 
  in diagnose_paged_report() in case of a tibble object.
      
* Fixed an error where the base_family argument was not applied to the plot 
  in the Outlier section in diagnose_web_report().   
      
* Remove unnecessary formula output when executing binning_by(). 
    
* Fixed an issue that the correlation visualization was not plot in the EDA 
  report in "Grouped Correlation" sction in eda_web_report(). (#55)
      
* Fixed the logic has been modified so that an error does not occur even 
  when the independent variable x has only one unique value in compare_numeric().   
    
* Fixed an error in the visualization where the number of levels of a 
  categorical variable was 1 combined with this variable in the 
  "Compare Categorical Variables" section of the report in eda_web_report(),
  eda_paged_report().

## MAJOR CHANGES

* Changed the way fonts are imported into the R environment to insert fonts 
  into the plot. For this method, the extrafont package was changed to the 
  showtext package. (#48, #53)

  - The reason is that the extrafont package uses the Rttf2pt1 package, 
    because Winston Chang, the author of the Rttf2pt1 package, says:

  - "ttf2pt1 has not been maintained since 2003, and it is getting 
    increasingly difficult to make it compile without significant warnings on 
    modern compilers, which in turn is making it difficult to maintain 
    this package on CRAN. It is possible that in the future, 
    I will not have time to keep this package on CRAN."

* Added the ability to specify the font family in the report function. 
  This feature is useful for visualization by users using non-ASCII characters.
    - diagnose_paged_report(), diagnose_web_report()
    - eda_paged_report(), eda_web_report()
    - transformation_paged_report(), transformation_web_report()      
      
* Added the ability to specify the font family in the visualization function. 
  This feature is useful for visualization by users using non-ASCII characters.
    - plot_outlier.data.frame(), plot_outlier.target_df(), plot_outlier.tbl_dbi()
    - plot_hist_numeric.data.frame(), plot_hist_numeric.grouped_df(),
    - plot_na_intersect(), plot_na_pareto(), plot_na_hclust()
    - plot.compare_category(), plot.compare_numeric()
    - plot.overview(), plot.relate()
    - plot.univar_category(), plot.univar_numeric()
    - plot_bar_category.data.frame(), plot_bar_category.grouped_df()
    - plot_qq_numeric.data.frame(), plot_qq_numeric.grouped_df()
    - plot_box_numeric.data.frame(), plot_box_numeric.grouped_df()
    - plot_hist_numeric.data.frame(), plot_hist_numeric.grouped_df()
    - plot_correlate.data.frame(), plot_correlate.grouped_df(),
      plot_correlate.tbl_dbi() 
    - plot_normality.data.frame(), plot_normality.grouped_df(), 
      plot_normality.tbl_dbi()
    - plot.bins(), plot.optimal_bins(), plot.performance_bin()
    - plot.imputation()
    - plot.transform()
      
* Added the ability to specify the font family in the reporting function. 
  This feature is useful for visualization by users using non-ASCII characters.
    - diagnose_web_report(), diagnose_paged_report()
      
## MINOR CHANGES
    
* Reduce the font size of subtitle in paged reports. 
    - diagnose_paged_report()
    - eda_paged_report()
    - transformation_paged_report()
      
## DEFUNCT
  
* defuncted import_liberation()

      
      
# dlookr 0.5.1
  
## BUG FIXES

* Fixed an error that is in diagnose_paged_report(), diagnose_web_report(),
  when there are no numerical variables in the dataset. (#49)
      
* Fixed an error that is in diagnose_paged_report(), when there are no 
  categorical variables in the dataset.
      
* Fixed an error that is in eda_paged_report(), eda_web_report(), 
  when there are no numerical variables in the dataset.
      
* Fixed an error that is in eda_paged_report(), when there are no 
  categorical variables in the dataset.
      
* Fixed an error that is in diagnose_paged_report(), diagnose_web_report(), 
  when there are only one variable that include missing values. (#50)
    

# dlookr 0.5.0

## NEW FEATURES
  
* Add a new function plot_hist_numeric() to report histogram of numerical 
  variables.
      
* Add a new function diagnose_web_report() to report the information of 
  data diagnosis with wep page that support dynamic analytics.   
      
* Add a new function eda_web_report() to report the information of 
  exploratory data analysis with wep page that support dynamic analytics.   
      
* Add a new function transformation_web_report() to report the information 
  of transform numerical variables with wep page that support dynamic analytics.  
      
* Add a new function diagnose_paged_report() to report the information of 
  data diagnosis with static pages.   
      
* Add a new function eda_paged_report() to report the information of 
  exploratory data analysis with static pages.   
      
* Add a new function transformation_paged_report() to report the information 
  of transform numerical variables with static pages.        
    
## BUG FIXES

* Fixed an error that is in diagnose_category.tbl_dbi(), when in_database 
  argument is FALSE. (thanks @verajosemanuel, #43)
      
* Fixed an issue that is x axis overlaps labels in plot.optimal_bins() 
  (thanks @verajosemanuel, #45)      
    
## MINOR CHANGES
    
* Added the duplicated information in overview() and summary.overview().

* In the result of overview(), included ordered in factor in data type.    

* Change grade of missing value in plot_na_pareto().
    
* Added the "date_categorical", "date_categorical2" for type argument 
  that find Date and POSIXct classes in find_class().
      
* Added the 'add_date' argument for Date, POSIXct class in diagnose_category(). 

* In describe(), the user can select the type of statistic.
    
* Added the ability to set x-axis labels in plot.optimal_bins(). 
  This is a useful function when labels overlap because they are long.

* plot_correlate() changed the look & feel that draw 
  the viz from high-level graphic function to ggplot2.



# dlookr 0.4.5

## NEW FEATURES
  
* Add a new function diagnose_sparese() checks cases that do not occur in 
  observations for all possible combinations of levels of categorical 
  variables. (thanks HyoJin Song, #35)
    
## BUG FIXES

* Fixed an error that is in plot.univar_category(), if the number of 
  categories is more than 10, no output after 10.
      
* Fixed a bug where the bar color of NA was not displayed in 
  plot.optimal_bins(). (#39)
      
* Fixed an error occurs when there are no observations corresponding to 
  levels (may occur depending on data characteristics when the binning 
  type is "equal") in plot.bins().  
      
* Fixed a miss typo in plot_outlier(). (thanks @jmanacup, #41)

* Fixed an error in the describe() for data containing variables 
  with 3 or fewer complete cases. (thanks @davidfgeorge, #42)
    
## MINOR CHANGES
    
* Added datasets that are 'heartfailure' and 'jobchange'. 
    
* Changed the manpages are diagnose(), diagnose_category(), 
  diagnose_numeric(), diagnose_outlier(), plot_outlier(),
  plot_outlier.target_df(), diagnose_report(), univar_category.data.frame(),
  summary.univar_category(), plot.univar_category(), univar_numeric.data.frame(), 
  summary.univar_numeric(), plot.univar_numeric(), plot_bar_category(), 
  plot_qq_numeric(), plot_box_numeric(), plot_na_hclust(), plot_na_pareto(), 
  plot_na_intersect(), imputate_na(), summary.imputation(), plot.imputation(),
  normality(), plot_normality(), transform(), summary.transform(),
  plot.transform(), transformation_report(), compare_category(), 
  compare_numeric(), summary.compare_category(), summary.compare_numeric(),
  plot.compare_category(), plot.compare_numeric(), get_column_info(), 
  diagnose.tbl_dbi(), diagnose_category.tbl_dbi(), diagnose_numeric.tbl_dbi(),
  diagnose_outlier.tbl_dbi(), plot_outlier.tbl_dbi(), normality.tbl_dbi(), 
  plot_normality.tbl_dbi(), correlate.tbl_dbi(), plot_correlate.tbl_dbi(), 
  describe.tbl_dbi(), target_by.tbl_dbi(), diagnose_report.tbl_dbi(),
  eda_report.tbl_dbi(), target_by.data.frame(), relate(), print.relate(), 
  plot.relate(), binning(), summary.bins(), plot.bins(), binning_by(), 
  summary.optimal_bins(), plot.optimal_bins(), plot.optimal_bins(),
  extract.bins(), performance_bin(), summary.performance_bin(),
  plot.performance_bin(), get_class(), find_na(), find_outliers(), 
  find_skewness(), correlate.data.frame(), plot_correlate.data.frame(),
  overview(), summary.overview(), plot.overview(), eda_report.data.frame(),
  describe.data.frame().
      
* Changed default type of diagnose_category() from "rank" to "n". 
    
    
      
# dlookr 0.4.4
      
## BUG FIXES

* Fixed an error that occurred when "section 1.2.1 Diagnosis of categorical 
  variables" of diagnose_report() shows unnecessary records in cases with 
  a lot of ties in the frequency rank. (thanks @sedechio, #34)
      
* Fixed a bug in which when an object created with diagnose_category(), 
  the rank variable is just row numbers.

## MINOR CHANGES

* The type argument was added to diagnose_category(). "rank" returns the 
  rows corresponding to the top n rank, and "n" returns the top n rows.
      
* Remove the suggested package that are stringr and DMwR.  
    
* Added exception handling logic in examples and vignettes using suggested 
  package(classInt, rpart, forecast).  

      

# dlookr 0.4.3
      
## BUG FIXES

* Fixed an error that occurred when calling get_transform() with 
  the Box-Cox method when the forecast package version is less than 8.3. 
  Changed forecast package dependency to 8.3 or higher.
      
* Fixed an error that occurred when calling eda_report() with 
  the shapiro.test() function when the data included many missing values. 
    
* Fixed .onAttach() error when loading package in MS-Windows OS. 
  (thanks @Roberto Passera)


      
# dlookr 0.4.2
      
## BUG FIXES

* Fixed a bug in which when an object created with binning() and binning_by() 
  is extracted with the extract() function, the ordered factor is also 
  extracted as a factor.
      
* Fixed .onAttach() error when installing package in solaris OS.
    
* Fixed extract() not extracting factor from "bins" object.
    
## MAJOR CHANGES

* Reduced page load overhead in pdf files for more than 5000 data in three 
  automated reports. (diagnose_report, eda_report, transformation_report)
  (@choonghyunryu, #30).
    
## MINOR CHANGES

* Import dependency was modified to limit the version of the knitr package 
  for report generation to 1.22 or higher.
    
* In the visualization result of plot.bins(), the x-axis text of the lower 
  plot was rotated 45 degrees and expressed without overlapping.
      
* changed the dependency of hrbrthemes from hrbrthemes to hrbrthemes(>= 0.8.0)
  because error that occurs in an environment where the hrbrthemes package 
  is installed before version 0.6.0. So, modified the dependency of 
  hrbrthemes(thanks @coissac, #33).    
      
    
    
# dlookr 0.4.1

## NEW FEATURES
  
* In the case of type = "all" in plot.optimal_bins(), the legend is not 
  displayed in the barchart.
      
## BUG FIXES

* Fixed .onAttach() failed which occurred when installing in Solaris environment.

* Fixed an issue where unnecessary blank plots were displayed in 
  plot_box_numeric(), plot_qq_numeric(), plot_bar_category().
      
* Fixed an Resolves an issue where an installation error occurs in 
  an environment using the old tidyselect package without all_of().
      
* Fixed an error in plot_box_numeric(), plot_bar_category(), plot_outlier(), 
  compare_numeric(), plot.univar_numeric() 
  when the ggplot2 package version is out of date.
      
* Fixed an error that occurred in plot_box_numeric.grouped_df(),
  plot_bar_category.grouped_df(), plot_qq_numeric.grouped_df() 
  when the dplyr package version is less than 0.8.0
      
* Fixed an error that occurred in binning_by(), target_by() 
  when the dplyr package version is less than 0.8.0
      
* Fixed an bug that occurred in binning(), when the approxy.lab argument 
  is TRUE, the maximum value is sometimes omitted from binning and mapped to NA.   
      
      

# dlookr 0.4.0

## NEW FEATURES
  
* Add a new function entropy() to compute Shannon's entropy.
    
* Add a new function get_percentile() to compute percentile position.
    
* Add a new function get_transform() to transform numeric variable.  
      
* Add a new function kld() to computes the Kullback-Leibler divergence 
  between two probability distributions.
    
* Add a new function jsd() to computes the Jensen-Shannon divergence 
  between two probability distributions.
    
* Add a new function overview() to describe overview of data.       

* Add a new function summary.overview() to summarizes the data 
  information from the overview class object created with overview().   
    
* Add a new function plot.overview() to visualizes the data information 
  from the overview class object created with overview().   
    
* Add a new function plot_bar_category() to visualizes the distribution of 
  categorical data by level or relationship to specific numerical data by level.    
    
* Add a new function plot_qq_numeric() to visualizes the Q-Q plot of 
  numeric data or relationship to specific categorical data.    
    
* Add a new function plot_box_numeric() to visualizes the box plot of 
  numeric data or relationship to specific categorical data.   
    
* Add a new function plot_outlier.target_df() to visualizes 
  the information of outliers by target variable.    
    
* Add a new function performance_bin(), summary.performance_bin(), 
  plot.performance_bin() to diagnose the binned variable for 
  binomial classification model.         

* Add a new function summary.optimal_bins() to summaries the binned 
  variable for optimal binning.  
    
* eda_report() change the correlation plot and normality test
  (log+1: include 0, log+a/Box-Cox: include minus). 
    
* plot_correlate() change the correlation plot that support 2 case 
  plots(number of variable >=20, <20). 
    
* plot_na_pareto() changed the legend that show the all levels and 
  display the more information.

* transform(), summary.transform(), plot.transform() supports 
  Box-Cox transform and Yeo-Johnson transform. (thanks @lucazav, #21).      
      
* plot_normality() supports log+1, log+a, 1/x, x^2, x^3, Box-Cox, 
  Yeo-Johnson transform. (thanks @lucazav, #21).
      
* transform(), summary.transform(), plot.transform() supports 
  Box-Cox transform. (thanks @lucazav, #21).
      
* transform(), summary.transform(), plot.transform() supports 
  Box-Cox transform. (thanks @lucazav, #21).
      
* transform(), summary.transform(), plot.transform() supports 
  Box-Cox transform. (thanks @lucazav, #21).
      
* plot.optimal_bins() changed the plot from smbinning package to 
  own code using ggplot2.
      
* plot.bins(), plot.compare_category(), plot.compare_numeric(), 
  plot_outlier(), plot_normality() changed the look & feel that draw 
  the viz from high-level graphic function to ggplot2.
      
* plot.optimal_bins(), plot_na_hclust(), plot_na_pareto(), 
  plot_na_intersect(), plot.relate(), plot_bar_category(), 
  plot_qq_numeric(), plot_box_numeric() append argemt typographic 
  thst is whether to apply focuses on typographic elements.     
      
* modified visualization of report by diagnose_report(), eda_report(), 
  transformation_report().
      
## BUG FIXES

* eda_report() fixed error when data have a only 1 complated numeric 
  variable (thanks @EvanLuff, #27).
      
* eda_report() fixed error when transform numeric variables that include 
  minus values (thanks @Roberto Passera).    



# dlookr 0.3.14

## NEW FEATURES
  
* Add a new function univar_category() to compute information to examine 
  the individual categorical variables. and print.univar_category(), 
  summary.univar_category() is print and summary for "univar_category" 
  class. (thanks @Roberto Passera)
      
* Add a new function plot.univar_category() to visualize bar plot by 
  attribute of "univar_category" class. (thanks @Roberto Passera)
      
* Add a new function univar_numeric() to compute information to examine 
  the indivisual numerical variables. and print.univar_numeric(), 
  summary.univar_numeric() is print and summary for "univar_numeric" class. 
  (thanks @Roberto Passera)   
    
* Add a new function plot.univar_numeric() to visualize box plot and 
  histogram by attribute of "univar_numeric" class. (thanks @Roberto Passera)  
    
* Add a new function compare_category() to compute information to examine 
  the relationship between numerical variables. and 
  print.compare_category(), summary.compare_category() is print and summary 
  for "compare_category" class. (thanks @Roberto Passera)  

* Add a new function plot.compare_category() to visualize mosaics plot by 
  attribute of "compare_category" class. (thanks @Roberto Passera)   
    
* Add a new function compare_numeric() to compute information to examine 
  the relationship between numerical variables. and print.compare_numeric(), 
  summary.compare_numeric() is print and summary for "compare_numeric" class. 
  (thanks @Roberto Passera)  
    
* Add a new function plot.compare_numeric() to visualize scatter plot 
  included boxplots by attribute of "compare_numeric" class. 
  (thanks @Roberto Passera)  
    
* Add a new function plot_na_pareto() to visualize pareto chart for 
  variables with missing value.   
    
* Add a new function plot_na_hclust() to visualize distribution of 
  missing value by combination of variables. (thanks @Luca Zavarella)  
    
* Add a new function plot_na_intersect() to visualize the patterns of 
  missing value, or rather the combinations of missing value across cases. 
  (thanks @Luca Zavarella)
    
* Add a new vignette `Introduce dlookr`.  
    
* correlate() add the non-parametric correlation coefficient, like 
  "spearman" and "kendall" (thanks @Roberto Passera) 
    
* plot_correlate() add the non-parametric correlation coefficient, 
  like "spearman"" and "kendall" (thanks @Roberto Passera)  
      
## BUG FIXES

* relate() fixed error when using character type as a categorical 
  variable (thanks @jgduenasl, #14).
      
* plot.transform() fixed miss typo in title of plot 
  (thanks @MarioPrado1148, #26).
      
* Corrected sentence and typo in manuals and vignettes.
    
    
  
# dlookr 0.3.13

## BUG FIXES
  
* plot_normality() fixed an issue where plots are not drawn correctly 
  if data contains Inf.
      
* normality() fixed an issue where NaN is returned in the result if 
  the data contains Inf. And  fixed warning message that is "`cols` 
  is now required."
 
* binning() fixed error an issue where some bining errors could occur 
  at values close to breaks for large numbers. And appended 
  approxy.lab argument that choice large number breaks are approximated to 
  pretty numbers. 
      
* describe() fixed warning message that is "`cols` is now required."
 


# dlookr 0.3.12

## NEW FEATURES
  
* imputate_na() appended no_attrs argument that choice the return value. 
  return object of imputation class or numerical/categorical variable. 

* imputate_outlier() appended no_attrs argument that choice the return 
  value. return object of imputation class or numerical vector. 
      
* diagnose_report() Adjusted the pdf margins to increase the number of 
  columns represented in the table. In the latex, duplicate table labels 
  were removed.
      
* eda_report() Adjusted the pdf margins to increase the number of 
  columns represented in the table. In the latex, duplicate table labels 
  were removed.
      
* transformation_report() Adjusted the pdf margins to increase the number 
  of columns represented in the table. In the latex, duplicate table labels 
  were removed.     
      
## BUG FIXES
  
* imputate_na() fixed error when method is 'rpart' or 'knn'. 
    
    

# dlookr 0.3.11

## BUG FIXES
  
* plot_outlier() fixed error run against a dataset with a numeric column 
  where all values are NA(thanks @rhinomlbox, #8).

* describe(), describe.grouped_df() fixed error run against a dataset with 
  a numeric column where number of complate values are 0 to 3.
      
* binning() fixed error like "'breaks' are not unique". and fixed error of 
  binning with a column where all values are NA. 
      
* imputate_na() fixed the problem of imputation using ('rpart', 'mode', 
  'mice') method with a column where all values are NA. 
      
* imputate_na() fixed the problem of imputation using 'knn' method 
  when the complete case is small. 
      
* summary.imputation() fixed the problem of imputation object isn't compleate.
      
* transformation_report() fixed the problem of trying to output 
  Korean language report in English operating system environment.
      
* transformation_report() fixed the LaTeX error like "Illegal unit of 
  measure (pt inserted)" in Binning section. 
      
* transformation_report() fixed the error imputate_na() function call.     



# dlookr 0.3.10

## BUG FIXES
  
* imputate_na() fixed error to imputation using 'method' argument value is "mice".
      


# dlookr 0.3.9

## NEW FEATURES
  
* find_class() handled 'labelled' vectors as categorical variables.
    
* imputate_na() modified to set the random number generation version 
  to 3.5.0 in the 'mice' method.
    
* Set the random number generation version to 3.5.0 before calling 
  set.seed() in the code of vignette of "EDA".
    
* Set the random number generation version to 3.5.0 before calling 
  set.seed() in the code of vignette of "Data Transformation".  
      
## BUG FIXES
  
* binning(), binning_by() fixed error to converts a numeric variable to 
  a categorization variable. (thanks @Green-16, #4).
      
      

# dlookr 0.3.8

## NEW FEATURES
  
* summary.imputation(), describe.grouped_df(), normality.grouped_df(),
  plot_normality.grouped_df(), correlate.grouped_df(), plot.relate(),
  plot_correlate.grouped_df(), relate.target_df() modified features 
  to correspond to dplyr 0.8.0 or later.
      
## BUG FIXES
  
* plot_correlate.grouped_df() fixed error in the main title of the plot 
  output the factor value as an integer.
      


# dlookr 0.3.7

## NEW FEATURES
  
* eda_report() Handle exceptions when there are fewer than two numeric 
  variables when outputting a reflation plot.
      
## BUG FIXES
  
* diagnose_report() fixed errors when number of numeric variables is zero.

* eda_report() fixed errors that are outputting abnormalities in pdf 
  documents when the target variable name contains "_".



# dlookr 0.3.6

## NEW FEATURES
  
* diagnose_report(), eda_report(), transformation_report() was converted 
  to Korean version of Hangul Report in Korean O/S.
      
* diagnose_report() was added an argument to choose whether to present 
  the report results to the browser.
      
* diagnose_report() limited the maximum number of cases per 
  "Categorical variable level top 10" to 50 cases.
      
* eda_report() was added an argument to choose whether to present 
  the report results to the browser.
      
* transformation_report() was added an argument to choose whether to 
  present the report results to the browser.



# dlookr 0.3.5

## NEW FEATURES
  
* plot_outlier() change message in data where all variables are 
  categorical variables.
      
* diagnose_report() modify the table column name in pdf report and 
  lower the number of decimal places.
      
## BUG FIXES
  
* diagnose_category() fixed subscript error in data where all variables 
  are numeric variables.

* diagnose_numeric(), diagnose_outlier() fixed subscript error in data 
  where all variables are categorical variables.
      
* eda_report() fixed errors in pdf report when variable name contains "_".      



# dlookr 0.3.4

## NEW FEATURES
  
* diagnose_report() Added ability to set font family of pdf report figure.
      
## BUG FIXES
  
* find_outliers() fixed errors in index or name extraction of variables 
  containing outliers.

* find_skewness() fixed errors in index or name extraction of variables 
  with skewness exceeds the threshold.
      
* eda_report() fixed in table caption of EDA report. and added ability to 
  set font family of pdf report figure. 
    
* fixed in table caption of Transformation report. 
  and added ability to set font family of pdf report figure.
      
  

# dlookr 0.3.3

## NEW FEATURES
  
* diagnose_report(), eda_report(), transformation_report() supports 
  Korean language(hangul) with pdf output. (thanks @cardiomoon).
      
## BUG FIXES
  
* eda_report() fixed in table/figure caption of EDA report.
      
      

# dlookr 0.3.2

## NEW FEATURES
  
* plot.relate() supports hexabin plotting when this target variable is 
  numeric and the predictor is also a numeric type.
    
* Add a new function get_column_info() to show the table 
  information of the DBMS.
      
* diagnose() supports diagnosing columns of table in the DBMS.
      
* diagnose_category() supports diagnosing character columns of table in the DBMS.
      
* diagnose_numeric() supports diagnosing numeric columns of table in the DBMS.
      
* diagnose_outlier() supports diagnosing outlier of numeric columns of 
  table in the DBMS.
      
* plot_outlier() supports diagnosing outlier of numeric columns of 
  table in the DBMS.
      
* normality() supports test of normality for numeric columns of table in the DBMS.
      
* plot_normality() supports test of normality for numeric columns of table in the DBMS.
      
* correlate(), plot_correlate() supports computing the correlation 
  coefficient of numeric columns of table in the DBMS.
      
* describe() supports computing descriptive statistic of numeric 
  columns of table in the DBMS.
      
* target_by() supports columns of table in the DBMS.
      
## BUG FIXES
  
* Fixed in 4.1.1 of EDA report without target variable..



# dlookr 0.3.1

## NEW FEATURES
  
* The `plot_outlier()` supports a col argument that a color to be used 
  to fill the bars. (thanks @hangtime79, #3).
      
* Remove the name of the numeric vector to return when index = TRUE 
  in `find_na ()`, `find_outliers()`, `find_skewness()`.
      
## BUG FIXES
  
* Fixed typographical errors in EDA Report headings (thanks @hangtime79, #2).
    