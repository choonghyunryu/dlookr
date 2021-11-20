.onAttach <- function(libname, pkgname) {
  library(showtext)
  
  ## English font
  # Online font
  font_add_google(name = "Roboto Condensed", family = "Roboto Condensed") 
  
  # Offline font
  font_path <- system.file("fonts", "LiberationSansNarrow", package = "dlookr")
  font_add(family = "Liberation Sans Narrow",
           regular = paste(font_path, "LiberationSansNarrow-Regular.ttf", sep = "/"))
  
  ## Korean font
  # Online
  font_add_google(name = "Noto Sans KR", family = "Noto Sans Korean") 
  
  showtext_auto()
}
