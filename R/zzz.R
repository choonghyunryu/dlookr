.onAttach <- function(libname, pkgname) {
  # library(showtext)
  
  ## English font
  # Online font
  sysfonts::font_add_google(name = "Roboto Condensed", family = "Roboto Condensed") 
  
  # Offline font
  font_path <- system.file("fonts", "LiberationSansNarrow", package = "dlookr")
  sysfonts::font_add(family = "Liberation Sans Narrow",
           regular = paste(font_path, "LiberationSansNarrow-Regular.ttf", sep = "/"))
  
  ## Korean font
  # Online
  sysfonts::font_add_google(name = "Noto Sans KR", family = "Noto Sans Korean") 
  
  # Offline font
  font_path <- system.file("fonts", "NanumSquare", package = "dlookr")
  sysfonts::font_add(family = "NanumSquare",
           regular = paste(font_path, "NanumSquareOTF_acR.otf", sep = "/"))
  
  showtext::showtext_auto()
}
