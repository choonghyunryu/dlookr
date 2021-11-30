.onAttach <- function(libname, pkgname) {
  tryCatch(
    expr = {
      # Online font
      # English font
      sysfonts::font_add_google(name = "Roboto Condensed", 
                                family = "Roboto Condensed") 
      
      # Korean font
      sysfonts::font_add_google(name = "Noto Sans KR", 
                                family = "Noto Sans Korean") 
      
      options(dlookr_offline = FALSE)
    },
    error = function(e){ 
      options(dlookr_offline = TRUE)
      
      message("Because it is an offline environment, only offline ports are imported.")
    },    
    finally = {
      # Offline font
      # English font
      font_path <- system.file("fonts", "LiberationSansNarrow", 
                               package = "dlookr")
      sysfonts::font_add(
        family = "Liberation Sans Narrow",
        regular = paste(font_path, "LiberationSansNarrow-Regular.ttf", 
                        sep = "/")
      )
      
      # Korean font
      font_path <- system.file("fonts", "NanumSquare", package = "dlookr")
      sysfonts::font_add(
        family = "NanumSquare",
        regular = paste(font_path, "NanumSquareOTF_acR.otf", sep = "/")
      )
    }
  )  
  
  showtext::showtext_auto()
}
