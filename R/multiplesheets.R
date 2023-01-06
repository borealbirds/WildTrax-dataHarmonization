multipleSheets <- function(fname){
  #get all excel sheets
  sheets <- readxl::excel_sheets(fname)
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x))
  df <- lapply(tibble, as.data.frame)
  
  # assign names to data frames
  names(df) <- sheets
  #print data frame 
  print(df)
}