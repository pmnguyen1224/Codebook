#Function to calculate sss seconds before and after hhmmss
#
# x: Time to calculate on ('hh0mm0ss0')
# y: Duration, time elapsed ('sss')
# Output: hh1mm1ss1
# hh, mm, ss are intermediary values
#
#1. Calculate time (in seconds) after hhmmss
time_after <- function(x, y){
  x <- str_pad(x, 6, pad = "0") 
  hh0 <- as.numeric(substr(x, start = 1, stop = 2))
  mm0 <- as.numeric(substr(x, start = 3, stop = 4))
  ss0 <- as.numeric(substr(x, start = 5, stop = 6))
  mm <- y%/%60
  ss <- y%%60
  if (ss0 + ss > 60) {
    ss1 <- ss0 + ss - 60
    mm1 <- mm0 + mm + 1
    if (mm1 >= 60) {
      mm1 <- mm1 - 60
      hh1 <- hh0 + 1}
    else {hh1 <- hh0}}
  else {
    ss1 <- ss0 + ss
    mm1 <- mm0 + mm
    if (mm1 >= 60) {
      mm1 <- mm1 - 60
      hh1 <- hh0 + 1}
    else {hh1 <- hh0}}
  return(paste0(str_pad(hh1, 2, "left", "0"), 
                str_pad(mm1, 2, "left", "0"),
                str_pad(ss1, 2, "left", "0")))}


#2. Calculate time (in seconds) before hhmmss
#
time_before <- function(x, y){
  x <- str_pad(x, 6, pad = "0") 
  hh0 <- as.numeric(substr(x, start = 1, stop = 2))
  mm0 <- as.numeric(substr(x, start = 3, stop = 4))
  ss0 <- as.numeric(substr(x, start = 5, stop = 6))
  mm <- y%/%60
  ss <- y%%60
  if (ss0 - ss < 0) {
    ss1 <- ss0 - ss + 60
    mm1 <- mm0 - mm - 1
    if (mm1 < 0) {
      mm1 <- mm1 + 60
      hh1 <- hh0 - 1}
    else {hh1 <- hh0}}
  else {
    ss1 <- ss0 - ss
    mm1 <- mm0 - mm
    if (mm1 < 0) {
      mm1 <- mm1 + 60
      hh1 <- hh0 - 1}
    else {hh1 <- hh0}}
  return(paste0(str_pad(hh1, 2, "left", "0"), 
                str_pad(mm1, 2, "left", "0"),
                str_pad(ss1, 2, "left", "0")))}


#3. Calculate time (in seconds) between x and y
#x > y 
#Both have the form 'hhmmss'
#
time_between <- function(x, y){
  #split the time strings into hh, mm and ss
  x <- str_pad(x, 6, pad = "0") 
  hh0 <- as.numeric(substr(x, start = 1, stop = 2))
  mm0 <- as.numeric(substr(x, start = 3, stop = 4))
  ss0 <- as.numeric(substr(x, start = 5, stop = 6))
  y <- str_pad(y, 6, pad = "0") 
  hh1 <- as.numeric(substr(y, start = 1, stop = 2))
  mm1 <- as.numeric(substr(y, start = 3, stop = 4))
  ss1 <- as.numeric(substr(y, start = 5, stop = 6))
  #calculate the absolute differences
  hh = hh0 - hh1
  mm = mm0 - mm1
  ss = ss0 - ss1
  #account for negative values
  if (ss < 0){
    ss = ss + 60
    mm = mm - 1
    if (mm < 0){
      mm = mm + 60
      hh = hh - 1
    }
  }
  #calculate the difference in seconds
  sss = ss + mm*60 + hh*3600
  return(sss)}


time_between(033417, 033347)
