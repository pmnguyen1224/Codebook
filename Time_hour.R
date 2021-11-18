# Function to calculate sss seconds before and after hhmmss
# 
# Phuong Nguyen (pmnguyen1224@gmail.com)
# Version 2: with updates to account for hours
#
# x: Time to calculate on ('hh0mm0ss0')
# y: Duration, time elapsed ('sss')
# Output: hh1mm1ss1
# hh, mm, ss are intermediary values


# Load stringr
library(stringr)

# 1. Calculate time (in seconds) between x and y (x after y)
#
# Both have the form 'hhmmss'
#
calc.time.between <- function(x, y){
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
      if (hh < 0){
        hh = hh + 24
      }
    }
    else {
      if (hh < 0){
        hh = hh + 24
      }
    }
  }
  else {
    if (mm < 0){
      mm = mm + 60
      hh = hh - 1
      if (hh < 0){
        hh = hh + 24
      }
    }
    else {
      if (hh < 0){
        hh = hh + 24
      }
    }
  }
  #calculate the difference in seconds
  sss = ss + mm*60 + hh*3600
  return(sss)}


# 2. Calculate time y (in seconds) after x (hhmmss)
calc.time.after <- function(x, y){
  x <- str_pad(x, 6, pad = "0") 
  hh0 <- as.numeric(substr(x, start = 1, stop = 2))
  mm0 <- as.numeric(substr(x, start = 3, stop = 4))
  ss0 <- as.numeric(substr(x, start = 5, stop = 6))
  # Calculate the number of hr, min, sec to add on
  hh = y%/%3600
  mm = (y%%3600)%/%60
  ss = (y%%3600)%%60
  # Add the hr, min, sec
  hh1 = hh0 + hh
  mm1 = mm0 + mm
  ss1 = ss0 + ss
  # Deal with the negatives
  if (ss1 >= 60) {
    ss1 = ss1 - 60
    mm1 = mm1 + 1
    if (mm1 >= 60) {
      mm1 = mm1 - 60
      hh1 = hh1 + 1
      if (hh1 >= 24) {
        hh1 = hh1 - 24
      }
    }
    else {
      if (hh1 >= 24) {
        hh1 = hh1 - 24
      }
    }
  }
  else {
    if (mm1 >= 60) {
      mm1 = mm1 - 60
      hh1 = hh1 + 1
      if (hh1 >= 24) {
        hh1 = hh1 - 24
      }
    }
    else {
      if (hh1 >= 24) {
        hh1 = hh1 - 24
      }
    }
  }
  return(paste0(str_pad(hh1, 2, "left", "0"), 
                str_pad(mm1, 2, "left", "0"),
                str_pad(ss1, 2, "left", "0")))}


# 3. Calculate time (in seconds) before hhmmss
#
calc.time.before <- function(x, y){
  x <- str_pad(x, 6, pad = "0") 
  hh0 <- as.numeric(substr(x, start = 1, stop = 2))
  mm0 <- as.numeric(substr(x, start = 3, stop = 4))
  ss0 <- as.numeric(substr(x, start = 5, stop = 6))
  # Calculate the number of hr, min, sec to add on
  hh = y%/%3600
  mm = (y%%3600)%/%60
  ss = (y%%3600)%%60
  # Subtract the hr, min, sec
  hh1 = hh0 - hh
  mm1 = mm0 - mm
  ss1 = ss0 - ss
  if (ss1 < 0) {
    ss1 = ss1 + 60
    mm1 = mm1 - 1
    if (mm1 < 0) {
      mm1 = mm1 + 60
      hh1 = hh1 - 1
      if (hh1 < 0){
        hh1 = hh1 + 24
      }
    }
    else {
      if (hh1 < 0){
        hh1 = hh1 + 24
      }
    }}
  else {
    if (mm1 < 0) {
      mm1 = mm1 + 60
      hh1 = hh1 - 1
      if (hh1 < 0){
        hh1 = hh1 + 24
      }
    }
    else {
      if (hh1 < 0){
        hh1 = hh1 + 24
      }
    }
  }
  return(paste0(str_pad(hh1, 2, "left", "0"), 
                str_pad(mm1, 2, "left", "0"),
                str_pad(ss1, 2, "left", "0")))}

