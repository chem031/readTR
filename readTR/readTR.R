library(rvest)
readTR <- function(sundate, month, year = 2017){
  data3 <- matrix(ncol = 5)
  d <- sundate
  for (i in 1:7){
    d <- d + i
    
    if (month == (1 || 3 || 5 || 7 || 8 || 10 || 12)){
      if (d > 31){
        d <- 1
        month <- month + 1
      }
    }
    
    if (month == (4 || 6 || 9 || 11)){
      if (d > 30){
        d <- 1
        month <- month + 1
      }
    }
    
    if (month == 2){
      if (year %% 4 == 0){
        if (year %% 100 != 0){
          if (d > 29){
            d <- 1
            month <- month + 1
          }
        }
        if (year %% 100 == 0){
          if (year %% 400 != 0){
            if (d > 29){
              d <- 1
              month <- month + 1
            }
          }
          if (year %% 400 == 0){
            if (d > 28){
              d <- 1
              month <- month + 1
            }
          }
        }
      }
      if (d > 28){
        d <- 1
        month <- month + 1
      }
    }
    
    if (month > 12){
      month <- 1
      year <- year + 1
    }
    
    if (d < 10){
      date <- paste("0", as.character(d), sep = "")
    } else {
      date <- as.character(d)
    }
  
    if (month < 10){
      mon <- paste("0", as.character(month), sep = "")  
    } else {
      mon <- as.character(month)
    }
    
    yr <- as.character(year)
  
    
    theurl <- paste("https://www.teamrankings.com/ncb/schedules/?date=", yr , "-", mon, "-", date, sep = "")
    html <- read_html(theurl)
    table <- html_nodes(html, ".text-left")
    data1 <- html_text(table) 
    data2 <- matrix(data1, ncol=5, byrow = TRUE)
    data3 <- rbind(data2, data3)
  
  }
  data3.sort <- data3[order(data3[,2]) , ]
  data3.sort[, 2:3]
}