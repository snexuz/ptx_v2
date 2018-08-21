library(dplyr)
library(httr)

get_ptx_data <- function(app_id, app_key, url){
  # First save your current locale
  loc <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "en_US.UTF8")
  
  # "Tue, 21 Aug 2018 01:18:42 GMT"
  xdate <- as.POSIXlt(Sys.time(), tz = "GMT") %>% format("%a, %d %b %Y %H:%M:%S GMT")
  sig <- httr::hmac_sha1(app_key, paste("x-date:", xdate)) 
  
  # hmac username="APP ID", algorithm="hmac-sha1", headers="x-date", 
  # signature="Base64(HMAC-SHA1("x-date: " + x-date , APP Key))"
  
  authorization <- paste0(
    'hmac username="', app_id, '", ',
    'algorithm="hmac-sha1", ',
    'headers="x-date", ',
    'signature="', sig, '\"', sep = '')
  
  auth_header <- c(
    'Authorization'= authorization,
    'x-date'= as.character(xdate))
  
  dat <- GET(url, config = config(ssl_verifypeer = 0L), 
             add_headers(.headers = auth_header))
  
  
  print(http_status(dat)$message)
  Sys.setlocale('LC_TIME', loc)
  
  # return(dat)
  return(content(dat))
}


app_id <- ''
app_key <- ''
url <- 'https://ptx.transportdata.tw/MOTC/v2/Bus/Stop/City/YilanCounty?$top=30&$format=xml'

x <- get_ptx_data(app_id, app_key, url)

library(XML)
dat <- xmlParse(x, encoding = 'utf-8') # 以 xmlParse 解析 XML 檔案
xmlfiles <- xmlRoot(dat) # 將 root 設定到 content 層級（一個偷吃步的做法）
y <- xmlToDataFrame(xmlfiles, stringsAsFactors = FALSE) # 轉換成 dataframe
head(y)