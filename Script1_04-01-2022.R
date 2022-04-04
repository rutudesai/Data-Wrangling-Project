#Getting Libraries
library("tidyverse")
library("rvest")
library("RSelenium")

#Getting firefox on and navigating to youtube using selenium client
rD <- rsDriver(browser = "firefox", port= 4544L, verbose = F)
remDr <- rD[["client"]]
remDr$navigate("https://www.youtube.com")

#Fill the search space for entering topic
topic <- "Harry Potter"
remDr$findElement("name","search_query")$sendKeysToElement(list(topic))
remDr$findElements("id", "search-icon-legacy")[[1]]$clickElement()
a1 <- remDr$findElements("class","style-scope ytd-video-renderer")

#Getting html of the output page
html <- remDr$getPageSource()[[1]]
html
