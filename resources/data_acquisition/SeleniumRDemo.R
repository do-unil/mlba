# to learn more about the functionalities of selenium, please see
# https://docs.ropensci.org/RSelenium/articles/basics.html
# install.packages("RSelenium")

library(RSelenium)
# start a selenium drive
selServ <- RSelenium::rsDriver(
  remoteServerAddr = "localhost",
  browser = c("firefox"),
  verbose = FALSE,
  # this can any port, but the same number should not be used twice,
  # that's why we're simply just randomly selecting a port nuumber
  port = sample(1:10000, 1)
)
# start a firefox browser
remDr <- selServ[["client"]]

# navigate to a website
remDr$navigate("https://www.google.com/")

# agree with Google's terms and conditions
# we add a bit of pause just for susponse of the demo, otherwise you don't need
# it in a real application
Sys.sleep(3)
remDr$findElement(using = "id", value = "L2AGLb")$clickElement()
Sys.sleep(3)

# look for search box
webElem <- remDr$findElement(using = "css", "[name = 'q']")

# search for whatever you would like
webElem$sendKeysToElement(list("How to learn scraping in R?", "\uE007"))

# scroll down and then scroll up again
Sys.sleep(1)
webElem <- remDr$findElement("css", "body")
webElem$sendKeysToElement(list(key = "end"))
Sys.sleep(2)
webElem$sendKeysToElement(list(key = "home"))
Sys.sleep(1)


# then, choose any of the returned results and navigate to it
webElems <- remDr$findElements(using = "css selector", "h3")
resHeaders <- unlist(lapply(webElems, function(x) {x$getElementText()}))

# for instance, here we go for the third result
webElem <- webElems[[which(resHeaders == "Tutorial: Web Scraping in R with rvest - Dataquest")]]
webElem$clickElement()
Sys.sleep(2)

webElem <- remDr$findElement("css", "body")
webElem$sendKeysToElement(list(key = "end"))
Sys.sleep(1)
webElem$sendKeysToElement(list(key = "home"))
Sys.sleep(1)

# we go back a page and close the session
remDr$goBack()
Sys.sleep(1)
remDr$goBack()
Sys.sleep(1)

# close the browser and the server
remDr$close()
remDr$closeall()
selServ$server$stop()