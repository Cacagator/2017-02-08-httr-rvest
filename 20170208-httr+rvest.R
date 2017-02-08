## If you don't have these packages already installed:
# install.packages(c("httr", "rvest"))

### First part -- httr 

library(httr)

## check that the request was successful
ru_check <- function(req) {
  if (req$status_code >= 400) {
    msg <- http_status(req)$message
    stop("HTTP failure: ", req$status_code, " ", msg)
  }
}

## parse the result of the request
ru_parse <- function(req) {
  text <- content(req, as = "text")
  if (identical(text, ""))
    stop("No output to parse")
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

## Send the request to the server
ru_GET <- function(url=ru_url, ...) {
  req <- GET(url=url, ...)
  ru_check(req)
  ru_parse(req)
}

## let's use a global variable to store the URL of the API
ru_url <- "https://randomuser.me/api"

## function to get user profiles from some countries
ru_from_countries <- function(country, n_profile=1) {
  country <- match.arg(country,
                       c("AU", "BR",
                         "CA", "CH",
                         "DE", "DK", "ES",
                         "FI", "FR"), 
                       several.ok = TRUE)
  country <- paste(country, collapse=",")
  ru_GET(url=modify_url(ru_url,
                        query=list(nat = country,
                                   results=n_profile)))
}

#### Part 2 -- rvest

library(rvest)

## how to extract the title of a movie from an IMBd page
html <- read_html("http://www.imdb.com/title/tt1490017/")
html %>% 
  xml_find_all(".//h1") %>% 
  html_text()

## How to extract the cast for the movie

## Use selectorgadget to find the XPath on the page
## extract info from webpage
cast <- html_nodes(html, "#titleCast span.itemprop") %>% 
  html_text()


