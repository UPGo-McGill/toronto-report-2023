#### QC KIJIJI URLS ############################################################

library(tidyverse)
library(rvest)
library(future)
library(doFuture)
plan(multisession)
library(progressr)
handlers(global = TRUE)


# Prep --------------------------------------------------------------------

proxy_list <- read_lines(.proxy_URL)
proxy_list <- str_split(proxy_list, ":")
user_name <- proxy_list[[1]][[3]]
pw <- proxy_list[[1]][[4]]
proxy_list <- map_chr(proxy_list, \(x) paste0("http://", x[[1]], ":", x[[2]]))

url_start <- "https://www.kijiji.ca"
str_url <- c("https://www.kijiji.ca/b-short-term-rental/city-of-toronto/page-", 
             "/c42l1700273?ad=offering&siteLocale=en_CA")
ltr_url <- c("https://www.kijiji.ca/b-apartments-condos/city-of-toronto/page-", 
             "/c37l1700273?ad=offering&siteLocale=en_CA")
class_n_results <- "//*[@class=\"resultsShowingCount-1707762110\"]"
class_n_results_2 <- "sc-fac25546-0 hkvhEj"


# Get number of STR pages -------------------------------------------------

n <- 1

while (n < 5) {
  
  # Setup
  n <- n + 1
  ua <- upgo:::user_agents[[ceiling(runif(1, 1, length(upgo:::user_agents)))]]
  proxy <- proxy_list[[ceiling(runif(1, 1, length(proxy_list)))]]
  
  # Get first listings page
  listings_to_scrape <- httr::GET(
    paste0(str_url[1], "1", str_url[2]), httr::user_agent(ua), 
    httr::use_proxy(proxy, username = user_name, password = pw))
  
  # Remove proxy and try again on a 403 error
  if (listings_to_scrape$status_code == 403) {
    proxy_list <- proxy_list[proxy_list != proxy]
    next
  } 
  
  # Get the number of listings with a 200 code, and stop otherwise
  if (listings_to_scrape$status_code == 200) {
    listings_to_scrape <- 
      listings_to_scrape |> 
      xml2::read_html() |> 
      rvest::html_node(xpath = class_n_results) |> 
      rvest::html_text()
  } else stop("The server returned ", listings_to_scrape$status_code, ".")
  
  # Stop if the response is empty  
  if (nchar(listings_to_scrape) == 0) {
    stop("The server returned an empty response.")
  }
  
  # Calculate pages to scrape
  listings_to_scrape <- 
    listings_to_scrape |> 
    stringr::str_extract("(?<= of ).*(?=( Ads)|( results))") |> 
    stringr::str_replace(",", "") |> 
    as.integer()
  
  pages <- min(ceiling(listings_to_scrape / 40), 100)
  
  # Break the loop if it is successful
  break
  
}


# Get STR URLs, part 1 ----------------------------------------------------

n <- 1
missing_pages <- rep(TRUE, pages)
url_list_1 <- vector("list", pages)
upgo:::handler_upgo("STR URLs (part 1)")

progressr::with_progress({
  
  while (n < 5 && sum(missing_pages) > 0) {
    
    n <- n + 1
    p <- progressor(steps = pages)
    
    url_list_new <- foreach(i = seq_len(pages)) %dofuture% {
      
      # Skip if the result isn't missing
      if (!missing_pages[[i]]) {
        p()
        return(list(NULL, NULL))
      }
      
      # Prep UA/proxy/URL
      ua <- upgo:::user_agents[[i %% length(upgo:::user_agents) + 1]]
      proxy <- proxy_list[[i %% length(proxy_list) + 1]]
      url <- paste0(str_url[1], i, str_url[2])
      
      # Get page
      page <- tryCatch({
        httr::GET(url, httr::user_agent(ua), 
                  httr::use_proxy(proxy, username = user_name, password = pw))
      }, error = function(e) NULL)
      
      # Signal progress
      p()
      
      # Return results
      if (is.null(page)) {
        bad_proxy <- FALSE
        return(list(page, bad_proxy))
      }
      
      if (page$status_code == 403) {
        bad_proxy <- TRUE
        page <- NULL
        return(list(page, bad_proxy))
      } else bad_proxy <- FALSE
      
      if (page$status_code == 200) {
        page <- 
          page |> 
          xml2::read_html() |> 
          rvest::html_nodes(xpath = "//*[@class=\"title\"]") |> 
          xml2::xml_children() |> 
          rvest::html_attr("href") |> 
          stats::na.omit()
        return(list(page, bad_proxy))
      }
      
      return(list(page, bad_proxy))
      
    }
    
    # Remove bad proxies
    bad_proxies <- which(map_lgl(url_list_new, \(x) x[[2]]))
    if (length(bad_proxies) > 0) proxy_list <- proxy_list[-bad_proxies]
    
    # Update url_list_1
    url_list_1[missing_pages] <- url_list_new[missing_pages]
    
    # Update missing pages
    missing_new <- map_lgl(url_list_new, 
                           \(x) !is.null(x[[1]]) & !is.character(x[[1]]))
    missing_pages[missing_pages] <- missing_new[missing_pages]
    
  }
  
})


url_list_1 <- unique(unlist(map(url_list_1, \(x) x[[1]])))


# Get STR URLs, part 2 ----------------------------------------------------

n <- 1
missing_pages <- rep(TRUE, pages)
url_list_2 <- vector("list", pages)
upgo:::handler_upgo("STR URLs (part 2)")

progressr::with_progress({
  
  while (pages == 100 && n < 5 && sum(missing_pages) > 0) {
    
    n <- n + 1
    p <- progressor(steps = pages)
    
    url_list_new <- foreach(i = seq_len(pages)) %dofuture% {
      
      # Skip if the result isn't missing
      if (!missing_pages[[i]]) {
        p()
        return(list(NULL, NULL))
      }
      
      # Prep UA/proxy/URL
      ua <- upgo:::user_agents[[i %% length(upgo:::user_agents) + 1]]
      proxy <- proxy_list[[i %% length(proxy_list) + 1]]
      url <- paste0(str_url[1], i, str_url[2], "&sort=dateAsc")
      
      # Get page
      page <- tryCatch({
        httr::GET(url, httr::user_agent(ua), 
                  httr::use_proxy(proxy, username = user_name, password = pw))
      }, error = function(e) NULL)
      
      # Signal progress
      p()
      
      # Return results
      if (is.null(page)) {
        bad_proxy <- FALSE
        return(list(page, bad_proxy))
      }
      
      if (page$status_code == 403) {
        bad_proxy <- TRUE
        page <- NULL
        return(list(page, bad_proxy))
      } else bad_proxy <- FALSE
      
      if (page$status_code == 200) {
        page <- 
          page |> 
          xml2::read_html() |> 
          rvest::html_nodes(xpath = "//*[@class=\"title\"]") |> 
          xml2::xml_children() |> 
          rvest::html_attr("href") |> 
          stats::na.omit()
        return(list(page, bad_proxy))
      }
      
      return(list(page, bad_proxy))
      
    }
    
    # Remove bad proxies
    bad_proxies <- which(map_lgl(url_list_new, \(x) x[[2]]))
    if (length(bad_proxies) > 0) proxy_list <- proxy_list[-bad_proxies]
    
    # Update url_list_2
    url_list_2[missing_pages] <- url_list_new[missing_pages]
    
    # Update missing pages
    missing_new <- map_lgl(url_list_new, 
                           \(x) !is.null(x[[1]]) & !is.character(x[[1]]))
    missing_pages[missing_pages] <- missing_new[missing_pages]
    
  }
  
})

url_list_2 <- unique(unlist(map(url_list_2, \(x) x[[1]])))


# Get STR URLs, part 3 ----------------------------------------------------

n <- 1
missing_pages <- rep(TRUE, pages)
url_list_3 <- vector("list", pages)
upgo:::handler_upgo("STR URLs (part 3)")

progressr::with_progress({
  
  while (pages == 100 && n < 5 && sum(missing_pages) > 0) {
    
    n <- n + 1
    p <- progressor(steps = pages)
    
    url_list_new <- foreach(i = seq_len(pages)) %dofuture% {
      
      # Skip if the result isn't missing
      if (!missing_pages[[i]]) {
        p()
        return(list(NULL, NULL))
      }
      
      # Prep UA/proxy/URL
      ua <- upgo:::user_agents[[i %% length(upgo:::user_agents) + 1]]
      proxy <- proxy_list[[i %% length(proxy_list) + 1]]
      url <- paste0(str_url[1], i, str_url[2], "&sort=priceAsc")
      
      # Get page
      page <- tryCatch({
        httr::GET(url, httr::user_agent(ua), 
                  httr::use_proxy(proxy, username = user_name, password = pw))
      }, error = function(e) NULL)
      
      # Signal progress
      p()
      
      # Return results
      if (is.null(page)) {
        bad_proxy <- FALSE
        return(list(page, bad_proxy))
      }
      
      if (page$status_code == 403) {
        bad_proxy <- TRUE
        page <- NULL
        return(list(page, bad_proxy))
      } else bad_proxy <- FALSE
      
      if (page$status_code == 200) {
        page <- 
          page |> 
          xml2::read_html() |> 
          rvest::html_nodes(xpath = "//*[@class=\"title\"]") |> 
          xml2::xml_children() |> 
          rvest::html_attr("href") |> 
          stats::na.omit()
        return(list(page, bad_proxy))
      }
      
      return(list(page, bad_proxy))
      
    }
    
    # Remove bad proxies
    bad_proxies <- which(map_lgl(url_list_new, \(x) x[[2]]))
    if (length(bad_proxies) > 0) proxy_list <- proxy_list[-bad_proxies]
    
    # Update url_list_3
    url_list_3[missing_pages] <- url_list_new[missing_pages]
    
    # Update missing pages
    missing_new <- map_lgl(url_list_new, 
                           \(x) !is.null(x[[1]]) & !is.character(x[[1]]))
    missing_pages[missing_pages] <- missing_new[missing_pages]
    
  }
  
})


url_list_3 <- unique(unlist(map(url_list_3, \(x) x[[1]])))


# Get STR URLs, part 4 ----------------------------------------------------

n <- 1
missing_pages <- rep(TRUE, pages)
url_list_4 <- vector("list", pages)
upgo:::handler_upgo("STR URLs (part 4)")

progressr::with_progress({
  
  while (pages == 100 && n < 5 && sum(missing_pages) > 0) {
    
    n <- n + 1
    p <- progressor(steps = pages)
    
    url_list_new <- foreach(i = seq_len(pages)) %dofuture% {
      
      # Skip if the result isn't missing
      if (!missing_pages[[i]]) {
        p()
        return(list(NULL, NULL))
      }
      
      # Prep UA/proxy/URL
      ua <- upgo:::user_agents[[i %% length(upgo:::user_agents) + 1]]
      proxy <- proxy_list[[i %% length(proxy_list) + 1]]
      url <- paste0(str_url[1], i, str_url[2], "&sort=priceDesc")
      
      # Get page
      page <- tryCatch({
        httr::GET(url, httr::user_agent(ua), 
                  httr::use_proxy(proxy, username = user_name, password = pw))
      }, error = function(e) NULL)
      
      # Signal progress
      p()
      
      # Return results
      if (is.null(page)) {
        bad_proxy <- FALSE
        return(list(page, bad_proxy))
      }
      
      if (page$status_code == 403) {
        bad_proxy <- TRUE
        page <- NULL
        return(list(page, bad_proxy))
      } else bad_proxy <- FALSE
      
      if (page$status_code == 200) {
        page <- 
          page |> 
          xml2::read_html() |> 
          rvest::html_nodes(xpath = "//*[@class=\"title\"]") |> 
          xml2::xml_children() |> 
          rvest::html_attr("href") |> 
          stats::na.omit()
        return(list(page, bad_proxy))
      }
      
      return(list(page, bad_proxy))
      
    }
    
    # Remove bad proxies
    bad_proxies <- which(map_lgl(url_list_new, \(x) x[[2]]))
    if (length(bad_proxies) > 0) proxy_list <- proxy_list[-bad_proxies]
    
    # Update url_list_4
    url_list_4[missing_pages] <- url_list_new[missing_pages]
    
    # Update missing pages
    missing_new <- map_lgl(url_list_new, 
                           \(x) !is.null(x[[1]]) & !is.character(x[[1]]))
    missing_pages[missing_pages] <- missing_new[missing_pages]
    
  }
  
})


url_list_4 <- unique(unlist(map(url_list_4, \(x) x[[1]])))


# Compile STR URLs --------------------------------------------------------

url_list <- unique(c(url_list_1, url_list_2, url_list_3, url_list_4))
url_list <- url_list[url_list != "https://www.kijiji.ca"]
short_urls <- paste0("https://www.kijiji.ca/v-short-term-rental/", 
                     stringr::str_extract(url_list, '(?<=/)[:digit:]{5,}'))


# Get number of LTR pages -------------------------------------------------

n <- 1

while (n < 5) {
  
  # Setup
  n <- n + 1
  ua <- upgo:::user_agents[[ceiling(runif(1, 1, length(upgo:::user_agents)))]]
  proxy <- proxy_list[[ceiling(runif(1, 1, length(proxy_list)))]]
  
  # Get first listings page
  listings_to_scrape <- httr::GET(
    paste0(ltr_url[1], "1", ltr_url[2]), httr::user_agent(ua), 
    httr::use_proxy(proxy, username = user_name, password = pw))
  
  # Remove proxy and try again on a 403 error
  if (listings_to_scrape$status_code == 403) {
    proxy_list <- proxy_list[proxy_list != proxy]
    next
  } 
  
  # Get the number of listings with a 200 code, and stop otherwise
  if (listings_to_scrape$status_code == 200) {
    listings_to_scrape <-
      listings_to_scrape |> 
      xml2::read_html() |> 
      html_text2() |> 
      str_extract('(?<=Showing 1 - 40 of )\\d{2,4}(?= results)') |> 
      as.integer()
  } else stop("The server returned ", listings_to_scrape$status_code, ".")
  
  # Stop if the response is empty  
  if (nchar(listings_to_scrape) == 0) {
    stop("The server returned an empty response.")
  }
  
  pages <- min(ceiling(listings_to_scrape / 40), 100)
  
  # Break the loop if it is successful
  break
  
}


# Get LTR URLs, part 1 ----------------------------------------------------

n <- 1
missing_pages <- rep(TRUE, pages)
url_list_1 <- vector("list", pages)
upgo:::handler_upgo("LTR URLs (part 1)")

progressr::with_progress({
  
  while (n < 5 && sum(missing_pages) > 0) {
    
    n <- n + 1
    p <- progressor(steps = pages)
    
    url_list_new <- foreach(i = seq_len(pages)) %dofuture% {
      
      # Skip if the result isn't missing
      if (!missing_pages[[i]]) {
        p()
        return(list(NULL, NULL))
      }
      
      # Prep UA/proxy/URL
      ua <- upgo:::user_agents[[i %% length(upgo:::user_agents) + 1]]
      proxy <- proxy_list[[i %% length(proxy_list) + 1]]
      url <- paste0(ltr_url[1], i, ltr_url[2])
      
      # Get page
      page <- tryCatch({
        httr::GET(url, httr::user_agent(ua), 
                  httr::use_proxy(proxy, username = user_name, password = pw))
      }, error = function(e) NULL)
      
      # Signal progress
      p()
      
      # Return results
      if (is.null(page)) {
        bad_proxy <- FALSE
        return(list(page, bad_proxy))
      }
      
      if (page$status_code == 403) {
        bad_proxy <- TRUE
        page <- NULL
        return(list(page, bad_proxy))
      } else bad_proxy <- FALSE
      
      if (page$status_code == 200) {
        page <- 
          page |> 
          xml2::read_html() |> 
          rvest::html_nodes(xpath = "//*[@class=\"title\"]") |> 
          xml2::xml_children() |> 
          rvest::html_attr("href") |> 
          stats::na.omit()
        return(list(page, bad_proxy))
      }
      
      return(list(page, bad_proxy))
      
    }
    
    # Remove bad proxies
    bad_proxies <- which(map_lgl(url_list_new, \(x) x[[2]]))
    if (length(bad_proxies) > 0) proxy_list <- proxy_list[-bad_proxies]
    
    # Update url_list_1
    url_list_1[missing_pages] <- url_list_new[missing_pages]
    
    # Update missing pages
    missing_new <- map_lgl(url_list_new, 
                           \(x) !is.null(x[[1]]) & !is.character(x[[1]]))
    missing_pages[missing_pages] <- missing_new[missing_pages]
    
  }
  
  
})

url_list_1 <- unique(unlist(map(url_list_1, \(x) x[[1]])))


# Get LTR URLs, part 2 ----------------------------------------------------

n <- 1
missing_pages <- rep(TRUE, pages)
url_list_2 <- vector("list", pages)
upgo:::handler_upgo("LTR URLs (part 2)")

progressr::with_progress({
  
  while (pages == 100 && n < 5 && sum(missing_pages) > 0) {
    
    n <- n + 1
    p <- progressor(steps = pages)
    
    url_list_new <- foreach(i = seq_len(pages)) %dofuture% {
      
      # Skip if the result isn't missing
      if (!missing_pages[[i]]) {
        p()
        return(list(NULL, NULL))
      }
      
      # Prep UA/proxy/URL
      ua <- upgo:::user_agents[[i %% length(upgo:::user_agents) + 1]]
      proxy <- proxy_list[[i %% length(proxy_list) + 1]]
      url <- paste0(ltr_url[1], i, ltr_url[2], "&sort=dateAsc")
      
      # Get page
      page <- tryCatch({
        httr::GET(url, httr::user_agent(ua), 
                  httr::use_proxy(proxy, username = user_name, password = pw))
      }, error = function(e) NULL)
      
      # Signal progress
      p()
      
      # Return results
      if (is.null(page)) {
        bad_proxy <- FALSE
        return(list(page, bad_proxy))
      }
      
      if (page$status_code == 403) {
        bad_proxy <- TRUE
        page <- NULL
        return(list(page, bad_proxy))
      } else bad_proxy <- FALSE
      
      if (page$status_code == 200) {
        page <- 
          page |> 
          xml2::read_html() |> 
          rvest::html_nodes(xpath = "//*[@class=\"title\"]") |> 
          xml2::xml_children() |> 
          rvest::html_attr("href") |> 
          stats::na.omit()
        return(list(page, bad_proxy))
      }
      
      return(list(page, bad_proxy))
      
    }
    
    # Remove bad proxies
    bad_proxies <- which(map_lgl(url_list_new, \(x) x[[2]]))
    if (length(bad_proxies) > 0) proxy_list <- proxy_list[-bad_proxies]
    
    # Update url_list_2
    url_list_2[missing_pages] <- url_list_new[missing_pages]
    
    # Update missing pages
    missing_new <- map_lgl(url_list_new, 
                           \(x) !is.null(x[[1]]) & !is.character(x[[1]]))
    missing_pages[missing_pages] <- missing_new[missing_pages]
    
  }
  
})

url_list_2 <- unique(unlist(map(url_list_2, \(x) x[[1]])))


# Get LTR URLs, part 3 ----------------------------------------------------

n <- 1
missing_pages <- rep(TRUE, pages)
url_list_3 <- vector("list", pages)
upgo:::handler_upgo("LTR URLs (part 3)")

progressr::with_progress({
  
  while (pages == 100 && n < 5 && sum(missing_pages) > 0) {
    
    n <- n + 1
    p <- progressor(steps = pages)
    
    url_list_new <- foreach(i = seq_len(pages)) %dofuture% {
      
      # Skip if the result isn't missing
      if (!missing_pages[[i]]) {
        p()
        return(list(NULL, NULL))
      }
      
      # Prep UA/proxy/URL
      ua <- upgo:::user_agents[[i %% length(upgo:::user_agents) + 1]]
      proxy <- proxy_list[[i %% length(proxy_list) + 1]]
      url <- paste0(ltr_url[1], i, ltr_url[2], "&sort=priceAsc")
      
      # Get page
      page <- tryCatch({
        httr::GET(url, httr::user_agent(ua), 
                  httr::use_proxy(proxy, username = user_name, password = pw))
      }, error = function(e) NULL)
      
      # Signal progress
      p()
      
      # Return results
      if (is.null(page)) {
        bad_proxy <- FALSE
        return(list(page, bad_proxy))
      }
      
      if (page$status_code == 403) {
        bad_proxy <- TRUE
        page <- NULL
        return(list(page, bad_proxy))
      } else bad_proxy <- FALSE
      
      if (page$status_code == 200) {
        page <- 
          page |> 
          xml2::read_html() |> 
          rvest::html_nodes(xpath = "//*[@class=\"title\"]") |> 
          xml2::xml_children() |> 
          rvest::html_attr("href") |> 
          stats::na.omit()
        return(list(page, bad_proxy))
      }
      
      return(list(page, bad_proxy))
      
    }
    
    # Remove bad proxies
    bad_proxies <- which(map_lgl(url_list_new, \(x) x[[2]]))
    if (length(bad_proxies) > 0) proxy_list <- proxy_list[-bad_proxies]
    
    # Update url_list_3
    url_list_3[missing_pages] <- url_list_new[missing_pages]
    
    # Update missing pages
    missing_new <- map_lgl(url_list_new, 
                           \(x) !is.null(x[[1]]) & !is.character(x[[1]]))
    missing_pages[missing_pages] <- missing_new[missing_pages]
    
  }
  
})

url_list_3 <- unique(unlist(map(url_list_3, \(x) x[[1]])))


# Get LTR URLs, part 4 ----------------------------------------------------

n <- 1
missing_pages <- rep(TRUE, pages)
url_list_4 <- vector("list", pages)
upgo:::handler_upgo("LTR URLs (part 4)")

progressr::with_progress({
  
  while (pages == 100 && n < 5 && sum(missing_pages) > 0) {
    
    n <- n + 1
    p <- progressor(steps = pages)
    
    url_list_new <- foreach(i = seq_len(pages)) %dofuture% {
      
      # Skip if the result isn't missing
      if (!missing_pages[[i]]) {
        p()
        return(list(NULL, NULL))
      }
      
      # Prep UA/proxy/URL
      ua <- upgo:::user_agents[[i %% length(upgo:::user_agents) + 1]]
      proxy <- proxy_list[[i %% length(proxy_list) + 1]]
      url <- paste0(ltr_url[1], i, ltr_url[2], "&sort=priceDesc")
      
      # Get page
      page <- tryCatch({
        httr::GET(url, httr::user_agent(ua), 
                  httr::use_proxy(proxy, username = user_name, password = pw))
      }, error = function(e) NULL)
      
      # Signal progress
      p()
      
      # Return results
      if (is.null(page)) {
        bad_proxy <- FALSE
        return(list(page, bad_proxy))
      }
      
      if (page$status_code == 403) {
        bad_proxy <- TRUE
        page <- NULL
        return(list(page, bad_proxy))
      } else bad_proxy <- FALSE
      
      if (page$status_code == 200) {
        page <- 
          page |> 
          xml2::read_html() |> 
          rvest::html_nodes(xpath = "//*[@class=\"title\"]") |> 
          xml2::xml_children() |> 
          rvest::html_attr("href") |> 
          stats::na.omit()
        return(list(page, bad_proxy))
      }
      
      return(list(page, bad_proxy))
      
    }
    
    # Remove bad proxies
    bad_proxies <- which(map_lgl(url_list_new, \(x) x[[2]]))
    if (length(bad_proxies) > 0) proxy_list <- proxy_list[-bad_proxies]
    
    # Update url_list_4
    url_list_4[missing_pages] <- url_list_new[missing_pages]
    
    # Update missing pages
    missing_new <- map_lgl(url_list_new, 
                           \(x) !is.null(x[[1]]) & !is.character(x[[1]]))
    missing_pages[missing_pages] <- missing_new[missing_pages]
    
  }
  
})

url_list_4 <- unique(unlist(map(url_list_4, \(x) x[[1]])))


# Compile LTR URLs --------------------------------------------------------

url_list <- unique(c(url_list_1, url_list_2, url_list_3, url_list_4))
url_list <- url_list[url_list != "https://www.kijiji.ca"]
long_urls <- paste0("https://www.kijiji.ca/v-apartments-condos/",
                    stringr::str_extract(url_list, '(?<=/)[:digit:]{5,}'))


# Scrape listings ---------------------------------------------------------

url_list <- c(short_urls, long_urls)

n <- 1
missing_listings <- rep(TRUE, length(url_list))
listings <- vector("list", length(url_list))
upgo:::handler_upgo("Listing")

progressr::with_progress({
  
  while (n < 10 && length(proxy_list) > 0 && sum(missing_listings) > 0) {
    
    n <- n + 1
    p <- progressor(along = listings)
    
    listings_new <- foreach(i = seq_along(listings)) %dofuture% {
      
      # Skip if the result isn't missing
      if (!missing_listings[[i]]) {
        p()
        return(NULL)
      }
      
      # Prep UA/proxy/URL
      ua <- upgo:::user_agents[[i %% length(upgo:::user_agents) + 1]]
      proxy <- proxy_list[[i %% length(proxy_list) + 1]]
      url <- url_list[[i]]
      
      # Get listing
      listing <- tryCatch({
        httr::GET(url, httr::user_agent(ua), 
                  httr::use_proxy(proxy, username = user_name, password = pw))
      }, error = function(e) NULL)
      
      # Signal progress
      p()
      
      # Return results
      return(listing)
      
    }
    
    # Remove bad proxies
    bad_proxies <- which(map_lgl(
      listings_new, \(x) !is.null(x) && x$status_code == 403))
    
    if (length(bad_proxies) > 0) {
      
      bad_proxies <- 
        bad_proxies %% length(proxy_list) |> 
        unique() |> 
        sort()
      
      bad_proxies[bad_proxies == 0] <- length(proxy_list)
      bad_proxies <- sort(bad_proxies)
      proxy_list <- proxy_list[-bad_proxies]
      
    }
    
    # Update listings
    listings[missing_listings] <- listings_new[missing_listings]
    
    # Update missing_listings
    missing_new <- map_lgl(listings_new,
                           \(x) is.null(x) || !x$status_code %in% c(200, 404))
    missing_listings[missing_listings] <- missing_new[missing_listings]
    
  }
  
})

stopifnot(all(map_int(listings, \(x) x$status_code) %in% c(200, 404)))


# Parse listings ----------------------------------------------------------

xpath_location <- '//*[@id="ViewItemPage"]/div[5]/div/div[2]/div/div[3]/span'
class_page_expired <- "//*[@id = \"PageExpiredVIP\"]"
class_text <- "//*[@class = \"descriptionContainer-231909819\"]"
class_details <- "//*[@id=\"mainPageContent\"]"
class_id <- "//*[@class = \"adId-4111206830\"]"
class_created <- "//*[@class = \"datePosted-383942873\"]/span"
class_photos <- paste0("//*[starts-with(@class, \"container-4202182046 ", 
                       "heroImageBackgroundContainer-811153256 ", 
                       "backgroundImage\")]/picture")

parsed <- map(listings, \(x) {
  
  if (x$status_code == 404) return(upgo:::helper_error_kj())
  
  listing <- xml2::read_html(x, options = "HUGE")
  
  expired_check <- 
    listing  |> 
    rvest::html_node(xpath = class_page_expired) |> 
    rvest::html_text()
  
  if (!is.na(expired_check)) return(upgo:::helper_error_kj())
  
  text_check <- tryCatch({
    listing |> 
      rvest::html_node(xpath = class_text) |> 
      rvest::html_node("div") |> 
      rvest::html_text()
    TRUE
  }, error = function(e) FALSE)
  
  if (!text_check) return(upgo:::helper_error_kj())
  
  x_details <- 
    listing |> 
    rvest::html_node(xpath = class_details) |> 
    xml2::xml_child(2) |> 
    rvest::html_text()
  
  if (is.na(x_details)) return(upgo:::helper_error_kj())
  
  bed_bath <- 
    x_details |> 
    str_extract("Bedrooms.{1,20}Bathrooms[^ ,].{6}")
  
  bed_field <- 
    bed_bath |> 
    stringr::str_extract("(?<=Bedrooms).*(?=Bathrooms)") |> 
    stringr::str_replace(": ", "") |> 
    stringr::str_trim()
  
  bath_field <- 
    bed_bath |> 
    stringr::str_extract("(?<=Bathrooms).*(?=Ov|Fu|Ut|UR)") |> 
    stringr::str_replace(": ", "") |> 
    stringr::str_trim()
  
  if (is.na(bath_field)) {
    bath_field <- 
      x_details |> 
      stringr::str_extract("(?<=Bathrooms).{1,5}?(?=Furnished|Overview)") |> 
      stringr::str_replace(": ", "") |> 
      stringr::str_trim()
  }
  
  tibble(
    id = listing |> 
      rvest::html_node(xpath = class_id) |> 
      rvest::html_text() |> 
      paste0("kj-", txt = _), 
    
    url = x$request[[2]],
    
    title = listing |> 
      rvest::html_node("head") |>
      rvest::html_node("title") |> 
      rvest::html_text(), 
    
    short_long = if_else(stringr::str_detect(
      url, "v-location-court-terme|v-short-term-rental"), "short", "long"), 
    
    created = coalesce(listing |> 
                         rvest::html_node(xpath = "//*/time/@datetime") |> 
                         rvest::html_text() |> 
                         as.Date(), 
                       listing |> 
                         rvest::html_element(xpath = class_created) |> 
                         rvest::html_attr("title") |> 
                         lubridate::parse_date_time("%B %d, %y %I:%M %p") |> 
                         lubridate::as_date()), 
    
    scraped = Sys.Date(), 
    
    price = listing |> 
      rvest::html_node(xpath = "//*[@class = \"priceContainer-1419890179\"]") |> 
      rvest::html_node(xpath = "span") |> 
      rvest::html_node(xpath = "span/@content") |> 
      rvest::html_text() |> 
      stringr::str_replace("\\..*$", "") |> 
      as.numeric(), 
    
    # city = city_name, # TKTK
    
    city = "TKTK",
    
    location = listing |> 
      rvest::html_node(xpath = xpath_location) |> 
      rvest::html_text(), 
    
    bedrooms = bed_field, 
    
    bathrooms = bath_field, 
    
    furnished = x_details |> 
      stringr::str_extract("(?<=Meublé|Furnished)(Yes|No)"), 
    
    details = x_details, 
    
    text = listing |> 
      rvest::html_node(xpath = class_text) |> 
      rvest::html_node("div") |> 
      rvest::html_text(), 
    
    photos = suppressWarnings(list(
      listing |> 
        rvest::html_nodes(xpath = class_photos) |> 
        as.character() |> 
        stringr::str_extract("(?<=srcset..).*(?=..type)")))
    
  ) |> 
    
    mutate(bedrooms = case_when(
      bedrooms == "1 chambre et salon" ~ "1 + Den", 
      bedrooms == "2 chambres et coin détente" ~ "2 + Den", 
      bedrooms == "6+" ~ "6+", 
      bedrooms == "6 chambres ou plus" ~ "5+", 
      bedrooms == "Studio" ~ "Bachelor/Studio", 
      .default = bedrooms)) |> 
    
    mutate(furnished = case_when(
      .data$furnished %in% c("Oui", "Yes") ~ TRUE, 
      .data$furnished %in% c("Non", "No") ~ FALSE, 
      is.na(.data$furnished) ~ NA)) |> 
    
    mutate(postal_code = case_when(
      # Full postal code
      str_detect(location, "\\w\\d\\w \\d\\w\\d") ~ 
        str_extract(location, "\\w\\d\\w \\d\\w\\d"),
      .default = str_extract(location, "\\w\\d\\w$"))) |> 
    
    mutate(location = case_when(
      # E.g. 4070 Drolet St Montreal, QC H2W 2L4, Canada, Montreal, QC, H2W 2L4
      str_detect(location, "(?<=Canada, ).*(?=, QC, \\w\\d\\w \\d\\w\\d$)") ~
        str_extract(location, "(?<=Canada, ).*(?=, QC, \\w\\d\\w \\d\\w\\d$)"),
      # Everything else
      .default = location |> 
        str_remove("(, | )(QC|NS)( |,).*$") |> 
        str_remove(", (QC|NS)$") |> 
        str_remove("(, | )\\w\\d\\w.{0,4}$") |> 
        str_remove("(^.*, )?") |> 
        str_remove(" (QC|NS)$") |> 
        str_remove("\\w\\d\\w.{0,4}$") |> 
        str_squish())) 
  
}) |> 
  bind_rows()


# Save output -------------------------------------------------------------

qs::qsave(parsed, file = "output/data/kj_2023_07_27.qs", 
          nthreads = availableCores())



