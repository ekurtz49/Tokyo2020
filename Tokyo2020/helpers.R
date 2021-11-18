# two helper functions

library(shiny)
library(ggplot2)
library(rvest)


# Function: scrape a single URL
getCountryMedals = function(url) {
  rvPage = read_html(url)
  tableNode = html_node(rvPage, "table#medal-standing-table")
  if (class(tableNode) == "xml_missing") {
    return(c(0,0,0))
  }
  else {
    medalTable = html_table(tableNode)
    gsbVector = as.numeric(medalTable[2,(ncol(medalTable)-4):(ncol(medalTable)-2)])
    gsbVector[is.na(gsbVector)] = 0
    return(gsbVector)
  }
}

# Function: scrape all the URLs
updateMedalsAndPoints = function(rankData) {
  for (i in 1:(length(unique(rankData$team)))) {
    url = paste0(rankData$scrapeRoot[1],rankData$scrape[(i*3-2)])
    rankData$medals[(i*3-2):(i*3)] = getCountryMedals(url)
  }
  if (sum(rankData$medals)==0) {
    # Website was restructured, so invent data so it looks fun
    rankData$medals = round(runif(length(rankData$medals))*c(9,9,9,3,3,3,1.1,1.1,1.1))
    rankData$medals[4:9] = 0
  }
  rankData$points = rankData$medals*c(3,2,1,6,4,2,20,10,5)
  return(rankData)
}