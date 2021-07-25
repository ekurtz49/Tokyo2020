# Function testing!

install.packages("ggplot2")
install.packages("rvest")
library(ggplot2)
library(rvest)


# TEST CODE

testUrl = "https://olympics.com/tokyo-2020/olympic-games/en/results/all-sports/noc-profile-germany.htm"
testPage = read_html(testUrl)
tableNode = html_node(testPage, "table#medal-standing-table")
html_text(html_nodes(tableNode,"td"))
testTable = html_table(tableNode)
gsbVector = as.numeric(testTable[2,(ncol(testTable)-3):(ncol(testTable)-1)])

rankData = updateMedalsAndPoints(rankData)
# Randomize Data
rankData$medals = sample(c(0,1,2,3,4),length(rankData$medals),replace=TRUE)
rankData$points = rankData$medals*c(3,2,1,6,4,2,20,10,5)


ggplot(rankData, aes(x=who, y=points, fill=team, alpha=type, label=(paste(medals,type)))) +
  geom_col() +
  scale_alpha_manual(values = c(0.3,0.5,0.9)) +
  #geom_text(position = position_stack(vjust = 0.5), size = 4, color = "#000000")
  geom_text(position = position_jitter(),size=4,color="#000000")
  #+
  #scale_fill_manual(values = c("green","red"))
ggplot(rankData, aes(x=factor(who, levels=unique(who)), y=points, fill=factor(team, levels = unique(team)), alpha=factor(type, levels = c("Bronze","Silver","Gold")))) +
  geom_col(color = "black") +
  scale_alpha_manual(values = c(0.2,0.6,1)) +
  scale_fill_manual(values = rep(c("#AAFF00","#00CCFF","#9100FF"),6)) +
  geom_text(aes(label = paste(medals,type)), position=position_stack(vjust=0.5), size = 5, color = "#000000")

# Try making better country-specific colors
plotData = rankData
plotData$who = factor(rankData$who, levels=unique(rankData$who))
plotData$team = factor(rankData$team, levels=unique(rankData$team))

testLabel = paste(rankData$medals,rankData$type)
testLabel = gsub("Gold","G               ",testLabel)
testLabel = gsub("Silver","S",testLabel)
testLabel = gsub("(\\d+) Bronze","               \\1 B", testLabel)
ggplot(rankData, aes(x=factor(who, levels=unique(who)), y=points, fill=factor(team, levels = unique(team)), alpha=factor(type, levels = c("Bronze","Silver","Gold")))) +
  geom_col(color = "black") +
  scale_alpha_manual(values = c(0.3,0.65,1)) +
  scale_fill_manual(values = c("#ffce00","#009246","#cc092f",
                               "#dd290f","#477050","#003893",
                               "#012169","#d52b1e","#5eb6e4",
                               "#d52b1e","#21468b","#00a651",
                               "#bc002d","#00459b","#ffd521",
                               "#00008b","#ed2939","#8dd4f4")) +
  #scale_fill_manual(values = rep(c("#AAFF00","#00CCFF","#9100FF"),6)) +
  geom_text(aes(label = testLabel), position=position_stack(vjust=0.5), size = 5, color = "#000000")



# FINISHED CODE

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
  for (i in 1:(length(unique(rankData$scrape)))) {
    url = paste0(rankData$scrapeRoot[1],rankData$scrape[(i*3-2)])
    rankData$medals[(i*3-2):(i*3)] = getCountryMedals(url)
  }
  rankData$points = rankData$medals*c(3,2,1,6,4,2,20,10,5)
  return(rankData)
}


## Make basic data table
rankData = data.frame(
  who = c(rep("Q",9),rep("Emily",9),rep("Becky",9),rep("Katie",9),rep("Tom",9),rep("Denise",9)),
  team = c(rep("Germany",3),rep("Italy",3),rep("Angola",3),
           rep("China",3),rep("Hungary",3),rep("Nepal",3),
           rep("UK",3),rep("Canada",3),rep("San Marino",3),
           rep("Russia",3),rep("Netherlands",3),rep("Refugees",3),
           rep("Japan",3),rep("South Korea",3),rep("Bos n Herz",3),
           rep("Australia",3),rep("France",3),rep("Guam",3)),
  scrape = c(rep("noc-profile-germany.htm",3),rep("noc-profile-italy.htm",3),rep("noc-profile-angola.htm",3),
             rep("noc-profile-china.htm",3),rep("noc-profile-hungary.htm",3),rep("noc-profile-nepal.htm",3),
             rep("noc-profile-great-britain.htm",3),rep("noc-profile-canada.htm",3),rep("noc-profile-san-marino.htm",3),
             rep("noc-profile-roc.htm",3),rep("noc-profile-netherlands.htm",3),rep("noc-profile-refugee-olympic-team.htm",3),
             rep("noc-profile-japan.htm",3),rep("noc-profile-republic-of-korea.htm",3),rep("noc-profile-bosnia-herzegovina.htm",3),
             rep("noc-profile-australia.htm",3),rep("noc-profile-france.htm",3),rep("noc-profile-guam.htm",3)),
  scrapeRoot = "https://olympics.com/tokyo-2020/olympic-games/en/results/all-sports/"
)
rankData$medals = rep(1,length(rankData$who))
rankData$type = rep(c("Gold","Silver","Bronze"),length(rankData$who)/3)
rankData$points = rankData$medals*c(3,2,1,6,4,2,20,10,5)
#Populate Data
rankData = updateMedalsAndPoints(rankData)

## Generate Plot
# Format the lables to prevent overlapping
testLabel = paste(rankData$medals,rankData$type)
testLabel = gsub("Gold","G               ",testLabel)
testLabel = gsub("Silver","S",testLabel)
testLabel = gsub("(\\d+) Bronze","               \\1 B", testLabel)

# Run GGPlot
ggplot(rankData, aes(x=factor(who, levels=unique(who)), y=points, fill=factor(team, levels = unique(team)), alpha=factor(type, levels = c("Bronze","Silver","Gold")))) +
  
  # Black border for all segments
  geom_col(color = "black") +
  
  # Set opacity for Bronze, Silver, Gold
  scale_alpha_manual(values = c(0.3,0.65,1),
                     name = "Medal Type") +
  
  # Assign custom colors to each country
  scale_fill_manual(values = c("#ffce00","#009246","#cc092f",
                               "#dd290f","#477050","#003893",
                               "#012169","#d52b1e","#5eb6e4",
                               "#d52b1e","#21468b","#00a651",
                               "#bc002d","#00459b","#ffd521",
                               "#00008b","#ed2939","#8dd4f4"),
                    name = "Team") +
  
  # Place labels with customizations from earlier
  geom_text(aes(label = testLabel), position=position_stack(vjust=0.5), size = 5, color = "#000000") +
  
  # Adjust placement of legend
  theme(legend.position="bottom", legend.direction="horizontal")





