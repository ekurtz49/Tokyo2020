#
# All code done by Ryan. Emily attempted to put into Shiny context.

library(shiny)
library(ggplot2)
library(rvest)
source("helpers.R")





# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ## Make basic data table
  rankData = data.frame(
    who = c(rep("USA",9),rep("Q",9),rep("Emily",9),rep("Becky",9),rep("Katie",9),rep("Tom",9),rep("Denise",9)),
    team = c(rep("USA",3),rep("PH1",3),rep("PH2",3),
             rep("Germany",3),rep("Italy",3),rep("Angola",3),
             rep("China",3),rep("Hungary",3),rep("Nepal",3),
             rep("UK",3),rep("Canada",3),rep("San Marino",3),
             rep("Russia",3),rep("Netherlands",3),rep("Refugees",3),
             rep("Japan",3),rep("South Korea",3),rep("Bos n Herz",3),
             rep("Australia",3),rep("France",3),rep("Guam",3)),
    scrape = c(rep("noc-profile-united-states.htm",3),rep("noc-profile-angola.htm",3),rep("noc-profile-angola.htm",3),
               rep("noc-profile-germany.htm",3),rep("noc-profile-italy.htm",3),rep("noc-profile-angola.htm",3),
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
  # Initialize a reactive variable
  rctvStuff = reactiveValues(newDataRequested = TRUE)
   
  
  ## Manual Update
  observeEvent(input$refreshData,{
    # Run update
    rankData = updateMedalsAndPoints(rankData)
    # Toggle this variable to wake up relevant output functions
    rctvStuff$newDataRequested = FALSE
    rctvStuff$newDataRequested = TRUE
  })
  
  
  ## Plotting Function
  output$mainBarChart <- renderPlot({
    # Enable manual refresh
    if (rctvStuff$newDataRequested == FALSE) {return()}
    
    # Record time of refresh
    refreshTime = Sys.time()
    attr(refreshTime, "tzone") = "America/New_York"
    
    # Format the lables to prevent overlapping
    testLabel = paste(rankData$medals,rankData$type)
    testLabel = gsub("Gold","G               ",testLabel)
    testLabel = gsub("Silver","S",testLabel)
    testLabel = gsub("(\\d+) Bronze","               \\1 B", testLabel)
    
    # Run GGPlot
    ggplot(rankData, aes(x=factor(who, levels=unique(who)), y=points, fill=factor(team, levels = unique(team)), alpha=factor(type, levels = c("Bronze","Silver","Gold")))) +
      
      # Black border for all segments
      geom_col(color = "black") +
      
      # Assign custom colors to each country, and format legend
      scale_fill_manual(values = c("#00459b","#000000","#000000",
                                   "#ffce00","#009246","#cc092f",
                                   "#dd290f","#477050","#003893",
                                   "#012169","#d52b1e","#5eb6e4",
                                   "#d52b1e","#21468b","#00a651",
                                   "#bc002d","#00459b","#ffd521",
                                   "#00008b","#ed2939","#8dd4f4"),
                        name = "Team",
                        guide = guide_legend(nrow = 3, title.position = "top", title.hjust = 0.5, title.theme = element_text(face = "bold"))) +
      
      # Set opacity for Bronze, Silver, Gold, and format legend
      scale_alpha_manual(values = c(0.3,0.65,1),
                         name = "Medal Type",
                         guide = guide_legend(title.position = "top", title.hjust = 0.5, title.theme = element_text(face = "bold"))) +
      
      # Place labels with customizations from earlier
      geom_text(aes(label = testLabel), position=position_stack(vjust=0.5), size = 5, color = "#000000") +
      #geom_text(aes(label = stat(score), group = who), stat = 'summary', fun = sum, vjust = -1) +
    
      # Adjust axis labels and assign title
      ggtitle("Current Scoreboard", subtitle = paste0("(As of ", refreshTime," ET)")) +
      xlab("Player") +
      ylab("Points") +
      
      # Adjust placement of legend and general formatting
      theme(legend.position="bottom", legend.direction="horizontal",
            plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size=16),
            axis.title = element_text(size=16, face = "bold")
            )
    
    
  })
  
  
  ## Note last refresh time
  # output$lastUpdate <- renderText({
  #   if (rctvStuff$newDataRequested == FALSE) {return()}
  #   paste("Last Refresh:",timestamp(prefix = "",suffix=""))
  # })
  
})
