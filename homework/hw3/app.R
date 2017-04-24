library(shiny)
library(ggplot2)
library(reshape2)
library(lubridate)

setwd("~/Desktop/Facebook_metrics/")

facebook <- read.csv("dataset_Facebook.csv", sep = ";", stringsAsFactors = F)
facebook <- facebook[complete.cases(facebook),]
facebook$Type <- factor(facebook$Type)
facebook$Category <- factor(facebook$Category)
facebook$Paid <- factor(facebook$Paid)
facebook$Post.Month <- factor(facebook$Post.Month, levels = 1:12, labels = month.abb, ordered = T)
facebook$Post.Weekday <- factor(facebook$Post.Weekday, levels = 1:7,
                                labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                           "Saturday", "Sunday"), ordered = T)
facebook$Post.Hour <- factor(facebook$Post.Hour, levels = 1:23, labels = 1:23, ordered = T)

# scatterplot matrix
# https://gastonsanchez.wordpress.com/2012/08/27/scatterplot-matrices-with-ggplot/
makePairs <- function(data) 
{
  grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
  grid <- subset(grid, x != y)
  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol], 
               x = data[, xcol], y = data[, ycol], data)
  }))
  all$xvar <- factor(all$xvar, levels = names(data))
  all$yvar <- factor(all$yvar, levels = names(data))
  densities <- do.call("rbind", lapply(1:ncol(data), function(i) {
    data.frame(xvar = names(data)[i], yvar = names(data)[i], x = data[, i])
  }))
  list(all=all, densities=densities)
}


ui <- fluidPage(
  headerPanel("Multivariate"),
  
  conditionalPanel("input.conditionedPanels==1",
                   sidebarPanel(width = 3,
                                selectInput("x_axis", "Select X Axis", colnames(facebook), selected = "Post.Hour"),
                                selectInput("y_axis", "Select Y Axis", colnames(facebook)),
                                selectInput("color", "Select color variable", colnames(facebook), selected = "Category"),
                                selectInput("size", "Select size variable", colnames(facebook), selected = "Type"),
                                sliderInput("bubble_size", "Change size of bubbles", 1, 15, 10, 1, ticks = F))
  ),
  
  conditionalPanel("input.conditionedPanels==2",
                   sidebarPanel(width = 3,
                                selectizeInput("scatter_cols", "Select variables to plot",
                                               names(sapply(facebook, is.numeric))[sapply(facebook, is.numeric)],
                                               multiple = T, selected = c("Page.total.likes", "Lifetime.Post.Total.Reach")),
                                selectInput("scatter_color", "Select color variable",
                                            names(sapply(facebook, is.factor))[sapply(facebook, is.factor)]))
  ),
  
  mainPanel(
    
    tabsetPanel(
      tabPanel("Bubble plot",
               plotOutput("bubble_plot", hover = hoverOpts(id ="bubble_plot_hover")),
               verbatimTextOutput("bubble_hover_info"),
               value = 1),
      tabPanel("Scatter plot",
               plotOutput("scatter_plot"),
               value = 2),
      tabPanel("Parallel coordinates plot",
               plotOutput("parallel_plot"),
               value = 3),
      id = "conditionedPanels"
    )
    
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ###############
  # BUBBLE PLOT #
  ###############
  output$bubble_plot <- renderPlot({
    ggplot(facebook, aes_string(x=input$x_axis,
                                y=input$y_axis,
                                color=input$color,
                                size=input$size)) +
      geom_point() +
      scale_size_discrete(range = c(1, input$bubble_size)) +
      theme_bw()
  })
  
  output$bubble_hover_info <- renderPrint({
    if(!is.null(input$bubble_plot_hover)){
      hover=input$bubble_plot_hover
      points <- nearPoints(facebook, hover, threshold = 50, maxpoints = 1, addDist = TRUE)
      if(nrow(points)>0){
        for(col in colnames(facebook)){
          cat(col, ": ", points[1,col], "\n", sep="")
        }
      }
    } else{
      cat("Hover on a point to see additional information.")
    }
  })
  
  ################
  # SCATTER PLOT #
  ################
  
  output$scatter_plot <- renderPlot({
    validate(
      need(length(input$scatter_cols) >= 2, label = "At least 2 variables")
    )
    gg1 = makePairs(facebook[,input$scatter_cols])
    
    # new data frame mega iris
    mega_facebook = data.frame(gg1$all, Type=rep(facebook[,input$scatter_color], length=nrow(gg1$all)))
    
    # pairs plot
    ggplot(mega_facebook, aes_string(x = "x", y = "y")) + 
      facet_grid(xvar ~ yvar, scales = "free") + 
      geom_point(aes(colour=Type), na.rm = TRUE, alpha=0.8) + 
      stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)), 
                   data = gg1$densities, position = "identity", 
                   colour = "grey20", geom = "line") +
      theme_bw() +
      xlab("") + ylab("")
  })
  
  ##################
  # PARALLEL LINES #
  ##################
  
  output$parallel_plot <- renderPlot({
    ggplot(facebook) +
      #geom_segment(aes(x=0.6, xend=1.6, y=X5YR_, yend=X10YR_), size=1, color="grey70") +
      #geom_segment(aes(x=1.85, xend=2.85, y=X10YR_, yend=X20YR_), size=1, color="grey70") +
      
      #geom_text(label=facebook$CancerType, y=df$X5YR_, x=rep.int(0.2, nrow(df)), size=4) +
      #geom_text(label=df$X5YR, y=df$X5YR_, x=rep.int(0.5, nrow(df)), size=4) +
      #geom_text(label=df$X10YR, y=df$X10YR_, x=rep.int(1.72, nrow(df)), size=4) +
      #geom_text(label=df$X20YR, y=df$X20YR_, x=rep.int(2.97, nrow(df)), size=4) +
      
      #geom_text(label="5 years", y=103, x=0.5, size=5) +
      #geom_text(label="10 years", y=103, x=1.72, size=5) +
      #geom_text(label="20 years", y=103, x=2.97, size=5) +
      
      #xlab("") + ylab("") + 
      #xlim(0, 3) +
      #ylim(0, 105) +
      
      theme(panel.background = element_blank(),
            panel.grid=element_blank(),
            axis.ticks=element_blank(),
            axis.text=element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=1))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
