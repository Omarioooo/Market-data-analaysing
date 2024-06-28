library(shiny)
library(dplyr)
library(cluster)
library(arules)
library(ggplot2)

# UI design
ui <- pageWithSidebar(
  headerPanel("Data Analayzing"),
  
  sidebarPanel(
    fileInput("file1", "Load your file", accept = ".csv"),
    actionButton("cleanbutton", "Clean the data"),
    uiOutput("newButtons") 
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Data set",
               dataTableOutput("Data_set")), 
      
      tabPanel("Visualization", 
               plotOutput("cash_and_credit_total"), 
               plotOutput("total_spending_by_age"),
               plotOutput("total_spending_by_city"), 
               plotOutput("total_spending_distribution")),
      
      tabPanel("Clustering",
               plotOutput("cluster_plot"),
               plotOutput("barplot_cluster"),
               dataTableOutput("cluster_table")),
      
      tabPanel("Association",
               tableOutput("Rules")
      )
    )
    )
) # the end of the UI page

# Server logic
server <- function(input, output, session) {
  
  # Initialize an empty reactive value to store cleaned data
  project <- reactiveVal(NULL)
  
  # Read the file of the data set
  ProjectData <- reactive({
          req(input$file1)
          read.csv(input$file1$datapath)
     })
  
  # Handling the clean button
  observeEvent(input$cleanbutton, {
    if (!is.null(ProjectData())) {
      cleaned <- unique(ProjectData())
      cleaned <- na.omit(cleaned)
      project(cleaned)  # Update the cleaned data
    }
  })
  
  # Display the data table
  output$Data_set <- renderDataTable({
     if (!is.null(project())){
          data <- project() 
     } else{
       data <-ProjectData()
    } 
    return(data)
  })
  
  # Display the pie_chart for comparing between cash and credit 
  output$cash_and_credit_total <- renderPlot({
    if (!is.null(project())) {
      # 1.calculate the percentage between cash and credit
      Payment <- table(project()$paymentType)
      Payment_percentage <- round(100 * Payment / sum(Payment), 1)
      
      # 2.create the pie cart
      pie(Payment,
          labels = paste0(Payment_percentage, "%"),
          main = "Total of Cash and Credit",
          col = c("aquamarine", "#FF6666"))
      
      # 3.create the legend for some info about the pie_cart
      legend("bottomright", legend = c("Cash", "Credit"), fill = c("aquamarine", "#FF6666"))
    }
  })
  
  # Display the scatter_plot to find the relation between age and total spending
  output$total_spending_by_age <- renderPlot({
    if (!is.null(project())) {
      
      # Step 1: Group the total of spending money depending on the ages
      total <- project() %>%
        group_by(age) %>%
        summarise(total_spent = sum(total))
      
      # Step 2: Create scatter for displaying the relation between the age and the spending
      plot(x =  total$age, 
           y = total$total_spent,
           main = "Relation between age and spending money",
           xlab = "Age",
           ylab = " ",
           ylim = c(750000,1750000),
           xlim = c(20,60),
           las = 2,
           col = "red", 
           pch = 19,
           cex.axis = 0.8, 
           cex.main = 1.3,
           cex = 1.4)
    }   
  })
  
  # Display a plot comparing between the total spending for each city
  output$total_spending_by_city <- renderPlot({
    if(!is.null(project())){
      
      # Step 1: Group by city and calculate total spending
      total_spending_with_cities <- project() %>%
        group_by(city) %>%
        summarise(total_spent = sum(total)) %>%
        arrange(desc(total_spent))  # Corrected arrange function call
      
      # Step 2: create a Plot for dispalying the relation between cities and total spending
      barplot(height = total_spending_with_cities$total_spent,
              names.arg = total_spending_with_cities$city,
              main = "Total Spending by each city",
              col = rainbow(nrow(total_spending_with_cities)),
              cex.names = 0.8,
              cex.axis = 0.8 ,
              las = 2 )
    }
  })
  
  # Display total spending distribution
  output$total_spending_distribution <- renderPlot({
    if(!is.null(project())){
      
      boxplot( x = project()$total, 
               main = "Distribution of total spending",
               col = "green",
               ylim = c(0,2500),
               las = 2,
               length = 7)
    }
  })  
  
  # create buttons for clustering and assoctiation
  observeEvent(input$cleanbutton , {
    if(!is.null(project())){
         output$newButtons <- renderUI({
           # create the list of some buttons
         tagList(
          selectInput("numclusters", "Number of clusters", choices = c(2, 3, 4), selected = 2),
         sliderInput("support_slider", "Support values ", min = 0.001, max = 1, value = 0.03),
         sliderInput("confidece_slider", "confidence values", min = 0.001, max = 1, value = 0.4))
           })
      }
    })
  
  # K_Means...
  # Collecting the data for clustering
  display <- reactive({
    if (!is.null(project())) {
             project() %>%
             group_by(customer, age) %>%
             summarise(Total_Spending = sum(total))
    }
  })
  
  # Perform k-means clustering
  k_means_model <- reactive({ 
    if (!is.null(display())) {
      kmeans(display()[, c("age", "Total_Spending")], centers = input$numclusters) 
    }
    
  })
  
  
  # Plot clusters using ggplot2
  output$cluster_plot <- renderPlot({
    if (!is.null(k_means_model())) {
      # Generate a random colors for clustering
      cluster_colors <- rainbow(input$numclusters)
   
         
      # Create a ggplot for clusters groups
      cluster_plot <- ggplot(data = display(),
                             aes(x = age, y = Total_Spending,
                                 color = factor(k_means_model()$cluster))
                             )+
                      geom_point(size=3) +
                      scale_color_manual(values = cluster_colors , name = "Cluster") +
                      labs(title = "Clusters", x = " ", y = " ") +
                      xlim(20, 60)+
                      theme_minimal()
      
      # Print the plot
      print(cluster_plot)
    }
  })
  
  
  # plot clusters using barplot
  output$barplot_cluster <- renderPlot({
    
    # Generate a random colors for clustering
    cluster_colors <- rainbow(input$numclusters)
    
    barplot(height = display()$Total_Spending,
            names.arg = display()$customer,
            col = cluster_colors[k_means_model()$cluster],
            main = "Sum of Total Spending to Customer",
            cex.names = 0.9,
            xlab = "",
            ylab = "",
            las = 2)
   })
  
  # Table
  output$cluster_table <- renderDataTable({ 
              display()
      })
  
  # Association...
  # Perform Association rules
  apriori_model <- reactive({
    
    # Ensure project data is available and not NULL
  if (!is.null(project())) {
              # Transform the file from CSV to transaction
          transactions_data <- strsplit(project()$items, ",")
      
              # Perform the Apriori algorithm
          rules <- apriori(transactions_data, 
                           parameter = list(support = input$support_slider,
                                         confidence = input$confidece_slider,
                                         minlen=2))
       
            # convert the transaction data type to a DF
          rules_df <- as(rules, "data.frame")
                  return(rules_df)
  }else{      
        return(NULL)  # Return NULL if project data is not available
    }
  })

  # create the table to display the rules of the Apriori algorithm
  output$Rules <- renderTable({ 
    # Get the association rules using reactive expression
    rules <- apriori_model()
  })
  
  
} # the end of the server


shinyApp(ui, server)
