#############################################
# Title: 50_shiny_dashboard
# Author: Abhinav Krishnan
# Date: 07 December 2022
# Inputs: in_dt_13f
# Outputs: 
#############################################

# =============================================================================
# =============================== Description =================================
# =============================================================================

    # OKay so this dashboard needs to do the following:
        # Display the datatable
        # illustrate the purchases made by each entity
        # illustrate change in purchases over time by entity and 

# =============================================================================
# ================================= Setup =====================================
# =============================================================================

# ============== #
# Load Libraries
# ============== #

library(data.table)
library(ggplot2)
library(stringr)

library(shiny)
    
# ============== #
# Load Data
# ============== #

in_dt_13f <- readRDS(file = "dt_13f")
readRDS(file = "frqtbl_shares")

entity_names <- unique(in_dt_13f[, "Entity"])

# =============================================================================
# =============================== Workspace ===================================
# =============================================================================

    gg_base <- ggplot(in_dt_13f)

    #gg_base + geom_bar(mapping = aes(x = )

# =============================================================================
# ================================= UI ======================================
# =============================================================================


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("University Endowment Investments Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("entity",
                        "Name of University",
                        choices = entity_names)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("tableEntities")
        )
    )
)

# =============================================================================
# =============================== Server =====================================
# =============================================================================


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$tableEntities <- renderTable(expr = in_dt_13f[])
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
