library(readxl)
library(readr)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(gridExtra)
library(ggrepel)
library(ggalt)
library(maps)
library(ggmap)
library(zipcode)

ui<-dashboardPage(
  dashboardHeader(title="Farmers' Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction",  tabName = "introduction",icon =icon("dashboard")),
      menuItem("Overview",tabName = "overview",icon =icon("dashboard"),
               menuSubItem("Distribution Graphs", tabName = "graph1"),
               menuSubItem("Correlation", tabName = "correlation")),
      menuItem("Comparison",  tabName = "graph2",icon =icon("dashboard")),
      menuItem("Map",  tabName = "map",icon =icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="introduction",
              fluidRow(
                box(title =strong("The Purpose of Farmers' Dashboard"),
                    h4("This dashboard is built to uncover the different characteristics of customers who have a home policy and customers who are cross sold."),width =12),
                box(title =strong("How to Use Farmers' Dashboard"),
                    h4("In the left navigation bar, you can choose the overview tab to understand the distribution of the important characteristics among customers in Arizona. You can also dive into the correlation between two chosen variables "),
                    h4("In the comparison tab, you can explore the difference of the selected characteristic between the two groups. "),
                    h4("In the map tab, an interactive part is provided so that you can gain an understanding of how our customers distribute across Arizona. Specifically, you may filter customers by whether they have auto insurance and whether they have umbrella insurance."),width =12),
                box(title =strong("Available Characteristics"),
                    h4("There are four categories of characteristics, which are policy, property condition, safeness, and client. You can select the category first in the first drop-down menu, then choose the variable you want."),width =12)
                
              )
      ),
      
      ######################### Correlation starts ########################
      tabItem(
        tabName = "correlation",
        ## Variable Choose Box
        fluidRow(
          
          ## Variable Choose
          box(title = strong("Detect the relationship between two chosen variables: "),
              status = "primary",
              collapsible = TRUE,
              solidHeader = TRUE,
              width= 12,
              fluidRow(
                column(width = 10,
                       fluidRow( 
                         column(3, boxPad("        ")),
                         column(3, offset = 1, boxPad(strong("Category"),align = "center")),
                         column(3, boxPad(strong("Variable"),align = "center"))
                       ),
                       fluidRow(
                         column(3, offset = 1, boxPad(strong("Variable 1"),align = "right")
                         ),
                         column(3, uiOutput("choose_cat_1")),
                         column(3, uiOutput("choose_var_1"))
                       ),
                       fluidRow(
                         column(3, offset = 1, boxPad(strong("   Variable 2"),align = "right")),
                         column(3, uiOutput("choose_cat_2")),
                         column(3, uiOutput("choose_var_2"))
                       )
                )
              ),
              fluidRow(
                hr(),
                column(5,br()),
                column(3,
                       uiOutput("actionbutton")),
                column(5,br())
              )
          ),
          ## Plot
          box(title = strong("Result"),
              status = "primary",
              width = 12,
              plotOutput("E_plot1")
          )
          
        )
      ),
      ##################### End ##########################
      ################ Distribution Graphs Starts#########################
      
      tabItem(
        tabName = 'graph1',
        fluidRow(
          box(
            title = strong("Explore distribution of the chosen variable: "),
            status = "primary",
            collapsible = TRUE,
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              width = 10,
              column(3, boxPad("      ")),
              column(3,
                     offset = 1,
                     pickerInput(inputId = "category", 
                                 label = "Category", 
                                 choices = c("Policy","Property Safeness","Property Condition","Client")
                     ),
                     uiOutput("vari")
              )
            )
          ),
          ## outputID: c_plot,c_table
          box(width = 12,
              title = strong("Result"),
              status = "primary",
              column(9,
                     plotOutput("c_plot")
              ),
              column(2,
                     tableOutput("c_table")     
              )
          )
        )
      ),
      ##################### End ##########################
      
      ################ Comparison Graphs Start#########################
      tabItem(
        tabName = 'graph2',
        fluidRow(
          box(
            title = strong("Explore the difference between customers with/without auto insurance: "),
            status = "primary",
            collapsible = TRUE,
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              width = 10,
              column(3, boxPad("      ")),
              column(3,
                     offset = 1,
                     pickerInput(inputId = "X_choose_cat", 
                                 label = "Category", 
                                 choices = c("Policy","Property Safeness","Property Condition","Client")),
                     uiOutput(outputId = 'X_choose_var')
              )
            ))
        ),
        box(width = 14,
            title = strong("Result"),
            status = "primary",
            plotOutput(outputId = 'X_plot')
        )
      )
      
      ,
      ##################### End ##########################
      
      ################ Map Starts ################
      tabItem(
        tabName = 'map',
        fluidRow(
          box(
            title = strong("Explore the difference in regions of Arizona: "),
            status = "primary",
            collapsible = TRUE,
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              width = 10,
              column(4,
                     
                     pickerInput(inputId = "number.of.discount", 
                                 label = "Total no. of policy discounts", 
                                 choices = c("No filtering by no. of discounts" = 'NA', "0 policy discounts" = 0,"1 policy discount" = 1,
                                             "2 policies' discounts" = 2,
                                             "3 policies' discounts" = 3,"4 policies' discounts" = 4,"5 policies' discounts"= 5,
                                             "6 policies' discounts" = 6,"7 policies' discounts" = 7,"8 policies' discounts" = 8,
                                             "9 policies' discounts" = 9)
                     )
              ),
              column(4,
                     pickerInput(inputId = "HM_UMB_DISC_IND", 
                                 label = "Home/Umbrella Discount", 
                                 choices = c("Yes" = "Y","No"= "N","No filtering by H/U discount" = "Ni")
                     )
              ),
              column(4,
                     pickerInput(inputId = "AH_DISC_IND", 
                                 label = "Customers With/Without Auto", 
                                 choices = c("Customers with auto" = "Y",
                                             "Customers without auto" = "N",
                                             "No filtering by H/A discount" = "nf")
                     )
                     
              )
            ),
            fluidRow(
              width = 10,
              column(4, boxPad("      ")),
              column(5, textOutput("Takeaway1"))
              
            )
            
          )
        ),
        box(width = 14,
            height = 800,
            title = strong("Result"),
            status = "primary",
            plotOutput(outputId = 'plot')
        )
        
      )
    )
  ))
####### map ends #########