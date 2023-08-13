#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dashboardthemes)

#example for deployinh simple ML in R shiny
# User Interface

ui <- shinyUI(
    dashboardPage(
        dashboardHeader(title ="Customer Analysis Dashboard",titleWidth = "350px"),
        
        dashboardSidebar(
            
            sidebarMenu(
                style = "position: fixed; overflow: visible;",
                menuItem("Raw Data",tabName = "raw",icon=icon("table")),
                menuItem("EDA - Summary",tabName = "summary",icon=icon("lock-open",lib="font-awesome")),
                menuItem("EDA - Plots",tabName = "plots",icon=icon("bar-chart-o")),
                menuItem("Cluster determination",tabName = "clusters",icon=icon("chart-area",lib="font-awesome")),
                menuItem("Clustering Results",tabName= "kmeans",icon=icon("users",lib="font-awesome")),
                menuItem("Clustering Results (PAM)",tabName= "pam",icon=icon("users",lib="font-awesome")),
                menuItem("Clustered Data",tabName = "finaldata",icon=icon("table"))
            )),
        dashboardBody(
          ### changing theme
          shinyDashboardThemes(
            theme = "blue_gradient"
          ),
          
            tabItems(
                tabItem(tabName = "plots",h1("Exploratory Analysis - Plots"),fluidRow(
                    box(plotOutput("barplot")),
                    box(plotOutput("histogram1"))
                ),
                fluidRow(
                    box(plotOutput("histogram2")),
                    box(plotOutput("barplts1"))
                ),
                
                ),
                tabItem(tabName = "raw",h1("Customer Data"),fluidRow(column(5,tableOutput("rawdata")))),
                tabItem(tabName = "summary",h1("Data Summary"),
                        fluidRow(
                            box(title = "Attribute", 
                                status = "primary", 
                                solidHeader = TRUE,
                                width = 6,
                                selectInput("informedDset", label="Select Category",
                                            choices = colnames(data), selected = "Work_Experience")
                                ),
                            
                            box(
                                title = "Attribute Statistics", 
                                status = "warning", 
                                solidHeader = TRUE,
                                width = 6,
                                height = 142,
                                verbatimTextOutput("summaryDset")),
                            
                            box(title = "Attribute NC", 
                                status = "primary", 
                                solidHeader = TRUE,
                                width = 6,
                                selectInput("informedCat", label="Select Categorical Variable",
                                            choices = list('Gender','Ever_Married','Graduated',
                                                           'Profession','Spending_Score'), selected = "Gender")
                            )
                        ),
                        fluidRow(      
                            box(
                                title = "Attribute Statistics NC", 
                                status = "warning", 
                                solidHeader = TRUE,
                                width = 12,
                                height = 160,
                                verbatimTextOutput("summaryCat"))
                        )
                        ),
             
                tabItem(tabName = "clusters",h1("Number of Clusters"),
                        fluidRow(
                            box(h2("Elbow Method"),plotOutput("method1")),
                            box(h2("Average silhouette method"),plotOutput("method2"))),
                        fluidRow(  
                            box(h2("Gap Statistic method"),plotOutput("method3"))
                            # box(h2("NbClust method"),plotOutput("method4"))
                        )),
                tabItem(tabName = "kmeans",h1("Clustering Results"),
                        fluidRow(
                            box(plotOutput("clusterchart")),
                            box(plotOutput("clusterBoxPlot")),
                            box(sliderInput("clustnum","Number of clusters",1,10,4)),
                            # box(selectInput(inputId = "ClusterName",
                            #                 label = "Select Clustering Algorithm",
                            #                 choices = c("K-means","PAM", "DBScan", "Hierarchical"),
                            #                 selected = "50 Free",
                            #                 width = "220px"
                            # ))
                        ),
                        fluidRow(
                            box(plotOutput("agevswxp_pt")),
                            box(plotOutput("fsvswxp_pt")),
                            box(plotOutput("agevsfs_pt")),
 
                        ),
                        
                        h2("K-means cluster vs each NC variables "),
                        fluidRow(
                          box(plotOutput("agebpt")),
                          box(plotOutput("workexbp")),
                          box(plotOutput("famsizebpt"))
                        )),
                tabItem(tabName = "pam",h1("PAM Clustering Results"),
                        fluidRow(
                            box(plotOutput("clusterchart_pam")),
                            box(plotOutput("clusterBoxPlot_pam")),
                            box(sliderInput("clustnum_pam","Number of clusters",1,10,4)),
                        ),

                        h2("PAM cluster vs each NC variables "),
                        fluidRow(
                            box(plotOutput("agebpt_pam")),
                            box(plotOutput("workexbp_pam")),
                            box(plotOutput("famsizebpt_pam"))
                        )),
                tabItem(tabName = "finaldata",h1("Clustered Data"),fluidRow(column(5,tableOutput("clustdata"))))
                
            ))))#,skin='green'))    
