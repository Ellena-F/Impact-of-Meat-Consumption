#install package
# install.packages('rworldmap')
# install.packages('shinydashboard')
# install.packages('geomnet')
# install.packages('countrycode')
# install.packages('PBSmapping')
# install.packages('plotrix')
# install.packages('packcircles')
# install.packages('svgPanZoom')
# install.packages('treemap')
# install.packages('GGally')

#import library 
library(shiny)
library(readxl)
library(rworldmap)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(viridis)
library(countrycode)
library(data.table)
library(plyr)
library(PBSmapping)
library(plotrix)
library(packcircles)
library(svgPanZoom)
library(treemap)
library(GGally)

require(shinydashboard)
#ui
##create and design dashboard with menu
ui <- dashboardPage( skin="purple",
                     dashboardHeader(title = "Impact of meat consumption",titleWidth = 310),
                     dashboardSidebar(width=310,
                                      sidebarMenu(
                                        menuItem("Welcome",tabName = "first",icon = icon('fas fa-paper-plane')),
                                        menuItem("Meat Consumption Level", tabName = "Meat", icon = icon("dashboard")),
                                        menuItem("Human Health", tabName = "Human", icon = icon("fas fa-user-md")),
                                        menuItem("Greenhouse Gases Emission", tabName = "Gases",icon=icon("fas fa-thermometer-three-quarters")),
                                        menuItem("Deforestation",tabName="Deforestation",icon=icon("fas fa-tree"))
                                      )
                     ),
                     dashboardBody(
                       tags$head(tags$style((HTML('.main-header .logo { font-family: "Geogia",Times,"Times New Roman",serif; font-weight:bold;font-size:20x;}')))),
                       tabItems(
                         # first tab content
                         tabItem(tabName = "first",
                                 fluidRow(box(title = "Welcome!",width=10,status = "primary",solidHeader = TRUE,
                                              strong('This is a project that shows the impact of meat consumption level.',style = "font-family: 'mono'; font-si20pt"),
                                              p('You can choose the area that you are interested from the',span("menu",style="color:blue"),style = "font-family: 'mono'; font-si20pt"),
                                              div('or follow the order on the menu for a better information flow.',style = "font-family: 'mono'; font-si20pt")#,
                                              #imageOutput('image')
                                              
                                 )),
                                 fluidRow(box(title="Meat consumption level over the year",width=10,solidHeader = TRUE, status = "primary",
                                              p('Meat consumption has grown rapidly over the past 50 years, 
                                                 from 71 million tonnes in 1961 to 318 million tonnes in 2014 globally, 
                                                 which is almost a 450% growth (Our World in Data). At the same time, 
                                                 global climate change is worsening each year since 1951, 
                                                 with the increasing carbon dioxide level and global land-ocean temperature (NASA, 2018). 
                                                 Moreover, according to World Health Organization (World Health Organisation, 2007), 
                                                 there is a globally increasing trend of chronic diseases that is related to diet and nutrition, 
                                                 such as cardiovascular diseases (heart attack, stroke) and cancer. 
                                                 Therefore, in order to determine the solutions for these issues we will first have to 
                                                 identify the main cause for these problems. ')))),
                         #second tab content
                         tabItem(tabName="Meat",
                                 
                                 fluidRow( box(title="Meat Consumption level",
                                               width=10,solidHeader = TRUE,
                                               status="primary",
                                               #output map
                                               withSpinner(plotOutput("meat_map",height=500,width='auto')),
                                               #use slider to put different year as input
                                               sliderInput('year','Year:',1990,2018,2016)
                                 ))),
                         
                         
                         
                         #third tab content
                         tabItem(tabName="Human",
                                 fluidRow( box(title="Cancer Rate",collapsible=TRUE,
                                               width=10,solidHeader = TRUE,
                                               status="primary",
                                               withSpinner(plotOutput("cancer_map",height = 500,width='auto')),
                                               sliderInput('year_cancer','Year:',1990,2018,2016))),
                                 fluidRow(box(title = "Cardiovascular disease rate",width=10,collapsible=TRUE,
                                              solidHeader = TRUE,status="primary",
                                              withSpinner(plotOutput("heart_map",height = 500,width = 'auto')),sliderInput('year_heart','Year:',1993,2016,2016))),
                                 fluidRow( box(title="correlation",width=8,solidHeader = TRUE,status="primary",
                                               #output correlation graph
                                               withSpinner(plotOutput("corr1",height = 500,width=500))
                                               
                                 )
                                 ),
                                 #text box
                                 fluidRow(box(width = 8,
                                              p('Correlation: To test the relationship between two variables/ or how they are related'),
                                              p('A correlation coefficient is a value to represent the relationship between the two,'),
                                              p('it has a value between -1 to 1, where >0 = a positive relationship and <0 = negative,'),
                                              p('0 = there is no relationship between the vairables.'),
                                              p('In other word, a correlation coefficient of 1 means that 1 unit increase in A will cause,
                                                 one unit increase in B.')
                                 )),
                                 fluidRow(box(width = 8,
                                              p('Therefore, in this case, there is a strong positive relationship between meat consunmption level and cancer rate,
                                                 as it has a 0.8 coefficient correlation.'),
                                              p('With a p-value < 2.2e-16 (for three factors), it indicates that there is a strong evidence against the null hypothesis, 
                                                 therefore it means the correlation between the two is significant.')
                                              
                                 ))),
                         
                         
                         
                         tabItem(tabName = "Gases",
                                 fluidRow(box(title="Greenhouse gases Emission from different sectors",width=10,collapsible=TRUE,solidHeader = TRUE,status="primary",
                                              selectInput("ghg",'Greenhouse gases Emission:',
                                                          c("Nitrous Oxide Emission"="N2O_data","Methane"= "methane_data")),
                                              #output area graph
                                              withSpinner(plotOutput("area1",height= 500,width='auto')))),
                                 
                                 fluidRow(box(title="Greenhouse Gases Emission from agriculture products",align="middle",width='auto',solidHeader = TRUE,status="primary",
                                              #add zoom in function for packed bubble
                                              svgPanZoomOutput("bubble1",height = 500,width = 800)))),
                         
                         tabItem(tabName = "Deforestation",
                                 fluidRow(box(title="Causes of deforestation",collapsible=TRUE,width=10,solidHeader = TRUE,status="primary",
                                              #output pie chart
                                              withSpinner(plotOutput("pie1",height=500,width = 'auto')),
                                              strong('One football field of rainforest is being cleared every second'))),
                                 
                                 fluidRow(box(title="land used for agriculture product",width=10,solidHeader = TRUE,status="primary",
                                              #output treemap
                                              withSpinner(plotOutput("tree1",height = 500,width = 'auto')))))
                         
                       )))
