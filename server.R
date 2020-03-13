#import library 
library(shiny)
library(readxl)
# install.packages('rworldmap')
library(rworldmap)
# install.packages('shinydashboard')
library(shinydashboard)
library(ggplot2)
library(dplyr)
# install.packages('geomnet')
# library(geomnet)
library(viridis)
# install.packages('countrycode')
library(countrycode)
library(data.table)
library(plyr)
# install.packages('PBSmapping')
library(PBSmapping)
# install.packages('plotrix')
library(plotrix)
# install.packages('packcircles')
library(packcircles)
install.packages('svgPanZoom')
library(svgPanZoom)
# install.packages('treemap')
library(treemap)
# install.packages('GGally')
library(GGally)
require(shinydashboard)

#import dataset
defor_data <- read_excel("cause of deforestation in Amazon.xlsx")
N2O_data <- read_excel("edited_Nitrous_oxide_emission.xlsx")
bubble = read.csv('Edited_ghg_food.csv')
tree = read.csv('Edited_land_use.csv')
dataset1 = read.csv('edited_lv_of_meat_consumption.csv')
cancer = read.csv('Edited_number_of_people_with_cancer.csv')
heart = read.csv('Edited_Cardiovascular_disease.csv')
methane_data <- read_excel("edited_Methane.xlsx")
heart_can_meat1=read.csv('heart_can_meat1.csv')

server <- function(input, output) {
  #take out rows from dataset 
  #rename columns
  #calculate correlation and plot correlation graph
  output$corr1 <- renderPlot({
    meat1<-heart_can_meat1[,1]
    cancer1<-heart_can_meat1[,2]
    heart1<-heart_can_meat1[,4]
    colnames(heart_can_meat1)[1]<-'Meat consumption'
    colnames(heart_can_meat1)[2]<-'Cancer rate'
    colnames(heart_can_meat1)[4]<-'Cardiovascular disease rate'
    ggcorr(heart_can_meat1,palette = 'RdBu',label=TRUE,name='Correlation')
  })
  
  #plot 3D pie chart with % as label
  #add legend
  output$pie1 <- renderPlot({
    
    pielabels<-paste(defor_data$percentage,"%",sep="")
    pie3D(defor_data$percentage,labels=pielabels,explode=0.1,main="Factors that contribute to deforestation")
    par(xpd=TRUE)
    legend(1,-0.5,c(defor_data$`Causes of deforestation in Amazon`),
           title ='Factors contribute to deforestation',
           cex=0.6,width200,fill=rainbow(length(defor_data$percentage)))
    
  })
  
  #plot area graph
  #take different greenhouse gases chose by users as input
  output$area1 <- renderPlot({
    
    ggplot() +
      geom_area(data=get(input$ghg),aes(x=Year,y=Value,fill=Sectors),alpha=0.8)+
      
      theme_classic()+ 
      xlab("Year")+ylab("Greenhouse Gases Emission")+
      labs(title = "Greenhouse gases emission",
           #subtitle = "Nirtous oxide",
           caption = "Data from edited_Nitrous_oxide_emission.xlsx")
  })
  
  #plot packed bubble
  #with zoom in function
  #create label 
  output$bubble1 <- renderSvgPanZoom({
    
    bubble_name=paste(bubble$Meat.Type, "\n", round(bubble$Greenhouse.gases.per.kcal,2))
    
    # Generate the layout
    packing <- circleProgressiveLayout(bubble$Greenhouse.gases.per.kcal, sizetype='area')
    bubble = cbind(bubble, packing)
    
    dat.gg <- circleLayoutVertices(packing, npoints=50)
    
    # Make the plot with a few differences compared to the static version:
    p<-ggplot() + 
      geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
      
      geom_text(data = bubble, aes(x, y, size=Greenhouse.gases.per.kcal,label =bubble_name)) +
      theme_void() + 
      theme(legend.position="none") +
      coord_equal()+
      labs(
        title = "Greenhouse gases emission",
        subtitle = "From different types of agriculture produce", 
        caption = "Data: INSEE | Creation: Ellena Fung | r-graph-gallery.com"
      )+
      theme(
        text = element_text(color = "#22211d"), 
        plot.background = element_rect(fill = "#FFFFFF", color = NA), 
        panel.background = element_rect(fill = "#FFFFFF", color = NA), 
        
        plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
        plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm")))
    svgPanZoom(p,controlIconsEnabled = T)
  })
  
  #plot treemap
  output$tree1 <-renderPlot({
    
    
    colnames(tree)[2]<-"value"
    
    treemap(tree,
            index="Meat.type",
            vSize="value",
            
            
            palette = "Spectral",
            title = "Land used to produce different type of Agriculture products",
            
            position.legend = "right")
  })
  
  #plot map with users chosen year as input 
  #data wrangling process: get country name and longitute and latitude
  output$meat_map <- renderPlot({
    
    consumption_in_2016<-dataset1[dataset1$TIME == input$year,]
    
    
    data_map = joinCountryData2Map(consumption_in_2016, joinCode = 'ISO3', nameJoinColumn = 'LOCATION')
    data_map_poly= fortify(data_map)
    myCodes<-(consumption_in_2016$LOCATION)
    cc<-countrycode(myCodes,"iso3c","country.name")
    
    consumption_in_2016["region"]<-NA
    consumption_in_2016$`region`<- cc
    consumption_in_2016$region[consumption_in_2016$region=="United States"]<- "USA"
    
    country<-map_data("world")
    
    mm<-merge(x=consumption_in_2016,y=country,by="region",all=TRUE)
    mm1<-mm[order(mm$order),]
    
    
    
    #**
    ggplot() + 
      geom_polygon(data =mm1,aes(x = long, y = lat, fill=Value,group = group),size=0,alpha=0.9)+  
      coord_fixed(1.3) +
      
      theme_void()+
      scale_fill_viridis(option="plasma",direction = -1,begin = 0,end=0.5) +
      guides(fill=guide_legend(title = expression(atop("Amount of consumption","(in thousand tonnes)"))))+
      labs(
        title = "Meat Consumption level",
        subtitle = "amount of consumption", 
        caption = "Data: INSEE | Creation: Ellena Fung | r-graph-gallery.com"
      ) +
      theme(
        text = element_text(color = "#22211d"), 
        plot.background = element_rect(fill = "#FFFFFF", color = NA), 
        panel.background = element_rect(fill = "#FFFFFF", color = NA), 
        legend.background = element_rect(fill = "#FFFFFF", color = NA),
        
        plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
        plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") )
        
      ) 
  })
  
  #plot map
  output$cancer_map<- renderPlot({
    cancer_2016<-cancer[cancer$year == input$year_cancer,]
    
    cancer_val_10000<-cancer_2016[,'value']/10000
    
    cancer_2016['value']<-NA
    cancer_2016$value<-cancer_val_10000
    
    cancer_join = joinCountryData2Map(cancer_2016, joinCode = 'NAME', nameJoinColumn = 'Entity')
    cancer_map_poly= fortify(cancer_join)
    colnames(cancer_map_poly)[6]<-"Entity"
    
    
    cancer_long<-merge(x=cancer_2016,y=cancer_map_poly,by="Entity",all=TRUE)
    cancer_final<-cancer_long[order(cancer_long$order),]
    
    #**
    ggplot() + 
      geom_polygon(data =cancer_final,aes(x = long, y = lat, fill=value,group = group),size=0,alpha=0.9)+  
      coord_fixed(1.3) +
      theme_void()+
      scale_fill_viridis(option = "viridis",direction = -1,begin = 0,end = 0.3) +
      guides(fill=guide_legend(title = expression(atop("Number of people diagnosed","(in ten thousand)"))))+
      labs(
        title = "Cancer rate",
        subtitle = "Number of people diagnosed", 
        caption = "Data: INSEE | Creation: Ellena Fung "
      ) +
      theme(
        text = element_text(color = "#22211d"), 
        plot.background = element_rect(fill = "#FFFFFF", color = NA), 
        panel.background = element_rect(fill = "#FFFFFF", color = NA), 
        legend.background = element_rect(fill = "#FFFFFF", color = NA),
        
        plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
        plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") )
        
        
      ) 
  })
  
  #plot map
  output$heart_map<- renderPlot({
    heart_2016<-heart[heart$year == input$year_heart,]
    colnames(heart_2016)[3]<-"value"
    
    heart_val_10000<-heart_2016[,'value']/10000
    heart_2016["value"]<-NA
    heart_2016$value<-heart_val_10000
    
    
    heart_join = joinCountryData2Map(heart_2016, joinCode = 'NAME', nameJoinColumn = 'country')
    heart_map_poly= fortify(heart_join)
    colnames(heart_map_poly)[6]<-"country"
    
    
    heart_long<-merge(x=heart_2016,y=heart_map_poly,by="country",all=TRUE)
    heart_final<-heart_long[order(heart_long$order),]
    
    #**
    ggplot() + 
      geom_polygon(data =heart_final,aes(x = long, y = lat, fill=value,group = group),size=0,alpha=0.9,show.legend =TRUE)+  
      coord_fixed(1.3) +
      theme_void()+
      scale_fill_viridis(option="magma",direction = -1,begin = 0.2,end=0.7)+
      guides(fill=guide_legend(title = expression(atop("Number of people diagnosed","(in ten thousand)"))))+
      
      
      labs(
        title = "Cardiovascular disease rate ",
        subtitle = "Number of people diagnosed in 2016", 
        caption = "Data: INSEE | Creation: Ellena Fung | r-graph-gallery.com"
      ) +
      theme(
        text = element_text(color = "#22211d"), 
        plot.background = element_rect(fill = "#FFFFFF", color = NA), 
        panel.background = element_rect(fill = "#FFFFFF", color = NA), 
        legend.background = element_rect(fill = "#FFFFFF", color = NA),
        
        
        plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
        plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") )
        
        
      )
  })
  
  #show image
  output$image <- renderImage({
    
    #return(
    list(
      src = 'earth-globe.png',
      filetype = 'png'
      #)
    )})
  
  
  
}

# server <- shinyServer(function(input, output){})
