#install.packages("cowplot")
#install.packages("googleway")
#install.packages("ggrepel")
#install.packages("ggspatial")
#install.packages("rnaturalearthdata")
#install.packages("libwgeom")
#install.packages("writexl")

#library("writexl")
#write_xlsx(mod_world,"C:\\Users\\Laptop_Dell\\Desktop\\world.xlsx")

SN <- read.csv("C:\\Users\\Laptop_Dell\\Desktop\\addata.csv", sep=";")



library("ggplot2")
library("sf")
library(dplyr)
library(ggthemes) # Load
library(ggplot2)
library(viridis)
library(shiny)
library("rnaturalearth")
library("rnaturalearthdata")
theme_set(theme_bw())


world <- ne_countries(scale = "medium", returnclass = "sf")


world <- world %>% 
  left_join(SN, by = c("name"="name"))


class(world)



ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
  labs(fill="Population")



ggplot(data = world) +
  geom_sf(aes(fill = gdp)) +
  scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
  labs(fill="GDP")





ggplot(data = world) +
  geom_sf(aes(fill = capita)) +
  scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
  labs(fill="GDP Per Capita")




ggplot(data = world) +
  geom_sf(aes(fill = birthrate)) +
  scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
  labs(fill="Fertility rate")


#north america
ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  coord_sf(xlim = c(-14.15, -180.12), ylim = c(5.65, 83.97), expand = FALSE)+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")


#south america
ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  coord_sf(xlim = c(-14.15, -115.12), ylim = c(-60.65, 15.97), expand = FALSE)+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")


#europe
ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  coord_sf(xlim = c(-40.15, 70.12), ylim = c(25.65, 83.97), expand = FALSE)+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")


#africa
ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  coord_sf(xlim = c(-35.15, 90.12), ylim = c(-40.65, 45), expand = FALSE)+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")


#asia
ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  coord_sf(xlim = c(40.15, 180.12), ylim = c(1.65, 83.97), expand = FALSE)+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

#oceania
ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  coord_sf(xlim = c(80.15, 182.12), ylim = c(-55.65, 10.97), expand = FALSE)+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")




#App2

# Define UI for app
ui2 <- fluidPage(
  
  # App title ----
  titlePanel("World in numbers"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      

      radioButtons("radio1", h3("Select theme"),
                   choices = list("Turbo" = 1, "Plasma" = 2),selected = 1),
      
      
      radioButtons("radio2", h3("Select parameter"),
                   choices = list("GDP" = 1, "Per Capita GDP" = 2,"Population" = 3,"Fertility" = 4),selected = 1),
      
      radioButtons("radio3", h3("Select region"),
                   choices = list("World" = 1, "North America" = 2,"South America" = 3,"Europe" = 4,"Africa & Middle east" = 5,"Asia" = 6,"Australia & Oceania" = 7),selected = 1),
      
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  ),
  
  h4("Projekt AiWD"),
  h4("Filip Kozieniec"),
  h4("WCY19IJ2S1"),
  img(src="https://promocja.wat.edu.pl/wp-content/uploads/2014/03/Logotyp_WAT4.jpg", width="30%"),
  
)


# Define server logic required to draw a histogram ----
server2 <- function(input, output) {
  
  
  output$distPlot <- renderPlot({
    
    library("ggplot2")
    library("sf")
    library(dplyr)
    library(ggthemes) # Load
    library(ggplot2)
    library(viridis)
    library(shiny)
    library("rnaturalearth")
    library("rnaturalearthdata")
    theme_set(theme_bw())
    
    
    radio1<-input$radio1
    radio2<-input$radio2
    radio3<-input$radio3
    
    SN <- read.csv("C:\\Users\\Laptop_Dell\\Desktop\\addata.csv", sep=";")
    
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    
    world <- world %>% 
      left_join(SN, by = c("name"="name"))
    
    
    class(world)
    
    if(radio1==1)
    {
      if(radio2==1)
      {
        
        if(radio3==1) #world
        {
      ggplot(data = world) +
        geom_sf(aes(fill = gdp)) +
        scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
        xlab("Longitude") + ylab("Latitude") +
        ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
        labs(fill="GDP")
      
        }
        
        else if(radio3==2) #north america
        {
          ggplot(data = world) +
            geom_sf(aes(fill = gdp)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP")+
            coord_sf(xlim = c(-14.15, -180.12), ylim = c(5.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==3) #south america
        {
          ggplot(data = world) +
            geom_sf(aes(fill = gdp)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP")+
            coord_sf(xlim = c(-14.15, -115.12), ylim = c(-60.65, 15.97), expand = FALSE)
          
        }
        
        else if(radio3==4) #europe
        {
          ggplot(data = world) +
            geom_sf(aes(fill = gdp)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP")+
            coord_sf(xlim = c(-40.15, 70.12), ylim = c(25.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==5) #africa
        {
          ggplot(data = world) +
            geom_sf(aes(fill = gdp)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP")+
            coord_sf(xlim = c(-35.15, 90.12), ylim = c(-40.65, 45), expand = FALSE)
          
        }
        
        else if(radio3==6) #asia
        {
          ggplot(data = world) +
            geom_sf(aes(fill = gdp)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP")+
            coord_sf(xlim = c(40.15, 180.12), ylim = c(1.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==7) #oceania
        {
          ggplot(data = world) +
            geom_sf(aes(fill = gdp)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP")+
            coord_sf(xlim = c(80.15, 182.12), ylim = c(-55.65, 10.97), expand = FALSE)
          
        }
        
          
        
      }
      else if(radio2==2)
      {
        
        if(radio3==1) #world
        {
          ggplot(data = world) +
            geom_sf(aes(fill = capita)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP per capita")
          
        }
        
        else if(radio3==2) #north america
        {
          ggplot(data = world) +
            geom_sf(aes(fill = capita)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP per capita")+
            coord_sf(xlim = c(-14.15, -180.12), ylim = c(5.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==3) #south america
        {
          ggplot(data = world) +
            geom_sf(aes(fill = capita)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP per capita")+
            coord_sf(xlim = c(-14.15, -115.12), ylim = c(-60.65, 15.97), expand = FALSE)
          
        }
        
        else if(radio3==4) #europe
        {
          ggplot(data = world) +
            geom_sf(aes(fill = capita)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP per capita")+
            coord_sf(xlim = c(-40.15, 70.12), ylim = c(25.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==5) #africa
        {
          ggplot(data = world) +
            geom_sf(aes(fill = capita)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP per capita")+
            coord_sf(xlim = c(-35.15, 90.12), ylim = c(-40.65, 45), expand = FALSE)
          
        }
        
        else if(radio3==6) #asia
        {
          ggplot(data = world) +
            geom_sf(aes(fill = capita)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP per capita")+
            coord_sf(xlim = c(40.15, 180.12), ylim = c(1.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==7) #oceania
        {
          ggplot(data = world) +
            geom_sf(aes(fill = capita)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP per Capita")+
            coord_sf(xlim = c(80.15, 182.12), ylim = c(-55.65, 10.97), expand = FALSE)
          
        }
        
        
      }
      
      else if(radio2==3)
      {
        
        if(radio3==1) #world
        {
          ggplot(data = world) +
            geom_sf(aes(fill = pop_est)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Population")
          
        }
        
        else if(radio3==2) #north america
        {
          ggplot(data = world) +
            geom_sf(aes(fill = pop_est)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Population")+
            coord_sf(xlim = c(-14.15, -180.12), ylim = c(5.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==3) #south america
        {
          ggplot(data = world) +
            geom_sf(aes(fill = pop_est)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Population")+
            coord_sf(xlim = c(-14.15, -115.12), ylim = c(-60.65, 15.97), expand = FALSE)
          
        }
        
        else if(radio3==4) #europe
        {
          ggplot(data = world) +
            geom_sf(aes(fill = pop_est)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Population")+
            coord_sf(xlim = c(-40.15, 70.12), ylim = c(25.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==5) #africa
        {
          ggplot(data = world) +
            geom_sf(aes(fill = pop_est)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Population")+
            coord_sf(xlim = c(-35.15, 90.12), ylim = c(-40.65, 45), expand = FALSE)
          
        }
        
        else if(radio3==6) #asia
        {
          ggplot(data = world) +
            geom_sf(aes(fill = pop_est)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Population")+
            coord_sf(xlim = c(40.15, 180.12), ylim = c(1.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==7) #oceania
        {
          ggplot(data = world) +
            geom_sf(aes(fill = pop_est)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Population")+
            coord_sf(xlim = c(80.15, 182.12), ylim = c(-55.65, 10.97), expand = FALSE)
          
        }
        
        
      }  
      
      
      else if(radio2==4)
      {
        
        if(radio3==1) #world
        {
          ggplot(data = world) +
            geom_sf(aes(fill = birthrate)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Fertility")
          
        }
        
        else if(radio3==2) #north america
        {
          ggplot(data = world) +
            geom_sf(aes(fill = birthrate)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Fertility")+
            coord_sf(xlim = c(-14.15, -180.12), ylim = c(5.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==3) #south america
        {
          ggplot(data = world) +
            geom_sf(aes(fill = birthrate)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Fertility")+
            coord_sf(xlim = c(-14.15, -115.12), ylim = c(-60.65, 15.97), expand = FALSE)
          
        }
        
        else if(radio3==4) #europe
        {
          ggplot(data = world) +
            geom_sf(aes(fill = birthrate)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Fertility")+
            coord_sf(xlim = c(-40.15, 70.12), ylim = c(25.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==5) #africa
        {
          ggplot(data = world) +
            geom_sf(aes(fill = birthrate)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Fertility")+
            coord_sf(xlim = c(-35.15, 90.12), ylim = c(-40.65, 45), expand = FALSE)
          
        }
        
        else if(radio3==6) #asia
        {
          ggplot(data = world) +
            geom_sf(aes(fill = birthrate)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Fertility")+
            coord_sf(xlim = c(40.15, 180.12), ylim = c(1.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==7) #oceania
        {
          ggplot(data = world) +
            geom_sf(aes(fill = birthrate)) +
            scale_fill_viridis_c(option = "turbo", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Fertility")+
            coord_sf(xlim = c(80.15, 182.12), ylim = c(-55.65, 10.97), expand = FALSE)
          
        }
        
        
      }  
      
      
      
      
    }
    else if(radio1 == 2)
    {
      
      if(radio2==1)
      {
        
        if(radio3==1) #world
        {
          ggplot(data = world) +
            geom_sf(aes(fill = gdp)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP")
          
        }
        
        else if(radio3==2) #north america
        {
          ggplot(data = world) +
            geom_sf(aes(fill = gdp)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP")+
            coord_sf(xlim = c(-14.15, -180.12), ylim = c(5.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==3) #south america
        {
          ggplot(data = world) +
            geom_sf(aes(fill = gdp)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP")+
            coord_sf(xlim = c(-14.15, -115.12), ylim = c(-60.65, 15.97), expand = FALSE)
          
        }
        
        else if(radio3==4) #europe
        {
          ggplot(data = world) +
            geom_sf(aes(fill = gdp)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP")+
            coord_sf(xlim = c(-40.15, 70.12), ylim = c(25.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==5) #africa
        {
          ggplot(data = world) +
            geom_sf(aes(fill = gdp)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP")+
            coord_sf(xlim = c(-35.15, 90.12), ylim = c(-40.65, 45), expand = FALSE)
          
        }
        
        else if(radio3==6) #asia
        {
          ggplot(data = world) +
            geom_sf(aes(fill = gdp)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP")+
            coord_sf(xlim = c(40.15, 180.12), ylim = c(1.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==7) #oceania
        {
          ggplot(data = world) +
            geom_sf(aes(fill = gdp)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP")+
            coord_sf(xlim = c(80.15, 182.12), ylim = c(-55.65, 10.97), expand = FALSE)
          
        }
        
        
        
      }
      else if(radio2==2)
      {
        
        if(radio3==1) #world
        {
          ggplot(data = world) +
            geom_sf(aes(fill = capita)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP per capita")
          
        }
        
        else if(radio3==2) #north america
        {
          ggplot(data = world) +
            geom_sf(aes(fill = capita)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP per capita")+
            coord_sf(xlim = c(-14.15, -180.12), ylim = c(5.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==3) #south america
        {
          ggplot(data = world) +
            geom_sf(aes(fill = capita)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP per capita")+
            coord_sf(xlim = c(-14.15, -115.12), ylim = c(-60.65, 15.97), expand = FALSE)
          
        }
        
        else if(radio3==4) #europe
        {
          ggplot(data = world) +
            geom_sf(aes(fill = capita)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP per capita")+
            coord_sf(xlim = c(-40.15, 70.12), ylim = c(25.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==5) #africa
        {
          ggplot(data = world) +
            geom_sf(aes(fill = capita)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP per capita")+
            coord_sf(xlim = c(-35.15, 90.12), ylim = c(-40.65, 45), expand = FALSE)
          
        }
        
        else if(radio3==6) #asia
        {
          ggplot(data = world) +
            geom_sf(aes(fill = capita)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP per capita")+
            coord_sf(xlim = c(40.15, 180.12), ylim = c(1.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==7) #oceania
        {
          ggplot(data = world) +
            geom_sf(aes(fill = capita)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="GDP per Capita")+
            coord_sf(xlim = c(80.15, 182.12), ylim = c(-55.65, 10.97), expand = FALSE)
          
        }
        
        
      }
      
      else if(radio2==3)
      {
        
        if(radio3==1) #world
        {
          ggplot(data = world) +
            geom_sf(aes(fill = pop_est)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Population")
          
        }
        
        else if(radio3==2) #north america
        {
          ggplot(data = world) +
            geom_sf(aes(fill = pop_est)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Population")+
            coord_sf(xlim = c(-14.15, -180.12), ylim = c(5.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==3) #south america
        {
          ggplot(data = world) +
            geom_sf(aes(fill = pop_est)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Population")+
            coord_sf(xlim = c(-14.15, -115.12), ylim = c(-60.65, 15.97), expand = FALSE)
          
        }
        
        else if(radio3==4) #europe
        {
          ggplot(data = world) +
            geom_sf(aes(fill = pop_est)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Population")+
            coord_sf(xlim = c(-40.15, 70.12), ylim = c(25.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==5) #africa
        {
          ggplot(data = world) +
            geom_sf(aes(fill = pop_est)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Population")+
            coord_sf(xlim = c(-35.15, 90.12), ylim = c(-40.65, 45), expand = FALSE)
          
        }
        
        else if(radio3==6) #asia
        {
          ggplot(data = world) +
            geom_sf(aes(fill = pop_est)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Population")+
            coord_sf(xlim = c(40.15, 180.12), ylim = c(1.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==7) #oceania
        {
          ggplot(data = world) +
            geom_sf(aes(fill = pop_est)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Population")+
            coord_sf(xlim = c(80.15, 182.12), ylim = c(-55.65, 10.97), expand = FALSE)
          
        }
        
        
      }  
      
      
      else if(radio2==4)
      {
        
        if(radio3==1) #world
        {
          ggplot(data = world) +
            geom_sf(aes(fill = birthrate)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Fertility")
          
        }
        
        else if(radio3==2) #north america
        {
          ggplot(data = world) +
            geom_sf(aes(fill = birthrate)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Fertility")+
            coord_sf(xlim = c(-14.15, -180.12), ylim = c(5.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==3) #south america
        {
          ggplot(data = world) +
            geom_sf(aes(fill = birthrate)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Fertility")+
            coord_sf(xlim = c(-14.15, -115.12), ylim = c(-60.65, 15.97), expand = FALSE)
          
        }
        
        else if(radio3==4) #europe
        {
          ggplot(data = world) +
            geom_sf(aes(fill = birthrate)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Fertility")+
            coord_sf(xlim = c(-40.15, 70.12), ylim = c(25.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==5) #africa
        {
          ggplot(data = world) +
            geom_sf(aes(fill = birthrate)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Fertility")+
            coord_sf(xlim = c(-35.15, 90.12), ylim = c(-40.65, 45), expand = FALSE)
          
        }
        
        else if(radio3==6) #asia
        {
          ggplot(data = world) +
            geom_sf(aes(fill = birthrate)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Fertility")+
            coord_sf(xlim = c(40.15, 180.12), ylim = c(1.65, 83.97), expand = FALSE)
          
        }
        
        else if(radio3==7) #oceania
        {
          ggplot(data = world) +
            geom_sf(aes(fill = birthrate)) +
            scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
            xlab("Longitude") + ylab("Latitude") +
            ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))+
            labs(fill="Fertility")+
            coord_sf(xlim = c(80.15, 182.12), ylim = c(-55.65, 10.97), expand = FALSE)
          
        }
        
        
      }  
      
      
    }

    
    
  })
  
}




shinyApp(ui = ui2, server = server2)






















