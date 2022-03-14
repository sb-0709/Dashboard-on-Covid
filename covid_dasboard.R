library(shiny)
library(shinydashboard)
#library(shinycssloaders)
library(plotly)
library(ggplot2)
library(gganimate)
library(dplyr)
library(highcharter)
library(shinythemes)
library(gifski)
library(gridExtra)
library(magick)
theme_set(theme_bw(base_size=13))
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))

o1<-read.csv("test.csv")
w<-read.csv("unemp quar analysis.csv")
p<-read.csv("pollution.csv")
com=read.csv("combine.csv")
agri=read.csv("agriculturedv.csv")
gva=read.csv("GVAdv.csv")
df <- data.frame( x = gva$Sector,y = gva$CurrentPrice,name = gva$Sector)
data=c("agriculture", "construction")
body <-dashboardBody(
tabItems(
tabItem(tabName="GDP",h2("COVID EFFECT ON ECONOMY"),
 tabsetPanel(
      tabPanel("Sectors",
                fluidRow( 
                         box(
                             title = "COVID EFFECT ON GDP OF DIFFERENT SECTORS"
                             ,status = "primary"
                             ,solidHeader = TRUE 
                             ,collapsible = TRUE 
                             ,selectInput("choice", "Choose", choices = data, selected = NULL)
                             ,plotlyOutput("abc", height = "400px")
                             )#completion of box1
                          ,box(
                              title = "CURRENT PRICES IN DIFFERENT SECTORS"
                             ,status = "info"
                             ,solidHeader = TRUE 
                             ,collapsible = TRUE 
                            ,highchartOutput("abc1", height = "300px")
                              ) #completion of box2
                        )#completion of fluidrow1
               ),#completion of sectors
       tabPanel("Overall and Agriculture",
                 fluidRow(
                      tabBox(
                          title = NULL, width = 10,
                          id = "tabset1", height = "250px",
                          tabPanel("AGRICULTURE GDP GROWTH DURING COVID",imageOutput("plot1")),
                          tabPanel("Effect of GDP of different sectors on overall GDP of India",plotOutput("plot2")),
                          tabPanel("line",imageOutput("plot3")),
                          tabPanel(" DATA",dataTableOutput("data1"))
                            )#completion of tabbox
                          
                          )#completion of fluidrow
                   
                 
               )#completion of overall and agriculture
              
    
                )#completion of tabsetpanel
                ),#completion of tabname1
tabItem(tabName = "Pollution",h2("EFFECT OF COVID LOCKDOWN ON POLLUTION LEVEL"),
            fluidRow(
              tabBox(
                title = "COVID AFFECT ON POLLUTION LEVEL IN DELHI NCR", width = 12,
                id = "tabset3", height = "550px",
                tabPanel("PLOT",plotlyOutput("result"))
              )#completion of tabbox
              ),#completion of fluidrow
            fluidRow(
              tabBox(
                title = "DATASETS", width = 8,
                id = "tabset2", height = "600px",
                tabPanel("POLLUTION",dataTableOutput("p"))
              )#completion of tabbox
              )#completion of fluidrow2
            ),#completion of pollution
    tabItem(tabName = "Unemployment", h2("EFFECT OF COVID ON EMPLOYMENT SECTOR"),
          fluidRow(
            tabBox(
              title = "MONTHLY UNEMPLOYMENT TREND", width = 9,
              id = "tabset4", height = "600px",
              tabPanel("PLOT",imageOutput("d3"))
            )#completion of tabbox
          ),#completion of fluidrow
          fluidRow(
            tabBox(
              title = "QUARTERLY UNEMPLOYMENT ANALYSIS", width = 10,
              id = "tabset5", height = "600px",
              tabPanel("PLOT",plotlyOutput("c1"))
            ),#completion of tabbox
            tabBox(
              title = "DATASETS", width = 10,
              id = "tabset6", height = "1100px",
              tabPanel("MONTHLY",dataTableOutput("o1")),
              tabPanel("QUARTERLY",dataTableOutput("w"))
              )#completion of tabbox
            )#completion of fluidrow
          )#completion of unemployment


          )#completion of tabitems
 )#completion of body

shinyApp(
    ui = dashboardPage(skin="purple",
        dashboardHeader(title = "CovidIndia",
         dropdownMenu(type = "tasks", badgeStatus = "success",
  taskItem(value = 100, color = "green","Swastika Pandey"),
  taskItem(value = 100, color = "aqua","Sanskriti Bansal"),
  taskItem(value = 100, color = "yellow","SriHarsha"))),

        dashboardSidebar(sidebarMenu(
                       menuItem("GDP",tabName="GDP",icon=icon("circle")),
                       menuItem("Unemployment", icon = icon("th"), tabName = "Unemployment",badgeLabel = "trend", badgeColor = "green"),
                       menuItem("Pollution", tabName = "Pollution", icon = icon("dashboard"))
                                 )
                                
),
        body
    ),
    server = function(input, output,session){
    
    
   output$abc <- renderPlotly({
ggplotly(ggplot(com,aes(mining,y= get(input$choice)))+geom_point(color=com$year)+geom_line()+ylab("sectors"))
    
  })
output$abc1 <-  renderHighchart({
    
df %>%hchart("pie", hcaes(x = name, y = y),name = "CURRENT PRICES IN DIFFERENT SECTORS" )
  })

output$plot1 <- renderImage({
    outfile <- tempfile(fileext='.gif')

    # now make the animation
  a1=ggplot(data=agri,aes(Month,GDP))+geom_bar(stat="identity",fill="red")+ggtitle("AGRICULTURE GDP GROWTH DURING COVID")+theme(plot.title = element_text(hjust = 0.5))+transition_states (Month,transition_length=2,state_length=1)+ease_aes('sine-in-out')+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    anim_save("outfile.gif", animate(a1))
  list(src="outfile.gif",contentType='image/gif', 
         width = 500,
         height = 400)},deleteFile=FALSE)

output$plot3 <- renderImage({
    outfile1 <- tempfile(fileext='.gif')

    # now make the animation
  a3=ggplot(com,aes(mining,construction))+geom_point(color=com$year)+geom_line()+ggtitle("RELATION BETWEEN CONSTRUCTION GDP AND MINING GDP")+transition_reveal(mining)

    anim_save("outfile1.gif", animate(a3))
  list(src="outfile1.gif",contentType='image/gif', 
         width = 500,
         height = 400)},deleteFile=FALSE)

output$plot2 <-renderPlot({
ggplot(com, aes(y=overalgdp))+geom_violin(aes(x=mining),trim = FALSE, fill="yellow")+geom_violin(aes(x=agriculture),trim = FALSE, fill="tan")+geom_violin(aes(x=construction),trim = FALSE, fill="black")+ggtitle("Effect of GDP of different sectors on Overall GDP of India")+theme(plot.title = element_text(hjust = 0.5))+xlab("GDP of different sectors")+ylab("Overall GDP of India")

})
output$data1<- renderDataTable({com=read.csv("combine.csv")})
  output$c1 <- renderPlotly({
      c1<- plot_ly(w, x = ~f_value, y = ~m_value, z = ~value, color = ~quarter, marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 150), text = ~paste('Category:', category, '<br>Quarter:', quarter, '<br>overall value:', value, '<br>female value:', f_value, '<br>male value:',m_value))
      c1 %>% layout(title = 'Monthly Unemployment Rate During Covid 19 Period', scene = list(xaxis = list(title = 'female value'),yaxis= list(title='male value'), zaxis= list(title='overall value')))
      })
    output$o1 <- renderDataTable({read.csv("test.csv")})
    output$w <- renderDataTable({read.csv("unemp quar analysis.csv")})
    output$p <- renderDataTable({read.csv("pollution.csv")})
    
    output$d3 <- renderImage({
      outfile <- tempfile(fileext='.gif')
      
      # now make the animation
      d3=ggplot(o1,aes(months,value, group=category, colour= category))+geom_point(aes(group = seq_along(label),size=1.5))+ geom_line(aes(size=1)) + scale_colour_manual("", breaks = c("OVERALL", "URBAN","RURAL"),values = c("red", "green","blue"))+transition_reveal(label)+ggtitle("MONTHLY UNEMPLOYMENT RATE")+ylab("Unemployment Rate")+ theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))      
      anim_save("outfile.gif", animate(d3, renderer = gifski_renderer()))
      list(src="outfile.gif",contentType='image/gif',width= 800, height= 400)},deleteFile=FALSE)
    
    output$result <-renderPlotly({
     
      w1=ggplotly(ggplot(data = p,aes(x=city, y=PM10.mg.m3., fill=period)) +geom_col(position = "dodge")+scale_fill_manual(values = c("orange", "#820505"))+ggtitle("DELHI NCR 'PM 10' AQI level in March 2020 lockdown")+ylab("Concentration (mg/m3)")+ theme(plot.title = element_text(hjust = 0.5))+transition_states(city,wrap = FALSE)+theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1)))
      w2=ggplotly(ggplot(data = p,aes(x=city, y=PM2.5.mg.m3., fill=period)) +geom_col(position = "dodge")+scale_fill_manual(values = c("red", "green"))+ggtitle("DELHI NCR 'PM 2.5' AQI level in March 2020 lockdown")+ylab("Concentration (mg/m3)")+ theme(plot.title = element_text(hjust = 0.5))+transition_states(city,wrap = FALSE)+theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1)))
      w3=ggplotly(ggplot(data = p,aes(x=city, y=NO2, fill=period)) +geom_col(position = "dodge")+scale_fill_manual(values = c("blue", "pink"))+ggtitle("DELHI NCR 'NO2' AQI level in March 2020 lockdown")+ylab("Concentration (mg/m3)")+ theme(plot.title = element_text(hjust = 0.5))+transition_states(city,wrap = FALSE)+theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1)))
      
    
      w4=ggplotly(ggplot(data = p,aes(x=city, y=NOx, fill=period)) +geom_col(position = "dodge")+scale_fill_manual(values = c("brown", "orange"))+ggtitle("DELHI NCR 'NOx' AQI level in March 2020 lockdown")+ylab("Concentration (mg/m3)")+ theme(plot.title = element_text(hjust = 0.5))+transition_states(city,wrap = FALSE)+theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1)))
      
      
      w5=ggplotly(ggplot(data = p,aes(x=city, y=SO2, fill=period)) +geom_col(position = "dodge")+scale_fill_manual(values = c("#0f7527", "#8ef725"))+ggtitle("DELHI NCR 'SO2' AQI level in March 2020 lockdown")+ylab("Concentration (mg/m3)")+ theme(plot.title = element_text(hjust = 0.5))+transition_states(city,wrap = FALSE)+theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1)))
      

      w6=ggplotly(ggplot(data = p,aes(x=city, y=CO, fill=period)) +geom_col(position = "dodge")+scale_fill_manual(values = c("blue", "yellow"))+ggtitle("DELHI NCR 'CO' AQI level in March 2020 lockdown")+ylab("Concentration (mg/m3)")+ theme(plot.title = element_text(hjust = 0.5))+transition_states(city,wrap = FALSE)+theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1)))
      subplot(list(w1,w2,w3,w4,w5,w6), nrows = 2,margin =c(0.05,0.05,0.12,0.12))
                                  })      
  }
)
