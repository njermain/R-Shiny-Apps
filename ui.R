library(shiny)
library(DT)
library(shiny)
library(ggplot2)
library(shinythemes)
library(DT)
library(leaflet)
library(httpuv)

statePanel <- tabPanel(
    title='Individual Growth Dynamics',
    selectInput(
        inputId='StateSelector',
        label='Choose a state',
        choices=list(
            'NY', 'NJ', 'MA', 'TX'
        )
    ),
    
    textOutput(
        outputId='DisplayState'
    )
)



EPRPanel<-tabPanel(
  title='Eggs per Recruit Model',
  sliderInput(inputId = "L50.val.", 
                       label = "L50 Value", 
                       value = 303, 
                       min = 200, 
                       max = 400,
                       step = 10),
  sliderInput(inputId = "k.val.", 
              label = "K value", 
              value = .67, 
              min = 0, 
              max = 1,
              step = .05),
  sliderInput(inputId = "Linf.val.", 
              label = "Linfinity value", 
              value = 600, 
              min = 550, 
              max = 800,
              step = 10),
  sliderInput(inputId = "mort.rate.", 
              label = "Natural Mortality", 
              value = .2, 
              min = 0, 
              max = 1,
              step = .05),
  textOutput("text"),
  plotOutput(outputId="contour.plot", width="65%")
  
)

plotPanel <- tabPanel(
    title='Spawning Stock Biomass Per Recruit',
    fluidRow(
        column(
            width=3,
            selectInput(
                inputId='CarColumn',
                label='Please choose a column to plot',
                choices=names(mtcars)
            )
        ),
        column(
            width=9,
            plotOutput(outputId='CarHist')
        )
    )
)

pizzaPanel <- tabPanel(
    title='Surplus Production Model',
    fluidRow(
        column(
            width=6,
            DT::dataTableOutput(outputId='PizzaTable')
        ),
        column(
            width=6,
            leaflet::leafletOutput(outputId = 'PizzaMap')
        )
    )
)

navbarPage(
    title='Population Dynamics Workshop',
    selected='Pizza',
    theme=shinytheme(theme='spacelab'),
    EPRPanel,
    statePanel,
    plotPanel,
    pizzaPanel
)