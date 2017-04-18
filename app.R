rm(list = ls())
cat('\014')

library(ggplot2)
library(ggvis)
library(shiny)
library(dplyr)
library(reshape2)

# setwd('~/Desktop/MSAN622/Homework/HW2/')
# Read in the base data sets, drop unnecessary columns, melt tables for ggplot
life <- read.csv('life_expect.csv', stringsAsFactors = F) %>%
  select(-Indicator.Name, -Indicator.Code, -X2015, -X2016) %>%
  melt(variable = 'year', value.name = 'life_expectancy')

fert <- read.csv('fert.csv', stringsAsFactors = F) %>%
  select(-Country.Code, -Indicator.Name, -Indicator.Code, -X2015, -X2016) %>%
  melt(variable = 'year', value.name = 'fertility_rate')

pop <- read.csv('population.csv', stringsAsFactors = F) %>%
  select(-Country.Code, -Indicator.Name, -Indicator.Code, -X2015, -X2016) %>%
  melt(variable = 'year', value.name = 'population')

meta <- read.csv('meta_data.csv', stringsAsFactors = F) %>% 
  select(Country.Code, Region) 

# Merge the data sets and clean the columns
dat <- left_join(fert, life, by = c('Country.Name', 'year')) %>%
  left_join(pop, by = c('Country.Name', 'year')) %>%
  left_join(meta, by = c('Country.Code')) %>%
  filter(Region != '', !is.na(Region)) %>%
  mutate(year = as.numeric(substring(year, 2)))

dat <- dat[complete.cases(dat), ] %>%
  mutate(id = 1:length(year),
         opacity = 1)

# Shiny
ui <- fluidPage(
  fluidRow(column(12, headerPanel('Life expectancy vs. Fertility rate'))),
  fluidRow(
    column(width = 6, ggvisOutput("ggvis")),
    column(
      width = 4,
      sidebarLayout(checkboxGroupInput("region", "Region",
                                       choices = unique(dat$Region)),
                    sliderInput("pop", "Population", 0, 1, 0.5)), offset = 2
      )
    ),
  fluidRow(
    column(
      width = 5, offset = 1,
      sliderInput("year", "Year", value = 1984, min = 1960, max = 2014, 
                  width = "100%", step = 1, ticks=FALSE, sep = '',
                  animate = animationOptions(interval = 500)))
    )
)

server <- function(input, output) {
  
  hover_tooltip <- function(x) {
    if(is.null(x)) return(NULL)
    row <- dat[dat$id == x$id, 'Country.Name']
    paste0(row)
  }
  
  dat_r <- reactive({dat %>% filter(year == input$year)})
  
  vis <- reactive({
    
    dat_r() %>%
      mutate(population = population^(0.75+0.2*input$pop) / 100000 + 30,
             opacity = if (length(input$region) == 0) 1 
                       else ifelse(Region %in% input$region, 1, 0.4)) %>%
      ggvis(~life_expectancy, ~fertility_rate, fill = ~Region,
            key := ~id, stroke := 'black', strokeWidth := 0.25,
            size := ~population, opacity := ~opacity) %>%
        layer_points() %>%
        hide_legend(c("size")) %>%
        add_tooltip(hover_tooltip, "hover") %>%
        add_tooltip(hover_tooltip, "click") %>%
        scale_numeric("x", domain = c(10, 90)) %>%
        scale_numeric("y", domain = c(0, 9)) %>%
        add_axis("x", title = "\nLife Expectancy") %>%
        add_axis("y", title = "Fertility Rate\n") %>%
        set_options(height = 600, width = 800)
  })
  vis %>% bind_shiny("ggvis")
}

shinyApp(ui = ui, server = server)
