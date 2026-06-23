

library(shiny)
library(tidyverse)
library(DT)
library(highcharter)



default_country <- "Slovenia"


# https://ec.europa.eu/eurostat/databrowser/view/NRG_CB_PEM__custom_200961/default/table?lang=en
raw_data <- read_csv(
  "nrg_cb_pem_linear_2_0.csv.gz",
  na = c("", ":"),
  show_col_types = FALSE
)


pem <- raw_data |>
  filter(
    unit == "GWH",
    !is.na(OBS_VALUE),
    nchar(geo) == 2
  ) |>
  mutate(
    date = as.Date(paste0(TIME_PERIOD, "-01")),
    year = as.integer(format(date, "%Y"))
  ) |>
  transmute(
    country = `Geopolitical entity (reporting)`,
    code = geo,
    source = `Standard international energy product classification (SIEC)`,
    month = TIME_PERIOD,
    date = date,
    year = year,
    gwh = OBS_VALUE
  )


total_source <- "Total"


countries <- pem |>
  distinct(country) |>
  arrange(country) |>
  pull(country)

months <- pem |>
  distinct(month, date) |>
  arrange(date) |>
  pull(month)

sources <- pem |>
  filter(source != total_source) |>
  distinct(source) |>
  arrange(source) |>
  pull(source)


ui <- navbarPage(
  "Monthly Electricity Generation",
  
  header = tags$head(
    tags$style(HTML("
    body {
      color: #434348;
    }

    .navbar-default {
      background-color: #434348;
    }

    .navbar-default .navbar-brand,
    .navbar-default .navbar-nav > li > a {
      color: white;
    }
    
      .navbar-default .navbar-brand:hover,
  .navbar-default .navbar-brand:focus {
    background-color: #434348;
    color: white;
  }
  
    .navbar-default .navbar-nav > li > a:hover {
      background-color: #5A5A63;
      color: white;
    }

    .navbar-default .navbar-nav > .active > a,
    .navbar-default .navbar-nav > .active > a:hover,
    .navbar-default .navbar-nav > .active > a:focus {
      background-color: #7CB5EC;
      color: #2C3E50;
    }

    h1, h2, h3 {
      color: #2C3E50;
    }

    .form-control:focus {
      border-color: #7CB5EC;
      box-shadow: 0 0 4px #7CB5EC;
    }
  "))
  ),
  
  tabPanel(
    "Data",
    br(),
    DTOutput("table")
  ),
  
  tabPanel(
    "Total generation",
    br(),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "total_country",
          "Country",
          choices = countries,
          selected = default_country
        )
      ),
      
      mainPanel(
        highchartOutput("total_plot", height = "600px")
      )
    )
  ),
  
  tabPanel(
    "Generation by source",
    br(),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "source_country",
          "Country",
          choices = countries,
          selected = default_country
        ),
        
        selectInput(
          "selected_sources",
          "Production sources",
          choices = sources,
          selected = "Nuclear fuels and other fuels n.e.c.",
          multiple = TRUE
        )
      ),
      
      mainPanel(
        highchartOutput("source_plot", height = "600px")
      )
    )
  ),
  
  tabPanel(
    "Europe map",
    br(),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "map_source",
          "Production source",
          choices = sources
        ),
        
        selectInput(
          "map_month",
          "Month",
          choices = months,
          selected = tail(months, 1)
        ),
        
        p("Map shows the selected source as a percentage of total generation.")
      ),
      
      mainPanel(
        highchartOutput("europe_map", height = "650px")
      )
    )
  ),
  
  tabPanel(
    "Pie chart",
    br(),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "pie_source",
          "Production source",
          choices = sources
        ),
        
        selectInput(
          "pie_month",
          "Month",
          choices = months,
          selected = tail(months, 1)
        )
      ),
      
      mainPanel(
        highchartOutput("pie_plot", height = "650px")
      )
    )
  )
)


server <- function(input, output) {
  
  # Data table
  output$table <- renderDT({
    
    table_data <- pem |>
      select(
        Country = country,
        Source = source,
        Month = month,
        `Generation (GWh)` = gwh
      )
    
    datatable(
      table_data,
      filter = "top",
      rownames = FALSE
    )
  })
  

  output$total_plot <- renderHighchart({
    
    total_data <- pem |>
      filter(
        country == input$total_country,
        source == total_source
      ) |>
      arrange(date)
    
    hchart(
      total_data,
      "line",
      hcaes(x = date, y = gwh)
    ) |>
      hc_title(
        text = paste("Total generation:", input$total_country)
      ) |>
      hc_yAxis(
        title = list(text = "Generation (GWh)")
      ) |>
      hc_tooltip(
        valueSuffix = " GWh"
      )
  })
  

  output$source_plot <- renderHighchart({
    
    selected_data <- pem |>
      filter(
        country == input$source_country,
        source %in% input$selected_sources
      )
    

    annual_data <- selected_data |>
      group_by(year, source) |>
      summarise(
        gwh = sum(gwh),
        .groups = "drop"
      ) |>
      arrange(year)
    
    hchart(
      annual_data,
      "column",
      hcaes(x = year, y = gwh, group = source)
    ) |>
      hc_title(
        text = paste("Annual generation:", input$source_country)
      ) |>
      hc_xAxis(
        type = "linear",
        tickInterval = 1,
        allowDecimals = FALSE,
        title = list(text = "Year")
      ) |>
      hc_yAxis(
        title = list(text = "Generation (GWh)")
      ) |>
      hc_plotOptions(
        column = list(stacking = "normal")
      ) |>
      hc_tooltip(
        valueSuffix = " GWh"
      )
  })
  

  output$europe_map <- renderHighchart({
    
    total_data <- pem |>
      filter(
        month == input$map_month,
        source == total_source
      ) |>
      select(
        code,
        total_gwh = gwh
      )
    
    source_data <- pem |>
      filter(
        month == input$map_month,
        source == input$map_source
      ) |>
      select(
        code,
        source_gwh = gwh
      )
    

    map_data <- total_data |>
      left_join(source_data, by = "code") |>
      mutate(
        source_gwh = replace_na(source_gwh, 0),
        share = 100 * source_gwh / total_gwh,
        
        map_code = recode(
          code,
          EL = "GR",
          UK = "GB",
          .default = code
        )
      )
    
    hcmap(
      "custom/europe",
      data = map_data,
      value = "share",
      joinBy = c("iso-a2", "map_code"),
      name = "Share of generation",
      showInLegend = FALSE
    ) |>
      hc_mapNavigation(enabled = TRUE) |>  
      hc_title(
        text = paste(input$map_source, "share of generation")
      ) |>
      hc_subtitle(
        text = input$map_month
      ) |>
      hc_colorAxis(
        min = 0,
        max = 100
      ) |>
      hc_tooltip(
        pointFormat = "<b>{point.value:.1f}%</b>"
      )
  })

  output$pie_plot <- renderHighchart({
    
    pie_data <- pem |>
      filter(
        month == input$pie_month,
        source == input$pie_source,
        gwh > 0
      ) |>
      group_by(country) |>
      summarise(
        gwh = sum(gwh),
        .groups = "drop"
      ) |>
      arrange(desc(gwh))
    
    hchart(
      pie_data,
      "pie",
      hcaes(name = country, y = gwh)
    ) |>
      hc_title(
        text = paste(input$pie_source, "-", input$pie_month)
      ) |>
      hc_tooltip(
        pointFormat = "<b>{point.y:,.1f} GWh</b><br/>{point.percentage:.1f}%"
      )
  })
}

shinyApp(ui = ui, server = server)