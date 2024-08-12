

library(treemapify)
library(ggpattern)
library(shinyWidgets)
library(openxlsx2)
library(mschart)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(lubridate)
library(bslib)

walmart_data <- read.csv("https://raw.githubusercontent.com/Jorge-hercas/r-para-inteligencia-de-negocios/main/CSV/walmart.csv")

shinyApp(
  ui = page_sidebar(
  title = "Ejemplo de un reporte de ventas",
  sidebar = sidebar(
    h3("Filtros"),
    checkboxGroupInput(
      "branch", "Branch", choices = unique(walmart_data$Branch),
      selected = unique(walmart_data$Branch),
      inline = T
    ),
    checkboxGroupInput(
      "genero", "Genero", choices = unique(walmart_data$Gender),
      selected = unique(walmart_data$Gender),
      inline = T
    ),
    selectInput(
      "ciudad", "Ciudad", choices = unique(walmart_data$City),
      selected = unique(walmart_data$City), multiple = T
    ),
    selectInput(
      "tipo_cliente", "Tipo de cliente", choices = unique(walmart_data$Customer.type),
      selected = unique(walmart_data$Customer.type), multiple = T
    ),
    selectInput(
      "tipo_pago", "Tipo de pago", choices = unique(walmart_data$Payment),
      selected = unique(walmart_data$Payment), multiple = T
    ),
    downloadButton("download", "Descargar reporte", icon = icon("file-excel"))
  ),
  layout_column_wrap(
    value_box(title = "Margen promedio",
              min_height = "100px",
              value = textOutput("margen"),
              showcase = icon("percent"),
              theme = "text-success", 
              width = "300px"
    ),
    value_box(title = "Ingreso bruto total",
              min_height = "100px",
              value = textOutput("ingreso_bruto"),
              showcase = icon("dollar-sign"),
              theme = "text-success"),
    value_box(title = "Categoría de producto más vendida",
              min_height = "100px",
              value = textOutput("categoria"),
              showcase = icon("list"),
              theme = "text-success")
  ),
  br(),br(),
  column(
    width = 12,
    fluidRow(
      column(
        width = 12,
        align = "center",
        plotOutput("historico")
      ),
      column(
        width = 5,
        align = "center",
        plotOutput("top")
      ),
      column(
        width = 7,
        align = "center",
        plotOutput("cateogorias_ventas")
      )
    )
  )
),
  server = function(input, output, session){

    data <- reactive({

      walmart_data |>
        filter(Branch %in% input$branch & Gender %in% input$genero
               & City %in% input$ciudad & Customer.type
               %in% input$tipo_cliente & Payment %in% input$tipo_pago ) |>
        mutate(Ingreso = Unit.price*Quantity,
               Date = floor_date(as_date(Date), "week"))

    })

    output$margen <- renderText({

      x <-
      data() |>
        summarise(
          margen = mean(gross.margin.percentage, na.rm = T)
        )

      scales::percent(x$margen/100, accuracy = 0.1)

    })

    output$ingreso_bruto <- renderText({

      x <-
        data() |>
        summarise(Ingreso = sum(Ingreso, na.rm = T))

      scales::comma(x$Ingreso, accuracy = 0.1, prefix = "$")

    })

    output$categoria <- renderText({

      x <-
        data() |>
        group_by(Product.line) |>
        summarise(Ventas = sum(Ingreso, na.rm = T)) |>
        arrange(desc(Ventas))

      stringr::str_trunc(x$Product.line[1], width = 14)

    })

    historico_data <- reactive({

      data() |>
        group_by(Date) |>
        summarise(Ingreso = sum(Ingreso, na.rm = T))

    })

    output$historico <- renderPlot({

      historico_data() |>
        ggplot(aes(x = Date, y = Ingreso)) +
        geom_area_pattern(pattern = "gradient",
                          pattern_fill = "white",
                          pattern_fill2 = "#74b47c") +
        theme_minimal() +
        labs(title = "Ingresos por fecha") +
        geom_text(aes(label=scales::comma(Ingreso)),
                  position=position_dodge(width=0.9), vjust=-0.25)

    })

    top_data <- reactive({

      data() |>
        group_by(City) |>
        summarise(Ingreso = sum(Ingreso)) |>
        mutate(City = forcats::fct_reorder(City, Ingreso))

    })

    output$top <- renderPlot({

      top_data() |>
        ggplot(aes(area = Ingreso, fill = City,
                   label = scales::comma(round(Ingreso),
                                         prefix = "$"))) +
        geom_treemap() +
        scale_fill_manual(
          values = c("#7c8c84", "#74b47c", "#32a640")
        ) +
        geom_treemap_text(
          color = "white",
          padding.y = unit(20, "mm"),
          size = 15) +
        theme_minimal() +
        labs(title = "Ingresos por region")

    })

    categorias_ventas_data <- reactive({

      data() |>
        group_by(Product.line) |>
        summarise(Ventas = sum(Ingreso, na.rm = T)) |>
        arrange(desc(Ventas))

    })

    output$cateogorias_ventas <- renderPlot({

      categorias_ventas_data() |>
        ggplot(aes(y = forcats::fct_reorder(Product.line, Ventas), x = Ventas,
                   fill = Ventas)) +
        geom_col(width = 0.5) +
        scale_fill_viridis_b() +
        theme_minimal() +
        labs(y = "Cateogoría de producto")
    })

    output$download <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        wb_workbook() |>
          wb_add_worksheet("Datos historicos",
                           gridLines = F, tabColor = colores[1]) |>
          wb_add_worksheet("Tops",
                           gridLines = F, tabColor = colores[2]) |>
          wb_add_worksheet("Categorías",
                           gridLines = F, tabColor = colores[3]) |>
          wb_add_mschart(
            sheet = "Datos historicos",
            graph = ms_linechart(
              historico_data(),x = "Date", y = "Ingreso") |>
              chart_ax_x(num_fmt = "[$-es-ES]mmm yyyy") |>
              chart_ax_y(num_fmt = "#,##0⁠"),
            dims = "D2") |>
          wb_add_mschart(
            sheet = "Tops",
            graph = ms_barchart(
              top_data(),x = "City", y = "Ingreso") |>
              chart_ax_x(num_fmt = "[$-es-ES]mmm yyyy") |>
              chart_ax_y(num_fmt = "#,##0⁠"),
            dims = "D2") |>
          wb_add_mschart(
            sheet = "Categorías",
            graph = ms_barchart(
              categorias_ventas_data(),x = "Product.line", y = "Ventas") |>
              chart_ax_x(num_fmt = "[$-es-ES]mmm yyyy") |>
              chart_ax_y(num_fmt = "#,##0⁠"),
            dims = "D2") |>
          wb_save(path = file)
      }
    )

  }
)








