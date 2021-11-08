library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(leaflet)
library(leaflet.providers)
library(RColorBrewer)
library(rgdal)
library(shinycssloaders)

rm(list = ls())

# Carga de datos

load('datos/df_m.RData')
load('datos/df_zbs.RData')
load("datos/fec_act.Rdata")
load('datos/pcrs_mun.Rdata')
load('datos/pcrs_zbs.Rdata')

# pcrs_mun <- pcrs_mun %>% 
#   mutate(porc_positivos = 'No disponible', 
#          porc_regional = 'No disponible')
# 
# pcrs_zbs <- pcrs_zbs %>% 
#   mutate(porc_positivos = 'No disponible', 
#          porc_regional = 'No disponible')

municipios <- levels(df_m$municipio)[-1]
codmunicipios <- unique(df_m$codmunicipio[df_m$municipio %in% municipios])
nomb_zbs <- levels(df_zbs$zonabasica)[-1]
codigos_zbs <- unique(df_zbs$codzbs[df_zbs$zonabasica %in% nomb_zbs])

# Fecha del día de ayer

# t <- Sys.time()
# attr(t, "tzone") <- "Europe/Madrid"


t <- unlist(strsplit(fec_act$mtime, " "))
fecha_actual <- as.Date(as.Date((t[1])) - 1)
t <- paste0(format(as.Date(t[1]), '%d/%m/%Y'), " (",t[2], ")")

# Cargamos el shapefile.

zonasbasicasph <- readOGR("datos/zbs/ZBS2020/ZBS2020.shp")

municipiossph <- readOGR("datos/municipios/MUN2020.shp")

ui <- fluidPage(
  tagList(tags$head(tags$style(type = 'text/css','.navbar-brand{display:none;}')),
          navbarPage('', 
                     collapsible = TRUE,
                     theme = shinythemes::shinytheme("simplex"), 
                     footer = div(paste0("Datos actualizados a ",t), style = 'font-size: 10px'),
                     tabPanel("IA por municipios", fluid = TRUE, 
                              sidebarLayout(
                                sidebarPanel(
                                  titlePanel(h4(paste0("IA a ", format(fecha_actual, "%d/%m/%Y")))), 
                                  titlePanel(h5("Parámetros")),
                                  fluidRow(column(8, div(selectInput("periodo", 
                                                                     "Periodo (días)",
                                                                     choices = c('14 días', '7 días')), 
                                                         style = "font-size:90%"))
                                  ),
                                  fluidRow(column(4, 
                                                  div(actionButton("ordena1", "Ordena IA")), style = "font-size:75%")),
                                  fluidRow(br()),
                                  fluidRow(column(4, 
                                                  div(actionButton("refresca1", "Recargar")), style = "font-size:75%")),
                                  width = 3
                                ), 
                                mainPanel(
                                  fluidRow(
                                    column(12, offset = 0, 
                                           div(DTOutput("iamunicipios"), title = "IA por municipios",
                                               style = "font-size:95%; height:97.5%")) %>% 
                                      withSpinner(color="#9c1f2e", type = 8)
                                  )
                                )
                              )), 
                     tabPanel("Serie IA municipio", fluid = TRUE, 
                              sidebarLayout(
                                sidebarPanel(
                                  titlePanel(h5("Parámetros")), 
                                  fluidRow(column(8, div(selectInput("periodo2", 
                                                                     "Periodo (días)",
                                                                     choices = c('14 días', '7 días')), 
                                                         style = "font-size:90%"))
                                  ),
                                  fluidRow(column(12, div(selectInput("ventana", 
                                                                      "Serie", 
                                                                      choices = c('Desde 01/07/2020', 
                                                                                  'Últimos 120 días', 
                                                                                  'Últimos 60 días',
                                                                                  'Últimos 28 días')),
                                                          style = "font-size:90%"))
                                  ),
                                  fluidRow(column(12, div(selectInput("codmunicipio", 
                                                                      "Municipio", 
                                                                      choices = setNames(c( '1205', codmunicipios),
                                                                                         c('Región de Murcia', municipios))), 
                                                          style = "font-size:90%"))
                                  ),
                                  width = 3
                                ),
                                mainPanel(
                                  plotlyOutput("iagraph", height = "435px", width = "650px") 
                                  %>% withSpinner(color="#9c1f2e", type = 8)
                                )
                              )),
                     tabPanel("Mapa municipios", fluid = TRUE, 
                              sidebarLayout(
                                sidebarPanel(
                                  titlePanel(h4(paste0("IA a ", format(fecha_actual, "%d/%m/%Y")))), 
                                  fluidRow(
                                    column(8, h5("Parámetros"))
                                  ),
                                  fluidRow(column(8, div(selectInput("periodo3", 
                                                                     "Periodo (días)",
                                                                     choices = c('14 días', '7 días')), 
                                                         style = "font-size:90%"))
                                  ),
                                  width = 3
                                ),
                                mainPanel(
                                  leafletOutput("mapmun", height = "475px", width = "110%") %>%
                                    withSpinner(color="#9c1f2e", type = 8),
                                  div(tags$caption("Servicio de Epidemiología. D.G. de Salud Pública y Adicciones. Consejería de Salud"), 
                                      style = 'font-size:9px')
                                )
                              )),
                     
                     tabPanel("IA por ZBS", fluid = TRUE, 
                              sidebarLayout(
                                sidebarPanel(
                                  titlePanel(h4(paste0("IA a ", format(fecha_actual, "%d/%m/%Y")))), 
                                  titlePanel(h5("Parámetros")),
                                  fluidRow(column(8, div(selectInput("periodo4", 
                                                                     "Periodo (días)",
                                                                     choices = c('14 días', '7 días')), 
                                                         style = "font-size:90%"))
                                  ),
                                  fluidRow(column(4, 
                                                  div(actionButton("ordena2", "Ordena IA")), style = "font-size:75%")),
                                  fluidRow(br()),
                                  fluidRow(column(4, 
                                                  div(actionButton("refresca2", "Recargar")), style = "font-size:75%")),
                                  width = 3
                                ), 
                                mainPanel(
                                  fluidRow(
                                    column(12, offset = 0, 
                                           div(DTOutput("iazonabasica"),
                                               style = "font-size:95%; height:97.5%")) %>% 
                                      withSpinner(color="#9c1f2e", type = 8)
                                  )
                                )
                              )), 
                     tabPanel("Serie IA ZBS", fluid = TRUE, 
                              sidebarLayout(
                                sidebarPanel(
                                  titlePanel(h5("Parámetros")), 
                                  fluidRow(column(8, div(selectInput("periodo5", 
                                                                     "Periodo (días)",
                                                                     choices = c('14 días', '7 días')), 
                                                         style = "font-size:90%"))
                                  ),
                                  fluidRow(column(12, div(selectInput("ventana2", 
                                                                      "Serie", 
                                                                      choices = c('Desde 01/07/2020', 
                                                                                  'Últimos 120 días', 
                                                                                  'Últimos 60 días',
                                                                                  'Últimos 28 días')), 
                                                          style = "font-size:90%"))
                                  ),
                                  fluidRow(column(12, div(selectInput("zonabasica", 
                                                                      "Zona básica de salud", 
                                                                      choices = nomb_zbs), 
                                                          style = "font-size:90%"))
                                  ),
                                  width = 3
                                ),
                                mainPanel(
                                  plotlyOutput("iagraphzb", height = "435px", width = "650px") %>% 
                                    withSpinner(color="#9c1f2e", type = 8)
                                )
                              )), 
                     tabPanel("Mapa ZBS por área de salud", fluid = TRUE, 
                              sidebarLayout(
                                sidebarPanel(
                                  titlePanel(h4(paste0("IA a ", format(fecha_actual, "%d/%m/%Y")))), 
                                  fluidRow(
                                    column(8, h5("Parámetros"))
                                  ),
                                  fluidRow(column(8, div(selectInput("periodo6", 
                                                                     "Periodo (días)",
                                                                     choices = c('14 días', '7 días')), 
                                                         style = "font-size:90%"))
                                  ),
                                  fluidRow(column(12, div(selectInput("codarea", 
                                                                      "Área de salud", 
                                                                      choices = setNames(c("0",
                                                                                           as.character(1:9)), 
                                                                                         c("Región de Murcia",
                                                                                           paste0("Área ", 1:9)))), 
                                                          style = "font-size:90%"))
                                  ),
                                  width = 3
                                ),
                                mainPanel(
                                  leafletOutput("mapzbsarea", height = "475px", width = "110%") %>% 
                                    withSpinner(color="#9c1f2e", type = 8), 
                                  div(tags$caption("Servicio de Epidemiología. D.G. de Salud Pública y Adicciones. Consejería de Salud"), 
                                      style = 'font-size:9px')
                                )
                              )), 
                     tabPanel("Mapa ZBS grandes municipios", fluid = TRUE, 
                              sidebarLayout(
                                sidebarPanel(
                                  titlePanel(h4(paste0("IA a ", format(fecha_actual, "%d/%m/%Y")))), 
                                  fluidRow(
                                    column(8, h5("Parámetros"))
                                  ), 
                                  fluidRow(column(8, div(selectInput("periodo7", 
                                                                     "Periodo (días)",
                                                                     choices = c('14 días', '7 días')), 
                                                         style = "font-size:90%"))
                                  ),
                                  fluidRow(column(10, div(selectInput("codmunicipio3", 
                                                                      "Municipio", 
                                                                      choices = setNames(c("0",
                                                                                           "30016", 
                                                                                           "30024", 
                                                                                           "30030"), 
                                                                                         c("Región de Murcia",
                                                                                           "Cartagena", 
                                                                                           "Lorca", 
                                                                                           "Murcia"))), 
                                                          style = "font-size:90%"))
                                  ),
                                  width = 3
                                ),
                                mainPanel(
                                  leafletOutput("mapzbmun", height = "475px", width = "110%") %>% 
                                    withSpinner(color="#9c1f2e", type = 8),
                                  div(tags$caption("Servicio de Epidemiología. D.G. de Salud Pública y Adicciones. Consejería de Salud"), 
                                      style = 'font-size:9px')
                                )
                              )), 
                     tabPanel("Serie IA regional por edad", fluid = TRUE, 
                              sidebarLayout(
                                sidebarPanel(
                                  titlePanel(h5("Parámetros")), 
                                  fluidRow(column(8, div(selectInput("periodo8", 
                                                                     "Periodo (días)",
                                                                     choices = c('14 días', '7 días')), 
                                                         style = "font-size:90%"))
                                  ),
                                  fluidRow(column(8, div(selectInput("grupo_edad",
                                                                     "Categorias de edad",
                                                                     choices = list(`Categorías 1` = 1, `Categorías 2` = 2), 
                                                                     ),
                                                         style = "font-size:90%"))
                                  ),
                                  fluidRow(column(12, div(selectInput("ventana3", 
                                                                      "Serie", 
                                                                      choices = c('Desde 01/07/2020', 
                                                                                  'Últimos 120 días',
                                                                                  'Últimos 60 días',
                                                                                  'Últimos 30 días')), 
                                                          style = "font-size:90%"))
                                  ),
                                  width = 3
                                ),
                                mainPanel(
                                  plotlyOutput("iagraphedad", height = "435px", width = "725px") %>%
                                    withSpinner(color="#9c1f2e", type = 8)
                                )
                              )), 
                     tabPanel("Notas", 
                              HTML("<p style='color:blue'>Poblaciones ajustadas al padrón municipal correspondiente al año 2020. El % de positividad en las pruebas es sobre casos prevalentes.</p>"),
                              HTML("La fuente de datos procede de los resultados emitidos por los laboratorios de microbiología de la Región de Murcia.
                          La fecha del evento es la fecha del diagnóstico microbiológico (Pruebas de Diagnóstico de Infección Activa, PDIA: PCR o test de detección rápida de antígeno). 
                          Del conjunto de datos se han excluido los casos detectados que no residen en la comunidad autónoma. 
                          Los datos se presenta a la fecha actual menos 24 horas.</br></br>"),
                              HTML(
                                "La <b>incidencia acumulada (IA)</b> es la proporción entre en el número de casos diagnosticados en el periodo en estudio (7 o 14 días previos) y
                                la población del área de referencia multiplicada por 100 000. Se puede considerar como una estimación del riesgo de adquirir la infección por el virus SARS-CoV2 en los 7 o 
                           14 días previos. Esta proporción se calcula día a día y, por tanto, su valor cambia en función del número de casos nuevos que aparecen diariamente.
                           Estos cambios en la IA día a día, generan la <b>serie temporal</b> que se muestra, la cual nos informa de la tendencia que presenta la epidemia en
                           el área indicada. El ECDC (European Centre for Disease Prevention and Control) categoriza aquellas regiones europeas con una IA &ge; 120 por 100 000
                           habitantes en los 14 días previos como de muy alta incidencia.</br></br>
                           Para poner en perspectiva la magnitud de la epidemia por SARS-CoV2, podemos establecer la siguiente comparación con las ondas epidémicas de la gripe.
                           La intensidad de la onda epidémica gripal se establece para cada CCAA en base a las series históricas de las ondas epidémicas de los últimos 10 años. 
                           Para la Región de Murcia y para la temporada pasada (2019 - 2020), se establecían los siguientes umbrales para categorizar la intensidad de la onda
                           epidémica: </br></br>
                           <ul>
                            <li><b>Intensidad baja:</b> IA 7 días de 44.1 casos por 100 000 habitantes.</li>
                            <li><b>Intensidad media:</b> IA 7 días de 222.28 casos por 100 000 habitantes.</li>
                            <li><b>Intensidad alta:</b> IA 7 días de 413.85 casos por 100 000 habitantes.</li>
                            <li><b>Intensidad muy alta:</b> IA 7 días de 544.69 casos por 100 000 habitantes.</li>
                           </ul>
                           Por ejemplo, la temporada de gripe 2017 - 2018, que fue la última con una tasa de incidencia especialmente elevada, alcanzó su pico en la semana 2
                           con una IA semanal de 517 casos por cada 100 000 habitantes.</br></br>
                            "
                              )
                              
                     )
          )
          
  )
)

# input <- list()
# input$periodo <- input$periodo1 <- input$periodo2 <- input$periodo3 <- input$periodo4 <- input$periodo5 <- input$periodo6 <- input$periodo7 <- '14 días'
# input$ventana <- input$ventana1 <- input$ventana2 <- input$ventana3 <- input$ventana4 <- fecha_actual
# input$ordena <- input$ordena1 <- input$ordena2 <- FALSE
# input$codmunicipio <- '30002'

server <- function(input, output, session) {
  
  # Generación df para salida IA diaria por municipio
  covid_mun <- reactive({
    cov_mun <- df_m %>% 
      filter(edad == 'Todos' & periodo == input$periodo & fecha == fecha_actual) %>% 
      select(municipio, poblacion, ncasos, ia)
    if (input$ordena1){
      cov_mun %>% 
        arrange(desc(ia))
    }else{
      cov_mun
    }
  })
  
  # Salida IA diaria por municipio
  output$iamunicipios <- renderDT({
    tab <- covid_mun()
    col_ia <- paste0("IA", " ", input$periodo, " previos")
    names(tab) <- c("Municipio", "Población", "N casos", col_ia)
    tab <- tab %>%
      DT::datatable(, 
                    caption = htmltools::tags$caption("Servicio de Epidemiología. D.G. Salud Pública y Adicciones. Consejería de Salud",
                                                      style = 'caption-side: bottom; font-size:9px', 
                                                      noWS = 'before'),
                    rownames = FALSE,
                    options = list(pageLength = 10,
                                   searching = FALSE,
                                   lengthMenu = list(c(10, 20, -1), c("10", "20", "Todos")),
                                   language = list(url = 'Español.json'),
                                   columnDefs = list(list(width = "100px", targets = 3),
                                                     list(width = "75px", targets = 2),
                                                     list(width = "175px", targets = 0), 
                                                     list(width = "50px", targets = 1),
                                                     list(targets = 1:2, className = "dt-center"), 
                                                     list(targets = 3, className = "dt-right"),
                                                     list(targets = 0, className = "dt-left"), 
                                                     list(targets = 0:3, className = "dt-head-center"))
                    )) %>%
      formatRound(col_ia)
    tab
  }
  )
  
  # Generación del df para el gráfico serie IA por municipios
  
  covid_mun_fig <- reactive({
    switch (input$ventana,
            'Desde 01/07/2020' = df <- df_m %>% 
              filter(edad == 'Todos' & periodo == input$periodo2), 
            'Últimos 120 días' = df <- df_m %>% 
              filter(fecha >= (fecha_actual - 120) & edad == 'Todos' & periodo == input$periodo2),
            'Últimos 60 días' = df <- df_m %>% 
              filter(fecha >= (fecha_actual - 60) & edad == 'Todos' & periodo == input$periodo2),
            'Últimos 28 días' = df <- df_m %>% 
              filter(fecha >= (fecha_actual - 28) & edad == 'Todos' & periodo == input$periodo2)
    )
    
    df <- df %>% 
      filter(codmunicipio == "1205" | codmunicipio == input$codmunicipio) %>% 
      mutate(municipio = droplevels(municipio))
    df
  })
  
  # Generación del gráfico Serie IA por municipios
  output$iagraph <- renderPlotly({
    colores <- c("#D14E72FF", "#0D0887FF")
    axis_y_title <- paste0("Incidencia acumulada ", input$periodo2)
    if(length(unique(covid_mun_fig()$codmunicipio)) == 1) {
      colores <- colores[1]
      y <- -0.1
    } else {
      y <- -0.225
    }
    p <- plot_ly(covid_mun_fig(), x = ~ fecha, y = ~ round(ia, 2), color = ~ municipio, colors = colores) %>% 
      add_lines() %>% 
      layout(title = list(text = "Serie temporal IA",
                          xanchor = "left",
                          yanchor = "bottom", 
                          yref = "paper", 
                          xref = "paper",
                          x = 0, 
                          y = 1.1, 
                          font = list(size = 17)),
             legend = list(orientation = 'h'), 
             xaxis = list(title = ""), 
             yaxis = list(title = axis_y_title), 
             hovermode = 'x')
    p <- config(p, locale = 'es')
    p <- p %>% 
      layout(annotations = list(text = 'Servicio de Epidemiologia. D.G. Salud Pública y Adicciones. Consejería de Salud.',
                                font = list(size = 9),
                                showarrow = FALSE,
                                xref = 'paper', x = 0,
                                yref = 'paper', y = y))
    print(p)
  })
  
  # Mapa de IA y % de positividad por municipios
  
  # Generación de los datos
  
  covidmapmun <- reactive({
    cov_mun <- df_m %>% 
      filter(periodo == input$periodo3 & fecha == fecha_actual & edad == 'Todos' & municipio != 'R. de Murcia')
    
    municipiossph@data <- municipiossph@data %>% 
      left_join(cov_mun %>% 
                  select(codmunicipio, ncasos, ia), 
                by = c('codmncp' = 'codmunicipio'))
    municipiossph@data <- municipiossph@data %>% 
      left_join(pcrs_mun, by = c('codmncp' = 'codmunicipio'))
    municipiossph
  })
  
  # Mapa municipios
  
  output$mapmun <- renderLeaflet({
    pal <- colorNumeric(palette = "YlOrRd", domain = covidmapmun()$ia)
    labels <- sprintf("<b, font-size = 28px> %s </b></br> Nº casos: %s </br> IA: %s </br> Porcentaje PDIA positivas: %s 
                  </br> Porcentaje PDIA positivas regional: %s", 
                      covidmapmun()$municip, 
                      covidmapmun()$ncasos, 
                      format(covidmapmun()$ia, digits = 3, nsmall = 2), 
                      format(covidmapmun()$porc_positivos, digits = 2, nsmall = 1), 
                      format(covidmapmun()$porc_regional, digits = 2, nsmall = 1 )) %>% 
      lapply(htmltools::HTML)
    # labels <- sprintf("<b> %s </b></br> Nº casos: %s </br> IA: %s", 
    #                   covidmapmun()$municip, covidmapmun()$ncasos, format(covidmapmun()$ia, digits = 3, nsmall = 2)) %>% 
    #   lapply(htmltools::HTML)
    
    l <- leaflet(covidmapmun())
    l <- l %>% addPolygons(color = "gray", weight = 1, fillColor = ~ pal(ia), fillOpacity = 1,
                           highlightOptions = highlightOptions(color = "white",weight = 1, bringToFront = TRUE),
                           label = labels,
                           labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "2px 6px"),
                                                       textsize = "12px",
                                                       direction = "auto",
                                                       opacity = 0.7)) %>%
      addLegend(pal = pal, 
                values = ~ ia, 
                opacity = 0.5, 
                title = HTML(paste0("<b style = font-size:14px>", paste0("IA ", input$periodo3, " días previos"), "</b>")), 
                position = "topright",
                bins = 10,
                labFormat = labelFormat(digits = 2)) 
    
  })
  
  # Salida para IA por zonabasica de salud
  
  # IA puntual por zona básica de salud
  
  covidsumzb <- reactive({
    cov_zb <- df_zbs %>% 
      filter(fecha == fecha_actual, periodo == input$periodo4) %>% 
      select(zonabasica, poblacion, ncasos, ia)
    
    if(input$ordena2){
      cov_zb %>% 
        arrange(desc(ia))
    }else{
      cov_zb
    }
  })
  
  output$iazonabasica <- renderDT({
    tab <- covidsumzb()
    col_ia <- paste0("IA", " ", input$periodo4, " previos")
    names(tab) <- c("Zona básica", "Población", "N casos", col_ia)
    tab <- tab %>%
      datatable(, 
                caption = htmltools::tags$caption("Servicio de Epidemiología. D.G. Salud Pública y Adicciones.Consejería de salud",
                                                  style = 'caption-side: bottom; font-size:9px'),
                rownames = FALSE,
                options = list(pageLength = 10,
                               searching = FALSE,
                               lengthMenu = list(c(10, 20, -1), c("10", "20", "Todos")),
                               language = list(url = "Español.json"),
                               columnDefs = list(list(width = "100px", targets = 3),
                                                 list(width = "75px", targets = 2),
                                                 list(width = "200px", targets = 0), 
                                                 list(width = "50px", targets = 1),
                                                 list(targets = 1:2, className = "dt-center"), 
                                                 list(targets = 3, className = "dt-right"),
                                                 list(targets = 0, className = "dt-left"), 
                                                 list(targets = 0:3, className = "dt-head-center"))
                )) %>%
      formatRound(col_ia)
    tab
  }
  )
  
  # Serie IA por ZBS
  
  covdifigzb <- reactive({
    
    switch (input$ventana2,
            'Desde 01/07/2020' = df <- df_zbs %>% 
              filter(periodo == input$periodo5), 
            'Últimos 120 días' = df <- df_zbs %>% 
              filter(fecha >= (fecha_actual - 120) & periodo == input$periodo5),
            'Últimos 60 días' = df <- df_zbs %>% 
              filter(fecha >= (fecha_actual - 60) & periodo == input$periodo5),
            'Últimos 28 días' = df <- df_zbs %>% 
              filter(fecha >= (fecha_actual - 28) & periodo == input$periodo5)
    )
    df <- df %>% 
      filter(zonabasica == "R. de Murcia" | zonabasica == input$zonabasica) %>% 
      mutate(zonabasica = droplevels(zonabasica))
    df
  })
  
  output$iagraphzb <- renderPlotly({
    colores <- c("#D14E72FF", "#0D0887FF")
    axis_y_title <- paste0("Incidencia acumulada ", input$periodo5)
    p <- plot_ly(covdifigzb(), x = ~ fecha, y = ~ round(ia, 2), color = ~ zonabasica, colors = colores) %>% 
      add_lines() %>% 
      layout(title = list(text = "Serie temporal IA",
                          xanchor = "left",
                          yanchor = "bottom", 
                          yref = "paper", 
                          xref = "paper",
                          x = 0, 
                          y = 1.1, 
                          font = list(size = 17)),
             legend = list(orientation = 'h'), 
             xaxis = list(title = ""), 
             yaxis = list(title = axis_y_title), 
             hovermode= "x")
    p <- config(p, locale = 'es')
    p <- p %>% 
      layout(annotations = list(text = 'Servicio de Epidemiologia. D.G. Salud Pública y Adicciones. Consejería de Salud.',
                                font = list(size = 9),
                                showarrow = FALSE,
                                xref = 'paper', x = 0,
                                yref = 'paper', y = -0.225))
    print(p)
  })
  
  
  # Mapa ZBS por área de salud
  
  covidmapazb <- reactive({
    cov_zb2 <- df_zbs %>% 
      filter(fecha == fecha_actual & periodo == input$periodo6 & codzbs != '1205')
    
    zonasbasicasph@data <- zonasbasicasph@data %>% 
      left_join(cov_zb2 %>% 
                  select(codzbs, ncasos, ia), 
                by = c('ZBS_MS2009' = 'codzbs'))
    
    zonasbasicasph@data <- zonasbasicasph@data %>% 
      left_join(pcrs_zbs, by = c('ZBS_MS2009' = 'codzb'))
    
    if (input$codarea == '0'){
      zonasbasicasph
    }else{
      subset(zonasbasicasph, AS_MS_2009 == as.numeric(input$codarea))
    }
  })
  
  output$mapzbsarea <- renderLeaflet({
    mapzb <- covidmapazb()
    
    pal <- colorNumeric(palette = "YlOrRd", domain = mapzb$ia)
    
    labels <- sprintf("<b, font-size = 28px> %s </b></br> Nº casos: %s </br> IA: %s </br> Porcentaje PDIA positivas: %s 
                  </br> Porcentaje PDIA positivas regional: %s", 
                      mapzb$zonabasica, 
                      mapzb$ncasos, 
                      format(mapzb$ia, digits = 3, nsmall = 2), 
                      format(mapzb$porc_positivos, digits = 2, nsmall = 1), 
                      format(mapzb$porc_regional, digits = 2, nsmall = 1 )) %>% 
      lapply(htmltools::HTML)
    
    l <- leaflet(mapzb)
    l %>% addPolygons(color = "gray", weight = 1, fillColor = ~ pal(ia), fillOpacity = 1,
                      highlightOptions = highlightOptions(color = "white",weight = 1, bringToFront = TRUE),
                      label = labels,
                      labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "2px 6px"),
                                                  textsize = "12px",
                                                  direction = "auto",
                                                  opacity = 0.7)) %>%
      addLegend(pal = pal, 
                values = ~ ia, 
                opacity = 0.5, 
                title = HTML(paste0("<b style = font-size:14px>", paste0("IA ", input$periodo6, " días previos"), "</b>")), 
                position = "topright",
                bins = 10,
                labFormat = labelFormat(digits = 2)) 
  })
  
  
  # Mapa ZBS por grandes municipios
  
  covidmapazbmun <- reactive({
    cov_zb2 <- df_zbs %>% 
      filter(fecha == fecha_actual & periodo == input$periodo7 & codzbs != '1205')
    
    zonasbasicasph@data <- zonasbasicasph@data %>% 
      left_join(cov_zb2 %>% 
                  select(codzbs, ncasos, ia), 
                by = c('ZBS_MS2009' = 'codzbs'))
    
    zonasbasicasph@data <- zonasbasicasph@data %>% 
      left_join(pcrs_zbs, by = c('ZBS_MS2009' = 'codzb'))
    
    if (input$codmunicipio3 != "0"){
      if(input$codmunicipio3 == "30030"){
        mapzb <- subset(zonasbasicasph,
                        CODMUNICIP == input$codmunicipio3 | CODMUNICIP == "30005")
      }else{
        mapzb <- subset(zonasbasicasph,
                        CODMUNICIP == input$codmunicipio3 | CODMUNIC00 == input$codmunicipio3)
      }
    }else{
      zonasbasicasph
    }
  })
  
  output$mapzbmun <- renderLeaflet({
    mapzb <- covidmapazbmun()
    
    pal <- colorNumeric(palette = "YlOrRd", domain = mapzb$ia)
    
    labels <- sprintf("<b, font-size = 28px> %s </b></br> Nº casos: %s </br> IA: %s </br> Porcentaje PDIA positivas: %s 
                  </br> Porcentaje PDIA positivas regional: %s", 
                      mapzb$zonabasica, 
                      mapzb$ncasos, 
                      format(mapzb$ia, digits = 3, nsmall = 2), 
                      format(mapzb$porc_positivos, digits = 2, nsmall = 1), 
                      format(mapzb$porc_regional, digits = 2, nsmall = 1 )) %>% 
      lapply(htmltools::HTML)
    
    l <- leaflet(mapzb)
    l %>% addPolygons(color = "gray", weight = 1, fillColor = ~ pal(ia), fillOpacity = 1,
                      highlightOptions = highlightOptions(color = "white",weight = 1, bringToFront = TRUE),
                      label = labels,
                      labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "2px 6px"),
                                                  textsize = "12px",
                                                  direction = "auto",
                                                  opacity = 0.7)) %>%
      addLegend(pal = pal, 
                values = ~ ia, 
                opacity = 0.5, 
                title = HTML(paste0("<b style = font-size:14px>", paste0("IA ", input$periodo7, " días previos"), "</b>")), 
                position = "topright",
                bins = 10,
                labFormat = labelFormat(digits = 2))  
  })
  
  observe({
    if (input$refresca1 | input$refresca2){
      session$reload()
    }
  })
  
  # Serie IA por grupos de edad
  
  output$iagraphedad <- renderPlotly({
    
    switch (input$ventana3,
            'Desde 01/07/2020' = df <- df_m %>% 
              filter(codmunicipio == '1205' & periodo == input$periodo8), 
            'Últimos 120 días' = df <- df_m %>%
              filter(fecha >= fecha_actual - 120 & codmunicipio == '1205' & periodo == input$periodo8),
            'Últimos 60 días' = df <- df_m %>%
              filter(fecha >= fecha_actual - 60 & codmunicipio == '1205' & periodo == input$periodo8),
            'Últimos 30 días' = df <- df_m %>%
              filter(fecha >= fecha_actual - 30 & codmunicipio == '1205' & periodo == input$periodo8)
    )
    
    df2 <- df %>% 
      filter(edad == 'Todos')
    
    if (input$grupo_edad == 1){
      df <- df %>% 
        filter(edad != 'Todos' & grupo_edad == input$grupo_edad) %>% 
        mutate(edad = factor(edad), 
               edad = factor(edad, 
                             levels = levels(edad)[c(2, 4, 3, 5, 6, 1)]))
      
      colores <- c("#0072B2", "#56B4E9", "#009E73", "#F0E442", "#E69F00", "#D55E00", "#CC79A7")
      
      p <- plot_ly()
      
      
      p <- p %>% add_lines(
        x = df$fecha, 
        y = round(df$ia, 2), 
        color = df$edad, 
        colors = colores[1:6]
      )
      
      axis_y_title <- paste('Incidencia acumulada', input$periodo8)
      
      p <- p %>% add_trace(x = df2$fecha, y = round(df2$ia, 2), 
                           name = "R. de Murcia", 
                           line = list(color = colores[7], 
                                       dash = 'dash'), 
                           mode = "lines") %>% 
        layout(title = list(text = "Serie temporal IA",
                            xanchor = "left",
                            yanchor = "bottom", 
                            yref = "paper", 
                            xref = "paper",
                            x = 0, 
                            y = 1.1, 
                            font = list(size = 17)),
               legend = list(orientation = 'l'), 
               xaxis = list(title = ""), 
               yaxis = list(title = axis_y_title, 
                            range = c(0, max(df$ia) + 0.10 * max(df$ia))), 
               hovermode = 'x')
      p <- config(p, locale = 'es')
      p <- p %>% 
        layout(annotations = list(text = 'Servicio de Epidemiologia. D.G. Salud Pública y Adicciones. Consejería de Salud.',
                                  font = list(size = 9),
                                  showarrow = FALSE,
                                  xref = 'paper', x = 0,
                                  yref = 'paper', y = -0.10))
    }
    if(input$grupo_edad == 2){
      
      df <- df %>% 
        filter(edad != 'Todos' & grupo_edad == input$grupo_edad) %>% 
        mutate(edad = factor(edad), 
               edad = factor(edad, 
                             levels = levels(edad)[c(1, 3, 4, 5, 6, 7, 8, 9, 2)]))
      
      colores <- c('#00FFFF', '#FF00FF', '#993299', '#0000FF', '#008080', 
                   '#008000', '#808000', '#FF0000', '#800000', '#CC79A7')
      
      p <- plot_ly()
      
      
      p <- p %>% add_lines(
        x = df$fecha, 
        y = round(df$ia, 2), 
        color = df$edad, 
        colors = colores[1:9]
      )
      
      axis_y_title <- paste('Incidencia acumulada', input$periodo8)
      
      p <- p %>% add_trace(x = df2$fecha, y = round(df2$ia, 2), 
                           name = "R. de Murcia", 
                           line = list(color = colores[10], 
                                       dash = 'dash'), 
                           mode = "lines") %>% 
        layout(title = list(text = "Serie temporal IA",
                            xanchor = "left",
                            yanchor = "bottom", 
                            yref = "paper", 
                            xref = "paper",
                            x = 0, 
                            y = 1.1, 
                            font = list(size = 17)),
               legend = list(orientation = 'l'), 
               xaxis = list(title = ""), 
               yaxis = list(title = axis_y_title, 
                            range = c(0, max(df$ia) + 0.10 * max(df$ia))), 
               hovermode = 'x')
      p <- config(p, locale = 'es')
      p <- p %>% 
        layout(annotations = list(text = 'Servicio de Epidemiologia. D.G. Salud Pública y Adicciones. Consejería de Salud.',
                                  font = list(size = 9),
                                  showarrow = FALSE,
                                  xref = 'paper', x = 0,
                                  yref = 'paper', y = -0.10))
    }
    print(p)
  })
}

shinyApp(ui, server)


