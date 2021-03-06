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

load("datos/Covid.Rdata")
load("datos/fec_act.Rdata")
load('datos/pcrs_mun.Rdata')
load('datos/pcrs_zbs.Rdata')

# Proceso de carga de la población de los municipios
municipios <- readxl::read_xlsx("datos/Municipios.xlsx") %>% 
  select(-MUNICIPIOINE)
pobtotal <- sum(municipios$POBMUN, na.rm = TRUE)

# Proceso de carga de la población por zonas básicas
zonabasica <- readxl::read_xlsx("datos/ZonaBasica.xlsx")
zonabasica <- zonabasica %>% 
  mutate(codzb = as.character(codzb))

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
                                                                     choices = c(14, 7)), 
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
                                                                     choices = c(14, 7)), 
                                                         style = "font-size:90%"))
                                  ),
                                  fluidRow(column(12, div(selectInput("ventana", 
                                                                      "Serie", 
                                                                      choices = setNames(
                                                                        rev(c(fecha_actual, "2020-07-01")),
                                                                        rev(c("Últimos 28 días", "Desde 01/07/2020")))), 
                                                          style = "font-size:90%"))
                                  ),
                                  fluidRow(column(12, div(selectInput("codmunicipio", 
                                                                      "Municipio", 
                                                                      choices = setNames(c( '9', municipios$CODMUNICIPIO[!municipios$municipio == 'Desconocido']),
                                                                                         c('Región de Murcia', municipios$municipio[!municipios$municipio == 'Desconocido']))), 
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
                                                                     choices = c(14, 7)), 
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
                                                                     choices = c(14, 7)), 
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
                                                                     choices = c(14, 7)), 
                                                         style = "font-size:90%"))
                                  ),
                                  fluidRow(column(12, div(selectInput("ventana2", 
                                                                      "Serie", 
                                                                      choices = setNames(
                                                                        rev(c(fecha_actual, "2020-07-01")),
                                                                        rev(c("Últimos 28 días", "Desde 01/07/2020")))), 
                                                          style = "font-size:90%"))
                                  ),
                                  fluidRow(column(12, div(selectInput("zonabasica", 
                                                                      "Zona básica de salud", 
                                                                      choices = sort(zonabasica$zonabasica[!zonabasica$zonabasica == 'Desconocida'])), 
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
                                                                     choices = c(14, 7)), 
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
                                                                     choices = c(14, 7)), 
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
                                                                     choices = c(14, 7)), 
                                                         style = "font-size:90%"))
                                  ),
                                  # fluidRow(column(8, div(selectInput("sexo", 
                                  #                                    "Sexo",
                                  #                                    choices = c("Ambos", "Mujer", "Hombre")), 
                                  #                        style = "font-size:90%"))
                                  # ),
                                  fluidRow(column(12, div(selectInput("ventana3", 
                                                                      "Serie", 
                                                                      choices = setNames(
                                                                        rev(c(fecha_actual, "2020-07-01")),
                                                                        rev(c("Últimos 28 días", "Desde 01/07/2020")))), 
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
                              HTML("<p style='color:blue'>Por problemas técnicos en la migración de datos a una nueva BD hemos cambiado transitoriamente la fuente de datos 
                                   a los informes procedentes de los laboratorios. El % de positividad en las pruebas es sobre casos prevalentes.</p>"),
                              HTML("La fuente de datos para los resultados que presentamos es aquella que se recopila tras la entrevista con cada uno de los pacientes, en la que
                           se obtienen datos sociodemográficos, clínicos y epidemiológicos y donde se verifican los datos con eliminación de duplicados.
                          La fecha del evento es la fecha del diagnóstico microbiológico (Pruebas de Diagnóstico de Infección Activa, PDIA: PCR o test de detección rápida de antígeno). 
                          Del conjunto de datos se han excluido los casos detectados que no residen en la comunidad autónoma. 
                          Los datos se presenta a la fecha actual menos 48 horas.</br></br>"),
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
# input$periodo <- input$periodo1 <- input$periodo2 <- input$periodo3 <- input$periodo4 <- input$periodo5 <- 14
# input$ventana <- input$ventana1 <- input$ventana2 <- input$ventana3 <- input$ventana4 <- fecha_actual
# input$ordena <- input$ordena1 <- input$ordena2 <- FALSE
# input$codmunicipio <- '9'

server <- function(input, output, session) {
  
  # Agrega por municipio
  covidsum <- reactive({
    cov_mun <- covid %>%
      filter(fecdx <= fecha_actual & fecdx > fecha_actual - as.numeric(input$periodo)) %>% 
      group_by(codmunicipio_asig) %>% 
      summarize(ncasos = n())
    cov_mun <- municipios %>% 
      left_join(cov_mun, by = c("CODMUNICIPIO" = "codmunicipio_asig"))
    cov_mun$ncasos[is.na(cov_mun$ncasos)] <- 0
    
    cov_mun <- cov_mun %>% 
      mutate(ia = 1e5 * ncasos/POBMUN, 
             ncasos = as.integer(ncasos), 
             POBMUN = as.integer(POBMUN)) %>% 
      select(municipio, POBMUN, ncasos, ia)
    cov_reg <- tibble(municipio = "Región de Murcia", 
                      POBMUN = pobtotal,
                      ncasos = sum(cov_mun$ncasos), 
                      ia = 1e5 * sum(cov_mun$ncasos, na.rm = TRUE)/pobtotal)
    cov_mun <- cov_reg %>% 
      bind_rows(cov_mun)
    cov_mun <- cov_mun %>% 
      filter(municipio != 'Desconocido')
    if (input$ordena1){
      cov_mun %>% 
        arrange(desc(ia))
    }else{
      cov_mun
    }
  })
  
  # Salida IA diaria por municipio
  output$iamunicipios <- renderDT({
    tab <- covidsum()
    col_ia <- paste0("IA", " ", input$periodo, " días previos")
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
  
  # Genera el conjunto de datos para la serie por municipios
  
  covdifig <- reactive({
    if (input$ventana == fecha_actual){
      fecha_actual_vec <- fecha_actual - 0:28 # Modificar cuando sea Sys.Date()
    }else{
      fecha_actual_vec <- as.Date(fecha_actual:as.Date(input$ventana), origin = "1970-01-01")
    }
    
    for(i in 1:length(fecha_actual_vec)){
      index <- covid$fecdx > fecha_actual_vec[i] - as.numeric(input$periodo2) & covid$fecdx <= fecha_actual_vec[i]
      covidsum2 <- covid[index, ] %>% 
        group_by(codmunicipio_asig) %>% 
        summarize(ncasos = n())
      
      dftemp <- municipios %>% 
        left_join(covidsum2, by = c("CODMUNICIPIO" = "codmunicipio_asig")) %>% 
        mutate(fecha = fecha_actual_vec[i], 
               ncasos = replace_na(ncasos, 0), 
               ia = 1e5 * ncasos/POBMUN) %>% 
        select(fecha, CODMUNICIPIO, municipio, POBMUN, ncasos, ia)
      
      df_reg <- tibble(fecha = fecha_actual_vec[i], 
                       CODMUNICIPIO = '9',
                       municipio = "Región de Murcia", 
                       POBMUN = pobtotal,
                       ncasos = sum(dftemp$ncasos), 
                       ia = 1e5 * sum(dftemp$ncasos)/pobtotal)
      dftemp <- dftemp %>% 
        bind_rows(df_reg)
      
      if (i == 1){
        df <- dftemp
      }else{
        df <- df %>% 
          bind_rows(dftemp)
      }
    }
    
    df <- df %>% 
      filter(municipio != 'Desconocido')
    
    df <- df %>% 
      filter(CODMUNICIPIO == "9" | CODMUNICIPIO == input$codmunicipio)%>% 
      mutate(municipio = factor(municipio), 
             municipio = relevel(municipio, "Región de Murcia")) 
    df
  })
  
  output$iagraph <- renderPlotly({
    colores <- c("#D14E72FF", "#0D0887FF")
    axis_y_title <- paste0("Incidencia acumulada ", input$periodo2, " días")
    if(length(unique(covdifig()$CODMUNICIPIO)) == 1) {
      colores <- colores[1]
      y <- -0.1
      } else {
        y <- -0.225
      }
    p <- plot_ly(covdifig(), x = ~ fecha, y = ~ round(ia, 2), color = ~ municipio, colors = colores
    ) %>% 
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
  
  # Mapa municipios
  
  covidmapmun <- reactive({
    cov_mun <- covid %>% 
      filter(fecdx <= fecha_actual & fecdx > fecha_actual - as.numeric(input$periodo3)) %>% 
      group_by(codmunicipio_asig) %>% 
      summarize(ncasos = n())
    
    municipiossph@data <- municipiossph@data %>% 
      left_join(cov_mun, by = c("codmncp" = "codmunicipio_asig")) %>% 
      mutate(ncasos = replace_na(ncasos, 0)) %>% 
      mutate(ia = 1e5 * ncasos/POBMUN)
    municipiossph@data <- municipiossph@data %>% 
      left_join(pcrs_mun, by = c('codmncp' = 'codmunicipio'))
    municipiossph
  })
  
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
  
  # IA puntual por zona básica de salud
  
  covidsumzb <- reactive({
    cov_zb <- covid %>% 
      filter(fecdx <= fecha_actual & fecdx > fecha_actual - as.numeric(input$periodo4)) %>% 
      group_by(codzonabasica_asig) %>% 
      summarize(ncasos = n())
    
    cov_zb <- zonabasica %>% 
      left_join(cov_zb, by = c("codzona2" = "codzonabasica_asig")) %>% 
      mutate(ncasos = replace_na(ncasos, 0)) %>% 
      mutate(ia = 1e5 * ncasos/pobzona) %>% 
      select(-codzb) %>% 
      arrange(zonabasica)
    
    cov_reg <- tibble(zonabasica = "Región de Murcia", 
                      pobzona = pobtotal, 
                      ncasos = sum(cov_zb$ncasos), 
                      ia = 1e5 * sum(cov_zb$ncasos)/pobtotal)
    cov_zb <- cov_reg %>% 
      bind_rows(cov_zb)
    cov_zb <- cov_zb %>% 
      filter(zonabasica != 'Desconocida')
    cov_zb <- cov_zb %>% 
      select(zonabasica, pobzona, ncasos, ia)
    if(input$ordena2){
      cov_zb %>% 
        arrange(desc(ia))
    }else{
      cov_zb
    }
  })
  
  output$iazonabasica <- renderDT({
    tab <- covidsumzb()
    col_ia <- paste0("IA", " ", input$periodo4, " días previos")
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
    if (input$ventana2 == fecha_actual){
      fecha_actual_vec <- fecha_actual - 0:28 # Modificar cuando sea Sys.Date()
    }else{
      fecha_actual_vec <- as.Date(fecha_actual:as.Date(input$ventana2), origin = "1970-01-01")
    }
    
    for(i in 1:length(fecha_actual_vec)){
      covidsum2 <- covid %>% 
        filter(fecdx > fecha_actual_vec[i] - as.numeric(input$periodo5) & fecdx <= fecha_actual_vec[i]) %>% 
        group_by(zonabasica_asig) %>% 
        summarize(ncasos = n())
      
      dftemp <- zonabasica %>% 
        left_join(covidsum2, by = c("zonabasica" = "zonabasica_asig")) %>% 
        mutate(fecha = fecha_actual_vec[i], 
               ncasos = replace_na(ncasos, 0), 
               ia = 1e5 * ncasos/pobzona) %>% 
        select(fecha, zonabasica, pobzona, ncasos, ia)
      
      df_reg <- tibble(fecha = fecha_actual_vec[i], 
                       zonabasica = "Región de Murcia", 
                       pobzona = pobtotal,
                       ncasos = sum(dftemp$ncasos), 
                       ia = 1e5 * sum(dftemp$ncasos)/pobtotal)
      dftemp <- dftemp %>% 
        bind_rows(df_reg)
      
      if (i == 1){
        df <- dftemp
      }else{
        df <- df %>% 
          bind_rows(dftemp)
      }
    }
    
    df <- df %>%
      filter(zonabasica != 'Desconocida')
    
    df <- df %>% 
      filter(zonabasica == "Región de Murcia" | zonabasica == input$zonabasica) %>% 
      mutate(zonabasica = factor(zonabasica), 
             zonabasica = relevel(zonabasica, ref = "Región de Murcia"))
    df
  })
  
  output$iagraphzb <- renderPlotly({
    colores <- c("#D14E72FF", "#0D0887FF")
    axis_y_title <- paste0("Incidencia acumulada ", input$periodo5, " días")
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
    cov_zb2 <- covid %>% 
      filter(fecdx<= fecha_actual & fecdx > fecha_actual - as.numeric(input$periodo6)) %>% 
      group_by(zonabasica_asig) %>% 
      summarize(ncasos = n())
    
    zonasbasicasph@data <- zonasbasicasph@data %>% 
      left_join(cov_zb2, by = c("zonabasica" = "zonabasica_asig")) %>% 
      mutate(ncasos = replace_na(ncasos, 0))
    
    zonasbasicasph@data <- zonasbasicasph@data %>% 
      mutate(ia = 1e5 * ncasos/pobzona)
    
    zonasbasicasph@data <- zonasbasicasph@data %>% 
      left_join(pcrs_zbs, by = c('ZBS_MS2009' = 'codzb'))
    
    zonasbasicasph
  })
  
  output$mapzbsarea <- renderLeaflet({
    
    if (input$codarea == "0"){
      mapzb <- covidmapazb()
    }else{
      mapzb <- subset(covidmapazb(),covidmapazb()$AS_MS_2009 == as.numeric(input$codarea))
    }
    
    pal <- colorNumeric(palette = "YlOrRd", domain = mapzb$ia)
    
    labels <- sprintf("<b, font-size = 28px> %s </b></br> Nº casos: %s </br> IA: %s </br> Porcentaje PDIA positivas: %s 
                  </br> Porcentaje PDIA positivas regional: %s", 
                      mapzb$zonabasica, 
                      mapzb$ncasos, 
                      format(mapzb$ia, digits = 3, nsmall = 2), 
                      format(mapzb$porc_positivos, digits = 2, nsmall = 1), 
                      format(mapzb$porc_regional, digits = 2, nsmall = 1 )) %>% 
      lapply(htmltools::HTML)
    
    # labels <- sprintf("<b> %s </b></br> Nº de casos: %s </br> IA: %s", 
    #                   mapzb$ZBS_MS2000, mapzb$ncasos, 
    #                   format(mapzb$ia, digits = 3, nsmall = 2)) %>% 
    #   lapply(htmltools::HTML)
    
    
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
    cov_zb2 <- covid %>% 
      filter(fecdx<= fecha_actual & fecdx > fecha_actual - as.numeric(input$periodo7)) %>% 
      group_by(zonabasica_asig) %>% 
      summarize(ncasos = n())
    
    zonasbasicasph@data <- zonasbasicasph@data %>% 
      left_join(cov_zb2, by = c("zonabasica" = "zonabasica_asig")) %>% 
      mutate(ncasos = replace_na(ncasos, 0))
    
    zonasbasicasph@data <- zonasbasicasph@data %>% 
      mutate(ia = 1e5 * ncasos/pobzona)
    
    zonasbasicasph@data <- zonasbasicasph@data %>% 
      left_join(pcrs_zbs, by = c('ZBS_MS2009' = 'codzb'))
    
    zonasbasicasph
  })
  
  output$mapzbmun <- renderLeaflet({
    
    if (input$codmunicipio3 != "0"){
      if(input$codmunicipio3 == "30030"){
        mapzb <- subset(covidmapazbmun(),
                        covidmapazbmun()$CODMUNICIP == input$codmunicipio3 | covidmapazbmun()$CODMUNICIP == "30005")
      }else{
        mapzb <- subset(covidmapazbmun(),
                        covidmapazbmun()$CODMUNICIP == input$codmunicipio3 | covidmapazbmun()$CODMUNIC00 == input$codmunicipio3)
      }
    }else{
      mapzb <- subset(covidmapazbmun())
    }
    
    pal <- colorNumeric(palette = "YlOrRd", domain = mapzb$ia)
    
    labels <- sprintf("<b, font-size = 28px> %s </b></br> Nº casos: %s </br> IA: %s </br> Porcentaje PDIA positivas: %s 
                  </br> Porcentaje PDIA positivas regional: %s", 
                      mapzb$zonabasica, 
                      mapzb$ncasos, 
                      format(mapzb$ia, digits = 3, nsmall = 2), 
                      format(mapzb$porc_positivos, digits = 2, nsmall = 1), 
                      format(mapzb$porc_regional, digits = 2, nsmall = 1 )) %>% 
      lapply(htmltools::HTML)
    
    # labels <- sprintf("<b> %s </b></br> Nº de casos: %s </br> IA: %s", 
    #                   mapzb$ZBS_MS2000, mapzb$ncasos, 
    #                   format(mapzb$ia, digits = 3, nsmall = 2)) %>% 
    #   lapply(htmltools::HTML)
    
    
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
    if (input$ventana3 == fecha_actual){
      fecha_actual_vec <- fecha_actual - 0:28 # Modificar cuando sea Sys.Date()
    }else{
      fecha_actual_vec <- as.Date(fecha_actual:as.Date(input$ventana3), origin = "1970-01-01")
    }
    
    # if (input$sexo != "Ambos"){
    #   covid1 <- covid %>% 
    #     filter(sexo == input$sexo)
    #   if (input$sexo == "Hombre"){
    #     pob_edad <- c(23303,
    #                   126995,
    #                   160959,
    #                   333969,
    #                   56961,
    #                   45428)
    #   }else{
    #     pob_edad <- c(22112,
    #                   119253,
    #                   153755,
    #                   320149,
    #                   64154,
    #                   66860)
    #   }
    # }else{
    #   covid1 <- covid
    #   pob_edad <- c(45415, 
    #                 246248, 
    #                 314714, 
    #                 654118, 
    #                 121115, 
    #                 112288)
    # }
    
    covid1 <- covid
    pob_edad <- c(45415,
                  246248,
                  314714,
                  654118,
                  121115,
                  112288)
    
    covidsum <- covid1 %>% 
      group_by(fecdx, edadcat) %>% 
      summarize(ncasos = n()) %>% 
      ungroup() %>% 
      complete(fecdx, edadcat, fill = list(ncasos = 0))
    
    for (i in 1:length(fecha_actual_vec)){
      df_temp <- covidsum %>% 
        filter(fecdx > fecha_actual_vec[i] - as.numeric(input$periodo8) & fecdx <= fecha_actual_vec[i]) %>% 
        group_by(edadcat) %>% 
        summarize(fecha = fecha_actual_vec[i], 
                  nc = sum(ncasos)) %>% 
        bind_cols(poblacion = pob_edad) %>% 
        mutate(ia = 1e5 * nc/poblacion) %>% 
        select(- poblacion)
      if (i > 1){
        df <- df %>% 
          bind_rows(df_temp)
      }else{
        df <- df_temp
      }
    }
    
    df2 <- df %>% 
      group_by(fecha) %>% 
      summarize(n = sum(nc), 
                ia = 1e5 * n/sum(pob_edad))
    
    colores <- c("#0072B2", "#56B4E9", "#009E73", "#F0E442", "#E69F00", "#D55E00", "#CC79A7")
    
    p <- plot_ly()
    
    
    p <- p %>% add_lines(
      x = df$fecha, 
      y = round(df$ia, 2), 
      color = df$edadcat, 
      colors = colores[1:6]
    )
    
    axis_y_title <- paste('Incidencia acumulada', input$periodo8, "días")
    
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
             yaxis = list(title = axis_y_title), 
             hovermode = 'x')
    p <- config(p, locale = 'es')
    p <- p %>% 
      layout(annotations = list(text = 'Servicio de Epidemiologia. D.G. Salud Pública y Adicciones. Consejería de Salud.',
                                font = list(size = 9),
                                showarrow = FALSE,
                                xref = 'paper', x = 0,
                                yref = 'paper', y = -0.10))
    print(p)
    
  })
}

shinyApp(ui, server)


