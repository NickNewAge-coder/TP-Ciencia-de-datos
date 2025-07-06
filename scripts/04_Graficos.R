library(shiny)
library(ggplot2)
library(dplyr)
library(forcats)
library(scales)

ui <- fluidPage(
  titlePanel("Comparativa de celulares – Claro vs Personal"),
  tabsetPanel(
    tabPanel("Cantidad de dispositivos",
             plotOutput("plot_cant_dispositivos")),
    tabPanel("Diferencia de precios",
             fluidRow(
               column(4, offset = 4,
                      selectInput("marca_sel", "Elegí la marca:",
                                  choices = c("Samsung", "Motorola"),
                                  selected = "Samsung"))
             ),
             fluidRow(
               column(8, offset = 2,
                      plotOutput("plot_dif_precios"))
             )),
    tabPanel("Boxplot precios",
             plotOutput("plot_box")),
    tabPanel("Métodos de pago",
             plotOutput("plot_pago")),
    tabPanel("Modelos por marca",
             plotOutput("plot_modelos"))
  )
)

server <- function(input, output, session) {

# - - - - - - - - - - - - - - - - 
# 1 – Cantidad de dispositivos
# - - - - - - - - - - - - - - - - 
  
    output$plot_cant_dispositivos <- renderPlot({
    ggplot(cant_productos_largo, aes(Marca, Cantidad, fill = Empresa)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c(Claro = "#da272c", Personal = "#00a6c0")) +
      labs(title = "Cantidad de dispositivos", fill = "Empresa")
  })
# - - - - - - - - - - - - - - - -   
# 2 – Diferencia de precios (selector)
# - - - - - - - - - - - - - - - - 
    
  output$plot_dif_precios <- renderPlot({
    datos <- if (input$marca_sel == "Samsung") compsam else compmoto
    
    ggplot(datos, aes(fct_reorder(modelo, dif_precio), dif_precio, fill = mas_economico)) +
      geom_col() +
      coord_flip() +
      labs(title = paste0("Diferencia de precios (más caro) – ", input$marca_sel),
           x = "Modelo", y = "Diferencia (Personal – Claro)") +
      scale_fill_manual(values = c(Personal = "#00a6c0", Claro = "#da272c", Igual = "gray"),
                        name = "") +
      scale_y_continuous(labels = comma_format(prefix = "$", big.mark = ".", decimal.mark = ","))
  })
  
# - - - - - - - - - - - - - - - -   
# 3 – Boxplot precios
# - - - - - - - - - - - - - - - - 
  
  output$plot_box <- renderPlot({
    ggplot(boxplott, aes(fct_reorder(Marca, Precio), Precio, fill = Empresa)) +
      geom_boxplot() +
      coord_flip() +
      labs(title = "Comparación de precios entre Claro y Personal",
           x = "Marca", y = "Precio") +
      scale_fill_manual(values = c(Claro = "#da272c", Personal = "#00a6c0")) +
      scale_y_continuous(labels = comma_format(prefix = "$", big.mark = ".", decimal.mark = ",")) +
      theme_minimal()
  })
  
# - - - - - - - - - - - - - - - - 
# 4 – Métodos de pago
# - - - - - - - - - - - - - - - - 
  
  output$plot_pago <- renderPlot({
    cuotas_res <- cuotas %>% count(Empresa, met_pago)
    
    ggplot(cuotas_res, aes(n, met_pago, fill = Empresa)) +
      geom_col(position = "dodge") +
      labs(title = "Métodos de pago por empresa",
           x = "Cantidad de productos", y = "Método de pago") +
      scale_fill_manual(values = c(Claro = "#da272c", Personal = "#00a6c0")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
# - - - - - - - - - - - - - - - - 
# 5 – Modelos por marca
# - - - - - - - - - - - - - - - - 
  
  output$plot_modelos <- renderPlot({
    ggplot(cantidad_modelos_total, aes(reorder(marca, total), total, fill = marca)) +
      geom_col() +
      coord_flip() +
      labs(title = "Cantidad total de modelos por marca (ambas tiendas)",
           x = "Marca", y = "Cantidad total") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_manual(values = c(
        Samsung = "#1428A0", Motorola = "#000000", TCL = "#E64C3D", ZTE = "#008ED3",
        Apple = "#A2AAAD", Nubia = "#e4011f", Xiaomi = "#FF6900", OPPO = "#006B33"))
  })
}

shinyApp(ui, server)

