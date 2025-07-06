# - - - - - - - - - - - - - - - - 
# Paso 1: Cargamos las librerias
# - - - - - - - - - - - - - - - - 

library(tidyverse)
library(RSelenium)
library(rvest)

# - - - - - - - - - - - - - - - - 
# Paso 2: Iniciamos el driver de Selenium y armamos el cuadro
# - - - - - - - - - - - - - - - - 

driver <- rsDriver(browser = "firefox", port = 4444L, chromever = NULL, check = FALSE)
remote_driver <- driver[["client"]] 

datos_personal <- tibble(
  nombre = character(),
  precio = character()
)

# - - - - - - - - - - - - - - - - 
# 2.1: Abrimos el navegador
# - - - - - - - - - - - - - - - - 

remote_driver$navigate("https://tienda.personal.com.ar/celulares")

Sys.sleep(2)

# - - - - - - - - - - - - - - - - 
# 2.2: Obtener la URL actual
# - - - - - - - - - - - - - - - - 

url_actual <- remote_driver$getCurrentUrl()[[1]]

# - - - - - - - - - - - - - - - - 
# 2.3: Boton next page
# - - - - - - - - - - - - - - - - 

boton <- remote_driver$findElement(using = "css selector", value = '[class="emsye89z emsye8a3 emsye8a6 "]')

# - - - - - - - - - - - - - - - - 
# Boton cerrar popup
# - - - - - - - - - - - - - - - - 
boton_popup <- remote_driver$findElement(using = "css selector", value = '[class="fa-light fa-xmark _1mveze46"]')
boton_popup$clickElement()

# - - - - - - - - - - - - - - - - 
# 3: Levantar los datos al tibble
# - - - - - - - - - - - - - - - - 

for(i in 1:10){
  html_renderizado <- remote_driver$getPageSource()[[1]]
  pagina <- read_html(html_renderizado)

  cuadros <- pagina %>% 
   html_elements(xpath = '//a[starts-with(@data-testid, "product-card-container")]')

nombre <- cuadros %>% 
    html_elements(css='[class="emsye89i"]') %>% 
    html_text2()

# - - - - - - - - - - - - - - - - 
# 3.1: Sortear cosas raras que tiene personal al exponer sus precios
# - - - - - - - - - - - - - - - - 

precio <- map_chr(cuadros, function(cuadro) {
  # Intentamos obtener el precio general
  precio_general <- cuadro %>% 
    html_elements(css = '[class=" emsye88t"]') %>% 
    html_text2()
  
  if (length(precio_general) > 0) {
    return(precio_general)
  } else { 
   
    precio_alternativo <- cuadro %>% 
      html_elements(css = '[class=" emsye88r"]') %>% 
      html_text2()
    
    if (length(precio_alternativo) > 0) {
      return(precio_alternativo)
    } else {
      
      precio_alternativo2 <- cuadro %>% 
        html_elements(css = '[class="emsye88r "]') %>% 
        html_text2()
      
      
      if (length(precio_alternativo2) > 0) {
        return(precio_alternativo2)
      } else {
      return(NA_character_)
    }
  }
}})

met_pago <- map_chr(cuadros, function(cuadro) {
  cuotas <- cuadro %>% 
    html_elements(css = '[class="emsye88y"]') %>% 
    html_text2()
  
  if (length(cuotas) > 0) {
    return(cuotas)
  } else {
      return(NA_character_)
    }
})

  todo <- cuadros %>% 
    html_text2()
  
  link <- cuadros %>% 
    html_attr('href')

  tmp <- tibble(
    nombre = nombre,
    precio = precio,
    met_pago = met_pago,
    link = link,
    todo = todo )
  
  datos_personal <- bind_rows(datos_personal, tmp)
  
# - - - - - - - - - - - - - - - - 
# Claramente IA
# - - - - - - - - - - - - - - - - 
  
  url_antes <- remote_driver$getCurrentUrl()[[1]]   # URL actual

  boton$clickElement()                              # clic
  Sys.sleep(2)                                      # dejá que cargue
  
  url_desp  <- remote_driver$getCurrentUrl()[[1]]   # URL luego del clic
  
  if (identical(url_antes, url_desp)) break         # ⛔ no avanzó → salgo 

# - - - - - - - - - - - - - - - - 
# Claramente IA
# - - - - - - - - - - - - - - - - 
  
 }


view(datos_personal)
driver$server$stop()

# - - - - - - - - - - - - - - - - 
# Exportacion
# - - - - - - - - - - - - - - - - 
write_csv2(datos_personal, "raw/datos_personal.csv")
