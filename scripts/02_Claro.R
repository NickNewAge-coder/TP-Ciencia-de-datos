# - - - - - - - - - - - - - - - - 
# Paso 1: Cargamos las librerias
# - - - - - - - - - - - - - - - - 

library(tidyverse)
library(RSelenium)
library(rvest)

# Paso 2: Iniciamos el driver de Selenium y armamos el cuadro

driver <- rsDriver(browser = "firefox", port = 4445L, chromever = NULL, check = FALSE)
remote_driver <- driver[["client"]] 

datos_claro <- tibble(
  nombre = character(),
  precio = character()
)

# - - - - - - - - - - - - - - - - 
# 2.1: Abrimos el navegador
# - - - - - - - - - - - - - - - - 

remote_driver$navigate("https://tienda.claro.com.ar/plp/equipos")

Sys.sleep(2)

# - - - - - - - - - - - - - - - - 
# 2.2: Obtener la URL actual
# - - - - - - - - - - - - - - - - 

url_actual <- remote_driver$getCurrentUrl()[[1]]

# - - - - - - - - - - - - - - - - 
# 2.3: Boton next page (no hay next page en Claro)
# - - - - - - - - - - - - - - - - 

#boton <- remote_driver$findElement(using = "css selector", value = '[class="emsye89z emsye8a3 emsye8a6 "]') (no me acuerdo por que lo dejé pero por las dudas no lo toco)

# - - - - - - - - - - - - - - - - 
# 3: Levantar los datos al tibble
# - - - - - - - - - - - - - - - - 

  html_renderizado <- remote_driver$getPageSource()[[1]]
  pagina <- read_html(html_renderizado)
  
  cuadros <- pagina %>% 
    html_elements(xpath = '//div[starts-with(@class, "CardProducto_cont-sin-comparar__UhTnG")]')
  
  nombre <- cuadros %>% 
    html_elements(css='[class="CardProducto_titulo__3ehLF"]') %>% 
    html_text2()
  
  precio <- cuadros %>% 
      html_elements(css = '[class="CardProducto_precio-principal__WZe2z"]') %>% 
      html_text2()
    
  met_pago <- cuadros %>% 
      html_elements(css = '[class="CardProducto_atributo-special__lwB75"]') %>% 
      html_text2()
  
  todo <- cuadros %>% 
    html_text2()
  
  link <- map_chr(cuadros, function(cuadro) {
    tag <- cuadro %>% html_element(css = '[class="genericLink_link__xNk6u"]')
    if (!is.na(tag)) {
      html_attr(tag, "href")
    } else {
      NA_character_
    }
  })
  
  tmp <- tibble(
    nombre = nombre,
    precio = precio,
    met_pago = met_pago,
    link = link,
    todo = todo )
  
  datos_claro <- bind_rows(datos_claro, tmp)


  
view(datos_claro)
driver$server$stop()

# - - - - - - - - - - - - - - - - 
# 4. Exportacion
# - - - - - - - - - - - - - - - - 
write.csv2(datos_claro,"raw/datos_claro.csv")
