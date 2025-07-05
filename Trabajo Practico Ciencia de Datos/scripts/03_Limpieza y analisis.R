library(tidyverse)

# - - - - - - - - -
# 1. Limpiar los datos de personal
# - - - - - - - - -

datos_personal2 <- datos_personal %>% 
  mutate(
    met_pago = str_squish(met_pago),  # elimina espacios de más
    met_pago = case_when(
      str_detect(met_pago, "Precio exclusivo Abonando en 1 pago") ~ "Precio contado",
      str_detect(met_pago, "Hasta 12 cuotas s/int. con tarjetas de crédito") ~ "12 cuotas s/int. tarjeta",
      str_detect(met_pago, "Hasta 9 cuotas s/int. con tarjetas de crédito") ~ "9 cuotas s/int. tarjeta",
      str_detect(met_pago, "6 cuotas s/int. en tu factura 12 cuotas s/int. con tarjeta") ~ "6/12 cuotas s/int. factura/tarjeta",
      str_detect(met_pago, "Hasta 6 cuotas s/int. con tarjetas de crédito") ~ "6 cuotas s/int. tarjeta",
      str_detect(met_pago, "Hasta 3 cuotas s/int. con tarjetas de crédito") ~ "3 cuotas s/int. tarjeta",
      TRUE ~ met_pago
    )
  )

personal_limpieza <- datos_personal2 %>% 
  mutate(
    marca = str_extract(nombre, "^[^ ]+"),
    modelo = str_remove(nombre, "^[^ ]+\\s+"),
    precio = str_remove_all(precio, "\\$|\\."),
    link = paste0("tienda.personal.com.ar", link)
  )

personal <- personal_limpieza %>% 
  group_by(marca) %>% 
  transmute(
    modelo = modelo,
    precio = as.numeric(precio),
    met_pago = met_pago,
    link = link
  )

# - - - - - - - - -
# 2. Limpiar los datos de claro
# - - - - - - - - -

datos_claro2 <- datos_claro %>%
  mutate(
    met_pago = case_when(
      str_detect(met_pago, "HASTA 12 CUOTAS EN TU FACTURA") ~ "12 cuotas en factura",
      str_detect(met_pago, "OFERTA POR TIEMPO LIMITADO") ~ "Precio contado",
      str_detect(met_pago, "FACTURA") & str_detect(met_pago, "TARJETA") ~ "Pago en factura + tarjeta",
      TRUE ~ met_pago
    )
  )

claro_limpieza <- datos_claro2 %>% 
  mutate(
    marca = str_extract(nombre, "^[^ ]+"),
    modelo = str_remove(nombre, "^[^ ]+\\s+"),
    precio = str_remove_all(precio, "\\$|\\."),
    link = paste0("tienda.claro.com.ar", link)
  )

claro <- claro_limpieza %>% 
  group_by(marca) %>% 
  transmute(
    modelo = modelo,
    precio = as.numeric(precio),
    met_pago = met_pago,
    link = link
  )
# - - - - - - - - -
# 3. Normalizacion
# - - - - - - - - -

claro_norm <- claro %>% 
  mutate(
      modelo = if_else(marca == "Moto", str_c("Moto ", modelo), modelo),
      marca = if_else(marca == "Moto", "Motorola", marca),
      modelo = modelo %>%
        str_replace_all("([0-9]+)\\s*GB", "\\1GB") %>%
        str_replace_all("\\b5G\\b", "") %>%
        str_squish())        
    

personal_norm <- personal %>% 
  mutate(
    modelo = if_else(marca == "iPhone", str_c("iPhone ", modelo), modelo), 
    marca = if_else(marca == "iPhone", "Apple", marca),
    modelo = str_trim(str_remove(modelo, " - .*") %>%   str_replace_all("([0-9]+)\\s*GB", "\\1GB") %>%
                        str_replace_all("\\b5G\\b", "") %>%
                                                               str_squish()
                      
  ))
personal_norm <- personal_norm %>%           # (o el nombre que uses)
  group_by(marca, modelo) %>%               # mismo teléfono
  slice_min(precio, n = 1, with_ties = FALSE) %>%  # ⬅️ el más barato
  ungroup()

# - - - - - - - - -
# 4. Exportacion
# - - - - - - - - -

write.csv2(claro_norm, "input/claro_norm.csv")
write.csv2(personal_norm, "input/personal_norm.csv")

# - - - - - - - - -
# 5. Comparacion
# - - - - - - - - -

comparacion <- full_join(
  claro_norm %>% 
    select(
      marca,
      modelo,
      precio_claro = precio,
      met_pago_claro = met_pago,
      link_claro = link
    ), 
    personal_norm %>% 
    select(
      marca,
      modelo,
      precio_personal = precio,
      met_pago_personal = met_pago,
      link_personal = link
    ),
  by = c("marca", "modelo")
)
  
comparacion <- comparacion %>% 
  transmute(
    marca,
    modelo,
    precio_claro,
    precio_personal,
    dif_precio = precio_personal - precio_claro,
    mas_caro = case_when(
      dif_precio < 0 ~ "Claro",
      dif_precio > 0 ~ "Personal",
      TRUE ~ "Igual"
    ),
    met_pago_claro,
    met_pago_personal,
    link_claro,
    link_personal
  )


# - - - - - - - - -
# 6. Datos para graficar
# - - - - - - - - -

cant_productos <- full_join(
  claro_norm %>% 
    group_by(marca) %>% 
    summarise(cant_claro = n()),
  personal_norm %>% 
    group_by(marca) %>% 
    summarise(cant_personal = n()), 
  by = "marca"
) %>% 
  mutate(
    cant_claro = replace_na(cant_claro, 0),
    cant_personal = replace_na(cant_personal),
  ) #%>% 
  bind_rows(
    tibble(
      marca = "Total",
      cant_claro = sum(cant_productos$cant_claro),
      cant_personal = sum(cant_productos$cant_personal)
    )
    )



cant_productos_largo <- cant_productos %>%
  pivot_longer(cols = starts_with("cant_"),
               names_to = "empresa",
               names_prefix = "cant_",
               values_to = "Cantidad") %>% 
  mutate(Empresa = str_to_title(empresa),
         Marca = marca)

compsam <- comparacion %>% 
  filter(!is.na(dif_precio),
         marca == "Samsung")

compmoto <- comparacion %>% 
  filter(!is.na(dif_precio),
         marca == "Motorola")

boxplott <- comparacion %>% 
  select(marca, modelo, precio_claro, precio_personal) %>% 
  pivot_longer(cols = starts_with("precio_"),
               names_to = "empresa",
               names_prefix = "precio_",
               values_to = "Precio") %>% 
  mutate(Empresa = str_to_title(empresa),
         Marca = marca) %>% 
  filter(!is.na(Precio))

comparacion_mismo_cel <- comparacion %>% 
  filter(!is.na(dif_precio))

cuotas <- comparacion_mismo_cel %>% 
  select(marca, modelo, met_pago_claro, met_pago_personal) %>% 
  pivot_longer(cols = starts_with("met_pago_"),
               names_to = "empresa",
               names_prefix = "met_pago_",
               values_to = "met_pago") %>% 
  mutate(Empresa = str_to_title(empresa),
         Marca = marca) %>% 
  filter(!is.na(met_pago))

abba <- comparacion_mismo_cel %>%
  group_by(marca) %>% 
  summarise(modelo = n())

cantidad_modelos_total <- full_join(cant_productos %>% 
  transmute(
    marca = marca,
    total = cant_claro + cant_personal
  ),
    abba, by = "marca"
    ) %>% 
  mutate(modelo = replace_na(modelo, 0)) %>% 
  transmute(
    marca = marca,
    total = total - modelo
  )

# - - - - - - - - -
# 7. Exportacion
# - - - - - - - - -

write.csv2(comparacion, "output/comparacion.csv")
write.csv2(compsam, "output/comparacion_samsung.csv")
write.csv2(compmoto, "output/comparacion_motorola.csv")
write.csv2(cant_productos, "output/cantidad_productos.csv")
write.csv2(boxplott, "output/boxplot_precios.csv")
write.csv2(cuotas, "output/comp_medios_pago.csv")
write.csv2(cantidad_modelos_total,"output/cantidad_modelos_total.csv")