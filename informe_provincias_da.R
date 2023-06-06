# Automatización de informes simples para provincias, en base a DA

rm(list = ls())

# Librerías

#devtools::install_github('CEProduccionXXI/DatosAbiertosCEP', force = TRUE)

library(data.table)
library(DatosAbiertosCEP)
library(ggplot2)
library(dplyr)
library(showtext)
library(sysfonts)
library(scales)
library(openxlsx)
library(xts)
library(RJDemetra)
library(stringr)
library(lubridate)
library(ggrepel)

# Rutas

# Rutas

ruta_pc <- "C:/Users/Usuario/"

# directorio pc escritorio: "C:/Users/nachi/"
# directorio pc lenovo: "C:/Users/Lenovo/"
# directorio pc HP: "C:/Users/usuario/"

ruta <- paste0(ruta_pc, "Documents/Datos Abiertos/reportes_informes/")

ruta_insumos <- paste0(ruta, "insumos/")

setwd(ruta)

# 0. SETUP ####

# Agregamos la fuente del ministerio que nos va a servir luego

# font_add_google("Encode Sans") por si falta la font

windowsFonts("roboto" = windowsFont("Roboto"))

### IMPORTANTE -> SETEAR ULTIMO MES DISPONIBLE / MES DE INFORME en formato 'yyyy-mm-dd'
ultimo_mes <- '2022-12-01'
### IMPORTANTE -> SETEAR ULTIMO MES DISPONIBLE / MES DE INFORME en formato 'yyyy-mm-dd'

# Traemos todo el universo de bases disponibles

bases <- DA_cruces_existentes()
setDT(bases)

# A priori, nos interesa quedarnos sólo de desagregación provincial, y del universo privado

bases <- bases[Jurisdiccion == "Provincia trabajo"]
bases <- bases[Sector == "NO"]
bases <- bases[Universo == "Privado"]

# Nos quedan cuatro bases simples: puestos, salario promedio y mediano, y share de mujer

indexes <- bases[, index]

# Necesitamos listas para guardar al loopear en cada serie las 24 provincias

# Para puestos, salario medio, salario mediano y participación femenina (las 4 variables):
# bases_1_full va a contener las series desde ene-2007 desagregadas por provincia

bases_1_full <- c()
bases_3_provs <- c()

# Queremos otra lista con los nombres de las variables particulares de cada base

variables <- c()

# Levantamos el diccionario para usar labels correctas para las variables

dicc_variables <- fread(paste0(ruta_insumos, "dicc_variables_da.csv"),
                        integer64 = "numeric",  encoding = 'Latin-1')

# 1. y 2. GRAFICOS SERIES DE TIEMPO GENERALES ####

# Vector de colores que necesitamos crear una sola vez y nos va a servir para los gráficos

interanual_color <- c("Crece" = "#009A46",
                      "Constante" = "#FFAB40",
                      "Cae" = "#C00000")

# Primer gran loop: escupe gráficos generales de las 4 variables para las 24 provincias. Con y sin var interanual

for(i in 1:length(indexes)){
  
  # Lo primero es alimentar la lista con las bases enteras
  
  tmp_da <- descarga_DA(indexes[i])
  setDT(tmp_da)
  bases_1_full[[i]] <- tmp_da
  
  # Necesitamos deflactar sólo las bases monetarias.
  
  if (i == 2 | i == 3) {
    tmp_da <- deflactar_DA(data = tmp_da, mes_base = ultimo_mes, pisar_datos = TRUE)
  }
  
  # De cada base, queremos los nombres de sus variables características
  
  variables[i] <- colnames(tmp_da)[3] # RECORDAR usamos bases de 3 columnas y la clave es la última
  
  # Guardamos la lista de provincias para usarla en el loop
  
  provs <- unique(tmp_da$zona_prov)
  
  # Nuevo loop para ir dentro de las 24 provincias
  
  for(p in 1:length(provs)){
    
    # Cortamos sólo la provincia donde estamos
    
    tmp_prov <- tmp_da[zona_prov == provs[p]]
    
    # Sólo para las bases monetarias, desestacionalizamos
    
    if (i == 2 | i == 3) {
      
      ts_prov <- ts(tmp_prov[, c(3)],start = c(2007), frequency = 12)
      
      #Descomponemos con RJDemetra
      
      ts_prov <- RJDemetra::x13(ts_prov)
      
      # Llevamos el resultado a dataframe
      
      ts_prov <- data.frame(.preformat.ts(ts_prov$final$series), stringsAsFactors = FALSE)
      
      # Pasar el nombre de las rows (fecha) a variable
      
      ts_prov <- tibble::rownames_to_column(ts_prov, var = 'fecha')
      
      # Damos formato a la fecha
      
      ts_prov$fecha <- lubridate::my(ts_prov$fecha)
      
      # Nos quedamos sólo con la serie desestacionalizada
      
      setDT(ts_prov)
      ts_prov <- ts_prov[, list(fecha, sa)]
      
      # Mergeamos con la serie anterior
      
      tmp_prov <- merge(tmp_prov[, list(fecha, zona_prov)], ts_prov, by='fecha')
      
      # Renombramos la desestacionalizada como una común
      
      setnames(tmp_prov, "sa", variables[i])
    }
    
    # Creamos una nueva columna con el año
    
    tmp_prov[, year := year(fecha)]
    
    # En un DT aparte sacamos los promedios, no podemos incluirlo de una porque difiere en cantidad de filas
    
    tmp_prov_anual <- tmp_prov[, .(prom_anual = mean(get(variables[i]))), by = "year"]
    
    # Ahora con tmp_prov_anual, sacamos la variación, salvo para el share de mujer que sacamos la dif en pp
    
    tmp_prov_anual[["prop_anual_var"]] <- NA
    
    # El loop es para todos los años menos para el primero, del cual no tenemos referncia interanual
    
    for (k in 1:(length(tmp_prov_anual$year)-1)) {
      
      if (i != 4) {
        
        # Asignamos, a partir de 2008, la variación interanual
        
        tmp_prov_anual[["prop_anual_var"]][k+1] <- tmp_prov_anual[["prom_anual"]][k+1] / 
          tmp_prov_anual[["prom_anual"]][k] - 1
        
      } else if (i==4) {
        
        # Acá va diferencia en puntos porcentuales
        
        tmp_prov_anual[["prop_anual_var"]][k+1] <- 
          (round(tmp_prov_anual[["prom_anual"]][k+1], digits = 4) * 100) -
          (round(tmp_prov_anual[["prom_anual"]][k], digits = 4) * 100)
        
      }
    }
    
    # Mergeamos en el original
    
    tmp_prov <- tmp_prov[tmp_prov_anual, on = "year"]
    tmp_prov$year <- NULL
    
    # Formateamos y limpiamos duplicados en variación
    
    tmp_prov$prop_anual_var[lubridate::month(tmp_prov$fecha) != 6] <- NA
    
    # Formateamos en cuatro dígitos
    
    tmp_prov$prop_anual_var <- round(tmp_prov$prop_anual_var, digits = 4)
    
    # Seteamos la variable que va a conectar con los colores que definimos antes
    
    tmp_prov$prop_anual_var_color <- cut(tmp_prov$prop_anual_var, c(-Inf, -0.01, 0.01, Inf), 
                                        labels = c("Cae", "Constante", "Crece"))
    
    # Guardamos de vuelta en lista datas
    
    bases_3_provs[[p+(24*(i-1))]] <- tmp_prov
    
    # Graficamos lo primero
    
    plot <- ggplot(tmp_prov, aes(x = fecha, y = !! sym((variables[[i]]))), lty = variables[i]) +
      geom_line(colour = "#0070C0", linewidth=0.5) +
      labs(title = paste0(dicc_variables[var %in% variables[i]]$lab," en ",
                          gsub("(?<=\\b.)(.*?)\\b", "\\L\\1", provs[p], perl=TRUE)),
           caption = 'Fuente: CEPXXI en base a datos de AFIP',
           x = ("Año"),
           y = (dicc_variables[var %in% variables[i]]$lab)) +
      xlab("Año") +
      scale_linetype("") +
      scale_x_date(date_breaks = "years", date_labels = "%Y") +
      theme(text = element_text(family = "roboto"),
            panel.background = element_rect(fill = "transparent"),
            axis.line = element_line(linewidth = 0.5, colour = "#000000", linetype = 1),
            panel.grid.major = element_line(color = "#D3D3D3", linetype = "dotted", size = 0.5),
            plot.title = element_text(size = 20, face='bold'),
            plot.subtitle = element_text(size=14,face='bold'),
            axis.text.x = element_text(color = "#000000", size = 12, hjust = -0.1),
            axis.text.y = element_text(color = "#000000", size = 12),
            plot.margin = margin(0.2,0.4,0.1,0.15, "cm"),
            plot.caption = element_text(hjust = 0))
    if (i==1) {
      plot <- plot + scale_y_continuous(labels = number_format(big.mark = ".", decimal.mark = ",")) +
        labs(subtitle=paste0("Empleo registrado privado. Serie mensual desde 2007 a ",
                             paste(month(ultimo_mes, label=T, local="es"), year(ultimo_mes), sep= "-")))
    } else if (i==2 | i==3) {
      plot <- plot + labs(subtitle=paste0("Serie desestacionalizada. En pesos a precios constantes de ",
                                          paste(month(ultimo_mes, label=T, local="es"), year(ultimo_mes), sep= "-"))) +
        scale_y_continuous(labels = number_format(big.mark = ".", decimal.mark = ","))
    } else if (i==4) {
      plot <- plot + labs(subtitle=paste0("Empleo registrado privado. Serie mensual desde 2007 a ",
                                          paste(month(ultimo_mes, label=T, local="es"), year(ultimo_mes), sep= "-"))) +
        scale_y_continuous(labels = percent_format(accuracy = 1))
    }
    
    # Guardamos en png. Puede ser en pdf también, pero tenía problemas con la fuente
    
    ggsave(paste0(ruta, "1.outputs_generales/", provs[p], "_", variables[[i]], ".png"),
           plot = last_plot(),
           width = 1296, height = 864, units = "px", dpi = 150)
    
    # Graficamos lo segundo. Es un copy paste con modificaciones en la linea, su opacidad, y agregados
    
    plot <- ggplot(tmp_prov, aes(x = fecha, y = !! sym((variables[[i]]))), lty = variables[i]) +
      geom_line(colour = "#95b3d7", linewidth=0.5, alpha=0.5) +
      labs(title = paste0(dicc_variables[var %in% variables[i]]$lab," en ",
                          gsub("(?<=\\b.)(.*?)\\b", "\\L\\1", provs[p], perl=TRUE)),
           caption = paste0('Fuente: CEPXXI en base a datos de AFIP. \n',
                    'Nota: Las líneas celestes indican promedio anual. Los porcentajes, variación entre promedios.'),
           x = ("Año"),
           y = (dicc_variables[var %in% variables[i]]$lab)) +
      scale_linetype("") +
      scale_x_date(date_breaks = "years", date_labels = "%Y") +
      geom_line(aes(y = prom_anual, group = prom_anual), color = "#00B0F0", linewidth=1) +
      scale_colour_manual(values = interanual_color) +
      theme(text = element_text(family = "roboto"),
            panel.background = element_rect(fill = "transparent"),
            axis.line = element_line(linewidth = 0.5, colour = "#000000", linetype = 1),
            panel.grid.major = element_line(color = "#D3D3D3", linetype = "dotted", size = 0.5),
            plot.title = element_text(size = 20, face='bold'),
            plot.subtitle = element_text(size=14,face='bold'),
            axis.text.x = element_text(color = "#000000", size = 12, hjust = -0.1),
            axis.text.y = element_text(color = "#000000", size = 12),
            plot.margin = margin(0.2,0.4,0.1,0.15, "cm"),
            plot.caption = element_text(hjust = 0))
    if (i==1) {
      plot <- plot + scale_y_continuous(labels = number_format(big.mark = ".", decimal.mark = ",")) +
        labs(subtitle=paste0("Empleo registrado privado. Serie mensual desde 2007 a ",
                             paste(month(ultimo_mes, label=T, local="es"), year(ultimo_mes), sep= "-"))) +
        geom_text(data=tmp_prov[!is.na(prop_anual_var)],
                  aes(y = prom_anual, label = scales::percent(prop_anual_var, accuracy = 0.1),
                      color = prop_anual_var_color, fontface = "bold"), 
                  size = 4, vjust = -1, hjust = 0.5, show.legend = FALSE)
    } else if (i==2 | i==3) {
      plot <- plot + labs(subtitle=paste0("Serie desestacionalizada. En pesos a precios constantes de ",
                                          paste(month(ultimo_mes, label=T, local="es"), year(ultimo_mes), sep= "-"))) +
        scale_y_continuous(labels = number_format(big.mark = ".", decimal.mark = ",")) +
        geom_text(data=tmp_prov[!is.na(prop_anual_var)],
                  aes(y = prom_anual, label = scales::percent(prop_anual_var, accuracy = 0.1),
                      color = prop_anual_var_color, fontface = "bold"), 
                  size = 4, vjust = -1, hjust = 0.5, show.legend = FALSE)
    } else if (i==4) {
      plot <- plot + labs(subtitle=paste0("Empleo registrado privado. Serie mensual desde 2007 a ",
                                          paste(month(ultimo_mes, label=T, local="es"), year(ultimo_mes), sep= "-"))) +
        geom_text(data=tmp_prov[!is.na(prop_anual_var)],
                  aes(y = prom_anual, label = paste0(as.character(prop_anual_var*100),"pb"),
                      color = prop_anual_var_color, fontface = "bold"), 
                  size = 3.8, vjust = -1, hjust = 0.5, show.legend = FALSE) +
        scale_y_continuous(labels = percent_format(accuracy = 1))
    }
    
    # Guardamos en png la variación
    
    ggsave(paste0(ruta, "2.outputs_generales_varanual/", provs[p], "_", variables[[i]], "_varanuales.png"),
           plot = last_plot(),
           width = 1296, height = 864, units = "px", dpi = 150)
    
      }
  }

# Recordar: los outliers en la serie de salario medio procesada en Jujuy y la Rioja son por indemnizaciones
# Son el cierre de Mina El Aguilar en febrero de 2021 en Jujuy y de la textil Unilever en octubre 2019

# Borramos excedentes temporales

rm(tmp_da, tmp_prov, tmp_prov_anual, ts_prov, interanual_color, plot)

# 3. y 4. GRAFICOS DE BARRAS PARA EL ULTIMO MES A LETRA Y CLAE2 ####

# Vamos con otro loop similar pero ahora buscando gráficos sectoriales, que vayan variando por prov

# Reiniciamos todo el universo de bases disponibles

bases <- DA_cruces_existentes()
setDT(bases)

# Ahora nos interesa quedarnos con desagregación provincial y del universo privado, pero apertura sectorial

bases <- bases[Jurisdiccion == "Provincia trabajo"]
bases <- bases[Universo == "Privado"]

# De vuelta nos quedan las cuatro bases de antes, pero ahora con las aperturas

indexes_provletra <- bases[Sector == "Letra"][, index]
indexes_provclae2 <- bases[Sector == "CLAE2"][, index]
rm(bases)

# El vector de variables ya lo tenemos, asique sólo llamamos a los nuevos vectores donde almacenar las bases

bases_2_letra <- c()
bases_2_clae2 <- c()

bases_3_provletra <- c()
bases_3_provclae2 <- c()

# Levantamos el diccionario para usar labels correctas para los sectores a letra y a clae2

dicc_letras <- fread(paste0(ruta_insumos, "dicc_agregado_letra.csv"),
                     integer64 = "numeric",  encoding = 'Latin-1')
dicc_clae2 <- fread(paste0(ruta_insumos, "dicc_clae2_red.csv"),
                     integer64 = "numeric",  encoding = 'UTF-8')

# Creamos los vectores con sus labels. Wrapeamos acá adentro

letras_labels <- setNames(str_wrap(dicc_letras$letra_y_desc_red, width = 19), dicc_letras$letra)
clae2_labels <- setNames(str_wrap(dicc_clae2$clae2_desc_reducida, width = 20), dicc_clae2$clae2)

# El vector wrapeado de letra lo agregamos al dicc letras porque lo vamos a levantar luego

dicc_letras <- cbind(dicc_letras, letras_labels)

# Tramos el dicc armado a mano de color por letra

letra_color <- c("A" = "#0070C0",
                 "B" = "#00B0F0",
                 "C" = "#95b3d7",
                 "D" = "#0097A7",
                 "E" = "#50B8B1",
                 "F" = "#009A46",
                 "G" = "#92A000",
                 "H" = "#92CC7E",
                 "I" = "#D1DE82",
                 "J" = "#F87743",
                 "K" = "#FF5353",
                 "L" = "#FEE882",
                 "M" = "#0074AD",
                 "N" = "#7030a0",
                 "O" = "#9982B4",
                 "P" = "#ee3d8f",
                 "Q" = "#00BCB8",
                 "R" = "#33CCCC",
                 "S" = "#C00000",
                 "T" = "#FFC000",
                 "U" = "#F64E2C",
                 "Z" = "#000000")

# Unificams el vector de las letras wraped con el de los codigos de colores

letra_color <- setNames(letra_color, letras_labels)

# Arrancamos el nuevo loop

for(i in 1:length(indexes)){
  
  # Primero alimentamos ambas bases
  
  tmp_da_letra <- descarga_DA(indexes_provletra[i])
  tmp_da_clae2 <- descarga_DA(indexes_provclae2[i])
  setDT(tmp_da_letra)
  setDT(tmp_da_clae2)
  bases_2_letra[[i]] <- tmp_da_letra
  bases_2_clae2[[i]] <- tmp_da_clae2
  
  # Deflactar las bases monetarias. Más allá qie funciona sólo en las necesarias, con el if es mas prolijo
  
  if (i == 2 | i == 3) {
    tmp_da_letra <- deflactar_DA(data = tmp_da_letra, mes_base = ultimo_mes, pisar_datos = TRUE)
    tmp_da_clae2 <- deflactar_DA(data = tmp_da_clae2, mes_base = ultimo_mes, pisar_datos = TRUE)
  }
  
  # Las listas de provincia y variable ya las tengo, vamos de una al loop provincial
  
  for(p in 1:length(provs)){
    
    # Cortamos sólo la provincia donde estamos
    
    tmp_prov_letra <- tmp_da_letra[zona_prov == provs[p]]
    tmp_prov_clae2 <- tmp_da_clae2[zona_prov == provs[p]]
    
    # Guardamos de vuelta en lista datas
    
    bases_3_provletra[[p+(24*(i-1))]] <- tmp_prov_letra
    bases_3_provclae2[[p+(24*(i-1))]] <- tmp_prov_clae2
    
    # Vamos a ver estadísticas para el último mes
    
    tmp_prov_letra <- tmp_prov_letra[fecha == ultimo_mes]
    tmp_prov_clae2 <- tmp_prov_clae2[fecha == ultimo_mes]
    
    # Sólo si estamos en puestos, queremos proporciones relativas
    
    if (i == 1) {
      tmp_prov_letra <- tmp_prov_letra[,prop:=(puestos / sum(puestos))]
      tmp_prov_letra$prop <- percent(tmp_prov_letra$prop, accuracy=0.1)
      
      tmp_prov_clae2 <- tmp_prov_clae2[,prop:=(puestos / sum(puestos))]
      tmp_prov_clae2$prop <- percent(tmp_prov_clae2$prop, accuracy=0.1)
    }
    
    # Sacamos arafue al sector residual
    
    tmp_prov_letra <- tmp_prov_letra[letra != "Z"]
    tmp_prov_clae2 <- tmp_prov_clae2[clae2 != 999]
    
    # Primero para letra, dejamos los top 10 sectores, de puestos, salarios o proporciones,la variable en cuesitón
    
    tmp_prov_letra <- head(setorderv(tmp_prov_letra,variables[i],order = -1),10)
    
    # Para letra, pasamos por la función factor para que salga en orden
    
    tmp_prov_letra$letra <- factor(tmp_prov_letra$letra, levels = tmp_prov_letra$letra)
    
    # Para clae2, agregamos la letra con el diccionario, porque con eso le vamos a dar color a las barras
    
    tmp_prov_clae2 <- diccionario_sectores(tmp_prov_clae2, agregacion = 'letra', descripciones = FALSE)
    
    # Mergeamos con el diccionario con letra y descprición
    
    tmp_prov_clae2 <- merge(tmp_prov_clae2, dicc_letras[,c('letra', 'letras_labels')], by = 'letra')
    
    # También mergeamos con el diccionario de letra color
    
    #tmp_prov_clae2 <-merge(tmp_prov_clae2, 
                           #data.table(letra = names(letra_color), cod_color_letra = unlist(letra_color)), 
                           #by = "letra")
    
    # Ahora sí para clae2 cortamos top 10 y factoreamos también
    
    tmp_prov_clae2 <- head(setorderv(tmp_prov_clae2,variables[i],order = -1),10)
    tmp_prov_clae2$clae2 <- factor(tmp_prov_clae2$clae2, levels = tmp_prov_clae2$clae2)
    
    # Graficamos primero por letra
    
    plot <- ggplot(data = tmp_prov_letra, aes(x = letra, y = !! sym(variables[i])))  +
            geom_bar(colour = "#63c6f3", fill = "#63c6f3", stat="identity") +
            labs(title = paste0("10 principales sectores según ",
                                tolower(dicc_variables[var %in% variables[i]]$lab)),
                 subtitle = paste0(gsub("(?<=\\b.)(.*?)\\b", "\\L\\1", provs[p], perl=TRUE), ", para ",
                                   paste(month(ultimo_mes, label=T, local="es"), year(ultimo_mes), sep= "-"),
                                        ". Empleo privado a nivel de letra"),
                caption = 'Fuente: CEPXXI en base a datos de AFIP.') +
            ylab(dicc_variables[var %in% variables[i]]$lab) +
            xlab("Sector") +
            scale_x_discrete(labels = letras_labels) +
            theme(text = element_text(family = "roboto"),
                  panel.background = element_rect(fill = "transparent"),
                  axis.line = element_line(linewidth = 0.5, colour = "#000000", linetype=1),
                  panel.grid.major = element_line(color = "#D3D3D3", linetype = "dotted", size = 0.5),
                  plot.title = element_text(size = 18, face='bold'),
                  plot.subtitle = element_text(size=14,face='bold'),
                  axis.text.x = element_text(color = "#000000", size = 12, angle=40,hjust=1),
                  axis.text.y = element_text(color = "#000000", size = 12),
                  plot.caption = element_text(hjust = 0))
    
    if (i==1) {
      plot + geom_text(data = tmp_prov_letra, aes(label=prop, y=!! sym((variables[[i]]))), 
                       size = 5, colour = '#000000', nudge_y = 6, fontface = 'bold') +
             scale_y_continuous(labels = number_format(big.mark = ".", decimal.mark = ","))
    } else if (i == 2 | i == 3) {
      plot + scale_y_continuous(labels = number_format(big.mark = ".", decimal.mark = ","))
    } else if (i == 4) {
      plot + scale_y_continuous(labels = percent_format(accuracy = 1))
    }
    
    ggsave(paste0(ruta, "3.outputs_letra/", provs[p], "_", variables[[i]], "_letra.png"),
           plot = last_plot(),
           width = 1296, height = 864, units = "px", dpi = 150)
    
    # Graficamos de nuevo por clae2
    
    plot <- ggplot(data = tmp_prov_clae2, aes(x = clae2, y = !! sym(variables[i]), fill = letras_labels))  +
      geom_bar(stat="identity") +
      labs(title = paste0("10 principales sectores según ",
                          tolower(dicc_variables[var %in% variables[i]]$lab)),
           subtitle = paste0(gsub("(?<=\\b.)(.*?)\\b", "\\L\\1", provs[p], perl=TRUE),
                             ", para ", paste(month(ultimo_mes, label=T, local="es"), year(ultimo_mes), sep= "-"),
                             ". Empleo privado a nivel de CLAE a 2 dígitos"),
           caption = 'Fuente: CEPXXI en base a datos de AFIP.',
           fill = "Sector a letra") +
      ylab(dicc_variables[var %in% variables[i]]$lab) +
      xlab("Sector") +
      scale_x_discrete(labels = clae2_labels) +
      scale_fill_manual(values = letra_color) +
      theme(text = element_text(family = "roboto"),
            panel.background = element_rect(fill = "transparent"),
            axis.line = element_line(linewidth = 0.5, colour = "#000000", linetype=1),
            panel.grid.major = element_line(color = "#D3D3D3", linetype = "dotted", size = 0.5),
            plot.title = element_text(size = 18, face='bold'),
            plot.subtitle = element_text(size=14,face='bold'),
            axis.text.x = element_text(color = "#000000", size = 10, angle=40, hjust=1),
            axis.text.y = element_text(color = "#000000", size = 12),
            plot.caption = element_text(hjust = 0),
            plot.margin = margin(0.2,0.3,0.1,0.5, "cm"))
    
    if (i==1) {
      plot + geom_text(data = tmp_prov_clae2, aes(label=prop, y=!! sym((variables[[i]]))), 
                       size = 5, colour = '#000000', nudge_y = 6, fontface = 'bold') +
        scale_y_continuous(labels = number_format(big.mark = ".", decimal.mark = ","))
    } else if (i == 2 | i == 3) {
      plot + scale_y_continuous(labels = number_format(big.mark = ".", decimal.mark = ","))
    } else if (i == 4) {
      plot + scale_y_continuous(labels = percent_format(accuracy = 1))
    }
    
    ggsave(paste0(ruta, "4.outputs_clae2/", provs[p], "_", variables[[i]], "_clae2.png"),
           plot = last_plot(),
           width = 1296, height = 864, units = "px", dpi = 150)
  }
  
}

# Sacamos residuos

rm(tmp_da_letra, tmp_prov_letra, tmp_da_clae2, tmp_prov_clae2, plot)

# 5. y 6. GRAFICOS DE PUNTOS DE DOS DIMENSIONES QUE CRUZAN REMU Y PUESTOS POR LETRA Y CLAE2 ####

# Nuevo loop para mezclar remuneracion y puestos por sector

# Definimos colores para futuros plots

letragg_color <- c("Extractivas" = "#33CCCC",
                   "Industria" = "#00B0F0",
                   "Ss. no calificados" = "#95b3d7",
                   "Ss. calificados" = "#0074AD",
                   "Adm. Pública" = "#7030a0")

# Puestos empieza en 1. W promedio empieza en 25

for(p in 1:length(provs)) {
  
  # Creamos las bases temporales a partir de mergear la que almacenamos de puestos con la de salario promedio

  tmp_letra_wypuestos <- merge(bases_3_provletra[[p]], bases_3_provletra[[p+24]],
                               by = c("fecha", "zona_prov", "letra"))
  tmp_clae2_wypuestos <- merge(bases_3_provclae2[[p]], bases_3_provclae2[[p+24]],
                               by = c("fecha", "zona_prov", "clae2"))

  # Cortamos a último mes
  
  tmp_letra_wypuestos <- tmp_letra_wypuestos[fecha == ultimo_mes]
  tmp_clae2_wypuestos <- tmp_clae2_wypuestos[fecha == ultimo_mes]

  # Ordenamos los 10 principales
  
  tmp_letra_wypuestos <- head(setorderv(tmp_letra_wypuestos,'puestos',order = -1),10)
  tmp_clae2_wypuestos <- head(setorderv(tmp_clae2_wypuestos,'puestos',order = -1),10)

  # Mergeamos con el diccionario manual
  
  tmp_letra_wypuestos <- merge(tmp_letra_wypuestos, dicc_letras[, c('letra', 'letras_labels', 'letras_agg')],
                             by = c("letra"))
  tmp_clae2_wypuestos <- diccionario_sectores(tmp_clae2_wypuestos, agregacion = 'letra', descripciones = TRUE)
  tmp_clae2_wypuestos <- merge(tmp_clae2_wypuestos, dicc_letras[, c('letra', 'letras_labels')],
                             by = c("letra"))
  
  # Sólo para clae2, tomamos la versión reducida y wrapeamos para evitar strings excesivamente largas
  
  tmp_clae2_wypuestos <- merge(tmp_clae2_wypuestos, dicc_clae2[, c('clae2', 'clae2_desc_reducida')],
                               by = c("clae2"))
  tmp_clae2_wypuestos$clae2_desc_reducida <- str_wrap(tmp_clae2_wypuestos$clae2_desc_reducida, width = 20)
  
  # Ploteamos

  ggplot(tmp_letra_wypuestos, aes(x=puestos,y=w_mean,color=letras_agg,label=letras_labels)) +
    geom_point (size = 4) +
    geom_text_repel(lineheight = 0.8, force = 3, fontface = 'bold') +
    labs(title = "Principales sectores según puestos y su salario bruto promedio",
         subtitle = paste0(gsub("(?<=\\b.)(.*?)\\b", "\\L\\1", provs[p], perl=TRUE), ", para ",
                           paste(month(ultimo_mes, label=T, local="es"), year(ultimo_mes), sep= "-"),
                           ". Empleo privado a nivel de letra"),
         caption = paste0('Fuente: CEPXXI en base a datos de AFIP. \n',
                          'Nota: Agrupación por rama según: A y B en Extractivas; C, D y E en Industria; ',
                          'J, K, M, N, P y Q en Servicios Calificados;\n',
                          'F, G, H, I, L, R y S en Servicios No Calificados')) +
    ylab("Salario promedio") +
    xlab("Puestos") +
    scale_y_continuous(labels = number_format(big.mark = ".", decimal.mark = ",")) +
    scale_x_continuous(labels = number_format(big.mark = ".", decimal.mark = ",")) +
    scale_colour_manual(values = letragg_color, name = "Rama de sectores") +
    theme(text = element_text(family = "roboto"),
          panel.background = element_rect(fill = "transparent"),
          axis.line = element_line(linewidth = 0.5, colour = "#000000", linetype=1),
          panel.grid.major = element_line(color = "#D3D3D3", linetype = "dotted", size = 0.5),
          plot.title = element_text(size = 16, face='bold'),
          plot.subtitle = element_text(size=14,face='bold'),
          axis.text.x = element_text(color = "#000000", size = 12),
          axis.text.y = element_text(color = "#000000", size = 12),
          plot.caption = element_text(hjust = 0),
          legend.key = element_rect(fill = "transparent"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12))
  
  ggsave(paste0(ruta, "5.outputs_cruce_letra/", provs[p], "_letra.png"),
         plot = last_plot(),
         width = 1296, height = 864, units = "px", dpi = 150)

  ggplot(tmp_clae2_wypuestos, aes(x=puestos,y=w_mean,color=letras_labels, label=clae2_desc_reducida)) +
    geom_point (size = 4) +
    geom_text_repel(lineheight = 0.8, force = 2) +
    labs(title = "Principales sectores según puestos y su salario bruto promedio",
         subtitle = paste0(gsub("(?<=\\b.)(.*?)\\b", "\\L\\1", provs[p], perl=TRUE), ", para ",
                           paste(month(ultimo_mes, label=T, local="es"), year(ultimo_mes), sep= "-"),
                           ". Empleo privado a nivel CLAE 2 dígitos"),
         caption = paste0('Fuente: CEPXXI en base a datos de AFIP. \n',
                          'Nota: Los colores indican sector a letra al cual pertenece el sector a CLAE a 2 dígitos')) +
    ylab("Salario promedio") +
    xlab("Puestos") +
    scale_y_continuous(labels = number_format(big.mark = ".", decimal.mark = ",")) +
    scale_x_continuous(labels = number_format(big.mark = ".", decimal.mark = ",")) +
    scale_colour_manual(values = letra_color, name = "Letra") +
    theme(text = element_text(family = "roboto"),
          panel.background = element_rect(fill = "transparent"),
          axis.line = element_line(linewidth = 0.5, colour = "#000000", linetype=1),
          panel.grid.major = element_line(color = "#D3D3D3", linetype = "dotted", size = 0.5),
          plot.title = element_text(size = 18, face='bold'),
          plot.subtitle = element_text(size=14,face='bold'),
          axis.text.x = element_text(color = "#000000", size = 12),
          axis.text.y = element_text(color = "#000000", size = 12),
          plot.caption = element_text(hjust = 0),
          legend.key = element_rect(fill = "transparent"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12))
  
  ggsave(paste0(ruta, "6.outputs_cruce_clae2/", provs[p], "_clae2.png"),
         plot = last_plot(),
         width = 1296, height = 864, units = "px", dpi = 150)
}

# Sacamos residuos

rm(tmp_clae2_wypuestos, tmp_letra_wypuestos)


##### Para hacer los informes cabeza, pero no tan cabeza: Saquemos la info relevante de la provincia elegida
p <- 'CORRIENTES'
ultimo_mes <- '2022-12-01'

# Filmina 3
puestos <- bases_1_full[[1]][zona_prov== p] #para ver los puestos al ultimo mes
puestos <- puestos[,var_inter := (puestos/lag(puestos, n=12)-1)]
puestos <- puestos[,var_inter:=round(var_inter, digits = 4)] # Respecto al mismo mes del año ..., se aprecia una variación positiva en los puestos del ..%
# La variación entre el promedio anual del 2022 y el del 2021... sacar del grafico directamente
 
# Filmina 4
puestos_letra <- bases_2_letra[[1]][zona_prov== p & fecha == ultimo_mes]
puestos_letra <- puestos_letra[, totales := sum(puestos, na.rm=T)]
puestos_letra <- puestos_letra[, share := round((puestos/totales)*100, digits = 1)]
setorder(puestos_letra, -share)
puestos_letra <- puestos_letra[1:10,] # los sectores que mas puestos de trabajo concentran... 
puestos_letra <- puestos_letra[, suma_share:= sum(share, na.rm=T)] #De un total de 22 sectores, los 10 más importantes explican el ...% 

# Filmina 5
puestos_clae <- bases_2_clae2[[1]][zona_prov== p & fecha == ultimo_mes]
puestos_clae <- puestos_clae %>% diccionario_sectores(agregacion_deseada = c('clae2', 'letra'))
setorder(puestos_clae, -puestos)
puestos_clae <- puestos_clae[1:10,] # 10 principales sectores

# Filmina 6
mujer <- bases_1_full[[4]][zona_prov== p] #para ver el share de mujer al ultimo mes
mujer <- mujer %>% mutate(prom12 = rollmean(share_mujer, k=12, fill=NA, align='right')) 
#Si se quiere ver el promedio anual completo, observar los numeros solo a diciembre de cada año
mujer <- mujer[,var_inter := round((prom12 -lag(prom12, n=12)), digits = 4)] # ¨Para calcular los puntos basicos, pero salen del grafico igual
# un aumento de ... puntos porcentuales o ... puntos básicos respecto al promedio del año anterior (...% en 2021 vs ...% en 2022).

# Filmina 7
mujer_letra <- bases_2_letra[[4]][zona_prov== p & fecha == ultimo_mes]
setorder(mujer_letra, -share_mujer)
mujer_letra <- mujer_letra[,share_mujer:=round(share_mujer*100, digits = 1)] 
# Para obtener los porcentajes que se quieran destacar. El primer bullet es un tanto particular para cada vez... intentar que generalmente diga lo mismo

# Filmina 8
mujer_clae <- bases_2_clae2[[4]][zona_prov== p & fecha == ultimo_mes]
mujer_clae <- mujer_clae %>% diccionario_sectores(agregacion_deseada = c('clae2', 'letra'))
setorder(mujer_clae, -share_mujer)
mujer_clae <- mujer_clae[1:10,] # 10 principales sectores

# Filmina 9. El problema aca es encontrar el numero correcto para pegarle a la base 
sal_prom <- bases_3_provs[[31]] #el salario bruto promedio -excluyendo el sueldo anual complementario-, se ubica en ... pesos de ese mes.
sal_prom <- sal_prom[,var_inter := (w_mean/lag(w_mean, n=12)-1)]
sal_prom <- sal_prom[,var_inter:=round(var_inter, digits = 4)]
# Descontado el efecto de la inflación, se aprecia una crecimiento real del ...% con respecto al mismo mes del año 2021
# La variacion de los promedios anuales se saca directo del grafico

# Filmina 10
sal_prom_letra <- bases_3_provletra[[31]][fecha == ultimo_mes] 
setorder(sal_prom_letra, -w_mean)
sal_prom_letra <- sal_prom_letra[1:10,] # 10 sectores de mayor salario promedio

# Filmina 11
sal_prom_clae <- bases_3_provclae2[[31]][fecha == ultimo_mes] 
sal_prom_clae <- sal_prom_clae %>% diccionario_sectores(agregacion_deseada = c('clae2', 'letra'))
setorder(sal_prom_clae, -w_mean)
sal_prom_clae <- sal_prom_clae[1:10,] # 10 principales sectores

# Filmina 12 # Aca de nuevo vamos a tener q cambiar el numero q pegar. EL mismo de antes mas 24
sal_med <- bases_3_provs[[55]] #el salario bruto mediano -excluyendo el sueldo anual complementario-, se ubica en ... pesos de ese mes.
sal_med <- sal_med[,var_inter := (w_median/lag(w_median, n=12)-1)]
sal_med <- sal_med[,var_inter:=round(var_inter, digits = 4)]
# Descontado el efecto de la inflación, se aprecia un crecimiento o decrecimento real del ...% con respecto al mismo mes del año 2021
# La variacion de los promedios anuales se saca directo del grafico

# Filmina 13
sal_med_letra <- bases_3_provletra[[55]][fecha == ultimo_mes] 
setorder(sal_med_letra, -w_median)
sal_med_letra <- sal_med_letra[1:10,] # 10 sectores de mayor salario mededio
# Esta filmina es un tanto para el caso, tratar de dejar el segundo bullet siempre igual y adaptar el primero segun cambien o no los sectores

# Filmina 14
sal_med_clae <- bases_3_provclae2[[55]][fecha == ultimo_mes] 
sal_med_clae <- sal_med_clae %>% diccionario_sectores(agregacion_deseada = c('clae2', 'letra'))
setorder(sal_med_clae, -w_median)
sal_med_clae <- sal_med_clae[1:10,] # 10 principales sectores
# Al considerar el salario bruto mediano, aparece en primer lugar .... (con un salario de .... pesos).
# El segundo bullet es demasiado ad hoc, va a haber q cambiarlo

# Filminas 15 y 16. Tambien son demasiado ad-hoc... Describirlas en base a lo que se observa en el grafico



  