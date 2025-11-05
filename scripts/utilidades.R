# Función de Pareto
# Dado un vector de números y un nivel de significancia de Pareto, calcula el valor del vector tal que la suma de los valores por encima de ese valor supera el total de la suma multiplicado por el nivel de significancia.

ParetoValue <- function(
        v, # vector numérico
        ParetoSignificancia # número en [0,1] con el grado de significancia de Pareto
){
    
    x <- sort(v,decreasing = TRUE)
    return(x[which(cumsum(x)>=ParetoSignificancia*sum(x))[1]])
}


#FUNCIÓN PARA DIBUJAR UNA PREDICCIÓN DINÁMICA CON USANDO plotly
GraficoDinamicoArima95CI <- function(
 data, # tssible con con los datos originales
 date, # string con el nombre se la variable temporal en data
 value, # string con el nombre se la variable numérica en data
 prediccion, # objeto con la predicción usando ARIMA u otro modelo
 TITLE # título del gráfico 
){
  
  fc <- prediccion %>%
  mutate(lower = NA) %>% 
  mutate(upper = NA) 

  for(k in 1:nrow(fc)){
    fc$lower[k] <- fc$.mean[k] - 1.96*unlist(fc[[value]][k])[2]
    fc$upper[k] <- fc$.mean[k] + 1.96*unlist(fc[[value]][k])[2]
  }
  
  data2 <- data 
  x <- data2[[date]][1]
  if(!(is.numeric(x) && !inherits(x, "Date") && !inherits(x, "POSIXt"))){
    data2[[date]] <- as.Date(data2[[date]])
    fc[[date]] <- as.Date(fc[[date]])
  }
  


p <- plot_ly() %>%
  add_lines(x=data2[[date]], y = data2[[value]], name = "Observación", line = list(color = "black")) %>%
  add_lines(x=fc[[date]], y = fc$.mean, name = "Predicción", line = list(color = "blue")) %>%
  add_ribbons(x = fc[[date]], ymin = fc$lower, ymax = fc$upper, 
              name = "95% CI", fillcolor = "rgba(0,0,150,0.2)", line = list(width = 0)) %>%
  layout(title = TITLE)

return(p) 
}
  

#FUNCIÓN PARA DIBUJAR UN MAPA COROPLÉTICO USANDO leaflet
MapaCoroplético <- function(
 geoj, # objeto gráfico geojson con los contornos de las regiones
 value, # vector de valores para colorear 
 region_labels, # vector de etiquetas para los desplegables
 legend_title # título de la leyenda 
){
pal <- colorQuantile("YlOrRd", value, n = 9)
p <-  geoj %>%
    leaflet() %>%  
    #setView(lng = 25, lat = 22, zoom = 2)  %>% 
    addPolygons(
      fillColor = ~pal(value), 
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions( 
        weight = 2,
        color = rgb(0.2,0.2,0.2),
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = region_labels 
    ) %>% 
    addLegend("bottomleft", 
      pal = pal, 
      values = value,
      title = legend_title,
      labFormat = function(type, cuts, p) {
        n = length(cuts) 
        x = (cuts[-n] + cuts[-1])/2
        x=prettyNum(round(x,
            digits=max(5-nchar(as.character(round(max(na.omit(value))))),0)), 
            big.mark = ","
        )
        as.character(x)
      },
      opacity = 1
    )
  return(p)
}



# CÁLCULO DE LA TRANSFORMACIÓN DE YEO–JOHNSON A PARTIR DE UN VECTOR Y UN VALOR DE lambda 
yeo.johnson <- function(y, lambda) {
  y_t <- numeric(length(y))
  
  # Para y >= 0
  pos_idx <- which(y >= 0)
  if (lambda == 0) {
    y_t[pos_idx] <- log(y[pos_idx] + 1)
  } else {
    y_t[pos_idx] <- ((y[pos_idx] + 1)^lambda - 1) / lambda
  }
  
  # Para y < 0
  neg_idx <- which(y < 0)
  if (lambda == 2) {
    y_t[neg_idx] <- -log(-y[neg_idx] + 1)
  } else {
    y_t[neg_idx] <- -(((-y[neg_idx] + 1)^(2 - lambda) - 1) / (2 - lambda))
  }
  
  return(y_t)
}


# ESTIMACIÓN DE lambda PARA LA TRANSFORMACIÓN DE YEO–JOHNSON DE UN VECTOR y
# OPTIMIZANDO  EL R2 DE LA REGRESIÓN LINEAL CON UN VECTOR x.  
optimize.yeojohnson.R2 <- function(x, y, lambda_range = c(-1, 1.9)) {
 
  # Función objetivo: R² negativo (porque optimize minimiza)
  r2_neg <- function(lambda) {
    y_t <- yeo.johnson(y, lambda)
    modelo <- lm(y_t ~ x)
    return(-summary(modelo)$r.squared)  # queremos maximizar R²
  }
  
  # Optimización de lambda
  opt <- optimize(r2_neg, interval = lambda_range)
  
  # Se retorna el valor óptimo de lambda 
  return(opt$minimum)
}


