# Cargado de datos y librerias --------------------------------------------


options(scipen = 999)
set.seed(777)
pkg <- c("survey", "tidyverse", "ggplot2", "tseries", "lmtest", "car","glmnet")
for (i in pkg) {
  if(!requireNamespace(i, quietly = T)){
    install.packages(i)
  }
}
invisible(lapply(pkg, character.only = T, library))

# Estratos con un solo conglomerado: se ajusta la varianza en vez de dar error
options(survey.lonely.psu = "adjust")

vars <- c("id", "c6_3", "c7", "total_trabajadores", "cine_eme_red",
          "informalidad", "c1_b", "sexo", "tramo_etario", "region",
          "antiguedad_laboral", "h1_3", "ganancia_final", "estrato", "conglomerado", "factor_eme",
          "num_micro")

url_github1 <- "https://raw.githubusercontent.com/GGavilan07/Fila-A/main/03_Datos_EME8/EME8_base_full_original.csv"
url_github2 <- "https://raw.githubusercontent.com/GGavilan07/Fila-A/main/03_Datos_EME8/ids_particiones_pliegues.csv"
base <- read.csv(url_github1, fileEncoding = "UTF-8", check.names = F)[,vars]
ids <- read.csv(url_github2, fileEncoding = "UTF-8")


datos <- merge(base, ids, by="id", all=FALSE)
factores <- c("sexo", "cine_eme_red", "informalidad", "tramo_etario", "region", "antiguedad_laboral", "h1_3", "c1_b")
datos[factores] <- lapply(datos[factores], as.factor)


# Verificaciones de integridad
stopifnot(nrow(base)==7170, nrow(datos)==6747, !anyDuplicated(datos$id))

# Particiones
train <- subset(datos, particion == "entrenamiento")
test  <- subset(datos, particion == "prueba")

# Verificación de particiones
stopifnot(nrow(train)==5360, nrow(test)==1387)
print(table(train$fold))
stopifnot(setequal(unique(as.integer(train$fold)), 1:5))

crear_diseno <- function(d) {
  svydesign(ids = ~conglomerado,
            strata = ~estrato,
            weights = ~factor_eme,
            data = d,
            nest = TRUE)
}

# Métricas en CLP: sin ponderar y ponderadas por factor_eme
metricas <- function(y, p, w) {
  e <- y - p
  c(MAE_pond  = weighted.mean(abs(e), w, na.rm = TRUE),
    RMSE_pond = sqrt(weighted.mean(e^2, w, na.rm = TRUE)),
    MAE       = mean(abs(e), na.rm = TRUE),
    RMSE      = sqrt(mean(e^2, na.rm = TRUE)))
}

# Inversa de asinh con escala s = 100000
inversa_asinh <- function(pred_asinh, s = 100000) {
  return(s * sinh(pred_asinh))
}


# Ejercicio 1.A -------------------------------------------------------------

cat("Total de registros", nrow(datos),"\n")
cat("Cantidad de indentificadores únicos:", length(unique(datos$id)), "\n")
cat("¿Existen IDs duplicados?:", anyDuplicated(datos$id) > 0, "\n")

str(datos) #Tipo de variables

cat("Valores faltantes por variable:\n")
colSums(is.na(datos))

summary(datos)
#Para antiguedad laboral, el valor 7 significa no respuesta
#Para h1_3, el valor de 99 coorresponde a no respuesta (no hay en la muestra)
#Para cine_eme_red, el nivel 1 agrupa "Sin educación formal" con "Nivel ignorado" 
#Las variables cuantitativas usadas (c6_3, c7, total_trabajadores, ganancia_final) no tienen códigos 88/99 según el diccionario
#No hay valores faltantes, por lo que no se realizó imputación



# Ejercicio 1.B -----------------------------------------------------------

cat("\nMicroemprendedores (num_micro == 1) en la base completa:", sum(base$num_micro == 1, na.rm = TRUE),
    "de", nrow(base), "registros\n")

cat("\nDivisión de la muestra suministrada:\n")
table(datos$particion)
#5360 observaciones de entrenamiento y 1387 de prueba

ganancias_nulas <- sum(datos$ganancia_final == 0, na.rm = TRUE)
ganancias_neg <- sum(datos$ganancia_final < 0, na.rm = TRUE)
total_casos <- sum(!is.na(datos$ganancia_final))

cat("Ganancias nulas ($0):", ganancias_nulas,
    "(", round((ganancias_nulas/total_casos)*100, 2), "% )\n")
cat("Ganancias negativas (< $0):", ganancias_neg,
    "(", round((ganancias_neg/total_casos)*100, 2), "% )\n")




# Ejercicio 1.C -----------------------------------------------------------

#Variables que NO deben usarse como predictoras por fuga de información:

#ganancia_final_anual: se construye directamente como ganancia_final x c7 (meses de funcionamiento),
#es decir, contiene la respuesta de forma exacta.


#ganancia_estimada (ingreso_mensual - gasto_mensual): es otra medida de la misma ganancia
#ingreso_mensual y gasto_mensual son sus componentes y también la reconstruyen.

#tramos_ganancias: se construye discretizando ganancia_final (la respuesta).



# Ejercicio 2.A -------------------------------------------------------------

diseño_train <- crear_diseno(train)

media <- svymean(~ganancia_final, diseño_train)
cat("Media de la ganancia final:", coef(media), "| EE:", SE(media), "\n")

desv <- sqrt(as.numeric(svyvar(~ganancia_final, diseño_train)))
cat("Desviación estandar de ganancia final:", desv, "\n")

minimo <- min(train$ganancia_final, na.rm = TRUE)
maximo <- max(train$ganancia_final, na.rm = TRUE)
amplitud_rango <- maximo - minimo
cat("Rango de ganancia final:", amplitud_rango, "(min =", minimo, ", max =", maximo, ")\n")

percentiles <- svyquantile(~ganancia_final, diseño_train,
                           quantiles = c(0.10, 0.25, 0.50, 0.75, 0.90),
                           na.rm = TRUE)

q_val <- tryCatch(as.numeric(coef(percentiles)),
                  error = function(e) as.numeric(percentiles$ganancia_final[,1]))
cat("Mediana de la ganancia final:", q_val[3], "\n")
iqr <- q_val[4] - q_val[2]
cat("Rango intercuartil en ganancias:", iqr, "\n")

cv <- (desv / as.numeric(coef(media))) * 100
cat("Coeficiente de variación en ganancias:", cv, "\n")

# Referencia sin ponderar
summary(train$ganancia_final)

media_sexo <- svyby(~ganancia_final, ~sexo, diseño_train, svymean)
media_sexo


# Ejercicio 2.B -----------------------------------------------------------

grafico_horas_pond <- ggplot(train, aes(x = c6_3, y = ganancia_final, size = factor_eme)) +
  geom_point(alpha = 0.4, color = "steelblue", na.rm = TRUE) +
  scale_size_continuous(guide = "none") +
  labs(title = "Ganancia Final vs Horas de Trabajo Semanales (tamaño = factor de expansión)",
       x = "Horas de Trabajo Semanales",
       y = "Ganancia Final ($)") +
  theme_minimal()

print(grafico_horas_pond)

#Revisar visualmente observaciones extremas (alta pérdida / alta ganancia) 
#(ver puntos que se alejan del resto según horas semanales)


# Los gráficos son descriptivos; los cuantiles ponderados por grupo se calculan con svyby/svyquantile (ver 2.C).
grafico_tamano_pond <- ggplot(train, aes(x = as.factor(total_trabajadores),
                                         y = ganancia_final,
                                         weight = factor_eme)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", na.rm = TRUE) +
  labs(title = "Ganancia Final según Tamaño del Negocio",
       x = "Total de Trabajadores",
       y = "Ganancia Final ($)") +
  theme_minimal()

print(grafico_tamano_pond)
#Estudiar atipicos según número de trabajadores



# Ejercicio 2.C -----------------------------------------------------------

grafico_informalidad <- ggplot(train, aes(x = as.factor(informalidad),
                                          y = ganancia_final,
                                          weight = factor_eme)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen", na.rm = TRUE) +
  scale_x_discrete(labels = c("0" = "Formal", "1" = "Informal")) +
  labs(title = "Distribución de Ganancia Final según Informalidad",
       x = "Condición de Informalidad",
       y = "Ganancia Final ($)") +
  theme_minimal()

print(grafico_informalidad)


grafico_sexo <- ggplot(train, aes(x = as.factor(sexo),
                                  y = ganancia_final,
                                  weight = factor_eme)) +
  geom_boxplot(fill = "lightblue", color = "steelblue", na.rm = TRUE) +
  scale_x_discrete(labels = c("1" = "Hombre", "2" = "Mujer")) +
  labs(title = "Distribución de Ganancia Final según Sexo",
       x = "Sexo del Microemprendedor",
       y = "Ganancia Final ($)") +
  theme_minimal()

print(grafico_sexo)


grafico_tecnologia <- ggplot(train, aes(x = as.factor(h1_3),
                                        y = ganancia_final,
                                        weight = factor_eme)) +
  geom_boxplot(fill = "khaki1", color = "darkgoldenrod4", na.rm = TRUE) +
  scale_x_discrete(labels = c("1" = "Usa maquinaria especializada",
                              "2" = "No usa")) +
  labs(title = "Distribución de Ganancia Final según Uso de Maquinaria especializada",
       x = "Uso de maquinaria específica (h1_3)",
       y = "Ganancia Final ($)") +
  theme_minimal()

print(grafico_tecnologia)


grafico_educacion <- ggplot(train, aes(x = as.factor(cine_eme_red),
                                       y = ganancia_final,
                                       weight = factor_eme)) +
  geom_boxplot(fill = "plum", color = "purple4", na.rm = TRUE) +
  scale_x_discrete(labels = c("1" = "Sin Ed. formal / Ignorado",
                              "2" = "Básica",
                              "3" = "Media",
                              "4" = "Sup. Técnica / IP",
                              "5" = "Sup. Universitaria",
                              "6" = "Postítulo / Magíster / Dr.")) +
  labs(title = "Distribución de Ganancia Final según Nivel Educacional",
       x = "Nivel Educacional del Emprendedor (CINE 2011)",
       y = "Ganancia Final ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

print(grafico_educacion)


# Ejercicio 3.A -----------------------------------------------------------

modelo_survey <- svyglm(ganancia_final ~ c6_3 + c7 + total_trabajadores + cine_eme_red +
                          informalidad + c1_b + sexo + tramo_etario + region +
                          antiguedad_laboral + h1_3, design = diseño_train)

summary(modelo_survey)

#Al incluir nuestras variables categoricas (factores) al modelo (como sexo, cine_eme_red, c1_b o region),
#se covierte cada nivel en una variable dummy y omite la primera categoria para evitar multicolinealidad
#Esta categoria omitida corresponde a la categoria de referencia base, que queda absorbida por el intercepto
#del modelo. 

#total_trabajadores: manteniendo los demás predictores constantes, por cada trabajador adicional se espera
#que la ganancia mensual cambie en promedio en $718.132 CLP.

#sexo2: manteniendo los demás predictores constantes, un microemprendimiento liderado por una mujer
#tiene en promedio -258.461 CLP de ganancia mensual respecto a uno liderado por un hombre (categoría de referencia).



# Ejercicio 3.B -----------------------------------------------------------

#Usamos el modelo lineal lm con pesos, ya que por limitancias de la libreria para analisis de supuestos,
#no podemos usar el modelo usado con survey. Los coeficientes de svyglm (gaussiano) y lm ponderado
#coinciden, solamente cambian los errores estandar.

modelo_inicial_lm <- lm(ganancia_final ~ c6_3 + c7 + total_trabajadores + cine_eme_red +
                          informalidad + c1_b + sexo + tramo_etario + region +
                          antiguedad_laboral + h1_3, weights = factor_eme, data = train)

summary(modelo_inicial_lm)


par(mfrow = c(1, 2))
plot(modelo_inicial_lm, which = 1, main = "Residuos vs Ajustados")
plot(modelo_inicial_lm, which = 2, main = "Normal Q-Q")
par(mfrow = c(1, 1))

# Prueba para Heterocedasticidad
prueba_bp <- bptest(modelo_inicial_lm)
print(prueba_bp)

#H0=Homocedasticidad vs H1=Heterocedasticidad

#El grafico de residuos vs ajustados permite evaluar si la dispersión cambia con los valores ajustados
#(patrón en embudo = heterocedasticidad). Si el p valor es menor a 0.05 se rechaza H0, por lo que existe
#suficiente evidencia estadistica para establecer que no existe homocedasticidad.

# Prueba de Jarque-Bera para Normalidad
prueba_jb <- jarque.bera.test(weighted.residuals(modelo_inicial_lm))
print(prueba_jb)

#H0=Los residuos se distribuyen normal vs H1=Los residuos no se distribuyen normal

#El QQ-Plot muestra si hay desviaciones respecto de la recta en los extremos; una cola derecha elevada
#indica outliers con altas ganancias. Si el p valor es menor al 0.05 existe suficiente evidencia estadistica
#para establecer que los residuos no siguen una distribucion normal.


# Ejercicio 3.C -----------------------------------------------------------

vif_modelo <- vif(modelo_inicial_lm)
print(vif_modelo)
#Valores menores a ~2.24 (raíz de 5) indican que no hay multicolinealidad severa, si todos cumplen, no es necesario excluir variables por redundancia.

prueba_reset <- resettest(modelo_inicial_lm, power = 2:3, type = "fitted")
print(prueba_reset)

#H0=El modelo esta bien especificado (forma funcional correcta) vs H1=Mala especificación de la forma funcional
#Un p valor menor al 0.05 indica evidencia de mala especificación de la media condicional. RESET no identifica
#la causa (variable omitida, no linealidad, etc.).

#Implicancias para la interpretacion
#El modelo es lineal en los parámetros, pero si la forma funcional de la media está mal especificada, los
#coeficientes estiman una aproximación lineal que puede diferir del efecto marginal real; la interpretación
#ceteris paribus se vuelve poco confiable (sesgo por mala especificación).

#Implicancias para la prediccion
#Al no capturar la curvatura de los datos, el modelo puede subajustar, prediciendo por debajo del valor real
#en algunos tramos y por encima en otros, lo que sugiere transformaciones de variables.


# Ejercicio 4.A -----------------------------------------------------------

#La restricción para log(y): y es la ganancia final mensual de los microemprendimientos (puede tomar valores negativos, 0 o positivos).
#Aplicar log restringe el dominio a argumentos estrictamente positivos, por lo que habría que excluir los
#microemprendimientos con ganancia negativa o $0, perdiendo información del negocio.

#Esta restricción también ocurre para la transformación Box-Cox clásica, definida solo para Y>0, debiendo limitar nuestras
#observaciones.

#La transformación Yeo-Johnson no pierde observaciones: está definida para Y positiva , negativas o nulas,
#mediante una definición por tramos según el signo de Y y el valor de lambda.

#La transformación asinh(Y/s) también está definida en todos los reales; es aproximadamente lineal cerca de 0 y
#logarítmica para valores grandes en magnitud; la escala s controla dónde ocurre la transición y debe justificarse.


# Ejercicio 4.B -----------------------------------------------------------

#Usamos s=100.000 (escala de transicion); es una constante fija, no estimada de los datos (no hay fuga)
train$ganancia_asinh<-asinh(train$ganancia_final / 100000)

#Volvemos a definir el diseño muestral, puesto que agregamos otra variable a la base de datos
diseño_asinh <- crear_diseno(train)


mod_asinh_survey<-svyglm(ganancia_asinh ~ c6_3 + c7 + total_trabajadores + cine_eme_red +
                           informalidad + c1_b + sexo + tramo_etario + region +
                           antiguedad_laboral + h1_3, design = diseño_asinh)

summary(mod_asinh_survey)

mod_asinh_lm<-lm(ganancia_asinh ~ c6_3 + c7 + total_trabajadores + cine_eme_red +
                   informalidad + c1_b + sexo + tramo_etario + region +
                   antiguedad_laboral + h1_3, data=train,weights=factor_eme)

summary(mod_asinh_lm)


#Modelo Yeo-Johnson
#Por limitancias en los paquetes de car, no podemos usar el modelo mediante survey,
#usamos el modelo lm clasico pero con los pesos (mismos coeficientes, cambian solo los errores estandar).

pt_yj <- powerTransform(modelo_inicial_lm, family = "yjPower")
summary(pt_yj)   # incluye intervalo de confianza de lambda
lambda_yj <- pt_yj$lambda

# Verificación: Yeo-Johnson no es invariante a la escala de Y; se contrasta con Y en unidades de 100.000
pt_yj_esc <- powerTransform(update(modelo_inicial_lm, I(ganancia_final/100000) ~ .), family = "yjPower")
cat("lambda YJ (Y en CLP):", lambda_yj, "| lambda YJ (Y en 100.000 CLP):", pt_yj_esc$lambda, "\n")

train$ganancia_yj <- yjPower(train$ganancia_final, lambda_yj)

#Volvemos a definir el diseño muestral, puesto que creamos otra variable en la base de datos
diseño_yeo <- crear_diseno(train)

mod_yeo_survey<-svyglm(ganancia_yj ~ c6_3 + c7 + total_trabajadores + cine_eme_red +
                         informalidad + c1_b + sexo + tramo_etario + region +
                         antiguedad_laboral + h1_3, design = diseño_yeo)
summary(mod_yeo_survey)

mod_yeo_lm<-lm(ganancia_yj ~ c6_3 + c7 + total_trabajadores + cine_eme_red +
                 informalidad + c1_b + sexo + tramo_etario + region +
                 antiguedad_laboral + h1_3, data=train, weights=factor_eme)

summary(mod_yeo_lm)

#Comportamiento de supuestos modelos con transformacion

#Modelo asinh
bptest(mod_asinh_lm)
jarque.bera.test(weighted.residuals(mod_asinh_lm))


#Modelo Yeo-Johnson
bptest(mod_yeo_lm)
jarque.bera.test(weighted.residuals(mod_yeo_lm))

#Modelo inicial
bptest(modelo_inicial_lm)
jarque.bera.test(weighted.residuals(modelo_inicial_lm))

# Tabla resumen de estadísticos de diagnóstico
tabla_diag_4b <- data.frame(
  Modelo = c("Inicial", "asinh", "Yeo-Johnson"),
  BP_stat = c(bptest(modelo_inicial_lm)$statistic, bptest(mod_asinh_lm)$statistic, bptest(mod_yeo_lm)$statistic),
  BP_p    = c(bptest(modelo_inicial_lm)$p.value, bptest(mod_asinh_lm)$p.value, bptest(mod_yeo_lm)$p.value),
  JB_stat = c(jarque.bera.test(weighted.residuals(modelo_inicial_lm))$statistic,
              jarque.bera.test(weighted.residuals(mod_asinh_lm))$statistic,
              jarque.bera.test(weighted.residuals(mod_yeo_lm))$statistic),
  JB_p    = c(jarque.bera.test(weighted.residuals(modelo_inicial_lm))$p.value,
              jarque.bera.test(weighted.residuals(mod_asinh_lm))$p.value,
              jarque.bera.test(weighted.residuals(mod_yeo_lm))$p.value)
)
tabla_diag_4b

#Al evaluar las transformaciones mediante Jarque-bera y breusch-pagan, tenemos que para todas
#las transformaciones se sigue rechazando nuestra hipotesis nula, con p valores infimos lo cual es
#esperable dado lo que estamos midiendo, lo que llama la atencion es como nuestra transformacion ASINH
#demostro ser mejor respecto a los otros, dado que nuestro estadistico Chi-cuadrado de asimetria se contrajo
#mas de un 99%, comprimiendo los outliers y reduciendo la asimetria.

#En Yeo-johnson el estadistico de BREUS-PAGAN quedo practicamente identico al modelo original, el algoritmo de
#maxima verosimilitud cuando itera para buscar el lambda optimo, este convergio a LAMBDA=1, donde si lambda=1,
#la transformacion de yeo-johnson actua como una funcion lineal simple (dejando la misma forma original).

lambda_yj


par(mfrow = c(3, 2),
    mar = c(4.5, 5.5, 2.5, 1),
    cex.main = 1,
    cex.axis = 0.85,
    cex.lab = 0.9)

#Modelo Inicial
plot(modelo_inicial_lm, which = 1, main = "Inicial: Residuos vs Ajustados", caption = "")
plot(modelo_inicial_lm, which = 2, main = "Inicial: Normal Q-Q", caption = "")

#Modelo Asinh
plot(mod_asinh_lm, which = 1, main = "Asinh: Residuos vs Ajustados", caption = "")
plot(mod_asinh_lm, which = 2, main = "Asinh: Normal Q-Q", caption = "")

#Modelo Yeo-Johnson
plot(mod_yeo_lm, which = 1, main = "Yeo-Johnson: Residuos vs Ajustados", caption = "")
plot(mod_yeo_lm, which = 2, main = "Yeo-Johnson: Normal Q-Q", caption = "")
par(mfrow = c(1, 1), cex.main = 1, cex.axis = 1, cex.lab = 1)


# Ejercicio 4.C -----------------------------------------------------------

inv_yj <- function(z, l) {
  pos <- if (abs(l) < 1e-8) exp(z) - 1 else (l * z + 1)^(1 / l) - 1
  neg <- if (abs(l - 2) < 1e-8) 1 - exp(-z) else 1 - (1 - (2 - l) * z)^(1 / (2 - l))
  ifelse(z >= 0, pos, neg)
}


metodos_4c <- c("OLS_original_CLP", "asinh_directa", "asinh_smearing", "YeoJohnson")
cv4c <- array(NA_real_, dim = c(5, 4, 4),
              dimnames = list(NULL, metodos_4c, c("MAE_pond", "RMSE_pond", "MAE", "RMSE")))

f_base  <- ganancia_final ~ c6_3 + c7 + total_trabajadores + cine_eme_red +
  informalidad + c1_b + sexo + tramo_etario + region + antiguedad_laboral + h1_3
f_asinh <- ganancia_asinh ~ c6_3 + c7 + total_trabajadores + cine_eme_red +
  informalidad + c1_b + sexo + tramo_etario + region + antiguedad_laboral + h1_3

#Bucle de Validación Cruzada (5-Folds)

for (k in 1:5) {
  train_cv <- subset(train, fold != k)
  test_cv  <- subset(train, fold == k)
  
  # (1) OLS original
  m1 <- lm(f_base, data = train_cv, weights = factor_eme)
  p1 <- predict(m1, newdata = test_cv)
  
  # (2) asinh, inversión directa
  mod_cv <- lm(f_asinh, data = train_cv, weights = factor_eme)
  pred_asinh_cv <- predict(mod_cv, newdata = test_cv)
  pred_clp_cv <- inversa_asinh(pred_asinh_cv, s = 100000)
  
  u <- residuals(mod_cv)
  w_tr <- train_cv$factor_eme
  pred_media_cv <- 100000 * (sinh(pred_asinh_cv) * weighted.mean(cosh(u), w_tr) +
                               cosh(pred_asinh_cv) * weighted.mean(sinh(u), w_tr))
  
  lam_k <- tryCatch(powerTransform(m1, family = "yjPower")$lambda, error = function(e) 1)
  train_cv$ganancia_yj_cv <- yjPower(train_cv$ganancia_final, lam_k)
  m4 <- lm(update(f_base, ganancia_yj_cv ~ .), data = train_cv, weights = factor_eme)
  p4 <- inv_yj(predict(m4, newdata = test_cv), lam_k)
  
  y_k <- test_cv$ganancia_final; w_k <- test_cv$factor_eme
  cv4c[k, "OLS_original_CLP", ] <- metricas(y_k, p1, w_k)
  cv4c[k, "asinh_directa", ]    <- metricas(y_k, pred_clp_cv, w_k)
  cv4c[k, "asinh_smearing", ]   <- metricas(y_k, pred_media_cv, w_k)
  cv4c[k, "YeoJohnson", ]       <- metricas(y_k, p4, w_k)
}

cv_rmse <- cv4c[, "asinh_directa", "RMSE_pond"]
cv_mae  <- cv4c[, "asinh_directa", "MAE_pond"]
cv_rmse_u <- cv4c[, "asinh_directa", "RMSE"]
cv_mae_u  <- cv4c[, "asinh_directa", "MAE"]

cat("\n--- Resultados Validación Cruzada 5-Folds (Escala CLP), asinh inversión directa ---\n")
cat("RMSE por pliegue: ", paste(round(cv_rmse, 0), collapse = " | "), "\n")
cat("RMSE Promedio CV: ", round(mean(cv_rmse), 0), "CLP\n\n")

cat("MAE por pliegue:  ", paste(round(cv_mae, 0), collapse = " | "), "\n")
cat("MAE Promedio CV:  ", round(mean(cv_mae), 0), "CLP\n")

tabla_4c <- round(apply(cv4c, c(2, 3), mean), 0)
print(tabla_4c)


#Contraste entre Inversión Directa vs. Estimación de la Media Condicional:

#Inversión Directa (Método utilizado): Al aplicar la función algebraica inversa 
#(el seno hiperbólico multiplicador c * sinh(Z)) sobre las predicciones del modelo lineal, 
#estamos estimando matemáticamente la *mediana condicional* de las ganancias, no su promedio.
#Esto ocurre porque la mediana es invariante ante transformaciones monótonas crecientes.


#2.Estimación de la Media Condicional :De acuerdo con la Desigualdad de Jensen, 
#la esperanza de una función no lineal no equivale a la función aplicada sobre la esperanza 
#Dado que la función sinh es convexa para valores positivos, 
#la inversión directa subestima sistemáticamente el valor promedio real de la ganancia en pesos.
#Para recuperar la media condicional insesgada, se requeriría aplicar un factor de corrección 
#(como el estimador Smearing de Duan sobre los residuos). Sin embargo, la decisión de utilizar 
#la inversión directa es coherente con el uso del MAE como métrica principal, ya que el 
#Error Absoluto Medio penaliza asimétricamente y favorece estimaciones de tipo mediana.


# Ejercicio 5.A -----------------------------------------------------------

train$log_horas <- log(1 + train$c6_3)
train$log_trabajadores <- log(1 + train$total_trabajadores)

diseño_transf_x <- crear_diseno(train)

mod_asinh_logX <- svyglm(ganancia_asinh ~ log_horas + c7 + log_trabajadores +
                           cine_eme_red + informalidad + c1_b + sexo +
                           tramo_etario + region + antiguedad_laboral + h1_3,
                         design = diseño_transf_x)

summary(mod_asinh_logX)
#Usamos la transormacion de log(1+x), puesto que tanto total_trabajadores como horas, cuentas con
#observaciones con valor 0, para evitar problemas, se le suma el 1 para no perder estas observaciones.


#B_horas=0.454
#B_trabajadores=0.62

#Si las horas de trabajo semanales incrementaran un 20%, manteniendo el resto de coeficientes constantes,
#se espera que el modelo estime un cambio aditivo en la variable de respuesta de 0.454*log(1.20), lo que equivale
#a un incremento estimado de aproximadamente 0.0827 unidades.


#Si la cantidad total de trabajadores aumenta unn 10%, manteniendo el resto de coeficientes constantes,
#se espera que el modelo estime un cambio aditivo en la variable de respuesta de 0.62*log(1.10), lo que equivale
#a un incremento estimado de aproximadamente 0.0591 unidades.

# Ejercicio 5.B -----------------------------------------------------------

diseño_transf_x <- crear_diseno(train)
mod_asinh_pol <- svyglm(ganancia_asinh ~ log_horas + c7 + I(c7^2) + log_trabajadores +
                          cine_eme_red + informalidad + c1_b + sexo +
                          tramo_etario + region + antiguedad_laboral + h1_3,
                        design = diseño_transf_x)

summary(mod_asinh_pol)

#El término cuadrático no es significativo (p > 0.05) no hay evidencia suficiente de una relación curva
#(no rechazar no demuestra que no exista). Por principio de parsimonia se conserva c7 en su forma lineal



# Ejercicio 5.C -----------------------------------------------------------

metricas_nombres <- c("MAE_pond", "RMSE_pond", "MAE", "RMSE")
cv5a <- matrix(NA_real_, 5, 4, dimnames = list(NULL, metricas_nombres))

for (k in 1:5) {
  train_cv <- subset(train, fold != k)
  test_cv  <- subset(train, fold == k)
  
  mod_cv <- lm(ganancia_asinh ~ log_horas + c7 + log_trabajadores +
                 cine_eme_red + informalidad + c1_b + sexo +
                 tramo_etario + region + antiguedad_laboral + h1_3,
               data = train_cv, weights = factor_eme)
  
  pred_asinh_cv <- predict(mod_cv, newdata = test_cv)
  pred_clp_cv <- inversa_asinh(pred_asinh_cv)
  
  cv5a[k, ] <- metricas(test_cv$ganancia_final, pred_clp_cv, test_cv$factor_eme)
}

cv_rmse_5a <- cv5a[, "RMSE_pond"]
cv_mae_5a  <- cv5a[, "MAE_pond"]

cat("--- Modelo 5.A (Transformado) ---\n")
cat("RMSE Promedio CV: ", round(mean(cv_rmse_5a), 0), "CLP\n")
cat("MAE Promedio CV:  ", round(mean(cv_mae_5a), 0), "CLP\n")

cat("--- Modelo 4.C (sin transformar predictores) ---\n")
mean(cv_mae)
mean(cv_rmse)



#El modelo con transformaciones en nuestras variables predictoras, resulto con un MAE promedio menor,
#Pasando de 578.513 CLP a 572.187 CLP, por lo que el modelo mediante predictores con transformaciones logaritmicas
#es mas precisa para predecir nuestra ganancia mensual final.


mod_5a_lm <- lm(ganancia_asinh ~ log_horas + c7 + log_trabajadores +
                  cine_eme_red + informalidad + c1_b + sexo +
                  tramo_etario + region + antiguedad_laboral + h1_3,
                data = train, weights = factor_eme)

bptest(mod_5a_lm)
jarque.bera.test(weighted.residuals(mod_5a_lm))
resettest(mod_5a_lm, power = 2:3, type = "fitted")

#Seguimos contando con pvalores < 0.05 de significacncia para supuestos de normalidad y homocedasticidad
#Para test reset, existe suficiente evidencia para no rechazaxr H0, por lo que podemos establecer que tras las transformaciones
#el modelo está bien especificado (relación lineal entre los predictores y la ganancia)


# Ejercicio 6.A -----------------------------------------------------------

mod_diagnostico<-lm(ganancia_asinh ~ log_horas + c7 + log_trabajadores +
                      cine_eme_red + informalidad + c1_b + sexo +
                      tramo_etario + region + antiguedad_laboral + h1_3,
                    data=train,weights=factor_eme)

n<-nrow(train)
p<-length(coef(mod_diagnostico))

res_estud <- rstudent(mod_diagnostico)
leverage  <- hatvalues(mod_diagnostico)
cook_dist <- cooks.distance(mod_diagnostico)

# Las medidas deben corresponder 1 a 1 con las filas de train
stopifnot(length(cook_dist) == nrow(train))

umbral_res<-3
umbral_lev<-2*p/n
umbral_cook<-4/n

casos_res_extremos <- sum(abs(res_estud) > umbral_res, na.rm = TRUE)
casos_alto_lev     <- sum(leverage > umbral_lev, na.rm = TRUE)
casos_influyentes  <- sum(cook_dist > umbral_cook, na.rm = TRUE)

cat("1. Residuos Estudentizados (> 3):", casos_res_extremos, "casos\n")
cat("2. Alto Leverage (> 2p/n, umbral =", round(umbral_lev, 5), "):", casos_alto_lev, "casos\n")
cat("3. Distancia de Cook (> 4/n, umbral =", round(umbral_cook, 5), "):", casos_influyentes, "casos\n")



par(mfrow = c(1, 3), mar = c(4.5, 4.5, 3, 1), cex.main = 1.1)

plot(res_estud, pch = 20, col = "darkgray",
     main = "Residuos Estudentizados\n(Atípicos en Y)",
     xlab = "Índice", ylab = "Valor Estudentizado")
abline(h = c(-umbral_res, umbral_res), col = "red", lty = 2, lwd = 2)

plot(leverage, pch = 20, col = "darkgray",
     main = "Leverage\n(Atípicos en X)",
     xlab = "Índice", ylab = "Apalancamiento")
abline(h = umbral_lev, col = "red", lty = 2, lwd = 2)

plot(cook_dist, type = "h", col = "darkgray",
     main = "Distancia de Cook\n(Influencia Global)",
     xlab = "Índice", ylab = "Distancia")
abline(h = umbral_cook, col = "red", lty = 2, lwd = 2)

par(mfrow = c(1, 1))


#Residuos estudentizados identifica observaciones atipicas en la variable de respuesta Y (valor observado vs el modelo).
#Leverage identifica observaciones atipicas en las variables X (como muchas horas semanales trabajadas, etc), comparado con el resto.
#Distancia de Cook mide cuanto cambiarían los coeficientes/ajustes si se retirara esa observacion, identificando puntos influyentes.

#Primer grafico: negocios que ganan o pierden cantidades grandes respecto de lo que predice el modelo.
#Segundo grafico: negocios con combinaciones atipicas de características.
#Tercer grafico: negocios que combinan ambos aspectos y arrastran el ajuste (puntos influyentes).


# Ejercicio 6.B -----------------------------------------------------------

posiciones_top5 <- order(cook_dist, decreasing = TRUE)[1:5]
posiciones_top5

top_5_casos <- data.frame(
  id = train$id[posiciones_top5],
  distancia_cook = cook_dist[posiciones_top5],
  leverage = leverage[posiciones_top5],
  res_estudentizado = res_estud[posiciones_top5],
  ganancia = train$ganancia_final[posiciones_top5]
)

print(top_5_casos)

ids_top_5 <- top_5_casos$id
ids_top_5



# Ejerccicio 6.C ----------------------------------------------------------

train_sensibilidad <- subset(train, !(id %in% ids_top_5))
diseño_sensibilidad <- crear_diseno(train_sensibilidad)
mod_sensibilidad <- svyglm(ganancia_asinh ~ log_horas + c7 + log_trabajadores +
                             cine_eme_red + informalidad + c1_b + sexo +
                             tramo_etario + region + antiguedad_laboral + h1_3,
                           design = diseño_sensibilidad)

comparacion_betas <- data.frame(
  Beta_Original = coef(mod_asinh_logX),
  Beta_Sin_Outliers = coef(mod_sensibilidad)
)

comparacion_betas$Cambio_Absoluto <- comparacion_betas$Beta_Sin_Outliers - comparacion_betas$Beta_Original
comparacion_betas$Variacion_Porcentual <-
  (comparacion_betas$Cambio_Absoluto / abs(comparacion_betas$Beta_Original)) * 100

comparacion_betas$Cambio_en_EE <- comparacion_betas$Cambio_Absoluto / SE(mod_asinh_logX)

cat("\n--- Comparación de Coeficientes (Análisis de Sensibilidad) ---\n")
print(round(comparacion_betas, 4))

#Robustez estructural: Los predictores principales (log_horas, log_trabajadores, c7) 
#son muy estables (varían menos de un 5.5%), por lo que el corazón del modelo no está 
#sesgado por estos 5 casos extremos.
#el coeficiente de maquinaria (h1_3) y los tramos 
#altos de antigüedad laboral cuentan con (variaciones > 40%), demostrando que 
#su estimación depende casi exclusivamente de estos microemprendedores atípicos.


# Ejercicio 7.A -----------------------------------------------------------

mod_vacio <- lm(ganancia_asinh ~ 1, data = train, weights = factor_eme)
mod_asinh <- lm(ganancia_asinh ~ log_horas + c7 + log_trabajadores + cine_eme_red +
                  informalidad + c1_b + sexo + tramo_etario + region +
                  antiguedad_laboral + h1_3, data = train, weights = factor_eme)


mod_forward <- step(mod_vacio,
                    direction = "forward",
                    scope = list(lower = mod_vacio, upper = mod_asinh),
                    trace = 0)
mod_fwd_survey <- svyglm(formula(mod_forward), design = diseño_transf_x)
summary(mod_fwd_survey)
formula(mod_forward)   

mod_backward <- step(mod_asinh, direction = "backward", trace = 0)
mod_bwd_survey <- svyglm(formula(mod_backward), design = diseño_transf_x)
summary(mod_bwd_survey)
formula(mod_backward)

mod_stepwise <- step(mod_vacio, direction = "both", scope = list(lower = mod_vacio, upper = mod_asinh), trace = 0)
mod_stp_survey <- svyglm(formula(mod_stepwise), design = diseño_transf_x)
summary(mod_stp_survey)
formula(mod_stepwise)

mod_stepwise_bic <- step(mod_vacio, direction = "both", scope = list(lower = mod_vacio, upper = mod_asinh),
                         k = log(nrow(train)), trace = 0)

formula(mod_stepwise_bic)
formula(mod_stepwise)
formula(mod_backward)
formula(mod_forward)

# Ejercicio 7.B -----------------------------------------------------------


mod_full <- lm(ganancia_asinh ~ log_horas + c7 + log_trabajadores + cine_eme_red +
                 informalidad + c1_b + sexo + tramo_etario + region +
                 antiguedad_laboral + h1_3, data = train, weights = factor_eme)
sigma2_full <- summary(mod_full)$sigma^2
calcular_metricas_seleccion <- function(modelo_lm, sigma2_ref) {
  p <- length(coef(modelo_lm))
  sse <- sum(weights(modelo_lm) * residuals(modelo_lm)^2)
  aic_val <- AIC(modelo_lm)
  bic_val <- BIC(modelo_lm)
  n <- nobs(modelo_lm)
  cp_val <- (sse / sigma2_ref) - n + (2 * p)
  
  return(c(AIC = aic_val,
           BIC = bic_val,
           Cp_Mallows = cp_val,
           p = p,
           sigma2_ref = sigma2_ref))
}

tabla_7b <- rbind(
  Forward  = calcular_metricas_seleccion(mod_forward, sigma2_full),
  Backward = calcular_metricas_seleccion(mod_backward, sigma2_full),
  Stepwise = calcular_metricas_seleccion(mod_stepwise, sigma2_full),
  Stepwise_BIC = calcular_metricas_seleccion(mod_stepwise_bic, sigma2_full),
  Completo = calcular_metricas_seleccion(mod_full, sigma2_full)
)

tabla_7b <- as.data.frame(tabla_7b)
tabla_7b


# Ejercicio 7.C -----------------------------------------------------------


#Para ver las rutas de los modelos mediante búsqueda paso a paso
#basta con cambiar el argumento "trace" de la función step
#con trace = 0 ocultamos la ruta y nos muestra únicamente el modelo final.
#con trace = 1 nos muestra el paso a paso de la búsqueda
#forward

mod_forward <- step(mod_vacio,
                    direction = "forward",
                    scope = list(lower = mod_vacio, upper = mod_asinh),
                    trace = 1)

#backward
mod_backward <- step(mod_asinh, direction = "backward", trace = 1)
#stepwise
mod_stepwise <- step(mod_vacio, direction = "both", scope = list(lower = mod_vacio, upper = mod_asinh), trace = 1)
#modelos selccionados

formula(mod_forward)
formula(mod_backward)
formula(mod_stepwise)

#rutas de selección (describir con la salida de trace = 1):
#Forward parte del modelo vacío e incorpora, en cada paso, el bloque de covariables que más reduce el AIC,
#hasta que ninguna incorporación lo reduce.
#Backward parte del modelo completo y elimina, en cada paso, el bloque cuya salida más reduce el AIC.
#Stepwise bidireccional parte del vacío y en cada paso evalúa tanto agregar como quitar.
#Se compara si los tres convergen al mismo modelo y qué variable(s) quedan fuera (ver formula() de cada uno).

#Limitaciones bajo fallas de supuestos:
#AIC/BIC/Cp se derivan bajo errores normales y homocedásticos; con heterocedasticidad y colas pesadas (BP y JB rechazan)
#las penalizaciones son aproximadas. Las rutas dependen del punto de partida y del criterio (son búsquedas locales,
#no garantizan el mejor subconjunto global), los modelos finales ignoran la incertidumbre de la selección, y
#los pesos de expansión no son pesos de precisión, por lo que AIC/BIC del lm ponderado son solo referenciales.
#Por eso la decisión final se apoya en el error fuera de muestra (validación cruzada).


# Ejercicio 8 -----------------------------------------------------------


cv_sel <- matrix(NA_real_, 5, 4, dimnames = list(NULL, metricas_nombres))

for (k in 1:5) {
  train_cv <- subset(train, fold != k)
  test_cv  <- subset(train, fold == k)
  
  m0_cv <- lm(ganancia_asinh ~ 1, data = train_cv, weights = factor_eme)
  mf_cv <- lm(ganancia_asinh ~ log(1+c6_3) + c7 + log(1+total_trabajadores) + cine_eme_red +
                informalidad + c1_b + sexo + tramo_etario + region + antiguedad_laboral + h1_3,
              data = train_cv, weights = factor_eme)
  mod_cv <- step(m0_cv, direction = "both", scope = list(lower = m0_cv, upper = mf_cv), trace = 0)
  
  pred_asinh_cv <- predict(mod_cv, newdata = test_cv)
  
  pred_clp_cv <- inversa_asinh(pred_asinh_cv)
  
  cv_sel[k, ] <- metricas(test_cv$ganancia_final, pred_clp_cv, test_cv$factor_eme)
}

cv_rmse_sel <- cv_sel[, "RMSE_pond"]
cv_mae_sel  <- cv_sel[, "MAE_pond"]

cat("\n--- Resultados Validación Cruzada (selección stepwise repetida en cada pliegue) ---\n")
cat("RMSE por pliegue: ", paste(round(cv_rmse_sel, 0), collapse = " | "), "\n")
cat("RMSE Promedio CV: ", round(mean(cv_rmse_sel), 0), "CLP\n\n")

cat("MAE por pliegue:  ", paste(round(cv_mae_sel, 0), collapse = " | "), "\n")
cat("MAE Promedio CV:  ", round(mean(cv_mae_sel), 0), "CLP\n")

mean(cv_rmse_sel)
mean(cv_mae_sel)

mean(cv_rmse_5a)
mean(cv_mae_5a)


#- Pliegues: se usan los 5 pliegues predefinidos (columna fold), sin reasignar.
#- Selección de variables: step() se repite dentro del entrenamiento de cada pliegue.
#- Prueba: el conjunto de prueba no se usó en ninguna decisión; solo se evalúa al final (Ejercicio 10).

mod_final_sel <- mod_stepwise
summary(mod_final_sel)
#Finalmente, el modelo seleccionado puede ser cualquiera de los de backward,forward y stepwise, ya que convergen
#al mismo modelo



# Ejercicio 9.A -----------------------------------------------------------


formula_regresion <- ganancia_asinh ~ log(1+c6_3) + c7 + log(1+total_trabajadores) + cine_eme_red +
  informalidad + c1_b + sexo + tramo_etario +
  region + antiguedad_laboral + h1_3

X_train <- model.matrix(formula_regresion, data = train)[,-1]
y_train <- train$ganancia_asinh
w_train <- train$factor_eme

foldid <- as.integer(train$fold)

#ridge
cv_ridge <- cv.glmnet(X_train, y_train, weights = w_train, alpha = 0,
                      foldid = foldid, standardize = TRUE)
lambda_ridge <- cv_ridge$lambda.1se
#lasso
cv_lasso <- cv.glmnet(X_train, y_train, weights = w_train, alpha = 1,
                      foldid = foldid, standardize = TRUE)
lambda_lasso <- cv_lasso$lambda.1se

#elastic net
#para elastic net buscamos el alpha (mezcla L1/L2) que mejor se adapte a los datos, con los mismos pliegues
#para todos los alphas, de modo que la comparación sea justa
alphas <- c(0.1, 0.25, 0.5, 0.75, 0.9)
search_enet <- lapply(alphas, function(a) {
  cv.glmnet(X_train, y_train, weights = w_train, alpha = a, foldid = foldid, standardize = TRUE)
})

errores_enet <- sapply(search_enet, function(cv) min(cv$cvm))
best_alpha_idx <- which.min(errores_enet)
best_alpha <- alphas[best_alpha_idx]
cv_enet <- search_enet[[best_alpha_idx]]
lambda_enet <- cv_enet$lambda.1se

mod_ridge_final <- glmnet(X_train, y_train, weights = w_train, alpha = 0, lambda = lambda_ridge, standardize = TRUE)
mod_lasso_final <- glmnet(X_train, y_train, weights = w_train, alpha = 1, lambda = lambda_lasso, standardize = TRUE)
mod_enet_final  <- glmnet(X_train, y_train, weights = w_train, alpha = best_alpha, lambda = lambda_enet, standardize = TRUE)


# Ejercicio 9.B -----------------------------------------------------------

cat("Ridge:  alpha = 0   | lambda.1se =", lambda_ridge, "| lambda.min =", cv_ridge$lambda.min, "\n")
cat("LASSO:  alpha = 1   | lambda.1se =", lambda_lasso, "| lambda.min =", cv_lasso$lambda.min, "\n")
cat("ENet:   alpha =", best_alpha, "| lambda.1se =", lambda_enet, "| lambda.min =", cv_enet$lambda.min, "\n")
cat("Errores CV por alpha (ENet):", paste(alphas, round(errores_enet, 5), sep = "=", collapse = " | "), "\n")

#En general, se priorizó la parsimonia del modelo, al seleccionar lambda.1se se obtiene un modelo más penalizado,
#con mayor reducción de varianza y menor cantidad de coeficientes.
#Si bien lambda.min minimiza el error de VALIDACIÓN CRUZADA, es propenso a sobreajuste y a incluir covariables con coeficientes
#pequeños con tal de optimizar el mínimo; lambda.1se elige el modelo más simple dentro de 1 error estándar del mínimo.

# CV en CLP (5 pliegues predefinidos; lambda se elige DENTRO de cada pliegue, sin fuga)
cv_glmnet_clp <- function(alpha, s = 100000) {
  res <- sapply(1:5, function(k) {
    tr <- train[train$fold != k, ]
    va <- train[train$fold == k, ]
    Xtr <- model.matrix(formula_regresion, tr)[, -1]
    Xva <- model.matrix(formula_regresion, va)[, -1]
    m <- cv.glmnet(Xtr, tr$ganancia_asinh, weights = tr$factor_eme, alpha = alpha,
                   nfolds = 5, standardize = TRUE)
    pred <- s * sinh(as.numeric(predict(m, Xva, s = "lambda.1se")))
    metricas(va$ganancia_final, pred, va$factor_eme)
  })
  rowMeans(res)
}
cv_ridge_clp <- cv_glmnet_clp(0)
cv_lasso_clp <- cv_glmnet_clp(1)
cv_enet_clp  <- cv_glmnet_clp(best_alpha)
print(round(rbind(Ridge = cv_ridge_clp, LASSO = cv_lasso_clp, ElasticNet = cv_enet_clp), 0))


par(mfrow = c(1, 3), mar = c(4.5, 4.5, 3, 1))

plot(cv_ridge, main = "Ridge (alpha = 0)")

plot(cv_lasso, main = "LASSO (alpha = 1)")

plot(cv_enet, main = paste0("Elastic Net (alpha = ", best_alpha, ")"))

par(mfrow = c(1, 1))


# 9.c ---------------------------------------------------------------------

#estabilidad y selección (completar con la salida de coef() abajo y el conteo de coeficientes no nulos):
cat("Coeficientes no nulos (sin intercepto): Ridge =", sum(coef(mod_ridge_final)[-1] != 0),
    "| LASSO =", sum(coef(mod_lasso_final)[-1] != 0),
    "| ENet =", sum(coef(mod_enet_final)[-1] != 0), "de", ncol(X_train), "\n")
  #LASSO puede anular coeficientes (selección); Ridge contrae todos sin anularlos; Elastic Net es intermedio según alpha.
#Comparar la magnitud/signo de las covariables principales entre los tres modelos y su desempeño en CV (tabla anterior).

#distinción teórica:
#Es fundamental precisar que la eliminación o reducción de coeficientes por regularización L1/L2
#responde a la optimización de la capacidad predictiva fuera de muestra y no a una prueba
#de hipótesis estadística sobre la población. Los coeficientes penalizados están sesgados
#por diseño para maximizar el desempeño predictivo, por lo que no disponen de errores estándar
#ni de valores p válidos para contrastar H0: beta_j = 0.


coef(mod_ridge_final)
coef(mod_lasso_final)
coef(mod_enet_final)

# Ejercicio 10 ------------------------------------------------------------


test$ganancia_asinh <- asinh(test$ganancia_final / 100000)
test$log_horas <- log(1 + test$c6_3)
test$log_trabajadores <- log(1 + test$total_trabajadores)
X_test <- model.matrix(formula_regresion, data = test)[, -1]

u_tr <- residuals(mod_asinh_lm)
z_te <- predict(mod_asinh_lm, newdata = test)
p_smear <- 100000 * (sinh(z_te) * weighted.mean(cosh(u_tr), train$factor_eme) +
                       cosh(z_te) * weighted.mean(sinh(u_tr), train$factor_eme))

pred_test <- list(
  OLS_original      = predict(modelo_inicial_lm, newdata = test),
  OLS_asinh_directa = inversa_asinh(z_te),
  OLS_asinh_smearing = p_smear,
  OLS_YeoJohnson    = inv_yj(predict(mod_yeo_lm, newdata = test), lambda_yj),
  OLS_asinh_logX    = inversa_asinh(predict(mod_5a_lm, newdata = test)),
  Ridge      = inversa_asinh(as.numeric(predict(mod_ridge_final, X_test))),
  LASSO      = inversa_asinh(as.numeric(predict(mod_lasso_final, X_test))),
  ElasticNet = inversa_asinh(as.numeric(predict(mod_enet_final, X_test)))
)

tabla_test <- t(sapply(pred_test, function(p) metricas(test$ganancia_final, p, test$factor_eme)))

tabla_cv <- rbind(
  OLS_original       = tabla_4c["OLS_original_CLP", ],
  OLS_asinh_directa  = tabla_4c["asinh_directa", ],
  OLS_asinh_smearing = tabla_4c["asinh_smearing", ],
  OLS_YeoJohnson     = tabla_4c["YeoJohnson", ],
  OLS_asinh_logX     = colMeans(cv5a),
  Ridge      = cv_ridge_clp[metricas_nombres],
  LASSO      = cv_lasso_clp[metricas_nombres],
  ElasticNet = cv_enet_clp[metricas_nombres]
)

tabla_final <- data.frame(
  Modelo = rownames(tabla_cv),
  CV_MAE = tabla_cv[, "MAE"], CV_RMSE = tabla_cv[, "RMSE"],
  Test_MAE = tabla_test[rownames(tabla_cv), "MAE"], Test_RMSE = tabla_test[rownames(tabla_cv), "RMSE"],
  CV_MAE_pond = tabla_cv[, "MAE_pond"], CV_RMSE_pond = tabla_cv[, "RMSE_pond"],
  Test_MAE_pond = tabla_test[rownames(tabla_cv), "MAE_pond"], Test_RMSE_pond = tabla_test[rownames(tabla_cv), "RMSE_pond"],
  row.names = NULL
)
print(tabla_final, digits = 6)

#Modelo final OLS_asinh_logX 
#Elegimos este modelo porque logra el menor error absoluto medio poblacional 
#(Test_MAE_pond = $474.181 vs $657.930 del OLS original). 

# La combinación de la transformación asinh en Y (que maneja los valores en cero 
# y la asimetría de las ganancias) junto con log en las X continuas estabiliza 
# las relaciones elásticas. Aunque el OLS original da un RMSE_pond ligeramente 
# menor en test, esto se debe a que sobrepenaliza los atípicos extremos; para 
# proyectar ingresos en microempresas, el MAE ponderado refleja mucho mejor 
# el error típico real en la población.


#Limitaciones a comentar:heterocedasticidad y colas pesadas de la ganancia.
#la inversión directa de asinh predice la mediana, no la media


writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
file.show("sessionInfo.txt")


# EDA TRANSFORMACION ------------------------------------------------------

diseño_train <- crear_diseno(train)


media_asinh <- svymean(~ganancia_asinh, diseño_train)
cat("Media de ganancia (asinh):", coef(media_asinh), "| EE:", SE(media_asinh), "\n")

desv_asinh <- sqrt(as.numeric(svyvar(~ganancia_asinh, diseño_train)))
cat("Desviación estándar (asinh):", desv_asinh, "\n")

percentiles_asinh <- svyquantile(~ganancia_asinh, diseño_train,
                                 quantiles = c(0.10, 0.25, 0.50, 0.75, 0.90),
                                 na.rm = TRUE)

q_val_asinh <- tryCatch(as.numeric(coef(percentiles_asinh)),
                        error = function(e) as.numeric(percentiles_asinh$ganancia_asinh[,1]))

cat("Mediana (asinh):", q_val_asinh[3], "\n")
cat("IQR (asinh):", q_val_asinh[4] - q_val_asinh[2], "\n")

summary(train[, c("ganancia_asinh", "log_horas", "log_trabajadores")])

media_sexo_asinh <- svyby(~ganancia_asinh, ~sexo, diseño_train, svymean)
print(media_sexo_asinh)




grafico_horas_pond <- ggplot(train, aes(x = log_horas, y = ganancia_asinh)) +
  geom_point(aes(size = factor_eme), alpha = 0.3, color = "steelblue", na.rm = TRUE) +
  geom_smooth(aes(weight = factor_eme), method = "loess", color = "darkred", se = FALSE, na.rm = TRUE) +
  scale_size_continuous(guide = "none") +
  labs(title = "Ganancia (asinh) vs Log(Horas Semanales)",
       subtitle = "Línea roja: tendencia local loess ponderada",
       x = "log_horas",
       y = "ganancia_asinh") +
  theme_minimal()

print(grafico_horas_pond)

grafico_tamano_pond <- ggplot(train, aes(x = log_trabajadores, y = ganancia_asinh)) +
  geom_point(aes(size = factor_eme), alpha = 0.3, color = "darkblue", na.rm = TRUE) +
  geom_smooth(aes(weight = factor_eme), method = "lm", color = "darkred", se = FALSE, na.rm = TRUE) +
  scale_size_continuous(guide = "none") +
  labs(title = "Ganancia (asinh) vs Log(Total Trabajadores)",
       subtitle = "Línea roja: tendencia lineal ponderada (lm)",
       x = "log_trabajadores",
       y = "ganancia_asinh") +
  theme_minimal()

print(grafico_tamano_pond)

grafico_informalidad <- ggplot(train, aes(x = as.factor(informalidad), y = ganancia_asinh, weight = factor_eme)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen", na.rm = TRUE) +
  scale_x_discrete(labels = c("0" = "Formal", "1" = "Informal")) +
  labs(title = "Ganancia (asinh) según Informalidad",
       x = "Condición de Informalidad",
       y = "ganancia_asinh") +
  theme_minimal()

print(grafico_informalidad)

grafico_sexo <- ggplot(train, aes(x = as.factor(sexo), y = ganancia_asinh, weight = factor_eme)) +
  geom_boxplot(fill = "lightblue", color = "steelblue", na.rm = TRUE) +
  scale_x_discrete(labels = c("1" = "Hombre", "2" = "Mujer")) +
  labs(title = "Ganancia (asinh) según Sexo",
       x = "Sexo del Microemprendedor",
       y = "ganancia_asinh") +
  theme_minimal()

print(grafico_sexo)

grafico_tecnologia <- ggplot(train, aes(x = as.factor(h1_3), y = ganancia_asinh, weight = factor_eme)) +
  geom_boxplot(fill = "khaki1", color = "darkgoldenrod4", na.rm = TRUE) +
  scale_x_discrete(labels = c("1" = "Usa maquinaria", "2" = "No usa")) +
  labs(title = "Ganancia (asinh) según Uso de Maquinaria",
       x = "Uso de maquinaria (h1_3)",
       y = "ganancia_asinh") +
  theme_minimal()

print(grafico_tecnologia)

grafico_educacion <- ggplot(train, aes(x = as.factor(cine_eme_red), y = ganancia_asinh, weight = factor_eme)) +
  geom_boxplot(fill = "plum", color = "purple4", na.rm = TRUE) +
  scale_x_discrete(labels = c("1" = "Sin Ed. / Ignorado",
                              "2" = "Básica",
                              "3" = "Media",
                              "4" = "Sup. Técnica",
                              "5" = "Sup. Universitaria",
                              "6" = "Postgrado")) +
  labs(title = "Ganancia (asinh) según Nivel Educacional",
       x = "Nivel Educacional (CINE 2011)",
       y = "ganancia_asinh") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

print(grafico_educacion)


