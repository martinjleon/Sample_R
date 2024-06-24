#Codigo para realizar un analisis de datos de clientes.
#El objetivo es realizar una segmentacion de clientes utilizando
#tecnicas de agrupamiento, a partir de una base de datos publica.


#Librerias a utilizar
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(grid)
library(gridExtra)
library(shiny)
library(cluster)
library(factoextra)

#Limpiar entorno de trabajo
rm(list = ls())
dev.off()


#Defino directorio de trabajo y set
setwd("/Users/martinleon/Desktop/Trabajo/PORFOLIO")

datos <- read.csv("UCI_Credit_Card.csv", header = T)
attach(datos)

#Metadatos:

#ID: ID of each client
#LIMIT_BAL: Amount of given credit in NT dollars (includes individual and family/supplementary credit
#SEX: Gender (1=male, 2=female)
#EDUCATION: (1=graduate school, 2=university, 3=high school, 4=others, 5=unknown, 6=unknown)
#MARRIAGE: Marital status (1=married, 2=single, 3=others)
#AGE: Age in years
#PAY_0: Repayment status in September, 2005 (-1=pay duly, 1=payment delay for one month, 2=payment delay for two months, … 8=payment delay for eight months, 9=payment delay for nine months and above)
#PAY_2: Repayment status in August, 2005 (scale same as above)
#PAY_3: Repayment status in July, 2005 (scale same as above)
#PAY_4: Repayment status in June, 2005 (scale same as above)
#PAY_5: Repayment status in May, 2005 (scale same as above)
#PAY_6: Repayment status in April, 2005 (scale same as above)
#BILL_AMT1: Amount of bill statement in September, 2005 (NT dollar)
#BILL_AMT2: Amount of bill statement in August, 2005 (NT dollar)
#BILL_AMT3: Amount of bill statement in July, 2005 (NT dollar)
#BILL_AMT4: Amount of bill statement in June, 2005 (NT dollar)
#BILL_AMT5: Amount of bill statement in May, 2005 (NT dollar)
#BILL_AMT6: Amount of bill statement in April, 2005 (NT dollar)
#PAY_AMT1: Amount of previous payment in September, 2005 (NT dollar)
#PAY_AMT2: Amount of previous payment in August, 2005 (NT dollar)
#PAY_AMT3: Amount of previous payment in July, 2005 (NT dollar)
#PAY_AMT4: Amount of previous payment in June, 2005 (NT dollar)
#PAY_AMT5: Amount of previous payment in May, 2005 (NT dollar)
#PAY_AMT6: Amount of previous payment in April, 2005 (NT dollar)
#default.payment.next.month: Default payment (1=yes, 0=no)


#Análisis gráfico preliminar:---------------------------------------------------
grafico_preliminar <- ggplot(datos, aes(x = LIMIT_BAL, fill = "orange")) +
  geom_histogram() +
  theme_minimal() +
  labs(x = "Límite de crédito", y = "Conteo de clientes", title = "Histograma de clientes totales") +
  guides(fill = FALSE)


#Selección de los datos mas frecuentes en cuanto al límite de crédito:----------
tabla_frecuencias <- datos %>%
  count(LIMIT_BAL) %>%
  rename(Frecuencia = n) %>%
  arrange(desc(Frecuencia))

tabla_frec <- head(tabla_frecuencias)#El límite de crédito mas frecuente es $50000


#Subgrupo de datos
credito <- subset(datos, LIMIT_BAL == 50000)


#Analisis grafico descriptivo de metadatos para el grupo de credito seleccionado:

credito <- credito %>%#Transformacion de datos numericos a categoricos
  mutate(SEX = case_when(
    SEX == 1 ~ 'Hombre',
    SEX == 2 ~ 'Mujer',
    TRUE ~ as.character(SEX)
  ))

credito <- credito %>%#Transformacion de datos numericos a categoricos
  mutate(MARRIAGE = case_when(
    MARRIAGE == 1 ~ 'Casado',
    MARRIAGE == 2 ~ 'Soltero',
    MARRIAGE == 3 ~ 'Otro',
    TRUE ~ as.character(MARRIAGE)
  ))

credito <- subset(credito, MARRIAGE != "0") #Elimino datos faltantes

credito <- credito %>%#Transformacion de datos numericos a categoricos
  mutate(EDUCATION = case_when(
    EDUCATION == 1 ~ 'Posgrado',
    EDUCATION == 2 ~ 'Universitario',
    EDUCATION == 3 ~ 'Secundario',
    EDUCATION == 4 ~ 'Otro',
    TRUE ~ as.character(EDUCATION)
  ))

credito <- subset(credito, !(EDUCATION %in% c("0","5","6"))) #Elimino datos faltantes


graf1 <- ggplot(credito, aes(x = SEX, fill=SEX)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Sexo", y = "Conteo", title = "Distribución de sexos") +
  guides(fill = FALSE)

graf2 <- ggplot(credito, aes(x = MARRIAGE, fill = MARRIAGE)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Estado civil", y = "Conteo", title = "Distribución de estado civil") +
  guides(fill = FALSE)

graf3 <- ggplot(credito, aes(x = EDUCATION, fill = EDUCATION)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Sexo", y = "Conteo", title = "Distribución de educación") +
  guides(fill = FALSE)

graf4 <- ggplot(credito, aes(x = AGE)) +
  geom_bar(fill = "lightblue", color = "cyan") +
  theme_minimal() +
  labs(x = "Edad", y = "Conteo", title = "Distribución de edades")

arreglo1 <- grid.arrange(graf1, graf2, graf3, graf4, ncol = 2)


#Análisis de agrupamiento por estado de pago durante el periodo abril 2005 a septiembre 2005

#Seleccion y normalizacion de los datos:
credito_grupo <- credito[,c(7:12)]

# Aplicar K-Means clustering
credito_grupo_s <- as.data.frame(scale(credito_grupo))

k <- 3 #número de clusters deseado

set.seed(123)  # Establecer una semilla para reproducibilidad
kmeans_result <- kmeans(credito_grupo_s, centers = k)

credito$cluster <- as.factor(kmeans_result$cluster)# Agregar el resultado de clustering al DataFrame original

#Validacion del numero de clusters con funcion silhouette
sil <- silhouette(kmeans_result$cluster, dist(credito_grupo_s))

#summary(sil)

#sil1 <- fviz_silhouette(sil)

sil2 <- fviz_cluster(kmeans_result, data = credito_grupo_s,
             ellipse.type = "norm",  # Tipo de elipse
             geom = "point", 
             stand = FALSE,
             frame = TRUE, 
             frame.type = "norm")

grid.arrange(sil1, sil2, ncol = 2)

#Metricas descriptivas de grupo
credito$PAY_0 <- as.numeric(credito$PAY_0)
credito$PAY_2 <- as.numeric(credito$PAY_2)
credito$PAY_3 <- as.numeric(credito$PAY_3)
credito$PAY_4 <- as.numeric(credito$PAY_4)
credito$PAY_5 <- as.numeric(credito$PAY_5)
credito$PAY_6 <- as.numeric(credito$PAY_6)
credito$promedio <- round((rowMeans(credito[,c(7:12)])),2)


cajas <- ggplot(credito, aes(x = cluster, y = promedio, fill = "green")) +
  geom_boxplot() + 
  labs(x = "Grupo", y = "Tipo de historial de pago", title = "Comparativo de grupos") + 
  theme_minimal() +
  guides(fill = FALSE)#El grupo 3 es el que mejor esetado crediticio presenta


#Descripcion del grupo 3
grupo3 <- subset(credito, cluster == "3")

graf5 <- ggplot(grupo3, aes(x = SEX, fill=SEX)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Sexo", y = "Conteo", title = "Distribución de sexos") +
  guides(fill = FALSE)

graf6 <- ggplot(grupo3, aes(x = MARRIAGE, fill = MARRIAGE)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Estado civil", y = "Conteo", title = "Distribución de estado civil") +
  guides(fill = FALSE)

graf7 <- ggplot(grupo3, aes(x = EDUCATION, fill = EDUCATION)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Nivel alcanzado", y = "Conteo", title = "Distribución de educación") +
  guides(fill = FALSE)

graf8 <- ggplot(grupo3, aes(x = AGE)) +
  geom_bar(fill = "lightblue", color = "cyan") +
  theme_minimal() +
  labs(x = "Edad", y = "Conteo", title = "Distribución de edades")

grid.arrange(graf5, graf6, graf7, graf8, ncol = 2)

#Solteros universitarios entre 20 y 30 años

#Creo el dashboard
ui <- fluidPage(
  titlePanel("Análisis de datos crediticios de clientes"),
  mainPanel(
    "Análisis realizado con el objetivo de identificar el subgrupo más numeroso de clientes ",
    "que presentase el mejor historial crediticio para el periodo abril-septiembre", 
    "del año 2005. Se utilizó como criterio el estado de pago mensual del crédito (con o sin retrazo).", 
    "Primero se analizó gráficamente el total de individuos para encontrar el grupo crediticio más numeroso.", 
    "Luego se agruparon los casos según el historial de pagos y finalmente se describe gráficamente",
    "el grupo con mejor historial hallado. Se utilizó una base de datos pública.",
    h3("Menú de gráficos"),
    verticalLayout(
    actionButton("mostrar1", "Histograma general"),
    "Se seleccionaron los individuos con créditos de $50000",
    "-------------------------------------------------------",
    actionButton("mostrar2", "Agrupamiento"),
    "Se agruparon los casos en 3 clusters según el historial de pagos", 
    "-------------------------------------------------------",
    actionButton("mostrar3", "Comparativo"),
    "Comparativo de los clusters, el grupo 3 presenta los mejores datos de pago", 
    "-------------------------------------------------------",
    actionButton("mostrar4", "Descriptivo 1"),
    "Análisis descriptivo del grupo con mejor historial de pagos", 
    "-------------------------------------------------------",
    actionButton("mostrar5", "Descriptivo 2"),
    "Análisis descriptivo del grupo con mejor historial de pagos", 
    "-------------------------------------------------------",
    actionButton("mostrar6", "Descriptivo 3"),
    "Análisis descriptivo del grupo con mejor historial de pagos", 
    "-------------------------------------------------------",
    actionButton("mostrar7", "Descriptivo 4"),
    "Análisis descriptivo del grupo con mejor historial de pagos", 
    "-------------------------------------------------------",
    "",
    h3("Conclusión:"),
    "Se halló un grupo de 514 casos que presentron el mejor historial crediticio",
    " dentro de la categoria de creditos de $50000. Este grupo representa una mayoria",
    "de muejres solteras, de grado universitario y de entre 20 y 30 años de edad."
    )
  )
)
# Definir el servidor
server <- function(input, output) {
  observeEvent(input$mostrar1, {
    showModal(modalDialog(
      title = "Histograma de límite de credito",
      plotOutput("grafico1")
    ))
  })

  
  observeEvent(input$mostrar2, {
    showModal(modalDialog(
      title = "Análisis de agrupamiento",
      plotOutput("grafico2")
    ))
  })
  
  observeEvent(input$mostrar3, {
    showModal(modalDialog(
      title = "Análisis comparativo",
      plotOutput("grafico3")
    ))
  })
  
  observeEvent(input$mostrar4, {
    showModal(modalDialog(
      title = "Análisis descriptivo del grupo",
      plotOutput("grafico4")
    ))
  })
  
  observeEvent(input$mostrar5, {
    showModal(modalDialog(
      title = "Análisis descriptivo del grupo",
      plotOutput("grafico5")
    ))
  })
  
  observeEvent(input$mostrar6, {
    showModal(modalDialog(
      title = "Análisis descriptivo del grupo",
      plotOutput("grafico6")
    ))
  })
  
  observeEvent(input$mostrar7, {
    showModal(modalDialog(
      title = "Análisis descriptivo del grupo",
      plotOutput("grafico7")
    ))
  })
  
  #--------------------------------
  output$grafico1 <- renderPlot({
    grafico_preliminar
  })
  
  output$grafico2 <- renderPlot({
    sil2
  })
  
  output$grafico3 <- renderPlot({
    cajas
  })
  
  output$grafico4 <- renderPlot({
    graf5
  })
  
  output$grafico5 <- renderPlot({
    graf6
  })
  
  output$grafico6 <- renderPlot({
    graf7
  })
  
  output$grafico7 <- renderPlot({
    graf8
  })
}

# Ejecutar la aplicación shiny
shinyApp(ui = ui, server = server)





