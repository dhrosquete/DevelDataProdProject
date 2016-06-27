suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(UsingR))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(utils))



datos<-read.csv("Data/Galton.csv")

#Funcion de calculo del Root Mean Square Error
RMSE <- function(x,y)
{
    RMSEVector<- numeric(length(x))
    for (i in 1:length(x))
        RMSEVector[i]<-sqrt(sum(x[i] - y[i])^2)
    RMSEVector
}

# Limpiando los datos
# Evitando el grupo familiar para tratarlos independientes
datos<-datos[,-1]
datos<-datos[,-5]
set.seed(787878)

# Convirtiendo a centimetros
datos[,1]<-round(datos[,1]*2.54,0)
datos[,2]<-round(datos[,2]*2.54,0)
datos[,4]<-round(datos[,4]*2.54,0)
datos <- datos[datos$Height <= 190 & datos$Height >= 150,]
datos <- datos[datos$Father <= 190 & datos$Father >= 150,]
datos <- datos[datos$Mother <= 190 & datos$Mother >= 150,]


#Picando los datos
inTrain <- createDataPartition(y=datos$Height,p=0.75,list = FALSE)
training<-datos[inTrain,]
test <- datos[-inTrain,]

# Entrenando el modelo
modelFit <- lm(Height~.,data = training)



#Prediccion
pred<-predict(modelFit,newdata = test)

#Calcular la calidad de los resultados
result<-RMSE(pred,test$Height)
mn<-mean(result)
s<-sd(result)
lenDatos <- reactive({length(datos)})
predSize <- mn + s

function(input, output,session) {

    dataset <- reactive({
        datos[sample(nrow(datos), input$sampleSize),]
        
    })
    
    output$plot <- renderPlot({
        p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()

        if (input$color != 'None')
            p <- p + aes_string(color=input$color)

        if (input$jitter)
            p <- p + geom_jitter()
        if (input$smooth)
            p <- p + geom_smooth()
        print(p)


    },height = 550)


        predSize <- reactive({round(as.numeric(input$fatherSize)*modelFit$coefficients[[2]]+
                                                           as.numeric(input$motherSize)*modelFit$coefficients[[3]]+
                                                           modelFit$coefficients[[1]]+
                                                           (if(input$gender=="Male") 1 else 0)*modelFit$coefficients[[4]],digits = 1)})


        output$predSizeBox <- renderValueBox({
            valueBox("Height:", h2(paste0(predSize(), "cm")),icon("sort-by-attributes",lib = "glyphicon"),
                color = if(input$gender=="Male") "blue" else "maroon")

        })

        output$probSizeBox <- renderInfoBox({
            infoBox(title = h5("It is between:"), h5(paste0(round((predSize() - s),0), " and ",round((predSize() + s),0), "cm")), icon = icon("signal",lib = "glyphicon"),
                     color = if(input$gender=="Male") "blue" else "maroon")

        })


}


