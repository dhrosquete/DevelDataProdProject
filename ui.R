suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(UsingR))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(utils))

datos<-read.csv("Data/Galton.csv")

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

header <- dashboardHeader(
    title = "Galton Height's"
)

body <- dashboardBody(
    tableOutput("filetable"),
    fluidRow(
        column(width = 8,
               box(width=NULL, status="warning", collapsible = TRUE,
                   p(class = "text-muted",
                     h3("Data used for prediction model"),
                     h4("On this side, you can manipulate and resample
                     the used data (Galton´s data), in order to check the following:"),
                     h4("* The bigger you set the sample size, the better you see
                         the adjusted formula. The model was made using 865 observations."),
                     h4("* Change the X and Y parameters to check the relation with the mother or the father
                        on the children´s height"),
                     h4("* The gender is quite important, that´s why it is grouped separately. You can see that the males
                        are always taller than females")
                   )
               ),
               box(width=NULL, status="danger",
                   h4("Manipulate the data model"),
                   sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(datos),
                               value=min(501, nrow(datos)), step=50, round=0),

                    tags$table(
                       border="0",
                       cellpadding="3",
                       cellspacing="10",
                       tags$tr(
                           tags$td(selectInput('x', 'X', names(datos),width = 100)),
                           tags$td(selectInput('y', 'Y', names(datos), names(datos)[[4]],width = 100)),
                           tags$td(selectInput('color', 'Color', c('None', names(datos)),selected = names(datos)[[3]],width = 100)),
                           tags$td(checkboxInput('jitter', 'Jitter')),
                           tags$td(checkboxInput('smooth', 'Smooth',value = TRUE))
                       )
                   )
               ),
               box(width = NULL, solidHeader = TRUE,
                   plotOutput('plot'),height = 600),

               box(width = NULL, status="success",
                   h3("References"),
                   h4("Data downloaded from:"),
                   a("The University of Alabama in Huntsville", href="http://www.math.uah.edu/stat/data/Galton.html"))


        ),
        column(width = 3,
               box(width=NULL, status="info",
                   p(class = "text-muted",
                     h3("Prediction Model"),
                     h4("Let´s see if I can guess your age!")
                   )
                   ),
               box(width = NULL, status = "info",
                   sliderInput('fatherSize', 'Your father size', min=100, max=230,
                               value=170,step=1, round=0,animate = TRUE),
                   sliderInput('motherSize', 'Your mother size', min=100, max=230,
                               value=145,step=1, round=0,animate = TRUE),
                   radioButtons('gender', 'Your gender',choices = c("Male","Female"),selected = "Male")),
            fluidRow(
            valueBoxOutput("predSizeBox",width = 12),
            h3("95% chance"),
            infoBoxOutput("probSizeBox",width = 12)),

            
            h4("Model created by:"),
            h5("Daniel Rosquete - dhrosquete@gmail.com")
        )
    )
)

dashboardPage(skin="blue",
    header,
    dashboardSidebar(disable = TRUE),
    body
)
