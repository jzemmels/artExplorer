library(shiny)
library(shinyjs)
library(tidyverse)
library(rvest)

source("code/helper-functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(shinybusy::add_busy_spinner(),
                # shinyjs::useShinyjs(),
                tags$head(
                ),
                # Application title
                titlePanel("Art Explorer"),

                # Sidebar with a slider input for number of bins
                sidebarLayout(
                  sidebarPanel(width = 2,
                               textInput(inputId = "keyword",label = "Keyword",placeholder = "e.g., Mountain"),
                               numericInput(inputId = "pageNumber",label = "Page Number",value = 1,min = 1,max = 10,step = 1),
                               actionButton(inputId = "searchExecute","Search")
                  ),

                  # Show a plot of the generated distribution
                  mainPanel(width = 10,
                            uiOutput(outputId = "artworkThumbnails")
                  )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  artData <- shiny::reactiveVal()


  output$artworkThumbnails <- renderUI({

    if(length(artData()) == 0){

      return(HTML("<h3>Enter a keyword associated with art that you would like to see and press Search.</h3>"))

    }

    req(nrow(artData()) > 0)

    image_output_list <- pmap(.l = list(artData()$image_id,
                                        artData()$title,
                                        artData()$width,
                                        artData()$height,
                                        artData()$alt_text,
                                        artData()$href,
                                        artData()$src),
                              function(im_id,im_title,
                                       im_width,im_height,im_alt_text,
                                       hrefLink,srcLink){

                                # tags$a(target="_blank",
                                #        href = hrefLink,
                                #        tags$img(src = srcLink,
                                #                 width = "20%",height = "20%",
                                #                 id = paste0(im_id,"_image"),
                                #                 title = paste0(im_title,"\n",im_width," by ",im_height),
                                #                 alt = im_alt_text)) %>%
                                #   shiny::tagAppendAttributes(download = "image.jpg")

                                HTML(paste0("<a target=_blank href='",hrefLink,"' download='img.jpg'>",
                                            "<img src='",srcLink,"' width = '20%' height = '20%' id='",
                                            im_id,"_image' title='",im_title,"\n",im_width," by ",im_height,"'",
                                            "alt='",im_alt_text,"'> </a>"))

                              })

    tags$div(tags$h3("Hover (and pause) over an image to see its dimensions in pixels.
                     Click on an image to download it or open it in a new tab.
                     Increase the page number to load more images."),
             br(),
             image_output_list)

  })

  observeEvent(list(input$searchExecute,input$pageNumber),{

    req(input$searchExecute > 0)

    artData(artQuery(queryString = input$keyword,pageNum = input$pageNumber))

    if(length(artData()) == 0){

      output$artworkThumbnails <- renderUI({

        HTML(paste0("<h3>Hmm...we didn't find any art associated with '",input$keyword,".' Try again with a different keyword.</h3>"))

      })

    }

  })
}

# Run the application
shinyApp(ui = ui, server = server)
