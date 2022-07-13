library(shiny)
# library(shinyjs)
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
                               uiOutput(outputId = "searchButton")
                  ),

                  # Show a plot of the generated distribution
                  mainPanel(width = 10,
                            uiOutput(outputId = "artworkThumbnails")
                  )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # artData will store the data frame of art information
  artData <- shiny::reactiveVal()

  # this officially renders the art images
  output$artworkThumbnails <- renderUI({

    #artwork is reactive to the artData data frame. If it's empty to start off
    #with, then a message will display
    if(length(artData()) == 0){

      return(HTML("<h3>Enter a keyword associated with art that you would like to see and press Search.</h3>"))

    }

    # ...otherwise there is art in the artData object
    req(nrow(artData()) > 0)

    # image list contains HTML elements
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
                                            "<img src='",srcLink,"' width = '30%' height = '30%' id='",
                                            im_id,"_image' title='",im_title,"\n",im_width," by ",im_height,"'",
                                            "alt='",im_alt_text,"'> </a>"))

                              })

    # return the image list along with a message about how to interact
    tags$div(tags$h3("Hover (and pause) over an image to see its dimensions in pixels.
                     Click on an image to download it or open it in a new tab.
                     Click 'Search for more art' to load more images."),
             br(),
             image_output_list)

  })

  # if the user updates the keyword field, then they are interested in a new search.
  observeEvent(input$keyword,{

    # req(length(input$keyword) > 0)s

    # render  a button to execute a new search
    output$searchButton <- renderUI({

      actionButton(inputId = "searchExecute","Search")

    })

    # render a message about how to execute a search
    output$artworkThumbnails <- renderUI({

      HTML("<h3>Enter a keyword associated with art that you would like to see and press Search.</h3>")

    })

    pageCount(0)

  })

  # once a search is executed, we'll load a new button to load additional images.
  pageCount <- reactiveVal(0)

  observeEvent(list(input$searchExecute),{

    req(input$searchExecute > 0)

    # render the button to load new images
    output$searchButton <- renderUI({

      actionButton(inputId = "loadMoreImages",label = "Load more images")

    })

    # the first time after Search is pressed, we'll load the first page of the
    # search result
    artData(artQuery(queryString = input$keyword))

    pageCount(1)

    # if the artData object returned is empty, then there weren't any search
    # results given the keyword
    if(length(artData()) == 0){

      output$artworkThumbnails <- renderUI({

        HTML(paste0("<h3>Hmm...we didn't find any art associated with '",input$keyword,".' Try again with a different keyword.</h3>"))

      })

    }

  })

  # load another page of images if the loadMoreImages button is pressed
  observeEvent(input$loadMoreImages,{

    pageCount(pageCount() + 1)

    artData(artQuery(queryString = input$keyword,pageNum = pageCount()))

  })
}

# Run the application
shinyApp(ui = ui, server = server)
