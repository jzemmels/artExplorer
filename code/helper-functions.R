artQuery <- function(queryString = "",pageNum = 1){

  # Build a query from the U Chicago API
  urlDomain <- "https://api.artic.edu/api/v1/artworks/search?"

  urlFilter <- "&query[bool][must][][term][classification_titles.keyword]=painting&query[bool][must][][term][is_public_domain]=true&"

  urlFields <- "fields=id,thumbnail,title,image_id,api_link,height,width,artist_display,dimensions,colorfulness,color,term_titles?"

  urlPages <- paste0("page=",pageNum,"&limit=100")

  # the U chicago artwork comes in the form a JSON file that we parse to get
  # art/image information
  chicagoData <- jsonlite::fromJSON(paste0(urlDomain,"q=",queryString,urlFilter,urlFields,urlPages))$data

  if(length(chicagoData) > 0){

    chicagoData <- chicagoData %>%
      unnest(thumbnail) %>%
      select(title,alt_text,image_id,width,height) %>%
      mutate(href = paste0("https://www.artic.edu/iiif/2/",image_id,"/full/",width,",",height,"/0/default.jpg"),
             src = paste0("https://www.artic.edu/iiif/2/",image_id,"/full/843,/0/default.jpg"))

  }

  if(!httr::http_error(paste0("https://artvee.com/main/page/",pageNum,"/?s=",queryString,"&per-page=70"))){

    # artvee.com is a different site that compiles openly-available data from
    #a variety of sites. This is just standard HTML that we parse
    artveeHTML <- rvest::read_html(paste0("https://artvee.com/main/page/",pageNum,"/?s=",queryString,"&per-page=70")) %>%
      html_elements("body") %>%
      html_elements("div")

    # The data-url takes us to a webpage containing important meta information
    # about the art/image
    artvee_urls <- artveeHTML %>%
      html_attr("data-url")

    artveeData <- data.frame(
      # title of the painting
      title = artvee_urls[which(!is.na(artvee_urls))] %>%
        str_remove("/dl/") %>%
        str_replace("-"," "),
      # thumbnail image to be shown on the app
      src = artveeHTML %>%
        .[which(!is.na(artvee_urls))] %>%
        html_elements("img") %>%
        html_attr("data-wood-src"),
      # width of the highest-quality downloadable image
      width = artveeHTML  %>%
        .[which(!is.na(artvee_urls))] %>%
        html_attr("data-sk")  %>%
        str_extract('\"hdlimagesize\":\"[0-9]{1,} x [0-9]{1,}') %>%
        str_remove('\"hdlimagesize\":\"') %>%
        str_split(" x ") %>%
        map_dbl(~ as.numeric(.[1])),
      # height of the highest-quality downloadable image
      height = artveeHTML  %>%
        .[which(!is.na(artvee_urls))] %>%
        html_attr("data-sk")  %>%
        str_extract('\"hdlimagesize\":\"[0-9]{1,} x [0-9]{1,}') %>%
        str_remove('\"hdlimagesize\":\"') %>%
        str_split(" x ") %>%
        map_dbl(~ as.numeric(.[2]))) %>%
      mutate(
        # numeric ID of the image of the painting
        image_id = str_remove(src,"https://mdl.artvee.com/ft/"),
        # download link of the high-quality image
        href = paste0("https://mdl.artvee.com/hdl/",image_id),
        # alt_text in case the image doesn't render
        alt_text = title
      )


  }
  else{

    artveeData <- NULL

  }

  if(length(chicagoData) > 0 & length(artveeData) > 0){

    return(bind_rows(chicagoData,artveeData) %>%
             # shuffle the paintings for fun
             slice_sample(n = nrow(.)) %>%
             # remove paintings missing important meta information
             filter(!is.na(width)))

  }
  else{return(NULL)}

}
