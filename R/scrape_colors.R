#' a function for getting images links from http://wesandersonpalettes.tumblr.com/
get_image_links <- function(url="http://wesandersonpalettes.tumblr.com/"){
  html       <- rvest::html(url)
  image_urls <- rvest::html_attr(rvest::html_nodes(html, "img[width='500']"),"src")
  return(image_urls)
}


#' a function for downloading images from http://wesandersonpalettes.tumblr.com/
download_images <- function(image_urls, folder=""){
  fnames    <- paste0(seq_along(image_urls),".jpg")
  for(i in seq_along(image_urls)){
    download.file(image_urls[i], paste0(folder,"/",fnames[i]), mode="wb")
  }
  fnames <- paste0(folder,"/",fnames)
  return( fnames )
}

#' a function for laoding images into workspace
load_images <- function(fnames){
  images <- list()
  for ( i in seq_along(fnames))  {
    images[[i]] <- jpeg::readJPEG(fnames[i])
  }
  return(images)
}

#' a function for processing images and extracitng colors
process_images <- function(images){
  worker <- function(image){
    bottom <- tail(1:dim(image)[1],100)
    red   <- as.vector(image[bottom, ,1])
    green <- as.vector(image[bottom, ,2])
    blue  <- as.vector(image[bottom, ,3])
    colorh      <- rgb(red, green, blue)
    colorh_tab  <- table(colorh)
    colorscheme <- names(colorh_tab[colorh_tab>1000 & colorh_tab<20000])
    return(colorscheme)
  }
  lapply(images, worker)
}

#' a function to get wes anderson color schemes from the web
get_wes_andersons <- function(){
  fnames       <- download_images(get_image_links())
  colorschemes <- process_images(load_images(fnames))
  message(getwd())
  #file.remove(fnames)
  return(colorschemes)
}

#' function for plotting lists of color sets
plot_Colors <- function(Colors){
  seq_col <- seq_along(Colors)
  n_col   <- length(Colors)
  max_col <- max(unlist(lapply(Colors, length)))
  plot( x    = c(1-0.5,max_col+0.5)[1:(n_col+2)], 
        y    = c(0, seq_col, n_col+1), 
        type = "n", 
        xlab = "colors", 
        ylab = "schemes"
  )
  for( i in seq_col ) {
    for( k in seq_along(Colors[[i]]) ){
      segments(k-0.5, i, k+0.5, i, col=Colors[[i]][k], lwd=25)
    }
  }
}
