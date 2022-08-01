my_preview <- function(input, 
                       out_format = 'bookdown::gitbook',
                       output_dir="preview-chapters",
                       config_file ="_chapter.yml") {
  if(!(grepl("thesis", getwd()))) {
    setwd("./thesis")
  }
  bookdown::preview_chapter(input = input,
                            clean = TRUE,
                            output_dir = output_dir,
                            output_format = out_format,
                            config_file =  "_chapter.yml"
  )
  if(out_format == 'bookdown::gitbook' | out_format == 'bookdown::html_document2') {
    fn <- sub(".Rmd", "", input)
    fn <- paste0("preview-",fn,".html")
    l <- list.files("preview-chapters/")
    # stopifnot("_main.html" %in% l)
    # keep <- which(l == fn)
    # file.remove(paste0(output_dir, "/", l[!keep]))
    if(out_format == "bookdown::html_document2") {
      from <- paste0("_main.html")
      to <- paste0(output_dir, "/", fn)
      file.rename(from, to) 
    }
  }
  if(out_format == 'bookdown::pdf_book' | out_format == "bookdown::pdf_document2") {
    fn <- sub(".Rmd", "", input)
    fn <- paste0("preview-",fn,".pdf")
    from <- paste0(output_dir, "/","_main.pdf")
    to <- paste0(output_dir, "/", fn)
    file.rename(from = from, to = to)
  }
  setwd("..")
}

my_preview("5t-method-fused-lasso.Rmd",
           out_format = "bookdown::pdf_document2")

quick <- function(input, out_folder) {
  if(!(grepl("thesis", getwd()))) {
    setwd("./thesis")
  }
  fn <- sub(".Rmd", "", input)
  fn <- paste0("preview-",fn,".html")
  output <- paste0("quick/", fn)
  knitr::knit2html(input = input, output = output)
  setwd("..")
  
}

quick("5t-method-fused-lasso.Rmd")
