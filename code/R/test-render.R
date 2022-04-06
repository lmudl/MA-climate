bookdown::preview_chapter("./code/Rmarkdown/thesis/3-clustering-results.Rmd",
                          output_dir="./code/Rmarkdown/thesis")

library(bookdown)
setwd("code/Rmarkdown/thesis/")
render_book(input="3-clustering-results.Rmd",
                output_file="chapter-output/3-clusterin-results.html",
            preview = TRUE,
            clean = TRUE)

setwd("code/Rmarkdown/thesis/")


serve_book(dir = "./3-clustering-results.Rmd", output_dir = "_book", preview = TRUE,
           in_session = TRUE, quiet = FALSE)

render_book()
