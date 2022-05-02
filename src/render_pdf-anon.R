# the_output <- "latex"

rmarkdown::render("ms.Rmd", output_file="doc/ms-anon.pdf",
                  params=list(anonymous=TRUE,doublespacing=TRUE,removetitleabstract=TRUE),
                  bookdown::pdf_document2(template = stevetemplates::templ_article2(),
                                          latex_engine = "xelatex", dev= "cairo_pdf", toc = FALSE,
                                          number_sections = FALSE))
