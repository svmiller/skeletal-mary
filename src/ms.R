
rmarkdown::render("ms.Rmd", 
                  output_file="doc/ms.pdf", 
                  bookdown::pdf_document2(template = stevetemplates::templ_article2(), 
                                          latex_engine = "xelatex", dev="cairo_pdf", toc = FALSE,
                                          number_sections = FALSE))

gc()
system("rm -f *.log *.fff *.ttt")

# the_output <- "latex"

rmarkdown::render("ms.Rmd", output_file="doc/ms-anon.pdf",
                  params=list(anonymous=TRUE,doublespacing=TRUE,removetitleabstract=TRUE),
                  bookdown::pdf_document2(template = stevetemplates::templ_article2(),
                                          latex_engine = "xelatex", dev= "cairo_pdf", toc = FALSE,
                                          number_sections = FALSE))

system("rm -f *.log *.fff *.ttt")

rmarkdown::render("ms.Rmd", output_file="doc/ms.docx",
                  bookdown::word_document2(reference_docx = stevetemplates::templ_word(),
                                           toc = FALSE, number_sections = FALSE))
