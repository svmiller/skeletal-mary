the_output <- "word"

rmarkdown::render("ms.Rmd", output_file="doc/ms.docx",
                  bookdown::word_document2(reference_docx = stevetemplates::templ_word(),
                                           toc = FALSE, number_sections = FALSE))