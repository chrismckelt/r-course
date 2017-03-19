+####output codebook.md
 + library(knitr)
 + library(markdown)
 + rmd <- file.path("C:\\dev\\r-course\\5-statistical-inference", "assignment_1.rmd")
 + knit(rmd, output = "assignment_1.md", encoding = "ISO8859-1", quiet = FALSE)
 + mark("assignment_1.md", "codebook.html")