temp_html <- "_cv.html"

quarto::quarto_render(
  "_cv.qmd",
  output_file = temp_html,
  quiet = TRUE
)

pagedown::chrome_print(
  temp_html,
  output = "cv-vernon.pdf",
  options = list(preferCSSPageSize = TRUE)
)

file.remove(temp_html)
