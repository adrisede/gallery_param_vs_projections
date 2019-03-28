all: file.html

clean: 
rm -f file.md file.html figure.png

.PHONY: all clean
.DELETE_ON_ERROR:
.SECONDARY:

# Render a graphViz file
%.png: %.gv
dot -Tpng -o $@ $<

# Knit a RMarkdown document
%.md: %.Rmd
Rscript -e 'knitr::knit("file.Rmd")'

# Render a Markdown document to HTML
%.html: %.md 
pandoc -s -o $@ $<

# Dependencies on figures
file.html: figure.png