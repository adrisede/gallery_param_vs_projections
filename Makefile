all: file.html

clean: 
rm -f file.md file.html figure.png

<<<<<<< HEAD
# declare targets that are not actual files to be made
.PHONY: all clean

# if rule runs but exits due to error, outputs written will be deleted
.DELETE_ON_ERROR:

# do not delete intermediate files of a chain of pattern rules
=======
.PHONY: all clean
.DELETE_ON_ERROR:
>>>>>>> b341f48c278d246fad9d5e9587f997c27e64be25
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