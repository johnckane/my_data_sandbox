Rmd2htmlWP <- function(infile,outfile,sup=T){
  # Objective: Copy and paste .Rmd knitted HTML into the WordPress post 'Text'-box (not 'Visual') and be done
  # Why:       With the WP Jetpack plugin installed, you don't need other plugins like WP-Markdown, WP-LaTeX, or MathJax LaTeX     
  # How:       Use RStudio to create an .Rmd infile 
  #            Use the $latex ...$ code for LaTeX equations 
  #            Run Rmd2htmlWP(infile="your.Rmd",outfile="your.html") to get an HTML file 
  #            Copy the HTML code generated in the outfile, add a new post to your WP blog, and paste the HTML into the box of the 'Text'-tab   
  # Notes:     Preview the post on the blog itself and not by choosing the 'Visual' tab; this will erase some HTML that is needed
  #            It is Ok to delete the <!DOCTYPE html> entry, leave the rest untouched if possible
  #            If WordPress does not render the superscript automatically when it encouters ^, use sup=FALSE, note effect on LaTeX code using ^  
  #
  # Created by Fred Hasselman
  # Contact meâˆžfredhasselmanâ€¢com if you have any questions about this file
  require(markdown)
  require(knitr)
  tOpt <- markdownHTMLOptions(default=T)
  tOpt <- tOpt[tOpt!="mathjax"]
  tExt <- markdownExtensions()
  tExt <- tExt[tExt!="latex_math"]
  if(sup==T){tExt <- tExt[tExt!="superscript"]}
  knit2html(input = infile, output = outfile, options = c(tOpt), extensions= c(tExt))
}

library(RWordPress)
options(WordpressLogin = c(johnckane = 'cz14F4b12'),
        WordpressURL = 'http://user.wordpress.com/xmlrpc.php')
library(knitr)
knit2wp(
input = '/home/john/My Data Sandbox/Fumbles/write_up.Rmd', 
title = "My Take on Fumble Analysis",
action = newPost, 
publish = TRUE)

Rmd2htmlWP <- function(infile, outfile, sup = T) {
  require(markdown)
  require(knitr)
  mdOpt <- markdownHTMLOptions(default = T)
  mdOpt <- mdOpt[mdOpt != "mathjax"]
  mdExt <- markdownExtensions()
  mdExt <- mdExt[mdExt != "latex_math"]
  if (sup == T) {
    mdExt <- mdExt[mdExt != "superscript"]
  }
  knit2html(input = infile, output = outfile, options = c(mdOpt), extensions = c(mdExt))
}

Rmd2htmlWP(infile="/home/john/My Data Sandbox/Fumbles/write_up.Rmd",
           outfile="/home/john/My Data Sandbox/Fumbles/wp_post.html")
