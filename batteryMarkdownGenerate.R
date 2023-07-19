library(ggplot2)
library(tidyverse)
library(glue)
library(blastula)

#get deployed gliders
deployedGliders <- read.csv("/echos/deployedGliders.txt", 
                            sep = "",
                            header = FALSE)
colnames(deployedGliders)[1] = "Name"
colnames(deployedGliders)[2] = "ahrCap"

#only process "real" ones
deployedGliders <- deployedGliders %>%
  filter(!str_starts(Name,"#")) #remove any commented lines

#initialize list
gliders_live <- list()
for (i in deployedGliders$Name){
  
  #load latest live data file
  load(paste0("/echos/", i, "/glider_live.RData"))
  
  if (ahrCap$ahrCap > 0){
    msg <- envelope() %>%
      render("batteryMarkdown.Rmd")
  }
  
}


# Prepare the image (as a text input)
gg_image <- add_ggplot(plot_object = couLive)

# Generate the body text
body_text <-
  md(c(
    "
Hello,

I just wanted to let you know that the *ggplot* you \\
wanted to see is right here, in this email:",

gg_image,
"

Cheers,

Me
"
  ))

# Compose the email message
out <- compose_email(body = body_text)

htmltools::save_html(out$html_html, "test3.html")

# use cid_images to read in the file
email_obj <- blastula:::cid_images("test3.html")

htmltools::save_html(email_obj$html_html, "test4.html")
