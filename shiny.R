
library(devtools)
library(shinyapps)


shinyapps::setAccountInfo(name='thebigparticle',
                          token='C1838A51B02F274FECBAED3980C91739',
                          secret='9g0r6+PoxaLypYKf2SSThec9Oq5WTP+PJIZrv+kZ')


library(shinyapps)
shinyapps::deployApp('path/to/your/app')