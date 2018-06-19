# adityamangal410.github.io
github pages repo for my blog

- Link - adityamangal.com
- fab live_build - build and live watch at http://localhost:8080 
- fab enter_dns_file - add dns file before pushing to master github
- fab github - push files to github on to the prod blog
- fab make_entry:"India Transportation Accidents Exploration Project" - Add new article with the title
- fab build_rmd - Render R Markdown files
- Theme blog - https://blog.alexandrevicenzi.com/ 
- Theme blog conf file - https://github.com/alexandrevicenzi/blog/blob/master/pelicanconf.py 
- Theme blog git repo - https://github.com/alexandrevicenzi/blog 
- Theme git repo (and wiki) - https://github.com/alexandrevicenzi/Flex 
- Building data science portfolio - https://www.dataquest.io/blog/data-science-portfolio-project/ 
- Pelican help blog - http://nafiulis.me/making-a-static-blog-with-pelican.html
- Pelican Rmarkdown setup blog - http://michaeltoth.me/how-to-write-pelican-blog-posts-using-rmarkdown-knitr-20.html


Setup Steps - 
- conda create --name portfolio python=3.5
- source activate portfolio
- conda install -c conda-forge -c anaconda -c r pelican markdown fabric3 livereload rpy2 r-knitr r-tidyverse r-leaflet r-rgeos r-base r-rgdal libgdal r-webshot
- or conda env create -f environment.yml
- git clone --recursive https://github.com/getpelican/pelican-themes ~/pelican-themes
- git clone --recursive https://github.com/getpelican/pelican-plugins ~/pelican-plugins
