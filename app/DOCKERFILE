# Use the official R Shiny + Tidyverse base image (saves build time!)
FROM rocker/shiny-verse:latest

# Install the specific packages your app needs
RUN R -e "install.packages(c('bslib', 'plotly', 'DT', 'viridis', 'shinycssloaders', 'lmerTest', 'zoo'), repos='http://cran.rstudio.com/')"

# Set the working directory inside the container
WORKDIR /app

# Copy all your local files into the container
COPY . /app/

# Hugging Face Spaces strictly requires apps to run on port 7860
EXPOSE 7860

# Command to run the Shiny app on the correct port and host
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 7860)"]