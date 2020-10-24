library(devtools)
library(fs)
library(tidyverse)

# Functions to include
use_package('ggplot2')


# Tests
tracer <- read_delim('tracer.txt', delim = ';')
tracer

plot_tracer(tracer_data = tracer, time_threshold = 40000)

