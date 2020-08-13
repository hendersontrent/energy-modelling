#----------------------------------------
# This script sets out to load all 
# things required for the project
#----------------------------------------

#----------------------------------------
# Author: Trent Henderson, 13 August 2020
#----------------------------------------

# Load packages

library(tidyverse)
library(readxl)
library(data.table)
library(janitor)
library(scales)
library(lubridate)
library(Cairo)
library(rstan)

# Turn off scientific notation

options(scipen = 999)

# Create an output folder if none exists:

if(!dir.exists('output')) dir.create('output')