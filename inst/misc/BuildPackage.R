#----------------------------------------------------
# Build and Tests for waterlogged
#----------------------------------------------------

library(usethis)
library(testthat)
library(rhub)
library(devtools)
library(qpdf)
library(testthat)

# Loading unfinished package to memory...
rm(list = ls())
devtools::load_all()
devtools::document()
devtools::test()  # Run tests - all passed Feb 18 2026  :)
devtools::check(vignettes = FALSE) # Operating system test



# ===================================
## TO FORMALLY INSTALL PACKAGE
# ===================================
# remove.packages("waterlogged")
# install.packages(getwd(), repos = NULL, type = "source")
# devtools::install_github("dfo-pacific-science/waterlogged")
# library(waterlogged)
# Installing unfinished package to computer...
# Check on cran against LTR version of R
# devtools::check_win_release()
# citation("waterlogged")




#----------------------------------------------------
# Make logo
#----------------------------------------------------
# See: https://github.com/GuangchuangYu/hexSticker
# library(hexSticker)
# image12.png
# imgurl <- "image12.png"
# sticker(imgurl, package="JoeModelCE", p_size=20, s_x=1, s_y=.65, s_width=.7,
#         filename="logo.png")
