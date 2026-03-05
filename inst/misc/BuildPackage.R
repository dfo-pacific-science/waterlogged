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



# Duration: 430.3 s
#
# ── Failed tests ──────────────────────────────────────────────────────────────────────────────────────────────────────
# Failure (test-add_nearest_baro.R:13:3): Test no usable barometric files
# `add_nearest_baro(...)` did not throw the expected error.
#
# Error (test-adjust_wl_zero.R:63:3): test adjust wl zero
# Error in `expect_all_equal(wl_unadjusted_time$adjustment, 0)`: could not find function "expect_all_equal"
#
# Error (test-dissox_qaqc.R:20:3): Test dissox QAQC
# Error in `convert_do_mgl_percsat(DO_with_Baro[[1]], output_dir = path_to_output_folder,
#                                  version_label = "v0.3")`: unused argument (output_dir = path_to_output_folder)
#
# [ FAIL 3 | WARN 70 | SKIP 0 | PASS 152 ]
# @  FAIL 3 | WARN 56 | SKIP 0 | PASS 223 ]

# > 430.3/60
# [1] 7.171667



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
