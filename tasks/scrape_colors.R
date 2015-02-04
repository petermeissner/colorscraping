## scraping color schemes


# get colors
Colors <- get_wes_andersons()
Colors

# overview as png
png("wes_anderson_colors.png")
  plot_Colors(Colors)
dev.off()

# overview as pdf
pdf("wes_anderson_colors.pdf")
  plot_Colors(Colors)
dev.off()

# overview in R
plot_Colors(Colors)

# save to rdata-file
save(Colors, file="wes_anderson_schemes.rdata")
