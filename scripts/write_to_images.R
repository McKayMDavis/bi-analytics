library(gridExtra)

make_img <- function(data, file_name) {
  path <- paste0("./images/", file_name)
  png(path, height = 6000, width = 3500, res = 500)
  grid.table(data, rows = NULL)
  dev.off()
}
