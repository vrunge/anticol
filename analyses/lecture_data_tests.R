


read_anticho("../data/CH-Rouffach-Prescriptions-2008-2018_v1.txt.csv")


setwd("C:/Users/runge/Dropbox/A9_anticholinergique/MyRCode/General/")
prescriptions <- read.csv(
  file = "CH-Rouffach-Prescriptions-2008-2018_v1.txt",
  sep = "|", dec = ".", header = TRUE, quote = "", fill = FALSE)

