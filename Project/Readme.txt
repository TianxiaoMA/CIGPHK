In my file, I use some historical prices from Apple to represent the prices of different kinds of assets, which includes different asset class(A & B), differnet currency(f & g), different geography(a & b) and different positions (6 & 7). And, in the line chart, there are some strange straight lines at the edge of different kind asset, which result from the select of asset kinds are not completely independent with each other.

And I do not finish editing exact appearance of each chart but just a simple frame of the dashboard, if you have any opinion, please tell me and I will edit it as soon as possible.

To run it, we can use following command line by line,

library(shiny) # If the package "shiny" need to install, use command install.package("shiny") at first
path <- "~/Project" # "~" means working directory of R
runApp(path)