
#rm(list = ls())
#devtools::load_all(".")
#app()

#function to run application
app = function() {
  require(shiny)
  require(euroformix)
  shinyApp(ui, server)
}