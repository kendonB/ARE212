shinyUI(bootstrapPage(
  
  sliderInput(inputId = "n_breaks",
      label = "Number of bins in histogram (approximate):",
      min = 10, max = 50, value = 30, step = 1),

  checkboxInput(inputId = "individual_obs",
      label = strong("Show individual observations"),
      value = TRUE),

  checkboxInput(inputId = "density",
      label = strong("Show density estimate"),
      value = TRUE),

  plotOutput(outputId = "main_plot", height = "300px"),
  
  # Display this only if the density is shown
  conditionalPanel(condition = "input.density == true",
    selectInput("kernel","Kernel:",
      list("gaussian", "epanechnikov", "rectangular", "triangular", "biweight","cosine", "optcosine"))),
  
  conditionalPanel(condition = "input.density == true",
    sliderInput(inputId = "bw_adjust",
        label = "Bandwidth adjustment:",
        min = 0.1, max = 2, value = 0.5, step = 0.1)
  )
))
