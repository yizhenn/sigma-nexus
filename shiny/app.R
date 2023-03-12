runApp(appDir = getwd(),
  launch.browser = getOption("shiny.launch.browser", interactive()),
  host = "127.0.0.1", port = 8100)