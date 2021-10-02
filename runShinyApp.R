message('library paths:\n', paste('... ', .libPaths(), sep='', collapse='\n'))
options(browser=file.path(getwd(),'GoogleChromePortable/App/Chrome-bin/chrome.exe'))
shiny::runApp('app', launch.browser=T)
