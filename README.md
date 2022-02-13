
# example-shiny-callr

This is an example app showing how to have button to interrupt plot creation without killing the app.

The idea is to put the plot creation in a background process with `callr` and kill the process if the button "Stop" is clicked.

