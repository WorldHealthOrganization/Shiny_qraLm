# Function to create the fullscreen button UI along with its JavaScript functionality
createFullscreenToggleFeature <- function(element_id) {
  ns = NS(element_id)
  btn_id <- ns("fullscreenBtn")
  
  # Define the UI button for fullscreen toggle
  btn_ui <- actionButton(
    btn_id, 
    label = "", 
    icon = icon("expand"),
    style = "position: absolute; top: 10px; right: 10px; background-color: transparent; border: none;"
  )
  
  # Define the JavaScript script for the fullscreen toggle feature
  js_code <- sprintf(
    "
    $(document).on('click', '#%s', function() {
      var elem = document.getElementById('%s'); // Ensure correct element selection
      
      function requestFullScreen(element) {
        if (element.requestFullscreen) {
          element.requestFullscreen();
        } else if (element.mozRequestFullScreen) { 
          element.mozRequestFullScreen();
        } else if (element.webkitRequestFullscreen) { 
          element.webkitRequestFullscreen();
        } else if (element.msRequestFullscreen) { 
          element.msRequestFullscreen();
        }
      }
      
      function exitFullScreen() {
        if (document.exitFullscreen) {
          document.exitFullscreen();
        } else if (document.mozCancelFullScreen) { 
          document.mozCancelFullScreen();
        } else if (document.webkitExitFullscreen) { 
          document.webkitExitFullscreen();
        } else if (document.msExitFullscreen) { 
          document.msExitFullscreen();
        }
      }

      // Check if any element is in fullscreen mode
      var isFullscreen = document.fullscreenElement || 
                         document.webkitFullscreenElement || 
                         document.mozFullScreenElement || 
                         document.msFullscreenElement;

      if (!isFullscreen) {
        requestFullScreen(elem);
        $('#%s i').removeClass('fa-expand').addClass('fa-compress');
      } else {
        exitFullScreen();
        $('#%s i').removeClass('fa-compress').addClass('fa-expand');
      }
    });
    ", btn_id, element_id, btn_id, btn_id)
  
  btn_script <- tags$script(HTML(js_code))
  
  # Return a tag list with both the button UI and its associated script
  return(tagList(btn_ui, btn_script))
}

# Usage in UI:
# createFullscreenToggleFeature("targetElementHTMLRenderedId")
