library(shiny)
library(rglwidget)
library(rgl)
library(htmlwidgets)
library(jsonlite)




rglwgtctrl <- function(inputId, value="", nrows, ncols) {
  # This code includes the javascript that we need and defines the html
  tagList(
    singleton(tags$head(tags$script(src = "rglwidgetaux.js"))),
    tags$div(id = inputId,class = "rglWidgetAux",as.character(value))
  )
}

rglwgtctrlinline <- function(inputId,value = "",nrows,ncols) {
  # This code includes the javascript that we need and defines the html
  tagList(
    singleton(
  #    tags$head(tags$script(src = "rglwidgetaux.js"))),
     tags$script(HTML(paste0("
var rglwidgetauxBinding = new Shiny.InputBinding();
$.extend(rglwidgetauxBinding, {
  find:function(scope) {
    return$(scope) .find('.rglWidgetAux');
  },
  getValue:function(el) {
    return el.value;
  },
  setValue:function(el,value) {
  / / $(el) .text(value);
    el.value = value;
  },
  getState:function(el) {
    return { value:this.getValue(el) };
  },
   receiveMessage:function(el,data) {
    var$el = $(el);

    switch(data.cmd) {
      case 'test':alert('Recieved Message');
      break;
      case 'getpar3d':
                    var rglel = $('#' + data.rglwidgetId);
      if (rglel.length == = 0) {
        alert('bad rglwidgetId:' + data.rglwidgetId);
        return null;
      }
      var rglinst = rglel[0] .rglinstance;
      var sid = rglinst.scene.rootSubscene;
      var par3d = rglinst.getObj(sid) .par3d;
      this.setValue(el,JSON.stringify(par3d));
                    $el.trigger('change');
      / / tell myself that I have changed
      break;
    }
  },
  subscribe:function(el,callback) {
    $(el) .on('change.rglwidgetauxBinding',function(e) {
      callback();
    });
  },
  unsubscribe:function(el) {
    $(el) .off('.rglwidgetauxBinding');
  }
});
Shiny.inputBindings.register(rglwidgetauxBinding);
  ")))),
  tags$div(id = inputId,class = "rglWidgetAux",as.character(value))
  )
}

ui <- fluidPage(
  rglwgtctrl('ctrlplot3d'),
  actionButton("regen", "Regen Scene"),
  actionButton("queryumat", "Query User Matrix"),
  rglwidgetOutput("plot3d"),
  tableOutput("usermatrix")
)

server <- function(input, output, session) 
{
  observe({
    # tell our rglWidgetAux to query the plot3d for its par3d
    input$queryumat
    session$sendInputMessage("ctrlplot3d",list("cmd"="getpar3d","rglwidgetId"="plot3d"))
  })

  output$usermatrix <- renderTable({
    # grab the user matrix from the par3d stored in our rglWidgetAux
    # note we are using two different "validate"s here, which is quite the pain if you 
    # don't notice that it is declared in two different libraries
    shiny::validate(need(!is.null(input$ctrlplot3d),"User Matrix not yet queried"))
    umat <- matrix(0,4,4)
    jsonpar3d <- input$ctrlplot3d
    if (jsonlite::validate(jsonpar3d)){
      par3dout <- fromJSON(jsonpar3d)
      umat <- matrix(unlist(par3dout$userMatrix),4,4) # make list into matrix
    }
    return(umat)
  })
  
  scenegen <- reactive({
     # make a random scene
     input$regen
     n <- 1000
     x <- sort(rnorm(n))
     y <- rnorm(n)
     z <- rnorm(n) + atan2(x, y)
     plot3d(x, y, z, col = rainbow(n))
     scene1 <- scene3d()
     rgl.close() # make the app window go away
     return(scene1)
  })
  output$plot3d <- renderRglwidget({ rglwidget(scenegen()) })
}

shinyApp(ui=ui, server=server)