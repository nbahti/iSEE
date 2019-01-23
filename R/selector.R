#' Interactively select points 
#'
#' Provides an interface to interactively define multiple groups of interest, possibly for use as column/row metadata in the main \code{\link{iSEE}} app.
#' 
#' @param x A numeric vector of x-coordinates.
#' @param y A numeric vector of y-coordinates; alternatively, only \code{x} can be provided, see \code{\link{xy.coords}}.
#' @param multiSelect A string indicating how multiple selections should be handled.
#' @param width Integer scalar between 1 and 6 specifying the width of the plot in grid units.
#' @param height Integer scalar specifying the height of the plot in pixels.
#' @param ... Further arguments to be passed to \code{\link{plot}}.
#'
#' @details
#' This function will create an interface that is specialized for point selection.
#' Multiple groups can be easily selected in a single sitting, and the identity of the selected points returned upon stopping the app.
#' This is intended for users to create arbitrary metadata fields to use elsewhere, e.g., in the main \code{\link{iSEE}} app.
#' 
#' The nature of the stop-return value depends on the choice of \code{multiSelect}.
#' If \code{multiSelect="all"}, the return value is a list of integer vectors where each entry corresponds to one group and contains indices for all elements in that group.
#' Otherwise, the return value is a factor of length equal to \code{x} containing the group assignment for each element.
#' If \code{multiSelect="first"}, an element is assigned to the first group it is selected in, while if \code{multiSelect="last"}, it is assigned to the last group (i.e., overwrite).
#'
#' @return A shiny app object.
#'
#' @author Aaron Lun, based on old code in \pkg{scran}. 
#'
#' @export
#' @importFrom grDevices xy.coords
#' @importFrom shiny brushedPoints observeEvent shinyApp fluidPage fluidRow column actionButton brushOpts
#' 
#' @examples
#' x <- runif(1000)
#' y <- rnorm(1000)
#' app <- selector(x, y)
#' 
#' if (interactive()) {
#'    out <- shiny::runApp(app)
#' }
selector <- function(x, y=NULL, multiSelect=c("first", "last", "all"), width=5, height=500, ...) 
{   
    xy.out <- xy.coords(x=x, y=y)
    collected <- new.env()
    resetValues(collected, data.frame(x=xy.out$x, y=xy.out$y))
    multiSelect <- match.arg(multiSelect)

    # Internal functions, to avoid passing many arguments around.
    plotFun1 <- function(output) {
        generatePlot1(output, collected, ...)
    }
    plotFun2 <- function(output) {
        generatePlot2(output, collected, ...)
    }
    updateSelect <- function(input, output, setting) {
        collected$current.selected[brushed$selected_] <- setting
        plotFun1(output)
    }

    # Generating the page layout.
    ui <- fluidPage(
        fluidRow(
            column(width = width,
                plotOutput("plot1", height = height,
                    brush = brushOpts(id = "plot1_brush", resetOnNew=FALSE),
                    click = "plot1_click"
                )
            ),
            column(width = width,
                plotOutput("plot2", height = height)
            )
        ), hr(),
        actionButton("list_add", "Add to list"),
        actionButton("reset", "Reset all"),
        actionButton("finish", "Save list to R")
    )

    # Setting up the server actions.
    server <- function(input, output, session) {
        plotFun1(output)
        plotFun2(output)

        observeEvent(input$plot1_click, {
            if (!is.null(input$plot1_brush)) {
                return(NULL)
            }
            prev_lasso <- collected$lasso
            collected$lasso <- .update_lasso(input$plot1_click, prev_lasso)
            plotFun1(output)
        }, ignoreInit=TRUE)

        observeEvent(input$plot1_brush, {
            if (!is.null(collected$lasso)) {
                collected$lasso <- NULL
                plotFun1(output)
            }
        }, ignoreInit=TRUE, ignoreNULL=FALSE)

        observeEvent(input$list_add, {
            if (!is.null(input$plot1_brush)) {
                kept <- brushedPoints(collected$coords, xvar="x", yvar="y", input$plot1_brush)
                XMIN <- input$plot1_brush$xmin
                XMAX <- input$plot1_brush$xmax
                YMIN <- input$plot1_brush$ymin
                YMAX <- input$plot1_brush$ymax
                bounds <- rbind(c(XMIN, YMIN), c(XMIN, YMAX), c(XMAX, YMAX), c(XMAX, YMIN))
            } else if (!is.null(collected$lasso) && collected$lasso$closed) {
                out <- collected$lasso
                out$mapping <- list(x="x", y="y")
                kept <- lassoPoints(collected$coords, out)
                bounds <- collected$lasso$coord
            } else {
                showNotification(type="error", "no selection available")
                return(NULL)
            }

            collected$groups <- c(collected$groups, list(as.integer(rownames(kept))))
            collected$boundaries <- c(collected$boundaries, list(bounds))
            plotFun2(output)
        }, ignoreInit=TRUE)

        observeEvent(input$reset, {
            resetValues(collected)
            plotFun2(output)
        }, ignoreInit=TRUE)

        observeEvent(input$finish, {
            if (multiSelect=="all") {
                keep.val <- collected$groups
            } else {
                ordering <- seq_along(collected$groups)
                if (multiSelect=="first") {
                    ordering <- rev(ordering) # earliest group overwrites everything else.
                }

                keep.val <- integer(nrow(collected$coords))
                for (o in ordering) {
                    keep.val[collected$groups[[o]]] <- o
                }
                keep.val <- factor(keep.val)
            }

            stopApp(keep.val)
        }, ignoreInit=TRUE)
    }

    shinyApp(ui, server)
}

# A battery of internal functions, taken out to reduce the length of the main function.

resetValues <- function(collected, coords=NULL) 
{
    if (is.null(coords)) { 
        coords <- collected$coords 
    } else { 
        collected$coords <- coords 
    }
    collected$groups <- list()
    collected$boundaries <- list()
}

#' @importFrom shiny renderPlot
#' @importFrom graphics plot points lines polygon 
#' @importFrom grDevices rgb
generatePlot1 <- function(output, collected, ...) {
    output$plot1 <- renderPlot({
        x <- collected$coords$x
        y <- collected$coords$y
        plot(x, y, ...)

        # Plotting the lasso.
        if (!is.null(collected$lasso)) {
            points(collected$lasso$coord[,1], collected$lasso$coord[,2], col="dodgerblue", pch=16)
            lines(collected$lasso$coord[,1], collected$lasso$coord[,2], col="dodgerblue", lwd=2)
            if (collected$lasso$closed) {
                polygon(collected$lasso$coord[,1], collected$lasso$coord[,2], 
                    col=rgb(100, 100, 255, 255*.brushFillOpacity, maxColorValue=255), border=NA)
            }
        }
    })
}

#' @importFrom shiny renderPlot
#' @importFrom graphics plot text polygon
#' @importFrom grDevices grey.colors
generatePlot2 <- function(output, collected, ...) {
    output$plot2 <- renderPlot({
        x <- collected$coords$x
        y <- collected$coords$y
        plot(x, y, type="n", ...)

        n <- length(collected$boundaries)
        shading <- rev(grey.colors(n))
        for (i in seq_len(n)) {
            current <- collected$boundaries[[i]]
            polygon(current[,1], current[,2], col=shading[i], border="black", lwd=0.5)
            middle <- colMeans(current)
            text(middle[1], middle[2], i, cex=1.5, col="black")
        }
    })
}
