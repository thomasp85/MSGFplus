#' A simple GUI to create msgfPar objects
#' 
#' This function presents the user with a GUI where the different parameters can
#' be filled out interactively. When the window appears the different values
#' already present reflects the default values for MS-GF+ so leaving them as is
#' equals to not setting them in advance.
#' 
#' NOTE: This functions requires gWidgets and checks for the existance
#' beforehand. MSGFplus does not import gWidgets and gWidgets does thus not
#' necessarily exist on your system. In addition at least one of the gWidgetsXXX
#' packages are needed.
#' 
#' @return A msgfPar object with parameters set according to the final state of 
#' the GUI
#' 
#' @examples
#' \dontrun{
#' parameters <- msgfParGUI()
#' }
#' 
#' @seealso \code{\link{msgfPar-class}}
#' 
#' @export
#' 
msgfParGUI <- function(){
    if (!requireNamespace("gWidgets", quietly = TRUE)) {
        stop('The "gWidgets" package is needed for this functionality to work', call. = FALSE)
    }
    msgfParameter <- msgfPar()
    waiting <- TRUE
    
    parameters <- list()
    window <- gWidgets::gwindow('Specify MSGF+ parameters', visible=FALSE)
    topGrid <- gWidgets::glayout(container=window, spacing=5, expand=TRUE)
    
    topGrid[1,1, anchor=c(1,0)] <- 'Database:'
    database <- gWidgets::gfilebrowse(text='Path to database file...', type='open', filter=list("All files" = list(patterns = c("*")), "Database files" =list(patterns = c("*.fa","*.fasta"))), container=topGrid)
    topGrid[1,2, anchor=c(-1,0)] <- database
    parameters$database <- database
    
    topGrid[2,1, anchor=c(1,0)] <- 'Tolerance:'
    tolGroup <- gWidgets::ggroup(horizontal=TRUE, container=topGrid, spacing=0)
    tolValue <- gWidgets::gedit('40', container=tolGroup)
    gWidgets::size(tolValue)[1] <- 40
    tolUnit <- gWidgets::gcombobox(c('ppm', 'Da'), container=tolGroup)
    topGrid[2,2, anchor=c(-1,0)] <- tolGroup
    parameters$tolerance <- list(tolValue, tolUnit)
    
    topGrid[3,1, anchor=c(1,0)] <- 'Isotope error range:'
    isoGroup <- gWidgets::ggroup(horizontal=TRUE, container=topGrid, spacing=0)
    isoLow <- gWidgets::gspinbutton(from=-5, to=10, by=1, container=isoGroup)
    gWidgets::svalue(isoLow) <- 0
    gWidgets::size(isoLow)[1] <- 40
    gWidgets::glabel(' - ', container=isoGroup)
    isoHigh <- gWidgets::gspinbutton(from=-5, to=10, by=1, container=isoGroup)
    gWidgets::svalue(isoHigh) <- 1
    gWidgets::size(isoHigh)[1] <- 40
    topGrid[3,2, anchor=c(-1,0)] <- isoGroup
    parameters$isotopeError <- list(isoLow, isoHigh)
    
    topGrid[4,1, anchor=c(1,0)] <- 'Target-Decoy:'
    tdaCheck <- gWidgets::gcheckbox(checked=TRUE, container=topGrid)
    topGrid[4,2, anchor=c(-1,0)] <- tdaCheck
    parameters$tda <- tdaCheck
    
    topGrid[5,1, anchor=c(1,0)] <- 'Fragmentation method:'
    fragMethod <- gWidgets::gcombobox(c('From spectrum', 'CID', 'ETD', 'HCD', 'Merge'), container=topGrid)
    topGrid[5,2, anchor=c(-1,0)] <- fragMethod
    parameters$fragmentation <- fragMethod
    
    topGrid[6,1, anchor=c(1,0)] <- 'Instrument:'
    instrument <- gWidgets::gcombobox(c('Low-res LCQ/LTQ', 'High-res LTQ', 'TOF', 'Q-Exactive'), container=topGrid)
    topGrid[6,2, anchor=c(-1,0)] <- instrument
    parameters$instrument <- instrument
    
    topGrid[7,1, anchor=c(1,0)] <- 'Enzyme:'
    enzyme <- gWidgets::gcombobox(c('Unspecific', 'Trypsin', 'Chymotrypsin', 'Lys-C', 'Lys-N', 'Glu-C', 'Arg-C', 'Asp-N', 'alphaLP', 'None'), container=topGrid)
    gWidgets::svalue(enzyme) <- 'Trypsin'
    topGrid[7,2, anchor=c(-1,0)] <- enzyme
    parameters$enzyme <- enzyme
    
    topGrid[8,1, anchor=c(1,0)] <- 'Protocol:'
    protocol <- gWidgets::gcombobox(c('None', 'Phosphorylation', 'iTRAQ', 'iTRAQPhospho'), container=topGrid)
    topGrid[8,2, anchor=c(-1,0)] <- protocol
    parameters$protocol <- protocol
    
    topGrid[9,1, anchor=c(1,0)] <- 'Tolerable termini:'
    tolTerm <- gWidgets::gspinbutton(from=0, to=2, by=1, container=topGrid)
    gWidgets::svalue(tolTerm) <- 2
    gWidgets::size(tolTerm)[1] <- 40
    topGrid[9,2, anchor=c(-1,0)] <- tolTerm
    parameters$tolerableTerm <- tolTerm
    
    topGrid[10,1, anchor=c(1,0)] <- 'Peptide length:'
    lengthGroup <- gWidgets::ggroup(horizontal=TRUE, container=topGrid, spacing=0)
    lengthLow <- gWidgets::gspinbutton(from=1, to=100, by=1, container=lengthGroup)
    gWidgets::svalue(lengthLow) <- 6
    gWidgets::size(lengthLow)[1] <- 40
    gWidgets::glabel(' - ', container=lengthGroup)
    lengthHigh <- gWidgets::gspinbutton(from=1, to=100, by=1, container=lengthGroup)
    gWidgets::svalue(lengthHigh) <- 40
    gWidgets::size(lengthHigh)[1] <- 40
    topGrid[10,2, anchor=c(-1,0)] <- lengthGroup
    parameters$length <- list(lengthLow, lengthHigh)
    
    topGrid[11,1, anchor=c(1,0)] <- 'Peptide charge:'
    chargeGroup <- gWidgets::ggroup(horizontal=TRUE, container=topGrid, spacing=0)
    chargeLow <- gWidgets::gspinbutton(from=1, to=10, by=1, container=chargeGroup)
    gWidgets::svalue(chargeLow) <- 2
    gWidgets::size(chargeLow)[1] <- 40
    gWidgets::glabel(' - ', container=chargeGroup)
    chargeHigh <- gWidgets::gspinbutton(from=1, to=10, by=1, container=chargeGroup)
    gWidgets::svalue(chargeHigh) <- 3
    gWidgets::size(chargeHigh)[1] <- 40
    topGrid[11,2, anchor=c(-1,0)] <- chargeGroup
    parameters$charge <- list(chargeLow, chargeHigh)
    
    topGrid[12,1, anchor=c(1,0)] <- 'Matches per spectrum:'
    matches <- gWidgets::gspinbutton(from=1, to=100, by=1, container=topGrid)
    gWidgets::svalue(matches) <- 1
    gWidgets::size(matches)[1] <- 40
    topGrid[12,2, anchor=c(-1,0)] <- matches
    parameters$matches <- matches
    
    topGrid[13,1, anchor=c(1,0)] <- 'Modifications pr peptide:'
    modNum <- gWidgets::gspinbutton(from=1, to=10, by=1, container=topGrid)
    gWidgets::svalue(modNum) <- 2
    gWidgets::size(modNum)[1] <- 40
    topGrid[13,2, anchor=c(-1,0)] <- modNum
    modButtonGroup <- gWidgets::ggroup(horizontal=FALSE, container=topGrid, spacing=0)
    addMod <- gWidgets::gbutton('Add', container=modButtonGroup)
    gWidgets::addSpace(modButtonGroup, 12, horizontal=FALSE)
    remMod <- gWidgets::gbutton('Remove', container=modButtonGroup)
    topGrid[14, 1, anchor=c(1,-1)] <- modButtonGroup
    modTable <- gWidgets::gtable(data.frame(name='Carbamidomethyl', composition='C2H3N1O1', mass='', residues='C', type='fixed', position='any', stringsAsFactors=FALSE), container=topGrid)
    topGrid[14, 2] <- modTable
    parameters$modifications <- list(modNum, modTable)
    
    okButtonGroup <- gWidgets::ggroup(horizontal=TRUE, container=topGrid)
    gWidgets::addSpring(okButtonGroup)
    okButton <- gWidgets::gbutton('ok', container=okButtonGroup)
    topGrid[15, 1:2, expand=TRUE] <- okButtonGroup
    
    gWidgets::addHandlerClicked(addMod, handler=function(h, ...){
        addWidgets <- list()
        addDialog <- gWidgets::gbasicdialog('Add modification type', handler=function(h, ...){
            adding <- lapply(addWidgets, gWidgets::svalue)
            modTable[] <- rbind(modTable[], data.frame(adding, stringsAsFactors=FALSE))
        })
        addDialogLayout <- gWidgets::glayout(container=addDialog, spacing=5)
        addDialogLayout[1,1, anchor=c(1,0)] <- 'Name:'
        addDialogLayout[1,2, anchor=c(-1,0)] <- addWidgets[['name']] <- gWidgets::gedit(container=addDialogLayout)
        addDialogLayout[2,1, anchor=c(1,0)] <- 'Composition:'
        addDialogLayout[2,2, anchor=c(-1,0)] <- addWidgets[['composition']] <- gWidgets::gedit(container=addDialogLayout)
        addDialogLayout[3,1, anchor=c(1,0)] <- 'Mass:'
        addDialogLayout[3,2, anchor=c(-1,0)] <- addWidgets[['mass']] <- gWidgets::gedit(container=addDialogLayout)
        addDialogLayout[4,1, anchor=c(1,0)] <- 'Residues:'
        addDialogLayout[4,2, anchor=c(-1,0)] <- addWidgets[['residues']] <- gWidgets::gedit(container=addDialogLayout)
        addDialogLayout[5,1, anchor=c(1,0)] <- 'Type:'
        addDialogLayout[5,2, anchor=c(-1,0)] <- addWidgets[['type']] <- gWidgets::gcombobox(c('fixed', 'optional'), container=addDialogLayout)
        addDialogLayout[6,1, anchor=c(1,0)] <- 'Position:'
        addDialogLayout[6,2, anchor=c(-1,0)] <- addWidgets[['position']] <- gWidgets::gcombobox(c('any', 'n-term', 'c-term', 'prot-n-term', 'prot-c-term'), container=addDialogLayout)
        gWidgets::visible(addDialog, set=TRUE)
    })
    
    gWidgets::addHandlerClicked(remMod, handler=function(h, ...){
        selected <- gWidgets::svalue(modTable, index=TRUE)
        if(!is.na(selected)){
            modTable[] <- modTable[-selected, , drop=FALSE]
        }
    })
    
    gWidgets::addHandlerClicked(okButton, handler=function(h, ...){
        parameters <- list()
        parameters$database <- gWidgets::svalue(h$action$database)
        parameters$tolerance <- msgfParTolerance(value=as.numeric(gWidgets::svalue(h$action$tolerance[[1]])), unit=gWidgets::svalue(h$action$tolerance[[2]]))
        parameters$isotopeError <- msgfParIsotopeError(c(gWidgets::svalue(h$action$isotopeError[[1]]),gWidgets::svalue(h$action$isotopeError[[2]])))
        parameters$tda <- msgfParTda(gWidgets::svalue(h$action$tda))
        parameters$fragmentation <- msgfParFragmentation(gWidgets::svalue(h$action$fragmentation, index=TRUE)-1)
        parameters$instrument <- msgfParInstrument(gWidgets::svalue(h$action$instrument, index=TRUE)-1)
        parameters$enzyme <- msgfParEnzyme(gWidgets::svalue(h$action$enzyme, index=TRUE)-1)
        parameters$protocol <- msgfParProtocol(gWidgets::svalue(h$action$protocol, index=TRUE)-1)
        parameters$ntt <- msgfParNtt(gWidgets::svalue(h$action$tolerableTerm))
        parameters$lengthRange <- msgfParLengthRange(c(gWidgets::svalue(h$action$length[[1]]), gWidgets::svalue(h$action$length[[2]])))
        parameters$chargeRange <- msgfParChargeRange(c(gWidgets::svalue(h$action$charge[[1]]), gWidgets::svalue(h$action$charge[[2]])))
        parameters$matches <- msgfParMatches(gWidgets::svalue(h$action$matches))
        modifications <- h$action$modifications[[2]][,,drop=FALSE]
        modifications$mass <- as.numeric(modifications$mass)
        modificationPars <- list()
        for(i in 1:nrow(modifications)) modificationPars[[i]] <- do.call('msgfParModification', modifications[i,])
        parameters$modification <- msgfParModificationList(gWidgets::svalue(h$action$modifications[[1]]), modificationPars)
        msgfParameter <<- do.call('msgfPar', parameters)
        waiting <<- FALSE
        gWidgets::dispose(window)
    }, action=parameters)
    
    gWidgets::addHandlerUnrealize(window, handler=function(h, ...){
        waiting <<- FALSE
    })
    gWidgets::visible(window) <- TRUE
    
    while(waiting){
        Sys.sleep(0.05)
    }
    
    return(msgfParameter)
}
