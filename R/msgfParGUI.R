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
    if (require(gWidgets)) {
        msgfParameter <- msgfPar()
        waiting <- TRUE
        
        parameters <- list()
        window <- gwindow('Specify MSGF+ parameters', visible=FALSE)
        topGrid <- glayout(container=window, spacing=5, expand=TRUE)
        
        topGrid[1,1, anchor=c(1,0)] <- 'Database:'
        database <- gfilebrowse(text='Path to database file...', type='open', filter=list("All files" = list(patterns = c("*")), "Database files" =list(patterns = c("*.fa","*.fasta"))), container=topGrid)
        topGrid[1,2, anchor=c(-1,0)] <- database
        parameters$database <- database
        
        topGrid[2,1, anchor=c(1,0)] <- 'Tolerance:'
        tolGroup <- ggroup(horizontal=TRUE, container=topGrid, spacing=0)
        tolValue <- gedit('40', container=tolGroup)
        size(tolValue)[1] <- 40
        tolUnit <- gcombobox(c('ppm', 'Da'), container=tolGroup)
        topGrid[2,2, anchor=c(-1,0)] <- tolGroup
        parameters$tolerance <- list(tolValue, tolUnit)
        
        topGrid[3,1, anchor=c(1,0)] <- 'Isotope error range:'
        isoGroup <- ggroup(horizontal=TRUE, container=topGrid, spacing=0)
        isoLow <- gspinbutton(from=-5, to=10, by=1, container=isoGroup)
        svalue(isoLow) <- 0
        size(isoLow)[1] <- 40
        glabel(' - ', container=isoGroup)
        isoHigh <- gspinbutton(from=-5, to=10, by=1, container=isoGroup)
        svalue(isoHigh) <- 1
        size(isoHigh)[1] <- 40
        topGrid[3,2, anchor=c(-1,0)] <- isoGroup
        parameters$isotopeError <- list(isoLow, isoHigh)
        
        topGrid[4,1, anchor=c(1,0)] <- 'Target-Decoy:'
        tdaCheck <- gcheckbox(checked=TRUE, container=topGrid)
        topGrid[4,2, anchor=c(-1,0)] <- tdaCheck
        parameters$tda <- tdaCheck
        
        topGrid[5,1, anchor=c(1,0)] <- 'Fragmentation method:'
        fragMethod <- gcombobox(c('From spectrum', 'CID', 'ETD', 'HCD', 'Merge'), container=topGrid)
        topGrid[5,2, anchor=c(-1,0)] <- fragMethod
        parameters$fragmentation <- fragMethod
        
        topGrid[6,1, anchor=c(1,0)] <- 'Instrument:'
        instrument <- gcombobox(c('Low-res LCQ/LTQ', 'High-res LTQ', 'TOF', 'Q-Exactive'), container=topGrid)
        topGrid[6,2, anchor=c(-1,0)] <- instrument
        parameters$instrument <- instrument
        
        topGrid[7,1, anchor=c(1,0)] <- 'Enzyme:'
        enzyme <- gcombobox(c('Unspecific', 'Trypsin', 'Chymotrypsin', 'Lys-C', 'Lys-N', 'Glu-C', 'Arg-C', 'Asp-N', 'alphaLP', 'None'), container=topGrid)
        svalue(enzyme) <- 'Trypsin'
        topGrid[7,2, anchor=c(-1,0)] <- enzyme
        parameters$enzyme <- enzyme
        
        topGrid[8,1, anchor=c(1,0)] <- 'Protocol:'
        protocol <- gcombobox(c('None', 'Phosphorylation', 'iTRAQ', 'iTRAQPhospho'), container=topGrid)
        topGrid[8,2, anchor=c(-1,0)] <- protocol
        parameters$protocol <- protocol
        
        topGrid[9,1, anchor=c(1,0)] <- 'Tolerable termini:'
        tolTerm <- gspinbutton(from=0, to=2, by=1, container=topGrid)
        svalue(tolTerm) <- 2
        size(tolTerm)[1] <- 40
        topGrid[9,2, anchor=c(-1,0)] <- tolTerm
        parameters$tolerableTerm <- tolTerm
        
        topGrid[10,1, anchor=c(1,0)] <- 'Peptide length:'
        lengthGroup <- ggroup(horizontal=TRUE, container=topGrid, spacing=0)
        lengthLow <- gspinbutton(from=1, to=100, by=1, container=lengthGroup)
        svalue(lengthLow) <- 6
        size(lengthLow)[1] <- 40
        glabel(' - ', container=lengthGroup)
        lengthHigh <- gspinbutton(from=1, to=100, by=1, container=lengthGroup)
        svalue(lengthHigh) <- 40
        size(lengthHigh)[1] <- 40
        topGrid[10,2, anchor=c(-1,0)] <- lengthGroup
        parameters$length <- list(lengthLow, lengthHigh)
        
        topGrid[11,1, anchor=c(1,0)] <- 'Peptide charge:'
        chargeGroup <- ggroup(horizontal=TRUE, container=topGrid, spacing=0)
        chargeLow <- gspinbutton(from=1, to=10, by=1, container=chargeGroup)
        svalue(chargeLow) <- 2
        size(chargeLow)[1] <- 40
        glabel(' - ', container=chargeGroup)
        chargeHigh <- gspinbutton(from=1, to=10, by=1, container=chargeGroup)
        svalue(chargeHigh) <- 3
        size(chargeHigh)[1] <- 40
        topGrid[11,2, anchor=c(-1,0)] <- chargeGroup
        parameters$charge <- list(chargeLow, chargeHigh)
        
        topGrid[12,1, anchor=c(1,0)] <- 'Matches per spectrum:'
        matches <- gspinbutton(from=1, to=100, by=1, container=topGrid)
        svalue(matches) <- 1
        size(matches)[1] <- 40
        topGrid[12,2, anchor=c(-1,0)] <- matches
        parameters$matches <- matches
        
        topGrid[13,1, anchor=c(1,0)] <- 'Modifications pr peptide:'
        modNum <- gspinbutton(from=1, to=10, by=1, container=topGrid)
        svalue(modNum) <- 2
        size(modNum)[1] <- 40
        topGrid[13,2, anchor=c(-1,0)] <- modNum
        modButtonGroup <- ggroup(horizontal=FALSE, container=topGrid, spacing=0)
        addMod <- gbutton('Add', container=modButtonGroup)
        addSpace(modButtonGroup, 12, horizontal=FALSE)
        remMod <- gbutton('Remove', container=modButtonGroup)
        topGrid[14, 1, anchor=c(1,-1)] <- modButtonGroup
        modTable <- gtable(data.frame(name='Carbamidomethyl', composition='C2H3N1O1', mass='', residues='C', type='fixed', position='any', stringsAsFactors=FALSE), container=topGrid)
        topGrid[14, 2] <- modTable
        parameters$modifications <- list(modNum, modTable)
        
        okButtonGroup <- ggroup(horizontal=TRUE, container=topGrid)
        addSpring(okButtonGroup)
        okButton <- gbutton('ok', container=okButtonGroup)
        topGrid[15, 1:2, expand=TRUE] <- okButtonGroup
        
        addHandlerClicked(addMod, handler=function(h, ...){
            addDialog <- gbasicdialog('Add modification type', handler=function(h, ...){
                adding <- lapply(addWidgets, svalue)
                modTable[] <- rbind(modTable[], data.frame(adding, stringsAsFactors=FALSE))
            })
            addDialogLayout <- glayout(container=addDialog, spacing=5)
            addWidgets <- list()
            addDialogLayout[1,1, anchor=c(1,0)] <- 'Name:'
            addDialogLayout[1,2, anchor=c(-1,0)] <- addWidgets[['name']] <- gedit(container=addDialogLayout)
            addDialogLayout[2,1, anchor=c(1,0)] <- 'Composition:'
            addDialogLayout[2,2, anchor=c(-1,0)] <- addWidgets[['composition']] <- gedit(container=addDialogLayout)
            addDialogLayout[3,1, anchor=c(1,0)] <- 'Mass:'
            addDialogLayout[3,2, anchor=c(-1,0)] <- addWidgets[['mass']] <- gedit(container=addDialogLayout)
            addDialogLayout[4,1, anchor=c(1,0)] <- 'Residues:'
            addDialogLayout[4,2, anchor=c(-1,0)] <- addWidgets[['residues']] <- gedit(container=addDialogLayout)
            addDialogLayout[5,1, anchor=c(1,0)] <- 'Type:'
            addDialogLayout[5,2, anchor=c(-1,0)] <- addWidgets[['type']] <- gcombobox(c('fixed', 'optional'), container=addDialogLayout)
            addDialogLayout[6,1, anchor=c(1,0)] <- 'Position:'
            addDialogLayout[6,2, anchor=c(-1,0)] <- addWidgets[['position']] <- gcombobox(c('any', 'n-term', 'c-term', 'prot-n-term', 'prot-c-term'), container=addDialogLayout)
            visible(addDialog, set=TRUE)
        })
        
        addHandlerClicked(remMod, handler=function(h, ...){
            selected <- svalue(modTable, index=TRUE)
            if(!is.na(selected)){
                modTable[] <- modTable[-selected, , drop=FALSE]
            }
        })
        
        addHandlerClicked(okButton, handler=function(h, ...){
            parameters <- list()
            parameters$database <- svalue(h$action$database)
            parameters$tolerance <- msgfParTolerance(value=as.numeric(svalue(h$action$tolerance[[1]])), unit=svalue(h$action$tolerance[[2]]))
            parameters$isotopeError <- msgfParIsotopeError(c(svalue(h$action$isotopeError[[1]]),svalue(h$action$isotopeError[[2]])))
            parameters$tda <- msgfParTda(svalue(h$action$tda))
            parameters$fragmentation <- msgfParFragmentation(svalue(h$action$fragmentation, index=TRUE)-1)
            parameters$instrument <- msgfParInstrument(svalue(h$action$instrument, index=TRUE)-1)
            parameters$enzyme <- msgfParEnzyme(svalue(h$action$enzyme, index=TRUE)-1)
            parameters$protocol <- msgfParProtocol(svalue(h$action$protocol, index=TRUE)-1)
            parameters$ntt <- msgfParNtt(svalue(h$action$tolerableTerm))
            parameters$lengthRange <- msgfParLengthRange(c(svalue(h$action$length[[1]]), svalue(h$action$length[[2]])))
            parameters$chargeRange <- msgfParChargeRange(c(svalue(h$action$charge[[1]]), svalue(h$action$charge[[2]])))
            parameters$matches <- msgfParMatches(svalue(h$action$matches))
            modifications <- h$action$modifications[[2]][,,drop=FALSE]
            modifications$mass <- as.numeric(modifications$mass)
            modificationPars <- list()
            for(i in 1:nrow(modifications)) modificationPars[[i]] <- do.call('msgfParModification', modifications[i,])
            parameters$modification <- msgfParModificationList(svalue(h$action$modifications[[1]]), modificationPars)
            msgfParameter <<- do.call('msgfPar', parameters)
            waiting <<- FALSE
            dispose(window)
        }, action=parameters)
        
        addHandlerUnrealize(window, handler=function(h, ...){
            waiting <<- FALSE
        })
        visible(window) <- TRUE
        
        while(waiting){
            Sys.sleep(0.05)
        }
        
        return(msgfParameter)
    } else {
        stop('gWidgets needs to be installed. Run install.package(\"gWidgets\") for this function to work')
    }
}
