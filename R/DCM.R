#' @title Data Converter Module
#'
#' @author Delphi Intelligence
#'
#' @description Data Converter Module (DCM) converts the dataset format from split into stack and to the reverse.
#'
#' You can learn more about this package at:
#' http://www.uv.mx/personal/nehuerta/dcm/
#'
#' @details
#' The module has two functionalities STACK and SPLIT.
#'
#' STACK
#'
#' In this mode the package receives a matrix where the rows are typically the judges or customers that evaluate a product. The first column is the product code, the second column is the ID of the judge and the rest of the columns contain the data of the sensory attributes. The program allows replicated data.
#'
#' The output of the program is a matrix of stacked data; the first column gives the product code, the second column the ID of the judge, the third column the name of the attribute and the fourth column the value provided by the judge. This matrix is given in a csv file.
#'
#' SPLIT
#'
#' The input for this functionality is a matrix that has four columns: Product code, ID of a judge, attribute name and data.
#'
#' The output is a matrix where the rows correspond to product code-judge combination and the columns provide the sensory characteristics ordered alphabetically or not. This matrix is produced in a csv file.
#'
#' In both cases stack and split, empty spaces are kept that way.
#'
#' @return DCM is an graphic interface
#' @examples \dontrun{
#' ##Install package
#' library(DCM)
#' ##Call the package
#' DCM()
#' }
#'
#' @export DCM
#' @import graphics
#' @import grDevices
#' @import utils
#' @import tcltk
#' @import gWidgets
#' @import readxl
#' @import pander
#' @importFrom utils write.csv
#'
DCM<-function(){

  ## Environment
  env<- new.env()

  options("guiToolkit"="tcltk")

  ##Principal screen
  w<- gwindow("DCM",visible=FALSE,width = 800,height= 450)
  g<- ggroup(horizontal=FALSE, spacing=0, container = w)

  #Screens
  nb <- gnotebook(container=g,width = 800,height= 450)
  g1<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "DCM")
  g2<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "Stack")
  g3<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "Split")

  #Variables in the environment
  assign("gdata",NULL, envir =env)
  assign("gdata1",NULL, envir =env)
  assign("gdata2",NULL, envir =env)
  assign("p",0, envir =env)

  ##File menu
  #Open xlsx
  openex<-function(h,...){
    data<-tk_choose.files()
    xlsx<-read_excel(data,sheet = 1, col_names=TRUE)
    data2<-as.data.frame(xlsx)
    assign("gdata",data2, envir =env)
  }

  ##View
  view<-function(h,...){
    gdata<-get("gdata",envir =env)
    fix(gdata)
  }

  ##View1
  view1<-function(h,...){
    gdata1<-get("gdata1",envir =env)
    fix(gdata1)
  }

  ##View2
  view2<-function(h,...){
    gdata2<-get("gdata2",envir =env)
    fix(gdata2)
  }

  ##Re-start
  start<-function(h,...){
    dispose(w)
    DCM()
  }

  ##Close
  close<-function(h,...){
    dispose(w)
  }

  ##Save
  save<-function(h,...){
    gdata1<-get("gdata1",envir =env)
    gdata2<-get("gdata2",envir =env)
    p<-get("p",envir =env)

    if(p==1){
      filename <- tclvalue(tkgetSaveFile())
      nam<-paste0(filename,".csv")
      write.csv(gdata1,nam)
    }
    if (p==2) {
      filename <- tclvalue(tkgetSaveFile())
      nam<-paste0(filename,".csv")
      write.csv(gdata2,nam)
    }
    if (p==0){
      gmessage("Please use some method",title="Warning")
    }
  }

  ##Save1
  save1<-function(h,...){
    gdata1<-get("gdata1",envir =env)
    filename <- tclvalue(tkgetSaveFile())
    nam<-paste0(filename,".csv")
    write.csv(gdata1,nam)
  }

  ##Save2
  save2<-function(h,...){
    gdata2<-get("gdata2",envir =env)
    filename <- tclvalue(tkgetSaveFile())
    nam<-paste0(filename,".csv")
    write.csv(gdata2,nam)
  }

  #Stacked data
  stacked<-function(h,...){
    gdata<-get("gdata",envir =env)
    p<-get("p",envir =env)

    fromDataToStack <- function(gdata){
      nameColumns <- c(colnames(gdata)[1], "Label", "Data")

      dataStacked <- data.frame(stringsAsFactors=FALSE)
      for (row in 1:nrow(gdata) ){
        for (col in 2:ncol(gdata) ){
          newRow <- c(gdata[row,1], colnames(gdata)[col], gdata[row, col])
          dataStacked = rbind(dataStacked, data.frame(t(newRow)))
        }
      }
      colnames(dataStacked) <- nameColumns
      dataStacked[is.na(dataStacked)] <- ""
      text<-print("Information structure",quote=FALSE)
      Z<-pandoc.table(head(dataStacked),plain.ascii = FALSE,justify = c('center', 'left', 'center'))
      newList <- list("des"=text,"names" = Z)
      assign("gdata1",dataStacked, envir =env)
      assign("p",1, envir =env)
    }

    tbl<-glayout(container=g2)
    gseparator(horizontal=TRUE, container=g2)
    outputArea<- gtext(container=g2, expand=TRUE,width = 780,height= 410)
    gr1<- ggroup(horizontal=TRUE, spacing=0, container = g2)
    b1<-gbutton("Save data",container=gr1,handler=save1)
    b2<-gbutton("View data",container=gr1,handler=view1)
    out <- capture.output(fromDataToStack(gdata))
    dispose(outputArea)
    if(length(out)>0)
      add(outputArea, out)
  }
  #Split ordered
  split<-function(h,...){
    gdata<-get("gdata",envir =env)
    p<-get("p",envir =env)
    datasource<-gdata

    fromStackToData <- function(datasource){
      nameColumns <- c()

      dataStacked <- data.frame()

      previousProduct <- datasource[1,1]
      currentProduct <- datasource[1,1]

      previousJuge <- datasource[1,2]
      currentJuge <- datasource[1,2]

      blockCode <- data.frame(t(c(currentProduct, currentJuge)))
      nameColumns <- cbind(nameColumns, colnames(datasource)[1])
      nameColumns <- cbind(nameColumns, colnames(datasource)[2])
      isSetHeaders <- FALSE

      for (row in 1:nrow(datasource)){
        currentProduct <- datasource[row, 1]
        if (currentProduct != previousProduct){
          dataStacked <- rbind(dataStacked, data.frame(blockCode))
          currentJuge <- datasource[row, 2]
          previousJuge <- currentJuge
          blockCode <- data.frame(t(c(currentProduct, currentJuge)))
          blockCode <- cbind(blockCode, datasource[row, ncol(datasource)])
          previousProduct <- currentProduct
        }else{
          currentJuge <- datasource[row, 2]
          if (currentJuge != previousJuge){
            dataStacked <- rbind(dataStacked, data.frame(blockCode))
            blockCode <- data.frame(t(c(currentProduct, currentJuge)))
            blockCode <- cbind(blockCode, datasource[row, ncol(datasource)])
            previousJuge <- currentJuge
            if (isSetHeaders == FALSE){
              isSetHeaders <- TRUE
            }
          }else{
            blockCode <- cbind(blockCode, datasource[row, ncol(datasource)])

            if (isSetHeaders == FALSE){
              nameColumns <- cbind(nameColumns, datasource[row, 3])
            }
          }

        }
      }
      dataStacked <- rbind(dataStacked, data.frame(blockCode))
      colnames(dataStacked) <- nameColumns
      dataStacked[is.na(dataStacked)] <- ""
      gdata2<-dataStacked
      text<-print("Information structure",quote=FALSE)
      Z<-pandoc.table(head(gdata2),plain.ascii = FALSE)
      newList <- list("des"=text,"names" = Z)
      assign("gdata2",gdata2, envir =env)
      assign("p",2, envir =env)
    }

    tbl<-glayout(container=g3)
    gseparator(horizontal=TRUE, container=g3)
    outputArea<- gtext(container=g3, expand=TRUE,width = 780,height= 410)
    gr1<- ggroup(horizontal=TRUE, spacing=0, container = g3)
    b1<-gbutton("Save data",container=gr1,handler=save2)
    b2<-gbutton("View data",container=gr1,handler=view2)
    out <- capture.output(fromStackToData(gdata))
    dispose(outputArea)
    if(length(out)>0)
      add(outputArea, out)
  }

  #Split
  split1<-function(h,...){
    gdata<-get("gdata",envir =env)
    p<-get("p",envir =env)
    datasource<-gdata

    fromStackToData <- function(datasource){
      nameColumns <- c()

      dataStacked <- data.frame()

      previousProduct <- datasource[1,1]
      currentProduct <- datasource[1,1]

      previousJuge <- datasource[1,2]
      currentJuge <- datasource[1,2]

      blockCode <- data.frame(t(c(currentProduct, currentJuge)))
      nameColumns <- cbind(nameColumns, colnames(datasource)[1])
      nameColumns <- cbind(nameColumns, colnames(datasource)[2])
      isSetHeaders <- FALSE

      for (row in 1:nrow(datasource)){
        currentProduct <- datasource[row, 1]
        if (currentProduct != previousProduct){
          dataStacked <- rbind(dataStacked, data.frame(blockCode))
          currentJuge <- datasource[row, 2]
          previousJuge <- currentJuge
          blockCode <- data.frame(t(c(currentProduct, currentJuge)))
          blockCode <- cbind(blockCode, datasource[row, ncol(datasource)])
          previousProduct <- currentProduct
        }else{
          currentJuge <- datasource[row, 2]
          if (currentJuge != previousJuge){
            dataStacked <- rbind(dataStacked, data.frame(blockCode))
            blockCode <- data.frame(t(c(currentProduct, currentJuge)))
            blockCode <- cbind(blockCode, datasource[row, ncol(datasource)])
            previousJuge <- currentJuge
            if (isSetHeaders == FALSE){
              isSetHeaders <- TRUE
            }
          }else{
            blockCode <- cbind(blockCode, datasource[row, ncol(datasource)])

            if (isSetHeaders == FALSE){
              nameColumns <- cbind(nameColumns, datasource[row, 3])
            }
          }

        }
      }
      dataStacked <- rbind(dataStacked, data.frame(blockCode))
      colnames(dataStacked) <- nameColumns
      dataStacked[is.na(dataStacked)] <- ""
      gdata<-dataStacked[c(-1,-2)]
      gdata1<-gdata[,order(colnames(gdata))]
      gdata2<-cbind(dataStacked[c(1,2)],gdata1)
      text<-print("Information structure",quote=FALSE)
      Z<-pandoc.table(head(gdata2),plain.ascii = FALSE)
      newList <- list("des"=text,"names" = Z)
      assign("gdata2",gdata2, envir =env)
      assign("p",2, envir =env)
    }

    tbl<-glayout(container=g3)
    gseparator(horizontal=TRUE, container=g3)
    outputArea<- gtext(container=g3, expand=TRUE,width = 780,height= 410)
    gr1<- ggroup(horizontal=TRUE, spacing=0, container = g3)
    b1<-gbutton("Save data",container=gr1,handler=save2)
    b2<-gbutton("View data",container=gr1,handler=view2)
    out <- capture.output(fromStackToData(gdata))
    dispose(outputArea)
    if(length(out)>0)
      add(outputArea, out)
  }

  #MENUS
  menulistA<-list(u1=gaction("Open",handler=openex),u2=gaction("View",handler=view),u3=gaction("Refresh",handler=start),u4=gaction("Save",handler=save),u5=gaction("Close",handler=close))
  S<-list(u0=gaction("Split Ordered",handler=split1),u1=gaction("Split",handler=split))
  menulistZ<-list(u0=gaction("Stack",handler=stacked),Split=S)

  ##Help menu
  #Manual
  y1<- function(h,..) gmessage("http://www.uv.mx/personal/nehuerta/dcm/",title="Link")
  menulistY<-list(u0=gaction("Information",handler=y1))

  ##Principal menu
  mb_list <-list(File=menulistA,Method=menulistZ,Help=menulistY)
  gmenu(mb_list, container=g)

  #Information
  tmp1 <- gframe("", container=g1, expand=TRUE,horizontal=FALSE)
  img<-gimage("R/logo/DICS.gif",container=tmp1)
  visible(w) <- TRUE
}
