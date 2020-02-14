
# import travel volume reports from:
#  https://travel.trade.gov/outreachpages/inbound.general_information.inbound_overview.asp


if(!require('tm')) install.packages('tm'); library(tm)
if(!require('readxl')) install.packages('readxl'); library(readxl)
if(!require('readr')) install.packages('readr'); library(readr)
if(!require('rvest')) install.packages('rvest'); library(rvest)
if(!require('reshape2')) install.packages('reshape2'); library(reshape2)
if(!require('dplyr')) install.packages('dplyr'); library(dplyr)
if(!require('tibble')) install.packages('tibble'); library(tibble)
if(!require('countrycode')) install.packages('countrycode'); library(countrycode)

# Country Codes
source("source/ISO_code_source.R")

files <- list.files(pattern = "pdf$")

year.range <- c(2001,2019)

dir.create(file.path("data","travel_reports"))


# Annual regional reports of travel to the U.S. (pdf reports) -------------
# --> Download and save the annual regional reports of travel to the U.S. (pdf reports)
# http://tinet.ita.doc.gov/view/f-2002-201-001/top50markets.csv

download_pdfs <- FALSE
download_pdfs <- TRUE

if (download_pdfs) {

    #install.packages("pdftools")
    library(pdftools)
    library(XML)
    
    year <- 2007:2015
    region <- c("Africa", "Asia", "Caribbean", "Europe", "Middle_East", "Nordic", "South_America", 
                "Oceania", "Central_America", "Eastern_Europe", "Western_Europe")
    
    for (y in year){
        for (reg.tmp in region){
            if (y==2015 & reg.tmp=='Asia') reg.tmp <- 'ASIA'
            
            try.test <- try(download.file(paste0("https://travel.trade.gov/outreachpages/download_data_table/", y, "_", reg.tmp, "_Market_Profile.pdf"), 
                              file.path("data","travel_reports",paste0(reg.tmp, "_", y, ".pdf")), mode = "wb"), TRUE)
            if (class(try.test)=="try-error") next
        }
    }
    
    
    
    txt <- pdf_text(file.path("data","travel_reports",paste0(reg.tmp, "_", y, ".pdf")))
    # first page text
    cat(txt[1])
    # second page text
    cat(txt[2])
    
}





# # Top 50 Markets ----------------------------------------------------------
# download_top50 <- F
# 
# if (download_top50) {
#     year <- year.range[1]:year.range[2]
#     for (y in year){
#         try.test <- try(download.file(paste0("http://tinet.ita.doc.gov/view/f-", y, "-201-001/top50markets.csv"), 
#                                       paste0("data/Travel_ITA/Travel_Top50_",y), mode = "wb"), TRUE)
#             if (class(try.test)=="try-error") next
#     }
# }
# 





# International Visitors (Table C) ----------------------------------------
# https://travel.trade.gov/view/m-2002-I-001/table_c.csv

download_visitors <- T  # No need to re-download unless including new years 
dir.create("data/Travel_ITA")
if (download_visitors){
    
    region.table.all <- data.frame(NULL)
    year <- year.range[1]:year.range[2]
    for (y in year){
        
        if (y<2004){
            if(y<2001){
                next
                # # http://tinet.ita.doc.gov/view/m-2000-I-001/tcy1200.rpt_data_file.csv
                # try.test <- try({download.file(paste0("http://tinet.ita.doc.gov/view/m-", y, "-I-001/tcy1200.rpt_data_file.csv"),
                #                                paste0("data/Travel_ITA/Inbound_Visitors_",y, ".csv"), mode = "wb")
                #     region.table <- read.csv(paste0("http://tinet.ita.doc.gov/view/m-", y, "-I-001/tcy1200.rpt_data_file.csv"), stringsAsFactors=F)
                # }, TRUE)
                # # Clean it up
                # region.table <- region.table[-(1:4),]
                #         
        
            } else if (y>=2001) {
                try.test <- try({download.file(paste0("https://travel.trade.gov/view/m-", y, "-I-001/table_c.csv"),
                                               paste0("data/Travel_ITA/Inbound_Visitors_",y, ".csv"), mode = "wb")
                    region.table <- read.csv(paste0("https://travel.trade.gov/view/m-", y, "-I-001/table_c.csv"), stringsAsFactors=F)
                }, TRUE)
                
                # Clean it up
                region.table <- region.table[-(1:3),]
                region.table <- region.table[,-grep("%", as.character(region.table[3,]))] # Remove % columns
                
                # Fix split rows
                row.inds <- which(region.table[,1]=='MONTH/QUARTER')
                for (j in row.inds){
                    region.table[j, region.table[j+1,]!=""] <- paste0(region.table[j, region.table[j+1,]!=""], ".", region.table[j+1, region.table[j+1,]!=""])
                }
                
                # Get rid of unwanted columns
                row.names.tmp <- c("MONTH/QUARTER", toupper(month.name))
                region.table <- region.table[!is.na(match(region.table[,1], row.names.tmp)),]
                
                # Put all into columns
                n.temp.tables <- sum(region.table[,1]=="MONTH/QUARTER")
                t.row <- c(which(region.table[,1]=="MONTH/QUARTER"), nrow(region.table)+1)
                reg.tmp <- NULL
                for (t in 1:n.temp.tables){
                    tmp.tab <- region.table[t.row[t]:(t.row[t+1]-1),]
                    if (t==1) reg.tmp <- tmp.tab
                    else reg.tmp <- cbind(reg.tmp, tmp.tab[,-1])
                }
                region.table <- reg.tmp
                colnames(region.table) <- region.table[1,]
                region.table <-  region.table[-1,]
                region.table[,1] <- factor(region.table[,1], levels=toupper(month.name))
            }
            
        } else if (y>=2004){
            
            region.table <- NULL
            
            for (i in 1:6){
                tab_ind <- 2
                if (y<2006){
                    table.link <- paste0("https://travel.trade.gov/view/m-", y, "-I-001/table_", i, ".html")
                } else if (y>=2015) {
                    table.link <- paste0("https://travel.trade.gov/view/m-", y, "-I-001/table", i, ".asp")
                    tab_ind <- 3
                } else {
                    table.link <- paste0("https://travel.trade.gov/view/m-", y, "-I-001/table", i, ".html")
                }
                
                #Extract Table from HTML
                { webpage <- read_html(table.link)
                tbls <- html_nodes(webpage, "table")
                tbls_ls <- webpage %>%
                    html_nodes("table") %>%
                    .[tab_ind] %>%
                    html_table(fill = TRUE)
                tab.tmp <- tbls_ls[[1]] }
                
                if (y==2004){
                    if (i==1){
                        # Fix split rows
                        row.inds <- which(tab.tmp[,1]=='MONTH/QUARTER')
                        for (j in row.inds){
                            tab.tmp[j, tab.tmp[j+1,]!=""] <- paste0(tab.tmp[j, tab.tmp[j+1,]!=""], ".", tab.tmp[j+1, tab.tmp[j+1,]!=""])
                        }
                    } else {
                        tab.tmp <- tab.tmp[-1,]
                        tab.tmp <- tab.tmp[,!is.na(tab.tmp[1,])]
                        tab.tmp <- tab.tmp[,!tab.tmp[2,]=="% Change"]
                        tab.tmp[1,] <- toupper(tab.tmp[1,])
                    }
                    colnames(tab.tmp) <- tab.tmp[1,]
                }
                
                if (y>=2015){
                   # if (i==1){
                        # Fix split rows
                        row.inds <- which(tab.tmp[,1]=='MONTH/')
                        tab.tmp <- tab.tmp[, !colMeans(is.na(tab.tmp))>.75]
                        for (j in row.inds){
                            tab.tmp[j+1, tab.tmp[j,]!=""] <- paste0(tab.tmp[j, tab.tmp[j,]!=""], ".", tab.tmp[j+1, tab.tmp[j,]!=""])
                        }
                        tab.tmp[2,]       <- gsub("MONTH/.QUARTER", "MONTH/QUARTER", tab.tmp[2,]      )
                        colnames(tab.tmp) <- tab.tmp[2,]                  
                        tab.tmp <- tab.tmp[-2,]                                                 # Remove second Row
                   # }
                }    
                colnames(tab.tmp) <- gsub(" ", ".", colnames(tab.tmp))                  # Sub . for spaces in colnames
                tab.tmp <- tab.tmp[-1,]                                                 # Remove first Row
                tab.tmp <- tab.tmp[, colnames(tab.tmp)!=""]                             # Remove "% Change" columns
                tab.tmp <- tab.tmp[tab.tmp[,1]!="", ]                                   # Remove blank rows
                tab.tmp <- tab.tmp[!is.na(match(tab.tmp[,1], toupper(month.name))),]    # Restrict to months
                tab.tmp[,1] <- factor(tab.tmp[,1], levels=toupper(month.name))
                if (i==1){
                    region.table  <- tab.tmp
                } else {
                    region.table <- cbind(region.table, tab.tmp[,-1])
                }
            }
        }
        
        # Get "Non-Resident Arrivals to the United States: World Region of Residence" xls which has some of the regions that are dropped
        world.region.table <- NULL
        if (y>=2014){
            world.reg.file.path <- paste0("data/Travel_ITA/world_regions_",y, ".xls")
            try.test <- try({download.file(paste0("https://travel.trade.gov/view/m-", y, "-I-001/documents/world_regions.xls"),
                                           world.reg.file.path, mode = "wb")
            }, TRUE)
            
            # Get sheet names for reading
            sheet.names <- excel_sheets(world.reg.file.path)
            
            
            if (length(try.test)>0) {
                matches_ <- sapply(month.abb, grep, sheet.names)
                matched.months <- month.abb[lengths(matches_)>0]
                
                for (m in 1:length(matched.months)) {
                    world.region.table.tmp <- data.frame(read_excel(world.reg.file.path, sheet=m, skip=8, col_names=FALSE)[1:9,1:2], stringsAsFactors = FALSE)
                    if (m==1){
                        world.region.table <- data.frame(matrix(NA, nrow=12, ncol=nrow(world.region.table.tmp)), stringsAsFactors = FALSE)
                        reg.names.tmp <- world.region.table.tmp[,1]
                    }
                    world.region.table[m,] <- world.region.table.tmp[,2]
                    
                }
                colnames(world.region.table) <- reg.names.tmp
                row.names(world.region.table) <- month.abb
                colnames(world.region.table) <- gsub(" ", ".", colnames(world.region.table))
            }
        }
        
        colnames(region.table) <- toupper(colnames(region.table))
        colnames(region.table) <- gsub(" ", ".", colnames(region.table))
        colnames(region.table)[1] <- "MONTH"
        colnames(region.table) <- gsub(",", "", colnames(region.table))
        colnames(region.table) <- gsub("[*]", "", colnames(region.table))
        colnames(region.table) <- gsub("[(]", "", colnames(region.table))
        colnames(region.table) <- gsub("MEXICO.", "MEXICO", colnames(region.table))
        colnames(region.table)[grep("NETHER", colnames(region.table))] <- "NETHERLANDS"
        colnames(region.table)[grep("TAIWAN", colnames(region.table))] <- "TAIWAN"
        colnames(region.table)[grepl("HONG", colnames(region.table)) & 
                                   grepl("KONG", colnames(region.table)) &
                                   grepl("EXCL", colnames(region.table))] <- "CHINA"
        colnames(region.table)[grepl("HK", colnames(region.table)) & 
                                   grepl("EXCL", colnames(region.table))] <- "CHINA"
        colnames(region.table)[grepl("HONG", colnames(region.table)) & grepl("KONG", colnames(region.table)) & !grepl("EXCL", colnames(region.table))] <- "HONG.KONG"
        colnames(region.table)[grep("TOTAL.OVERSEA", colnames(region.table))] <- "TOTAL.OVERSEAS"
        
        # Fix changed column 
        if (sum(grepl("TOTAL.VISITATION", colnames(region.table)))>=1) {
            colnames(region.table)[grep("TOTAL.VISITATION", colnames(region.table))] <- "TOTAL.ARRIVALS"
        }
        
        region.table[region.table=="-"] <- NA
        
        region.table <- data.frame(YEAR=rep(y, 12), region.table) # Add YEAR column
        
        # ADD world.region.table data, if available
        if (!is.null(world.region.table)) {
            matched.columns <- sapply(tolower(colnames(world.region.table)), grep, tolower(colnames(region.table)))
            cols.to.add <- lengths(matched.columns)==0
            region.table <- data.frame(region.table, world.region.table[,cols.to.add])
        }
        
        # --- Merge with previous year --------------------------------------------------------------------
        if (y==2001) { region.table.all <- region.table
        } else if (y>2001) { region.table.all <- merge(region.table.all, region.table, all=TRUE) }
        
        #write.csv(region.table, paste0("data/Travel_ITA/Inbound_Visitors_",y, ".csv"), row.names=F)
    
    }
    region.table.all <- as.data.frame(region.table.all)
    
    
    
    save(region.table.all, file=paste0("data/Travel_ITA/Inbound_Visitors_", year.range[1], "-", year.range[2], ".RData"))
    
    
    tmp <- data.frame(apply(region.table.all, 2, as.character), stringsAsFactors=F)
    tmp[,-2] <- as.integer(gsub(",", "", as.matrix(tmp[,-2])))
    tmp <- data.frame(tmp, stringsAsFactors=F)
    inbound.travel <- tmp
    View(inbound.travel)
    rm(tmp, region.table, region.table.all)
    # Get Europe for full timeframe
    
    missing.euro <- is.na(inbound.travel$EUROPE)
    inbound.travel$EUROPE[missing.euro] <- inbound.travel$WESTERN.EUROPE[missing.euro] + inbound.travel$EASTERN.EUROPE[missing.euro]
    
    save(inbound.travel, file=paste0("data/Travel_ITA/Inbound_Visitors_", year.range[1], "-", year.range[2], ".RData"))
    write.csv(inbound.travel, paste0("data/Travel_ITA/Inbound_Visitors_", year.range[1], "-", year.range[2], ".csv"), row.names=F)
    
}
            


# ADD UPDATED 2017, 2018, 2019 -----------------------------------------------
# ** Some dat are not super up to date in those links used above, but they are updated in other links, though these are harder to find.
# ** This section manually finds those sources and completes the data as much as possible.


y <- 2018
year.range <- c(2001,y)
load(file=paste0("data/Travel_ITA/Inbound_Visitors_", year.range[1], "-", year.range[2], ".RData")) # inbound.travel


# Get updated 2017-2019 data
y <- 2019 
world.reg.file.path <- paste0("data/Travel_ITA/world_regions_",y, ".xlsx")
try.test <- try({download.file("https://travel.trade.gov/view/m-2017-I-001/documents/Final%20COR%20Volume.xlsx", # for some reason stored under 2017
                               world.reg.file.path, mode = "wb") }, TRUE)
sheet.names <- excel_sheets(world.reg.file.path)



# Get 2015-18 data
travelyrdata_all <- NULL
for (y_ in 2015:2019){
    sheet_ <- paste0(y_, " COR Month-QTR Vol")
    sheet_ <- sheet.names[grep(tolower(sheet_), tolower(sheet.names))]
    # world.region.table.tmp <- data.frame(read_xlsx(world.reg.file.path, sheet=sheet_, range="B3:Q72", 
    #                                                col_names=FALSE), stringsAsFactors = FALSE)
    world.region.table.tmp <- as.matrix(read_xlsx(world.reg.file.path, sheet=sheet_, #range="B3:Q72", 
                                                   col_names=FALSE))#, stringsAsFactors = FALSE)
    # Reduce to wanted data
    na_col_ <- which(apply(world.region.table.tmp, 2, FUN=function(x) all(is.na(x))))
    world.region.table.tmp <- world.region.table.tmp[,-c(1, na_col_[1]:ncol(world.region.table.tmp))] # reduce to just volume data
    #world.region.table.tmp <- world.region.table.tmp[-c(2,4,6,8,9,19,20),-c(5,9,13)]
    world.region.table.tmp <- as_tibble(world.region.table.tmp[-1,])
    print(as.character(world.region.table.tmp[1,]))
    
    colnames(world.region.table.tmp) <- world.region.table.tmp[1,]
    world.region.table.tmp <- world.region.table.tmp[-1,]
    world.region.table.tmp <- world.region.table.tmp[-which(apply(world.region.table.tmp[,-1], 1, FUN=function(x) all(is.na(x)))),]
    # world.region.table.tmp <- world.region.table.tmp[-c(2,4,6,8,9,19,20),]

    # Fix column names
    cols_ <- colnames(world.region.table.tmp)
    cols_ <- gsub(" ", "", cols_)
    cols_ <- as.integer(cols_)
    cols_ <- as.Date(as.numeric(cols_), origin = "1900-01-01")
    cols_ <- paste0(lubridate::month(cols_, label=TRUE), "-", lubridate::year(cols_))
    colnames(world.region.table.tmp) <- cols_
    colnames(world.region.table.tmp)[1] <- 'location'
    world.region.table.tmp <- world.region.table.tmp[, colnames(world.region.table.tmp)!="NA-NA"]
    world.region.table.tmp <- world.region.table.tmp %>% rowwise() %>% 
        filter(!grepl("total canada", tolower(location)) & !grepl("canada total", tolower(location)) & 
               !grepl("total mexico", tolower(location)) & !grepl("mexico total", tolower(location)))

    world.region.table.tmp <- world.region.table.tmp %>%
        mutate(location = gsub("[*]","",location)) %>%
        mutate(location = toupper(gsub(" ",".", stringr::str_trim(location))))
    world.region.table.tmp$location[1:4] <- c("TOTAL.ARRIVALS","TOTAL.OVERSEAS","CANADA","MEXICO")
    world.region.table.tmp <- world.region.table.tmp %>% mutate_if(is.character, list(~na_if(., "na")))
    
    travelyrdata <- as.data.frame(t(world.region.table.tmp), stringsAsFactors = F) 
    colnames(travelyrdata) <- as.character(travelyrdata[1,])
    travelyrdata <- travelyrdata[-1,]
    travelyrdata <- travelyrdata[,which(!apply(travelyrdata, 2, FUN=function(x) all(is.na(x))))]
    
    travelyrdata <- travelyrdata %>% mutate(YEAR = y_, MONTH = toupper(month.name)[1:nrow(travelyrdata)]) %>% 
        select(YEAR, MONTH, everything())
    
    # Convert data to integers
    travelyrdata[,3:ncol(travelyrdata)] <- sapply(travelyrdata[,3:ncol(travelyrdata)],as.integer)
    travelyrdata <- travelyrdata %>% mutate(EUROPE = WESTERN.EUROPE + EASTERN.EUROPE)
    
    # Save the year
    write.csv(travelyrdata, paste0("data/Travel_ITA/Inbound_Visitors_", y_, ".csv"), row.names=F)
    
    # Merge years
    travelyrdata_all <- bind_rows(travelyrdata_all, travelyrdata)
}

# Merge with other data

inbound.travel <- inbound.travel %>% filter(YEAR<2015) # remove years from 2015+
inbound.travel <- full_join(inbound.travel, travelyrdata_all)

# Check grand totals
# tmp <- rowSums(inbound.travel[,4:6], na.rm=TRUE)
# inbound.travel$GRAND.TOTAL - tmp   

inbound.travel <- inbound.travel %>% rowwise() %>% mutate(GRAND.TOTAL = ifelse(is.na(GRAND.TOTAL), CANADA+MEXICO+TOTAL.OVERSEAS, GRAND.TOTAL))


year.range <- range(inbound.travel$YEAR)
save(inbound.travel, file=paste0("data/Travel_ITA/Inbound_Visitors_", year.range[1], "-", year.range[2], ".RData"))
write.csv(inbound.travel, paste0("data/Travel_ITA/Inbound_Visitors_", year.range[1], "-", year.range[2], ".csv"), row.names=F)





# OUTBOUND FLIGHTS AMONG RESIDENTS ------------------------------------


#year.range <- c(2001,2018)
year.range <- c(2015,2018)

# Outbound Travel - Residents ---------------------------------------------
# -- Source: https://travel.trade.gov/outreachpages/outbound_historical_statistics_analyses.html

download_residents <- T  # No need to re-download unless including new years 

if (download_residents){
    outbound.dat.all <- NULL
    year <- year.range[1]:year.range[2]
    
    # First download all of the data
    for (y in year){

        if (y<=2005){
            try.test <- try(download.file(paste0("https://travel.trade.gov/view/m-", y, "-O-001/", substr(y,3,4), "outbound.csv"),
                                          paste0("data/Travel_ITA/Outbound_Travel_",y,'.csv'), mode = "wb"), TRUE)
            try.test <- try(download.file(paste0("https://travel.trade.gov/view/m-", y, "-O-001/", substr(y,3,4), "outbound.csv"),
                                          paste0("data/Travel_ITA/Outbound_Travel_",y,'.xls'), mode = "wb"), TRUE)
            if (class(try.test)=="try-error") next

        } else if (y<2011 & y>2005){
            try.test <- try(download.file(paste0("https://travel.trade.gov/view/m-", y, "-O-001/", substr(y,3,4), "outbound.csv"),
                                          paste0("data/Travel_ITA/Outbound_Travel_",y,'.xls'), mode = "wb"), TRUE)
            if (class(try.test)=="try-error") next

        } else {
            try.test <- try(download.file(paste0("https://travel.trade.gov/view/m-", y, "-O-001/", substr(y,3,4), "outbound.xls"),
                                          paste0("data/Travel_ITA/Outbound_Travel_",y,'.xls'), mode = "wb"), TRUE)
            if (class(try.test)=="try-error") next
        }
    }
    
    # Second Try
    # for (y in year){
    #     
    #         try.test <- try({
    #                         download.file(paste0("https://travel.trade.gov/view/m-", y, "-O-001/", substr(y,3,4), "outbound.csv"), 
    #                                       paste0("data/Travel_ITA/Outbound_Travel_",y,'.csv'), mode = "wb")
    #                         
    #                         download.file(paste0("https://travel.trade.gov/view/m-", y, "-O-001/", substr(y,3,4), "outbound.csv"), 
    #                                       paste0("data/Travel_ITA/Outbound_Travel_",y,'.xls'), mode = "wb")
    #                         
    #                         }, TRUE)
    #         
    #         if (class(try.test)=="try-error"){
    #             try.test <- try(download.file(paste0("https://travel.trade.gov/view/m-", y, "-O-001/", substr(y,3,4), "outbound.xls"), 
    #                                       paste0("data/Travel_ITA/Outbound_Travel_",y,'.xls'), mode = "wb"), TRUE)
    #         }
    # }
    
    # Now load appropriate sheets
    for (y in year){
        
        if (y<=2005){
            try.test <- try({
                outbound.dat <- read.csv(paste0("data/Travel_ITA/Outbound_Travel_",y,'.csv'), header=F, skip=3, nrow=15, sep=',', strip.white=T, stringsAsFactors=F)
            }, TRUE)
            if (class(try.test)=="try-error"){
                outbound.dat <- read_excel(paste0("data/Travel_ITA/Outbound_Travel_",y,'.xls'), sheet=1, col_names=F, skip=3)
            }
    
        } else if (y>=2015){
            sheets <- excel_sheets(paste0("data/Travel_ITA/Outbound_Travel_",y,'.xls'))
            matches_ <- sapply("Dec", grep, sheets)
            outbound.dat <- read_excel(paste0("data/Travel_ITA/Outbound_Travel_",y,'.xls'), sheet=sheets[matches_], col_names=F, skip=3)
        
        } else {
            sheets <- excel_sheets(paste0("data/Travel_ITA/Outbound_Travel_",y,'.xls'))
            matches_ <- sapply(month.abb, grep, sheets)

            sheets <- sheets[!grepl('chart', tolower(sheets))]
            sheets <- sheets[!grepl('graph', tolower(sheets))]
            outbound.dat <- read_excel(paste0("data/Travel_ITA/Outbound_Travel_",y,'.xls'), sheet=sheets[length(sheets)], col_names=F, skip=3)
        } 
        
        outbound.dat <- as.data.frame(outbound.dat, stringsAsFactors=F)
        outbound.dat <- outbound.dat[grep('REGION', toupper(outbound.dat[,1]))[1]:grep('GRAND TOTAL', toupper(outbound.dat[,1]))[1],] # Clean Rows
        outbound.dat <- outbound.dat[!is.na(outbound.dat[,1]),]
        outbound.dat <- outbound.dat[,1:13] # Reduce to only Region and month columns
        colnames(outbound.dat) <- toupper(outbound.dat[1,])
        outbound.dat <- data.frame(apply(outbound.dat, 2, as.character), stringsAsFactors=F)
        outbound.dat[,-1] <- gsub(",", "", as.matrix(outbound.dat[,-1]))
        outbound.dat[grepl("Total Overseas", outbound.dat[,1]),1] <- 'Total Overseas'
        outbound.dat[,1] <- trimws(outbound.dat[,1], which = c("both")) # Trim trailing whitespace
        outbound.dat[grepl("Mexico", outbound.dat[,1]) & !grepl("Total", outbound.dat[,1]),1] <- 'Mexico'
        outbound.dat[grepl("Canada", outbound.dat[,1]) & !grepl("Total", outbound.dat[,1]),1] <- 'Canada'
        
        # get rid of canada and mexico land travel
        outbound.dat <- outbound.dat[!(grepl("Mexico", outbound.dat[,1]) & grepl("Total", outbound.dat[,1])),]
        outbound.dat <- outbound.dat[!(grepl("Canada", outbound.dat[,1]) & grepl("Total", outbound.dat[,1])),]
        outbound.dat <- outbound.dat[!(grepl("North America", outbound.dat[,1])),]
        
        
        # Transpose to merge
        outbound.dat <- data.frame(t(outbound.dat), stringsAsFactors=F) # transpose for merging
        colnames(outbound.dat) <- outbound.dat[1,]
        outbound.dat <- data.frame(Year=y, outbound.dat[-1,])
        outbound.dat[,-2] <- data.frame(apply(outbound.dat[,-2], 2, as.integer), stringsAsFactors=F)
        colnames(outbound.dat)[2] <- 'Month'
        # Merge with previous year
        outbound.dat.all <- bind_rows(outbound.dat.all, outbound.dat) 
    }
    
    # Clean up columns
    #outbound.dat.all <- outbound.dat.all[,-c(15:19)]
    
    # save(outbound.dat.all, file=paste0("data/Travel_ITA/Outbound_", year.range[1], "-", year.range[2], ".RData"))
    # write.csv(outbound.dat.all, paste0("data/Travel_ITA/Outbound_", year.range[1], "-", year.range[2], ".csv"), row.names=F)
}

outbound.dat.2015to18 <- outbound.dat.all
load(file=paste0("data/Travel_ITA/Outbound_", 2001, "-", 2017, ".RData")) #outbound.dat.all #previously created

# Merge them
outbound.dat.all <- full_join(outbound.dat.all, outbound.dat.2015to18)

year.range <- c(2001,2018)
save(outbound.dat.all, file=paste0("data/Travel_ITA/Outbound_", year.range[1], "-", year.range[2], ".RData"))
write.csv(outbound.dat.all, paste0("data/Travel_ITA/Outbound_", year.range[1], "-", year.range[2], ".csv"), row.names=F)







# COMBINE OUTBOUND AND INBOUND --------------------------------------------


combine_InandOut <- TRUE  
year.range <- c(2001,2017)
year.range <- c(2001,2018)

if (combine_InandOut){
    
    years <- year.range[1]:year.range[2]
    
    load(file=paste0("data/Travel_ITA/Outbound_", year.range[1], "-", year.range[2], ".RData"))          # loads outbound.dat.all
    load(file=paste0("data/Travel_ITA/Inbound_Visitors_", year.range[1], "-", year.range[2], ".RData"))  # loads inbound.travel
    outbound.travel <- outbound.dat.all; rm(outbound.dat.all)
    colnames(outbound.travel) <- toupper(colnames(outbound.travel))
    colnames(inbound.travel) <- gsub("\\.(?=\\.*$)", "", colnames(inbound.travel), perl=TRUE)
    inbound.travel <- inbound.travel[,colMeans(is.na(inbound.travel))<.85]
    colnames(inbound.travel)[colnames(inbound.travel)=='TOTAL.ARRIVALS'] <- 'GRAND.TOTAL'
    
    # View(outbound.travel)
    # View(inbound.travel)
    
    # colnames(outbound.travel)
    # colnames(inbound.travel)
    
    outbound.travel <- data.frame(TRAVEL.DIRECTION='OUTBOUND', outbound.travel, stringsAsFactors=F)
    inbound.travel <- data.frame(TRAVEL.DIRECTION='INBOUND', inbound.travel, stringsAsFactors=F)
    
    outbound.travel$MONTH <- toupper(substr(outbound.travel$MONTH,1,3))
    inbound.travel$MONTH <- toupper(substr(inbound.travel$MONTH,1,3))
    outbound.travel$MONTH <- factor(toupper(outbound.travel$MONTH), levels=toupper(month.abb))
    inbound.travel$MONTH  <- factor(toupper(inbound.travel$MONTH), levels=toupper(month.abb))
    
    # Combine Inbound and Outbound (Visitor & Resident) travel into 1 data table, but keep separate
    all.travel <- merge(inbound.travel, outbound.travel, all=TRUE)
    
    # Sum Resident and Visitor Travel 
    total.travel <- all.travel[,-1] %>% group_by(YEAR, MONTH) %>% summarise_all(sum)
    total.travel <- total.travel[, colMeans(is.na(total.travel))<.8]
    total.travel <- total.travel[!is.na(total.travel$MONTH),]
}

# Get max month of each (inbound and outbound)
max.month = "APR"
max.year = 2018

total.travel <- total.travel %>% arrange(YEAR, MONTH) %>% filter(!(YEAR==2018 & MONTH %in% toupper(month.abb[5:12])))

year.range <- c(2001,2018)
save(total.travel, file=paste0("data/Travel_ITA/TOTALTRAVEL_", year.range[1], "-", year.range[2], ".RData"))
write.csv(total.travel, paste0("data/Travel_ITA/TOTALTRAVEL_", year.range[1], "-", year.range[2], ".csv"), row.names=F)








# RUN REGRESSIONS TO PROJECT THROUGH 2021 ---------------------------------

fc_year_max <- 2021

dir.create("figures/Travel_Forecast", recursive = TRUE)
library(mgcv)
regions_to_forecast <- c("EUROPE","ASIA","AFRICA","OCEANIA","CENTRAL.AMERICA","SOUTH.AMERICA","MIDDLE.EAST","CARIBBEAN")



# OUTBOUND 

year.range <- c(2001,2018)
years <- year.range[1]:year.range[2]
forecast_years <- (year.range[2]+1):fc_year_max
fc_range <- c(min(forecast_years), max(forecast_years))
tot_yrs <- length(year.range[1]:fc_range[2])
years_all <- c(year.range[1], fc_range[2])

load(file=paste0("data/Travel_ITA/Outbound_", year.range[1], "-", year.range[2], ".RData"))          # loads outbound.dat.all
outbound.travel <- outbound.dat.all; rm(outbound.dat.all)
colnames(outbound.travel) <- toupper(colnames(outbound.travel))
outbound.travel <- data.frame(TRAVEL.DIRECTION='OUTBOUND', outbound.travel, stringsAsFactors=F)
outbound.travel$MONTH <- factor(toupper(substr(outbound.travel$MONTH,1,3)), levels=toupper(month.abb))



outbound.travel <- outbound.travel %>% select(colnames(total.travel))
outbound.travel.long <- outbound.travel %>% 
    select(YEAR, MONTH, regions_to_forecast) %>% gather(key="region",value="travel", -YEAR, -MONTH) %>% 
    mutate(MONTH_YR = factor(paste0(MONTH, "-", YEAR), 
                             levels=paste0(rep(toupper(month.abb), tot_yrs), "-", sort(rep(2001:fc_range[2], 12))))) %>%
    mutate(MONTH = factor(MONTH, levels=toupper(month.abb))) %>%
    mutate(month_num = as.integer(MONTH)) %>%
    arrange(YEAR, MONTH)


outbound.travel.withforecast <- NULL
for (i in 1:length(regions_to_forecast)){
    
    outbound.long.region <- outbound.travel.long %>% filter(region==regions_to_forecast[i])
    
    # LM was bad, use GAM
    gam_out <- gam(travel ~ te(YEAR, month_num,
                               k = c(length(years), 12), bs = c("cr", "ps")) ,
                   data=outbound.long.region, family = gaussian)
    
    tmp <- bind_rows(outbound.long.region %>% select(YEAR, MONTH, month_num, MONTH_YR, travel) %>%
                         mutate(source="Real"),
                     outbound.long.region %>% select(YEAR, MONTH, month_num, MONTH_YR) %>% 
                         mutate(travel=gam_out$fitted.values, source="Projected"))
    # # Plot it
    # ggplot(tmp, aes(x=MONTH_YR, y=travel, group=source, color=source)) + geom_line()
    
    data_proj <- data.frame(YEAR=sort(rep(forecast_years, 12)), 
                            MONTH=rep(toupper(month.abb), length(forecast_years)),
                            month_num=rep(1:12,length(forecast_years)))
    data_proj$travel <- predict.gam(gam_out, data_proj)
    data_proj <- data_proj %>% 
        mutate(MONTH_YR = factor(paste0(MONTH, "-", YEAR), 
                                 levels=paste0(rep(toupper(month.abb), tot_yrs), "-", sort(rep(2001:fc_range[2], 12)))))
    res <- full_join(tmp, data_proj %>% mutate(source="Forecast"))
    
    # Plot it
    ggplot(res, aes(x=MONTH_YR, y=travel, group=source, color=source)) + geom_line() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_x_discrete(breaks = levels(res$MONTH_YR)[c(T, rep(F,11))]) +
        ggtitle(paste0(regions_to_forecast[i], " Travel to U.S."))
    
    ggsave(paste0("figures/Travel_Forecast/",regions_to_forecast[i],"_travelforecast_outbound.png"), width=8, height = 5)
    
    outbound.travel.withforecast <- bind_rows(outbound.travel.withforecast, 
                                              res %>% mutate(Region=regions_to_forecast[i]))
}

# Save Outbound

save(outbound.travel.withforecast, file=paste0("data/Travel_ITA/OUTBOUND_TRAVEL_FORECAST_", years_all[1], "-", years_all[2], ".RData"))
write.csv(outbound.travel.withforecast, paste0("data/Travel_ITA/OUTBOUND_TRAVEL_FORECAST_", years_all[1], "-", years_all[2], ".csv"), row.names=F)







# INBOUND  .........................

year.range <- c(2001,2019)
years <- year.range[1]:year.range[2]
forecast_years <- (year.range[2]):fc_year_max
fc_range <- c(min(forecast_years), max(forecast_years))
tot_yrs <- length(year.range[1]:fc_range[2])
years_all <- c(year.range[1], fc_range[2])

load(file=paste0("data/Travel_ITA/Inbound_Visitors_", year.range[1], "-", year.range[2], ".RData"))  # loads inbound.travel
colnames(inbound.travel) <- gsub("\\.(?=\\.*$)", "", colnames(inbound.travel), perl=TRUE)
inbound.travel <- inbound.travel[,colMeans(is.na(inbound.travel))<.85]
colnames(inbound.travel)[colnames(inbound.travel)=='TOTAL.ARRIVALS'] <- 'GRAND.TOTAL'
inbound.travel <- data.frame(TRAVEL.DIRECTION='INBOUND', inbound.travel, stringsAsFactors=F)
inbound.travel$MONTH  <- factor(toupper(substr(inbound.travel$MONTH,1,3)), levels=toupper(month.abb))



inbound.travel <- inbound.travel %>% select(colnames(total.travel))
inbound.travel.long <- inbound.travel %>% 
    select(YEAR, MONTH, regions_to_forecast) %>% gather(key="region",value="travel", -YEAR, -MONTH) %>% 
    mutate(MONTH_YR = factor(paste0(MONTH, "-", YEAR), 
                             levels=paste0(rep(toupper(month.abb), tot_yrs), "-", sort(rep(2001:fc_range[2], 12))))) %>%
    mutate(MONTH = factor(MONTH, levels=toupper(month.abb))) %>%
    mutate(month_num = as.integer(MONTH)) %>%
    arrange(YEAR, MONTH)


inbound.travel.withforecast <- NULL
for (i in 1:length(regions_to_forecast)){
    
    inbound.long.region <- inbound.travel.long %>% filter(region==regions_to_forecast[i] & !is.na(travel))
    
    
    # LM was bad, use GAM
    gam_out <- gam(travel ~ te(YEAR, month_num,
                               k = c(length(years)-1, 12), bs = c("cr", "ps")) ,
                   data=inbound.long.region, family = gaussian)
    
    tmp <- bind_rows(inbound.long.region %>% select(YEAR, MONTH, month_num, MONTH_YR, travel) %>%
                         mutate(source="Real"), #%>% filter(!is.na(travel)),
                     inbound.long.region %>% select(YEAR, MONTH, month_num, MONTH_YR) %>% 
                         mutate(travel=gam_out$fitted.values, source="Projected"))

    data_proj <- data.frame(YEAR=sort(rep(forecast_years, 12)), 
                            MONTH=rep(toupper(month.abb), length(forecast_years)),
                            month_num=rep(1:12, length(forecast_years)))
    data_proj$travel <- predict.gam(gam_out, data_proj)
    data_proj <- data_proj %>% 
        mutate(MONTH_YR = factor(paste0(MONTH, "-", YEAR), 
                                 levels=paste0(rep(toupper(month.abb), tot_yrs), "-", sort(rep(2001:fc_range[2], 12)))))
    res <- full_join(tmp, data_proj %>% mutate(source="Forecast"))
    
    # Plot it
    ggplot(res, aes(x=MONTH_YR, y=travel, group=source, color=source)) + geom_line() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_x_discrete(breaks = levels(res$MONTH_YR)[c(T, rep(F,11))]) +
        ggtitle(paste0(regions_to_forecast[i], " Travel to U.S."))
    ggsave(paste0("figures/Travel_Forecast/",regions_to_forecast[i],"_travelforecast_inbound.png"), width=8, height = 5)
    
    inbound.travel.withforecast <- bind_rows(inbound.travel.withforecast, 
                                              res %>% mutate(Region=regions_to_forecast[i]))
}

# Save Outbound
save(inbound.travel.withforecast, file=paste0("data/Travel_ITA/INBOUND_TRAVEL_FORECAST_", years_all[1], "-", years_all[2], ".RData"))
write.csv(inbound.travel.withforecast, paste0("data/Travel_ITA/INBOUND_TRAVEL_FORECAST_", years_all[1], "-", years_all[2], ".csv"), row.names=F)









# COMBINE FORECASTED DATA -------------------------------------------------


year.range <- c(2001,fc_year_max)

years <- year.range[1]:year.range[2]

load(file=paste0("data/Travel_ITA/INBOUND_TRAVEL_FORECAST_", year.range[1], "-", year.range[2], ".RData"))   # loads inbound.travel.withforecast
load(file=paste0("data/Travel_ITA/OUTBOUND_TRAVEL_FORECAST_", year.range[1], "-", year.range[2], ".RData"))  # loads outbound.travel.withforecast

outbound.travel <- outbound.travel.withforecast %>% filter(source!="Projected"); rm(outbound.travel.withforecast)
inbound.travel <- inbound.travel.withforecast   %>% filter(source!="Projected"); rm(inbound.travel.withforecast)

outbound.travel <- outbound.travel %>% 
    mutate(monthyrregion = paste0(MONTH_YR,"-", Region),
           MONTH = factor(MONTH, levels=toupper(month.abb))) %>% 
    mutate(source = factor(source, levels=c("Real", "Forecast"))) %>%
    arrange(YEAR, MONTH, Region, source) %>%
    mutate(dupl = duplicated(monthyrregion)) %>%
    filter(!dupl) %>% mutate(type="OUTBOUND")

inbound.travel <- inbound.travel %>% 
    mutate(monthyrregion = paste0(MONTH_YR,"-", Region),
           MONTH = factor(MONTH, levels=toupper(month.abb))) %>% 
    mutate(source = factor(source, levels=c("Real", "Forecast"))) %>%
    arrange(YEAR, MONTH, Region, source) %>%
    mutate(dupl = duplicated(monthyrregion)) %>%
    filter(!dupl) %>% mutate(type="INBOUND")

# Combine Inbound and Outbound (Visitor & Resident) travel into 1 data table, but keep separate
all.travel <- full_join(inbound.travel, outbound.travel, all=TRUE) %>% select(-monthyrregion, -source, -dupl)

# Sum Resident and Visitor Travel 
total.travel <- all.travel %>% group_by(YEAR, MONTH, Region) %>% summarise(sum(travel))
total.travel <- total.travel %>% arrange(YEAR, MONTH, Region)
total.travel <- total.travel %>% mutate(source = ifelse(YEAR<2018, "REAL", "FORECAST"))

year.range <- range(total.travel$YEAR)
years <- year.range[1]:year.range[2]

total.travel.long <- total.travel
total.travel.long <- total.travel.long %>% rename(travel=`sum(travel)` ) %>%
    mutate(MONTH_YR = factor(paste0(MONTH, "-", YEAR), levels = paste0(rep(toupper(month.abb), length(years)), "-", sort(rep(years, 12)))))
total.travel.long <- total.travel.long %>% mutate(travel = as.integer(round(travel))) %>% as.data.frame()

save(total.travel.long, file=paste0("Data/Travel_ITA/totaltravel_long_withforecast", year.range[1], "-", year.range[2], ".RData"))
write_csv(total.travel.long, paste0("Data/Travel_ITA/totaltravel_long_withforecast", year.range[1], "-", year.range[2], ".csv"))


total.travel <- total.travel.long %>% 
    select(-source) %>%
    spread(key = Region, value=travel)

save(total.travel, file=paste0("data/Travel_ITA/totaltravel_withforecast", year.range[1], "-", year.range[2], ".RData"))
write.csv(total.travel, paste0("data/Travel_ITA/totaltravel_withforecast", year.range[1], "-", year.range[2], ".csv"), row.names=F)





# PLOT FORECASTED DATA ----------------------------------------------------

ggplot(total.travel.long, aes(x=MONTH_YR, y=travel, group=source, color=source)) + geom_line() +
    facet_wrap(vars(Region), ncol=3) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_discrete(breaks = levels(total.travel.long$MONTH_YR)[c(T, rep(F,11))]) +
    ggtitle("Regional Travel to U.S.")

ggsave(paste0("Figures/Travel_Forecast/ALL_travelforecast.png"), width=10, height = 10)
















# Match Travel Data Regions to Countries ----------------------------------
# -- Regions are the United Nations Country Grouping regions

# tmp <- read.table("http://54.245.224.94/wordpress/wp-content/uploads/2013/11/iso-3166-and-un-code-list.txt", header=T, sep="\t",
#                   stringsAsFactors=F, nrows=63)

# Add New Variable With More Specific Regions

middle.east <- toupper(c('Bahrain','Cyprus','Egypt','Iran','Iraq','Israel','Jordan','Kuwait','Lebanon','Oman','Qatar',
                 'Saudi Arabia','Syria','Turkey','United Arab Emirates','Yemen'))
middle.east.iso <- sapply(middle.east, get.iso)

regions.to.match <- colnames(total.travel)[-(1:3)]

region1_tmp <- gsub(" ",".", toupper(iso_data$Region))
region2_tmp <- gsub(" ",".", toupper(iso_data$Sub.Region))
country1_tmp <- gsub(" ",".", toupper(iso_data$Country))
#country2_tmp <- gsub(" ",".", toupper(iso_data$Country2))

travel.region <- character(nrow(iso_data))
for (i in 1:nrow(iso_data)) {
    match1 <- match(region1_tmp[i], regions.to.match)
    if (!is.na(match1)) {   travel.region[i] <- regions.to.match[match1]  }
    
    match2 <- match(region2_tmp[i], regions.to.match)
    if (!is.na(match2)) {   travel.region[i] <- regions.to.match[match2]  }
    
    match3 <- match(iso_data$ISO3[i], middle.east.iso)
    if (!is.na(match3)) {   travel.region[i] <- 'MIDDLE.EAST'  }
    
    match4 <- match(country1_tmp[i], regions.to.match)
    if (!is.na(match4)) {   travel.region[i] <- regions.to.match[match4]  }
}

iso_data$Travel.Region <- travel.region



# Function to get match country to Travel Region using ISO
get.travel.region.iso <- function(ISO){
    # Get WHO Regions using ISO
    return(as.character(iso_data$Travel.Region[which(iso_data$ISO3==ISO)]))
}





# TESTING -----------------------------------------------------------------

# test.countries <- c("Austria", "Belgium", "Bulgaria", "Cyprus",  "Czech.Republic", "Denmark", "Estonia", "Finland",
#                     "France",  "Germany", "Greece",  "Hungary", "Iceland", "Ireland", "Italy", "Latvia", 
#                     "Lithuania", "Luxembourg", "Malta", "Netherlands", "Norway",  "Poland",  "Portugal", 
#                     "Romania", "Slovakia", "Spain",   "Sweden", "United.Kingdom")
#
# sapply(test.countries, get.iso)
# sapply(as.character(sapply(test.countries, get.iso)), get.travel.region.iso)


