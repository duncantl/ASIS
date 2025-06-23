# Not all files downloaded for a given year pertain to that year.
# This is because, for example, the GC Reports page is not specific to a year.

# From ~/OGS/GradCouncil/meetings.R

#library(RCurl)
#library(XML)

#library(UCDGUARD)
#source("~/Davis/UCDGARD/R/guard.R")

getViewForYear =
function(year = NA, whiteboard = NA)    
{
    if(!is.na(year)) {} # set whiteboard if NA
    
    els = c(agenda = TRUE, Summary = TRUE)
    if(whiteboard)
        els["committees_whiteboard"] = TRUE
    


    paste(names(els), collapse = "|")
}

getCommitteeDocs = 
function(id, dir = "PDFs", con, year = NA, view = getViewForYear(year), u = "https://asis.ucdavis.edu/committee_v2/view_committee.cfm")
{
    params = list(id = id)# , view = "topics")
    if(!is.na(year))
        params$year = year
    
    txt = getForm(u, .params = params, curl = con)
#txt = getURLContent("https://asis.ucdavis.edu/committee_v2/view_committee.cfm?id=109&view=topics", curl = con)

    doc = htmlParse(txt)
    
    ll = getHTMLLinks(doc)

    ll2 = grep("view_file.cfm", ll, value = TRUE)


    if(length(view)) {
        w = grepl(paste0("view=", view), ll2)
        ll2 = ll2[w]
    }

    if(length(ll2) == 0) {
        warning("no documents for ", id, " year ", year)
        return(character())
    }

    u2 = getRelativeURL(ll2, u)

    files = lapply(u2, getURLContent, curl = con, binary = TRUE)

    if(!file.exists(dir))
        dir.create(dir)

    invisible( sapply(u2, downloadFile2, dir, con) )
#    invisible(mapply(savePDF, files, sprintf("%s/%d.pdf", dir, seq(along = files))))
}

downloadFile2 =
    # See committeeFiles.R for other version. Need to keep separate or merge.
function(url, dir, curl = getCurlHandle(followlocation = TRUE))
{
    #browser()
    content = getURLContent(url, binary = TRUE, curl = curl, header = TRUE)
    # can get the header back in the call to getURLContent
    # hdr = httpHEAD(url, curl = con, header = TRUE)$header

    hdr = content$header
    content = content$body$body
    
    if(file.info(dir)$isdir) {
        #els = strsplit(hdr[c("Content-disposition", "Content-Type")], ";")
        els = strsplit(hdr["Content-disposition"], ";")[[1]]
        filename = file.path(dir, gsub('"', '', gsub("filename=", "", grep("filename", els, value = TRUE))))
        if(file.exists(filename)) {
            filename = gsub("(\\.(pdf|doc|docx))", "-1\\1", filename)
            if(file.exists(filename))
                stop("problem with duplicate file name")
        }
    } else
        filename = dir

    savePDF(content, filename)
    filename
}



if(FALSE) {

    con = getCurlHandle(cookie = readLines("cookie")[1], followlocation = TRUE)

    ids = c(GC = 109, EPC = 177, PRC = 191, Bylaws = 208, APD = 176, Courses = 110, PRCC = 233)

    invisible(mapply(getCommitteeDocs, ids, names(ids), MoreArgs = list(con = con)))

    y = 2016:2021
    y = 2001:2016
    y = 1985:2021
    # 2008-2009
    years = paste(y, y + 1, sep = "-")
    z = invisible(lapply(years, function(y) {
        dir = gsub("-", "_", y)
        if(!file.exists(dir))
            dir.create(dir)
        print(dir)
        view = if(y == "2022-2023")
                   getViewForYear(, TRUE)
               else
                   getViewForYear(, FALSE)
        mapply(getCommitteeDocs, ids, file.path(dir, names(ids)), MoreArgs = list(year = y, con = con, view = view))
    }))
}



getDate =
function(f)
{
    doc = xmlParse(f)
    txt = xpathSApply(doc, "//page[@number = 1]/text", xmlValue)
    grep("(19|20)[0-9]{2}", txt, value = TRUE)
}
