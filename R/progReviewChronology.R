library(XML)
library(ReadPDF)

if(FALSE) {}

getXMLDocs =
    function()
{
    xml = list.files(pattern = "\\.xml", full.names = TRUE, recursive = TRUE)
    xml = grep("Program Review Resources", xml, invert = TRUE, value = TRUE)
    w = grepl("2008|2009|20[12][0-9]", basename(xml))
    xml2 = xml[w]
    # 178 in; 69 out.
    xml2
}


go = 
function(xml2 = getXMLDocs(),
         fix = TRUE)
{    
    o = lapply(xml2, \(x) try(prChronology(x)))
    names(o) = xml2
    err = sapply(o, inherits, 'try-error')
    # 6 with errors fromm 182
    #  when looking for the footnote line, got multiple lines on that page. Use the bottom one.
    # Now down to 1. No timeline. So return NULL.
    # No errors.
    empty = sapply(o[!err], is.null)
    # 23 empty from 178 w/o errors.
    warning(paste(sum(empty), " timelines from ", length(xml2), " documents"))
    
    tl = o[!empty]
    tls = do.call(rbind, tl)
    tls$program = rep(dirname(xml2[!empty]), sapply(tl, nrow))
    tls$file = rep(xml2[!empty], sapply(tl, nrow))

    if(fix)
        tls = fixDates(tls)
    
    tls
}

if(FALSE) {
    # Now clean up rows with
    #  problematic dates
    #    missing :
    # Convert dates

    tls2 = fixDates(tls)

    dur = tapply(tls2$date2, tls2$file, function(x) diff(range(x, na.rm = TRUE), units = "days"))
}


prChronology =
function(doc)
{
    if(is.character(doc))
        doc = readPDFXML(doc)
    
    p = getNodeSet(doc, "//page[./text[contains(., 'Chronology')]]")
    if(length(p)) 
        return(prChronology1(p[[1]], doc))

    p = getNodeSet(doc, "//page[./text[contains(., 'chronology') or contains(., 'time line')]]")
    if(length(p)) 
        return(prChronologyFN(p[[1]], doc))

    return(NULL)
    Open(gsub("\\.xml$", ".pdf", docName(doc)))
    browser()
}

prChronology1 =
function(p, doc, bbox = getBBox2(p, TRUE))
{
    i = grep("Chronology", bbox$text)
    cr = bbox[i,]
    arrangeLinesBelow(bbox, cr$top + cr$height, cr$height)
}

arrangeLinesBelow =
function(bbox, start, height = NA)        
{    
    bb2 = bbox[bbox$top > start & trimws(bbox$text) != "",]

    if(is.na(height))
        height = median(bb2$height)

    g = split(bb2, cut(bb2$top, c(seq(start, by = height * 1.2, length = 20), Inf)))
    g2 = g[sapply(g, nrow) > 0]

    ll = trimws(sapply(g2, function(x) paste(x$text, collapse = " ")))
    procEvents(ll)
}

procEvents =
function(txt)
{
    txt = txt[!(txt %in% c("UNIVERSITY OF CALIFORNIA", "UNIVERSITY OF CALIFORNIA--(Letterhead for Interdepartmental use)"))]

    rx = sprintf("(%s) ([0-9]{1,2}|[0-9]{1,2}-[0-9]{1,2}), [0-9]{4}$", MonthNamesRX)
    w = grepl(rx, txt)
    if(sum(w) > .65*length(txt)) {

        rx2 = sprintf("(.*) (%s)", rx)
        date = gsub(rx2, "\\2", txt)
        ev = gsub(rx2, "\\1", txt)        
        
    } else {
        # Some are missing :
        date = gsub("^([^:]+): .*", "\\1", txt)
        date[date == "N/A"] = NA
        
        ev = gsub("^([^:]+): (.*)", "\\2", txt)

        rx = sprintf("^(%s) ", MonthNamesRX)

        if( sum(grepl(rx, txt)) < .65*length(txt) && sum(grepl("^[A-Za-z]", txt)) > length(txt)*.6) {
            tmp = ev 
            ev = date
            date = tmp
        }
    }
    

    data.frame(date = date, #  as.Date(date, "%m/%d/%Y"),
               event = ev,
               row.names = NULL)
}



prChronologyFN =
function(p, doc, bbox = getBBox2(p, TRUE))
{
    sh = getBBox(p, TRUE)
    if(nrow(sh) == 0)
        return(NULL)
    
    #XXX need better estimate of height than 10
    arrangeLinesBelow(bbox, max(sh$y1))
}


###########

MonthNames = months(as.Date(sprintf("2025/%d/1", 1:12)))
MonthNamesRX = paste(MonthNames, collapse = "|")

fixDates =
function(df, ...)
{
    d = df$date


    # fix e.g.,
    #  "6/4/18; 11/16/18 (Follow-up Report)"
    #  "05/04/18 and 05/17/18"
    #  "05/04/18-05/17/18"
    # Important for the last of these to be done before the next gsub() which also handles -
    d = gsub("^([0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4})(; | and|-)([0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4})", "\\3", d)    

    # For "March 12 - 13, 2009"
    # "January 21-22, 2020"
    # NOT for  "12/06/2023 - 12/07/2023"
    d = gsub("[0-9]{1,2} ?- ?([0-9]{1,2},)", "\\1", d)
    
    mn = paste(MonthNames, collapse = "|")
    rx = paste0("^(", mn, ")(,?) ([0-9]{4})$")

    d = gsub(rx, "\\1 15, \\3", d)
    
    d = trimws(gsub("^1 ", "", d) )

    # fix 1/1/18 to be 2018
    d = gsub("^([0-9]{1,2}/[0-9]{1,2}/)([0-9]{2})( |$)", "\\120\\2", d)

    # For "December 2, 222"
    d = gsub(", 222$", ", 2022", d)

   
    #
    d2 = as.Date(d, "%m/%d/%Y")
    w = is.na(d2) 
    d2[w] = as.Date(d[w], "%B %d, %Y")
    w = is.na(d2) & d != "UNIVERSITY OF CALIFORNIA"

    df$date2 = d2
    # The cleaned up version so we can compare. Only useful 
    #    df$date3 = d
    
    mergeManual(df, ...)
}


mergeManual =
function(df, xlsx = "manualFix.xlsx")
{
    m = readxl(xlsx)
    w = is.na(df$date2) & !is.na(df$date) & df$date != "N/A"

    i = match(df$date[w], m$date)

    nd = as.Date(as.integer(m$date2[i]), origin = "1899-12-30")
    df$date2[w] = nd

    w = is.na(m$date)


    # Read the second sheet.
    m2 = readxl(xlsx, 2)
    tmp = m2[is.na(m2[[1]]), -c(4)]
    names(tmp)[3] = "event"
    tmp = tmp[names(df)]

#    browser()    

    # drop these rows in df
    j = which(is.na(m$date2))    
    w2 = !is.na(df$date) & df$date %in% m$date[j]
    df = df[!w2,]

    df = rbind(df, tmp)

    ww = !(is.na(df$date2) & !is.na(df$date) & df$date != "N/A")
    df = df[ww, ]
    df$event = trimws( df$event)
    df
}


mapEvents =
function(ev)
{
    ev2 = ev
    w = grepl("self-?review", ev, ignore.case = TRUE)
    
    ev
}

library(GradPrograms)
codes = programCodes()
codes = c(codes,
          "Atmospheric Science" = "GATM",
          "Accountancy" = "GPAC",
          "Preventative Veterinary Medicine" = "MVPM",
          "Molecular, Cellular, and Integrative Physiology" = "MCIP",
          "Public Health Sciences" = "MMPH",
          "French and Francophone" = "GFFS",
          "Earth and Planetary Sciences (formerly Geology)" = "GEPS",
          "Art Studio" = "GART",
          "Business Administration (Online)" = "SMBO",
          "Chemistry & Chemical Biology" = "GCCB",
          "Biochemistry, Molecular, Cell and Developmental Biology" = "GBCB"
          )
          
p = basename(dirname(tls2$file))
m = match(p, names(codes))
table(is.na(m))
unique(p[is.na(m)])

# DEs
