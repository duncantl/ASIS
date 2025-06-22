library(ReadPDF)
library(XML)

if(FALSE) {
  zz = getEPCHistories()
}

if(FALSE) {
    xml = list.files(pattern = "^2.*\\.xml$")
    hh = lapply(xml, getEPCHistories)
    names(hh) =  gsub("[ _].*", "", xml)
    hh
}



getEPCHistories =
function(doc = "2025.06.20_GC Meeting Call.xml")    
{
    if(is.character(doc))
        doc = readPDFXML(doc)

    #p = xmlParent(xmlParent(h[[1]]))
    xpathApply(doc, "//page[.//text()[normalize-space(.) = 'History:']]", getEPCHistory)
}

getEPCHistory =
function(page)    
{
    p = page
    class(p) = c("PDFToXMLPage", "ConvertedPDFPage", class(p))

    #bx = getBBox(p, TRUE)
    #mar = margins(p)    
    bb = getBBox2(p, TRUE)
    ll = getLines(p)
    
    w = cut(bb$top, unique(c(ll$y0, Inf)))

    bb.inBox = subset(bb, top >= min(ll$y0) & (top + height) <= max(ll$y0))
#    plot(density(bb.inBox$left))

    # d = density(bb.inBox$left)
    # mid = d$x[which.max(d$y)] 

    mid = min(bb.inBox$left[ grepl("^(EPC|Bylaws|APD)", bb.inBox$text)])

    g = split(bb, w)
    g = g[ sapply(g, nrow) > 0]
    browser()
    tmp = lapply(g, function(x) split( x$text,  cut(x$left, unique(c(0, mid - 10, Inf)))))

    # drop the remainder
    tmp = tmp[ - length(tmp) ]


    mkHistoryDF(tmp)
}

mkHistoryDF =
function(tmp)
{
    dt = sapply(tmp, `[`, 1)
    dt = fixDate(dt)
    txt = normalizeSpace(sapply(tmp, function(x) paste(x[[2]], collapse = " ")))
    data.frame(date = as.Date(dt, "%B %d, %Y"),
               steps = steps(txt),               
               text = txt,
               row.names = NULL)
}

normalizeSpace =
function(x)
    trimws(gsub("[[:space:]]+", " ", x) )

MonthNames = months(as.Date(sprintf("2025/%d/1", 1:12)))
fixDate =
function(x)
{
    return(gsub("Novermber", "November", x))
    
    els =  strsplit(x, " ")
    m = sapply(els, `[`, 1)
    w = m %in% MonthNames
    if(!all(w)) {
        x = sapply(els, paste, collapse = " ")
    }

    x
}


steps =
function(x)
{
    w = strsplit(x, " ")
    verb = sapply(w, `[`, 2)
    steps = c("received" = "submitted", "returned" = "returned", "requested" = "returned", "suggested" = "returned", "voted" = "final")
    steps[verb]
}



