#library(ReadPDF)
#library(XML)

if(FALSE) {
  zz = getHistories()
}

if(FALSE) {
    xml = list.files(pattern = "^2.*\\.xml$")
    hh = lapply(xml, getHistories)
    names(hh) =  gsub("[ _].*", "", xml)
    hh2 = hh[sapply(hh, length) > 0]

    hh3 = combineHistories(unlist(hh, recursive = FALSE))
    w = !is.na(hh3$date) & hh3$date > Sys.Date()
    hh3$date[w] = as.Date("2023/11/21")
    plot.Histories(hh3)
}



getHistories =
function(doc = "2025.06.20_GC Meeting Call.xml")    
{
    if(is.character(doc))
        doc = readPDFXML(doc)

    #p = xmlParent(xmlParent(h[[1]]))
 
    xpathApply(doc, "//page[.//text()[normalize-space(.) = 'History:']]", getHistory)
}

getHistory =
function(page)    
{
    p = page
    class(p) = c("PDFToXMLPage", "ConvertedPDFPage", class(p))

    #bx = getBBox(p, TRUE)
    #mar = margins(p)    
    bb = getBBox2(p, TRUE)
    ll = getLines(p, marThreshold = 12)
    
    w = cut(bb$top, unique(c(ll$y0, Inf)))

    bb.inBox = subset(bb, top >= min(ll$y0) & (top + height) <= max(ll$y0))

#    plot(density(bb.inBox$left))

    # d = density(bb.inBox$left)
    # mid = d$x[which.max(d$y)] 

    mid = min(bb.inBox$left[ grepl("^(EPC|Bylaws|APD)", bb.inBox$text)])

    g = split(bb, w)
    g = g[ sapply(g, nrow) > 0]
#    browser()
    tmp = lapply(g, function(x) split( x$text,  cut(x$left, unique(c(0, mid - 10, Inf)))))

    # drop the remainder
    tmp = tmp[ - length(tmp) ]

    ans = mkHistoryDF(tmp)
    meta = mkHistoryMetaInfo(page, ans, bb)
    attr(ans, "metaInfo") = meta
    
    ans = rbind(ans,
                data.frame(date = as.Date(fixDate(meta$approved), "%B %d, %Y"), steps = "approved", text = NA))
    ans
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
    x = gsub("Novermber", "November", x)
    if(any(w <- grepl("^[0-9]+/[0-9]+/[0-9]+", x))) 
       x[w] = format(as.Date(x[w], "%m/%d/%Y"), "%B %d, %Y")

    return(x)
    
    
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



mkHistoryMetaInfo =
function(page, h, bbox = getBBox2(page, TRUE))
{
    #    subj = getNodeSet(page, ".//text[. = 'SUBJECT:']")[[1]]
    #    font = xmlGetAttr(subj, "font")
    #    approved = getNodeSet(page, sprintf(".//text[contains(., 'APPROVED:') and @font = '%s']", font))[[1]]

    ia = i = grep("APPROV(ED|AL):", bbox$text)
    approved = toRight(bbox[i, ], bbox)

    i = grep("SUBJECT:", bbox$text)
    subj = toRight(bbox[i, ], bbox)


#    committee = toupper(strsplit(h$text[1], " ")[[1]][1])
    committee2 = gsub(" APPROV(ED|AL).*", "", bbox$text[ia])
#    if(committee != committee2)     browser()
    list(subject = subj,
         approved = approved,
         committee = committee2)
    
}

toRight =
function(row, bbox, asText = TRUE, threshold = 4)
{
   ans =  bbox[ bbox$left > (row$left + row$width) &
                abs(bbox$top - row$top) < threshold &
                 abs( (bbox$top + bbox$height) - (row$top + row$height)) < threshold, ]

   if(asText)
      trimws(paste(ans$text, collapse = " "))
   else
       ans
}


###########################################################################

combineHistories =
function(x)
{
    tmp = mapply(expandHistory, x, names(x), SIMPLIFY = FALSE)
    ans = do.call(rbind, tmp)
    ans$item = rep(1:length(x), sapply(tmp, nrow))
    ans
}

expandHistory =
function(x, label = "",  meta = attr(x, "metaInfo"))
{
    x$committee = meta$committee
    x$topic = meta$subject
    x$from = label
    x
}



plot.Histories =
function(x, ...)
{
    x$action = mkActions(x$steps)

    ggplot(x, aes(x = date,
                  y = ordered(item, unique(item)),
                  color = committee,
                  shape = action)
           ) +
        geom_line() +
        geom_point() +
        ylab("item") 
#        labs(shape = "action")
}

mkActions =
function(x)
{
    ans = rep("?", length(x))
    w = !is.na(x)
    ans[w] = capitalize(x[w])
    ans
}

capitalize =
function(x)    
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))

