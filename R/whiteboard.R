# https://asis.ucdavis.edu/committee_v2/view_committee.cfm?id=177&view=topics

if(FALSE) {
    wbs2 = lapply(CommitteeIds, \(id) try(getWhiteboard(id, acon, comments = TRUE)))
    names(wbs2) = names(CommitteeIds)



    com = lapply(wbs2, commentsDF)
    w = !sapply(com, is.null)
    lapply(names(com)[w], function(nm) { png(paste0("comments", nm, ".png")); on.exit(dev.off()); print(plot(com[[nm]]))})
}



WBBaseURL = "https://asis.ucdavis.edu/committee_v2/view_committee.cfm"

getWhiteboard =
function(comId = 177, con = mkASISCon(), comments = FALSE)
{
    url = WBBaseURL
    tt = getForm(url, id = comId, view = "topics", curl = con)
    doc = htmlParse(tt)

    if(FALSE) {
        xp = sprintf("//a[contains(@href, '/committee_v2/whiteboard.cfm?committee_id=%s&id=')]", comId)
        a = getNodeSet(doc, xp)
        lnks = structure(sapply(a, xmlGetAttr, "href"), names = sapply(a, xmlValue))
        lnks = getRelativeURL(lnks, url)
    }

    ans = wb(doc)
    if(comments)
        ans$comments = wbDetails(ans, con)

    ans
}

wb = 
function(doc)
{
    rows = xpathApply(doc, "//div[@class = 'whiteboards']/div[starts-with(@class, 'item')]", procWBRowItem)
    if(is.null(rows))
        return(NULL)
    ans = do.call(rbind, rows)
    w = sapply(ans, is.character)
    ans[w] = lapply(ans[w], normalizeSpace)

    ans$title = gsub(" \\(View File\\)$", "", ans$title)
    ans$numComments = as.integer(gsub("Comments \\(([0-9]+)\\)$", "\\1", ans$numComments))
    dv = c("due", "created")
    ans[ dv ] = lapply(ans[dv], as.Date, "%m/%d/%Y")
    ans
}

procWBRowItem =
    #
    #  cell and then 3 cell right_cell nodes
    #  cell
    #    title
    #    content
    #  3 right_cell  Comments, Due, Created
function(x)
{
    k = x[names(x) == "div"]

    a = k[[1]]
    b = a[names(a) == "div"]

    lnk = xpathSApply(b[[1]], ".//a/@href")[[1]]
    params = getFormParams(lnk)
    
    data.frame(
        title = xmlValue(b[[1]]),
        content = if(length(b) > 1) xmlValue(b[[2]]) else NA,
        numComments = xmlValue(k[[2]]),
        due = xmlValue(k[[3]]),
        created = xmlValue(k[[4]]),
        id = orNA(params["id"]),
        url = if(!is.null(lnk)) getRelativeURL(lnk, WBBaseURL) else NA,
        row.names = NULL
    )
}



wbItemDetails =
function(url, con = mkASISCon())
{
    if(is.na(url))
        return(NULL)
    
    doc = htmlParse(getURLContent(url, curl = con))
    class = c("name", "date", "comment_text")
    xps = sprintf("//div[@class = 'comments']//div[@class = '%s']", class)
    ans = lapply(xps, function(xp) normalizeSpace(xpathApply(doc, xp, xmlValue)))
    names(ans) = class
    names(ans)[3] = "comment"
    ans$date = as.Date(gsub("- ", "", ans$date), "%m/%d/%Y")
    as.data.frame(ans)    
}

wbDetails =
function(df, con = mkASISCon())
{
    lapply(df$url, wbItemDetails, con)
}


commentsDF =
    # admin, courses, welfare have no whiteboards
    # so are returned as null.
function(x, coms = x$comments)    
{
    e2 = do.call(rbind, coms)
    nr = sapply(coms, function(x)  if(!is.null(x)) nrow(x) else 0)
    e2$id = rep(x$id, nr)
    e2$created = rep(x$created, nr)
    e2$due = rep(x$due, nr)
    if(!is.null(e2))
        class(e2) = c("CommitteeComments", class(e2))
    e2
}

plot.CommitteeComments =
function(x, ...)
{
    ggplot(x, aes(x = date, y = factor(id))) +
        geom_line() +
        geom_point() +
        geom_point(aes(x = due, y = factor(id)), col = "red") + 
        geom_point(aes(x = created, y = factor(id)), col = "green")  +
        ylab("item")
}

if(FALSE) {
    z = commentsDF(wbs2$epc)

    len = wbs2$epc$due - wbs2$epc$created

    plot(z)
    
    ggplot(z, aes(x = date, y = factor(id))) + geom_line() + geom_point() +
        geom_point(aes(x = due, y = factor(id)), col = "red") + 
        geom_point(aes(x = created, y = factor(id)), col = "green")
}

