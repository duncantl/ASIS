CommitteeIds = c(gc = 109, admin = 209, apd = 176, epc = 177, prc = 191, bylaws = 208, prcc = 233, welfare = 242, courses = 110)

if(FALSE) {
    meetings = lapply(CommitteeIds, getMeetingDates)
    names(meetings) = names(CommitteeIds)

    meetings$gc = meetings$gc[-nrow(meetings$gc),]
    
    meetings2 = do.call(rbind, meetings)
    meetings2$committee = rep(names(meetings), sapply(meetings, nrow))

    library(ggplot2)
    tt = sort(table(meetings2$committee))
    ggplot(meetings2, aes(x = Date, y = factor(committee, names(tt)), colour = committee)) + geom_line() + geom_point() + ylab("Committee")
}


getMeetingDates =
function(comId, con = mkASISCon())
{    
    txt = getForm("https://asis.ucdavis.edu/committee_v2/view_committee.cfm", id = comId, curl = con)
    doc = htmlParse(txt)
    meet = readHTMLTable(getNodeSet(doc, "//table[contains(., 'Date')]")[[1]])
    meet[] = lapply(meet, normalizeSpace)
    names(meet) = normalizeSpace(names(meet))
    meet$Date = as.Date(meet$Date, "%m/%d/%Y")
    meet[order(meet$Date), ]
}

mkASISCon =
function(...)
{
    k = cookie("~/asis.cookie")
    getCurlHandle(cookie = k, ...)
}

