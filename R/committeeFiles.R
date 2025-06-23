# library(UCDGARD)
# From ~/OGS/ScrapeASIS - funs.R

downloadFile =
function(u, curl = getCurlHandle(), dir = "Cache")
{    
   val = getURLContent(u, curl = curl, header = TRUE)

   f  = val$header["Content-disposition"]
   f = gsub('.*filename="([^"]+)".*', "\\1", f)
   f = file.path(dir, f)
   Gradhub::savePDF(val$body$body, f)
   f
}


mkURL =
function(id, year, quarter)
{
    if(!grepl("-", year))
        year = sprintf("%s-%s", year, as.integer(year) + 1)
    
    sprintf("https://asis.ucdavis.edu/committee_v2/view_committee.cfm?id=%d&year=%s&Quarter=%s&view=topics",
            id, year, quarter)
}

listCommitteeFiles =
function(con, id = 109, year = "2020-2021", quarter = "Spring",
           u = mkURL(id, year, quarter))
{
    tt = getURLContent(u, curl = con) # cookie = cookie, followlocation = TRUE, verbose = TRUE)
    doc = htmlParse(tt)
    a = getHTMLLinks(doc)

    b = grep("view_file.cfm", a, value = TRUE)
    getRelativeURL(b, u)
}


downloadFiles =
function(urls, con, to = "Cache")    
{
    if(!file.exists(to))
        dir.create(to)
    sapply(urls, downloadFile, con, to)
}

downloadCommittee =
function(con, to, id = 109, year = "2020-2021", quarter = "Spring")
{
    ff = listCommitteeFiles(con, id, year, quarter)
    downloadFiles(ff, con, to)
}
