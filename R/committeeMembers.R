# https://asis.ucdavis.edu/committee_v2/view_committee.cfm?id=191&year=2020-2021&Quarter=
# From ~/OGS/Senate/Committees/get.R




if(FALSE) {
    # committees = c(gc = 109, prc = 191, prcc = 233, apd = 176, admin = 209, bylaws = 208, courses = 110, epc = 177, welfare = 242)
    # See CommitteeIds
    years = 1994:2024
    con = mkASISCon()
    a = lapply(CommitteeIds, function(id) structure(lapply(years, getMembers, id = id, con = con), names = years))
    names(a) = names(CommitteeIds)
    saveRDS(a, "GradCommitteeMembers.rds")


    comb = lapply(a, function(x) {
        tmp = do.call(rbind, x)
        tmp$year = rep(names(x), sapply(x, nrow))
        tmp
    })

    sapply(comb, function(x) sort(table(x$name[x$role != 'Staff Advisor']), decreasing = TRUE))


    all = do.call(rbind, comb)
    all$committee = rep(names(comb), sapply(comb, nrow))
}

# See mkASISCon()
if(FALSE) {
mkCon =
function(k = cookie("cookie"), ...)
  getCurlHandle(cookie = k, followlocation = TRUE)
}


getMembers =
    # getMembers(2024, )
    # id is the committee id. Can be 
function(year, id = 191, con = mkASISCon(...), ..., url = "https://asis.ucdavis.edu/committee_v2/view_committee.cfm")
{
    if(is.character(id) && !grepl("^[0-9]+$", id)) 
        id = CommitteeIds[tolower(id)]

    if(is.character(year))
        year = as.integer(year)
    
    tt = getForm(url, id = id, year = sprintf("%d-%d", year, year+1), Quarter = "", curl = con)
    doc = htmlParse(tt)

    readMembers(doc)
}


readMembers =
function(doc)    
{
#    divs = getNodeSet(doc, "//div[@id = 'members']//div[@class = 'section_content']/div[@class='member_info']")

    k = c("name", "dept_name")
    
    els = lapply(k,
           function(c)
              normalizeSpace(xpathSApply(doc, sprintf("//div[@id = 'members']//div[@class = 'section_content']/div[@class='member_info']/div[@class = '%s']", c), xmlValue, trim = TRUE)))

    d = as.data.frame(els)
    names(d) = c("name", "department")
    
    d$role = gsub(" - .*", "", d$name)
    d$name = gsub(".* - ", "", d$name)
    d$department = gsub(".*: ", "", d$department)
    d
}


history =
    # ???
function(x)    
{
    tt = table(x)
    paste(as.integer(tt), names(tt), sep = "-", collapse = ", ")
}
