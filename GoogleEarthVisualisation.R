
'''

Author: Neha Taneja
Data source: https://en.m.wikipedia.org/wiki/List_of_countries_by_refugee_population#By_country_of_origin

'''

#Load the data of LatLon, a dataframe with Country.Name, CIA.Codes, Latitude, Longitude
load("~/Documents/hw8/hw8.rda")

install.packages(c("XML", "RCurl"))

library(XML)
library(RCurl)

## XML library tested and resulted document is saved as test_xml
doc = newXMLDoc()
parent = newXMLNode("parent", doc = doc)
child = newXMLNode("child", "leaf node", parent = parent)

saveXML(doc, indent = TRUE, file = "~/Documents/hw8/test_xml.xml")

## RCurl test

text = getURL("http://ptrckprry.com/course/ssd/data/positive-words.txt")

# url from where list of countries by refugee population has to be fetched.
url = "https://en.m.wikipedia.org/wiki/List_of_countries_by_refugee_population"


listOfCountriesByRefugeePopulationHtmlPage = getURL(url = url)


# Working on the table UNHCR registered refugees by country/territory of asylum between mid-2015 and 2007

refugee  = readHTMLTable(listOfCountriesByRefugeePopulationHtmlPage)[[1]]

# Replaced the appropriate column names
refugee = refugee[, c(1, 2, 3)]

names(refugee[, c(1, 2, 3)]) <-
    c("Country.Name", "refugees_per_1000", "refugee_2015")

# Created a dataframe called AllData by merging Country.Name of refugee table with LatLon table
refugee[, "Country.Name"] = toupper(refugee[, "Country.Name"])

AllData = merge(x = refugee, y = LatLon, by = "Country.Name")


# Missing values are replaced with medians in order to clean the data
replaceMissingByMedian = function(col) {
    numericCol = as.numeric(gsub(",", "", as.character(col)))
    if (length(numericCol[!is.na(numericCol)]) > 0) {
        numericCol[is.na(numericCol)] = median(numericCol, na.rm = TRUE)
        col = numericCol
    }
    return(col)
}

AllData = apply(AllData, 2, replaceMissingByMedian)


# function to make the base document with the name Refugee_Distribution
makeBaseDocument = function() {
    ### This code creates the template for KML document
    ### Your code here 
    kmlDoc = newXMLDoc("Refugee_Distribution")
    kml = newXMLNode("kml", namespaceDefinitions  = "http://www.opengis.net/kml/2.2", doc = kmlDoc)
    document = newXMLNode("Document", parent = kml)
    return(kmlDoc)
    
}

# function to add placemark on Google Earth
addPlacemark = function(lat,
                        lon,
                        ctryCode,
                        ctryName,
                        ref,
                        ref1000,
                        parent,
                        ref1000Cut,
                        refCut,
                        style = FALSE) {
    pm = newXMLNode(
        "Placemark",
        newXMLNode("name", ctryName),
        attrs = c(id = ctryCode),
        parent = parent
    )
    newXMLNode(
        "description",
        paste(
            ctryName,
            "\n Refugee Population: ",
            ref,
            "\n Refugees per 1,000 inhabitants: ",
            ref1000,
            sep = ""
        ),
        parent = pm
    )
    
    newXMLNode("Point", newXMLNode("coordinates", paste(lat, lon, sep = ",")), parent = pm)
    
    ### You need to fill in the code for making the Point node above,
    ### including coordinates. The line below won't work until you've run
    ### the code for the next section to set up the styles.
    
    if (style)
        newXMLNode("styleUrl",
                   paste("#YOR", ref1000Cut, "-", refCut, sep = ''),
                   parent = pm)
}

# Created kmlDoc
kmlDoc = makeBaseDocument()

document = xmlRoot(kmlDoc)["Document"]

addPlacemarks = function(row) {
    addPlacemark(row[5], row[6], row[4], row[1], row[3], row[2], document, 1, 1)
}

apply(AllData, 1, addPlacemarks)

# saved the kmlDoc as Part2 kml to be opened via Google Earth
saveXML(kmlDoc, "~/Documents/hw8/Part2.kml")

### Part III.  Add Style to your KML
### Now you are going to make the visualizatiion a bit fancier. To be more specific, instead of pushpins, we
### want different circle labels for countris with size refugee population and the color representing
### the refugees rate.
### Pretty much all the code is given to you below to create style elements.
### Here, you just need to figure out what it all does.

### Start fresh with a new KML document, by calling makeBaseDocument()

doc2 = makeBaseDocument()

ref1000Cut = cut(as.numeric(AllData[, "refugees_per_1000"]), breaks = c(0, 10, 25, 50, 75, 225))
ref1000Cut = as.numeric(ref1000Cut)
refCut = cut(as.numeric(AllData[, "refugee_2015"]), breaks = 5)
refCut = as.numeric(refCut)


# Added style to kml document 
scale = c(1, 2, 3, 4, 5)
colors = c("blue", "green", "yellow", "orange", "red")

addStyle = function(col1, pop1, parent, DirBase, scales = scale) {
    st = newXMLNode("Style",
                    attrs = c("id" = paste("YOR", col1, "-", pop1, sep =
                                               "")),
                    parent = parent)
    newXMLNode(
        "IconStyle",
        newXMLNode("scale", scales[pop1]),
        newXMLNode("Icon",
                   newXMLNode(
                       "href",
                       paste(DirBase, "color_label_circle_", colors[col1], ".png", sep = "")
                   )),
        parent = st
    )
}


root2 = xmlRoot(doc2)
DocNode = root2[["Document"]]


for (k in 1:5) {
    for (j in 1:5) {
        addStyle(j, k, DocNode, 'color_label_circle/')
    }
}


for (rowIndex in 1:nrow(AllData)) {
    row = AllData[rowIndex, ]
    addPlacemark(row[5], row[6], row[4], row[1], row[3], row[2], DocNode, ref1000Cut[rowIndex], refCut[rowIndex], style = TRUE)
}

saveXML(doc2, "~/Documents/hw8/Part3.kml")
