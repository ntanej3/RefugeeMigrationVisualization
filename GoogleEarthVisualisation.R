"
title: Refugee population migration analysis
author: Neha Taneja

This document is used to analyze the refugee population migration for the year 2015.

Data source: https://en.m.wikipedia.org/wiki/List_of_countries_by_refugee_population#By_country_of_origin
"

install.packages(c("XML", "RCurl"))
library(XML)
library(RCurl)

#Load the data of LatLon, a dataframe with Country.Name, CIA.Codes, Latitude, Longitude
load("LatLon.rda")

# url from where list of countries by refugee population has to be fetched.
listOfCountriesByRefugeePopulationHtmlPage = getURL(url = "https://en.m.wikipedia.org/wiki/List_of_countries_by_refugee_population")

# Working on the table UNHCR registered refugees by country/territory of asylum between mid-2015 and 2007
refugee  = readHTMLTable(listOfCountriesByRefugeePopulationHtmlPage)[[2]]

# Replaced the appropriate column names
refugee = refugee[-1, c(1, 2, 3)]

colnames(refugee) <- c("Country.Name", "refugees_per_1000", "refugee_2015")

# Created a dataframe called AllData by merging Country.Name of refugee table with LatLon table
refugee[, "Country.Name"] = toupper(refugee[, "Country.Name"])

allData = merge(x = refugee, y = LatLon, by = "Country.Name")

# Missing values are replaced with medians in order to clean the data
replaceMissingByMedian = function(col) {
    numericCol = as.numeric(gsub(",", "", as.character(col)))
    if (length(numericCol[!is.na(numericCol)]) > 0) {
        numericCol[is.na(numericCol)] = median(numericCol, na.rm = TRUE)
        col = numericCol
    }
    return(col)
}

allData = apply(allData, 2, replaceMissingByMedian)


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
    
    if (style)
        newXMLNode("styleUrl",
                   paste("#YOR", ref1000Cut, "-", refCut, sep = ''),
                   parent = pm)
}

# Create kmlDoc
kmlDoc = makeBaseDocument()

document = xmlRoot(kmlDoc)["Document"]

addPlacemarks = function(row) {
    addPlacemark(row[5], row[6], row[4], row[1], row[3], row[2], document, 1, 1)
}

apply(allData, 1, addPlacemarks)

saveXML(kmlDoc, "refugee_population.kml")

# Refugee population visualization with visible scale of migratrion.
doc2 = makeBaseDocument()

ref1000Cut = cut(as.numeric(allData[, "refugees_per_1000"]), breaks = c(0, 10, 25, 50, 75, 225))
ref1000Cut = as.numeric(ref1000Cut)
refCut = cut(as.numeric(allData[, "refugee_2015"]), breaks = 5)
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


for (rowIndex in 1:nrow(allData)) {
    row = allData[rowIndex, ]
    addPlacemark(row[5], row[6], row[4], row[1], row[3], row[2], DocNode, ref1000Cut[rowIndex], refCut[rowIndex], style = TRUE)
}

saveXML(doc2, "scale_of_migration_of_refugee_population.kml")

zip(zipfile = "scale_of_migration_of_refugee_population.kmz", files = c("scale_of_migration_of_refugee_population.kml", "color_label_circle"))

file.remove("scale_of_migration_of_refugee_population.kml")
