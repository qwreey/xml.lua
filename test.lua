local myXML = require("myXML")
local new = myXML.xmlToItem("<test>asdf<a>asdf</a><b option=\"t/est\"/>asdf</test>")
local prt = require("pretty-print").prettyPrint
prt(new)

local xml = myXML.itemToXml(new)
prt(xml)