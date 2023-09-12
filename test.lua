-- local myXML = require("myXML")
-- local new = myXML.xmlToItem("<test>asdf<a>asdf</a><b option=\"t/est\"/>asdf</test>")
-- local prt = require("pretty-print").prettyPrint
-- prt(new)

-- local xml = myXML.itemToXml(new)
-- prt(xml)


local myxml = require"myxml-next"

local element = myxml.element

local data = element.new("body",nil,{
    element.new("div",{qwreey="babo"},{
    }),
    element.new("div",nil,{
        element.new("p",nil,{"wow"})
    }),
    "wow"
})

data:push(element.new("img"))
local style = element.new("style")
data:unshift(style)
-- data:splice(1,2)
style:destroy()

print(element.debug(data))
