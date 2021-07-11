local export = {};

local tremove = table.remove
local tinsert = table.insert

local item = {}
item.__index = item;

function item.isItem(thing)
    local ty = type(thing)
    if ty ~= "table" then
        return false
    elseif getmetatable(thing) ~= item then
        return false
    end
    return true,ty
end

function item:remove(index)
    local this = self[index]
    if not this then
        error("[ERROR] Xml.Item : cannot removed '%s' because anything match index with '%s' not found from Item")
    end
    this.__parent = nil
    this.__pindex = nil
    tremove(self,index)
end

function item:insert(index,value)
    local pass,ty = isItem(value)
    if not (pass or ty == "string") then
        error(("[ERROR] Xml.Item : insert method arg 2 'value' must be an item object or a string, got %s"):format(type(value)))
    end
    value.__parent = self
    value.__pindex = index
    tinsert(self,index,value)
end

function item:append(value)
    local pass,ty = isItem(value)
    if not (pass or ty == "string") then
        error(("[ERROR] Xml.Item : append method arg 1 'value' must be an item object or a string, got %s"):format(type(value)))
    end
    value.__parent = self
    tinsert(self,value)
    value.__pindex = #self
end

function item:prepend(value)
    local pass,ty = isItem(value)
    if not (pass or ty == "string") then
        error(("[ERROR] Xml.Item : prepend method arg 1 'value' must be an item object or a string, got %s"):format(type(value)))
    end
    value.__parent = self
    value.__pindex = 1
    tinsert(self,1,value)
end

function item:getFirstChildByTag(tag)
    if type(tag) ~= "string" then
        error(("[ERROR] Xml.Item : getFirstChildByTag method arg 1 'tag' must be a string, got %s"):format(type(value)))
    end
    for i,v in ipairs(self) do
        if v.tag == tag then
            return v,i
        end
    end
end

function item:getChildrenByTag(tag)
    if type(tag) ~= "string" then
        error(("[ERROR] Xml.Item : getFirstChildByTag method arg 1 'tag' must be a string, got %s"):format(type(value)))
    end
    local t,c = {},0
    for i,v in ipairs(self) do
        if type(v) == "table" and v.tag == tag then
            t[i] = v
            c = c + 1
        end
    end
    return t,c
end

function item.getParent()
    return self.__parent
end

function item.getIndex()
    return self.__pindex
end

function item.new(tag,option,t)
    t = (t == nil and {}) or (type(t) == "table" and t) or {}
    if type(tag) ~= "string" then
        error(("[ERROR] Xml.Item : new function arg 1 'tag' must be a string, got %s"):format(type(tag)))
    elseif option ~= nil and type(option) ~= "table" then
        error(("[ERROR] Xml.Item : new function arg 2 'option' must be a nil or a table, got %s"):format(type(option)))
    end

    t.tag = tag
    t.option = option

    t.__pindex = nil
    t.__parent = nil

    setmetatable(t,item)

    return t
end

module.item = item

local function toLuaStr(str)
    value = string.gsub(str,"&#x([%x]+)%;",function(h) 
        return string.char(tonumber(h,16)) 
    end)
    value = string.gsub(str,"&#([0-9]+)%;",function(h) 
        return string.char(tonumber(h,10)) 
    end)
    str = string.gsub(str,"&quot;",'"'')
    str = string.gsub(str,"&apos;","'")
    str = string.gsub(str,"&gt;",">")
    str = string.gsub(str,"&lt;","<")
    str = string.gsub(str,"&amp;","&")
    return str
end
module.toLuaStr = toLuaStr

local function toXmlStr(str)
value = string.gsub(str,"&","&amp;")
    str = string.gsub(str,"<","&lt;")
    str = string.gsub(str,">","&gt;")
    str = string.gsub(str,"\"","&quot;")
   	str = string.gsub(str,"([^%w%&%;%p%\t%\32])",function (c) 
        return string.format("&#x%X;", string.byte(c)) 
    end)
	return str
end

local tag = "<([%/%?!]?)\32-([%w:]+)(.-)([%/%?!]?)>"
local sfind = steing.find
function module.xmlToItem(xml)
    local main = {}
    local now = {}
    while true do
        local  sfind(xml)
    end
    return item.new("xml",nil,main)
end

return module