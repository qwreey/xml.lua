local module = {};

-- TODO: 코맨트 무시 필요, !CDATA 읽기 수정 필요

local tremove = table.remove
local tinsert = table.insert
local sgsub = string.gsub
local sgmatch = string.gmatch
local smatch = string.match
local srep = string.rep
local tconcat = table.concat
local sbyte = string.byte
local sformat = string.format
local sfind = string.find
local ssub = string.sub
local schar = string.char

---@class xmlItem
local item = {}
item.__index = item

-- check is item instance
function item.isItem(thing)
	local ty = type(thing)
	if ty ~= "table" then
		return false,ty
	elseif getmetatable(thing) ~= item then
		return false,ty
	end
	return true,ty
end
local isItem = item.isItem

---Remove item with index
---@param index number index of item that you want to remove
---@return string|nil|xmlItem removed removed item
function item:remove(index)
	local this = self[index]
	if not this then
		error("[ERROR] Xml.Item : cannot removed '%s' because anything match index with '%s' not found from Item")
	end
	this.__parent = nil
	this.__pindex = nil
	return tremove(self,index)
end

---Remove item with value (find and remove)
---@param value string|xmlItem item to remove
---@return boolean passed
function item:removeValue(value)
	local pass,ty = isItem(value)
	if not (pass or ty == "string") then
		error(("[ERROR] Xml.Item : removeValue method arg 1 'value' must be an item object or a string, but got %s"):format(type(value)))
	end
	for i,v in pairs(self) do
		if v == value then
			if ty ~= "string" then
				value.__parent = nil
				value.__pindex = nil
			end
			tremove(self, i)
			return true
		end
	end
	return false
end

---Add new item
---@param index number index of new item
---@param value string|xmlItem item to add
function item:insert(index,value)
	local pass,ty = isItem(value)
	if not (pass or ty == "string") then
		error(("[ERROR] Xml.Item : insert method arg 2 'value' must be an item object or a string, but got %s"):format(type(value)))
	end
	tinsert(self,index,value)
	if ty ~= "string" then
		value.__parent = self
		value.__pindex = index
	end
end

---Append new item
---@param value string|xmlItem new item to append
---@return number index where this item was added
function item:append(value)
	local pass,ty = isItem(value)
	if not (pass or ty == "string") then
		error(("[ERROR] Xml.Item : append method arg 1 'value' must be an item object or a string, but got %s"):format(type(value)))
	end
	tinsert(self,value)
	if ty ~= "string" then
		value.__parent = self
		value.__pindex = #self
	end
	return #self
end

---Prepend new item
---@param value string|xmlItem new item to prepend
function item:prepend(value)
	local pass,ty = isItem(value)
	if not (pass or ty == "string") then
		error(("[ERROR] Xml.Item : prepend method arg 1 'value' must be an item object or a string, but got %s"):format(type(value)))
	end
	if ty ~= "string" then
		value.__parent = self
		value.__pindex = 1
	end
	tinsert(self,1,value)
end

--[[!autoDoc!
@define class:item
	function item:getFirstChildByTag(tag)
		i: get child object by using tag
		arg1 string tag: tag of child
		return: class:item/nil child item , int index
!autoDoc!]]
---get child object by using tag
---@param tag string tag of child
---@return xmlItem|nil childitem
---@return number|nil index
function item:getFirstChildByTag(tag)
	if type(tag) ~= "string" then
		error(("[ERROR] Xml.Item : getFirstChildByTag method arg 1 'tag' must be a string, but got %s"):format(type(tag)))
	end
	for i,v in ipairs(self) do
		if v.tag == tag then
			return v,i
		end
	end
end

--[[!autoDoc!
@define class:item
	function item:getChildrenByTag(tag)
		i: get child objects by using tag
		arg string tag: tag of children
		return: table of class:item/nil
!autoDoc!]]
---get child objects by using tag
---@param tag string tag of children
---@return table children table of children
---@return number count length of children array
function item:getChildrenByTag(tag)
	if type(tag) ~= "string" then
		error(("[ERROR] Xml.Item : getFirstChildByTag method arg 1 'tag' must be a string, but got %s"):format(type(tag)))
	end
	local t,c = {},0
	for _,v in ipairs(self) do
		if type(v) == "table" and v.tag == tag then
			tinsert(t,v)
			c = c + 1
		end
	end
	return t,c
end

--[[!autoDoc!
@define class:item
	function item:getParent()
		i: get parent of item
		return: item/nil
!autoDoc!]]
---get parent of this item
---@return xmlItem|nil
function item:getParent()
	return self.__parent
end

--[[!autoDoc!
@define class:item
	function item:getIndex()
		i: get index of item from parent item
		return: int/nil
!autoDoc!]]
---get index in parent object
---@return number index
function item:getIndex()
	return self.__pindex
end

--[[!autoDoc!
@define class:item
	function item:destroy()
		i: destroy it self and remove from parent item (if is exist)
!autoDoc!]]
---destroy it self and remove from parent item (if is exist)
function item:destroy()
	local p = self.__parent
	if p then
		p:removeValue(self)
	end

	self.__parent = nil
	self.__pindex = nil

	setmetatable(self,nil)
end

--[[!autoDoc!
@define calss:item
	i: make new handler of xml item
	string item.tag
		i: tag of itself, it must be string
	table/nil item.option
		i: option of itself, you can make this property to empty with nil
	string/calss:item item[n]
		i: child object, you can modify with table lib and methods of class:item
	function item.new(tag,option,t)
		i: make new item
		arg1 string tag: tag of new item, it means <tag></tag>
		arg2 table/nil option: option of new item, it means <a option></a>
		arg3 table/nil t: include child items, you can make empty this
		return: class:item
!autoDoc!]]
---make new handler of xml item
---@param tag string tag of new item, it means <tag></tag>
---@param option table|nil of itself, you can make this property to empty with nil
---@param t table|nil include child items, you can make empty this
---@return xmlItem
function item.new(tag,option,t)
	t = (t == nil and {}) or (type(t) == "table" and t) or {}
	if type(tag) ~= "string" then
		error(("[ERROR] Xml.Item : new function arg 1 'tag' must be a string, but got %s"):format(type(tag)))
	elseif option ~= nil and type(option) ~= "table" then
		error(("[ERROR] Xml.Item : new function arg 2 'option' must be a nil or a table, but got %s"):format(type(option)))
	end

	t.tag = tag
	t.option = option

	t.__pindex = nil
	t.__parent = nil

	setmetatable(t,item)

	return t
end
module.item = item

local function hexStr(h)
	return schar(tonumber(h,16))
end
local function decStr(h)
	return schar(tonumber(h,10))
end
local function toLuaStr(str)
	str = sgsub(str,"&#x([%x]+);",hexStr)
	str = sgsub(str,"&#([0-9]+);",decStr)
	str = sgsub(str,"&quot;",'"')
	str = sgsub(str,"&apos;","'")
	str = sgsub(str,"&gt;",">")
	str = sgsub(str,"&lt;","<")
	str = sgsub(str,"&amp;","&")
	str = sgsub(str,"\\n","\n")
	str = sgsub(str,"\\t","\t")
	return str
end
module.toLuaStr = toLuaStr

local function strToHex(c)
	return sformat("&#x%X;", sbyte(c))
end
local function toXmlStr(str)
	str = sgsub(str,"&","&amp;")
	str = sgsub(str,"<","&lt;")
	str = sgsub(str,">","&gt;")
	str = sgsub(str,"\"","&quot;")
   	str = sgsub(str,"([^%w%&%;%p%\t%\32])",strToHex)
	str = sgsub(str,"\n","\\n")
	str = sgsub(str,"\t","\\t")
	return str
end
module.toXmlStr = toXmlStr

-- <a option1="test" option2="test2"></a>
-- => {option1 = "test", option2 = "test2"}
local optionFind = "\32-([%w-_]+)\32-="
local function strToOption(str)
	local option = {};
	while true do
		local nameStart,nameEnd,name = sfind(str,optionFind)
		if not nameStart then
			return option;
		end
		local qtStart,_ = sfind(str,"\"",nameEnd+1)
		local qtEnd,_ = sfind(str,"\"",qtStart+1)
		option[name] = toLuaStr(ssub(str,qtStart+1,qtEnd-1))
		str = ssub(str,qtEnd+1,-1)
	end
	return option
end
module.strToOption = strToOption

-- {option1 = "test", option2 = "test2"}
-- => <a option1="test" option2="test2"></a>
local optionFormat = '%s%s="%s" '
local function optionToStr(option)
	local str = ""
	if not option then
		return str
	end
	for i,v in pairs(option) do
		str = optionFormat:format(str,i,toXmlStr(v))
	end
	return ssub(str,1,-2)
end
module.optionToStr = optionToStr

local holderTag = "myXML_Main"
local tagFind = "<([%/%?!]?)[\32\t\n\r]-([%w:%-_]+)(.-)([%/%?!]?)>"
function module.xmlToItem(xml)
	local main = item.new(holderTag)
	local stack = {}
	local now = main
	while true do
		local posStart,posEnd,prefix,tag,option,suffix = sfind(xml,tagFind)
		if posStart == nil then
			break
		end
		option = strToOption(option)

		-- append string
		local str = ssub(xml,1,posStart-1)
		if sfind(str,"[^\32\n]") then
			now:append(str)
		end

		if suffix and suffix ~= "" then -- self start, self end
			now:append(item.new(tag,option))
		elseif prefix and prefix ~= "" then -- end last tag
			if now.tag ~= tag then
				error(("[ERROR] Xml.xmlToItem : start tag and end tag is not match! startTag: %s; endTag: %s;"):format(now.tag,tag))
			end
			tremove(stack)
			now = stack[#stack] or main
		else
			local this = item.new(tag,option)
			now:append(this)
			now = this
			tinsert(stack,this)
		end

		xml = string.sub(xml,posEnd+1,-1)
	end
	setmetatable(main,nil)
	main.tag = nil
	main.option = nil
	return main
end

local function itemToXml(thing)
	local pass,ty = isItem(thing)
	if not pass then
		if ty == "table" then
			local str = {}
			for _,v in ipairs(thing) do
                tinsert(str,itemToXml(v))
			end
			return tconcat(str," ")
		end
		return thing
	end
	local optStr = optionToStr(thing.option)
	local tag = thing.tag
	if #thing == 0 then
		return ("<%s%s%s/>"):format(tag,optStr == "" and "" or "\32",optStr)
	end
	local str = ("<%s%s%s>%%s</%s>"):format(tag,optStr == "" and "" or "\32",optStr,tag)
	local child = ""
	for i,v in ipairs(thing) do
		child = child .. itemToXml(v)
	end
	return str:format(child)
end

function module.itemToXml(nitem)
	local pass,ty = isItem(nitem)
	if not (pass or ty == "table") then
		error(("[ERROR] Xml.itemToXml : arg 1 'nitem' must be a table or an item, but got %s"):format(ty))
	end
	return itemToXml(nitem)
end

local selectorParser = {} do
	module.selectorParser = selectorParser
    local opRegex = {}
    local opNames = {}
	local accessNames = {}

    local spec = true

	-- End of selector string
	local OP_EOF = #opNames+1
    tinsert(opRegex,"^[ \n\t]*$")
    tinsert(opNames,"OP_EOF")

    -- Property selector open and close
    local OP_PROPERTY_OPEN = #opNames+1
	local REG_PROPERTY_OPEN = "^%[[ \t\n]*"
    tinsert(opRegex,REG_PROPERTY_OPEN)
    tinsert(opNames,"OP_PROPERTY_OPEN")
    local OP_PROPERTY_CLOSE = #opNames+1
	local REG_PROPERTY_CLOSE = "^[ \t\n]*%]"
    tinsert(opRegex,REG_PROPERTY_CLOSE)
    tinsert(opNames,"OP_PROPERTY_CLOSE")

    -- Property selector matcher
    local OP_PROPERTY_CONDITION_EQUAL = #opNames+1
    tinsert(opRegex,[[^[ \t\n]*=[ \t\n]*]])
    tinsert(opNames,"OP_PROPERTY_CONDITION_EQUAL")
    local OP_PROPERTY_CONDITION_WORD = #opNames+1
    tinsert(opRegex,[[^[ \t\n]*~=[ \t\n]*]])
    tinsert(opNames,"OP_PROPERTY_CONDITION_WORD")
    local OP_PROPERTY_CONDITION_DASH = #opNames+1
    tinsert(opRegex,[[^[ \t\n]*|=[ \t\n]*]])
    tinsert(opNames,"OP_PROPERTY_CONDITION_DASH")
    local OP_PROPERTY_CONDITION_BEGIN = #opNames+1
    tinsert(opRegex,[[^[ \t\n]*%^=[ \t\n]*]])
    tinsert(opNames,"OP_PROPERTY_CONDITION_BEGIN")
    local OP_PROPERTY_CONDITION_END = #opNames+1
    tinsert(opRegex,[[^[ \t\n]*$=[ \t\n]*]])
    tinsert(opNames,"OP_PROPERTY_CONDITION_END")
    local OP_PROPERTY_CONDITION_CONTAIN = #opNames+1
    tinsert(opRegex,[[^[ \t\n]*%*=[ \t\n]*]])
    tinsert(opNames,"OP_PROPERTY_CONDITION_CONTAIN")

    -- Strings
    local OP_STRING_DOUBLE_QUOTE = #opNames+1
    tinsert(opRegex,[[^"]])
    tinsert(opNames,"OP_STRING_DOUBLE_QUOTE")
    local OP_STRING_SINGLE_QUOTE = #opNames+1
    tinsert(opRegex,[[^']])
    tinsert(opNames,"OP_STRING_SINGLE_QUOTE")

    -- Abstract selector
    local OP_ABSTRACT_EMPTY = #opNames+1
    tinsert(opRegex,[[^[ \t\n]*:empty]])
    tinsert(opNames,"OP_ABSTRACT_EMPTY")
    local OP_ABSTRACT_FIRST_CHILD = #opNames+1
    tinsert(opRegex,[[^[ \t\n]*:first%-child]])
    tinsert(opNames,"OP_ABSTRACT_FIRST_CHILD")
    local OP_ABSTRACT_FIRST_OF_TYPE = #opNames+1
    tinsert(opRegex,[[^[ \t\n]*:first%-of%-type]])
    tinsert(opNames,"OP_ABSTRACT_FIRST_OF_TYPE")
    local OP_ABSTRACT_LAST_CHILD = #opNames+1
    tinsert(opRegex,[[^[ \t\n]*:last%-child]])
    tinsert(opNames,"OP_ABSTRACT_LAST_CHILD")
    local OP_ABSTRACT_LAST_OF_TYPE = #opNames+1
    tinsert(opRegex,[[^[ \t\n]*:last%-of%-type]])
    tinsert(opNames,"OP_ABSTRACT_LAST_OF_TYPE")
    local OP_ABSTRACT_NOT = #opNames+1
    tinsert(opRegex,[[^[ \t\n]*:not]])
    tinsert(opNames,"OP_ABSTRACT_NOT")
    local OP_ABSTRACT_NTH_CHILD = #opNames+1
    tinsert(opRegex,[[^[ \t\n]*:nth%-child]])
    tinsert(opNames,"OP_ABSTRACT_NTH_CHILD")
    local OP_ABSTRACT_NTH_LAST_CHILD = #opNames+1
    tinsert(opRegex,[[^[ \t\n]*:nth%-last%-child]])
    tinsert(opNames,"OP_ABSTRACT_NTH_LAST_CHILD")
    local OP_ABSTRACT_NTH_LAST_OF_TYPE = #opNames+1
    tinsert(opRegex,[[^[ \t\n]*:nth%-last%-of%-type]])
    tinsert(opNames,"OP_ABSTRACT_NTH_LAST_OF_TYPE")
    local OP_ABSTRACT_NTH_OF_TYPE = #opNames+1
    tinsert(opRegex,[[^[ \t\n]*:nth%-of%-type]])
    tinsert(opNames,"OP_ABSTRACT_NTH_OF_TYPE")
    local OP_ABSTRACT_OLNY_OF_TYPE = #opNames+1
    tinsert(opRegex,[[^[ \t\n]*:only%-of%-type]])
    tinsert(opNames,"OP_ABSTRACT_OLNY_OF_TYPE")
    local OP_ABSTRACT_ONLY_CHILD = #opNames+1
    tinsert(opRegex,[[^[ \t\n]*:only%-child]])
    tinsert(opNames,"OP_ABSTRACT_ONLY_CHILD")
	-- TODO: firefox's abstract selector such as :is()
	-- TODO: :is() is useful. see :is(h1,h2,h3):not(.colored)

    -- Abstract option open and close
    local OP_ABSTRACT_OPTION_OPEN = #opNames+1
    tinsert(opRegex,[[^[ \t\n]*%(]])
    tinsert(opNames,"OP_ABSTRACT_OPTION_OPEN")
    local OP_ABSTRACT_OPTION_CLOSE = #opNames+1
    tinsert(opRegex,[[^[ \t\n]*%)]])
    tinsert(opNames,"OP_ABSTRACT_OPTION_CLOSE")

    -- Escape
    local OP_ESCAPE = #opNames+1
    tinsert(opRegex,[[^\]])
    tinsert(opNames,"OP_ESCAPE")

    -- Access
    local OP_ACCESS_DIRECT_CHILD = #opNames+1
	local REG_ACCESS_DIRECT_CHILD = [[^[ \t\n]*>[ \t\n]*]]
	accessNames[OP_ACCESS_DIRECT_CHILD] = "DIRECT_CHILD"
    tinsert(opRegex,REG_ACCESS_DIRECT_CHILD)
    tinsert(opNames,"OP_ACCESS_DIRECT_CHILD")
    local OP_ACCESS_SIBLING = #opNames+1
	local REG_ACCESS_SIBLING = [[^[ \t\n]*%+[ \t\n]*]]
	accessNames[OP_ACCESS_SIBLING] = "SIBLING"
    tinsert(opRegex,REG_ACCESS_SIBLING)
    tinsert(opNames,"OP_ACCESS_SIBLING")
    local OP_ACCESS_AFTER = #opNames+1
	local REG_ACCESS_AFTER = [[^[ \t\n]*~[ \t\n]*]]
	accessNames[OP_ACCESS_AFTER] = "AFTER"
    tinsert(opRegex,REG_ACCESS_AFTER)
    tinsert(opNames,"OP_ACCESS_AFTER")

    -- More
    local OP_MORE = #opNames+1
    tinsert(opRegex,[[^[ \t\n]*,[ \t\n]*]])
    tinsert(opNames,"OP_MORE")

    -- DFS
    local OP_ACCESS_DESCENDANT = #opNames+1
	accessNames[OP_ACCESS_DESCENDANT] = "DESCENDANT"
    tinsert(opRegex,[[^[ \t\n]+]])
    tinsert(opNames,"OP_ACCESS_DESCENDANT")

    -- Tag Name
    local OP_TAG = #opNames+1
    tinsert(opRegex,"^([%-_%w]+)")
    tinsert(opNames,"OP_TAG")

    -- class
    local OP_CLASS = #opNames+1
    tinsert(opRegex,"^%.([%-_%w]+)")
    tinsert(opNames,"OP_CLASS")

    -- id
    local OP_ID = #opNames+1
    tinsert(opRegex,"^#([%-_%w]+)")
    tinsert(opNames,"OP_ID")

    -- All
    local OP_WILDCARD = #opNames+1
    tinsert(opRegex,"^%*")
    tinsert(opNames,"OP_WILDCARD")

	-- utf8 char
	local OP_UTFCHAR = #opNames+1
	tinsert(opRegex,"("..utf8.charpattern..")")
	tinsert(opNames,"OP_UTFCHAR")

	-- condition types
	local conditionTypes = {}
	local CONDITION_CLASS = #conditionTypes+1
	tinsert(conditionTypes,"CLASS")
	local CONDITION_ID = #conditionTypes+1
	tinsert(conditionTypes,"ID")
	local CONDITION_TAG = #conditionTypes+1
	tinsert(conditionTypes,"TAG")
	local CONDITION_WILDCARD = #conditionTypes+1
	tinsert(conditionTypes,"WILDCARD")
	local CONDITION_PROPERTY = #conditionTypes+1
	tinsert(conditionTypes,"PROPERTY")

	-- indexes
	local INDEX_CONDITION_TYPE = 1
	local INDEX_CONDITION_VALUE = 2
	local INDEX_CONDITION_OPTION = 3
	local INDEX_SEGMENT_CONDITION = 1
	local INDEX_SEGMENT_ACCESSMODE = 2

    local nthDigit = {
        [0] = "%dth",
        "%dst", -- 1st 11st
        "%dnd", -- 2nd 12nd
        "%drd", -- 3rd 13rd
        "%dth","%dth","%dth","%dth","%dth","%dth",
    }
    local function formatNTH(n)
        return sformat(nthDigit[n%10],n)
    end
    local stringEscapes = {
        ["'"] = "'",
        ['"'] = '"',
        ["n"] = "\n",
        ["r"] = "\r",
        ["t"] = "\t"
        -- todo: check more escapes
    }

    function selectorParser.parseString(str,init,initChar)
        local pos = init+1
        local buffer = {}
        while true do
            local start,_,char = sfind(str,"([\\'\"])",pos)
            if not start then
                error(("[SelectorParser.ParseString: buffer exhausted] String is opened at %s char (%s). But String is not closed."):format(formatNTH(init),initChar))
            end

            -- insert last string
            tinsert(buffer,ssub(str,pos,start-1))
            pos = start + 1

            if char == initChar then
                -- closed
                break
            elseif char == "\\" then
                -- escape
                local newChar = stringEscapes[ssub(str,pos,pos)]
                if not newChar then
                    error(("[SelectorParser.ParseString: unknown escape] Escape '\\%s', which is %s char is not defined."):format(ssub(str,pos,pos),formatNTH(init)))
                end
                tinsert(buffer,newChar)
                pos = pos + 1
            else
                tinsert(buffer,char)
            end
        end
        return tconcat(buffer),pos
    end
    if spec then
        local ok,result

        ok,result = pcall(selectorParser.parseString,"?  'Test String'  ?",4,"'")
        assert(ok and result == "Test String",sformat("SPEC: not match with expection (result: '%s')",result))

        ok,result = pcall(selectorParser.parseString,"?  'Test String  ?",4,"'")
        assert(not ok,sformat("SPEC: not match with expection (result: '%s')",result))

        ok,result = pcall(selectorParser.parseString,"?  'Test\\nString'  ?",4,"'")
        assert(ok and result == "Test\nString",sformat("SPEC: not match with expection (result: '%s')",result))
    end

	local function buildErrorTrace(str,tokenStart,tokenEnd,opcode)
		return sformat("\n[ErrorTrace] Invalid token started at %s char.\ntoken: '%s' (OPcode: %s)\n%s\n%s%s",
			formatNTH(tokenStart),
			ssub(str,tokenStart,tokenEnd),
			opNames[opcode] or "Invalid",
			str,
			srep("\32",tokenStart-1),
			srep("^",tokenEnd-tokenStart+1)
		)
	end

	local function prettyParsed(parsed)
		local buffer = {"Query {\n"}
		for selectorIndex,selector in ipairs(parsed) do
			tinsert(buffer,sformat("  Selector #%d {\n",selectorIndex))
			for segmentIndex,segment in ipairs(selector) do
				tinsert(buffer,sformat("    Segment #%d: AccessMode = %s {\n",segmentIndex,accessNames[segment[INDEX_SEGMENT_ACCESSMODE]] or "NONE"))
				for conditionIndex,condition in ipairs(segment[INDEX_SEGMENT_CONDITION]) do
					if condition[INDEX_CONDITION_OPTION] then
						tinsert(buffer,sformat("      Condition #%d: Type = %s {\n        Value = '%s'\n        Option = %s\n}\n",conditionIndex,conditionTypes[condition[INDEX_CONDITION_TYPE]],condition[INDEX_CONDITION_VALUE],condition[INDEX_CONDITION_OPTION]))
					else
						tinsert(buffer,sformat("      Condition #%d: Type = %s '%s'\n",conditionIndex,conditionTypes[condition[INDEX_CONDITION_TYPE]],condition[INDEX_CONDITION_VALUE]))
					end
				end
				tinsert(buffer,"    };\n")
			end
			tinsert(buffer,"  };\n")
		end
		tinsert(buffer,"};")
		return tconcat(buffer)
	end

    -- Check if objects belong to negative group
    -- function selectorParser.checkNot(notDescription)
    -- isSubset is not field. if isSubset is true, parse function will return when reachs to closer ")"
    function selectorParser.parse(str,init,isSubset)
		local pos = init or 1
		local lastPos
		local startAt,endAt,matched,op
		local segment = {}
		local selector = {segment}
		local parsed = {selector}
		while true do
			-- get current token
			for index,regex in ipairs(opRegex) do
				startAt,endAt,matched = sfind(str,regex,pos)
				op = index
				if startAt then break end
			end
			if not startAt then break end

			-- End of file (selector)
			if op == OP_EOF then
				break

			-- Access mode
			-- + ~ > space
			elseif
			op == OP_ACCESS_SIBLING or -- +
			op == OP_ACCESS_DIRECT_CHILD or -- >
			op == OP_ACCESS_DESCENDANT or -- space
			op == OP_ACCESS_AFTER then -- ~
				if #selector == 1 and (not segment[INDEX_SEGMENT_CONDITION]) then
					if op == OP_ACCESS_DESCENDANT then
						-- ignore heading spaces
					else
						error("[SelectorParser.parse: accessMode got before the first condition] accessMode cannot be inserted on the front. Possible cause: accessMode is on front (example: > body)"..buildErrorTrace(str,startAt,endAt,op))
					end
				else
					if not segment[INDEX_SEGMENT_CONDITION] then
						error("[SelectorParser.parse: unused accessMode] Last accessMode is not consumed. Possible cause: Repeated accessMode token (example: body > > div)"..buildErrorTrace(str,startAt,endAt,op))
					end
					segment = { [INDEX_SEGMENT_ACCESSMODE] = op }
					tinsert(selector,segment)
				end
				pos = endAt+1

			-- More
			-- ,
			elseif op == OP_MORE then
				if not segment[INDEX_SEGMENT_CONDITION] then
					error("[SelectorParser.parse: Empty last segment condition] Last segment's condition is empty. Possible cause: more token(,) immediately follows accessMode (example: body > ,)"..buildErrorTrace(str,startAt,endAt,op))
				end

				segment = {}
				selector = {segment}
				tinsert(parsed,selector)
				pos = endAt+1

			-- Property
			-- [label=...]
			elseif op == OP_PROPERTY_OPEN then

			elseif op == OP_PROPERTY_CLOSE then
				error("[SelectorParser.parse: unexpected property bracket close] Got unexpected property bracket close. Possible cause: Repeated bracket close or invalid bracket close (example: [label]])"..buildErrorTrace(str,startAt,endAt,op))
			elseif
			op == OP_PROPERTY_CONDITION_BEGIN or
			op == OP_PROPERTY_CONDITION_CONTAIN or
			op == OP_PROPERTY_CONDITION_DASH or
			op == OP_PROPERTY_CONDITION_END or
			op == OP_PROPERTY_CONDITION_EQUAL or
			op == OP_PROPERTY_CONDITION_WORD then
				error("[SelectorParser.parse: unexpected property condition] Condition statement cannot be used outside of the property bracket. Possible cause: Condition statement in invalid position (example: ~=[label])"..buildErrorTrace(str,startAt,endAt,op))

			-- Class
			-- .a
			elseif op == OP_CLASS then
				local condition = segment[INDEX_SEGMENT_CONDITION]
				if not condition then
					condition = {}
					segment[INDEX_SEGMENT_CONDITION] = condition
				end
				tinsert(condition,{[INDEX_CONDITION_TYPE] = CONDITION_CLASS, [INDEX_CONDITION_VALUE] = matched})
				pos = endAt+1

			-- tag
			-- body
			elseif op == OP_TAG then
				local condition = segment[INDEX_SEGMENT_CONDITION]
				if not condition then
					condition = {}
					segment[INDEX_SEGMENT_CONDITION] = condition
				end
				local last = condition[#condition]
				-- if last and last.type == "tag" then
				-- This is little hacky, if got '.wow가가wew' wew will inserted at segment which has 'class' type
				if last then
					last[INDEX_CONDITION_VALUE] = last[INDEX_CONDITION_VALUE] .. matched -- concat more after utf8. (e: a가나다b)
				else
					tinsert(condition,{[INDEX_CONDITION_TYPE] = CONDITION_TAG, [INDEX_CONDITION_VALUE] = matched})
				end
				pos = endAt+1

			-- id
			-- #a
			elseif op == OP_ID then
				local condition = segment[INDEX_SEGMENT_CONDITION]
				if not condition then
					condition = {}
					segment[INDEX_SEGMENT_CONDITION] = condition
				end
				tinsert(condition,{[INDEX_CONDITION_TYPE] = CONDITION_ID, [INDEX_CONDITION_VALUE] = matched})
				pos = endAt+1

			elseif op == OP_WILDCARD then
				local condition = segment[INDEX_SEGMENT_CONDITION]
				if not condition then
					condition = {}
					segment[INDEX_SEGMENT_CONDITION] = condition
				end
				tinsert(condition,{[INDEX_CONDITION_TYPE] = CONDITION_WILDCARD})
				pos = endAt+1

			-- Some unicode character
			elseif op == OP_UTFCHAR then
				local condition = segment[INDEX_SEGMENT_CONDITION]
				if condition then
					-- concat to last condition
					local last = condition[#condition]
					last[INDEX_CONDITION_VALUE] = last[INDEX_CONDITION_VALUE] .. matched
				else
					-- create new condition (by type=tag)
					segment[INDEX_SEGMENT_CONDITION] = {{[INDEX_CONDITION_TYPE] = CONDITION_TAG, [INDEX_CONDITION_VALUE] = matched}}
				end
				pos = endAt+1

			-- escape (not in string)
			-- consume next char
			-- TODO: help...
			elseif op == OP_ESCAPE then
				-- append on last condition and search more characters
				-- local condition = segment[INDEX_SEGMENT_CONDITION]
				-- if not condition then

				-- end
				-- "[%-_%w]+"
				-- stringEscapes
			end

			if lastPos == pos then
				error(sformat("[SelectorParser.parse: stuck in an infinite loop] The value of pos is not being updated for unknown reasons. This may be a bug in the library implementation. Please write a bug report about:\nDEBUG:\nstr: %s\ninit: %s\npos: %s\nisSubset: %s",tostring(str),tostring(init),tostring(pos),tostring(isSubset)))
			end
			lastPos = pos
		end
		return parsed
    end

	p((selectorParser.parse("와wow와.aㄷ > b")))
	print(prettyParsed(selectorParser.parse("와wow와.aㄷ > b")))
	-- p(selectorParser.parse("wow#working.wowo > a > wow#trash.string"))
	-- p(selectorParser.parse("wow#working.wowo > > wow#trash.string"))
	-- p(selectorParser.parse("  wow#working.wowo > wow#trash.string"))
	-- p(selectorParser.parse(">wow#working.wowo > wow#trash.string"))


    -- function selectorParser.testParse(str)
    --     local results,position = {},1
    --     local mode_property = false
    --     local mode_option = false
    --     local buffer = {}

    --     while true do
    --         if mode_string then

    --         end

    --         for op,regex in ipairs(opRegex) do
    --             sfind(str,regex,)
    --         end
    --     end
    --     return results
    -- end
    -- -- { condition: {} }

    -- function selectorParser:getToken(at)

    -- end

    -- local function parseSelector(selector)

    -- end

end

return module
