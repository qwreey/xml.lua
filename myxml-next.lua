local module = {};

-- TODO: CDATA
-- TODO: Ignore comment
-- TODO: Read document type
-- TODO: querySelector
-- TODO: html items
-- TODO: Rewrite xml parser
-- TODO: :root

local tremove = table.remove
local tinsert = table.insert
local sgsub = string.gsub
local smatch = string.match
local srep = string.rep
local tconcat = table.concat
local sbyte = string.byte
local sformat = string.format
local sfind = string.find
local ssub = string.sub
local schar = string.char

---------------------------------------------
--             Module: utility
---------------------------------------------
local utility = {} do

	-- format number to nth
	local nthDigit = {
		[0] = "%dth",
		"%dst", -- 1st 11st
		"%dnd", -- 2nd 12nd
		"%drd", -- 3rd 13rd
		"%dth","%dth","%dth","%dth","%dth","%dth",
	}
	utility.nthDigit = nthDigit
	function utility.formatNTH(n)
		return sformat(nthDigit[n%10],n)
	end


	-- escape string to make it usable in plain regex match
	function utility.regexEscape(pattern)
		pattern = sgsub(pattern,"%.","%.") -- .
		pattern = sgsub(pattern,"%$","%$") -- $
		pattern = sgsub(pattern,"%^","%^") -- ^
		pattern = sgsub(pattern,"%(","%(") -- (
		pattern = sgsub(pattern,"%)","%)") -- )
		pattern = sgsub(pattern,"%[","%[") -- [
		pattern = sgsub(pattern,"%]","%]") -- ]
		pattern = sgsub(pattern,"%*","%*") -- *
		pattern = sgsub(pattern,"%-","%-") -- -
		pattern = sgsub(pattern,"%+","%+") -- +
		pattern = sgsub(pattern,"%?","%?") -- ?
		pattern = sgsub(pattern,"%%","%%") -- %
		pattern = sgsub(pattern,"\r","") -- for windows remove crlf
		return pattern
	end


	-- string escape list
	utility.stringEscapes = {
		["n"] = "\n",
		["r"] = "\r",
		["t"] = "\t",
		["\\"] = "\\",
		["\""] = "\"";
		["\'"] = "\'";
	}

	local function hexStr(h)
		return schar(tonumber(h,16))
	end
	local function decStr(h)
		return schar(tonumber(h,10))
	end
	function utility.toLuaStr(str)
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

	local function strToHex(c)
		return sformat("&#x%X;", sbyte(c))
	end
	function utility.toXmlStr(str)
		str = sgsub(str,"&","&amp;")
		str = sgsub(str,"<","&lt;")
		str = sgsub(str,">","&gt;")
		str = sgsub(str,"\"","&quot;")
		str = sgsub(str,"'","&apos;")
		str = sgsub(str,"([^%w%&%;%p%\t%\32])",strToHex)
		str = sgsub(str,"\n","\\n")
		str = sgsub(str,"\t","\\t")
		return str
	end

	utility.utf8OnlyRegex = "[%z\1-\127\194-\244][\128-\191]+"

	local stringEscapes = utility.stringEscapes
	local formatNTH = utility.formatNTH

	do
		local opRegex = {}
		local OP_UNICODE = #opRegex+1
		tinsert(opRegex,"^("..utility.utf8OnlyRegex..")")
		local OP_ESCAPE = #opRegex+1
		tinsert(opRegex,"^\\(.)")
		local OP_CLOSE = #opRegex+1
		tinsert(opRegex,"^([\"'])") -- should match with initChar
		local OP_CHARS = #opRegex+1
		tinsert(opRegex,"^([^\\\"]+)")

		function utility.parseString(str,init,initChar)
			initChar = initChar or ssub(str,init,init)
			local pos = init+1 -- ignore " / '
			local buffer = {}
			while true do
				local startsAt,endsAt,catch,op
				for thisOp,regex in ipairs(opRegex) do
					startsAt,endsAt,catch = sfind(str,regex,pos)
					op = thisOp
					if startsAt then
						break
					end
				end

				if not startsAt then
					error(("[Utility.parseString: buffer exhausted] String is opened at %s char (%s). But String is not closed."):format(formatNTH(init),initChar))
				end

				pos = endsAt + 1
				if op == OP_UNICODE or op == OP_CHARS then
					tinsert(buffer,catch)
				elseif op == OP_ESCAPE then
					tinsert(buffer,stringEscapes[catch] or catch)
				elseif op == OP_CLOSE then
					if catch == initChar then
						break
					end
					tinsert(buffer,catch)
				end
			end
			return tconcat(buffer),pos
		end
	end

	local function get(object,key)
		return object[key]
	end
	local function pcallGet(object,key)
		local pass,item = pcall(object,key)
		if pass then return item end
		return nil
	end
	local function pcallGetMetatable(t)
		local pass,result = pcall(getmetatable,t)
		if pass then return result end
	end
	utility.get = get
	utility.pcallGet = pcallGet
	utility.pcallGetMetatable = pcallGetMetatable
end

---------------------------------------------
--          Module: selectorParser
---------------------------------------------
local selectorParser = {} do

	---------------------------------------------
	--                Imports
	---------------------------------------------
	local formatNTH = utility.formatNTH
	local regexEscape = utility.regexEscape
	local parseString = utility.parseString
	local stringEscapes = utility.stringEscapes

	---------------------------------------------
	--          selectorParser Consts
	---------------------------------------------
	local opRegex = {}
	local opNames = {[-1] = "NONE"}
	local accessNames = {}
	local conditionTypes = {}

	-- End of selector string
	local OP_EOF = #opNames+1
	tinsert(opRegex,"^[ \n\t]*$")
	tinsert(opNames,"OP_EOF")

	-- Property selector open and close
	local OP_PROPERTY_OPEN = #opNames+1
	tinsert(opRegex,"^%[[ \t\n]*")
	tinsert(opNames,"OP_PROPERTY_OPEN")
	local OP_PROPERTY_CLOSE = #opNames+1
	tinsert(opRegex,"^[ \t\n]*%]")
	tinsert(opNames,"OP_PROPERTY_CLOSE")

	-- Property selector matcher
	local OP_PROPERTY_CONDITION_EQUAL = #opNames+1
	tinsert(opRegex,"^[ \t\n]*=[ \t\n]*")
	tinsert(opNames,"OP_PROPERTY_CONDITION_EQUAL")
	local OP_PROPERTY_CONDITION_WORD = #opNames+1
	tinsert(opRegex,"^[ \t\n]*~=[ \t\n]*")
	tinsert(opNames,"OP_PROPERTY_CONDITION_WORD")
	local OP_PROPERTY_CONDITION_DASH = #opNames+1
	tinsert(opRegex,"^[ \t\n]*|=[ \t\n]*")
	tinsert(opNames,"OP_PROPERTY_CONDITION_DASH")
	local OP_PROPERTY_CONDITION_BEGIN = #opNames+1
	tinsert(opRegex,"^[ \t\n]*%^=[ \t\n]*")
	tinsert(opNames,"OP_PROPERTY_CONDITION_BEGIN")
	local OP_PROPERTY_CONDITION_END = #opNames+1
	tinsert(opRegex,"^[ \t\n]*$=[ \t\n]*")
	tinsert(opNames,"OP_PROPERTY_CONDITION_END")
	local OP_PROPERTY_CONDITION_CONTAIN = #opNames+1
	tinsert(opRegex,"^[ \t\n]*%*=[ \t\n]*")
	tinsert(opNames,"OP_PROPERTY_CONDITION_CONTAIN")

	-- Strings
	local OP_STRING_DOUBLE_QUOTE = #opNames+1
	tinsert(opRegex,"^(\")")
	tinsert(opNames,"OP_STRING_DOUBLE_QUOTE")
	local OP_STRING_SINGLE_QUOTE = #opNames+1
	tinsert(opRegex,"^(')")
	tinsert(opNames,"OP_STRING_SINGLE_QUOTE")

	-- Abstract selector
	local OP_ABSTRACT_EMPTY = #opNames+1
	tinsert(opRegex,"^[ \t\n]*:empty")
	tinsert(opNames,"OP_ABSTRACT_EMPTY")
	local OP_ABSTRACT_FIRST_CHILD = #opNames+1
	tinsert(opRegex,"^[ \t\n]*:first%-child")
	tinsert(opNames,"OP_ABSTRACT_FIRST_CHILD")
	local OP_ABSTRACT_FIRST_OF_TYPE = #opNames+1
	tinsert(opRegex,"^[ \t\n]*:first%-of%-type")
	tinsert(opNames,"OP_ABSTRACT_FIRST_OF_TYPE")
	local OP_ABSTRACT_LAST_CHILD = #opNames+1
	tinsert(opRegex,"^[ \t\n]*:last%-child")
	tinsert(opNames,"OP_ABSTRACT_LAST_CHILD")
	local OP_ABSTRACT_LAST_OF_TYPE = #opNames+1
	tinsert(opRegex,"^[ \t\n]*:last%-of%-type")
	tinsert(opNames,"OP_ABSTRACT_LAST_OF_TYPE")
	local OP_ABSTRACT_NOT = #opNames+1
	tinsert(opRegex,"^[ \t\n]*:not")
	tinsert(opNames,"OP_ABSTRACT_NOT")
	local OP_ABSTRACT_NTH_CHILD = #opNames+1
	tinsert(opRegex,"^[ \t\n]*:nth%-child")
	tinsert(opNames,"OP_ABSTRACT_NTH_CHILD")
	local OP_ABSTRACT_NTH_LAST_CHILD = #opNames+1
	tinsert(opRegex,"^[ \t\n]*:nth%-last%-child")
	tinsert(opNames,"OP_ABSTRACT_NTH_LAST_CHILD")
	local OP_ABSTRACT_NTH_LAST_OF_TYPE = #opNames+1
	tinsert(opRegex,"^[ \t\n]*:nth%-last%-of%-type")
	tinsert(opNames,"OP_ABSTRACT_NTH_LAST_OF_TYPE")
	local OP_ABSTRACT_NTH_OF_TYPE = #opNames+1
	tinsert(opRegex,"^[ \t\n]*:nth%-of%-type")
	tinsert(opNames,"OP_ABSTRACT_NTH_OF_TYPE")
	local OP_ABSTRACT_OLNY_OF_TYPE = #opNames+1
	tinsert(opRegex,"^[ \t\n]*:only%-of%-type")
	tinsert(opNames,"OP_ABSTRACT_OLNY_OF_TYPE")
	local OP_ABSTRACT_ONLY_CHILD = #opNames+1
	tinsert(opRegex,"^[ \t\n]*:only%-child")
	tinsert(opNames,"OP_ABSTRACT_ONLY_CHILD")
	-- TODO: firefox's abstract selector such as :is()
	-- TODO: :is() is useful. see :is(h1,h2,h3):not(.colored)

	-- Abstract option open and close
	local OP_ABSTRACT_OPTION_OPEN = #opNames+1
	tinsert(opRegex,"^[ \t\n]*%(")
	tinsert(opNames,"OP_ABSTRACT_OPTION_OPEN")
	local OP_ABSTRACT_OPTION_CLOSE = #opNames+1
	tinsert(opRegex,"^[ \t\n]*%)")
	tinsert(opNames,"OP_ABSTRACT_OPTION_CLOSE")

	-- Escape
	local OP_ESCAPE = #opNames+1
	tinsert(opRegex,"^\\(.?)")
	tinsert(opNames,"OP_ESCAPE")

	-- Access
	local OP_ACCESS_CHILD = #opNames+1
	accessNames[OP_ACCESS_CHILD] = "CHILD"
	selectorParser.OP_ACCESS_CHILD = OP_ACCESS_CHILD
	tinsert(opRegex,"^[ \t\n]*>[ \t\n]*")
	tinsert(opNames,"OP_ACCESS_CHILD")
	local OP_ACCESS_SIBLING = #opNames+1
	accessNames[OP_ACCESS_SIBLING] = "SIBLING"
	selectorParser.OP_ACCESS_SIBLING = OP_ACCESS_SIBLING
	tinsert(opRegex,"^[ \t\n]*%+[ \t\n]*")
	tinsert(opNames,"OP_ACCESS_SIBLING")
	local OP_ACCESS_AFTER = #opNames+1
	accessNames[OP_ACCESS_AFTER] = "AFTER"
	selectorParser.OP_ACCESS_AFTER = OP_ACCESS_AFTER
	tinsert(opRegex,"^[ \t\n]*~[ \t\n]*")
	tinsert(opNames,"OP_ACCESS_AFTER")

	-- More
	local OP_MORE = #opNames+1
	tinsert(opRegex,"^[ \t\n]*,[ \t\n]*")
	tinsert(opNames,"OP_MORE")

	-- DFS
	local OP_ACCESS_DESCENDANT = #opNames+1
	accessNames[OP_ACCESS_DESCENDANT] = "DESCENDANT"
	selectorParser.OP_ACCESS_DESCENDANT = OP_ACCESS_DESCENDANT
	tinsert(opRegex,"^[ \t\n]+")
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
	-- tinsert(opRegex,"("..utf8.charpattern..")")
	tinsert(opRegex,"^("..utility.utf8OnlyRegex..")")
	tinsert(opNames,"OP_UTFCHAR")

	-- condition types
	local CONDITION_CLASS = #conditionTypes+1
	selectorParser.CONDITION_CLASS = CONDITION_CLASS
	tinsert(conditionTypes,"CLASS")
	local CONDITION_ID = #conditionTypes+1
	selectorParser.CONDITION_ID = CONDITION_ID
	tinsert(conditionTypes,"ID")
	local CONDITION_TAG = #conditionTypes+1
	selectorParser.CONDITION_TAG = CONDITION_TAG
	tinsert(conditionTypes,"TAG")
	local CONDITION_WILDCARD = #conditionTypes+1
	selectorParser.CONDITION_WILDCARD = CONDITION_WILDCARD
	tinsert(conditionTypes,"WILDCARD")
	local CONDITION_PROPERTY = #conditionTypes+1
	selectorParser.CONDITION_PROPERTY = CONDITION_PROPERTY
	tinsert(conditionTypes,"PROPERTY")

	-- indexes
	local INDEX_CONDITION_TYPE = 1
	selectorParser.INDEX_CONDITION_TYPE = INDEX_CONDITION_TYPE
	local INDEX_CONDITION_VALUE = 2
	selectorParser.INDEX_CONDITION_VALUE = INDEX_CONDITION_VALUE
	local INDEX_CONDITION_OPTION = 3
	selectorParser.INDEX_CONDITION_OPTION = INDEX_CONDITION_OPTION
	local INDEX_SEGMENT_CONDITION = 1
	selectorParser.INDEX_SEGMENT_CONDITION = INDEX_SEGMENT_CONDITION
	local INDEX_SEGMENT_ACCESSMODE = 2
	selectorParser.INDEX_SEGMENT_ACCESSMODE = INDEX_SEGMENT_ACCESSMODE

	-- create error position, information trace
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

	-- pretty parsed selector for debug
	local function prettyParsed(parsed)
		local buffer = {"Query {\n"}
		for selectorIndex,selector in ipairs(parsed) do
			tinsert(buffer,sformat("  Selector #%d {\n",selectorIndex))
			for segmentIndex,segment in ipairs(selector) do
				tinsert(buffer,sformat("    Segment #%d: AccessMode = %s {\n",segmentIndex,accessNames[segment[INDEX_SEGMENT_ACCESSMODE]] or "NONE"))
				for conditionIndex,condition in ipairs(segment[INDEX_SEGMENT_CONDITION]) do
					if condition[INDEX_CONDITION_OPTION] then
						tinsert(buffer,sformat("      Condition #%d: Type = %s {\n        Value = '%s'\n        Option = '%s'\n      }\n",conditionIndex,conditionTypes[condition[INDEX_CONDITION_TYPE]],condition[INDEX_CONDITION_VALUE],type(condition[INDEX_CONDITION_OPTION]) == "table" and tconcat(condition[INDEX_CONDITION_OPTION],"'\n                 '") or condition[INDEX_CONDITION_OPTION]))
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
	selectorParser.prettyParsed = prettyParsed

	---------------------------------------------
	--     selectorParser Private methods
	---------------------------------------------

	-- create regexs by condition operator and value
	function selectorParser._buildValueRegex(propertyCondition,value)
		local escaped = value and regexEscape(value)
		if (not propertyCondition) or (not value) then
			return nil
		elseif propertyCondition == OP_PROPERTY_CONDITION_BEGIN then
			return sformat("^%s",escaped)
		elseif propertyCondition == OP_PROPERTY_CONDITION_CONTAIN then
			return escaped
		elseif propertyCondition == OP_PROPERTY_CONDITION_DASH then
			return {
				sformat("^%s$",escaped),
				sformat("^%s%%-",escaped)
			}
		elseif propertyCondition == OP_PROPERTY_CONDITION_END then
			return sformat("%s$",escaped)
		elseif propertyCondition == OP_PROPERTY_CONDITION_EQUAL then
			return sformat("^%s$",escaped)
		elseif propertyCondition == OP_PROPERTY_CONDITION_WORD then
			return {
				sformat("^%s[\n\t ]",escaped),
				sformat("[\n\t ]%s[\n\t ]",escaped),
				sformat("[\n\t ]%s$",escaped),
				sformat("^%s$",escaped)
			}
		end
	end

	-- check value is match with regexs
	function selectorParser._checkPropertyValueMatch(regex,value)
		if regex == nil then -- [src]
			return true
		elseif type(regex) == "string" then
			if smatch(value,regex) then return true end
		else
			for _,r in ipairs(regex) do
				if smatch(value,r) then return true end
			end
		end
		return false
	end

	-- [label="Hello"]
	-- find object which label property is Hello
	-- [src]
	-- find object which has src property
	do
		-- property parser regexs
		local propertyRegex = {}

		local POP_PROPERTY_OPEN = #propertyRegex+1
		tinsert(propertyRegex,opRegex[OP_PROPERTY_OPEN])
		local POP_PROPERTY_CLOSE = #propertyRegex+1
		tinsert(propertyRegex,opRegex[OP_PROPERTY_CLOSE])
		local POP_STRING_DOUBLE_QUOTE = #propertyRegex+1
		tinsert(propertyRegex,opRegex[OP_STRING_DOUBLE_QUOTE])
		local POP_STRING_SINGLE_QUOTE = #propertyRegex+1
		tinsert(propertyRegex,opRegex[OP_STRING_SINGLE_QUOTE])
		local POP_PROPERTY_CONDITION_EQUAL = #propertyRegex+1
		tinsert(propertyRegex,opRegex[OP_PROPERTY_CONDITION_EQUAL])
		local POP_PROPERTY_CONDITION_WORD = #propertyRegex+1
		tinsert(propertyRegex,opRegex[OP_PROPERTY_CONDITION_WORD])
		local POP_PROPERTY_CONDITION_DASH = #propertyRegex+1
		tinsert(propertyRegex,opRegex[OP_PROPERTY_CONDITION_DASH])
		local POP_PROPERTY_CONDITION_BEGIN = #propertyRegex+1
		tinsert(propertyRegex,opRegex[OP_PROPERTY_CONDITION_BEGIN])
		local POP_PROPERTY_CONDITION_END = #propertyRegex+1
		tinsert(propertyRegex,opRegex[OP_PROPERTY_CONDITION_END])
		local POP_PROPERTY_CONDITION_CONTAIN = #propertyRegex+1
		tinsert(propertyRegex,opRegex[OP_PROPERTY_CONDITION_CONTAIN])

		local POP_WORD = #propertyRegex+1
		tinsert(propertyRegex,"^([%-_%w]+)")

		local POP_ESCAPE = #propertyRegex+1
		tinsert(propertyRegex,"^\\(.?)")

		local POP_UTF8 = #propertyRegex+1
		tinsert(propertyRegex,"^("..utility.utf8OnlyRegex..")")

		function selectorParser._parseProperty(str,init)
			local pos = init
			local lastPos
			local name,value = {},{}
			local condition
			local startAt,endAt,matched,op

			while true do
				for index,regex in ipairs(propertyRegex) do
					startAt,endAt,matched = sfind(str,regex,pos)
					if startAt then
						op = index
						break
					end
				end

				if not startAt then
					error(sformat("[SelectorParser._parseProperty: buffer exhausted] unexpected token at %d",pos))

				elseif op == POP_PROPERTY_OPEN then
					error("[SelectorParser._parseProperty: unexpected property open] property open bracket was got inside of the property selector. Possible cause: '[' token was got in inside of '[]' (example: [label[]). Did you want to escape that token? place '\\' in front of '[' to resolve it"..buildErrorTrace(str,startAt,endAt,-1))

				elseif op == POP_PROPERTY_CLOSE then
					-- return data,endAt+1
					break

				elseif op == POP_STRING_DOUBLE_QUOTE or
					   op == POP_STRING_SINGLE_QUOTE then
					if not condition then
						error("[SelectorParser._parseProperty: unexpected string start] string can only start after property condition operator. Possible cause: ' or \" token was got before '~=', '^=', '*=' or some operators (example: [\"label\" ~= a]). Did you want to escape that token? place '\\' in front of that token to resolve it"..buildErrorTrace(str,startAt,endAt,-1))
					elseif #value ~= 0 then
						error("[SelectorParser._parseProperty: unexpected string start] got unexpected token ' or \" in value. Possible cause: ' or \" token was got in inside of value (example: [label ~= a\"b\"]). Did you want to escape that token? place '\\' in front of that token to resolve it"..buildErrorTrace(str,startAt,endAt,-1))
					end
					matched,pos = parseString(str,startAt,matched)
					value[-1] = true
					tinsert(value,matched)

				elseif op == POP_PROPERTY_CONDITION_EQUAL then
					if condition then
						error("[SelectorParser._parseProperty: condition operator was got already] Possible cause: got repeated operator token (example: [label ~= ~=]). Did you want to escape that token? place '\\' in each char of token to resolve it"..buildErrorTrace(str,startAt,endAt,OP_PROPERTY_CONDITION_WORD))
					end
					condition = OP_PROPERTY_CONDITION_EQUAL
					pos = endAt+1

				elseif op == POP_PROPERTY_CONDITION_WORD then
					if condition then
						error("[SelectorParser._parseProperty: unexpected condition token] condition operator was got already. Possible cause: got repeated operator token (example: [label ~= ~=]). Did you want to escape that token? place '\\' on each char of token to resolve it"..buildErrorTrace(str,startAt,endAt,OP_PROPERTY_CONDITION_WORD))
					end
					condition = OP_PROPERTY_CONDITION_WORD
					pos = endAt+1

				elseif op == POP_PROPERTY_CONDITION_DASH then
					if condition then
						error("[SelectorParser._parseProperty: unexpected condition token] condition operator was got already. Possible cause: got repeated operator token (example: [label ~= ~=]). Did you want to escape that token? place '\\' on each char of token to resolve it"..buildErrorTrace(str,startAt,endAt,OP_PROPERTY_CONDITION_DASH))
					end
					condition = OP_PROPERTY_CONDITION_DASH
					pos = endAt+1

				elseif op == POP_PROPERTY_CONDITION_BEGIN then
					if condition then
						error("[SelectorParser._parseProperty: unexpected condition token] condition operator was got already. Possible cause: got repeated operator token (example: [label ~= ~=]). Did you want to escape that token? place '\\' on each char of token to resolve it"..buildErrorTrace(str,startAt,endAt,OP_PROPERTY_CONDITION_BEGIN))
					end
					condition = OP_PROPERTY_CONDITION_BEGIN
					pos = endAt+1

				elseif op == POP_PROPERTY_CONDITION_END then
					if condition then
						error("[SelectorParser._parseProperty: unexpected condition token] condition operator was got already. Possible cause: got repeated operator token (example: [label ~= ~=]). Did you want to escape that token? place '\\' on each char of token to resolve it"..buildErrorTrace(str,startAt,endAt,OP_PROPERTY_CONDITION_END))
					end
					condition = OP_PROPERTY_CONDITION_END
					pos = endAt+1

				elseif op == POP_PROPERTY_CONDITION_CONTAIN then
					if condition then
						error("[SelectorParser._parseProperty: unexpected condition token] condition operator was got already. Possible cause: got repeated operator token (example: [label ~= ~=]). Did you want to escape that token? place '\\' on each char of token to resolve it"..buildErrorTrace(str,startAt,endAt,OP_PROPERTY_CONDITION_CONTAIN))
					end
					condition = OP_PROPERTY_CONDITION_CONTAIN
					pos = endAt+1

				elseif op == POP_WORD then
					if value[-1] then
						error("[SelectorParser._parseProperty: unexpected token after value string] property selector value was set by string already. you can't give more value. Possible cause: got more tokens after value string (example: [label = \"a\"a])")
					end
					tinsert(condition and value or name,matched)
					pos = endAt+1

				elseif op == POP_ESCAPE then
					if value[-1] then
						error("[SelectorParser._parseProperty: unexpected token after value string] property selector value was set by string already. you can't give more value. Possible cause: got more tokens after value string (example: [label = \"a\"a])")
					end
					tinsert(condition and value or name,stringEscapes[matched] or matched)
					pos = endAt+1

				elseif op == 13 then
					-- "("..utf8.charpattern..")",
					-- utf8
					if value[-1] then
						error("[SelectorParser._parseProperty: unexpected token after value string] property selector value was set by string already. you can't give more value. Possible cause: got more tokens after value string (example: [label = \"a\"a])")
					end
					tinsert(condition and value or name,matched)
					pos = endAt+1

				end

				if lastPos == pos then
					error(sformat("[SelectorParser._parseProperty: stuck in an infinite loop] The value of pos is not being updated for unknown reasons. This may be a bug in the library implementation. Please write a bug report about:\nDEBUG:\nstr: %s\ninit: %s\npos: %s",tostring(str),tostring(init),tostring(pos)))
				end
				lastPos = pos
			end

			-- create property segment
			return {
				[INDEX_CONDITION_TYPE] = CONDITION_PROPERTY,
				[INDEX_CONDITION_VALUE] = tconcat(name),
				[INDEX_CONDITION_OPTION] = selectorParser._buildValueRegex(condition,tconcat(value)),
			},endAt+1
		end
	end

	---------------------------------------------
	--          selectorParser methods
	---------------------------------------------

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
		local buflen = #str
		while true do
			-- get current token
			for index,regex in ipairs(opRegex) do
				startAt,endAt,matched = sfind(str,regex,pos)
				if startAt then
					op = index
					break
				end
			end
			if not startAt then
				if not segment[INDEX_SEGMENT_CONDITION] then
					error(sformat("[SelectorParser.parse: buffer exhausted] unexpected token at %d",pos))
				end
				return
			end

			-- End of file (selector)
			-- remove tailing spaces
			if op == OP_EOF then
				break

			-- Access modek
			-- + ~ > space
			elseif
			op == OP_ACCESS_SIBLING or -- +
			op == OP_ACCESS_CHILD or -- >
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
				-- ignore heading spaces
				pos = smatch(str,"^[ \t\n]*()",endAt+1)
				local condition
				condition,pos = selectorParser._parseProperty(str,pos)
				local conditionList = segment[INDEX_SEGMENT_CONDITION]
				if not conditionList then
					conditionList = {}
					segment[INDEX_SEGMENT_CONDITION] = conditionList
				end
				tinsert(conditionList,condition)
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
				local conditionList = segment[INDEX_SEGMENT_CONDITION]
				if not conditionList then
					conditionList = {}
					segment[INDEX_SEGMENT_CONDITION] = conditionList
				end
				tinsert(conditionList,{[INDEX_CONDITION_TYPE] = CONDITION_CLASS, [INDEX_CONDITION_VALUE] = matched})
				pos = endAt+1

			-- tag
			-- body
			elseif op == OP_TAG then
				local conditionList = segment[INDEX_SEGMENT_CONDITION]
				if not conditionList then
					conditionList = {}
					segment[INDEX_SEGMENT_CONDITION] = conditionList
				end
				local last = conditionList[#conditionList]
				-- This is little hacky, if got '.wow가가wew' wew will inserted at segment which has 'class' type
				if last and last[INDEX_CONDITION_TYPE] ~= CONDITION_PROPERTY then
					last[INDEX_CONDITION_VALUE] = last[INDEX_CONDITION_VALUE] .. matched -- concat more after utf8. (e: a가나다b)
				else
					tinsert(conditionList,{[INDEX_CONDITION_TYPE] = CONDITION_TAG, [INDEX_CONDITION_VALUE] = matched})
				end
				pos = endAt+1

			-- id
			-- #a
			elseif op == OP_ID then
				local conditionList = segment[INDEX_SEGMENT_CONDITION]
				if not conditionList then
					conditionList = {}
					segment[INDEX_SEGMENT_CONDITION] = conditionList
				end
				tinsert(conditionList,{[INDEX_CONDITION_TYPE] = CONDITION_ID, [INDEX_CONDITION_VALUE] = matched})
				pos = endAt+1

			-- all (wildcard)
			-- *
			elseif op == OP_WILDCARD then
				local conditionList = segment[INDEX_SEGMENT_CONDITION]
				if not conditionList then
					conditionList = {}
					segment[INDEX_SEGMENT_CONDITION] = conditionList
				end
				tinsert(conditionList,{[INDEX_CONDITION_TYPE] = CONDITION_WILDCARD})
				pos = endAt+1

			-- Some unicode character
			elseif op == OP_UTFCHAR then
				local conditionList = segment[INDEX_SEGMENT_CONDITION]
				local last = conditionList and conditionList[#conditionList]
				if last and last[INDEX_CONDITION_TYPE] == CONDITION_PROPERTY then
					-- last condition is property, create net condition
					tinsert(conditionList,{[INDEX_CONDITION_TYPE] = CONDITION_TAG, [INDEX_CONDITION_VALUE] = matched})
				elseif last then
					-- concat to last condition
					last[INDEX_CONDITION_VALUE] = last[INDEX_CONDITION_VALUE] .. matched
				else
					-- create new conditionList (by type=tag)
					segment[INDEX_SEGMENT_CONDITION] = {{[INDEX_CONDITION_TYPE] = CONDITION_TAG, [INDEX_CONDITION_VALUE] = matched}}
				end
				pos = endAt+1

			-- escape (not in string)
			-- consume next char
			-- TODO: Consume char by utf8
			elseif op == OP_ESCAPE then
				local conditionList = segment[INDEX_SEGMENT_CONDITION]
				local last = conditionList and conditionList[#conditionList]
				matched = stringEscapes[matched] or matched
				if last and last[INDEX_CONDITION_TYPE] == CONDITION_PROPERTY then
					-- last condition is property, create net condition
					tinsert(conditionList,{[INDEX_CONDITION_TYPE] = CONDITION_TAG, [INDEX_CONDITION_VALUE] = matched})
				elseif last then
					-- concat to last condition
					last[INDEX_CONDITION_VALUE] = last[INDEX_CONDITION_VALUE] .. matched
				else
					-- create new conditionList (by type=tag)
					segment[INDEX_SEGMENT_CONDITION] = {{[INDEX_CONDITION_TYPE] = CONDITION_TAG, [INDEX_CONDITION_VALUE] = matched}}
				end

				pos = endAt+1
			end

			if lastPos == pos then
				error(sformat("[SelectorParser.parse: stuck in an infinite loop] The value of pos is not being updated for unknown reasons. This may be a bug in the library implementation. Please write a bug report about:\nDEBUG:\nstr: %s\ninit: %s\npos: %s\nisSubset: %s",tostring(str),tostring(init),tostring(pos),tostring(isSubset)))
			elseif pos > buflen then
				if not segment[INDEX_SEGMENT_CONDITION] then
					error(sformat("[SelectorParser.parse: buffer exhausted] Buffer exhausted before got a condition. Possible cause: Got More/Access operation at end of buffer (example: a > / a , )"..buildErrorTrace(str,startAt,endAt,op)))
				end
				break
			end
			lastPos = pos
		end
		return parsed
	end

end

---------------------------------------------
--          Module: querySelector
---------------------------------------------
local querySelector = {} do
	querySelector.__index = querySelector
	local cache = setmetatable({},{__mod="kv"})

	function querySelector.new(selector)
		if type(selector) == "table" then return selector end

		local cached = cache[selector]
		if cached then return cached end

		cached = setmetatable(selectorParser.parse(selector),querySelector)
		cache[selector] = cached
		return cached
	end

	local OP_ACCESS_CHILD = selectorParser.OP_ACCESS_CHILD
	local OP_ACCESS_SIBLING = selectorParser.OP_ACCESS_SIBLING
	local OP_ACCESS_AFTER = selectorParser.OP_ACCESS_AFTER
	local OP_ACCESS_DESCENDANT = selectorParser.OP_ACCESS_DESCENDANT

	local CONDITION_CLASS = selectorParser.CONDITION_CLASS
	local CONDITION_ID = selectorParser.CONDITION_ID
	local CONDITION_TAG = selectorParser.CONDITION_TAG
	local CONDITION_WILDCARD = selectorParser.CONDITION_WILDCARD
	local CONDITION_PROPERTY = selectorParser.CONDITION_PROPERTY

	local INDEX_CONDITION_TYPE = selectorParser.INDEX_CONDITION_TYPE
	local INDEX_CONDITION_VALUE = selectorParser.INDEX_CONDITION_VALUE
	local INDEX_CONDITION_OPTION = selectorParser.INDEX_CONDITION_OPTION
	local INDEX_SEGMENT_CONDITION = selectorParser.INDEX_SEGMENT_CONDITION
	local INDEX_SEGMENT_ACCESSMODE = selectorParser.INDEX_SEGMENT_ACCESSMODE

	-- to contributor
	-- do not remove nested if.
	-- If you bind the conditions with the and operator, the next elseif statement will be executed, which is a more worse idea.
	-- ~~lua needs switch operation...~~
	local function isMatchConditions(element,conditionList)
		local option = element.option
		local classList
		for _,condition in ipairs(conditionList) do
			local conditionType = condition[INDEX_CONDITION_TYPE]
			if conditionType == CONDITION_WILDCARD then
				-- nothing to do
			elseif conditionType == CONDITION_ID then
				if not option then return end
				if option.id ~= condition[INDEX_CONDITION_VALUE] then return end
			elseif conditionType == CONDITION_TAG then
				if element.tag ~= condition[INDEX_CONDITION_VALUE] then return end
			elseif conditionType == CONDITION_CLASS then
				if not classList then classList = element:getClassList() end
				if not classList:has(condition[INDEX_CONDITION_VALUE]) then return end
			elseif conditionType == CONDITION_PROPERTY then
				if not option then return end
				local val = option[condition[INDEX_CONDITION_VALUE]]
				if not val then return end
				local pattern = condition[INDEX_CONDITION_OPTION]
				if pattern then -- condition has match pattern
					if not smatch(val,pattern) then return end
				end
			end
		end
		return true
	end

	-- not 아이디어:
	-- 캐시를 넣는다, 그렇게 해서 parent parent 하는 검증에서 좀더 빠를 수 있게 만든다
	-- true false 를 캐싱 한다음 각각의 요소에 대해서 검증하면, 같은 parent 를 가진 녀석을
	-- 확인하는데 결국 parent 가 이미 캐시되어 있으므로 좀더 빠를것이다. < 근데 결국 parent 가 세그먼트가 맞으면 그 parent의 parent 도 채크해야하는데 흠
	-- 대신 캐시를 segment 단위로 생성해야한다는 흠이 있긴 하겠지만 빠를것이다 (아마도)
	-- 실험이 필요함
	-- dfs 를 역순으로 생각하고 해보자, 맨 상위 :root 까지 찍고 다시 아래로 내려올 때 (dfs 가 하단찍고 올라올 때) 순차적으로 완전히 확인되었음을 기록하자
	-- 그러면 아마도, 다음 검사 객체가 왔을때 확인된 노드를 다시 확인 할 필요 없다
	-- 이러면 캐시를 하나만 넣어도 되는것 아닌가?
	-- 문제점은 , 을 찍어놓으면 더 골치아파진다, not(not()) 은 어떤가? 중첩???
	-- 검사해야할 parent 자체가 완전히 달라지는 조합이 생긴다
	-- 머리아프다 어쩌라는건지 잘 모르겠긴하다
	-- 확실한건 그냥 여집합의 교집합이 나을지도?
	-- 성능해킹을 위해 알고리즘을 넣으면 :is() :not() 이나 메타테그들에 너무 복잡성이 늘어난다
	local function notWith(target,notSelector)

	end

	function querySelector:find(root)

	end

	function querySelector:match(object)

	end

end

---------------------------------------------
--         Module: classNameParser
---------------------------------------------
local classNameParser = {} do
	local stringEscapes = utility.stringEscapes
	local opRegex = {}

	local OP_STRING = #opRegex+1
	tinsert(opRegex,"([^ \t\n\\]+)")

	local OP_ESCAPE = #opRegex+1
	tinsert(opRegex,"\\(.)")

	local OP_SPLIT = #opRegex+1
	tinsert(opRegex,"([ \t\n]+)")

	function classNameParser.parse(str)
		local pos,len = 1,#str
		local startAt,endAt,matched,op
		local array,buffer = {},{}
		while true do
			for index,regex in ipairs(opRegex) do
				startAt,endAt,matched = sfind(str,regex,pos)
				if startAt then
					op = index
					break
				end
			end

			if not startAt then
				break
			elseif op == OP_STRING then
				tinsert(buffer,matched)
			elseif op == OP_ESCAPE then
				tinsert(buffer,stringEscapes[matched] or matched)
			elseif op == OP_SPLIT then
				tinsert(array,tconcat(buffer))
				buffer = {}
			end
			pos = endAt+1

			if pos > len then break end
		end

		if #buffer ~= 0 then
			tinsert(array,tconcat(buffer))
		end

		return array
	end

	local replaces = {
		["\t"] = "\\t",
		["\n"] = "\\n",
		["\\"] = "\\\\",
		["\r"] = "",
	}
	local replaceRegex = "[\t\n\r\\]"
	function classNameParser.stringify(array)
		local buffer = {}
		for _,name in ipairs(array) do
			tconcat(buffer,sgsub(name,replaceRegex,replaces))
		end
		return tconcat(buffer," ")
	end
end

---------------------------------------------
--            Module: tokenList
---------------------------------------------
---@class tokenList
local tokenList = {} do
	tokenList.__index = tokenList

	function tokenList:has(token)
		for i,v in ipairs(self) do
			if v == token then
				return i
			end
		end
		return false
	end

	function tokenList:add(token)
		if self:has(token) then return end
		tinsert(self,token)
	end

	function tokenList:remove(token)
		local index = self:has(token)
		if not index then return end
		tremove(self,index)
	end

	function tokenList:toggle(token)
		local index = self:has(token)
		if not index then
			tinsert(self,token)
		else
			tremove(self,index)
		end
	end

	function tokenList:replace(from,to)
		local index = self:has(from)
		if not index then return false end
		self[index] = to
		return true
	end
end

---------------------------------------------
--            Module: classList
---------------------------------------------
---@class classList:tokenList
local classList = setmetatable({},tokenList) do
	classList.__index = classList

	local parse,stringify =
	classNameParser.parse,classNameParser.stringify

	function classList.new(str)
		return setmetatable(parse(str),classList)
	end

	function classList:stringify()
		return stringify(self)
	end
end

---------------------------------------------
--           Module: collection
---------------------------------------------
---@class collection
local collection = {} do
	collection.__index = collection

	function collection.new(initChildren)
		return setmetatable(initChildren or {},collection)
	end
	local new = collection.new

	---------------------------------------------
	--          Methods : Tag selector
	---------------------------------------------

	---@param tag string tag of child
	---@return element|nil childitem
	---@return number|nil index
	function collection:getFirstChildByTag(tag)
		if type(tag) ~= "string" then
			error(("[collection.getFirstChildByTag] getFirstChildByTag method arg 1 'tag' must be a string, but got %s"):format(type(tag)))
		end
		for index,child in ipairs(self) do
			if child.tag == tag then
				return child,index
			end
		end
	end

	---get child objects by using tag
	---@param tag string tag of children
	---@return collection children table of children
	function collection:getChildrenByTag(tag)
		if type(tag) ~= "string" then
			error(("[collection.getChildrenByTag] getFirstChildByTag method arg 1 'tag' must be a string, but got %s"):format(type(tag)))
		end
		local children = {}
		for _,child in ipairs(self) do
			if type(child) == "table" and child.tag == tag then
				tinsert(children,child)
			end
		end
		return new(children)
	end

	local function getFirstDescendantByTag(self,tag)
		local result
		for _,child in ipairs(self) do
			if type(child) == "table" then
				if child.tag == tag then return child end
				result = getFirstDescendantByTag(child)
				if result then return result end
			end
		end
	end
	function collection:getFirstDescendantByTag(tag)
		if type(tag) ~= "string" then
			error(("[collection.getFirstDescendantByTag] getFirstDescendantByTag method arg 1 'tag' must be a string, but got %s"):format(type(tag)))
		end
		return getFirstDescendantByTag(self,tag)
	end

	local function getDescendantsByTag(self,tag,list)
		for _,child in ipairs(self) do
			if type(child) == "table" then
				if child.tag == tag then tinsert(list,child) end
				getDescendantsByTag(self,child,list)
			end
		end
	end
	function collection:getDescendantsByTag(tag)
		if type(tag) ~= "string" then
			error(("[collection.getDescendantsByTag] getDescendantsByTag method arg 1 'tag' must be a string, but got %s"):format(type(tag)))
		end
		local list = {}
		getDescendantsByTag(self,tag,list)
		return new(list)
	end

	---------------------------------------------
	--           Methods : Option selector
	---------------------------------------------
	local function getOption(object,name)
		local option = object.option
		return option and option[name]
	end

	function collection:getFirstChildByOption(name,value)
		if type(name) ~= "string" then
			error(("[collection.getFirstChildByOption] getFirstChildByOption method arg 1 'name' must be a string, but got %s"):format(type(name)))
		end
		if type(value) ~= "string" then
			error(("[collection.getFirstChildByOption] getFirstChildByOption method arg 2 'value' must be a string, but got %s"):format(type(value)))
		end
		for index,child in ipairs(self) do
			if getOption(child,name) == value then
				return child,index
			end
		end
	end

	function collection:getChildrenByOption(name,value)
		if type(name) ~= "string" then
			error(("[collection.getChildrenByOption] getChildrenByOption method arg 1 'name' must be a string, but got %s"):format(type(name)))
		end
		if type(value) ~= "string" then
			error(("[collection.getChildrenByOption] getFirstChildByOption method arg 2 'value' must be a string, but got %s"):format(type(value)))
		end
		local children = {}
		for _,child in ipairs(self) do
			if type(child) == "table" and getOption(child,name) == value then
				tinsert(children,child)
			end
		end
		return new(children)
	end

	local function getFirstDescendantByOption(self,name,value)
		local result
		for _,child in ipairs(self) do
			if type(child) == "table" then
				if getOption(child,name) == value then return child end
				result = getFirstDescendantByOption(child,name,value)
				if result then return result end
			end
		end
	end
	function collection:getFirstDescendantByOption(name,value)
		if type(name) ~= "string" then
			error(("[collection.getFirstDescendantByOption] getFirstDescendantByOption method arg 1 'name' must be a string, but got %s"):format(type(name)))
		end
		if type(value) ~= "string" then
			error(("[collection.getFirstDescendantByOption] getFirstDescendantByOption method arg 2 'value' must be a string, but got %s"):format(type(value)))
		end
		if getOption(self,name) == value then return self end
		return getFirstDescendantByOption(self,name,value)
	end

	local function getDescendantsByOption(self,name,value,list)
		for _,child in ipairs(self) do
			if type(child) == "table" then
				if getOption(child,name) == value then tinsert(list,child) end
				getDescendantsByOption(child,name,value,list)
			end
		end
	end
	function collection:getDescendantsByOption(name,value)
		if type(name) ~= "string" then
			error(("[collection.getDescendantsByOption] getDescendantsByOption method arg 1 'name' must be a string, but got %s"):format(type(name)))
		end
		if type(value) ~= "string" then
			error(("[collection.getDescendantsByOption] getDescendantsByOption method arg 2 'value' must be a string, but got %s"):format(type(value)))
		end
		local list = {}
		if getOption(self,name) == value then tinsert(list,self) end
		getDescendantsByOption(self,name,value,list)
		return new(list)
	end

	---------------------------------------------
	--           Methods : Id selector
	---------------------------------------------
	function collection:getFirstChildById(id)
		if type(id) ~= "string" then
			error(("[collection.getFirstChildById] getFirstChildById method arg 1 'id' must be a string, but got %s"):format(type(id)))
		end
		return self:getFirstChildByOption("id",id)
	end

	function collection:getChildrenById(id)
		if type(id) ~= "string" then
			error(("[collection.getChildrenById] getFirstChildById method arg 1 'id' must be a string, but got %s"):format(type(id)))
		end
		return self:getChildrenByOption("id",id)
	end

	function collection:getFirstDescendantById(id)
		if type(id) ~= "string" then
			error(("[collection.getFirstDescendantById] getFirstDescendantById method arg 1 'id' must be a string, but got %s"):format(type(id)))
		end
		return self:getFirstDescendantByOption("id",id)
	end

	function collection:getDescendantsById(id)
		if type(id) ~= "string" then
			error(("[collection.getDescendantsById] getDescendantsById method arg 1 'id' must be a string, but got %s"):format(type(id)))
		end
		return self:getDescendantsByOption("id",id)
	end

	---------------------------------------------
	--         Methods : Class selector
	---------------------------------------------
	local hasToken = tokenList.has
	local classNameParse = classNameParser.parse
	local function hasClass(self,class)
		local str = getOption(self,"class")
		return str and hasToken(classNameParse(str),class)
	end

	function collection:getFirstChildByClassName(class)
		if type(class) ~= "string" then
			error(("[collection.getFirstChildByClassName] getFirstChildByClassName method arg 1 'class' must be a string, but got %s"):format(type(class)))
		end
		for index,child in ipairs(self) do
			if hasClass(child,class) then
				return child,index
			end
		end
	end

	function collection:getChildrenByClassName(class)
		if type(class) ~= "string" then
			error(("[collection.getChildrenByClassName] getChildrenByClassName method arg 1 'class' must be a string, but got %s"):format(type(class)))
		end
		local children = {}
		for _,child in ipairs(self) do
			if type(child) == "table" and hasClass(child,class) then
				tinsert(children,child)
			end
		end
		return new(children)
	end

	local function getFirstDescendantByClassName(self,class)
		local result
		for _,child in ipairs(self) do
			if type(child) == "table" then
				if hasClass(child,class) then return child end
				result = getFirstDescendantByClassName(child,class)
				if result then return result end
			end
		end
	end
	function collection:getFirstDescendantByClassName(class)
		if type(class) ~= "string" then
			error(("[collection.getFirstDescendantByClassName] getFirstDescendantByClassName method arg 1 'class' must be a string, but got %s"):format(type(class)))
		end
		if hasClass(self,class) then return self end
		return getFirstDescendantByClassName(self,class)
	end

	local function getDescendantsByClassName(self,class,list)
		for _,child in ipairs(self) do
			if type(child) == "table" then
				if hasClass(child,class) then tinsert(list,child) end
				getDescendantsByClassName(child,class,list)
			end
		end
	end
	function collection:getDescendantsByClassName(class)
		if type(class) ~= "string" then
			error(("[collection.getDescendantsByClassName] getDescendantsByClassName method arg 1 'class' must be a string, but got %s"):format(type(class)))
		end
		local list = {}
		if hasClass(self,class) then tinsert(list,self) end
		getDescendantsByClassName(self,class,list)
		return new(list)
	end

	---------------------------------------------
	--         Methods : Query selector
	---------------------------------------------
	function collection:querySelector(selector)
		if type(selector) == "string" then

		end

		while true do

		end
	end
end

---------------------------------------------
--            Module: element
---------------------------------------------
---@class element:collection
local element = setmetatable({},collection) do

	local pcallGetMetatable = utility.pcallGetMetatable
	-- check is instance of element class
	function element.isElement(object)
		if type(object) == "table" and pcallGetMetatable(object) == element and (not object.__disposed) then
			return true
		end
		return false
	end
	local isElement = element.isElement
	local pcallGet = utility.pcallGet
	local function isRoot(object)
		if type(object) ~= "table" or (nil == pcallGet(object,"__ROOT")) then
			return false
		end
		return true
	end

	function element.new(tag,option,optionalChildren,optionalParent)
		local this = optionalChildren or {}
		this.tag = tag or ""
		this.option = option or {}
		if optionalParent then
			if (not isElement(optionalParent)) and (not isRoot(optionalParent)) then
				error(("[element.new] class 'element' constructor arg 4 'optionalParent' must be a element or root, but got %s"):format(type(optionalParent)))
			end
			this.__parent = optionalParent
			tinsert(optionalParent,this)
			this.__pindex = #optionalParent
		end
		if optionalChildren then
			for index,child in ipairs(optionalChildren) do
				if isElement(child) then
					local childParent = child.__parent
					if childParent then
						tremove(childParent,child.__pindex)
					end
					child.__parent = this
					child.__pindex = index
				end
			end
		end
		setmetatable(this,element)
		return this
	end

	local toXmlStr = utility.toXmlStr
	function element:debug(_buffer,_depth)
		_depth = _depth or 0
		_buffer = _buffer or {}

		if _depth ~= 0 then tinsert(_buffer,"\n") end
		tinsert(_buffer,srep("  ",_depth))
		if isElement(self) or isRoot(self) then
			tinsert(_buffer,"<")
			tinsert(_buffer,self.tag)
			for i,v in pairs(self.option) do
				tinsert(_buffer," ")
				tinsert(_buffer,i)
				tinsert(_buffer,"=")
				tinsert(_buffer,"\"")
				tinsert(_buffer,toXmlStr(v))
				tinsert(_buffer,"\"")
			end
			tinsert(_buffer,#self == 0 and "/>" or ">")
			tinsert(_buffer," :") -- debug
			tinsert(_buffer,tostring(self.index))
			if #self ~= 0 then
				for _,v in ipairs(self) do
					element.debug(v,_buffer,_depth+1)
				end
				tinsert(_buffer,"\n")
				tinsert(_buffer,srep("  ",_depth))
				tinsert(_buffer,"</")
				tinsert(_buffer,self.tag)
				tinsert(_buffer,">")
			end
		else
			tinsert(_buffer,self)
		end

		if _depth == 0 then return tconcat(_buffer) end
	end

	-- index
	function element:__index(key)
		if key == "parent" then
			return self.__parent
		elseif key == "index" then
			return self.__pindex
		elseif key == "length" then
			return #self
		end
		return element[key]
	end
	function element:__newindex(key,value)
		if key == "index" or key == "parent" then
			error(("[element.%s] property %s is readonly value"):format(key,key))
		end
		rawset(self,key,value)
	end

	-- tostring
	function element:__tostring()
		return ("<%s>"):format(tostring(self.tag))
	end

	---Check element has some child
	---@param child element|string child of you will find index
	---@return boolean hasChild
	function element:includes(child)
		if (not isElement(child)) and type(child) ~= "string" then
			error(("[element.remove] remove method arg 1 'child' must be a element or number, but got %s"):format(type(child)))
		end
		for i,v in ipairs(self) do
			if v == child then
				return true
			end
		end
		return false
	end

	local function indexOf(self,child)
		for index,item in ipairs(self) do
			if item == child then
				return index
			end
		end
		return nil
	end
	---Get index of element or string
	---@param child element|string child of you will find index
	---@return number|nil index index of input child
	function element:indexOf(child)
		if (not isElement(child)) and type(child) ~= "string" then
			error(("[element.indexOf] remove method arg 1 'child' must be a element or number, but got %s"):format(type(child)))
		end
		return indexOf(self,child)
	end
	---Check element is empty
	---@return boolean isEmpty is this element empty
	function element:isEmpty()
		return #self == 0
	end

	local function removeIndex(self,index)
		local child = self[index]
		if not child then
			return nil
		end
		child.__parent = nil
		child.__pindex = nil
		local removed = tremove(self,index)
		if not removed then return nil end

		-- update children's pindex
		for i = index,#self do
			local item = self[i]
			if isElement(item) then
				item.__pindex = i
			end
		end
		return removed
	end
	---Remove item with index
	---@param index number index of item that you want to remove (ZERO BASED INDEX!!)
	---@return string|nil|element removed removed item
	function element:removeIndex(index)
		if type(index) ~= "number" and index ~= nil then
			error(("[element.removeIndex] removeIndex method arg 1 'index' must be a number or nil, but got %s"):format(type(index)))
		end
		return removeIndex(self,index or #self)
	end

	---Destroy it selfs and remove it self from parent
	function element:destroy()
		for _,child in ipairs(self) do
			if isElement(child) then
				child:destroy()
			end
		end

		local parent = self.__parent
		if parent then
			removeIndex(parent,self.__pindex)
		end

		self.__parent = nil
		self.__pindex = nil
		self.__disposed = true

		setmetatable(self,nil)
	end

	---Remove item with index
	---@param child element|string index of item that you want to remove
	---@return string|nil|element removed removed item
	function element:remove(child)
		if (not isElement(child)) and type(child) ~= "string" then
			error(("[element.remove] remove method arg 1 'child' must be a element or string, but got %s"):format(type(child)))
		end
		local index = self:indexOf(child)
		if not index then
			error(("[element.remove] child %s is not exist on %s"):format(tostring(child),tostring(self)))
		end
		return self:removeIndex(index)
	end

	local function insert(self,child,index)
		if index then
			tinsert(self,index,child)
		else
			tinsert(self,child)
		end
		if isElement(child) then
			local parent = child.__parent
			if parent then
				tremove(parent,child.__pindex)
			end
			child.__pindex = index or #self
			child.__parent = self
		end
		if index then
			for i=index+1,#self do
				local item = self[i]
				if isElement(item) then
					item.__pindex = i
				end
			end
		end
	end
	---Insert child at index
	---@param child element|string item to insert
	---@param index number|nil position of item
	function element:insert(child,index)
		local childIsElement = isElement(child)
		if (not childIsElement) and type(child) ~= "string" then
			error(("[element.insert] insert method arg 1 'child' must be a element or string, but got %s"):format(type(child)))
		elseif type(index) ~= "number" and index ~= nil then
			error(("[element.insert] insert method arg 2 'index' must be a number or nil, but got %s"):format(type(index)))
		end
		return insert(self,child,index)
	end

	---Replace child at index
	---@param child element|string item to replace by
	---@return string|element|nil replacedItem replaced item
	function element:replace(child,index)
		local childIsElement = isElement(child)
		if (not childIsElement) and type(child) ~= "string" then
			error(("[element.replace] replace method arg 1 'child' must be a element or string, but got %s"):format(type(child)))
		elseif type(index) ~= "number" then
			error(("[element.replace] replace method arg 2 'index' must be a number, but got %s"):format(type(index)))
		end
		local item = self[index]
		self[index] = child
		if childIsElement then
			local parent = child.__parent
			if parent then
				tremove(parent,child.__pindex)
			end
			child.__pindex = index
			child.__parent = self
		end
		if isElement(item) then
			item.__parent = nil
			item.__pindex = nil
		end
		return item
	end

	---js style methods
	---Add child at end of children list
	---@param child element|string item to add
	---@return number position position of new child
	function element:push(child)
		if (not isElement(child)) and type(child) ~= "string" then
			error(("[element.push] push method arg 1 'child' must be a element or string, but got %s"):format(type(child)))
		end
		insert(self,child)
		return #self
	end
	---js style methods
	---Remove child which positioned at end of children list
	---@return element|string|nil child remove child
	---@return number|nil position position of removed child
	function element:pop()
		local index = #self
		local removed = removeIndex(self,index)
		if removed then
			return removed,index
		end
		return nil,nil
	end
	---js style methods
	---Remove child which positioned at start of children list
	---@return element|string|nil child remove child
	function element:shift()
		return removeIndex(self,1)
	end
	---js style methods
	---Add child at start of children list
	---@param child element|string item to add
	function element:unshift(child)
		if (not isElement(child)) and type(child) ~= "string" then
			error(("[element.unshift] unshift method arg 1 'child' must be a element or string, but got %s"):format(type(child)))
		end
		insert(self,child,1)
	end
	---Remove one or many items. replace is not allowed
	---@param start number start index of removing (ZERO BASED INDEX!!)
	---@param length number start index of removing (ZERO BASED INDEX!!)
	---@return nil|table removed removed item
	function element:splice(start,length)
		local items = {}
		for i=1,length do
			tinsert(items,self:removeIndex(start))
		end
		return items
	end
end

---------------------------------------------
--             Module: Root
---------------------------------------------
local root = setmetatable({},element) do
	root.__index = root
	root.__ROOT = true
	root.tag = "root"

	function root.new(children)
		local this = children or {}
		setmetatable(this,root)
		return this
	end
end

---------------------------------------------
--         Module: parserBase
---------------------------------------------
-- need hooks
local parserBase = {} do
	local opRegex = {}

	-- utf8 char
	local OP_UTFCHAR = #opRegex+1
	tinsert(opRegex,"^("..utility.utf8OnlyRegex..")")

	local OP_WHITESPACE = #opRegex+1
	tinsert(opRegex,"^[ \n\t]+")

	-- Comment
	local OP_COMMENT_START = #opRegex+1
	tinsert(opRegex,"^<!%-")
	local OP_COMMENT_END = #opRegex+1
	tinsert(opRegex,"^%-%->")
	local REGEX_COMMENT_END = opRegex[OP_COMMENT_END]

	-- Doctype
	-- e) DOCTYPE
	-- TODO: DTD ENTITY and ELEMENT
	local OP_DTD = #opRegex+1
	tinsert(opRegex,"^<!DOCTYPE") -- closes with >

	-- CDATA
	local OP_CDATA_START = #opRegex+1
	tinsert(opRegex,"^<![CDATA[")
	local OP_CDATA_END = #opRegex+1
	tinsert(opRegex,"^]]>")

	-- tag
	local OP_OPEN = #opRegex+1
	tinsert(opRegex,"^<([!/]?)[\32\t\n\r]*([%w:%-_]*)")
	local OP_CLOSE = #opRegex+1
	tinsert(opRegex,"^([!/]?)>")
	
	-- char
	local OP_STRING = #opRegex+1
	tinsert(opRegex,"^(.+)")

	local parseString = utility.parseString

	function parserBase:parseTag()

	end

	function parserBase:parseDTD()

	end

	function parserBase:parseCDATA()

	end

	function parserBase:parseXml(str)
		local contentRoot = {}
		while true do

		end
		return root.new(contentRoot)
	end

	function parserBase.createNewParser(options)
		local this = {}
		if options then
			this.emptyTags = options.emptyTags
		end
	end

end

---------------------------------------------
--           Module: xmlParser
---------------------------------------------
-- normal xml parser

---------------------------------------------
--          Module: htmlParser
---------------------------------------------
-- escape <br> or some empty tags
-- <pre> can hold spaces/ \n
-- \n -> space

return {
	root = root;
	element = element;
	tokenList = tokenList;
	selectorParser = selectorParser;
	utility = utility;
	querySelector = querySelector;
	classNameParser = classNameParser;
	classList = classList;
	collection = collection;
	parserBase = parserBase;
}
