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
	}

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
	local OP_ACCESS_DIRECT_CHILD = #opNames+1
	accessNames[OP_ACCESS_DIRECT_CHILD] = "DIRECT_CHILD"
	tinsert(opRegex,"^[ \t\n]*>[ \t\n]*")
	tinsert(opNames,"OP_ACCESS_DIRECT_CHILD")
	local OP_ACCESS_SIBLING = #opNames+1
	accessNames[OP_ACCESS_SIBLING] = "SIBLING"
	tinsert(opRegex,"^[ \t\n]*%+[ \t\n]*")
	tinsert(opNames,"OP_ACCESS_SIBLING")
	local OP_ACCESS_AFTER = #opNames+1
	accessNames[OP_ACCESS_AFTER] = "AFTER"
	tinsert(opRegex,"^[ \t\n]*~[ \t\n]*")
	tinsert(opNames,"OP_ACCESS_AFTER")

	-- More
	local OP_MORE = #opNames+1
	tinsert(opRegex,"^[ \t\n]*,[ \t\n]*")
	tinsert(opNames,"OP_MORE")

	-- DFS
	local OP_ACCESS_DESCENDANT = #opNames+1
	accessNames[OP_ACCESS_DESCENDANT] = "DESCENDANT"
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
	tinsert(opRegex,"("..utf8.charpattern..")")
	tinsert(opNames,"OP_UTFCHAR")

	-- condition types
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

	-- property parser regexs
	local propertyRegex = {
		opRegex[OP_PROPERTY_OPEN],
		opRegex[OP_PROPERTY_CLOSE],
		opRegex[OP_STRING_DOUBLE_QUOTE],
		opRegex[OP_STRING_SINGLE_QUOTE],
		opRegex[OP_PROPERTY_CONDITION_EQUAL],
		opRegex[OP_PROPERTY_CONDITION_WORD],
		opRegex[OP_PROPERTY_CONDITION_DASH],
		opRegex[OP_PROPERTY_CONDITION_BEGIN],
		opRegex[OP_PROPERTY_CONDITION_END],
		opRegex[OP_PROPERTY_CONDITION_CONTAIN],
		"^([%-_%w]+)",
		"^\\(.?)",
		"("..utf8.charpattern..")"
	}

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

	---------------------------------------------
	--     selectorParser Private methods
	---------------------------------------------
	function selectorParser._parseString(str,init,initChar)
		local pos = init+1
		local buffer = {}
		while true do
			local start,_,char = sfind(str,"([\\'\"])",pos)
			if not start then
				error(("[SelectorParser._parseString: buffer exhausted] String is opened at %s char (%s). But String is not closed."):format(formatNTH(init),initChar))
			end

			-- insert last string
			tinsert(buffer,ssub(str,pos,start-1))
			pos = start + 1

			if char == initChar then
				-- closed
				break
			elseif char == "\\" then
				-- escape
				local target = ssub(str,pos,pos)
				-- if not newChar then
					-- error(("[SelectorParser._parseString: unknown escape] Escape '\\%s', which is %s char is not defined."):format(ssub(str,pos,pos),formatNTH(init)))
				-- end
				-- tinsert(buffer,newChar)
				tinsert(buffer,stringEscapes[target] or target)
				pos = pos + 1
			else
				tinsert(buffer,char)
			end
		end
		return tconcat(buffer),pos
	end

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

			elseif op == 1 then
				-- opRegex[OP_PROPERTY_OPEN],
				error("[SelectorParser._parseProperty: unexpected property open] property open bracket was got inside of the property selector. Possible cause: '[' token was got in inside of '[]' (example: [label[]). Did you want to escape that token? place '\\' in front of '[' to resolve it"..buildErrorTrace(str,startAt,endAt,-1))

			elseif op == 2 then
				-- opRegex[OP_PROPERTY_CLOSE],
				-- return data,endAt+1
				break

			elseif op == 3 then
				-- opRegex[OP_STRING_DOUBLE_QUOTE],
				if not condition then
					error("[SelectorParser._parseProperty: unexpected string start] string can only start after property condition operator. Possible cause: ' or \" token was got before '~=', '^=', '*=' or some operators (example: [\"label\" ~= a]). Did you want to escape that token? place '\\' in front of that token to resolve it"..buildErrorTrace(str,startAt,endAt,-1))
				elseif #value ~= 0 then
					error("[SelectorParser._parseProperty: unexpected string start] got unexpected token \" in value. Possible cause: ' or \" token was got in inside of value (example: [label ~= a\"b\"]). Did you want to escape that token? place '\\' in front of that token to resolve it"..buildErrorTrace(str,startAt,endAt,-1))
				end
				matched,pos = selectorParser._parseString(str,startAt,matched)
				value[-1] = true
				tinsert(value,matched)

			elseif op == 4 then
				-- opRegex[OP_STRING_SINGLE_QUOTE],
				if not condition then
					error("[SelectorParser._parseProperty: unexpected string start] string can only start after property condition operator. Possible cause: ' or \" token was got before '~=', '^=', '*=' or some operators (example: [\"label\" ~= a]). Did you want to escape that token? place '\\' in front of that token to resolve it"..buildErrorTrace(str,startAt,endAt,-1))
				elseif #value ~= 0 then
					error("[SelectorParser._parseProperty: unexpected string start] got unexpected token ' in value. Possible cause: ' or \" token was got in inside of value (example: [label ~= a\"b\"]). Did you want to escape that token? place '\\' in front of that token to resolve it"..buildErrorTrace(str,startAt,endAt,-1))
				end
				matched,pos = selectorParser._parseString(str,startAt,matched)
				value[-1] = true
				tinsert(value,matched)

			elseif op == 5 then
				-- opRegex[OP_PROPERTY_CONDITION_EQUAL],
				if condition then
					error("[SelectorParser._parseProperty: condition operator was got already] Possible cause: got repeated operator token (example: [label ~= ~=]). Did you want to escape that token? place '\\' in each char of token to resolve it"..buildErrorTrace(str,startAt,endAt,OP_PROPERTY_CONDITION_WORD))
				end
				condition = OP_PROPERTY_CONDITION_EQUAL
				pos = endAt+1

			elseif op == 6 then
				-- opRegex[OP_PROPERTY_CONDITION_WORD],
				if condition then
					error("[SelectorParser._parseProperty: unexpected condition token] condition operator was got already. Possible cause: got repeated operator token (example: [label ~= ~=]). Did you want to escape that token? place '\\' on each char of token to resolve it"..buildErrorTrace(str,startAt,endAt,OP_PROPERTY_CONDITION_WORD))
				end
				condition = OP_PROPERTY_CONDITION_WORD
				pos = endAt+1

			elseif op == 7 then
				-- opRegex[OP_PROPERTY_CONDITION_DASH],
				if condition then
					error("[SelectorParser._parseProperty: unexpected condition token] condition operator was got already. Possible cause: got repeated operator token (example: [label ~= ~=]). Did you want to escape that token? place '\\' on each char of token to resolve it"..buildErrorTrace(str,startAt,endAt,OP_PROPERTY_CONDITION_DASH))
				end
				condition = OP_PROPERTY_CONDITION_DASH
				pos = endAt+1

			elseif op == 8 then
				-- opRegex[OP_PROPERTY_CONDITION_BEGIN],
				if condition then
					error("[SelectorParser._parseProperty: unexpected condition token] condition operator was got already. Possible cause: got repeated operator token (example: [label ~= ~=]). Did you want to escape that token? place '\\' on each char of token to resolve it"..buildErrorTrace(str,startAt,endAt,OP_PROPERTY_CONDITION_BEGIN))
				end
				condition = OP_PROPERTY_CONDITION_BEGIN
				pos = endAt+1

			elseif op == 9 then
				-- opRegex[OP_PROPERTY_CONDITION_END],
				if condition then
					error("[SelectorParser._parseProperty: unexpected condition token] condition operator was got already. Possible cause: got repeated operator token (example: [label ~= ~=]). Did you want to escape that token? place '\\' on each char of token to resolve it"..buildErrorTrace(str,startAt,endAt,OP_PROPERTY_CONDITION_END))
				end
				condition = OP_PROPERTY_CONDITION_END
				pos = endAt+1

			elseif op == 10 then
				-- opRegex[OP_PROPERTY_CONDITION_CONTAIN],
				if condition then
					error("[SelectorParser._parseProperty: unexpected condition token] condition operator was got already. Possible cause: got repeated operator token (example: [label ~= ~=]). Did you want to escape that token? place '\\' on each char of token to resolve it"..buildErrorTrace(str,startAt,endAt,OP_PROPERTY_CONDITION_CONTAIN))
				end
				condition = OP_PROPERTY_CONDITION_CONTAIN
				pos = endAt+1

			elseif op == 11 then
				-- "^([%-_%w]+)",
				-- word
				if value[-1] then
					error("[SelectorParser._parseProperty: unexpected token after value string] property selector value was set by string already. you can't give more value. Possible cause: got more tokens after value string (example: [label = \"a\"a])")
				end
				tinsert(condition and value or name,matched)
				pos = endAt+1

			elseif op == 12 then
				-- "^\\(.?)",
				-- escape
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

	-- print(prettyParsed(selectorParser.parse("wow.a > b>a  z-.j    [label][src\\||='wow']")))
	-- if spec then
	--     local ok,result

	--     ok,result = pcall(selectorParser._parseString,"?  'Test String'  ?",4,"'")
	--     assert(ok and result == "Test String",sformat("SPEC: not match with expection (result: '%s')",result))

	--     ok,result = pcall(selectorParser._parseString,"?  'Test String  ?",4,"'")
	--     assert(not ok,sformat("SPEC: not match with expection (result: '%s')",result))

	--     ok,result = pcall(selectorParser._parseString,"?  'Test\\nString'  ?",4,"'")
	--     assert(ok and result == "Test\nString",sformat("SPEC: not match with expection (result: '%s')",result))
	-- end
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

	--[[!autoDoc!
	@define class:collection
		function collection:getFirstChildByTag(tag)
			i: get child object by using tag
			arg1 string tag: tag of child
			return: class:collection/nil child item , int index
	!autoDoc!]]
	---get child object by using tag
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

	--[[!autoDoc!
	@define class:collection
		function collection:getChildrenByTag(tag)
			i: get child objects by using tag
			arg string tag: tag of children
			return: table of class:collection/nil
	!autoDoc!]]
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
		if self.tag == tag then return self end
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
		if self.tag == tag then tinsert(list,self) end
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
	function collection:getFirstChildById(name,value)
		if type(name) ~= "string" then
			error(("[collection.getFirstChildById] getFirstChildById method arg 1 'name' must be a string, but got %s"):format(type(name)))
		end
		if type(value) ~= "string" then
			error(("[collection.getFirstChildById] getFirstChildById method arg 2 'value' must be a string, but got %s"):format(type(value)))
		end
		return self:getFirstChildByOption("id",value)
	end

	function collection:getChildrenById(name,value)
		if type(name) ~= "string" then
			error(("[collection.getChildrenById] getChildrenById method arg 1 'name' must be a string, but got %s"):format(type(name)))
		end
		if type(value) ~= "string" then
			error(("[collection.getChildrenById] getFirstChildById method arg 2 'value' must be a string, but got %s"):format(type(value)))
		end
		return self:getChildrenByOption("id",value)
	end

	function collection:getFirstDescendantById(name,value)
		if type(name) ~= "string" then
			error(("[collection.getFirstDescendantById] getFirstDescendantById method arg 1 'name' must be a string, but got %s"):format(type(name)))
		end
		if type(value) ~= "string" then
			error(("[collection.getFirstDescendantById] getFirstDescendantById method arg 2 'value' must be a string, but got %s"):format(type(value)))
		end
		return self:getFirstDescendantByOption("id",value)
	end

	function collection:getDescendantsById(name,value)
		if type(name) ~= "string" then
			error(("[collection.getDescendantsById] getDescendantsById method arg 1 'name' must be a string, but got %s"):format(type(name)))
		end
		if type(value) ~= "string" then
			error(("[collection.getDescendantsById] getDescendantsById method arg 2 'value' must be a string, but got %s"):format(type(value)))
		end
		return self:getDescendantsByOption("id",value)
	end

end

---------------------------------------------
--            Module: element
---------------------------------------------
---@class element:collection
local element = setmetatable({},collection) do
	element.__index = element

	local function pcallGetMetatable(t)
		local ok,result = pcall(getmetatable,t)
		if ok then return result end
	end

	-- check is instance of element class
	function element.isElement(object)
		if type(object) == "table" and pcallGetMetatable(object) == element then
			return true
		end
		return false
	end
	local isElement = element.isElement

	---Remove item with index
	---@param index number index of item that you want to remove
	---@return string|nil|element removed removed item
	function element:removeIndex(index)
		local this = self[index]
		if not this then
			error("[item.remove] couldn't remove  because anything match index with '%s' not found from Item")
		end
		this.__parent = nil
		this.__pindex = nil
		return tremove(self,index)
	end
end




---Remove item with value (find and remove)
---@param value string|xmlItem item to remove
---@return boolean passed
function item:removeValue(value)
	local pass,ty = isItem(value)
	if not (pass or ty == "string") then
		error(("[item.removeValue] removeValue method arg 1 'value' must be an item object or a string, but got %s"):format(type(value)))
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
		error(("[item.insert] insert method arg 2 'value' must be an item object or a string, but got %s"):format(type(value)))
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
		error(("[item.append] append method arg 1 'value' must be an item object or a string, but got %s"):format(type(value)))
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
		error(("[item.prepend] prepend method arg 1 'value' must be an item object or a string, but got %s"):format(type(value)))
	end
	if ty ~= "string" then
		value.__parent = self
		value.__pindex = 1
	end
	tinsert(self,1,value)
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
		error(("[item.] new function arg 1 'tag' must be a string, but got %s"):format(type(tag)))
	elseif option ~= nil and type(option) ~= "table" then
		error(("[item.] new function arg 2 'option' must be a nil or a table, but got %s"):format(type(option)))
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
				error(("[xmlToItem.] start tag and end tag is not match! startTag: %s; endTag: %s;"):format(now.tag,tag))
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
		error(("[itemToXml.] arg 1 'nitem' must be a table or an item, but got %s"):format(ty))
	end
	return itemToXml(nitem)
end

return module
