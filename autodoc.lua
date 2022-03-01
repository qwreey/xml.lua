

local function toMd(env)

end

local function getIndents(str)
    local indent = 0
    for this in str:gmatch"^[ \t]+" do
        indent = indent + (this == "\t" and 4 or 1)
    end
    return indent/4
end

local function tagAndItem(str)
    return str:match"[ \t]+(.-): ?(.-)"
end

local function getFunction(str)
    return str:match"function (.-)%((.-)%)"
end

local function parse(str,env)
    local class,func
    for line in str:gmatch"[^\n]+" do
        class = class or line:match"@define calss:.-"
        local tag,item = tagAndItem(line)
        if tag and item then
            
        end
    end
end

local function scan(str)
    local env = {}
    for this in str:gmatch("!autoDoc!(.-)!autoDoc!") do
        parse(this,env)
    end
    return toMd()
end