local parser = require'objlua.parser'
local formatlua = require'objlua.formatlua'

objlua = { }
objlua._VERSION = "1.0.0"
objlua._NAME = "ObjLua"
objlua._COPYRIGHT = "Copyright (C) 2013 Elijah Frederickson"

function objlua._FormatHeader()
    return objlua._NAME 
        .. " " .. objlua._VERSION 
        .. " " .. objlua._COPYRIGHT
end

-- Based heavily off of olua code :3

function objlua.translate(intext, name)
	local s, ast = parser.Parse(intext, name)
    if not s then return s, ast end
	return formatlua(ast)
end

function objlua.loadstring(intext, chunkname)
    chunkname = chunkname or ""
	local outtext, e = objlua.translate(intext, chunkname)
    --print(outtext, e)
    --if chunkname:find("exception") then f,x=io.open(chunkname .. "XXX", "wb") if f then f:write(outtext) f:close() else print(f,x) end end
	if outtext then
		return loadstring(outtext, "@"..chunkname)
	end
	return nil, e
end

function objlua.loadstream(file, filename)
    -- Compile and return the module
	local m, e = objlua.loadstring(assert(file:read("*a")), filename)
	if not m then
		error(e)
	end
    return function(...)
		local args = {...}
		local t = { xpcall(
			function() m(unpack(args)) end,
			function(e)
				if (type(e) == "table") then
					return e
				end
                return e
				--return (objlua and objlua.getClass and objlua.getClass("Exception")
                --    and objlua.getClass("Exception"):createWithMessage(e)) or e
			end) }
		if not t[1] then
			error(tostring(t[2]))
		end
        table.remove(t, 1)
		return unpack(t)
	end 
end

function objlua.dofile(filename, name)
    local f, e = io.open(filename, 'rb')
    if not f then error(e) end
    return objlua.loadstream(f, name or "")()
end

local function oluaload(modulename)
	local errmsg = ""
	local modulepath = string.gsub(modulename, "%.", "/")
	for path in string.gmatch(package.oluapath, "([^;]+)") do
		local filename = string.gsub(path, "%?", modulepath)
		local file = io.open(filename, "rb")
		if file then
			--print("Loading "..filename)
			local chunk = assert(objlua.loadstream(file, filename))
			file:close()
			return chunk
		end
		errmsg = errmsg.."\n\tno file '"..filename.."' (checked with custom loader)"
	end
	return errmsg
end

-- Copy the standard Lua path to make an objlua path.

package.oluapath = package.path:gsub("%.lua", ".olua")

-- Install our new loader.

table.insert(package.loaders, 2, oluaload)

if _VERSION:match('5.1') then
    require'objlua.runtime51'
else
    error("Unsupported Lua version")
end
require'objlua.bcl'

return objlua
