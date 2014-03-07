package.path = package.path .. ";./?/init.lua"
require'ObjLua'
local parser = require'ObjLua.parser'
local fmt = require'ObjLua.formatlua'

local function splitFilename(name)
	--table.foreach(arg, print)
	if name:find(".") then
		local p, ext = name:match("()%.([^%.]*)$")
		if p and ext then
			if #ext == 0 then
				return name, nil
			else
				local filename = name:sub(1,p-1)
				return filename, ext
			end
		else
			return name, nil
		end
	else
		return name, nil
	end
end

if #arg >= 1 then
	local name, ext = splitFilename(arg[1])
	local outname = arg[2] or name
    if ext ~= "olua" then
        print("Cannot compile non .olua files")
        return
    end
    ext = 'lua'
	if ext then outname = outname.."."..ext end
	--
	local inf = io.open(arg[1], 'r')
	if not inf then
		print("Failed to open `"..arg[1].."` for reading")
		return
	end
	--
	local sourceText = inf:read('*all')
	inf:close()
	--
	local st, ast = parser.Parse(sourceText)
	if not st then
		--we failed to parse the file, show why
		print(ast)
		return
	end
    
	local outf = io.open(outname, 'w')
	if not outf then
		print("Failed to open `"..outname.."` for writing")
		return
	end
	outf:write(fmt(ast))
	outf:close()
else
	print("Invalid arguments, Usage: oluac source [destination]")
end
