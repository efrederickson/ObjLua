local p = require'objlua.parser'
local fmt = require'objlua.formatlua'

local code = io.open("objlua/runtime.olua", 'rb')
assert(code)
local c = code:read'*a'
code:close()
x,y = p.Parse(c)
assert(x)
f=io.open("a.out", "wb")
f:write(fmt(y))
f:close()

x, y = p.Parse[=[

local function what() print"what?" end

@implementation test : Object

@static (number) something = 4;
@static something2 = 'mwahaha';

-a:(String)text do
    --print"test"
    print("text='".. tostring(text).."'")
    --print(text, [self b:'oh','oh2' c:169], [super toString], [self toString], something)
    something = 5
end

+b:(string)... c:c do
--+b:b c:c do
    --print("+b", arg, unpack(arg))
    --print("c", assert(c))
    return 5, c
end

-c do
    what()
end

-(String) d do
    return "hola, senor"
end

@end


local x = [[test alloc] init]

--[x a:"ha"]
--[x a];
--(function (...) print("[x b]", [x b:... c:3]) end)("a", 2, "c");

[x a:'hoho']
x:a{a='haha'}
x:a("hehe")

x.y = function(potato) assert(self) assert(potato) return potato+5 end
--print("Y:",getmetatable(objlua.getClass'test').methods['y'])
--print("Y2:", x.y)
--print("Y3:", ([[test alloc] init]).y)
print("x:y(5)==10",assert(x:y(5)==10))
--print("x:y(5)==10",assert([x y:5]==10))

return "SUCCESS"
]=]
--print(x, y)
--if x then print(require('Util').PrintTable(y)) end
if x then
    --print(fmt(y))
    x, y = loadstring(fmt(y))
    --print(x, y)
    if not x then print(y) else print(x()) end
else
    print(y)
end
