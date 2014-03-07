ObjLua brings Objective-C like syntax extensions to Lua. 
Compiles into Lua, with a Runtime (implemented in ObjLua) and a class library. 

License: WTFPL



Eventual help on ObjLua, currently unsorted references/notes
Large parts are still mostly unimplemented or buggy. I'm slowly working on it though. 
Eventually a C/ObjC binding may be written so that on iOS it replaces plain Lua bindings (e.g. Link2App)
Currently the runtime only supports 5.1, 5.0- and 5.2+ are not supported. LuaJIT probably is not supported either. RBX.lua definitely wont work and it (probably) never will.

When defining vararg ObjLua methods, e.g:
	-x:... do
use "arg" not "...", like:
	local arg1, arg2 = arg[1], arg[2]
not:
	local arg = { ... }

Features:
Full class system with inheritance and interfaces (theoretically, might not all work right though)
Implicit native-to-ObjLua type conversion; implicit type conversions

@implementation <class> (<bleh>) : <Inherits / Implements>
@end