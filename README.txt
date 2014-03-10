ObjLua brings Objective-C like syntax extensions to Lua. 
Compiles into Lua, with a Runtime (implemented in ObjLua) and a class library. 

License: WTFPL

TODO:
some sort of garbage collection

Eventual help on ObjLua, currently unsorted references/notes
Large parts are still mostly unimplemented or buggy. I'm slowly working on it though. 
Eventually a C/ObjC binding may be written so that on iOS it replaces plain Lua bindings (e.g. Link2App)
Currently supports 5.1 and 5.2 (by means of a getfenv wrapper), 5.0- is not supported. LuaJIT probably is not supported either. RBX.lua definitely wont work and it (probably) never will.

When defining vararg ObjLua methods, e.g:
	-x:... do
use "arg" not "...", like:
	local arg1, arg2 = arg[1], arg[2]
not:
	local arg = { ... }
oh look, saves a step too...

Features:
Full class system with inheritance and interfaces (theoretically, might not all work right though)
Implicit native-to-ObjLua type conversion; implicit type conversions

@implementation <class> (<bleh>) : <Inherits / Implements>
@end

Properties (well, emulating them at least):
@implementation test
-(type) property do
    return self._property
end
- setProperty:(type)value do
    self._property = value
end
@end

then, to use:
t.property = value    :: runtime (not compiler!) changes to [t setProperty:value]
[t property] or t.property()   :: t.property just returns the function pointer. TODO. doubt it will get fixed though...


Method alias:
@alias("toString", "TOSTRINGALIAS") --> objlua.createMethodAlias(<classname>, "toString", "TOSTRINGALIAS")
@alias (hash, getHashCode)