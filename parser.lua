
--
-- ParseLua.lua
--
-- The main lua parser and lexer.
-- LexLua returns a Lua token stream, with tokens that preserve
-- all whitespace formatting information.
-- ParseLua returns an AST, internally relying on LexLua.
--

--require'Strict'

local util = require'objlua.Util'
local lookupify = util.lookupify

local WhiteChars = lookupify{' ', '\n', '\t', '\r'}
local EscapeLookup = {['\r'] = '\\r', ['\n'] = '\\n', ['\t'] = '\\t', ['"'] = '\\"', ["'"] = "\\'"}
local LowerChars = lookupify{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
							 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
							 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'}
local UpperChars = lookupify{'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',
							 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
							 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'}
local Digits = lookupify{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'}
local HexDigits = lookupify{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
							'A', 'a', 'B', 'b', 'C', 'c', 'D', 'd', 'E', 'e', 'F', 'f'}

local Symbols = lookupify{'+', '-', '*', '/', '^', '%', ',', '{', '}', '[', ']', '(', ')', ';', '#'}
local Scope = require'objlua.Scope'

local Keywords = lookupify{
	'and', 'break', 'do', 'else', 'elseif',
	'end', 'false', 'for', 'function', 'goto', 'if',
	'in', 'local', 'nil', 'not', 'or', 'repeat',
	'return', 'then', 'true', 'until', 'while',
};

local function Lex(src)
	--token dump
	local tokens = {}

	local st, err = pcall(function()
		--line / char / pointer tracking
		local p = 1
		local line = 1
		local char = 1
        
		--get / peek functions
		local function get()
			local c = src:sub(p,p)
			if c == '\n' then
				char = 1
				line = line + 1
			else
				char = char + 1
			end
			p = p + 1
			return c
		end
		local function peek(n)
			n = n or 0
			return src:sub(p+n,p+n)
		end
		local function consume(chars)
			local c = peek()
			for i = 1, #chars do
				if c == chars:sub(i,i) then return get() end
			end
		end
        local function consumeUntil(chars)
            local str = ""
            while true do
                local c = peek()
                for i = 1, #chars do
                    if c == chars:sub(i,i) or c == "" then
                        return str
                    end
                end
                str = str .. c
                get()
            end
        end

		--shared stuff
		local function generateError(err)
			return error(">> :"..line..":"..char..": "..err, 0)
		end

		local function tryGetLongString()
			local start = p
			if peek() == '[' then
				local equalsCount = 0
				local depth = 1
				while peek(equalsCount+1) == '=' do
					equalsCount = equalsCount + 1
				end
				if peek(equalsCount+1) == '[' then
					--start parsing the string. Strip the starting bit
					for _ = 0, equalsCount+1 do get() end

					--get the contents
					local contentStart = p
					while true do
						--check for eof
						if peek() == '' then
                            p = start
                            return nil
							--generateError("Expected `]"..string.rep('=', equalsCount).."]` near <eof>.", 3)
						end

						--check for the end
						local foundEnd = true
						if peek() == ']' then
							for i = 1, equalsCount do
								if peek(i) ~= '=' then foundEnd = false end
							end
							if peek(equalsCount+1) ~= ']' then
								foundEnd = false
							end
						else
							if peek() == '[' then
								-- is there an embedded long string?
								local embedded = true
								for i = 1, equalsCount do
									if peek(i) ~= '=' then
										embedded = false
										break
									end
								end
								if peek(equalsCount + 1) == '[' and embedded then
									-- oh look, there was
									depth = depth + 1
									for i = 1, (equalsCount + 2) do
										get()
									end
								end
							end
							foundEnd = false
						end
						--
						if foundEnd then
							depth = depth - 1
							if depth == 0 then
								break
							else
								for i = 1, equalsCount + 2 do
									get()
								end
							end
						else
							get()
						end
					end

					--get the interior string
					local contentString = src:sub(contentStart, p-1)

					--found the end. Get rid of the trailing bit
					for i = 0, equalsCount+1 do get() end

					--get the exterior string
					local longString = src:sub(start, p-1)

					--return the stuff
					return contentString, longString
				else
					return nil
				end
			else
				return nil
			end
		end

		--main token emitting loop
		while true do
			--get leading whitespace. The leading whitespace will include any comments
			--preceding the token. This prevents the parser needing to deal with comments
			--separately.
			local leading = { }
			local leadingWhite = ''
			local longStr = false
			while true do
				local c = peek()
				if c == '#' and peek(1) == '!' and line == 1 then
					-- #! shebang for linux scripts
					get()
					get()
					leadingWhite = "#!"
					while peek() ~= '\n' and peek() ~= '' do
						leadingWhite = leadingWhite .. get()
					end
					local token = {
						Type = 'Comment',
						CommentType = 'Shebang',
						Data = leadingWhite,
						Line = line,
						Char = char
					}
					token.Print = function()
						return "<"..(token.Type .. string.rep(' ', 7-#token.Type)).."  "..(token.Data or '').." >"
					end
					leadingWhite = ""
					table.insert(leading, token)
				end
				if c == ' ' or c == '\t' then
					--whitespace
					--leadingWhite = leadingWhite..get()
					local c2 = get() -- ignore whitespace
					table.insert(leading, { Type = 'Whitespace', Line = line, Char = char, Data = c2 })
				elseif c == '\n' or c == '\r' then
                    --if line == 1 then line = line + 1 end
					local nl = get()
					if leadingWhite ~= "" then
						local token = {
							Type = 'Comment',
							CommentType = longStr and 'LongComment' or 'Comment',
							Data = leadingWhite,
							Line = line,
							Char = char,
						}
						token.Print = function()
							return "<"..(token.Type .. string.rep(' ', 7-#token.Type)).."  "..(token.Data or '').." >"
						end
						table.insert(leading, token)
						leadingWhite = ""
					end
					table.insert(leading, { Type = 'Whitespace', Line = line, Char = char, Data = nl })
				elseif c == '-' and peek(1) == '-' then
					--comment
					get()
					get()
					leadingWhite = leadingWhite .. '--'
					local _, wholeText = tryGetLongString()
					if wholeText then
						leadingWhite = leadingWhite..wholeText
						longStr = true
					else
						while peek() ~= '\n' and peek() ~= '' do
							leadingWhite = leadingWhite..get()
						end
					end
				else
					break
				end
			end
			if leadingWhite ~= "" then
				local token = {
					Type = 'Comment',
					CommentType = longStr and 'LongComment' or 'Comment',
					Data = leadingWhite,
					Line = line,
					Char = char,
				}
				token.Print = function()
					return "<"..(token.Type .. string.rep(' ', 7-#token.Type)).."  "..(token.Data or '').." >"
				end
				table.insert(leading, token)
			end

			--get the initial char
			local thisLine = line
			local thisChar = char
			local errorAt = ":"..line..":"..char..":> "
			local c = peek()

			--symbol to emit
			local toEmit = nil

			--branch on type
			if c == '' then
				--eof
				toEmit = { Type = 'Eof' }

			elseif UpperChars[c] or LowerChars[c] or c == '_' then
				--ident or keyword
				local start = p
				repeat
					get()
					c = peek()
				until not (UpperChars[c] or LowerChars[c] or Digits[c] or c == '_')
				local dat = src:sub(start, p-1)
				if Keywords[dat] then
					toEmit = {Type = 'Keyword', Data = dat}
				else
					toEmit = {Type = 'Ident', Data = dat}
				end

			elseif Digits[c] or (peek() == '.' and Digits[peek(1)]) then
				--number const
				local start = p
				if c == '0' and peek(1) == 'x' then
					get();get()
					while HexDigits[peek()] do get() end
					if consume('Pp') then
						consume('+-')
						while Digits[peek()] do get() end
					end
				else
					while Digits[peek()] do get() end
					if consume('.') then
						while Digits[peek()] do get() end
					end
					if consume('Ee') then
						consume('+-')
						while Digits[peek()] do get() end
					end
				end
				toEmit = {Type = 'Number', Data = src:sub(start, p-1)}

			elseif c == '\'' or c == '\"' then
				local start = p
				--string const
				local delim = get()
				local contentStart = p
				while true do
					local c = get()
					if c == '\\' then
						get() --get the escape char
					elseif c == delim then
						break
					elseif c == '' then
						generateError("Unfinished string near <eof>")
					end
				end
				local content = src:sub(contentStart, p-2)
				local constant = src:sub(start, p-1)
				toEmit = {Type = 'String', Data = constant, Constant = content}

			elseif c == '[' then
				local content, wholetext = tryGetLongString()
				if wholetext then
					toEmit = {Type = 'String', Data = wholetext, Constant = content}
				else
					get()
					toEmit = {Type = 'Symbol', Data = '['}
				end

            elseif c == '@' then
                -- TODO: more keywords
                local str = consumeUntil(' \t\r\n(')
                if str == "@interface" then
                    toEmit = { Type = 'ObjLuaSymbol', Data = str }
                elseif str == "@implementation" then
                    toEmit = { Type = 'ObjLuaSymbol', Data = str }
                elseif str == "@try" then
                    toEmit = { Type = 'ObjLuaSymbol', Data = str }
                elseif str == "@catch" then
                    toEmit = { Type = 'ObjLuaSymbol', Data = str }
                elseif str == "@finally" then
                    toEmit = { Type = 'ObjLuaSymbol', Data = str }
                elseif str == "@end" then
                    toEmit = { Type = 'ObjLuaSymbol', Data = str }
                elseif str == "@throw" then
                    toEmit = { Type = 'ObjLuaSymbol', Data = str }
                elseif str == "@statics" then
                    toEmit = { Type = 'ObjLuaSymbol', Data = str }
                elseif str == "@static" then
                    toEmit = { Type = 'ObjLuaSymbol', Data = str }
                elseif str == "@property" then
                    toEmit = { Type = 'ObjLuaSymbol', Data = str }
                elseif str == "@alias" then
                    toEmit = { Type = 'ObjLuaSymbol', Data = str }
                else
                    generateError("Unexpected Symbol `"..c.."` in source.", 2)
                end
			elseif consume('>=<') then
				if consume('=') then
					toEmit = {Type = 'Symbol', Data = c..'='}
				else
					toEmit = {Type = 'Symbol', Data = c}
				end

			elseif consume('~') then
				if consume('=') then
					toEmit = {Type = 'Symbol', Data = '~='}
				else
					generateError("Unexpected symbol `~` in source.", 2)
				end

			elseif consume('.') then
				if consume('.') then
					if consume('.') then
						toEmit = {Type = 'Symbol', Data = '...'}
					else
						toEmit = {Type = 'Symbol', Data = '..'}
					end
				else
					toEmit = {Type = 'Symbol', Data = '.'}
				end

			elseif consume(':') then
				if consume(':') then
					toEmit = {Type = 'Symbol', Data = '::'}
				else
					toEmit = {Type = 'Symbol', Data = ':'}
				end

			elseif Symbols[c] then
				get()
				toEmit = {Type = 'Symbol', Data = c}

			else
				local contents, all = tryGetLongString()
				if contents then
					toEmit = {Type = 'String', Data = all, Constant = contents}
				else
					generateError("Unexpected Symbol `"..c.."` in source.", 2)
				end
			end

			--add the emitted symbol, after adding some common data
			toEmit.LeadingWhite = leading -- table of leading whitespace/comments
			--for k, tok in pairs(leading) do
			--	tokens[#tokens + 1] = tok
			--end

			toEmit.Line = thisLine
			toEmit.Char = thisChar
			toEmit.Print = function()
				return "<"..(toEmit.Type..string.rep(' ', 7-#toEmit.Type)).."  "..(toEmit.Data or '').." >"
			end
			tokens[#tokens+1] = toEmit

			--halt after eof has been emitted
			if toEmit.Type == 'Eof' then break end
		end
	end)
	if not st then
		return false, err
	end

	--public interface:
	local tok = {}
	local savedP = {}
	local p = 1
	
	function tok:getp()
		return p
	end
	
	function tok:setp(n)
		p = n
	end
	
	function tok:getTokenList()
		return tokens
	end
	
	--getters
	function tok:Peek(n)
		n = n or 0
		return tokens[math.min(#tokens, p+n)]
	end
	function tok:Get(tokenList)
		local t = tokens[p]
		p = math.min(p + 1, #tokens)
		if tokenList then
			table.insert(tokenList, t)
		end
		return t
	end
	function tok:Is(t)
		return tok:Peek().Type == t
	end

	--save / restore points in the stream
	function tok:Save()
		savedP[#savedP+1] = p
	end
	function tok:Commit()
		savedP[#savedP] = nil
	end
	function tok:Restore()
		p = savedP[#savedP]
		savedP[#savedP] = nil
	end

	--either return a symbol if there is one, or return true if the requested
	--symbol was gotten.
	function tok:ConsumeSymbol(symb, tokenList)
		local t = self:Peek()
		if t.Type == 'Symbol' then
			if symb then
				if t.Data == symb then
					self:Get(tokenList)
					return true
				else
					return nil
				end
			else
				self:Get(tokenList)
				return t
			end
		else
			return nil
		end
	end

	function tok:ConsumeKeyword(kw, tokenList)
		local t = self:Peek()
		if t.Type == 'Keyword' and t.Data == kw then
			self:Get(tokenList)
			return true
		else
			return nil
		end
	end

	function tok:IsKeyword(kw)
		local t = tok:Peek()
		return t.Type == 'Keyword' and t.Data == kw
	end

	function tok:IsSymbol(s)
		local t = tok:Peek()
		return t.Type == 'Symbol' and t.Data == s
	end

    function tok:ConsumeObjLuaSymbol(kw, tl)
        local t = self:Peek()
        if t.Type == 'ObjLuaSymbol' and t.Data == kw then
            self:Get(tl)
            return true
        else
            return false
        end
    end
    
	function tok:IsEof()
		return tok:Peek().Type == 'Eof'
	end

	return true, tok
end


local function Parse(src, chunkName)
	local st, tok
	if type(src) ~= 'table' then
		st, tok = Lex(src)
	else
		st, tok = true, src
	end
	if not st then
		return false, tok
	end
	--
	local function GenerateError(msg)
		local err = ">> :"..tok:Peek().Line..":"..tok:Peek().Char..": "..msg.."\n"
        if chunkName then 
            err = chunkName .. err
        end
		--find the line
		local lineNum = 0
		if type(src) == 'string' then
			for line in src:gmatch("[^\n]*\n?") do
				if line:sub(-1,-1) == '\n' then line = line:sub(1,-2) end
				lineNum = lineNum+1
				if lineNum == tok:Peek().Line then
					err = err..">> `"..line:gsub('\t','    ').."`\n"
					for i = 1, tok:Peek().Char do
						local c = line:sub(i,i)
						if c == '\t' then
							err = err..'    '
						else
							err = err..' '
						end
					end
					err = err.."   ^^^^"
					break
				end
			end
		end
        --error(err)
		return err
	end
	--
	local VarUid = 0
	-- No longer needed: handled in Scopes now local GlobalVarGetMap = {} 
	local VarDigits = {'_', 'a', 'b', 'c', 'd'}
	local function CreateScope(parent)
		--[[
		local scope = {}
		scope.Parent = parent
		scope.LocalList = {}
		scope.LocalMap = {}

		function scope:ObfuscateVariables()
			for _, var in pairs(scope.LocalList) do
				local id = ""
				repeat
					local chars = "QWERTYUIOPASDFGHJKLZXCVBNMqwertyuioplkjhgfdsazxcvbnm_"
					local chars2 = "QWERTYUIOPASDFGHJKLZXCVBNMqwertyuioplkjhgfdsazxcvbnm_1234567890"
					local n = math.random(1, #chars)
					id = id .. chars:sub(n, n)
					for i = 1, math.random(0,20) do
						local n = math.random(1, #chars2)
						id = id .. chars2:sub(n, n)
					end
				until not GlobalVarGetMap[id] and not parent:GetLocal(id) and not scope.LocalMap[id]
				var.Name = id
				scope.LocalMap[id] = var
			end
		end
		
		scope.RenameVars = scope.ObfuscateVariables

		-- Renames a variable from this scope and down.
		-- Does not rename global variables.
		function scope:RenameVariable(old, newName)
			if type(old) == "table" then -- its (theoretically) an AstNode variable
				old = old.Name
			end
			for _, var in pairs(scope.LocalList) do
				if var.Name == old then
					var.Name = newName
					scope.LocalMap[newName] = var
				end
			end
		end

		function scope:GetLocal(name)
			--first, try to get my variable
			local my = scope.LocalMap[name]
			if my then return my end

			--next, try parent
			if scope.Parent then
				local par = scope.Parent:GetLocal(name)
				if par then return par end
			end

			return nil
		end

		function scope:CreateLocal(name)
			--create my own var
			local my = {}
			my.Scope = scope
			my.Name = name
			my.CanRename = true
			--
			scope.LocalList[#scope.LocalList+1] = my
			scope.LocalMap[name] = my
			--
			return my
		end]]
		local scope = Scope:new(parent)
		scope.RenameVars = scope.ObfuscateLocals
		scope.ObfuscateVariables = scope.ObfuscateLocals
		scope.Print = function() return "<Scope>" end
		return scope
	end

	local ParseExpr
	local ParseStatementList
	local ParseSimpleExpr, 
			ParseSubExpr,
			ParsePrimaryExpr,
			ParseSuffixedExpr
            
    local function ParseObjLuaFunctionBody(scope, tokenList)
        assert(scope)
		local funcScope = CreateScope(scope)
		--[[if not tok:ConsumeSymbol('(', tokenList) then
			return false, GenerateError("`(` expected.")
		end

		--arg list
		local argList = {}
		local isVarArg = false
		while not tok:ConsumeSymbol(')', tokenList) do
			if tok:Is('Ident') then
				local arg = funcScope:CreateLocal(tok:Get(tokenList).Data)
				argList[#argList+1] = arg
				if not tok:ConsumeSymbol(',', tokenList) then
					if tok:ConsumeSymbol(')', tokenList) then
						break
					else
						return false, GenerateError("`)` expected.")
					end
				end
			elseif tok:ConsumeSymbol('...', tokenList) then
				isVarArg = true
				if not tok:ConsumeSymbol(')', tokenList) then
					return false, GenerateError("`...` must be the last argument of a function.")
				end
				break
			else
				return false, GenerateError("Argument name or `...` expected")
			end
		end]]

		--body
        
		local st, body = ParseStatementList(funcScope)
        
		if not st then return false, body end
        
		--end
		if not tok:Peek().Data == 'end' then
			return false, GenerateError("`end` expected after function body")
        else
            tok:Get(tokenList)
		end
		local nodeFunc = {}
		nodeFunc.AstType   = 'ObjLuaFunction'
		nodeFunc.Scope     = funcScope
		--nodeFunc.Arguments = argList
		nodeFunc.Body      = body
		--nodeFunc.VarArg    = isVarArg
		nodeFunc.Tokens    = tokenList
		--
		return true, nodeFunc
	end

	local function ParseFunctionArgsAndBody(scope, tokenList)
		local funcScope = CreateScope(scope)
		if not tok:ConsumeSymbol('(', tokenList) then
			return false, GenerateError("`(` expected.")
		end

		--arg list
		local argList = {}
		local isVarArg = false
		while not tok:ConsumeSymbol(')', tokenList) do
			if tok:Is('Ident') then
				local arg = funcScope:CreateLocal(tok:Get(tokenList).Data)
				argList[#argList+1] = arg
				if not tok:ConsumeSymbol(',', tokenList) then
					if tok:ConsumeSymbol(')', tokenList) then
						break
					else
						return false, GenerateError("`)` expected.")
					end
				end
			elseif tok:ConsumeSymbol('...', tokenList) then
				isVarArg = true
				if not tok:ConsumeSymbol(')', tokenList) then
					return false, GenerateError("`...` must be the last argument of a function.")
				end
				break
			else
				return false, GenerateError("Argument name or `...` expected")
			end
		end

		--body
		local st, body = ParseStatementList(funcScope)
		if not st then return false, body end

		--end
		if not tok:ConsumeKeyword('end', tokenList) then
			return false, GenerateError("`end` expected after function body")
		end
		local nodeFunc = {}
		nodeFunc.AstType   = 'Function'
		nodeFunc.Scope     = funcScope
		nodeFunc.Arguments = argList
		nodeFunc.Body      = body
		nodeFunc.VarArg    = isVarArg
		nodeFunc.Tokens    = tokenList
		--
		return true, nodeFunc
	end


	function ParsePrimaryExpr(scope)
		local tokenList = {}

		if tok:ConsumeSymbol('(', tokenList) then
			local st, ex = ParseExpr(scope)
			if not st then return false, ex end
			if not tok:ConsumeSymbol(')', tokenList) then
				return false, GenerateError("`)` Expected.")
			end
			if false then
				--save the information about parenthesized expressions somewhere
				ex.ParenCount = (ex.ParenCount or 0) + 1
				return true, ex
			else
				local parensExp = {}
				parensExp.AstType   = 'Parentheses'
				parensExp.Inner     = ex
				parensExp.Tokens    = tokenList
				return true, parensExp
			end

		elseif tok:Is('Ident') then
			local id = tok:Get(tokenList)
			local var = scope:GetLocal(id.Data)
			if not var then
				var = scope:GetGlobal(id.Data)
				if not var then
					var = scope:CreateGlobal(id.Data)
				else
					var.References = var.References + 1
				end
			else
				var.References = var.References + 1
			end
			--
			local nodePrimExp = {}
			nodePrimExp.AstType   = 'VarExpr'
			nodePrimExp.Name      = id.Data
			nodePrimExp.Variable  = var
			nodePrimExp.Tokens    = tokenList
			--
			return true, nodePrimExp
		else
			return false, GenerateError("primary expression expected")
		end
	end

	function ParseSuffixedExpr(scope, onlyDotColon)
		--base primary expression
        
		local st, prim = ParsePrimaryExpr(scope)
        if not st then return false, prim end
		--
		while true do
			local tokenList = {}

			if tok:IsSymbol('.') or tok:IsSymbol(':') then
				local symb = tok:Get(tokenList).Data
				if not tok:Is('Ident') then
					return false, GenerateError("<Ident> expected.")
				end
				local id = tok:Get(tokenList)
				local nodeIndex = {}
				nodeIndex.AstType  = 'MemberExpr'
				nodeIndex.Base     = prim
				nodeIndex.Indexer  = symb
				nodeIndex.Ident    = id
				nodeIndex.Tokens   = tokenList
				--
				prim = nodeIndex

			elseif not onlyDotColon and tok:ConsumeSymbol('[', tokenList) then
				local st, ex = ParseExpr(scope)
				if not st then return false, ex end
				if not tok:ConsumeSymbol(']', tokenList) then
                    if tok:Is'Ident' then -- then it could in fact be a [a b] function call not an index
                        local object = ex
                        if not _ then return _, object end
                        local arguments = { }
                        while not tok:ConsumeSymbol(']', tokenList) do
                            local name, arg, _
                            if tok:Is'Ident' then
                                name = tok:Get(tokenList).Data
                            else
                                return false, GenerateError("Ident expected")
                            end
                            local arg_ = nil
                            if tok:ConsumeSymbol(':', tokenList) then
                                arg_ = { }
                                repeat
                                    _, arg = ParseExpr(scope)
                                    if not _ then return _, arg end
                                    table.insert(arg_, arg)
                                until not tok:ConsumeSymbol','
                            end
                            --[[if arg and #arg_ == 1 then
                                arg_ = arg_[1]
                            end]]
                            table.insert(arguments, { Name = name, Argument = arg_ })
                        end
                        local node = { }
                        node.AstType = 'ObjLuaExpr'
                        node.Object = object
                        node.Arguments = arguments
                        node.Tokens = tokenList
                        return true, node
                    end
                    
					return false, GenerateError("`]` expected.")
				end
				local nodeIndex = {}
				nodeIndex.AstType  = 'IndexExpr'
				nodeIndex.Base     = prim
				nodeIndex.Index    = ex
				nodeIndex.Tokens   = tokenList
				--
				prim = nodeIndex

			elseif not onlyDotColon and tok:ConsumeSymbol('(', tokenList) then
				local args = {}
				while not tok:ConsumeSymbol(')', tokenList) do
					local st, ex = ParseExpr(scope)
					if not st then return false, ex end
					args[#args+1] = ex
					if not tok:ConsumeSymbol(',', tokenList) then
						if tok:ConsumeSymbol(')', tokenList) then
							break
						else
							return false, GenerateError("`)` Expected.")
						end
					end
				end
				local nodeCall = {}
				nodeCall.AstType   = 'CallExpr'
				nodeCall.Base      = prim
				nodeCall.Arguments = args
				nodeCall.Tokens    = tokenList
				--
				prim = nodeCall

			elseif not onlyDotColon and tok:Is('String') then
				--string call
				local nodeCall = {}
				nodeCall.AstType    = 'StringCallExpr'
				nodeCall.Base       = prim
				nodeCall.Arguments  = { tok:Get(tokenList) }
				nodeCall.Tokens     = tokenList
				--
				prim = nodeCall

			elseif not onlyDotColon and tok:IsSymbol('{') then
				--table call
				local st, ex = ParseSimpleExpr(scope)
				-- FIX: ParseExpr(scope) parses the table AND and any following binary expressions.
				-- We just want the table
				if not st then return false, ex end
				local nodeCall = {}
				nodeCall.AstType   = 'TableCallExpr'
				nodeCall.Base      = prim
				nodeCall.Arguments = { ex }
				nodeCall.Tokens    = tokenList
				--
				prim = nodeCall

			else
				break
			end
		end
		return true, prim
	end


	function ParseSimpleExpr(scope)
		local tokenList = {}

		if tok:Is('Number') then
			local nodeNum = {}
			nodeNum.AstType = 'NumberExpr'
			nodeNum.Value   = tok:Get(tokenList)
			nodeNum.Tokens  = tokenList
			return true, nodeNum
            
        elseif tok:ConsumeSymbol('[', tokenList) then
            assert(scope)
            local _, object = ParseExpr(scope)
            if not _ then return _, object end
            local arguments = { }
            while not tok:ConsumeSymbol(']', tokenList) do
                local name, arg, _
                if tok:Is'Ident' then
                    name = tok:Get(tokenList).Data
                else
                    return false, GenerateError("Ident expected")
                end
                local arg_ = nil
                if tok:ConsumeSymbol(':', tokenList) then
                    arg_ = { }
                    repeat
                        _, arg = ParseExpr(scope)
                        table.insert(arg_, arg)
                        --print(arg)
                        --print(arg_[#arg_])
                        if not _ then return _, arg end
                    until not tok:ConsumeSymbol','
                end
                --[[if arg and #arg_ == 1 then
                    arg_ = arg_[1]
                end]]
                table.insert(arguments, { Name = name, Argument = arg_ })
            end
            local node = { }
            node.AstType = 'ObjLuaExpr'
            node.Object = object
            node.Arguments = arguments
            node.Tokens = tokenList
            
            return true, node
            
		elseif tok:Is('String') then
			local nodeStr = {}
			nodeStr.AstType = 'StringExpr'
			nodeStr.Value   = tok:Get(tokenList)
			nodeStr.Tokens  = tokenList
			return true, nodeStr

		elseif tok:ConsumeKeyword('nil', tokenList) then
			local nodeNil = {}
			nodeNil.AstType = 'NilExpr'
			nodeNil.Tokens  = tokenList
			return true, nodeNil

		elseif tok:IsKeyword('false') or tok:IsKeyword('true') then
			local nodeBoolean = {}
			nodeBoolean.AstType = 'BooleanExpr'
			nodeBoolean.Value   = (tok:Get(tokenList).Data == 'true')
			nodeBoolean.Tokens  = tokenList
			return true, nodeBoolean

		elseif tok:ConsumeSymbol('...', tokenList) then
			local nodeDots = {}
			nodeDots.AstType  = 'DotsExpr'
			nodeDots.Tokens   = tokenList
			return true, nodeDots

		elseif tok:ConsumeSymbol('{', tokenList) then
			local v = {}
			v.AstType = 'ConstructorExpr'
			v.EntryList = {}
			--
			while true do
				if tok:IsSymbol('[', tokenList) then
					--key
					tok:Get(tokenList)
					local st, key = ParseExpr(scope)
					if not st then
						return false, GenerateError("Key Expression Expected")
					end
					if not tok:ConsumeSymbol(']', tokenList) then
						return false, GenerateError("`]` Expected")
					end
					if not tok:ConsumeSymbol('=', tokenList) then
						return false, GenerateError("`=` Expected")
					end
					local st, value = ParseExpr(scope)
					if not st then
						return false, GenerateError("Value Expression Expected")
					end
					v.EntryList[#v.EntryList+1] = {
						Type  = 'Key';
						Key   = key;
						Value = value;
					}

				elseif tok:Is('Ident') then
					--value or key
					local lookahead = tok:Peek(1)
					if lookahead.Type == 'Symbol' and lookahead.Data == '=' then
						--we are a key
						local key = tok:Get(tokenList)
						if not tok:ConsumeSymbol('=', tokenList) then
							return false, GenerateError("`=` Expected")
						end
						local st, value = ParseExpr(scope)
						if not st then
							return false, GenerateError("Value Expression Expected")
						end
						v.EntryList[#v.EntryList+1] = {
							Type  = 'KeyString';
							Key   = key.Data;
							Value = value;
						}

					else
						--we are a value
						local st, value = ParseExpr(scope)
						if not st then
							return false, GenerateError("Value Exected")
						end
						v.EntryList[#v.EntryList+1] = {
							Type = 'Value';
							Value = value;
						}

					end
				elseif tok:ConsumeSymbol('}', tokenList) then
					break

				else
					--value
					local st, value = ParseExpr(scope)
					v.EntryList[#v.EntryList+1] = {
						Type = 'Value';
						Value = value;
					}
					if not st then
						return false, GenerateError("Value Expected")
					end
				end

				if tok:ConsumeSymbol(';', tokenList) or tok:ConsumeSymbol(',', tokenList) then
					--all is good
				elseif tok:ConsumeSymbol('}', tokenList) then
					break
				else
					return false, GenerateError("`}` or table entry Expected")
				end
			end
			v.Tokens  = tokenList
			return true, v

		elseif tok:ConsumeKeyword('function', tokenList) then
			local st, func = ParseFunctionArgsAndBody(scope, tokenList)
			if not st then return false, func end
			--
			func.IsLocal = true
			return true, func

		else
			return ParseSuffixedExpr(scope)
		end
	end


	local unops = lookupify{'-', 'not', '#'}
	local unopprio = 8
	local priority = {
		['+'] = {6,6};
		['-'] = {6,6};
		['%'] = {7,7};
		['/'] = {7,7};
		['*'] = {7,7};
		['^'] = {10,9};
		['..'] = {5,4};
		['=='] = {3,3};
		['<'] = {3,3};
		['<='] = {3,3};
		['~='] = {3,3};
		['>'] = {3,3};
		['>='] = {3,3};
		['and'] = {2,2};
		['or'] = {1,1};
	}
	function ParseSubExpr(scope, level)
		--base item, possibly with unop prefix
		local st, exp
		if unops[tok:Peek().Data] then
			local tokenList = {}
			local op = tok:Get(tokenList).Data
			st, exp = ParseSubExpr(scope, unopprio)
			if not st then return false, exp end
			local nodeEx = {}
			nodeEx.AstType = 'UnopExpr'
			nodeEx.Rhs     = exp
			nodeEx.Op      = op
			nodeEx.OperatorPrecedence = unopprio
			nodeEx.Tokens  = tokenList
			exp = nodeEx
		else
			st, exp = ParseSimpleExpr(scope)
			if not st then return false, exp end
		end

		--next items in chain
		while true do
			local prio = priority[tok:Peek().Data]
			if prio and prio[1] > level then
				local tokenList = {}
				local op = tok:Get(tokenList).Data
				local st, rhs = ParseSubExpr(scope, prio[2])
				if not st then return false, rhs end
				local nodeEx = {}
				nodeEx.AstType = 'BinopExpr'
				nodeEx.Lhs     = exp
				nodeEx.Op      = op
				nodeEx.OperatorPrecedence = prio[1]
				nodeEx.Rhs     = rhs
				nodeEx.Tokens  = tokenList
				--
				exp = nodeEx
			else
				break
			end
		end

		return true, exp
	end


	ParseExpr = function(scope)
		return ParseSubExpr(scope, 0)
	end
    
    
    local function ParseObjLuaImplementation(class, tokenList, scope)
        local stmts = { }
        --print(tok:Peek().Type, tok:Peek().Data)
        while --not tok:Is'ObjLuaSymbol' and 
        tok:Peek().Data ~= '@end' do
            if tok:ConsumeSymbol('-', tokenList) then
                -- member 
                local returnType = 'nil'
                if tok:Peek().Data == '(' then
                    tok:Get(tokenList)
                    if tok:Is'Ident' then
                        -- type name
                        returnType = tok:Get(tokenList).Data
                    elseif tok:ConsumeKeyword('function', tokenList) then
                        -- void function ?
                    
                    else
                        return false, GenerateError("Ident or 'function' expected")
                    end
                    if not tok:ConsumeSymbol(')', tokenList) then
                        return false, GenerateError("')' expected")
                    end
                end
                
                if tok:Is'Ident' then
                    local argList = { } -- each child: { Name=, Argument= }
                    while not tok:ConsumeKeyword('do', tokenList) do
                        local name = tok:Get(tokenList).Data
                        local ptype, argname = nil, ""
                        if tok:Peek().Data == ':' then
                            tok:Get(tokenList)
                            if tok:ConsumeSymbol('(', tokenList) then
                                if tok:Is'Ident' or tok:Is'Keyword' then
                                    if tok:ConsumeKeyword('function', tokenList) then
                                        --ptype = 'function'
                                    elseif tok:Is'Ident' then
                                        ptype = tok:Get(tokenList).Data
                                    else
                                        return false, GenerateError("Type name or 'function' expected")
                                    end
                                else
                                    return false, GenerateError("Type name or 'function' expected")
                                end
                                if not tok:ConsumeSymbol(')', tokenList) then
                                    return false, GenerateError("')' expected")
                                end
                            end
                            if tok:Is'Ident' or (tok:Is'Symbol' and tok:Peek().Data == '...') then
                            else
                                return false, GenerateError("Ident expected")
                            end
                            argname = tok:Get(tokenList).Data
                        end
                        table.insert(argList, { Name=name, ArgumentType=ptype, ArgumentName = argname })
                    end
                    local _, body = ParseObjLuaFunctionBody(scope)
                    if not _ then return _, body end
                    
                    local node = { AstType = 'ObjLuaDeclarationStatement' }
                    node.ReturnType = returnType
                    node.Arguments = argList
                    node.DefinitionType = 'Member'
                    node.Body = body 
                    node.Class = class
                    --node.Tokens = tokenList
                    table.insert(stmts, node)
                else
                    return false, GenerateError("Expected Ident")
                end
            elseif tok:ConsumeSymbol('+', tokenList) then
                -- member 
                local returnType = 'nil'
                if tok:Peek().Data == '(' then
                    tok:Get(tokenList)
                    if tok:Is'Ident' then
                        -- type name
                        returnType = tok:Get(tokenList).Data
                    elseif tok:ConsumeKeyword('function', tokenList) then
                        -- void function ?
                    
                    else
                        return false, GenerateError("Ident or 'function' expected")
                    end
                    if not tok:ConsumeSymbol(')', tokenList) then
                        return false, GenerateError("')' expected")
                    end
                end
                
                if tok:Is'Ident' then
                    local argList = { } -- each child: { Name=, Argument= }
                    while not tok:ConsumeKeyword('do', tokenList) do
                        local name = tok:Get(tokenList).Data
                        local ptype, argname = nil, ""
                        if tok:Peek().Data == ':' then
                            tok:Get(tokenList)
                            if tok:ConsumeSymbol('(', tokenList) then
                                if tok:Is'Ident' or tok:Is'Keyword' then
                                    if tok:ConsumeKeyword('function', tokenList) then
                                    elseif tok:Is'Ident' then
                                        ptype = tok:Get(tokenList).Data
                                    else
                                        return false, GenerateError("Type name or 'function' expected")
                                    end
                                else
                                    return false, GenerateError("Type name or 'function' expected")
                                end
                                if not tok:ConsumeSymbol(')', tokenList) then
                                    return false, GenerateError("')' expected")
                                end
                            end
                            if tok:Is'Ident' or (tok:Is'Symbol' and tok:Peek().Data == '...') then
                            else
                                return false, GenerateError("Ident expected")
                            end
                            argname = tok:Get(tokenList).Data
                            
                        end
                        table.insert(argList, { Name=name, ArgumentType=ptype, ArgumentName = argname })
                    end
                    local _, body = ParseObjLuaFunctionBody(scope)
                    if not _ then return _, body end
                    
                    local node = { AstType = 'ObjLuaDeclarationStatement' }
                    node.ReturnType = returnType
                    node.Arguments = argList
                    node.DefinitionType = 'Static'
                    node.Body = body 
                    node.Class = class
                    --node.Tokens = tokenList
                    table.insert(stmts, node)
                else
                    return false, GenerateError("Expected Ident")
                end
            elseif tok:ConsumeObjLuaSymbol('@static', tokenList) then
                local _, node1, node2, node3
                if tok:Is'Symbol' and tok:Peek().Data == '(' then
                    tok:Get(tokenList)
                    if tok:Is'Ident' then
                        -- type name
                        node1 = tok:Get(tokenList).Data
                    elseif tok:ConsumeKeyword('function', tokenList) then
                        -- void function ?
                    else
                        return false, GenerateError("Ident or 'function' expected")
                    end
                    if not tok:ConsumeSymbol(')', tokenList) then
                        return false, GenerateError("')' expected")
                    end
                end
                
                if not tok:Is'Ident' then return false, GenerateError("Expected Ident") end
                node2 = tok:Get().Data
                
                if tok:ConsumeSymbol'=' then
                    _, node3 = ParseExpr(scope)
                    if not _ then return _, node3 end
                end
                
                local node = { AstType = 'ObjLuaStatic' }
                node.Type = node1
                node.Var = node2
                node.Expr = node3
                node.Tokens = tokenList
                node.Class = class
                table.insert(stmts, node)
                
                if tok:Peek().Data == ';' then tok:Get(tokenList) end
            elseif tok:ConsumeObjLuaSymbol('@alias', tokenList) then
                if not tok:ConsumeSymbol('(', tokenList) then
                    return false, GenerateError("'(' expected")
                end
                
                local name1 = tok:Get(tokenList)
                if not tok:ConsumeSymbol(',', tokenList) then
                    return false, GenerateError("',' expected")
                end
                
                local name2 = tok:Get(tokenlist)
                
                if not tok:ConsumeSymbol(')', tokenList) then
                    return false, GenerateError("')' expected")
                end
                
                local node = { AstType = 'ObjLuaAlias',
                    Class = class,
                    Alias = name2.Constant or name2.Data,
                    AliasedMethod = name1.Constant or name1.Data,
                }
                table.insert(stmts, node)
            else
                return false, GenerateError("Expected '-', '+', '@static', '@alias', or '@end'")
            end
        end
        if not tok:ConsumeObjLuaSymbol('@end', tokenList) then
            return false, GenerateError("expected '@end'")
        end
        return true, stmts
    end


	local function ParseStatement(scope)
        --print(tok:Peek().Data)
		local stat = nil
		local tokenList = {}
		if tok:ConsumeKeyword('if', tokenList) then
			--setup
			local nodeIfStat = {}
			nodeIfStat.AstType = 'IfStatement'
			nodeIfStat.Clauses = {}

			--clauses
			repeat
				local st, nodeCond = ParseExpr(scope)
				if not st then return false, nodeCond end
				if not tok:ConsumeKeyword('then', tokenList) then
					return false, GenerateError("`then` expected.")
				end
				local st, nodeBody = ParseStatementList(scope)
				if not st then return false, nodeBody end
				nodeIfStat.Clauses[#nodeIfStat.Clauses+1] = {
					Condition = nodeCond;
					Body = nodeBody;
				}
			until not tok:ConsumeKeyword('elseif', tokenList)

			--else clause
			if tok:ConsumeKeyword('else', tokenList) then
				local st, nodeBody = ParseStatementList(scope)
				if not st then return false, nodeBody end
				nodeIfStat.Clauses[#nodeIfStat.Clauses+1] = {
					Body = nodeBody;
				}
			end

			--end
			if not tok:ConsumeKeyword('end', tokenList) then
				return false, GenerateError("`end` expected.")
			end

			nodeIfStat.Tokens = tokenList
			stat = nodeIfStat

		elseif tok:ConsumeKeyword('while', tokenList) then
			--setup
			local nodeWhileStat = {}
			nodeWhileStat.AstType = 'WhileStatement'

			--condition
			local st, nodeCond = ParseExpr(scope)
			if not st then return false, nodeCond end

			--do
			if not tok:ConsumeKeyword('do', tokenList) then
				return false, GenerateError("`do` expected.")
			end

			--body
			local st, nodeBody = ParseStatementList(scope)
			if not st then return false, nodeBody end

			--end
			if not tok:ConsumeKeyword('end', tokenList) then
				return false, GenerateError("`end` expected.")
			end

			--return
			nodeWhileStat.Condition = nodeCond
			nodeWhileStat.Body      = nodeBody
			nodeWhileStat.Tokens    = tokenList
			stat = nodeWhileStat

		elseif tok:ConsumeKeyword('do', tokenList) then
			--do block
			local st, nodeBlock = ParseStatementList(scope)
			if not st then return false, nodeBlock end
			if not tok:ConsumeKeyword('end', tokenList) then
				return false, GenerateError("`end` expected.")
			end

			local nodeDoStat = {}
			nodeDoStat.AstType = 'DoStatement'
			nodeDoStat.Body    = nodeBlock
			nodeDoStat.Tokens  = tokenList
			stat = nodeDoStat

		elseif tok:ConsumeKeyword('for', tokenList) then
			--for block
			if not tok:Is('Ident') then
				return false, GenerateError("<ident> expected.")
			end
			local baseVarName = tok:Get(tokenList)
			if tok:ConsumeSymbol('=', tokenList) then
				--numeric for
				local forScope = CreateScope(scope)
				local forVar = forScope:CreateLocal(baseVarName.Data)
				--
				local st, startEx = ParseExpr(scope)
				if not st then return false, startEx end
				if not tok:ConsumeSymbol(',', tokenList) then
					return false, GenerateError("`,` Expected")
				end
				local st, endEx = ParseExpr(scope)
				if not st then return false, endEx end
				local st, stepEx;
				if tok:ConsumeSymbol(',', tokenList) then
					st, stepEx = ParseExpr(scope)
					if not st then return false, stepEx end
				end
				if not tok:ConsumeKeyword('do', tokenList) then
					return false, GenerateError("`do` expected")
				end
				--
				local st, body = ParseStatementList(forScope)
				if not st then return false, body end
				if not tok:ConsumeKeyword('end', tokenList) then
					return false, GenerateError("`end` expected")
				end
				--
				local nodeFor = {}
				nodeFor.AstType  = 'NumericForStatement'
				nodeFor.Scope    = forScope
				nodeFor.Variable = forVar
				nodeFor.Start    = startEx
				nodeFor.End      = endEx
				nodeFor.Step     = stepEx
				nodeFor.Body     = body
				nodeFor.Tokens   = tokenList
				stat = nodeFor
			else
				--generic for
				local forScope = CreateScope(scope)
				--
				local varList = { forScope:CreateLocal(baseVarName.Data) }
				while tok:ConsumeSymbol(',', tokenList) do
					if not tok:Is('Ident') then
						return false, GenerateError("for variable expected.")
					end
					varList[#varList+1] = forScope:CreateLocal(tok:Get(tokenList).Data)
				end
				if not tok:ConsumeKeyword('in', tokenList) then
					return false, GenerateError("`in` expected.")
				end
				local generators = {}
				local st, firstGenerator = ParseExpr(scope)
				if not st then return false, firstGenerator end
				generators[#generators+1] = firstGenerator
				while tok:ConsumeSymbol(',', tokenList) do
					local st, gen = ParseExpr(scope)
					if not st then return false, gen end
					generators[#generators+1] = gen
				end
				if not tok:ConsumeKeyword('do', tokenList) then
					return false, GenerateError("`do` expected.")
				end
				local st, body = ParseStatementList(forScope)
				if not st then return false, body end
				if not tok:ConsumeKeyword('end', tokenList) then
					return false, GenerateError("`end` expected.")
				end
				--
				local nodeFor = {}
				nodeFor.AstType      = 'GenericForStatement'
				nodeFor.Scope        = forScope
				nodeFor.VariableList = varList
				nodeFor.Generators   = generators
				nodeFor.Body         = body
				nodeFor.Tokens       = tokenList
				stat = nodeFor
			end

		elseif tok:ConsumeKeyword('repeat', tokenList) then
			local st, body = ParseStatementList(scope)
			if not st then return false, body end
			--
			if not tok:ConsumeKeyword('until', tokenList) then
				return false, GenerateError("`until` expected.")
			end
			-- FIX: Used to parse in parent scope
			-- Now parses in repeat scope
			local st, cond = ParseExpr(body.Scope)
			if not st then return false, cond end
			--
			local nodeRepeat = {}
			nodeRepeat.AstType   = 'RepeatStatement'
			nodeRepeat.Condition = cond
			nodeRepeat.Body      = body
			nodeRepeat.Tokens    = tokenList
			stat = nodeRepeat

		elseif tok:ConsumeKeyword('function', tokenList) then
			if not tok:Is('Ident') then
				return false, GenerateError("Function name expected")
			end
			local st, name = ParseSuffixedExpr(scope, true) --true => only dots and colons
			if not st then return false, name end
			--
			local st, func = ParseFunctionArgsAndBody(scope, tokenList)
			if not st then return false, func end
			--
			func.IsLocal = false
			func.Name    = name
			stat = func

		elseif tok:ConsumeKeyword('local', tokenList) then
			if tok:Is('Ident') then
				local varList = { tok:Get(tokenList).Data }
				while tok:ConsumeSymbol(',', tokenList) do
					if not tok:Is('Ident') then
						return false, GenerateError("local var name expected")
					end
					varList[#varList+1] = tok:Get(tokenList).Data
				end

				local initList = {}
				if tok:ConsumeSymbol('=', tokenList) then
					repeat
						local st, ex = ParseExpr(scope)
						if not st then return false, ex end
						initList[#initList+1] = ex
					until not tok:ConsumeSymbol(',', tokenList)
				end

				--now patch var list
				--we can't do this before getting the init list, because the init list does not
				--have the locals themselves in scope.
				for i, v in pairs(varList) do
					varList[i] = scope:CreateLocal(v)
				end

				local nodeLocal = {}
				nodeLocal.AstType   = 'LocalStatement'
				nodeLocal.LocalList = varList
				nodeLocal.InitList  = initList
				nodeLocal.Tokens    = tokenList
				--
				stat = nodeLocal

			elseif tok:ConsumeKeyword('function', tokenList) then
				if not tok:Is('Ident') then
					return false, GenerateError("Function name expected")
				end
				local name = tok:Get(tokenList).Data
				local localVar = scope:CreateLocal(name)
				--
				local st, func = ParseFunctionArgsAndBody(scope, tokenList)
				if not st then return false, func end
				--
				func.Name         = localVar
				func.IsLocal      = true
				stat = func

			else
				return false, GenerateError("local var or function def expected")
			end

		elseif tok:ConsumeSymbol('::', tokenList) then
			if not tok:Is('Ident') then
				return false, GenerateError('Label name expected')
			end
			local label = tok:Get(tokenList).Data
			if not tok:ConsumeSymbol('::', tokenList) then
				return false, GenerateError("`::` expected")
			end
			local nodeLabel = {}
			nodeLabel.AstType = 'LabelStatement'
			nodeLabel.Label   = label
			nodeLabel.Tokens  = tokenList
			stat = nodeLabel

		elseif tok:ConsumeKeyword('return', tokenList) then
			local exList = {}
			if not tok:IsKeyword('end') then
				local st, firstEx = ParseExpr(scope)
				if st then
					exList[1] = firstEx
					while tok:ConsumeSymbol(',', tokenList) do
						local st, ex = ParseExpr(scope)
						if not st then return false, ex end
						exList[#exList+1] = ex
					end
				end
			end

			local nodeReturn = {}
			nodeReturn.AstType   = 'ReturnStatement'
			nodeReturn.Arguments = exList
			nodeReturn.Tokens    = tokenList
			stat = nodeReturn

		elseif tok:ConsumeKeyword('break', tokenList) then
			local nodeBreak = {}
			nodeBreak.AstType = 'BreakStatement'
			nodeBreak.Tokens  = tokenList
			stat = nodeBreak

		elseif tok:ConsumeKeyword('goto', tokenList) then
			if not tok:Is('Ident') then
				return false, GenerateError("Label expected")
			end
			local label = tok:Get(tokenList).Data
			local nodeGoto = {}
			nodeGoto.AstType = 'GotoStatement'
			nodeGoto.Label   = label
			nodeGoto.Tokens  = tokenList
			stat = nodeGoto

        elseif tok:ConsumeObjLuaSymbol('@interface', tokenList) then
            if not tok:Is'Ident' then
                return false, GenerateError("Label expected")
			end
            local name = tok:Get(tokenList).Data
            local class, protocols
            
            if tok:ConsumeSymbol('(', tokenList) then
                tok:Get(tokenList)
                if not tok:ConsumeSymbol(')', tokenList) then return false, GenerateError("Expected ')'") end
            end
            
            if tok:Peek().Type == 'Symbol' and tok:Peek().Data == ':' then
                -- @interface <name> : a<implements>, c<d, e>
                if not tok:Is'Ident' then
                    return false, GenerateError("Ident expected")
                end
                
                class = tok:Get(tokenList).Data
                protocols = { }
                if tok:Is'Symbol' and tok:Peek().Data == '<' then
                    repeat
                        if tok:Is'Ident' then
                            table.insert(protocols, tok:Get(tokenList).Data)
                        else
                            return false, GenerateError("Ident expected")
                        end
                    until tok:Peek().Data == '>' or tok:Peek().Data ~= ','
                    if tok:Peek().Data == '>' then 
                        tok:Get(tokenList)
                    else
                        return false, GenerateError("Expected '>'")
                    end
                end
            end
        
            local node = { AstType = 'InterfaceDeclarationStatement' }
            node.Name = name
            node.Inherits = class
            node.Protocols = protocols
            local x, y = ParseObjLuaInterface(node, tokenList)
            if not x then return x, y end
            node.Body = y
            node.Tokens = tokenList
            stat = node
            
            
        elseif tok:ConsumeObjLuaSymbol('@implementation', tokenList) then
            if not tok:Is'Ident' then
                return false, GenerateError("Label expected")
			end
            local name = tok:Get(tokenList).Data
            local class, protocols
            
            if tok:ConsumeSymbol('(', tokenList) then
                tok:Get(tokenList)
                if not tok:ConsumeSymbol(')', tokenList) then return false, GenerateError("Expected ')'") end
            end
            
            if tok:Peek().Type == 'Symbol' and tok:Peek().Data == ':' then
                tok:Get(tokenList)
                if not tok:Is'Ident' then
                    return false, GenerateError("Ident expected")
                end
                
                class = tok:Get(tokenList).Data
                protocols = { }
                if tok:Is'Symbol' and tok:Peek().Data == '<' then
                    repeat
                        if tok:Is'Ident' then
                            table.insert(protocols, tok:Get(tokenList).Data)
                        else
                            return false, GenerateError("Ident expected")
                        end
                    until tok:Peek().Data == '>' or tok:Peek().Data ~= ','
                    if tok:Peek().Data == '>' then 
                        tok:Get(tokenList)
                    else
                        return false, GenerateError("Expected '>'")
                    end
                end
                
            end
            
            local node = { AstType = 'InterfaceImplementationStatement' }
            node.Name = name
            node.Inherits = class
            node.Protocols = protocols
            local something, ast = ParseObjLuaImplementation(node, tokenList, scope)
            if not something then return something, ast end
            node.Body = ast
            node.Tokens = tokenList
            stat = node
            
        elseif tok:ConsumeObjLuaSymbol("@try", tokenList) then
            local _, stmts = ParseStatementList(scope)
            if not _ then return _, stmts end
            
            local catches = { }
            local catchAll = false
            
            while tok:ConsumeObjLuaSymbol('@catch', tokenList) do
                tok:Save()
                local hadParen = false
                local _, expr, catch, exVar
                if tok:Is'Ident' or (tok:Is'Symbol' and tok:Peek().Data == '(') then
                    hadParen = tok:ConsumeSymbol('(', tokenList)
                    --print('hadParen', hadParen)
                    _, expr = ParseExpr(scope)
                    --print(hadParen, _, expr)
                    
                    if not _ then
                        tok:Restore()
                        _, catch = ParseStatementList(scope)
                    else
                        if expr.AstType == 'VarExpr' then
                            if hadParen and tok:Is'Ident' then
                                exVar = tok:Get(tokenList).Data
                                if not tok:ConsumeSymbol(')', tokenList) then
                                    return false, GenerateError("')' expected")
                                end
                            end
                            tok:Commit()
                            _, catch = ParseStatementList(scope)
                        else
                            expr = nil
                            tok:Restore()
                            _, catch = ParseStatementList(scope)
                        end
                    end
                else
                    tok:Commit()
                    _, catch = ParseStatementList(scope)
                end
                
                if not _ then return _, catch end
                
                if catchAll and expr == nil then
                    return false, GenerateError("A catch all is already defined")
                end
                
                assert(catch)
                if not catchAll then catchAll = expr == nil end
                table.insert(catches, {
                    AstType = 'CatchStatement',
                    Expr = expr,
                    ExceptionVar = exVar,
                    Body = catch
                })
            end
            
            local finally
            
            if tok:ConsumeObjLuaSymbol('@finally', tokenList) then
                _, finally = ParseStatementList(scope)
                if not _ then return _, finally end
            end
            
            if not tok:ConsumeObjLuaSymbol('@end', tokenList) then
                return false, GenerateError("@end expected")
            end
            
            local node = { AstType = 'TryStatement' }
            node.Body = stmts
            node.Tokens = tokenList
            node.Catches = catches
            node.Finally = finally
            
            stat = node
            
        elseif tok:ConsumeObjLuaSymbol("@catch", tokenList) then
            return false, GenerateError("Unexpected '@catch'") -- implemented elsewhere
        elseif tok:ConsumeObjLuaSymbol("@finally", tokenList) then
            return false, GenerateError("Unexpected '@finally'") -- implemented elsewhere
        elseif tok:ConsumeObjLuaSymbol("@end", tokenList) then
            --error("Not yet implemented")
        elseif tok:ConsumeObjLuaSymbol("@throw", tokenList) then
            local _, expr = ParseExpr(scope)
            if not _ then return _, expr end
            local node = { AstType = 'ObjLuaThrowStatement' }
            node.ErrorObject = expr
            node.Tokens = tokenList
            stat = node
            
        elseif tok:ConsumeObjLuaSymbol("@statics", tokenList) then
            error("Not yet implemented")
        elseif tok:ConsumeObjLuaSymbol("@static", tokenList) then
            -- allows private, static members to be defined
            return false, GenerateError("Unexpected '@static'")
            
        --[[elseif tok:ConsumeObjLuaSymbol("@property", tokenList) then
            if not tok:Is'Ident' then
                return false, GenerateError("Label expected")
			end
            local name = tok:Get(tokenList).Data
            
            local node = { AstType = 'ObjLuaPropertyStatement' }
            node.Name = name
            node.Tokens = tokenList
            stat = node]]
            
        elseif tok:Is'Symbol' and tok:Peek().Data == '[' then
            --tok:ConsumeSymbol('[', tokenList) then
            local node = { }
            node.AstType = 'ObjLuaStatement'
            local _, expr = ParseExpr(scope)
            if not _ then
                return _, expr
            end
            
            --print(_, expr)
            node.Expr = expr
            node.Tokens = tokenList
            --return true, node
            stat = node
        
		else
			--statementParseExpr
			local st, suffixed = ParseSuffixedExpr(scope)
			if not st then return false, suffixed end

			--assignment or call?
			if tok:IsSymbol(',') or tok:IsSymbol('=') then
				--check that it was not parenthesized, making it not an lvalue
				if (suffixed.ParenCount or 0) > 0 then
					return false, GenerateError("Can not assign to parenthesized expression, is not an lvalue")
				end

				--more processing needed
				local lhs = { suffixed }
				while tok:ConsumeSymbol(',', tokenList) do
					local st, lhsPart = ParseSuffixedExpr(scope)
					if not st then return false, lhsPart end
					lhs[#lhs+1] = lhsPart
				end

				--equals
				if not tok:ConsumeSymbol('=', tokenList) then
					return false, GenerateError("`=` Expected.")
				end

				--rhs
				local rhs = {}
				local st, firstRhs = ParseExpr(scope)
				if not st then return false, firstRhs end
				rhs[1] = firstRhs
				while tok:ConsumeSymbol(',', tokenList) do
					local st, rhsPart = ParseExpr(scope)
					if not st then return false, rhsPart end
					rhs[#rhs+1] = rhsPart
				end

				--done
				local nodeAssign = {}
				nodeAssign.AstType = 'AssignmentStatement'
				nodeAssign.Lhs     = lhs
				nodeAssign.Rhs     = rhs
				nodeAssign.Tokens  = tokenList
				stat = nodeAssign

			elseif suffixed.AstType == 'CallExpr' or
				   suffixed.AstType == 'TableCallExpr' or
				   suffixed.AstType == 'StringCallExpr'
			then
				--it's a call statement
				local nodeCall = {}
				nodeCall.AstType    = 'CallStatement'
				nodeCall.Expression = suffixed
				nodeCall.Tokens     = tokenList
				stat = nodeCall
			else
				return false, GenerateError("Assignment Statement Expected")
			end
		end

		if tok:IsSymbol(';') then
			stat.Semicolon = tok:Get( stat.Tokens )
		end
		return true, stat
	end


	local statListCloseKeywords = lookupify{'end', 'else', 'elseif', 'until', 
        '@end', '@catch', '@finally'
    }

	ParseStatementList = function(scope)
		local nodeStatlist   = {}
		nodeStatlist.Scope   = CreateScope(scope)
		nodeStatlist.AstType = 'Statlist'
		nodeStatlist.Body    = { }
		nodeStatlist.Tokens  = { }
		--
		--local stats = {}
		--
		while not statListCloseKeywords[tok:Peek().Data] and not tok:IsEof() do
			local st, nodeStatement = ParseStatement(nodeStatlist.Scope)
			if not st then return false, nodeStatement end
			--stats[#stats+1] = nodeStatement
			nodeStatlist.Body[#nodeStatlist.Body + 1] = nodeStatement
		end

		if tok:IsEof() then
			local nodeEof = {}
			nodeEof.AstType = 'Eof'
			nodeEof.Tokens  = { tok:Get() }
			nodeStatlist.Body[#nodeStatlist.Body + 1] = nodeEof
		end

		--
		--nodeStatlist.Body = stats
		return true, nodeStatlist
	end


	local function mainfunc()
		local topScope = CreateScope()
		return ParseStatementList(topScope)
	end

	local st, main = mainfunc()
	--print("Last Token: "..PrintTable(tok:Peek()))
	return st, main
end

--[===[
x, y = Parse[=[
@interface test
- a:(object)text
+ b:blah
@end

@implementation test
-a:text do
    print"test"
    print(text)
end

+b:(string) blahblah do
    return 5
end

@end

local x = [test alloc]
[x init]
[x a]
]=]
print(x, y)
if x then print(require('Util').PrintTable(y)) end
]===]

return { Lex = Lex, Parse = Parse }
