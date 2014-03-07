-- Spews out Lua, compiling ObjLua extensions into Lua


local parser = require'objlua.parser'
local ParseLua = parser.Parse
local util = require'objlua.Util'
local lookupify = util.lookupify

local LowerChars = lookupify{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 
							 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 
							 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'}
local UpperChars = lookupify{'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 
							 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 
							 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'}
local Digits = lookupify{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'}
local Symbols = lookupify{'+', '-', '*', '/', '^', '%', ',', '{', '}', '[', ']', '(', ')', ';', '#'}

local function Format_Lua(ast)
	local formatStatlist, formatExpr;
    local formatStatement 
	local count = 0
  
    local function fmtProcols(prtcls)
        local ret = ""
        for k, v in pairs(prtcls) do
            ret = ret .. "'" .. v .. "'" .. ", "
        end
        return ret:len() > 0 and ", { " .. ret .. "}" or ""
    end
  
	--
	local function joinStatementsSafe(a, b, sep)
	--print(a, b)
		if count > 150 then
			count = 0
			return a.."\n"..b
		end
		sep = sep or ' '
		local aa, bb = a:sub(-1,-1), b:sub(1,1)
		if UpperChars[aa] or LowerChars[aa] or aa == '_' then
			if not (UpperChars[bb] or LowerChars[bb] or bb == '_' or Digits[bb]) then
				--bb is a symbol, can join without sep
				return a..b
			elseif bb == '(' then
				print("==============>>>",aa,bb)
				--prevent ambiguous syntax
				return a..sep..b
			else
				return a..sep..b
			end
		elseif Digits[aa] then
			if bb == '(' then
				--can join statements directly
				return a..b
			elseif Symbols[bb] then
				return a .. b
			else
				return a..sep..b
			end
		elseif aa == '' then
			return a..b
		else
			if bb == '(' then
				--don't want to accidentally call last statement, can't join directly
				return a..sep..b
			else
			--print("asdf", '"'..a..'"', '"'..b..'"')
				return a..b
			end
		end
	end

	formatExpr = function(expr, precedence)
		local precedence = precedence or 0
		local currentPrecedence = 0
		local skipParens = false
		local out = ""
		if expr.AstType == 'VarExpr' then
			if expr.Variable then
				out = out..expr.Variable.Name
			else
				out = out..expr.Name
			end

		elseif expr.AstType == 'NumberExpr' then
			out = out..expr.Value.Data

		elseif expr.AstType == 'StringExpr' then
			out = out..expr.Value.Data

		elseif expr.AstType == 'BooleanExpr' then
			out = out..tostring(expr.Value)

		elseif expr.AstType == 'NilExpr' then
			out = joinStatementsSafe(out, "nil")

		elseif expr.AstType == 'BinopExpr' then
			currentPrecedence = expr.OperatorPrecedence
			out = joinStatementsSafe(out, formatExpr(expr.Lhs, currentPrecedence))
			out = joinStatementsSafe(out, expr.Op)
			out = joinStatementsSafe(out, formatExpr(expr.Rhs))
			if expr.Op == '^' or expr.Op == '..' then
				currentPrecedence = currentPrecedence - 1
			end
			
			if currentPrecedence < precedence then
				skipParens = false
			else
				skipParens = true
			end
			--print(skipParens, precedence, currentPrecedence)
		elseif expr.AstType == 'UnopExpr' then
			out = joinStatementsSafe(out, expr.Op)
			out = joinStatementsSafe(out, formatExpr(expr.Rhs))

		elseif expr.AstType == 'DotsExpr' then
			out = out.."..."

		elseif expr.AstType == 'CallExpr' then
			out = out..formatExpr(expr.Base)
			out = out.."("
			for i = 1, #expr.Arguments do
				out = out..formatExpr(expr.Arguments[i])
				if i ~= #expr.Arguments then
					out = out..","
				end
			end
			out = out..")"

		elseif expr.AstType == 'TableCallExpr' then
			out = out..formatExpr(expr.Base)
			out = out..formatExpr(expr.Arguments[1])

		elseif expr.AstType == 'StringCallExpr' then
			out = out..formatExpr(expr.Base)
			out = out..expr.Arguments[1].Data

		elseif expr.AstType == 'IndexExpr' then
			out = out..formatExpr(expr.Base).."["..formatExpr(expr.Index).."]"

		elseif expr.AstType == 'MemberExpr' then
			out = out..formatExpr(expr.Base)..expr.Indexer..expr.Ident.Data

		elseif expr.AstType == 'Function' then
			--expr.Scope:ObfuscateVariables()
			out = out.."function("
			if #expr.Arguments > 0 then
				for i = 1, #expr.Arguments do
					out = out..expr.Arguments[i].Name
					if i ~= #expr.Arguments then
						out = out..","
					elseif expr.VarArg then
						out = out..",..."
					end
				end
			elseif expr.VarArg then
				out = out.."..."
			end
			out = out..")"
			out = joinStatementsSafe(out, formatStatlist(expr.Body))
			out = joinStatementsSafe(out, "end")

		elseif expr.AstType == 'ConstructorExpr' then
			out = out.."{"
			for i = 1, #expr.EntryList do
				local entry = expr.EntryList[i]
				if entry.Type == 'Key' then
					out = out.."["..formatExpr(entry.Key).."]="..formatExpr(entry.Value)
				elseif entry.Type == 'Value' then
					out = out..formatExpr(entry.Value)
				elseif entry.Type == 'KeyString' then
					out = out..entry.Key.."="..formatExpr(entry.Value)
				end
				if i ~= #expr.EntryList then
					out = out..","
				end
			end
			out = out.."}"
            
        elseif expr.AstType == 'ObjLuaExpr' then
            local function fix(x)
                if x.AstType == 'VarExpr' then
                    return "objlua.getClassOrObject(" .. formatExpr(x) .. ", '" .. formatExpr(x) .. "')"
                else
                    return formatExpr(x)
                end
            end
            
            out = joinStatementsSafe(out, fix(expr.Object))
            out = out .. ":"
            local hasNamedArgPairs = false
            for k, v in pairs(expr.Arguments) do
                --for k, v in pairs(v) do
                    if v.Argument ~= nil and v.Name ~= nil then
                        hasNamedArgPairs = true
                    end
                --end
            end
            
            local first = true
            
            if hasNamedArgPairs == false then
                for k, v in pairs(expr.Arguments) do
                    if first then
                        --print(v.Name)
                        out = out .. v.Name
                        out = out .. "("
                        first = false
                    else
                        --print(v.Name)
                        error'wat'
                    end
                end
            else
                local _ = expr.Arguments[1]
                out = out .. _.Name .. "("
            
                out = out .. "{ "
                for i = 1, #expr.Arguments do
                    local v = expr.Arguments[i]
                    out = out .. '["' .. v.Name .. '"]'
                    out = out .. '='
                    
                    --for k, v in pairs(v.Argument) do print(k,v) end
                    if #v.Argument > 1 then
                        out = out .. "{ __objlua_constructed=true, " 
                        for i = 1, #v.Argument do
                            out = out .. formatExpr(v.Argument[i])
                            if i ~= #v.Argument then
                                out = out .. ", "
                            end
                        end
                        out = out .. " }"
                    elseif v.Argument[1].AstType == 'DotsExpr' then 
                        out = out .. "{ __objlua_dotsexpr=true, {" 
                        out = out .. formatExpr(v.Argument[1])
                        out = out .. "} }"
                    else
                        out = out .. formatExpr(v.Argument[1])
                    end
                    if i ~= #expr.Arguments then
                        out = out .. ", "
                    end
                end
                out = out .. " }"
            end
            out = out .. ")"

		elseif expr.AstType == 'Parentheses' then
			out = out.."("..formatExpr(expr.Inner)..")"
            
        elseif expr.AstType == 'ObjLuaFunction' then
            out = out .. "function()"
            out = joinStatementsSafe(out, formatStatlist(expr.Body))
			out = joinStatementsSafe(out, "end")
        else
            error("Unknown Expr type '" .. (expr.AstType or tostring(expr)) .. "'")
		end
		--print(">>", skipParens, expr.ParenCount, out)
		if not skipParens then
			--print("hehe")
			out = string.rep('(', expr.ParenCount or 0) .. out
			out = out .. string.rep(')', expr.ParenCount or 0)
			--print("", out)
		end
		count = count + #out
		return --[[print(out) or]] out
	end

	formatStatement = function(statement, isTryCatch)
		local out = ''
		if statement.AstType == 'AssignmentStatement' then
			for i = 1, #statement.Lhs do
				out = out..formatExpr(statement.Lhs[i])
				if i ~= #statement.Lhs then
					out = out..","
				end
			end
			if #statement.Rhs > 0 then
				out = out.."="
				for i = 1, #statement.Rhs do
					out = out..formatExpr(statement.Rhs[i])
					if i ~= #statement.Rhs then
						out = out..","
					end
				end
			end

		elseif statement.AstType == 'CallStatement' then
			out = formatExpr(statement.Expression)

		elseif statement.AstType == 'LocalStatement' then
			out = out.."local "
			for i = 1, #statement.LocalList do
				out = out..statement.LocalList[i].Name
				if i ~= #statement.LocalList then
					out = out..","
				end
			end
			if #statement.InitList > 0 then
				out = out.."="
				for i = 1, #statement.InitList do
					out = out..formatExpr(statement.InitList[i])
					if i ~= #statement.InitList then
						out = out..","
					end
				end
			end

		elseif statement.AstType == 'IfStatement' then
			out = joinStatementsSafe("if", formatExpr(statement.Clauses[1].Condition))
			out = joinStatementsSafe(out, "then")
			out = joinStatementsSafe(out, formatStatlist(statement.Clauses[1].Body))
			for i = 2, #statement.Clauses do
				local st = statement.Clauses[i]
				if st.Condition then
					out = joinStatementsSafe(out, "elseif")
					out = joinStatementsSafe(out, formatExpr(st.Condition))
					out = joinStatementsSafe(out, "then")
				else
					out = joinStatementsSafe(out, "else")
				end
				out = joinStatementsSafe(out, formatStatlist(st.Body))
			end
			out = joinStatementsSafe(out, "end")

		elseif statement.AstType == 'WhileStatement' then
			out = joinStatementsSafe("while", formatExpr(statement.Condition))
			out = joinStatementsSafe(out, "do")
			out = joinStatementsSafe(out, formatStatlist(statement.Body))
			out = joinStatementsSafe(out, "end")

		elseif statement.AstType == 'DoStatement' then
			out = joinStatementsSafe(out, "do")
			out = joinStatementsSafe(out, formatStatlist(statement.Body))
			out = joinStatementsSafe(out, "end")

		elseif statement.AstType == 'ReturnStatement' then
            if isTryCatch then
                out = "__objlua_trycatch_retargs = { "
                for i = 1, #statement.Arguments do
                    out = joinStatementsSafe(out, formatExpr(statement.Arguments[i]))
                    if i ~= #statement.Arguments then
                        out = out..","
                    end
                end
                out = out .. " }"
            else
                out = "return"
                for i = 1, #statement.Arguments do
                    out = joinStatementsSafe(out, formatExpr(statement.Arguments[i]))
                    if i ~= #statement.Arguments then
                        out = out..","
                    end
                end
            end

		elseif statement.AstType == 'BreakStatement' then
			out = "break"

		elseif statement.AstType == 'RepeatStatement' then
			out = "repeat"
			out = joinStatementsSafe(out, formatStatlist(statement.Body))
			out = joinStatementsSafe(out, "until")
			out = joinStatementsSafe(out, formatExpr(statement.Condition))

		elseif statement.AstType == 'Function' then
			--statement.Scope:ObfuscateVariables()
			if statement.IsLocal then
				out = "local"
			end
			out = joinStatementsSafe(out, "function ")
			if statement.IsLocal then
				out = out..statement.Name.Name
			else
				out = out..formatExpr(statement.Name)
			end
			out = out.."("
			if #statement.Arguments > 0 then
				for i = 1, #statement.Arguments do
					out = out..statement.Arguments[i].Name
					if i ~= #statement.Arguments then
						out = out..","
					elseif statement.VarArg then
						--print("Apply vararg")
						out = out..",..."
					end
				end
			elseif statement.VarArg then
				out = out.."..."
			end
			out = out..")"
			out = joinStatementsSafe(out, formatStatlist(statement.Body))
			out = joinStatementsSafe(out, "end")

		elseif statement.AstType == 'GenericForStatement' then
			--statement.Scope:ObfuscateVariables()
			out = "for "
			for i = 1, #statement.VariableList do
				out = out..statement.VariableList[i].Name
				if i ~= #statement.VariableList then
					out = out..","
				end
			end
			out = out.." in"
			for i = 1, #statement.Generators do
				out = joinStatementsSafe(out, formatExpr(statement.Generators[i]))
				if i ~= #statement.Generators then
					out = joinStatementsSafe(out, ',')
				end
			end
			out = joinStatementsSafe(out, "do")
			out = joinStatementsSafe(out, formatStatlist(statement.Body))
			out = joinStatementsSafe(out, "end")

		elseif statement.AstType == 'NumericForStatement' then
			out = "for "
			out = out..statement.Variable.Name.."="
			out = out..formatExpr(statement.Start)..","..formatExpr(statement.End)
			if statement.Step then
				out = out..","..formatExpr(statement.Step)
			end
			out = joinStatementsSafe(out, "do")
			out = joinStatementsSafe(out, formatStatlist(statement.Body))
			out = joinStatementsSafe(out, "end")
		elseif statement.AstType == 'LabelStatement' then
			out = getIndentation() .. "::" .. statement.Label .. "::"
		elseif statement.AstType == 'GotoStatement' then
			out = getIndentation() .. "goto " .. statement.Label
		elseif statement.AstType == 'Comment' then
			-- ignore
		elseif statement.AstType == 'Eof' then
			-- ignore
            
            
        elseif statement.AstType == 'ObjLuaDefinitionStatement' then
            -- @implementation, ignored
            
        elseif statement.AstType == 'ObjLuaDeclarationStatement' then
            --[[ setMethod_(class, returnType, argTable, func)
            out = out .. 'objlua.getClass("' .. statement.Class.Name .. '")' .. '.Type.setMethod_("' .. statement.Class.Name .. '", '
            out = out .. statement.ReturnType .. ", { "
            for i = 1, #statement.Arguments do
                local v = statement.Arguments[i]
                out = out .. '[' .. i .. '] = {'
                out = out .. "['Name'] = '" .. v.Name .. "', "
                out = out .. "['ArgType'] = '" .. v.ArgumentType .. "', ['ArgName'] = '" .. v.ArgumentName .. "' }"
                if i ~= #statement.Arguments then
                    out = out .. ', '
                end
            end
            out = out .. " }, "
            out = out .. formatExpr(statement.Body)
            out = out .. ")"]]
            
            --setMethod(class, methodName, func, args)
            local func = statement.DefinitionType == 'Member' and "setMethod" or "setStaticMethod" -- same signatures. i made sure of this.
            
            out = out .. 'objlua.' .. func .. '(objlua.getClass"' .. statement.Class.Name .. '", '
            out = out .. '"' .. statement.Arguments[1].Name .. '", '
            out = out .. formatExpr(statement.Body) .. ', '
            out = out .. "{ "
            
            local function getArg(a, b)
                --print(a,b)
                if a ~= nil then
                    if a:len() > 0 then
                        return a
                    end
                end
                return b
            end
            
            for i = 1, #statement.Arguments do
                local v = statement.Arguments[i]
                if v.ArgumentType then
                    if v.ArgumentName and v.Name then
                        out = out .. '["' .. v.Name .. '"] = { Name = "' .. v.ArgumentName .. '", Type = "' .. v.ArgumentType .. '" }'
                        if i ~= #statement.Arguments then
                            out = out .. ', '
                        end 
                    else
                        out = out .. '"' .. getArg(v.ArgumentName, v.Name) .. '"'
                        if i ~= #statement.Arguments then
                            out = out .. ', '
                        end
                    end
                else
                    if v.ArgumentName and v.Name then
                        out = out .. '["' .. v.Name .. '"] = "' .. v.ArgumentName .. '"'
                        if i ~= #statement.Arguments then
                            out = out .. ', '
                        end 
                    else
                        out = out .. '"' .. getArg(v.ArgumentName, v.Name) .. '"'
                        if i ~= #statement.Arguments then
                            out = out .. ', '
                        end
                    end
                end
            end
            out = out .. " }, "
            out = out .. (statement.ReturnType and ("'" .. statement.ReturnType .. "'") or "nil")
            out = out .. ", {"
            
            -- arg count thing, used to map native lua function calls to objlua functions (example: x:y --> x(1) { y = 1 }. horrible diagram though...)
            for i = 1, #statement.Arguments do
                out = out .. '[' .. i .. '] = { "' .. statement.Arguments[i].ArgumentName .. '", "' .. statement.Arguments[i].Name .. '" }'
                if i ~= #statement.Arguments then
                    out = out .. ', '
                end
            end
            
            out = out .. "})"
        elseif statement.AstType == 'InterfaceDeclarationStatement' then
            
        elseif statement.AstType == 'InterfaceImplementationStatement' then
            local classVar = statement.Name --"__class__123456789__"
            --out = joinStatementsSafe(out, "local " .. classVar .. " = objlua.createClass('" .. statement.Name .. "'" .. (statement.Inherits and ", '" .. statement.Inherits .. "'" or "") .. (statement.Protocols and fmtProcols(statement.Protocols) or "") .. ")")
            out = joinStatementsSafe(out, classVar .. " = objlua.createClass('" .. statement.Name .. "'" .. (statement.Inherits and ", '" .. statement.Inherits .. "'" or "") .. (statement.Protocols and fmtProcols(statement.Protocols) or "") .. ")")
            
            for _, stat in pairs(statement.Body) do
                out = joinStatementsSafe(out, formatStatement(stat), ';')
            end
            
        
        elseif statement.AstType == 'ObjLuaStatement' then
            if out:sub(-1, -1) == ')' then 
                out = ';' .. joinStatementsSafe(out, formatExpr(statement.Expr)) .. ';'
            else
                out = joinStatementsSafe(out, formatExpr(statement.Expr)) .. ';'
            end
            --out = joinStatementsSafe(out, formatExpr(statement.Object))
            --out = out .. ":"
            --out = out .. ""
        elseif statement.AstType == 'ObjLuaThrowStatement' then
            if isTryCatch then
                out = joinStatementsSafe(out, "error(" .. formatExpr(statement.ErrorObject)) .. ');'
            else
                out = joinStatementsSafe(out, "objlua.throw(")
                out = out .. formatExpr(statement.ErrorObject)
                out = out .. ")"
            end
        elseif statement.AstType == 'ObjLuaStatic' then
            out = joinStatementsSafe(out, "objlua.setStaticField(objlua.getClass'" .. statement.Class.Name .. "', '")
            out = out .. statement.Var .. "', "
            if statement.Expr then
                out = out .. formatExpr(statement.Expr)
            else
                out = out .. "nil"
            end
            out = out .. ", false"
            if statement.Type then
                out = out .. ", '" .. statement.Type .. "'"
            end
            out = out .. ")"
        --elseif statement.AstType == 'ObjLuaPropertyStatement' then
        --    out = joinStatementsSafe(out, "
            
        elseif statement.AstType == 'TryStatement' then
            
            out = joinStatementsSafe(out, "local __objlua_trycatch_retargs; local __objlua_trycatch_args_ = { pcall(function()")
            -- body
            if statement.Body then
                out = out .. formatStatlist(statement.Body, true)
            end
            out = out .. " end) }"
            
            -- catches
            
            out = out .. 
[[
if not __objlua_trycatch_args_[1] then
    -- automatically convert to Exception
    if type(__objlua_trycatch_args_[2]) == 'string' then
        __objlua_trycatch_args_[2] = objlua.getClass("Exception"):alloc():init(__objlua_trycatch_args_[2])
    end
    
]]
            for k, catch in pairs(statement.Catches) do
                --out = joinStatementsSafe(out, "print(objlua.IsOfType(__objlua_trycatch_args_[2], '" .. formatExpr(catch.Expr) .. "'), objlua.TypeOf2(__objlua_trycatch_args_[2]), __objlua_trycatch_args_[2])")
                
                if catch.Expr then
                    out = out .. "if objlua.IsOfType(__objlua_trycatch_args_[2], '" .. formatExpr(catch.Expr) .. "') then"
                end
                if catch.ExceptionVar then
                    out = joinStatementsSafe(out, "local " .. catch.ExceptionVar .. " = __objlua_trycatch_args_[2]")
                --else
                    --out = joinStatementsSafe(out, "local exception = __objlua_trycatch_args_[2]")
                end
                out = joinStatementsSafe(out, formatStatlist(catch.Body))
                if catch.Expr then
                    out = joinStatementsSafe(out, "end")
                end
            end
            out = joinStatementsSafe(out, "end")
            
            -- finally
            if statement.Finally then
                out = joinStatementsSafe(out, formatStatlist(statement.Finally))
            end
            
            -- returns
            out = joinStatementsSafe(out, "if __objlua_trycatch_retargs then return unpack(__objlua_trycatch_retargs) end")
            
            
            --print(out)
		else
            print(util.PrintTable(statement))
			print("Unknown AST Type: " .. statement.AstType)
		end
		count = count + #out
		return out
	end

	formatStatlist = function(statList, isTryCatch)
		local out = ''
        
        assert(statList.Body)
        --statList.Scope:ObfuscateVariables()
        
		for _, stat in pairs(statList.Body) do
			out = joinStatementsSafe(out, formatStatement(stat, isTryCatch), ';')
		end
		return out
	end

	return formatStatlist(ast)
end

return Format_Lua
