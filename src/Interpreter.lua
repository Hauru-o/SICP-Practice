local function cons(a, b)
	return { a, b }
end

local function car(pair)
	return pair[1]
end

local function cdr(pair)
	return pair[2]
end

local function cadr(x)
	return car(cdr(x))
end

local function caar(x)
	return car(car(x))
end

local function cadar(x)
	return car(cdr(car(x)))
end

local function caddr(x)
	return car(cdr(cdr(x)))
end

local function caadr(x)
	return car(car(cdr(x)))
end

local function cadadr(x)
	return car(cdr(car(cdr(x))))
end

local function set_car(pair, x)
	pair[1] = x
end

local function list(...)
	local args = { ... }
	local result = {}
	for i = #args, 1, -1 do
		result = cons(args[i], result)
	end

	return result
end

local add = function(a, b)
	return a + b
end

local sub = function(a, b)
	return a - b
end

local mul = function(a, b)
	return a * b
end

local div = function(a, b)
	return a / b
end

local function isPrimitive(op)
	if op == add or op == sub or op == mul or op == div then
		return true
	end
end

local function isEmpty(t)
	if #t == 0 then
		return true
	else
		return false
	end
end

-- printList 函数：打印类似 Lisp 风格的列表
local function printList(pair)
	if isPrimitive(pair) then
		io.write("#<primitive>")
		return
	end

	if isEmpty(pair) then
		return
	end

	-- 打印左括号
	io.write("(")

	-- 递归打印列表的元素
	local first = car(pair)
	if type(first) == "table" and not isEmpty(first) then
		printList(first)
	else
		io.write(first) -- 打印当前元素
	end

	-- 如果列表的cdr部分不为nil，则继续打印
	local rest = cdr(pair)
	if not isEmpty(rest) then
		io.write(" ") -- 元素之间加空格

		if first ~= "closure" then
			printList(rest)
		else
			io.write("#<function>")
		end
	end

	-- 打印右括号
	io.write(")")
end

local frame0 = list(cons("+", add), cons("-", sub), cons("*", mul), cons("/", div))

local env0 = list(frame0)

local function apply_primop(op, args)
	return op(car(args), cadr(args))
end

local function oprator(exp)
	return car(exp)
end

local function isNumber(exp)
	if type(exp) == "number" then
		return true
	end
	return false
end

local function isSymbol(exp)
	if type(exp) == "string" then
		return true
	end
	return false
end

local function evlist(L, env)
	if isEmpty(L) then
		return {}
	else
		return cons(Eval(car(L), env), evlist(cdr(L), env))
	end
end

local function evcond(clauses, env)
	if isEmpty(clauses) then
		return {}
	elseif caar(clauses) == "else" then
		return Eval(cadar(clauses), env)
	elseif not Eval(caar(clauses), env) then
		return evcond(cdr(clauses), env)
	else
		return Eval(cadar(clauses), env)
	end
end

local function pair_up(vars, vals)
	if isEmpty(vars) then
		if isEmpty(vals) then
			return {}
		else
			error("Too Maney Arg")
		end
	elseif isSymbol(vars) then -- (x . y)
		return cons(cons(vars, vals), {})
	elseif isEmpty(vals) then
		error("TFA")
	else
		return cons(cons(car(vars), car(vals)), pair_up(cdr(vars), cdr(vals)))
	end
end

local function bind(vars, vals, env)
	return cons(pair_up(vars, vals), env)
end

local function assq(sym, alist)
	if isEmpty(alist) then
		return {}
	elseif sym == caar(alist) then
		return car(alist)
	else
		return assq(sym, cdr(alist))
	end
end

local function lookup(sym, env)
	if isEmpty(env) then
		error("Unbind Variable")
	else
		local vcell = assq(sym, car(env))

		if isEmpty(vcell) then
			return lookup(sym, cdr(env))
		else
			return cdr(vcell)
		end
	end
end

local function relast(L)
	if isEmpty(cdr(L)) then
		return car(L)
	else
		return relast(cdr(L))
	end
end

local function evbegin(exps, env)
	return relast(evlist(exps, env))
end

local function evdefine(var, val, env)
	local frame = car(env)

	if isEmpty(assq(var, frame)) then
		set_car(env, cons(cons(var, val), frame))
	else
		error("Already Defined")
	end
end

function Eval(exp, env)
	if isNumber(exp) then
		return exp
	elseif isSymbol(exp) then
		return lookup(exp, env)
	elseif oprator(exp) == "quote" then
		return cadr(exp)
	elseif oprator(exp) == "lambda" then
		return list("closure", cdr(exp), env)
	elseif oprator(exp) == "define" then
		return evdefine(cadr(exp), Eval(caddr(exp), env), env)
	elseif oprator(exp) == "cond" then
		return evcond(cdr(exp), env)
	elseif oprator(exp) == "begin" then
		return evbegin(cdr(exp), env)
	else
		local proc = Eval(car(exp), env)
		local args = evlist(cdr(exp), env)
		printList(proc)
		io.write(" : ")
		printList(args)
		io.write("\n")
		return Apply(proc, args)
	end
end

function Apply(proc, args)
	if isPrimitive(proc) then
		return apply_primop(proc, args)
	elseif car(proc) == "closure" then -- (closure ((x) (+ x y)) <env>)
		return Eval(cadadr(proc), bind(caadr(proc), args, caddr(proc)))
	else
		error("Not closure and not primitive!")
	end
end

-- 获取第一个Atom及剩余部分
local function getAtom(code)
	-- 先去掉前面的空白字符
	code = code:match("^%s*(.*)")

	-- 如果是空字符串，返回空原子
	if #code == 0 then
		return nil, code
	end

	-- 如果是左括号 '('
	if code:sub(1, 1) == "(" then
		return "(", code:sub(2)
	end

	-- 如果是右括号 ')'
	if code:sub(1, 1) == ")" then
		return ")", code:sub(2)
	end

	-- 其他字符，匹配到第一个空格或括号之前的原子
	local atom = code:match("^[^%s()]+")
	return atom, code:sub(#atom + 1)
end

local function code2atoms(code)
	local atoms = {}
	while #code > 0 do
		local atom, rest = getAtom(code)
		if atom then
			table.insert(atoms, atom)
			code = rest
		else
			break
		end
	end
	return atoms
end

local function findLastCloseParen(atoms, head, tail)
	local count = 0
	for i = head + 1, tail do
		if atoms[i] == "(" then
			count = count + 1
		elseif atoms[i] == ")" and count == 0 then
			return i
		elseif atoms[i] == ")" then
			count = count - 1
		end
	end

	error("Not Found Close Paren")
end

local function atoms2tree(atoms, head, tail) -- ( ... )
	local ast = {}
	local tail = findLastCloseParen(atoms, head, tail)
	local i = head + 1
	while i <= tail - 1 do
		if atoms[i] == "(" then
			local tree, closeTail = atoms2tree(atoms, i, tail - 1)
			table.insert(ast, tree)
			i = closeTail + 1
		else
			table.insert(ast, atoms[i])
			i = i + 1
		end
	end

	return ast, tail
end

local function convertType(x)
	if tonumber(x) then
		return tonumber(x)
	elseif tostring(x) then
		if string.sub(x, 1, 1) == "'" then
			return cons("quote", string.sub(x, 2))
		else
			return tostring(x)
		end
	else
		return x
	end
end

local function tree2lispStruct(tree)
	local result = {}
	for i = #tree, 1, -1 do
		if type(tree[i]) == "table" then
			result = cons(tree2lispStruct(tree[i]), result)
		else
			local a = convertType(tree[i])
			result = cons(a, result)
		end
	end
	return result
end

local Harip = function(code)
	local atoms = code2atoms(code)
	local ast = atoms2tree(atoms, 1, #atoms)
	local lispS = tree2lispStruct(ast)
	local result = Eval(lispS, env0)
	return result
end

local code = [[
(begin
	(define pow
		(lambda (x)
			(* x x)))
	(pow 10))
]]

print(Harip(code))
