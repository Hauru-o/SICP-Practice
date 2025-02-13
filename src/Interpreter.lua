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

local function caddr(x)
	return car(cdr(cdr(x)))
end

local function list(...)
	local args = { ... }
	local result = {}
	for i = 1, #args do
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
	if op == add then
		return true
	end
end

local frame0 = list(cons("+", add), cons("-", sub), cons("*", mul), cons("/", div))

local env0 = list(frame0)

local function apply_primop(op, args)
	return op(car(args), cdr(args))
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

local function eval(exp, env)
	if isNumber(exp) then
		return exp
	elseif isSymbol(exp) then
		return lookup(exp, env)
	elseif oprator(exp) == 'quote' then
		return cadr(exp)
	elseif oprator(exp) == 'lambda' then
		return list('closure', cdr(exp), env)
	elseif oprator(exp) == 'define' then
		return evdefine(cadr(exp), eval(caddr(exp), env), env)
	elseif oprator(exp) == 'cond' then
		return evcond(cdr(exp), env)
	elseif oprator(exp) == 'begin' then
		return evbegin(cdr(exp), env)
	else
		return apply(
			eval(car(exp), env),
			evlist(cdr(exp), env))
	end
end