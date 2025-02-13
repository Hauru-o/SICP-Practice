local function cons(a, b)
	return { a, b }
end

local function car(pair)
	return pair[1]
end

local function cdr(pair)
	return pair[2]
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
