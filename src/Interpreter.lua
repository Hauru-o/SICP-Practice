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

local function evlist(L, env)
	if L == {} then
		return {}
	else
		return cons(Eval(car(L), env), evlist(cdr(L), env))
	end
end

local function evcond(clauses, env)
	if clauses == {} then
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
	if vars == {} then
		if vals == {} then
			return {}
		else
			error("Too Maney Arg")
		end
	elseif isSymbol(vars) then -- (x . y)
		return cons(cons(vars, vals), {})
	elseif vals == {} then
		error("TFA")
	else
		return cons(cons(car(vars), car(vals)), pair_up(cdr(vars), cdr(vals)))
	end
end

local function bind(vars, vals, env)
	return cons(pair_up(vars, vals), env)
end

local function assq(sym, alist)
	if alist == {} then
		return {}
	elseif sym == caar(alist) then
		return car(alist)
	else
		return assq(sym, cdr(alist))
	end
end

local function lookup(sym, env)
	if env == {} then
		error("Unbind Variable")
	else
		local vcell = assq(sym, car(env))

		if vcell == {} then
			return lookup(sym, cdr(env))
		else
			return cdr(vcell)
		end
	end
end

local function relast(L)
	if cdr(L) == {} then
		return car(L)
	else
		relast(cdr(L))
	end
end

local function evbegin(exps, env)
	return relast(evlist(exps, env))
end

local function evdefine(var, val, env)
	local frame = car(env)

	if assq(var, frame) == {} then
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
		return Apply(Eval(car(exp), env), evlist(cdr(exp), env))
	end
end

function Apply(proc, args)
	if isPrimitive(proc) then
	elseif car(proc) == "closure" then -- (closure ((x) (+ x y)) <env>)
		return Eval(cadadr(proc), bind(caadr(proc), args, caddr(proc)))
	else
		error("Not closure and not primitive!")
	end
end
