-- Shared language front-end
-- Preprocessor that performs the following:
--
-- MACRO-EXPAND
-- A-NORMAL FORM
-- VARIABLE RENAMING (FIXME)
-- SIMPLIFY BLOCK (TODO)
-- SINGLE LAMBDA EXPR (TODO)
--
-- Separate later passes:
--
-- BLOCK FLATTENING
--
-- OUTPUT CLEANUP / LANG PPRINT
--
-- Macro expansion and ANF seem to go hand-in hand.
-- Variable renaming is useful for later block-flattening.

-- FIXME: This is still written in old se.unpack() style, and might
-- benefit from using the pattern matcher.  However, it works, so not
-- touching it for now.

local se = require('lure.se')
local comp = require('lure.comp')

local ins = table.insert
local a2l = se.array_to_list
local l = se.list


local class = {
   parameterize = comp.parameterize,
}

-- Bind macros to state object for gensym.
class.macro = {} ; do
   for name, m in pairs(require('lure.scheme_macros')) do
      -- log("MACRO: " .. name .. "\n")
      class.macro[name] = function(s, expr)
         return m(expr, { state = s, void = '#<void>' })
      end
   end
end


local function s_id(s, thing) return thing end

class.expander = {
   ['string'] = s_id,
   ['number'] = s_id,
   ['var']    = s_id,
   ['pair']   = function(s, expr)
      local car, cdr = unpack(expr)
      local m = s.macro[car]
      if m ~= nil then
         return m(s, expr), 'again'
      else
         return expr
      end
   end
}

local function trace(tag, expr)
   log('\n') ; log_se_n(expr, tag .. ": ")
end

function class.expand_step(s, expr)
   trace("STEP",expr)
   local typ = se.expr_type(expr)
   local f = s.expander[typ]
   if f == nil then error('expand: bad type ' .. typ) end
   return f(s, expr)
end

function class.expand(s, expr)
   local again = 'again'
   while again do
      assert(again == 'again')
      expr, again = s:expand_step(expr)
   end
   return expr
end

local void = '#<void>'

-- Note that primitive form evaluation and macro expansion are
-- intertwined.  Gives more compact code.  Convention is that the form
-- compilers return a primitive form, which is one of
local prim_out_forms = {'block','set!','if','lambda'}

class.form = {
   -- This is like 'begin', but without support for local definitons.
   ['sequence'] = function(s, expr)
      local _, forms = se.unpack(expr, {n = 1, tail = true})
      local function tx_form(seqform)
         return l('_', s:compile(seqform))
      end
      return l('block',se.map(tx_form, forms))
   end,
   ['set!'] = function(s, expr)
      local _, var, vexpr = se.unpack(expr, {n = 3})
      return s:anf(
         l(vexpr),
         function(e) return l('set!', s:var_ref(var), se.car(e)) end)
   end,
   ['if'] = function(s, expr)
      local _, econd, etrue, efalse = se.unpack(expr, {n = 4})
      return s:anf(
         l(econd),
         function(e) return l('if', se.car(e), s:compile(etrue), s:compile(efalse)) end)
   end,
   ['lambda'] = function(s, expr)
      local _, src_names, body = se.unpack(expr, {n = 2, tail = true})
      local vars = se.map(
         function(src_name)
            local var = s:var_def(src_name)
            assert(var and var.renamed)
            return var
         end,
         src_names)
      return l('lambda', vars,
               s:compile_extend(
                  {'begin',body},
                  vars))
   end
}

-- Compile in extended environment
function class.compile_extend(s, expr, vars)
   local new_env = s.env
   for var in se.elements(vars) do
      new_env = {var, new_env}
   end
   return s:parameterize(
      {env = new_env},
      function()
         -- log_se_n(expr, "COMPILE_EXTEND:")
         return s:compile(expr)
      end)
end

function is_var(var)
   return type(var) == 'table' and var.class == 'var'
end
local function is_prim(expr)
   -- variable
   if is_var(expr) then return false end
   local typ = type(expr)
   if typ == 'table' then
      -- abstract object
      if typ.class then return true
      -- expression
      else return false end
   end
   -- anything else
   return false
end


-- Convention is that fn returns a primitive output form.
function class.anf(s, exprs, fn)
   local normalform = {}
   local bindings = {}
   for e in se.elements(exprs) do
      -- FIXME: This should compile before checking if it's primitive.
      if type(e) == 'string' then
         -- Source variable
         ins(normalform, s:var_ref(e))
      elseif is_var(e) then
         -- Abstract variable reference (is this actually possible here?)
         ins(normalform, e)
      elseif is_prim(e) then
         ins(normalform, e)
      else
         assert(type(e) == 'table')
         -- Composite.  Bind it to a variable.  The name here is just
         -- for debugging.
         local var = s:var_def("tmp")
         ins(bindings, l(var, s:compile(e)))
         ins(normalform, var)
      end
   end
   if #bindings == 0 then
      return fn(a2l(normalform))
   else
      local form =
         l('block',
           a2l(bindings),
           fn(a2l(normalform)))
      -- 'let' is not primitive, so expand it here.
      return s:expand(form)
   end
end

local function apply(s, expr)
   trace("APPLY", expr)
   return s:anf(expr, function(e) return e end)
end

class.compiler = {
   ['number'] = s_id,
   ['string'] = function(s, str)
      local var = s:var_ref(str)
      assert(var and var.class == 'var')
      return var
   end,
   ['pair'] = function(s, expr)
      local car, cdr = unpack(expr)
      local f = s.form[car]
      if f ~= nil then
         return f(s, expr)
      else
         return apply(s, expr)
      end
   end
}
function class.compile(s, expr)
   expr = s:expand(expr)
   trace("COMPILE",expr)
   local typ = se.expr_type(expr)
   local f = s.compiler[typ]
   if f == nil then error('compile: bad type ' .. typ) end
   return f(s, expr)
end

function class.gensym(s, prefix)
   -- Gensyms should never clash with source variables.  We can't
   -- guarantee that atm.  FIXME.
   s.count = s.count + 1
   local sym = (prefix or "r") .. s.count
   return sym
end

-- Renames definitions and references.
function var_iolist(var)
   assert(var.var)
   assert(var.renamed)
   return {"#<var:",var.var,":",var.renamed,">"}
end
function class.var_def(s, name)
   assert(type(name) == 'string')
   local sym = s:gensym()
   return { var = name, renamed = sym, class = 'var', iolist = var_iolist }
end


function class.var_ref(s, var)
   -- Make it idempotent for var objects.
   if (is_var(var)) then return var end
   -- If it's a string, it's always going to be a source symbol, which
   -- has to be renamed.  First check lexical variables in
   -- environment.
   local name = var
   for v in se.elements(s.env) do
      if name == v.var then
         return v
      end
   end
   -- References to non-lexical variables are stored in a separate
   -- table.  We create those on demand, and have to re-use.
   local v = s.globals[name]
   if not v then
      v = s:var_def(name)
      s.globals[name] = v
   end
   return v
end



function class.new()
   local obj = { count = 0, globals = {}, env = {} }
   setmetatable(obj, { __index = class })
   return obj
end


return class

