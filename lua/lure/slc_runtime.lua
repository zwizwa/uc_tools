require('lure.log_se')
local se = require('lure.se')
local match = require('lure.match')

local rt = {}

function rt.new(mod)
   local lib = {}
   lib['module-register!'] = function(k,v) mod[k] = v end
   lib['new'] = function() error "new suppressed" end
   setmetatable(lib, {__index = rt})
   return function(k) return lib[k] end
end

-- Tail recursion trampoline.
-- See scheme_macros.lua
-- FIXME: The wrapped recursive calls only work in tail position!
--        Compiler should probably guarantee that.
-- FIXME: Optimzation: the argument vector and tag wrapper could be
-- re-used to avoid allocation while the loop is running.

rt['letrec-trampoline'] = function(init, ...)
   -- log('letrec-trampoline\n')
   local bodies = {...}

   -- We get passed the function bodies of each of the recursive
   -- functions, and the body of the letrec expression.  Each of these
   -- functions is "open", i.e. parameterized by the names of the
   -- recursive functions.

   -- What we do here is act as a fix point combinator for this
   -- collection of functions.  The bodies are position-encoded.

   -- Example:

   -- (letrec ((check (lambda (n) (if (n > 0) 'done (inc n))))
   --          (inc   (lambda (n) (check (+ n 1)))))
   --   (check 0))
   --
   -- Which might be an expansion of
   --
   -- (begin
   --   (define (check n) (if (n > 0) 'done (inc n)))
   --   (define (inc n)   (check (+ n 1)))
   --   (check 0))
   --
   -- Is represend as
   --
   -- bodies[1] = (lambda (check inc) (lambda (n) (if (n > 0) 'done (inc n))))
   -- bodies[2] = (lambda (check inc) (lambda (n) (check (+ n 1))))
   -- init      = (lambda (check inc) (lambda (n) (check 0)))
   --
   -- Where the lambda forms are converted to Lua in the usual way.


   -- We implement this by binding the function names to functions
   -- that construct a data structure that represents a postponed
   -- call.  This is what happens on the 'inside' of the function
   -- bodies. These wrappers are then returned to the trampoline
   -- because we expect these calls to always be in tail position
   -- (compiler needs to check!). The wrapper contains the argument
   -- list plus tag data that allows us to refer back to the correct
   -- function.
   local inside = {}
   for i=1,#bodies do
      inside[i] = function(...)
         -- We just need a unique tag here.  Something that is not
         -- known by the function bodies.  The table object will do.
         return {inside, i, {...}}
      end
   end
   -- On the outside, we bind the function bodies to those
   -- wrapper-generating functions.
   local outside = {}
   for i=1,#bodies do
      outside[i] = bodies[i](unpack(inside))
   end

   -- Package the loop
   local function start(i0, nxt_args)
      local nxt = outside[i0]
      local rvs
      while true do
         rvs = {nxt(unpack(nxt_args))}
         -- log_desc({rvs=rvs})
         local rvs1 = rvs[1]
         if type(rvs1) ~= 'table' then break end
         local maybe_tag, i, args = unpack(rvs1)
         if maybe_tag ~= inside then break end
         -- Found encoded tail call.
         nxt = outside[i]
         nxt_args = args
      end
      return unpack(rvs)
   end

   -- The loop is engaged when the init function calls one of the
   -- recursive functions.  So we parameterize init with another
   -- wrapper to start the loop.
   local starts = {}
   for i=1,#bodies do
      starts[i] = function(...) return start(i, {...}) end
   end
   return init(unpack(starts))()
end


function rt.vector(...)
   return {...}
end
rt['string-ref'] = function(str, i)
   local b = str:byte(i+1)
   -- log_desc({b=b})
   return b
end
rt['char->integer'] = function(char)
   return char
end
rt['integer->char'] = function(char)
   return char
end
rt['vector-ref'] = function(vec, i)
   return vec[i+1]
end
rt['vector-set!'] = function(vec, i, val)
   vec[i+1] = val
end

-- FIXME: These need review + documentation as Scheme is weird about equality.
local function lua_eqp(a, b) return a == b end
rt['eq?']    = lua_eqp
rt['eqv?']   = lua_eqp
rt['=']      = lua_eqp
rt['equal?'] = se.equalp


rt['vector?'] = function(a)
   return type(a) == 'table'
end
rt['read-char'] = function()
   return io.stdin:read(1)
end
rt['write-char'] = function(c)
   return io.stdout:write(c)
end
rt['table-ref'] = function(tab, key)
   return tab[key]
end
rt['table-set!'] = function(tab, key, val)
   tab[key] = val
end
rt['compile-qq-pattern'] = function(pat)
   local cns = se.constructor(pat)
   return match.compile(cns)
end

rt['match-qq-patterns'] = function(expr, clauses)
   for clause in se.elements(clauses) do
      local pat, handle = unpack(clause)
      local m = match.apply(pat, expr)
      if m then
         return handle(m)
      end
   end
   assert('no-match')
end
rt['log-se'] = function(se)
   log_se(se)
end
rt['log-se-n'] = function(se, tag)
   log_se_n(se, tag)
end
rt['assert'] = assert

local function import_from(other_mod, names)
   for _,name in ipairs(names) do
      rt[name] = other_mod[name]
   end
end
import_from(se, {'cons','list','car','cdr','cadr','reverse','map','zip','foldr','foldl','length'})

-- Compiler will generate code for these when applied, but we still
-- need to be able to reference the functions as values.
rt['+'] = function(a,b) return a + b end
rt['-'] = function(a,b) return a - b end
rt['*'] = function(a,b) return a * b end
rt['/'] = function(a,b) return a / b end
rt['>'] = function(a,b) return a > b end

-- (define p (make-parameter 'init_val))
-- (p) -> 'init_val
-- (p 'new_val) (p) -> 'new_val
-- See 'parameterize' in scheme_macros.lua
rt['make-parameter'] = function(init_val)
   local val = init_val
   return function(new_val)
      if new_val ~= nil then val = new_val end
      return val
   end
end


-- FIXME: for testing rvm
rt.input = "Detouq,htgnel-gnirts,fer-gnirts,fi,!rdc-tes,tsil>-rotcev,!tes-gnirts,enifed,!tes-rotcev,?rotcev,=,cc/llac,!tes,adbmal,rddc,gnirts-ekam,fer-rotcev,htgnel-rotcev,rotcev-ekam,lobmys>-gnirts,gnirts>-lobmys,?erudecorp,!rac-tes,tneitouq,enilwen,ton,lave,fer-tsil,rdddac,*,?tcejbo-foe,?lobmys,lper,?gnirts,rotcev>-tsil,+,etirw,rahc-keep,yalpsid,tsil>-gnirts,daer,gnirts>-tsil,?lauqe,,,,?llun,htgnel,,,,,rddac,rdac,-,,,<,,rac,?riap,,rahc-daer,rdc,,snoc,,?vqe,,,,,;8K!K8K@Z%@YGZ#^'i$~YM^YC@PvCvR3y]#7#YS*^z!S*9Bi&:EiS/ai&kkz!S/:kw'k]@'_*Z@aC_G^~F^{!>'^8>YHlbC`^'`~?_G_~F_|]D9C`^Uka_CaG`.ZDdCbAai$G`^~F_|!S+#`kn3^~i$#`kn3^~i$#`kn3^~i$#`kn3^~RJ^~?w)B^~?kH^~R^z]K#YS+a_l{]C#a_k#k_k~?iS/_{!.#b`n9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYOecEfNdboMa_~?x:^.ZKdUlbMbNa_~O?x6_9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYOecEfNdboMa_~?x:^.ZKdUlbMbNa_~O^~^?x1^#cMan~?x=^G_~F_#bUk``m~YL_|!94_@K^{]%4uy]?'i$9?C_@K^G^~F^z]I'i$'i$9IC^@YGG^~F^@KvC~F^z!E8EYS(^89vS7vF~Z(^9?YD^~YJ^8EZ)^~YL^4vL@ZIC^@YGG^@KvK~F^89vLvK~T^89vS;vF~?i%^89vS-vF~Z$^z!G8E^4vE@Z?i%YD^@KvE~YJ^z]O9O8@~?u^'^~Ik^Dy!@8@@D'^9O~?vR0^~I_vC'iS0~YM^YFy!?*V^@D'i&~OOIvD`*V^@D'i&~OO^~^?vL_*V^@D'i&~O^~^?vK^YFy]M*ZM^YC'i&@D~?vL^Wy!C9*`'^~^^YS%^YBAV^@D*Ai&YCx=@D~?vJ^8IYC'i%@D~?vS;^'i$@D~?vS-^YF@D~?vF^9M@D~?vK^'^~Ik^Wy!F'^!S-^Dy]H'^!S-iS.'^~?iS0^!S-^z!-9H^9HYS#~?iS.^'^~?iS0^iS-y!S-iS.!M(iS0^z]27%Z>'_@YS&Jc^@YS'Hc^BBZ>i$zBBZ>i$z]B#l`^{](Ql]+8IZLk^z]59Nb`H^|]-8P`H^{],i+]8i1!I#oS_^z]4Qo].8BZLvC^z]79Nb`H^|];8P`H^{]<i+!Di1!B#nS_^z!JQn]F'_'i$'i$9FLLvR%`YNbuC_~IvR/^~I_vR$G^~F^{]G9Fk^'i$~T^z!S%'i$5_k~^ZG^9GC^~?vPG^'i$~T^YD^z]E'^9E_`~IakAb^YHLYNu``vR%Z&u^{!S(8BZEi&^8BAZEi&L`kvP~Ik^z]3i(@YS)ki#!S,Bi#]P'^!S,AiS,^YS$^9PBa_'^~YA`B^H_~F_{]*9PiS,^z])i+!S$#m_i$z!LQm]J'`9JAca`Ll^~I_k|]L9Ji&`^{]A'^9ALl`C^~I`k{]N9'aZA`^|!P0ZA`^{!<'k8HSC_l~F^z!=(i&^z!O87B^z!76B^z]/+B^z!61B^z]9iS)]'iS'!,i+!0i1!*#k`^{!/Qk!A'i$'i$'i$'i$8AHaH_~YABaB_~YAJaJ_~R`'i$~?pJ_~R_'^~^?`^{]$(i$^z!:9>'i$(bJ^~R^zz!S.Lmk!S0Llk!':lkl!):lkm!8:lkn]>:lko!;:lkp!1:lkq!+:lkr!3:lks!S':lkt!S):lku!S&:lkv.!(:lkv/!2:lkv0!H:lkv1!5:lkv2!N:lkv3]&:lkv4!S#:lkv5!4:lkv6y"


return rt


