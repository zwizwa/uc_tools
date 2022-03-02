input = "Detouq,htgnel-gnirts,fer-gnirts,fi,!rdc-tes,tsil>-rotcev,!tes-gnirts,enifed,!tes-rotcev,?rotcev,=,cc/llac,!tes,adbmal,rddc,gnirts-ekam,fer-rotcev,htgnel-rotcev,rotcev-ekam,lobmys>-gnirts,gnirts>-lobmys,?erudecorp,!rac-tes,tneitouq,enilwen,ton,lave,fer-tsil,rdddac,*,?tcejbo-foe,?lobmys,lper,?gnirts,rotcev>-tsil,+,etirw,rahc-keep,yalpsid,tsil>-gnirts,daer,gnirts>-tsil,?lauqe,,,,?llun,htgnel,,,,,rddac,rdac,-,,,<,,rac,?riap,,rahc-daer,rdc,,snoc,,?vqe,,,,,;8K!K8K@Z%@YGZ#^'i$~YM^YC@PvCvR3y]#7#YS*^z!S*9Bi&:EiS/ai&kkz!S/:kw'k]@'_*Z@aC_G^~F^{!>'^8>YHlbC`^'`~?_G_~F_|]D9C`^Uka_CaG`.ZDdCbAai$G`^~F_|!S+#`kn3^~i$#`kn3^~i$#`kn3^~i$#`kn3^~RJ^~?w)B^~?kH^~R^z]K#YS+a_l{]C#a_k#k_k~?iS/_{!.#b`n9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYOecEfNdboMa_~?x:^.ZKdUlbMbNa_~O?x6_9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYOecEfNdboMa_~?x:^.ZKdUlbMbNa_~O^~^?x1^#cMan~?x=^G_~F_#bUk``m~YL_|!94_@K^{]%4uy]?'i$9?C_@K^G^~F^z]I'i$'i$9IC^@YGG^~F^@KvC~F^z!E8EYS(^89vS7vF~Z(^9?YD^~YJ^8EZ)^~YL^4vL@ZIC^@YGG^@KvK~F^89vLvK~T^89vS;vF~?i%^89vS-vF~Z$^z!G8E^4vE@Z?i%YD^@KvE~YJ^z]O9O8@~?u^'^~Ik^Dy!@8@@D'^9O~?vR0^~I_vC'iS0~YM^YFy!?*V^@D'i&~OOIvD`*V^@D'i&~OO^~^?vL_*V^@D'i&~O^~^?vK^YFy]M*ZM^YC'i&@D~?vL^Wy!C9*`'^~^^YS%^YBAV^@D*Ai&YCx=@D~?vJ^8IYC'i%@D~?vS;^'i$@D~?vS-^YF@D~?vF^9M@D~?vK^'^~Ik^Wy!F'^!S-^Dy]H'^!S-iS.'^~?iS0^!S-^z!-9H^9HYS#~?iS.^'^~?iS0^iS-y!S-iS.!M(iS0^z]27%Z>'_@YS&Jc^@YS'Hc^BBZ>i$zBBZ>i$z]B#l`^{](Ql]+8IZLk^z]59Nb`H^|]-8P`H^{],i+]8i1!I#oS_^z]4Qo].8BZLvC^z]79Nb`H^|];8P`H^{]<i+!Di1!B#nS_^z!JQn]F'_'i$'i$9FLLvR%`YNbuC_~IvR/^~I_vR$G^~F^{]G9Fk^'i$~T^z!S%'i$5_k~^ZG^9GC^~?vPG^'i$~T^YD^z]E'^9E_`~IakAb^YHLYNu``vR%Z&u^{!S(8BZEi&^8BAZEi&L`kvP~Ik^z]3i(@YS)ki#!S,Bi#]P'^!S,AiS,^YS$^9PBa_'^~YA`B^H_~F_{]*9PiS,^z])i+!S$#m_i$z!LQm]J'`9JAca`Ll^~I_k|]L9Ji&`^{]A'^9ALl`C^~I`k{]N9'aZA`^|!P0ZA`^{!<'k8HSC_l~F^z!=(i&^z!O87B^z!76B^z]/+B^z!61B^z]9iS)]'iS'!,i+!0i1!*#k`^{!/Qk!A'i$'i$'i$'i$8AHaH_~YABaB_~YAJaJ_~R`'i$~?pJ_~R_'^~^?`^{]$(i$^z!:9>'i$(bJ^~R^zz!S.Lmk!S0Llk!':lkl!):lkm!8:lkn]>:lko!;:lkp!1:lkq!+:lkr!3:lks!S':lkt!S):lku!S&:lkv.!(:lkv/!2:lkv0!H:lkv1!5:lkv2!N:lkv3]&:lkv4!S#:lkv5!4:lkv6y"


local stdo = io.stdout

local function putchar(c) stdo:write(string.char(c)); stdo:flush(); return c end

local push

local function getchar()
   push(io.stdin:read(1):byte(1) or -1)
end

local debug = false  --debug--

local function chars2str(s) if is==NIL then return "" else return chr(s[1]) .. chars2str(s[2]) end end --debug--
local function sym2str(s) return chars2str(s[2][1]) end --debug--
local function str(o) return "" .. o end --debug--

local function is_rib(x)  return type(x) == 'table' and x.class == 'rib' end  -- FIXME
-- local function is_rib(x)  return type(x) == 'table' end  -- FIXME

local pair_type = 0
local procedure_type = 1
local symbol_type = 2
local string_type = 3
local vector_type = 4
local singleton_type = 5

local function instance(typ)
   return function(x)
      return is_rib(x) and typ == x[3]
   end
end
local is_pair = instance(pair_type)


local stack=0


local pos=0
local function get_byte()
   pos = pos+1
   local int = input:byte(pos) or 0
   return int
end

local function rib(a,b,c)
   assert(a and b and c) --debug--
   return {
      class = 'rib', --debug--
      [1]=a,[2]=b,[3]=c
   }
end

local FALSE=rib(0,0,5)
local TRUE=rib(0,0,5)
local NIL=rib(0,0,5)

local function to_bool(x)
   if x then
      return TRUE
   else
      return FALSE
   end
end

push = function(x)
   stack = rib(x,stack,0)
end

local function pop()
   local x = stack[1]
   stack = stack[2]
   assert(x)
   return x
end

-- Agrument evaluation order is undefined in Lua, vs. Python left-to-right.
-- Since we need to be explicit anyway, we use normal argument order for f,
-- as opposed to Python RVM which has them reversed.
local function prim1(f) return function() local a=pop(); push(f(a)) end end
local function prim2(f) return function() local b=pop();local a=pop(); push(f(a,b)) end end
local function prim3(f) return function() local c=pop();local b=pop(); local a=pop(); push(f(a,b,c)) end end


local function arg2() local x = pop(); pop(); push(x) end
local function close() push(rib(pop()[1],stack,1)) end
local function f0s(x,y) x[1]=y; return y; end
local function f1s(x,y) x[2]=y; return y; end
local function f2s(x,y) x[3]=y; return y; end

local primitives = {
   prim3(rib), -- 0
   prim1(function(x) return x end), -- 1
   pop, -- 2
   arg2, -- 3
   close, -- 4
   prim1(function(x) return to_bool(is_rib(x)) end), -- 5
   prim1(function(x) return x[1] end), -- 6
   prim1(function(x) return x[2] end), -- 7
   prim1(function(x) return x[3] end), -- 8
   prim2(f0s), -- 9
   prim2(f1s), -- 10
   prim2(f2s), -- 11
   prim2(function(x,y) return to_bool(x == y) end), -- 12
   prim2(function(x,y) return to_bool(x<y) end), -- 13
   prim2(function(x,y) return x + y end), -- 14
   prim2(function(x,y) return x - y end), -- 15
   prim2(function(x,y) return x * y end), -- 16
   prim2(function(x,y) return math.floor(x / y) end),  -- 17  -- FIXME
   getchar, -- 18
   prim1(putchar) -- 19
}


local function get_code()
   local x = get_byte() - 35
   if x<0 then
      return 57
   else
      return x
   end
end

local function get_int(n)
   local x = get_code()
   n = n * 46
   if x<46 then
      return n+x
   else
      return get_int(n + x -46)
   end
end

local function list_tail(lst, i)
   if i==0 then
      return lst
   else
      return list_tail(lst[2],i-1)
   end
end

local n -- used a couple of times below

local symtbl


local function build_symtbl()
   symtbl = NIL
   n = get_int(0)
   while n>0 do
      n = n - 1
      -- log_desc({n=n,pos=pos})
      symtbl = rib(rib(0,rib(NIL,0,3),2),symtbl,0)
   end

   local accum = NIL
   n=0
   while true do
      local c = get_byte()
      if c == 44 then
         symtbl = rib(rib(0,rib(accum,n,3),2),symtbl,0)
         accum = NIL
         n = 0
      else
         if c == 59 then break end
         accum = rib(c, accum, 0)
         n = n + 1
      end
   end

   symtbl=rib(rib(0,rib(accum,n,3),2),symtbl,0)

   return symtbl
end

local function car(p) return p[1] end
local function cdr(p) return p[2] end
-- local function cons(a,d) return rib(a,d,0) end -- pair-type
-- local function set_car(p,v) p[1] = v end

local function sym(n)
   return car(list_tail(symtbl,n))
end




local rt = require('lure.slc_runtime')

local se = require('lure.se')
function convert(obj)
   if obj == FALSE then
      return false
   elseif obj == TRUE then
      return true
   elseif obj == NIL then
      return se.empty
   elseif not is_rib(obj) then
      -- log_desc({obj=obj})
      return obj
   else
      local typ = obj[3]
      -- log_desc({typ=typ})
      if typ == pair_type then
         -- return '#<pair>'
         return se.cons(convert(car(obj)), convert(cdr(obj)))
      elseif typ == procedure_type then
         return "<procedure>"
      elseif typ == symbol_type then
         -- return "<symbol>"
         -- return rt['string->symbol'](convert(obj[2]))
         return convert(obj[2])
      elseif typ == string_type then
         return rt['list->string'](se.map(rt['integer->char'],convert(obj[1])))
         -- return convert(obj[1])
      elseif typ == vector_type then
         return se.list('vector', convert(obj[1]))
      else
         return "<unknown>"
      end
   end
end

local function trace_instruction(name, opnd, stack)
   if not debug then return end

   function show_stack(stack)
      if is_pair(stack) then
         return se.cons(convert(car(stack)),
                        show_stack(cdr(stack)))
      else
         if stack == 0 then
            return se.empty
         else
            return se.list(
               'stack:', show_stack(stack[1]),
               'pc: ', convert(stack[3]))
         end
      end
   end

   log(": ")
   log(name)
   log(",")
   --if opnd then -- FIXME, if?
      log_w(se.iolist(convert(opnd)))
      log(",")
   --end
   log_w(se.iolist(show_stack(stack)))
   log("\n")

end




local function symbol_ref(n)
   return list_tail(symtbl,n)[1]
end

function decode_loop()
while true do
 local x=get_code()
 -- log_desc({get_code=x})
 local n=x
 local d=0
 local op=0
 while true do
  local ds={20,30,0,10,11,4}
  d=ds[op+1]
  if n<=2+d then break end
  n=n-(d+3) ; op=op+1
 end
 if x>90 then
  n=pop()
 else
  if op==0 then
     stack=rib(0,stack,0);
     op = op + 1
  end
  if n==d then
     n = get_int(0)
  elseif n>= d then
     n = symbol_ref(get_int(n-d-1))
  elseif op<3 then
     n = symbol_ref(n)
  end
  -- log_se_n(convert(n))
  if 4<op then
   n=rib(rib(n,0,pop()),0,1)
   if not is_rib(stack) then
      return n
   end
   op=4
  end
 end
 stack[1]=rib(op-1,n,stack[1])
end

end



local get_opnd = function(o)
   if is_rib(o) then
      return o
   else
      return list_tail(stack,o)
   end
end

local function get_cont()
   -- log_desc({'get_cont'})
   local s = stack
   while not is_rib(s[3]) do
      s = s[2]
   end
   return s
end

local function set_global(val)
   symtbl[1][1]=val
   symtbl=symtbl[2]
end



local function decode()
   build_symtbl()
   local main_proc = decode_loop()

   set_global(rib(0,symtbl,1)) -- procedure type, primitive 0
   set_global(FALSE)
   set_global(TRUE)
   set_global(NIL)

   return main_proc
end


local function run()
   local count = 0
   local n = decode()
   local pc = n[1][3]
   stack=rib(0,0,rib(5,0,0)) -- primordial continuation (executes halt instr.)

   while true do
      count = count + 1
      local o=pc[2]
      local i=pc[1]
      -- log_desc({count,i})
      if i<1 then -- jump/call
         trace_instruction("jump/call",o,stack)
         o=get_opnd(o)[1]
         c=o[1]
         if is_rib(c) then
            local c2=rib(0,o,0)
            local s2=c2
            nargs=c[1]
            while nargs > 0 do
               s2=rib(pop(),s2,0)
               nargs=nargs-1
            end
            if is_rib(pc[3]) then -- call
               c2[1]=stack
               c2[3]=pc[3]
            else -- jump
               k=get_cont()
               c2[1]=k[1]
               c2[3]=k[3]
            end
            stack=s2

         else
            -- log_desc({'prim',c})
            primitives[c+1]()
            if is_rib(pc[3]) then -- call
               c=pc
            else --  jump
               c=get_cont()
               stack[2]=c[1]
            end
         end
         pc=c[3]

      elseif i<2 then -- set
         trace_instruction("set",o,stack)
         x=pop()
         get_opnd(o)[1]=x
         pc=pc[3]

      elseif i<3 then -- get
         trace_instruction("get",o,stack)
         push(get_opnd(o)[1])
         pc=pc[3]

      elseif i<4 then -- const
         trace_instruction("const",o,stack)
         push(o)
         pc=pc[3]

      elseif i<5 then -- if
         trace_instruction("if",o,stack)
         local index
         if pop() == FALSE then
            index = 2
         else
            index = 1
         end
         pc=pc[index+1]

      else -- halt
         break
      end
   end

end

local function get_byte_or_read_char()
   if pos < #input then
      return get_byte()
   else
      local c = io.stdin:read(1)
      return c:byte(1) or -1
   end
end



run()

return {
   ['primitives-lua'] = primitives,
   input = input,
   putchar = putchar,
   getchar = getchar,
   ['get-input-byte'] = get_byte,
   ['_false'] = FALSE,
   ['_true'] = TRUE,
   ['_nil'] = NIL,
   ['build-symtbl-lua'] = build_symtbl,
   ['_rib'] = rib,
   ['get-byte-or-read-char'] = get_byte_or_read_char,
   ['get-code'] = get_code,
   ['get-int'] = get_int,
   ['sym'] = sym,
   ['decode-lua'] = decode,
   ['run-lua'] = run,
   ['trace-instruction'] = trace_instruction,
}
