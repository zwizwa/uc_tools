-- Transliterated from rvm.scm
-- This test wrapper uses lure, a Scheme-in-Lua library (see luarocks package)
local rt  = require('lure.slc_runtime')
local se  = require('lure.se')
local rvm = require('lure.rvm')

local debug = true


local is_rib = rvm.is_rib
local FALSE  = rvm.FALSE
local TRUE   = rvm.TRUE
local NIL    = rvm.NIL


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

local function chars2str(s) if is==NIL then return "" else return chr(s[1]) .. chars2str(s[2]) end end --debug--
local function sym2str(s) return chars2str(s[2][1]) end --debug--
local function str(o) return "" .. o end --debug--

local function car(p) return p[1] end
local function cdr(p) return p[2] end
-- local function cons(a,d) return rib(a,d,0) end -- pair-type
-- local function set_car(p,v) p[1] = v end

local function sym(n)
   return car(list_tail(symtbl,n))
end


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

local dbg = {
   trace_instruction = trace_instruction
}

local input = "Detouq,htgnel-gnirts,fer-gnirts,fi,!rdc-tes,tsil>-rotcev,!tes-gnirts,enifed,!tes-rotcev,?rotcev,=,cc/llac,!tes,adbmal,rddc,gnirts-ekam,fer-rotcev,htgnel-rotcev,rotcev-ekam,lobmys>-gnirts,gnirts>-lobmys,?erudecorp,!rac-tes,tneitouq,enilwen,ton,lave,fer-tsil,rdddac,*,?tcejbo-foe,?lobmys,lper,?gnirts,rotcev>-tsil,+,etirw,rahc-keep,yalpsid,tsil>-gnirts,daer,gnirts>-tsil,?lauqe,,,,?llun,htgnel,,,,,rddac,rdac,-,,,<,,rac,?riap,,rahc-daer,rdc,,snoc,,?vqe,,,,,;8K!K8K@Z%@YGZ#^'i$~YM^YC@PvCvR3y]#7#YS*^z!S*9Bi&:EiS/ai&kkz!S/:kw'k]@'_*Z@aC_G^~F^{!>'^8>YHlbC`^'`~?_G_~F_|]D9C`^Uka_CaG`.ZDdCbAai$G`^~F_|!S+#`kn3^~i$#`kn3^~i$#`kn3^~i$#`kn3^~RJ^~?w)B^~?kH^~R^z]K#YS+a_l{]C#a_k#k_k~?iS/_{!.#b`n9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYOecEfNdboMa_~?x:^.ZKdUlbMbNa_~O?x6_9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYOecEfNdboMa_~?x:^.ZKdUlbMbNa_~O^~^?x1^#cMan~?x=^G_~F_#bUk``m~YL_|!94_@K^{]%4uy]?'i$9?C_@K^G^~F^z]I'i$'i$9IC^@YGG^~F^@KvC~F^z!E8EYS(^89vS7vF~Z(^9?YD^~YJ^8EZ)^~YL^4vL@ZIC^@YGG^@KvK~F^89vLvK~T^89vS;vF~?i%^89vS-vF~Z$^z!G8E^4vE@Z?i%YD^@KvE~YJ^z]O9O8@~?u^'^~Ik^Dy!@8@@D'^9O~?vR0^~I_vC'iS0~YM^YFy!?*V^@D'i&~OOIvD`*V^@D'i&~OO^~^?vL_*V^@D'i&~O^~^?vK^YFy]M*ZM^YC'i&@D~?vL^Wy!C9*`'^~^^YS%^YBAV^@D*Ai&YCx=@D~?vJ^8IYC'i%@D~?vS;^'i$@D~?vS-^YF@D~?vF^9M@D~?vK^'^~Ik^Wy!F'^!S-^Dy]H'^!S-iS.'^~?iS0^!S-^z!-9H^9HYS#~?iS.^'^~?iS0^iS-y!S-iS.!M(iS0^z]27%Z>'_@YS&Jc^@YS'Hc^BBZ>i$zBBZ>i$z]B#l`^{](Ql]+8IZLk^z]59Nb`H^|]-8P`H^{],i+]8i1!I#oS_^z]4Qo].8BZLvC^z]79Nb`H^|];8P`H^{]<i+!Di1!B#nS_^z!JQn]F'_'i$'i$9FLLvR%`YNbuC_~IvR/^~I_vR$G^~F^{]G9Fk^'i$~T^z!S%'i$5_k~^ZG^9GC^~?vPG^'i$~T^YD^z]E'^9E_`~IakAb^YHLYNu``vR%Z&u^{!S(8BZEi&^8BAZEi&L`kvP~Ik^z]3i(@YS)ki#!S,Bi#]P'^!S,AiS,^YS$^9PBa_'^~YA`B^H_~F_{]*9PiS,^z])i+!S$#m_i$z!LQm]J'`9JAca`Ll^~I_k|]L9Ji&`^{]A'^9ALl`C^~I`k{]N9'aZA`^|!P0ZA`^{!<'k8HSC_l~F^z!=(i&^z!O87B^z!76B^z]/+B^z!61B^z]9iS)]'iS'!,i+!0i1!*#k`^{!/Qk!A'i$'i$'i$'i$8AHaH_~YABaB_~YAJaJ_~R`'i$~?pJ_~R_'^~^?`^{]$(i$^z!:9>'i$(bJ^~R^zz!S.Lmk!S0Llk!':lkl!):lkm!8:lkn]>:lko!;:lkp!1:lkq!+:lkr!3:lks!S':lkt!S):lku!S&:lkv.!(:lkv/!2:lkv0!H:lkv1!5:lkv2!N:lkv3]&:lkv4!S#:lkv5!4:lkv6y"


rvm.test(input, dbg)
