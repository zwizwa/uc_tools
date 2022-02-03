require('lure.log')

local rt = {}

-- See test_slc.lua
rt['named-let-trampoline'] = function(state, body)
   -- The named let symbol is bound to a wrapper so it behaves just
   -- like an ordinary function.
   local done
   local loop_tick = body(
      function(...)
         state = {...}
         done = false
      end)

   -- The trampoline
   while true do
      done = true
      local rvs = {loop_tick(unpack(state))}
      if done then return unpack(rvs) end
   end
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
rt['eq?'] = function(a, b)
   return a == b
end
rt['eqv?'] = function(a, b)
   return a == b
end
rt['='] = function(a, b)
   return a == b
end
rt['vector?'] = function(a)
   return type(a) == 'table'
end
rt['read-char'] = function()
   return io.stdin:read(1)
end
rt['write-char'] = function(c)
   return io.stdout:write(c)
end



rt['ok'] = function()
   error('ok')
end
rt['desc'] = log_desc


rt.input = "Detouq,htgnel-gnirts,fer-gnirts,fi,!rdc-tes,tsil>-rotcev,!tes-gnirts,enifed,!tes-rotcev,?rotcev,=,cc/llac,!tes,adbmal,rddc,gnirts-ekam,fer-rotcev,htgnel-rotcev,rotcev-ekam,lobmys>-gnirts,gnirts>-lobmys,?erudecorp,!rac-tes,tneitouq,enilwen,ton,lave,fer-tsil,rdddac,*,?tcejbo-foe,?lobmys,lper,?gnirts,rotcev>-tsil,+,etirw,rahc-keep,yalpsid,tsil>-gnirts,daer,gnirts>-tsil,?lauqe,,,,?llun,htgnel,,,,,rddac,rdac,-,,,<,,rac,?riap,,rahc-daer,rdc,,snoc,,?vqe,,,,,;8K!K8K@Z%@YGZ#^'i$~YM^YC@PvCvR3y]#7#YS*^z!S*9Bi&:EiS/ai&kkz!S/:kw'k]@'_*Z@aC_G^~F^{!>'^8>YHlbC`^'`~?_G_~F_|]D9C`^Uka_CaG`.ZDdCbAai$G`^~F_|!S+#`kn3^~i$#`kn3^~i$#`kn3^~i$#`kn3^~RJ^~?w)B^~?kH^~R^z]K#YS+a_l{]C#a_k#k_k~?iS/_{!.#b`n9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYOecEfNdboMa_~?x:^.ZKdUlbMbNa_~O?x6_9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYOecEfNdboMa_~?x:^.ZKdUlbMbNa_~O^~^?x1^#cMan~?x=^G_~F_#bUk``m~YL_|!94_@K^{]%4uy]?'i$9?C_@K^G^~F^z]I'i$'i$9IC^@YGG^~F^@KvC~F^z!E8EYS(^89vS7vF~Z(^9?YD^~YJ^8EZ)^~YL^4vL@ZIC^@YGG^@KvK~F^89vLvK~T^89vS;vF~?i%^89vS-vF~Z$^z!G8E^4vE@Z?i%YD^@KvE~YJ^z]O9O8@~?u^'^~Ik^Dy!@8@@D'^9O~?vR0^~I_vC'iS0~YM^YFy!?*V^@D'i&~OOIvD`*V^@D'i&~OO^~^?vL_*V^@D'i&~O^~^?vK^YFy]M*ZM^YC'i&@D~?vL^Wy!C9*`'^~^^YS%^YBAV^@D*Ai&YCx=@D~?vJ^8IYC'i%@D~?vS;^'i$@D~?vS-^YF@D~?vF^9M@D~?vK^'^~Ik^Wy!F'^!S-^Dy]H'^!S-iS.'^~?iS0^!S-^z!-9H^9HYS#~?iS.^'^~?iS0^iS-y!S-iS.!M(iS0^z]27%Z>'_@YS&Jc^@YS'Hc^BBZ>i$zBBZ>i$z]B#l`^{](Ql]+8IZLk^z]59Nb`H^|]-8P`H^{],i+]8i1!I#oS_^z]4Qo].8BZLvC^z]79Nb`H^|];8P`H^{]<i+!Di1!B#nS_^z!JQn]F'_'i$'i$9FLLvR%`YNbuC_~IvR/^~I_vR$G^~F^{]G9Fk^'i$~T^z!S%'i$5_k~^ZG^9GC^~?vPG^'i$~T^YD^z]E'^9E_`~IakAb^YHLYNu``vR%Z&u^{!S(8BZEi&^8BAZEi&L`kvP~Ik^z]3i(@YS)ki#!S,Bi#]P'^!S,AiS,^YS$^9PBa_'^~YA`B^H_~F_{]*9PiS,^z])i+!S$#m_i$z!LQm]J'`9JAca`Ll^~I_k|]L9Ji&`^{]A'^9ALl`C^~I`k{]N9'aZA`^|!P0ZA`^{!<'k8HSC_l~F^z!=(i&^z!O87B^z!76B^z]/+B^z!61B^z]9iS)]'iS'!,i+!0i1!*#k`^{!/Qk!A'i$'i$'i$'i$8AHaH_~YABaB_~YAJaJ_~R`'i$~?pJ_~R_'^~^?`^{]$(i$^z!:9>'i$(bJ^~R^zz!S.Lmk!S0Llk!':lkl!):lkm!8:lkn]>:lko!;:lkp!1:lkq!+:lkr!3:lks!S':lkt!S):lku!S&:lkv.!(:lkv/!2:lkv0!H:lkv1!5:lkv2!N:lkv3]&:lkv4!S#:lkv5!4:lkv6y"


return rt


