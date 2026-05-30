.PHONY: all clean
all: index.html


DEPS := $(shell find -name '*.d')
-include $(DEPS)


UC_TOOLS=$(shell readlink -f ..)
UC_TOOLS_EM=$(UC_TOOLS)/emscripten
EM_CACHE=$(shell readlink -f ./emscripten_cache)
CFLAGS=-I/$(UC_TOOLS_EM) -I/$(UC_TOOLS)

EMCC=EM_CACHE=$(EM_CACHE) emcc

# index.wasm: main.c
# 	ls -l $(UC_TOOLS_EM)/mod_emscripten_console.c
# 	$(EMCC)  $(CFLAGS) -MD -MF $(patsubst %.wasm,%.d,$@) main.c -o index.html -lwebsocket.js
# 	cat index.d


%.o: %.c
	$(EMCC) $(CFLAGS) -MMD -MF $(@:.o=.d) -c $< -o $@

OBJS=main.o
index.html: $(OBJS) shell.html
	$(EMCC) $(LDFLAGS)  $(OBJS)  -o index.html -lwebsocket.js --shell-file shell.html


clean:
	rm -f index.html index.wasm index.js *.d *.o *~



# python3 -m http.server 8000


