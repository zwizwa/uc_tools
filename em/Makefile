.PHONY: all clean
MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

# The html files act as toplevel:
# At run time they pull in the .js and .wasm from the server.
HTML=tui.html graph.html

JS=$(HTML:.html=.js)
all: $(HTML)


DEPS := $(shell find -name '*.d')
-include $(DEPS)


UC_TOOLS=$(shell readlink -f ..)
UC_TOOLS_EM=$(UC_TOOLS)/em
EM_CACHE=$(shell readlink -f ./emscripten_cache)
CFLAGS=-I/$(UC_TOOLS_EM) -I/$(UC_TOOLS) -I/$(UC_TOOLS)/gl

S_SDL=\
	-s USE_SDL=2 \
	-s FULL_ES2=1 \
	-s ALLOW_MEMORY_GROWTH=1

EMCC=\
	EM_CACHE=$(EM_CACHE) \
	emcc \
	-s EXPORTED_RUNTIME_METHODS='["cwrap","ccall"]' \
	$(S_SDL)



%.o: %.c
	$(EMCC) $(CFLAGS) -MMD -MF $(@:.o=.d) -c $< -o $@

%.html: %.o %-shell.html
	$(EMCC) $(LDFLAGS) $< -o $@ -lwebsocket.js --shell-file $*-shell.html

clean:
	rm -f $(HTML) $(JS) *.wasm *.d *.o *~


# In case there is no need for a custom (websocket) server, just use emrun.
%.run: %.html
	emrun --no_browser --port 8080 $<


# python3 -m http.server 8000


