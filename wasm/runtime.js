﻿// Produced by emscripten, then pretty printed.
var Module = typeof Module !== "undefined" ? Module : {};
var moduleOverrides = {};
var key;
for (key in Module) {
	if (Module.hasOwnProperty(key)) {
		moduleOverrides[key] = Module[key]
	}
}
Module["arguments"] = [];
Module["thisProgram"] = "./this.program";
Module["quit"] = (function(status, toThrow) {
	throw toThrow
});
Module["preRun"] = [];
Module["postRun"] = [];
var ENVIRONMENT_IS_WEB = false;
var ENVIRONMENT_IS_WORKER = false;
var ENVIRONMENT_IS_NODE = false;
var ENVIRONMENT_IS_SHELL = false;
if (Module["ENVIRONMENT"]) {
	if (Module["ENVIRONMENT"] === "WEB") {
		ENVIRONMENT_IS_WEB = true
	} else if (Module["ENVIRONMENT"] === "WORKER") {
		ENVIRONMENT_IS_WORKER = true
	} else if (Module["ENVIRONMENT"] === "NODE") {
		ENVIRONMENT_IS_NODE = true
	} else if (Module["ENVIRONMENT"] === "SHELL") {
		ENVIRONMENT_IS_SHELL = true
	} else {
		throw new Error("Module['ENVIRONMENT'] value is not valid. must be one of: WEB|WORKER|NODE|SHELL.")
	}
} else {
	ENVIRONMENT_IS_WEB = typeof window === "object";
	ENVIRONMENT_IS_WORKER = typeof importScripts === "function";
	ENVIRONMENT_IS_NODE = typeof process === "object" && typeof require === "function" && !ENVIRONMENT_IS_WEB && !ENVIRONMENT_IS_WORKER;
	ENVIRONMENT_IS_SHELL = !ENVIRONMENT_IS_WEB && !ENVIRONMENT_IS_NODE && !ENVIRONMENT_IS_WORKER
}
if (ENVIRONMENT_IS_NODE) {
	var nodeFS;
	var nodePath;
	Module["read"] = function shell_read(filename, binary) {
		var ret;
		if (!nodeFS) nodeFS = require("fs");
		if (!nodePath) nodePath = require("path");
		filename = nodePath["normalize"](filename);
		ret = nodeFS["readFileSync"](filename);
		return binary ? ret : ret.toString()
	};
	Module["readBinary"] = function readBinary(filename) {
		var ret = Module["read"](filename, true);
		if (!ret.buffer) {
			ret = new Uint8Array(ret)
		}
		assert(ret.buffer);
		return ret
	};
	if (process["argv"].length > 1) {
		Module["thisProgram"] = process["argv"][1].replace(/\\/g, "/")
	}
	Module["arguments"] = process["argv"].slice(2);
	if (typeof module !== "undefined") {
		module["exports"] = Module
	}
	process["on"]("uncaughtException", (function(ex) {
		if (!(ex instanceof ExitStatus)) {
			throw ex
		}
	}));
	process["on"]("unhandledRejection", (function(reason, p) {
		process["exit"](1)
	}));
	Module["inspect"] = (function() {
		return "[Emscripten Module object]"
	})
} else if (ENVIRONMENT_IS_SHELL) {
	if (typeof read != "undefined") {
		Module["read"] = function shell_read(f) {
			return read(f)
		}
	}
	Module["readBinary"] = function readBinary(f) {
		var data;
		if (typeof readbuffer === "function") {
			return new Uint8Array(readbuffer(f))
		}
		data = read(f, "binary");
		assert(typeof data === "object");
		return data
	};
	if (typeof scriptArgs != "undefined") {
		Module["arguments"] = scriptArgs
	} else if (typeof arguments != "undefined") {
		Module["arguments"] = arguments
	}
	if (typeof quit === "function") {
		Module["quit"] = (function(status, toThrow) {
			quit(status)
		})
	}
} else if (ENVIRONMENT_IS_WEB || ENVIRONMENT_IS_WORKER) {
	Module["read"] = function shell_read(url) {
		var xhr = new XMLHttpRequest;
		xhr.open("GET", url, false);
		xhr.send(null);
		return xhr.responseText
	};
	if (ENVIRONMENT_IS_WORKER) {
		Module["readBinary"] = function readBinary(url) {
			var xhr = new XMLHttpRequest;
			xhr.open("GET", url, false);
			xhr.responseType = "arraybuffer";
			xhr.send(null);
			return new Uint8Array(xhr.response)
		}
	}
	Module["readAsync"] = function readAsync(url, onload, onerror) {
		var xhr = new XMLHttpRequest;
		xhr.open("GET", url, true);
		xhr.responseType = "arraybuffer";
		xhr.onload = function xhr_onload() {
			if (xhr.status == 200 || xhr.status == 0 && xhr.response) {
				onload(xhr.response);
				return
			}
			onerror()
		};
		xhr.onerror = onerror;
		xhr.send(null)
	};
	if (typeof arguments != "undefined") {
		Module["arguments"] = arguments
	}
	Module["setWindowTitle"] = (function(title) {
		document.title = title
	})
}
Module["print"] = typeof console !== "undefined" ? console.log.bind(console) : typeof print !== "undefined" ? print : null;
Module["printErr"] = typeof printErr !== "undefined" ? printErr : typeof console !== "undefined" && console.warn.bind(console) || Module["print"];
Module.print = Module["print"];
Module.printErr = Module["printErr"];
for (key in moduleOverrides) {
	if (moduleOverrides.hasOwnProperty(key)) {
		Module[key] = moduleOverrides[key]
	}
}
moduleOverrides = undefined;
var STACK_ALIGN = 16;

function staticAlloc(size) {
	assert(!staticSealed);
	var ret = STATICTOP;
	STATICTOP = STATICTOP + size + 15 & -16;
	return ret
}

function alignMemory(size, factor) {
	if (!factor) factor = STACK_ALIGN;
	var ret = size = Math.ceil(size / factor) * factor;
	return ret
}
var functionPointers = new Array(0);
var GLOBAL_BASE = 1024;
var ABORT = 0;
var EXITSTATUS = 0;

function assert(condition, text) {
	if (!condition) {
		abort("Assertion failed: " + text)
	}
}

function getCFunc(ident) {
	var func = Module["_" + ident];
	assert(func, "Cannot call unknown function " + ident + ", make sure it is exported");
	return func
}
var JSfuncs = {
	"stackSave": (function() {
		stackSave()
	}),
	"stackRestore": (function() {
		stackRestore()
	}),
	"arrayToC": (function(arr) {
		var ret = stackAlloc(arr.length);
		writeArrayToMemory(arr, ret);
		return ret
	}),
	"stringToC": (function(str) {
		var ret = 0;
		if (str !== null && str !== undefined && str !== 0) {
			var len = (str.length << 2) + 1;
			ret = stackAlloc(len);
			stringToUTF8(str, ret, len)
		}
		return ret
	})
};
var toC = {
	"string": JSfuncs["stringToC"],
	"array": JSfuncs["arrayToC"]
};

function ccall(ident, returnType, argTypes, args, opts) {
	var func = getCFunc(ident);
	var cArgs = [];
	var stack = 0;
	if (args) {
		for (var i = 0; i < args.length; i++) {
			var converter = toC[argTypes[i]];
			if (converter) {
				if (stack === 0) stack = stackSave();
				cArgs[i] = converter(args[i])
			} else {
				cArgs[i] = args[i]
			}
		}
	}
	var ret = func.apply(null, cArgs);
	if (returnType === "string") ret = Pointer_stringify(ret);
	if (stack !== 0) {
		stackRestore(stack)
	}
	return ret
}

function cwrap(ident, returnType, argTypes) {
	argTypes = argTypes || [];
	var cfunc = getCFunc(ident);
	var numericArgs = argTypes.every((function(type) {
		return type === "number"
	}));
	var numericRet = returnType !== "string";
	if (numericRet && numericArgs) {
		return cfunc
	}
	return (function() {
		return ccall(ident, returnType, argTypes, arguments)
	})
}

function Pointer_stringify(ptr, length) {
	if (length === 0 || !ptr) return "";
	var hasUtf = 0;
	var t;
	var i = 0;
	while (1) {
		t = HEAPU8[ptr + i >> 0];
		hasUtf |= t;
		if (t == 0 && !length) break;
		i++;
		if (length && i == length) break
	}
	if (!length) length = i;
	var ret = "";
	if (hasUtf < 128) {
		var MAX_CHUNK = 1024;
		var curr;
		while (length > 0) {
			curr = String.fromCharCode.apply(String, HEAPU8.subarray(ptr, ptr + Math.min(length, MAX_CHUNK)));
			ret = ret ? ret + curr : curr;
			ptr += MAX_CHUNK;
			length -= MAX_CHUNK
		}
		return ret
	}
	return UTF8ToString(ptr)
}
var UTF8Decoder = typeof TextDecoder !== "undefined" ? new TextDecoder("utf8") : undefined;

function UTF8ArrayToString(u8Array, idx) {
	var endPtr = idx;
	while (u8Array[endPtr]) ++endPtr;
	if (endPtr - idx > 16 && u8Array.subarray && UTF8Decoder) {
		return UTF8Decoder.decode(u8Array.subarray(idx, endPtr))
	} else {
		var u0, u1, u2, u3, u4, u5;
		var str = "";
		while (1) {
			u0 = u8Array[idx++];
			if (!u0) return str;
			if (!(u0 & 128)) {
				str += String.fromCharCode(u0);
				continue
			}
			u1 = u8Array[idx++] & 63;
			if ((u0 & 224) == 192) {
				str += String.fromCharCode((u0 & 31) << 6 | u1);
				continue
			}
			u2 = u8Array[idx++] & 63;
			if ((u0 & 240) == 224) {
				u0 = (u0 & 15) << 12 | u1 << 6 | u2
			} else {
				u3 = u8Array[idx++] & 63;
				if ((u0 & 248) == 240) {
					u0 = (u0 & 7) << 18 | u1 << 12 | u2 << 6 | u3
				} else {
					u4 = u8Array[idx++] & 63;
					if ((u0 & 252) == 248) {
						u0 = (u0 & 3) << 24 | u1 << 18 | u2 << 12 | u3 << 6 | u4
					} else {
						u5 = u8Array[idx++] & 63;
						u0 = (u0 & 1) << 30 | u1 << 24 | u2 << 18 | u3 << 12 | u4 << 6 | u5
					}
				}
			}
			if (u0 < 65536) {
				str += String.fromCharCode(u0)
			} else {
				var ch = u0 - 65536;
				str += String.fromCharCode(55296 | ch >> 10, 56320 | ch & 1023)
			}
		}
	}
}

function UTF8ToString(ptr) {
	return UTF8ArrayToString(HEAPU8, ptr)
}

function stringToUTF8Array(str, outU8Array, outIdx, maxBytesToWrite) {
	if (!(maxBytesToWrite > 0)) return 0;
	var startIdx = outIdx;
	var endIdx = outIdx + maxBytesToWrite - 1;
	for (var i = 0; i < str.length; ++i) {
		var u = str.charCodeAt(i);
		if (u >= 55296 && u <= 57343) u = 65536 + ((u & 1023) << 10) | str.charCodeAt(++i) & 1023;
		if (u <= 127) {
			if (outIdx >= endIdx) break;
			outU8Array[outIdx++] = u
		} else if (u <= 2047) {
			if (outIdx + 1 >= endIdx) break;
			outU8Array[outIdx++] = 192 | u >> 6;
			outU8Array[outIdx++] = 128 | u & 63
		} else if (u <= 65535) {
			if (outIdx + 2 >= endIdx) break;
			outU8Array[outIdx++] = 224 | u >> 12;
			outU8Array[outIdx++] = 128 | u >> 6 & 63;
			outU8Array[outIdx++] = 128 | u & 63
		} else if (u <= 2097151) {
			if (outIdx + 3 >= endIdx) break;
			outU8Array[outIdx++] = 240 | u >> 18;
			outU8Array[outIdx++] = 128 | u >> 12 & 63;
			outU8Array[outIdx++] = 128 | u >> 6 & 63;
			outU8Array[outIdx++] = 128 | u & 63
		} else if (u <= 67108863) {
			if (outIdx + 4 >= endIdx) break;
			outU8Array[outIdx++] = 248 | u >> 24;
			outU8Array[outIdx++] = 128 | u >> 18 & 63;
			outU8Array[outIdx++] = 128 | u >> 12 & 63;
			outU8Array[outIdx++] = 128 | u >> 6 & 63;
			outU8Array[outIdx++] = 128 | u & 63
		} else {
			if (outIdx + 5 >= endIdx) break;
			outU8Array[outIdx++] = 252 | u >> 30;
			outU8Array[outIdx++] = 128 | u >> 24 & 63;
			outU8Array[outIdx++] = 128 | u >> 18 & 63;
			outU8Array[outIdx++] = 128 | u >> 12 & 63;
			outU8Array[outIdx++] = 128 | u >> 6 & 63;
			outU8Array[outIdx++] = 128 | u & 63
		}
	}
	outU8Array[outIdx] = 0;
	return outIdx - startIdx
}

function stringToUTF8(str, outPtr, maxBytesToWrite) {
	return stringToUTF8Array(str, HEAPU8, outPtr, maxBytesToWrite)
}
var UTF16Decoder = typeof TextDecoder !== "undefined" ? new TextDecoder("utf-16le") : undefined;
var WASM_PAGE_SIZE = 65536;
var ASMJS_PAGE_SIZE = 16777216;

function alignUp(x, multiple) {
	if (x % multiple > 0) {
		x += multiple - x % multiple
	}
	return x
}
var buffer, HEAP8, HEAPU8, HEAP16, HEAPU16, HEAP32, HEAPU32, HEAPF32, HEAPF64;

function updateGlobalBuffer(buf) {
	Module["buffer"] = buffer = buf
}

function updateGlobalBufferViews() {
	Module["HEAP8"] = HEAP8 = new Int8Array(buffer);
	Module["HEAP16"] = HEAP16 = new Int16Array(buffer);
	Module["HEAP32"] = HEAP32 = new Int32Array(buffer);
	Module["HEAPU8"] = HEAPU8 = new Uint8Array(buffer);
	Module["HEAPU16"] = HEAPU16 = new Uint16Array(buffer);
	Module["HEAPU32"] = HEAPU32 = new Uint32Array(buffer);
	Module["HEAPF32"] = HEAPF32 = new Float32Array(buffer);
	Module["HEAPF64"] = HEAPF64 = new Float64Array(buffer)
}
var STATIC_BASE, STATICTOP, staticSealed;
var STACK_BASE, STACKTOP, STACK_MAX;
var DYNAMIC_BASE, DYNAMICTOP_PTR;
STATIC_BASE = STATICTOP = STACK_BASE = STACKTOP = STACK_MAX = DYNAMIC_BASE = DYNAMICTOP_PTR = 0;
staticSealed = false;
var TOTAL_STACK = Module["TOTAL_STACK"] || 5242880;
var TOTAL_MEMORY = Module["TOTAL_MEMORY"] || 16777216;
if (TOTAL_MEMORY < TOTAL_STACK) Module.printErr("TOTAL_MEMORY should be larger than TOTAL_STACK, was " + TOTAL_MEMORY + "! (TOTAL_STACK=" + TOTAL_STACK + ")");
if (Module["buffer"]) {
	buffer = Module["buffer"]
} else {
	if (typeof WebAssembly === "object" && typeof WebAssembly.Memory === "function") {
		Module["wasmMemory"] = new WebAssembly.Memory({
			"initial": TOTAL_MEMORY / WASM_PAGE_SIZE,
			"maximum": TOTAL_MEMORY / WASM_PAGE_SIZE
		});
		buffer = Module["wasmMemory"].buffer
	} else {
		buffer = new ArrayBuffer(TOTAL_MEMORY)
	}
	Module["buffer"] = buffer
}
updateGlobalBufferViews();
HEAP32[0] = 1668509029;
HEAP16[1] = 25459;
if (HEAPU8[2] !== 115 || HEAPU8[3] !== 99) throw "Runtime error: expected the system to be little-endian!";

function callRuntimeCallbacks(callbacks) {
	while (callbacks.length > 0) {
		var callback = callbacks.shift();
		if (typeof callback == "function") {
			callback();
			continue
		}
		var func = callback.func;
		if (typeof func === "number") {
			if (callback.arg === undefined) {
				Module["dynCall_v"](func)
			} else {
				Module["dynCall_vi"](func, callback.arg)
			}
		} else {
			func(callback.arg === undefined ? null : callback.arg)
		}
	}
}
var __ATPRERUN__ = [];
var __ATINIT__ = [];
var __ATMAIN__ = [];
var __ATEXIT__ = [];
var __ATPOSTRUN__ = [];
var runtimeInitialized = false;
var runtimeExited = false;

function preRun() {
	if (Module["preRun"]) {
		if (typeof Module["preRun"] == "function") Module["preRun"] = [Module["preRun"]];
		while (Module["preRun"].length) {
			addOnPreRun(Module["preRun"].shift())
		}
	}
	callRuntimeCallbacks(__ATPRERUN__)
}

function ensureInitRuntime() {
	if (runtimeInitialized) return;
	runtimeInitialized = true;
	callRuntimeCallbacks(__ATINIT__)
}

function preMain() {
	callRuntimeCallbacks(__ATMAIN__)
}

function exitRuntime() {
	callRuntimeCallbacks(__ATEXIT__);
	runtimeExited = true
}

function postRun() {
	if (Module["postRun"]) {
		if (typeof Module["postRun"] == "function") Module["postRun"] = [Module["postRun"]];
		while (Module["postRun"].length) {
			addOnPostRun(Module["postRun"].shift())
		}
	}
	callRuntimeCallbacks(__ATPOSTRUN__)
}

function addOnPreRun(cb) {
	__ATPRERUN__.unshift(cb)
}

function addOnPostRun(cb) {
	__ATPOSTRUN__.unshift(cb)
}

function writeArrayToMemory(array, buffer) {
	HEAP8.set(array, buffer)
}
var Math_abs = Math.abs;
var Math_cos = Math.cos;
var Math_sin = Math.sin;
var Math_tan = Math.tan;
var Math_acos = Math.acos;
var Math_asin = Math.asin;
var Math_atan = Math.atan;
var Math_atan2 = Math.atan2;
var Math_exp = Math.exp;
var Math_log = Math.log;
var Math_sqrt = Math.sqrt;
var Math_ceil = Math.ceil;
var Math_floor = Math.floor;
var Math_pow = Math.pow;
var Math_imul = Math.imul;
var Math_fround = Math.fround;
var Math_round = Math.round;
var Math_min = Math.min;
var Math_max = Math.max;
var Math_clz32 = Math.clz32;
var Math_trunc = Math.trunc;
var runDependencies = 0;
var runDependencyWatcher = null;
var dependenciesFulfilled = null;

function addRunDependency(id) {
	runDependencies++;
	if (Module["monitorRunDependencies"]) {
		Module["monitorRunDependencies"](runDependencies)
	}
}

function removeRunDependency(id) {
	runDependencies--;
	if (Module["monitorRunDependencies"]) {
		Module["monitorRunDependencies"](runDependencies)
	}
	if (runDependencies == 0) {
		if (runDependencyWatcher !== null) {
			clearInterval(runDependencyWatcher);
			runDependencyWatcher = null
		}
		if (dependenciesFulfilled) {
			var callback = dependenciesFulfilled;
			dependenciesFulfilled = null;
			callback()
		}
	}
}
Module["preloadedImages"] = {};
Module["preloadedAudios"] = {};
var dataURIPrefix = "data:application/octet-stream;base64,";

function isDataURI(filename) {
	return String.prototype.startsWith ? filename.startsWith(dataURIPrefix) : filename.indexOf(dataURIPrefix) === 0
}

function integrateWasmJS() {
	var wasmTextFile = "a.out.wast";
	var wasmBinaryFile = "a.out.wasm";
	var asmjsCodeFile = "a.out.temp.asm.js";
	if (typeof Module["locateFile"] === "function") {
		if (!isDataURI(wasmTextFile)) {
			wasmTextFile = Module["locateFile"](wasmTextFile)
		}
		if (!isDataURI(wasmBinaryFile)) {
			wasmBinaryFile = Module["locateFile"](wasmBinaryFile)
		}
		if (!isDataURI(asmjsCodeFile)) {
			asmjsCodeFile = Module["locateFile"](asmjsCodeFile)
		}
	}
	var wasmPageSize = 64 * 1024;
	var info = {
		"global": null,
		"env": null,
		"asm2wasm": {
			"f64-rem": (function(x, y) {
				return x % y
			}),
			"debugger": (function() {
				debugger
			})
		},
		"parent": Module
	};
	var exports = null;

	function mergeMemory(newBuffer) {
		var oldBuffer = Module["buffer"];
		if (newBuffer.byteLength < oldBuffer.byteLength) {
			Module["printErr"]("the new buffer in mergeMemory is smaller than the previous one. in native wasm, we should grow memory here")
		}
		var oldView = new Int8Array(oldBuffer);
		var newView = new Int8Array(newBuffer);
		newView.set(oldView);
		updateGlobalBuffer(newBuffer);
		updateGlobalBufferViews()
	}

	function fixImports(imports) {
		return imports
	}

	function getBinary() {
		try {
			if (Module["wasmBinary"]) {
				return new Uint8Array(Module["wasmBinary"])
			}
			if (Module["readBinary"]) {
				return Module["readBinary"](wasmBinaryFile)
			} else {
				throw "on the web, we need the wasm binary to be preloaded and set on Module['wasmBinary']. emcc.py will do that for you when generating HTML (but not JS)"
			}
		} catch (err) {
			abort(err)
		}
	}

	function getBinaryPromise() {
		if (!Module["wasmBinary"] && (ENVIRONMENT_IS_WEB || ENVIRONMENT_IS_WORKER) && typeof fetch === "function") {
			return fetch(wasmBinaryFile, {
				credentials: "same-origin"
			}).then((function(response) {
				if (!response["ok"]) {
					throw "failed to load wasm binary file at '" + wasmBinaryFile + "'"
				}
				return response["arrayBuffer"]()
			})).catch((function() {
				return getBinary()
			}))
		}
		return new Promise((function(resolve, reject) {
			resolve(getBinary())
		}))
	}

	function doNativeWasm(global, env, providedBuffer) {
		if (typeof WebAssembly !== "object") {
			Module["printErr"]("no native wasm support detected");
			return false
		}
		if (!(Module["wasmMemory"] instanceof WebAssembly.Memory)) {
			Module["printErr"]("no native wasm Memory in use");
			return false
		}
		env["memory"] = Module["wasmMemory"];
		info["global"] = {
			"NaN": NaN,
			"Infinity": Infinity
		};
		info["global.Math"] = Math;
		info["env"] = env;

		function receiveInstance(instance, module) {
			exports = instance.exports;
			if (exports.memory) mergeMemory(exports.memory);
			Module["asm"] = exports;
			Module["usingWasm"] = true;
			removeRunDependency("wasm-instantiate")
		}
		addRunDependency("wasm-instantiate");
		if (Module["instantiateWasm"]) {
			try {
				return Module["instantiateWasm"](info, receiveInstance)
			} catch (e) {
				Module["printErr"]("Module.instantiateWasm callback failed with error: " + e);
				return false
			}
		}

		function receiveInstantiatedSource(output) {
			receiveInstance(output["instance"], output["module"])
		}

		function instantiateArrayBuffer(receiver) {
			getBinaryPromise().then((function(binary) {
				return WebAssembly.instantiate(binary, info)
			})).then(receiver).catch((function(reason) {
				Module["printErr"]("failed to asynchronously prepare wasm: " + reason);
				abort(reason)
			}))
		}
		if (!Module["wasmBinary"] && typeof WebAssembly.instantiateStreaming === "function" && !isDataURI(wasmBinaryFile) && typeof fetch === "function") {
			WebAssembly.instantiateStreaming(fetch(wasmBinaryFile, {
				credentials: "same-origin"
			}), info).then(receiveInstantiatedSource).catch((function(reason) {
				Module["printErr"]("wasm streaming compile failed: " + reason);
				Module["printErr"]("falling back to ArrayBuffer instantiation");
				instantiateArrayBuffer(receiveInstantiatedSource)
			}))
		} else {
			instantiateArrayBuffer(receiveInstantiatedSource)
		}
		return {}
	}
	Module["asmPreload"] = Module["asm"];
	var asmjsReallocBuffer = Module["reallocBuffer"];
	var wasmReallocBuffer = (function(size) {
		var PAGE_MULTIPLE = Module["usingWasm"] ? WASM_PAGE_SIZE : ASMJS_PAGE_SIZE;
		size = alignUp(size, PAGE_MULTIPLE);
		var old = Module["buffer"];
		var oldSize = old.byteLength;
		if (Module["usingWasm"]) {
			try {
				var result = Module["wasmMemory"].grow((size - oldSize) / wasmPageSize);
				if (result !== (-1 | 0)) {
					return Module["buffer"] = Module["wasmMemory"].buffer
				} else {
					return null
				}
			} catch (e) {
				return null
			}
		}
	});
	Module["reallocBuffer"] = (function(size) {
		if (finalMethod === "asmjs") {
			return asmjsReallocBuffer(size)
		} else {
			return wasmReallocBuffer(size)
		}
	});
	var finalMethod = "";
	Module["asm"] = (function(global, env, providedBuffer) {
		env = fixImports(env);
		if (!env["table"]) {
			var TABLE_SIZE = Module["wasmTableSize"];
			if (TABLE_SIZE === undefined) TABLE_SIZE = 1024;
			var MAX_TABLE_SIZE = Module["wasmMaxTableSize"];
			if (typeof WebAssembly === "object" && typeof WebAssembly.Table === "function") {
				if (MAX_TABLE_SIZE !== undefined) {
					env["table"] = new WebAssembly.Table({
						"initial": TABLE_SIZE,
						"maximum": MAX_TABLE_SIZE,
						"element": "anyfunc"
					})
				} else {
					env["table"] = new WebAssembly.Table({
						"initial": TABLE_SIZE,
						element: "anyfunc"
					})
				}
			} else {
				env["table"] = new Array(TABLE_SIZE)
			}
			Module["wasmTable"] = env["table"]
		}
		if (!env["memoryBase"]) {
			env["memoryBase"] = Module["STATIC_BASE"]
		}
		if (!env["tableBase"]) {
			env["tableBase"] = 0
		}
		var exports;
		exports = doNativeWasm(global, env, providedBuffer);
		if (!exports) abort("no binaryen method succeeded. consider enabling more options, like interpreting, if you want that: https://github.com/kripken/emscripten/wiki/WebAssembly#binaryen-methods");
		return exports
	})
}
integrateWasmJS();
STATIC_BASE = GLOBAL_BASE;
STATICTOP = STATIC_BASE + 1536;
__ATINIT__.push();
var STATIC_BUMP = 1536;
Module["STATIC_BASE"] = STATIC_BASE;
Module["STATIC_BUMP"] = STATIC_BUMP;
STATICTOP += 16;
DYNAMICTOP_PTR = staticAlloc(4);
STACK_BASE = STACKTOP = alignMemory(STATICTOP);
STACK_MAX = STACK_BASE + TOTAL_STACK;
DYNAMIC_BASE = alignMemory(STACK_MAX);
HEAP32[DYNAMICTOP_PTR >> 2] = DYNAMIC_BASE;
staticSealed = true;
Module["wasmTableSize"] = 0;
Module["wasmMaxTableSize"] = 0;
Module.asmGlobalArg = {};
Module.asmLibraryArg = {
	"STACKTOP": STACKTOP
};
var asm = Module["asm"](Module.asmGlobalArg, Module.asmLibraryArg, buffer);
Module["asm"] = asm;
var stackAlloc = Module["stackAlloc"] = (function() {
	return Module["asm"]["stackAlloc"].apply(null, arguments)
});
var stackRestore = Module["stackRestore"] = (function() {
	return Module["asm"]["stackRestore"].apply(null, arguments)
});
var stackSave = Module["stackSave"] = (function() {
	return Module["asm"]["stackSave"].apply(null, arguments)
});
Module["asm"] = asm;
Module["cwrap"] = cwrap;

function ExitStatus(status) {
	this.name = "ExitStatus";
	this.message = "Program terminated with exit(" + status + ")";
	this.status = status
}
ExitStatus.prototype = new Error;
ExitStatus.prototype.constructor = ExitStatus;
var initialStackTop;
dependenciesFulfilled = function runCaller() {
	if (!Module["calledRun"]) run();
	if (!Module["calledRun"]) dependenciesFulfilled = runCaller
};

function run(args) {
	args = args || Module["arguments"];
	if (runDependencies > 0) {
		return
	}
	preRun();
	if (runDependencies > 0) return;
	if (Module["calledRun"]) return;

	function doRun() {
		if (Module["calledRun"]) return;
		Module["calledRun"] = true;
		if (ABORT) return;
		ensureInitRuntime();
		preMain();
		if (Module["onRuntimeInitialized"]) Module["onRuntimeInitialized"]();
		postRun()
	}
	if (Module["setStatus"]) {
		Module["setStatus"]("Running...");
		setTimeout((function() {
			setTimeout((function() {
				Module["setStatus"]("")
			}), 1);
			doRun()
		}), 1)
	} else {
		doRun()
	}
}
Module["run"] = run;

function exit(status, implicit) {
	if (implicit && Module["noExitRuntime"] && status === 0) {
		return
	}
	if (Module["noExitRuntime"]) {} else {
		ABORT = true;
		EXITSTATUS = status;
		STACKTOP = initialStackTop;
		exitRuntime();
		if (Module["onExit"]) Module["onExit"](status)
	}
	if (ENVIRONMENT_IS_NODE) {
		process["exit"](status)
	}
	Module["quit"](status, new ExitStatus(status))
}
Module["exit"] = exit;

function abort(what) {
	if (Module["onAbort"]) {
		Module["onAbort"](what)
	}
	if (what !== undefined) {
		Module.print(what);
		Module.printErr(what);
		what = JSON.stringify(what)
	} else {
		what = ""
	}
	ABORT = true;
	EXITSTATUS = 1;
	throw "abort(" + what + "). Build with -s ASSERTIONS=1 for more info."
}
Module["abort"] = abort;
if (Module["preInit"]) {
	if (typeof Module["preInit"] == "function") Module["preInit"] = [Module["preInit"]];
	while (Module["preInit"].length > 0) {
		Module["preInit"].pop()()
	}
}
Module["noExitRuntime"] = true;
run()
