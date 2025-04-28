/*

How to tackle this?

Ideas:
- Focus on data representation: Lua and Rust data structures should be equivalent
- Gradually replace functions and data structures with Rust code

Notes:
- https://crates.io/crates/mlua
- https://github.com/mlua-rs/mlua/blob/main/examples/module/src/lib.rs

Questions:
- How to expose as a Lua library?
- How to map Lua data structures to Rust data structures?


Something to start with:
- try to formalize all the name mangling variations
- wrap it in a Lua plugin so it can be integrated
- find a way to translate the Lua tables


*/
extern crate mlua;
use mlua::prelude::*;

fn sum(_: &Lua, (a, b): (i64, i64)) -> LuaResult<i64> {
    Ok(a + b)
}

fn used_memory(lua: &Lua, _: ()) -> LuaResult<usize> {
    Ok(lua.used_memory())
}

#[mlua::lua_module]
fn dataflow_rs(lua: &Lua) -> LuaResult<LuaTable> {
    let exports = lua.create_table()?;
    exports.set("sum", lua.create_function(sum)?)?;
    exports.set("used_memory", lua.create_function(used_memory)?)?;
    Ok(exports)
}
