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


Another idea is to completely fix the api: only the patch level is
lua, all the rest is Rust.  This would allow type checking already at
that level, as bad types are not representable in the rust side.  But
it would need userdata. https://docs.rs/mlua/latest/mlua/#custom-userdata


EDIT: I am starting directly with the "user interface", which is the
dsl exposed to lua.


Note: this is by itself ... because the state is completely abstract
and just passed around as a mut reference, and node references can
just be copied, i.e. no borrow nonsense.


*/
#![allow(unused)]
extern crate mlua;
use std::u32;

use mlua::prelude::*;
use mlua::{Error, MetaMethod, Table, UserData, UserDataFields, UserDataMethods, Value};

// See test.lua : two objects are needed.  One for node, one for
// compiler, and a collection of primitives are needed.  The
// primitives can be methods of compiler.

// I do want to separate the pure Rust model and the Lua interface:
// The DSL should work in Rust as well.

#[derive(Debug, Copy, Clone)]
struct Node(usize);
#[derive(Debug, Clone)]
struct Compiler {
    code: Vec<Syntax>,
}
#[derive(Debug, Clone, Copy)]
enum Syntax {
    Add(Node, Node),
    Inc(Node),
    Lit(u32),
    Input(u32),
}
impl Compiler {
    fn node(&mut self, stx: Syntax) -> Node {
        let n = self.code.len();
        self.code.push(stx);
        Node(n)
    }

    fn add(&mut self, a: Node, b: Node) -> Node {
        self.node(Syntax::Add(a, b))
    }
    fn inc(&mut self, a: Node) -> Node {
        self.node(Syntax::Inc(a))
    }
    fn input(&mut self, i: u32) -> Node {
        self.node(Syntax::Input(i))
    }
}

fn test_compiler() {
    let mut c = Compiler { code: Vec::new() };
    let i1 = c.input(1);
    let i2 = c.input(2);
    let o = c.add(i1, i2);
    println!("{:#?}", c);
}

// Lua interface below

impl UserData for Node {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("id", |_, this| Ok(this.0));
    }
}
// Implementation of IntoLua is automatically provided for UserData,
// FromLua needs to be implemented manually.
impl FromLua for Node {
    fn from_lua(value: Value, lua: &Lua) -> Result<Node, LuaError> {
        match value {
            // I copied this from example.  It's a type sandwich...
            Value::UserData(ud) => Ok(*ud.borrow::<Self>()?),
            _ => unreachable!(),
        }
    }
}

// This is just for letting Lua code read a Lua representation of the
// syntax.  The Lua->Rust is via compiler methods only, no Lua data
// structure to Rust object conversion.

impl IntoLua for Syntax {
    fn into_lua(self, lua: &Lua) -> LuaResult<Value> {
        Ok(match self {
            Syntax::Input(i) => {
                let t = lua.create_table()?;
                t.push("input".into_lua(lua)?);
                t.push(Value::Number(f64::from(i)));
                Value::Table(t)
            }
            Syntax::Add(Node(a), Node(b)) => {
                let t = lua.create_table()?;
                t.push("add".into_lua(lua)?);
                t.push(a);
                t.push(b);
                Value::Table(t)
            }
            Syntax::Inc(Node(a)) => {
                let t = lua.create_table()?;
                t.push("inc".into_lua(lua)?);
                t.push(a);
                Value::Table(t)
            }
            Syntax::Lit(num) => Value::Number(f64::from(num)),
        })
    }
}

impl UserData for Compiler {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("code", |_, this| Ok(this.code.clone()));
    }
    // All DSL operations will be methods taking a set of nodes and producing a node.
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method_mut(
            "add",
            |_, mut this, (a, b): (Node, Node)| Ok(this.add(a, b)),
        );
        methods.add_method_mut("inc", |_, mut this, a: Node| Ok(this.inc(a)));
    }
}

fn test_internal(_: &Lua, _: ()) -> LuaResult<()> {
    test_compiler();
    Ok(())
}

fn sum(_: &Lua, (a, b): (i64, i64)) -> LuaResult<i64> {
    Ok(a + b)
}

fn used_memory(lua: &Lua, _: ()) -> LuaResult<usize> {
    Ok(lua.used_memory())
}
// https://docs.rs/mlua/latest/mlua/trait.UserData.html
struct MyUserData(i32);
impl UserData for MyUserData {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("val", |_, this| Ok(this.0));
    }
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method_mut("add", |_, mut this, value: i32| {
            this.0 += value;
            Ok(())
        });

        methods.add_meta_method(MetaMethod::Add, |_, this, value: i32| Ok(this.0 + value));
    }
}

#[mlua::lua_module]
fn dataflow_rs(lua: &Lua) -> LuaResult<LuaTable> {
    let exports = lua.create_table()?;
    exports.set("test_internal", lua.create_function(test_internal)?)?;
    exports.set("sum", lua.create_function(sum)?)?;
    exports.set("used_memory", lua.create_function(used_memory)?)?;
    exports.set("myobject", MyUserData(123))?;

    exports.set("input123", Syntax::Input(123))?;
    exports.set("add", Syntax::Add(Node(1), Node(2)));

    Ok(exports)
}
