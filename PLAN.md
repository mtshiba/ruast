# ruast ← syn API Alignment Plan

## Priority 1: Correctness Bugs (runtime panics on valid Rust code)

### 1a. `extern fn` without explicit ABI string panics (`.unwrap()`)
- **Location**: stmt.rs:1214, stmt.rs:3498, stmt.rs:4283, ty.rs:307
- `syn::Abi.name` is `None` for `extern fn foo()` (defaults to "C")
- Fix: `.unwrap()` → `.map(|n| n.value()).unwrap_or_else(|| "C".to_string())`

### 1b. `syn::Expr::Binary` with compound assign ops (`+=`, etc.) panics
- **Location**: expr.rs:2774-2783
- syn represents `a += b` as `ExprBinary { op: BinOp::AddAssign, .. }`, not a separate type
- The current `From<syn::BinOp> for BinOpKind` panics on assign ops
- Fix: In `From<syn::Expr>`, detect assign ops in Binary and produce `AssignOp` instead

### 1c. `GenericArgument::AssocType` hits `unreachable!()`
- **Location**: expr.rs:3890
- `Iterator<Item = u32>` produces `GenericArgument::AssocType` in syn
- Fix: Add `GenericArg::AssocType { ident, ty }` and `GenericArg::AssocConst { ident, value }` variants

### 1d. Missing `Expr::Async`, `Expr::Const`, `Expr::Infer` in From<syn::Expr>
- **Location**: expr.rs:816 (catch-all `unimplemented!()`)
- `Async`, `ConstBlock`, `Underscore` already exist in ruast but lack syn conversion arms
- Fix: Add match arms for these three

### 1e. Break/Continue label inconsistency
- **Location**: expr.rs:3788 vs expr.rs:3843
- `Break` uses `.to_string()` (includes `'`), `Continue` uses `.ident.to_string()` (no `'`)
- Fix: Unify to use `.ident.to_string()` (labels stored without `'`)

## Priority 2: Data Loss on Common Rust Patterns

### 2a. Loop labels dropped on `While`, `ForLoop`, `Loop`
- `'outer: while ...`, `'outer: for ...`, `'outer: loop ...` lose labels
- Fix: Add `label: Option<String>` to While, ForLoop, Loop structs

### 2b. `let-else` divergent block dropped
- **Location**: stmt.rs:244
- `let Some(x) = y else { return; }` loses the else block
- `LocalKind::InitElse` already exists but is never populated from syn
- Fix: Check `init.diverge` and produce `InitElse`

### 2c. `IdentPat` missing `is_ref` field
- `ref x`, `ref mut x` patterns lose the `ref` qualifier
- Fix: Add `is_ref: bool` to IdentPat

### 2d. MethodCall turbofish dropped
- `iter.collect::<Vec<_>>()` loses `::<Vec<_>>`
- Fix: Use `value.turbofish` when constructing the PathSegment

### 2e. `Visibility::Restricted` with complex path falls back to `pub(crate)`
- `pub(in crate::module)` → `pub(crate)`, which is wrong
- Fix: Handle `VisibilityScope::Path` properly

### 2f. `Impl` where clauses hardcoded to `None`
- Fix: Convert `value.generics.where_clause`

### 2g. Where clauses missing from Fn, StructDef, EnumDef, TraitDef, UnionDef
- Fix: Add `where_clauses: Option<Vec<WherePredicate>>` field to these types

### 2h. StructPat missing `..` rest pattern
- `Struct { field, .. }` loses the `..`
- Fix: Add `has_rest: bool` to StructPat

### 2i. Variant and FieldDef attrs dropped in syn conversion
- `#[serde(rename = "...")]` on fields/variants lost
- Fix: Convert `value.attrs` in From<syn::Variant> and From<syn::Field>

### 2j. Impl drops negative trait impl polarity
- `impl !Send for Foo {}` loses `!`
- Fix: Add `is_negative: bool` to Impl

### 2k. TraitDef::supertraits drops lifetime bounds
- `trait Foo: 'static` loses `'static`
- Fix: Change `supertraits` from `Vec<Type>` to `Vec<GenericBound>`

### 2l. Path missing leading `::` (global path)
- `::std::collections::HashMap` → `std::collections::HashMap`
- Fix: Add `is_global: bool` to Path

### 2m. TypeParam/ConstParam missing `default` value
- `T = i32` and `const N: usize = 0` defaults lost
- Fix: Add `default: Option<Type>` to TypeParam, `default: Option<Expr>` to ConstParam

### 2n. GenericParam::Lifetime missing bounds
- `'a: 'b` loses the bounds
- Fix: Change to `Lifetime { name: String, bounds: Vec<String> }` or similar

### 2o. Async block missing `move` keyword
- `async move { ... }` loses `move`
- Fix: Add `is_move: bool` to Async

## Priority 3: API Naming Alignment

### 3a. `lhs`/`rhs` → `left`/`right` in Binary, Assign, AssignOp
- Align with syn's naming

### 3b. `MethodCall::seg` → `MethodCall::method`
- Align with syn's naming

### 3c. `ExprField` → `FieldValue` (struct expression field)
- `ExprField { ident, expr }` is confusable with `Field` (field access)
- syn calls this `FieldValue`

## Priority 4: Structural Improvements

### 4a. Remove `Variant::vis` field
- Enum variants cannot have visibility in Rust
- Always hardcoded to `Inherited`

### 4b. Unify Fn conversion to use `fn_from_signature` helper
- Three near-identical Fn conversion paths exist
- Consolidate to prevent drift

## Non-changes (justified ruast design decisions)
- `LazyAnd`/`LazyOr` (more precise than syn's `And`/`Or`)
- `StructDef`/`EnumDef` etc. `*Def` naming (clear definition vs syn's `Item*`)
- `MacCall` naming (clearer than syn's `Macro`)
- `FieldDef` naming (avoids collision with field-access expression)
- `If::then`/`else_` (shorter than syn's `then_branch`/`else_branch`)
- `Pat::Box` (unstable feature support)
- `Closure::is_static` (keep for now)
- `LabelledBlock` as separate type
- `FnDecl` factored out of `Fn`
- `Const(Expr)` wrapper type
- `Semi`/`Empty` stmt variants
- `Type::ImplicitSelf`
- `Attribute`/`AttrKind` wrapper (doc comments as first-class)
- Generics flattened to `Vec<GenericParam>` (builder-friendly)
