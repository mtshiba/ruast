//! Property-based tests for Display and TokenStream correctness.
//!
//! Uses `arbtest` to generate random ASTs via the existing `Arbitrary` derives,
//! then checks that Display / TokenStream output is valid Rust parseable by `syn`.
//!
//! Run with: cargo test -p ruast --test test_properties --features "fuzzing,tokenize,syn" -- --ignored

use arbitrary::Arbitrary;
use ruast::*;

// ===========================================================================
// AST-based validators
//
// The AST model intentionally allows combinations that are not valid Rust
// (it's designed for flexible code generation, not for constraining to only
// valid programs). Rather than restricting the Arbitrary impls — which are
// used for fuzzing and intentionally explore a wide space — we reject known-
// invalid ASTs *before* testing Display/TokenStream output.
//
// Each validator recursively walks the AST, returning `true` when the tree
// contains a structural pattern that cannot represent valid Rust.
// ===========================================================================

/// ABI strings accepted by the Rust compiler.
const KNOWN_ABIS: &[&str] = &[
    "C",
    "Rust",
    "system",
    "cdecl",
    "stdcall",
    "win64",
    "sysv64",
    "aapcs",
    "fastcall",
    "C-unwind",
    "Rust-unwind",
    "system-unwind",
];

/// Reserved keywords that random identifier generation may produce.
const RESERVED_KEYWORDS: &[&str] = &[
    "do", "box", "priv", "final", "abstract", "become", "override", "virtual",
    "typeof", "unsized",
];

fn is_valid_abi(abi: &str) -> bool {
    KNOWN_ABIS.contains(&abi)
}

fn is_reserved_keyword(ident: &str) -> bool {
    RESERVED_KEYWORDS.contains(&ident)
}

/// Check if a Display string contains a reserved keyword as a standalone token.
fn contains_reserved_keyword_token(src: &str) -> bool {
    for kw in RESERVED_KEYWORDS {
        for (pos, _) in src.match_indices(kw) {
            let before_ok = pos == 0 || {
                let b = src.as_bytes()[pos - 1];
                !b.is_ascii_alphanumeric() && b != b'_'
            };
            let end = pos + kw.len();
            let after_ok = end >= src.len() || {
                let b = src.as_bytes()[end];
                !b.is_ascii_alphanumeric() && b != b'_'
            };
            if before_ok && after_ok {
                return true;
            }
        }
    }
    false
}

// ---------------------------------------------------------------------------
// Expr validator
// ---------------------------------------------------------------------------

fn should_skip_expr(expr: &Expr) -> bool {
    // Outer attributes on expressions are only valid at statement level
    if !expr.attrs.is_empty() {
        return true;
    }
    should_skip_expr_kind(&expr.kind)
}

fn should_skip_expr_kind(kind: &ExprKind) -> bool {
    match kind {
        // D: Rust has no `static` closures
        ExprKind::Closure(c) => {
            if c.is_static {
                return true;
            }
            if should_skip_fn_decl(&c.fn_decl) {
                return true;
            }
            should_skip_expr(&c.body)
        }
        // C: Macro bodies contain random tokens
        ExprKind::MacCall(_) => true,
        // const blocks are unstable (syn rejects)
        ExprKind::ConstBlock(_) => true,
        // yield is unstable
        ExprKind::Yield(_) => true,
        // Struct literals are ambiguous in many positions (if/while/for/match
        // conditions, after return/break). Rather than tracking context we
        // reject them entirely — they are a small fraction of the space.
        ExprKind::Struct(_) => true,

        // Recurse into sub-expressions
        ExprKind::Array(a) => a.0.iter().any(should_skip_expr),
        ExprKind::Call(c) => {
            should_skip_expr(&c.func) || c.args.iter().any(should_skip_expr)
        }
        ExprKind::MethodCall(mc) => {
            should_skip_expr(&mc.receiver)
                || mc.args.iter().any(should_skip_expr)
                || should_skip_path_segment(&mc.method)
        }
        ExprKind::Tuple(t) => t.0.iter().any(should_skip_expr),
        ExprKind::Binary(b) => should_skip_expr(&b.left) || should_skip_expr(&b.right),
        ExprKind::Unary(u) => should_skip_expr(&u.expr),
        ExprKind::Lit(_) | ExprKind::Underscore(_) => false,
        ExprKind::Cast(c) => {
            // impl Trait / dyn Trait are invalid as cast target types
            if matches!(&c.ty, Type::ImplTrait(_) | Type::TraitObject(_)) {
                return true;
            }
            should_skip_expr(&c.expr) || should_skip_type(&c.ty)
        }
        ExprKind::TypeAscription(ta) => {
            should_skip_expr(&ta.expr) || should_skip_type(&ta.ty)
        }
        ExprKind::Let(l) => {
            should_skip_pat(&l.pat) || should_skip_expr(&l.expr)
        }
        ExprKind::If(i) => {
            should_skip_expr(&i.cond)
                || contains_control_flow(&i.cond)
                || should_skip_block(&i.then)
                || i.else_.as_ref().map_or(false, |e| should_skip_expr(e))
        }
        ExprKind::While(w) => {
            should_skip_expr(&w.cond)
                || contains_control_flow(&w.cond)
                || should_skip_block(&w.body)
        }
        ExprKind::ForLoop(fl) => {
            should_skip_pat(&fl.pat)
                || should_skip_expr(&fl.expr)
                || contains_control_flow(&fl.expr)
                || should_skip_block(&fl.body)
        }
        ExprKind::Loop(l) => should_skip_block(&l.body),
        ExprKind::UnsafeBlock(ub) => should_skip_block(&ub.block),
        ExprKind::Match(m) => {
            should_skip_expr(&m.expr)
                || m.arms.iter().any(|arm| {
                    !arm.attrs.is_empty()
                        || should_skip_pat(&arm.pat)
                        || arm.guard.as_ref().map_or(false, |g| should_skip_expr(g))
                        || should_skip_expr(&arm.body)
                })
        }
        ExprKind::LabelledBlock(lb) => should_skip_block(&lb.block),
        ExprKind::Async(a) => should_skip_block(&a.block),
        ExprKind::Await(a) => should_skip_expr(&a.expr),
        ExprKind::TryBlock(tb) => should_skip_block(&tb.block),
        ExprKind::Assign(a) => {
            should_skip_expr(&a.left) || should_skip_expr(&a.right)
        }
        ExprKind::AssignOp(a) => {
            should_skip_expr(&a.left) || should_skip_expr(&a.right)
        }
        ExprKind::Field(f) => {
            should_skip_expr(&f.expr) || is_reserved_keyword(&f.ident)
        }
        ExprKind::Index(i) => {
            should_skip_expr(&i.expr) || should_skip_expr(&i.index)
        }
        ExprKind::Range(r) => {
            // Range expressions with block-like sub-expressions cause
            // parsing ambiguity (precedence, block boundaries, etc.)
            r.start.as_ref().map_or(false, |e| {
                should_skip_expr(e) || contains_block_expr(e)
            }) || r.end.as_ref().map_or(false, |e| {
                should_skip_expr(e) || contains_block_expr(e)
            })
        }
        ExprKind::Path(p) => should_skip_path(p),
        ExprKind::AddrOf(a) => should_skip_expr(&a.expr),
        ExprKind::Break(b) => {
            // break with a value that is a labeled expression creates ambiguity:
            // `break 'label: loop { }` — is 'label the break target or a loop label?
            if let Some(e) = &b.expr {
                if should_skip_expr(e) || has_label(&e.kind) || contains_block_expr(e) {
                    return true;
                }
            }
            false
        }
        ExprKind::Continue(_) => false,
        ExprKind::Return(r) => {
            r.expr.as_ref().map_or(false, |e| should_skip_expr(e))
        }
        ExprKind::Repeat(r) => {
            should_skip_expr(&r.expr) || should_skip_expr(&r.len.0)
        }
        ExprKind::Try(t) => should_skip_expr(&t.expr),
        ExprKind::Paren(p) => should_skip_expr(&p.0),
    }
}

/// Returns true if an expression kind is a labeled construct.
fn has_label(kind: &ExprKind) -> bool {
    matches!(kind,
        ExprKind::While(w) if w.label.is_some()
    ) || matches!(kind,
        ExprKind::ForLoop(fl) if fl.label.is_some()
    ) || matches!(kind,
        ExprKind::Loop(l) if l.label.is_some()
    ) || matches!(kind,
        ExprKind::LabelledBlock(lb) if lb.label.is_some()
    )
}

/// Returns true if expression contains break/continue/return/yield
/// at any nesting depth. These are invalid in condition positions
/// (if/while/for/match scrutinee).
fn contains_control_flow(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Break(_) | ExprKind::Continue(_) | ExprKind::Return(_)
        | ExprKind::Yield(_) => true,
        ExprKind::Paren(p) => contains_control_flow(&p.0),
        ExprKind::Cast(c) => contains_control_flow(&c.expr),
        ExprKind::TypeAscription(ta) => contains_control_flow(&ta.expr),
        ExprKind::Binary(b) => {
            contains_control_flow(&b.left) || contains_control_flow(&b.right)
        }
        ExprKind::Unary(u) => contains_control_flow(&u.expr),
        ExprKind::AddrOf(a) => contains_control_flow(&a.expr),
        ExprKind::Assign(a) => {
            contains_control_flow(&a.left) || contains_control_flow(&a.right)
        }
        ExprKind::AssignOp(a) => {
            contains_control_flow(&a.left) || contains_control_flow(&a.right)
        }
        ExprKind::Range(r) => {
            r.start.as_ref().map_or(false, |e| contains_control_flow(e))
                || r.end.as_ref().map_or(false, |e| contains_control_flow(e))
        }
        ExprKind::Try(t) => contains_control_flow(&t.expr),
        ExprKind::Tuple(t) => t.0.iter().any(contains_control_flow),
        ExprKind::Array(a) => a.0.iter().any(contains_control_flow),
        ExprKind::Index(i) => {
            contains_control_flow(&i.expr) || contains_control_flow(&i.index)
        }
        ExprKind::Field(f) => contains_control_flow(&f.expr),
        ExprKind::Await(a) => contains_control_flow(&a.expr),
        ExprKind::Call(c) => {
            contains_control_flow(&c.func) || c.args.iter().any(contains_control_flow)
        }
        ExprKind::MethodCall(mc) => {
            contains_control_flow(&mc.receiver)
                || mc.args.iter().any(contains_control_flow)
        }
        ExprKind::Let(l) => contains_control_flow(&l.expr),
        // Block-like expressions create their own context
        _ => false,
    }
}

/// Returns true if an expression contains block-like sub-expressions
/// (for, while, loop, if, match, unsafe, async, try, closure, struct
/// literals) that cause parsing ambiguity in certain positions like
/// range expressions or match scrutinees.
fn contains_block_expr(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::If(_) | ExprKind::While(_) | ExprKind::ForLoop(_)
        | ExprKind::Loop(_) | ExprKind::Match(_) | ExprKind::UnsafeBlock(_)
        | ExprKind::Async(_) | ExprKind::TryBlock(_) | ExprKind::ConstBlock(_)
        | ExprKind::LabelledBlock(_) | ExprKind::Closure(_)
        | ExprKind::Struct(_) | ExprKind::Break(_) | ExprKind::Continue(_)
        | ExprKind::Return(_) | ExprKind::Yield(_) => true,
        ExprKind::Paren(p) => contains_block_expr(&p.0),
        ExprKind::Cast(c) => contains_block_expr(&c.expr),
        ExprKind::TypeAscription(ta) => contains_block_expr(&ta.expr),
        ExprKind::Binary(b) => {
            contains_block_expr(&b.left) || contains_block_expr(&b.right)
        }
        ExprKind::Unary(u) => contains_block_expr(&u.expr),
        ExprKind::AddrOf(a) => contains_block_expr(&a.expr),
        ExprKind::Assign(a) => {
            contains_block_expr(&a.left) || contains_block_expr(&a.right)
        }
        ExprKind::AssignOp(a) => {
            contains_block_expr(&a.left) || contains_block_expr(&a.right)
        }
        ExprKind::Try(t) => contains_block_expr(&t.expr),
        ExprKind::Field(f) => contains_block_expr(&f.expr),
        ExprKind::Await(a) => contains_block_expr(&a.expr),
        ExprKind::Index(i) => {
            contains_block_expr(&i.expr) || contains_block_expr(&i.index)
        }
        ExprKind::Call(c) => {
            contains_block_expr(&c.func) || c.args.iter().any(contains_block_expr)
        }
        ExprKind::MethodCall(mc) => {
            contains_block_expr(&mc.receiver)
                || mc.args.iter().any(contains_block_expr)
        }
        ExprKind::Tuple(t) => t.0.iter().any(contains_block_expr),
        ExprKind::Array(a) => a.0.iter().any(contains_block_expr),
        ExprKind::Range(r) => {
            r.start.as_ref().map_or(false, |e| contains_block_expr(e))
                || r.end.as_ref().map_or(false, |e| contains_block_expr(e))
        }
        ExprKind::Let(l) => contains_block_expr(&l.expr),
        _ => false,
    }
}

// ---------------------------------------------------------------------------
// Type validator
// ---------------------------------------------------------------------------

fn should_skip_type(ty: &Type) -> bool {
    match ty {
        // C: Macro types with random tokens
        Type::Macro(_) => true,
        // Bare trait objects (without `dyn`) are ambiguous
        Type::TraitObject(to) => {
            if !to.is_dyn {
                return true;
            }
            to.bounds.iter().any(should_skip_generic_bound)
        }
        // `impl Trait` in nested type positions (arrays, tuples, slices)
        // is invalid. We allow it at top level only; since it can appear
        // nested via random generation we check children below.
        Type::ImplTrait(it) => it.bounds.iter().any(should_skip_generic_bound),

        Type::Slice(inner) => should_skip_type_non_impl_dyn(inner),
        Type::Array(inner, len) => {
            should_skip_type_non_impl_dyn(inner) || should_skip_expr(&len.0)
        }
        Type::Ptr(p) => should_skip_type_non_impl_dyn(&p.ty),
        Type::Ref(r) => should_skip_type_non_impl_dyn(&r.ty.ty),
        Type::BareFn(bf) => should_skip_bare_fn(bf),
        Type::Tuple(tys) => tys.iter().any(|t| should_skip_type_non_impl_dyn(t)),
        Type::Path(p) => should_skip_path(p),
        Type::Never | Type::Infer | Type::ImplicitSelf | Type::Err => false,
    }
}

/// Like `should_skip_type` but also rejects `impl Trait` and `dyn Trait`
/// which are invalid in nested positions (inside `[]`, `()`, `*`, `&`).
fn should_skip_type_non_impl_dyn(ty: &Type) -> bool {
    matches!(ty, Type::ImplTrait(_) | Type::TraitObject(_)) || should_skip_type(ty)
}

fn should_skip_bare_fn(bf: &BareFn) -> bool {
    if let Some(abi) = &bf.abi {
        if !is_valid_abi(abi) {
            return true;
        }
    }
    // for<...> only supports simple lifetime params (no type params, no bounds)
    if should_skip_higher_ranked_params(&bf.generic_params) {
        return true;
    }
    // BareFnArg fields are private — check via Display string.
    // This catches reserved keywords, impl/dyn trait in args, macros, etc.
    if bf.inputs.iter().any(|arg| {
        let s = arg.to_string();
        should_skip_bare_fn_arg_str(&s)
    }) {
        return true;
    }
    // Return type: `dyn Trait + bounds` or `impl Trait + bounds` as return
    // of a fn pointer causes `+` ambiguity when the fn ptr appears inside
    // `[]`, `()`, or other contexts.
    if let Some(out) = &bf.output {
        if matches!(out.as_ref(), Type::ImplTrait(_) | Type::TraitObject(_)) {
            return true;
        }
        if should_skip_type(out) {
            return true;
        }
    }
    false
}

/// String-based check for BareFnArg content (fields are private).
fn should_skip_bare_fn_arg_str(src: &str) -> bool {
    if contains_reserved_keyword_token(src) {
        return true;
    }
    // impl Trait / dyn Trait in fn pointer args
    if src.contains("impl ") || src.contains("dyn ") {
        return true;
    }
    // Macro types
    if src.contains("!(") || src.contains("![") || src.contains("!{") {
        return true;
    }
    // for<...> in nested positions
    if src.contains("for<") || src.contains("for <") {
        return true;
    }
    false
}

// ---------------------------------------------------------------------------
// Generic parameter & path validators
// ---------------------------------------------------------------------------

fn should_skip_generic_params(params: &[GenericParam]) -> bool {
    params.iter().any(should_skip_generic_param)
}

fn should_skip_generic_param(param: &GenericParam) -> bool {
    match param {
        GenericParam::TypeParam(tp) => {
            if is_reserved_keyword(&tp.ident) {
                return true;
            }
            tp.bounds.iter().any(should_skip_generic_bound)
                || tp.default.as_ref().map_or(false, should_skip_type)
        }
        GenericParam::ConstParam(cp) => {
            if is_reserved_keyword(&cp.ident) {
                return true;
            }
            // B: const param type must not be impl Trait / dyn Trait
            if matches!(&cp.ty, Type::ImplTrait(_) | Type::TraitObject(_)) {
                return true;
            }
            if should_skip_type(&cp.ty) {
                return true;
            }
            // A: const param default must be a simple const expression
            // (literal, path, or block). Complex expressions are rejected.
            if let Some(default) = &cp.default {
                if should_skip_const_param_default(default) {
                    return true;
                }
            }
            false
        }
        GenericParam::Lifetime(_) => false,
    }
}

/// for<...> only supports simple lifetime params on stable Rust.
fn should_skip_higher_ranked_params(params: &[GenericParam]) -> bool {
    params.iter().any(|p| !matches!(p, GenericParam::Lifetime(lp) if lp.bounds.is_empty()))
}

fn should_skip_const_param_default(expr: &Expr) -> bool {
    if !expr.attrs.is_empty() {
        return true;
    }
    match &expr.kind {
        // Only literals, paths, and simple braced blocks are allowed
        ExprKind::Lit(_) => false,
        ExprKind::Path(p) => should_skip_path(p),
        ExprKind::LabelledBlock(lb) if lb.label.is_none() => {
            should_skip_block(&lb.block)
        }
        // Everything else is rejected as a const param default
        _ => true,
    }
}

fn should_skip_generic_bound(bound: &GenericBound) -> bool {
    match bound {
        GenericBound::Trait(poly) => {
            if should_skip_higher_ranked_params(&poly.bound_generic_params) {
                return true;
            }
            should_skip_path(&poly.trait_ref)
        }
        GenericBound::Outlives(_) => false,
    }
}

fn should_skip_generic_arg(arg: &GenericArg) -> bool {
    match arg {
        GenericArg::Lifetime(_) => false,
        GenericArg::Type(ty) => should_skip_type(ty),
        GenericArg::Const(c) => should_skip_expr(&c.0),
        GenericArg::AssocType { ident, ty } => {
            is_reserved_keyword(ident) || should_skip_type(ty)
        }
        GenericArg::AssocConst { ident, value } => {
            is_reserved_keyword(ident) || should_skip_expr(&value.0)
        }
    }
}

fn should_skip_path(path: &Path) -> bool {
    path.segments.iter().any(should_skip_path_segment)
}

fn should_skip_path_segment(seg: &PathSegment) -> bool {
    if is_reserved_keyword(&seg.ident) {
        return true;
    }
    seg.args
        .as_ref()
        .map_or(false, |args| args.iter().any(should_skip_generic_arg))
}

// ---------------------------------------------------------------------------
// Pattern validator
// ---------------------------------------------------------------------------

fn should_skip_pat(pat: &Pat) -> bool {
    match pat {
        Pat::MacCall(_) => true,
        Pat::Wild | Pat::Rest => false,
        Pat::Ident(ip) => {
            is_reserved_keyword(&ip.ident)
                || ip.pat.as_ref().map_or(false, |p| should_skip_pat(p))
        }
        Pat::Struct(sp) => {
            should_skip_path(&sp.path)
                || sp.fields.iter().any(|f| should_skip_pat(&f.pat))
        }
        Pat::TupleStruct(ts) => {
            should_skip_path(&ts.path) || ts.pats.iter().any(should_skip_pat)
        }
        Pat::Or(pats) | Pat::Tuple(pats) | Pat::Slice(pats) => {
            pats.iter().any(should_skip_pat)
        }
        Pat::Box(p) | Pat::Paren(p) => should_skip_pat(p),
        Pat::Ref(r) => should_skip_pat(&r.pat),
        Pat::Lit(e) => should_skip_expr(e),
        Pat::Range(r) => {
            r.start.as_ref().map_or(false, |e| should_skip_expr(e))
                || r.end.as_ref().map_or(false, |e| should_skip_expr(e))
        }
        Pat::Type(tp) => should_skip_pat(&tp.pat) || should_skip_type(&tp.ty),
    }
}

// ---------------------------------------------------------------------------
// Block / statement validators
// ---------------------------------------------------------------------------

fn should_skip_block(block: &Block) -> bool {
    block.stmts.iter().any(should_skip_stmt)
}

fn should_skip_stmt(stmt: &Stmt) -> bool {
    match stmt {
        // Items inside expression blocks cause Display/TS divergence
        Stmt::Item(_) => true,
        Stmt::MacCallWithSemi(_) => true,
        Stmt::Expr(e) => should_skip_expr(e),
        Stmt::Semi(semi) => should_skip_expr(&semi.0),
        Stmt::Local(local) => {
            should_skip_pat(&local.pat)
                || local.ty.as_ref().map_or(false, should_skip_type)
                || match &local.kind {
                    LocalKind::Decl => false,
                    LocalKind::Init(e) => should_skip_expr(e),
                    LocalKind::InitElse(e, b) => {
                        should_skip_expr(e) || should_skip_block(b)
                    }
                }
        }
        Stmt::Empty(_) => false,
    }
}

// ---------------------------------------------------------------------------
// FnDecl validator
// ---------------------------------------------------------------------------

fn should_skip_fn_decl(decl: &FnDecl) -> bool {
    decl.inputs.iter().any(|p| {
        should_skip_pat(&p.pat) || should_skip_type(&p.ty)
    }) || decl
        .output
        .as_ref()
        .map_or(false, should_skip_type)
}

// ---------------------------------------------------------------------------
// Item validator (for Crate-level items)
// ---------------------------------------------------------------------------

fn should_skip_item(item: &Item) -> bool {
    // Inner attributes on items are not valid at item level
    if has_inner_attr(&item.attrs) {
        return true;
    }
    // Visibility on extern blocks is not valid Rust
    if matches!(&item.kind, ItemKind::ExternBlock(_))
        && !matches!(&item.vis, Visibility::Inherited)
    {
        return true;
    }
    should_skip_item_kind(&item.kind)
}

fn should_skip_item_kind(kind: &ItemKind) -> bool {
    match kind {
        // C: Macros with random tokens
        ItemKind::MacCallWithSemi(_) | ItemKind::MacroDef(_) => true,

        // E: Unions only allow named fields
        ItemKind::UnionDef(ud) => {
            if !matches!(&ud.fields, Fields::Struct(_)) {
                return true;
            }
            if is_reserved_keyword(&ud.ident) {
                return true;
            }
            should_skip_generic_params(&ud.generics)
                || should_skip_where_clauses(&ud.where_clauses)
                || should_skip_fields(&ud.fields)
                || fields_have_inner_attrs(&ud.fields)
        }

        // F: Type alias without body is only valid inside traits
        ItemKind::TyAlias(ta) => {
            if ta.ty.is_none() {
                return true;
            }
            if is_reserved_keyword(&ta.ident) {
                return true;
            }
            should_skip_generic_params(&ta.generics)
                || ta.ty.as_ref().map_or(false, should_skip_type)
        }

        // I: Negative impls
        ItemKind::Impl(imp) => {
            if imp.is_negative {
                return true;
            }
            // `impl !` (Never type as self_ty) is unstable
            if matches!(&imp.self_ty, Type::Never) {
                return true;
            }
            // `impl Trait` / `dyn Trait` as self_ty is invalid
            if matches!(&imp.self_ty, Type::ImplTrait(_) | Type::TraitObject(_)) {
                return true;
            }
            should_skip_generic_params(&imp.generics)
                || imp.of_trait.as_ref().map_or(false, should_skip_type)
                || should_skip_type(&imp.self_ty)
                || should_skip_where_clauses(&imp.where_clauses)
                || imp.items.iter().any(should_skip_assoc_item)
        }

        // J: Extern blocks with non-standard ABI
        ItemKind::ExternBlock(eb) => {
            if let Some(abi) = &eb.abi {
                if !is_valid_abi(abi) {
                    return true;
                }
            }
            should_skip_block(&eb.block)
        }

        ItemKind::Fn(f) => should_skip_fn(f),

        ItemKind::StructDef(sd) => {
            if is_reserved_keyword(&sd.ident) {
                return true;
            }
            should_skip_generic_params(&sd.generics)
                || should_skip_where_clauses(&sd.where_clauses)
                || should_skip_fields(&sd.fields)
                || fields_have_inner_attrs(&sd.fields)
        }

        ItemKind::EnumDef(ed) => {
            if is_reserved_keyword(&ed.ident) {
                return true;
            }
            should_skip_generic_params(&ed.generics)
                || should_skip_where_clauses(&ed.where_clauses)
                || ed.variants.iter().any(|v| {
                    is_reserved_keyword(&v.ident)
                        || has_inner_attr(&v.attrs)
                        || should_skip_fields(&v.fields)
                        || fields_have_inner_attrs(&v.fields)
                        || v.discriminant.as_ref().map_or(false, should_skip_expr)
                })
        }

        ItemKind::TraitDef(td) => {
            if is_reserved_keyword(&td.ident) {
                return true;
            }
            should_skip_generic_params(&td.generics)
                || should_skip_where_clauses(&td.where_clauses)
                || td.supertraits.iter().any(should_skip_generic_bound)
                || td.items.iter().any(should_skip_assoc_item)
        }

        ItemKind::Use(_) | ItemKind::ExternCrate(_) => false,

        ItemKind::StaticItem(si) => {
            // Static items at top level must have an initializer
            if si.expr.is_none() {
                return true;
            }
            is_reserved_keyword(&si.ident)
                || should_skip_type(&si.ty)
                || si.expr.as_ref().map_or(false, should_skip_expr)
        }

        ItemKind::ConstItem(ci) => {
            // Const items at top level must have an initializer
            if ci.expr.is_none() {
                return true;
            }
            is_reserved_keyword(&ci.ident)
                || should_skip_type(&ci.ty)
                || ci.expr.as_ref().map_or(false, should_skip_expr)
        }

        ItemKind::Mod(m) => match m {
            Mod::Loaded(lm) => {
                is_reserved_keyword(&lm.ident) || lm.items.iter().any(should_skip_item)
            }
            Mod::Unloaded(ident) => is_reserved_keyword(ident),
        },
    }
}

fn should_skip_fn(f: &Fn) -> bool {
    if is_reserved_keyword(&f.ident) {
        return true;
    }
    if let Some(abi) = &f.abi {
        if !is_valid_abi(abi) {
            return true;
        }
    }
    should_skip_generic_params(&f.generics)
        || should_skip_fn_decl(&f.fn_decl)
        || should_skip_where_clauses(&f.where_clauses)
        || f.body.as_ref().map_or(false, should_skip_block)
}

fn should_skip_assoc_item(item: &AssocItem) -> bool {
    match &item.kind {
        AssocItemKind::Fn(f) => should_skip_fn(f),
        AssocItemKind::TyAlias(ta) => {
            is_reserved_keyword(&ta.ident)
                || should_skip_generic_params(&ta.generics)
                || ta.ty.as_ref().map_or(false, should_skip_type)
        }
        AssocItemKind::ConstItem(ci) => {
            is_reserved_keyword(&ci.ident)
                || should_skip_type(&ci.ty)
                || ci.expr.as_ref().map_or(false, should_skip_expr)
        }
        AssocItemKind::MacCall(_) => true,
    }
}

fn should_skip_fields(fields: &Fields) -> bool {
    match fields {
        Fields::Unit => false,
        Fields::Tuple(fds) => fds.iter().any(|fd| {
            // Tuple fields must NOT have identifiers
            fd.ident.is_some() || should_skip_type(&fd.ty)
        }),
        Fields::Struct(fds) => fds.iter().any(|fd| {
            // Named struct fields must have an identifier
            fd.ident.is_none()
                || fd.ident.as_ref().map_or(false, |i| is_reserved_keyword(i))
                || should_skip_type(&fd.ty)
        }),
    }
}

/// Returns true if any `Attribute` is an inner attribute (`#![...]`).
fn has_inner_attr(attrs: &[Attribute]) -> bool {
    attrs.iter().any(|attr| {
        matches!(&attr.kind, AttrKind::Normal(ai) if matches!(ai.style, AttrStyle::Inner))
    })
}

/// Returns true if any field/variant in the definition has an inner attribute.
fn fields_have_inner_attrs(fields: &Fields) -> bool {
    let fds = match fields {
        Fields::Unit => return false,
        Fields::Tuple(fds) | Fields::Struct(fds) => fds,
    };
    fds.iter().any(|fd| has_inner_attr(&fd.attrs))
}

fn should_skip_where_clauses(wc: &Option<Vec<WherePredicate>>) -> bool {
    wc.as_ref().map_or(false, |preds| {
        preds.iter().any(|pred| match pred {
            WherePredicate::Type(pt) => {
                should_skip_type(&pt.bounded_ty)
                    || pt.bounds.iter().any(|bound_ty| {
                        // Where clause type bounds must be trait paths.
                        // The AST model uses Vec<Type> but only Type::Path
                        // is valid as a trait bound in `where T: Path`.
                        if !matches!(bound_ty, Type::Path(_)) {
                            return true;
                        }
                        should_skip_type(bound_ty)
                    })
            }
            WherePredicate::Lifetime(_) => false,
        })
    })
}

// ---------------------------------------------------------------------------
// Crate validator
// ---------------------------------------------------------------------------

fn should_skip_crate(krate: &Crate) -> bool {
    // G: Empty crate (no items) — syn expects at least something
    if krate.items.is_empty() {
        return true;
    }
    krate.items.iter().any(should_skip_item)
}

// ---------------------------------------------------------------------------
// P1: Display → valid Rust
// ---------------------------------------------------------------------------

#[test]
#[ignore] // Run explicitly: cargo test -- --ignored
fn prop_expr_display_is_valid_rust() {
    arbtest::arbtest(|u| {
        ruast::depth_limiter::set(50);
        let expr = Expr::arbitrary(u)?;
        if should_skip_expr(&expr) {
            return Ok(());
        }
        let src = expr.to_string();
        if let Err(e) = syn::parse_str::<syn::Expr>(&src) {
            panic!("Expr Display output is not valid Rust:\n  src: {src}\n  err: {e}");
        }
        Ok(())
    });
}

#[test]
#[ignore]
fn prop_type_display_is_valid_rust() {
    arbtest::arbtest(|u| {
        ruast::depth_limiter::set(50);
        let ty = Type::arbitrary(u)?;
        if should_skip_type(&ty) {
            return Ok(());
        }
        let src = ty.to_string();
        if let Err(e) = syn::parse_str::<syn::Type>(&src) {
            panic!("Type Display output is not valid Rust:\n  src: {src}\n  err: {e}");
        }
        Ok(())
    });
}

#[test]
#[ignore]
fn prop_crate_display_is_valid_rust() {
    arbtest::arbtest(|u| {
        ruast::depth_limiter::set(50);
        let krate = Crate::arbitrary(u)?;
        if should_skip_crate(&krate) {
            return Ok(());
        }
        let src = krate.to_string();
        if let Err(e) = syn::parse_file(&src) {
            panic!("Crate Display output is not valid Rust:\n  src: {src}\n  err: {e}");
        }
        Ok(())
    });
}

// ---------------------------------------------------------------------------
// P2: TokenStream → valid Rust
// ---------------------------------------------------------------------------

#[test]
#[ignore]
fn prop_expr_tokenstream_is_valid_rust() {
    arbtest::arbtest(|u| {
        ruast::depth_limiter::set(50);
        let expr = Expr::arbitrary(u)?;
        if should_skip_expr(&expr) {
            return Ok(());
        }
        let ts = ruast::TokenStream::from(expr);
        let src = ts.to_string();
        if let Err(e) = syn::parse_str::<syn::Expr>(&src) {
            panic!("Expr TokenStream output is not valid Rust:\n  src: {src}\n  err: {e}");
        }
        Ok(())
    });
}

#[test]
#[ignore]
fn prop_type_tokenstream_is_valid_rust() {
    arbtest::arbtest(|u| {
        ruast::depth_limiter::set(50);
        let ty = Type::arbitrary(u)?;
        if should_skip_type(&ty) {
            return Ok(());
        }
        let ts = ruast::TokenStream::from(ty);
        let src = ts.to_string();
        if let Err(e) = syn::parse_str::<syn::Type>(&src) {
            panic!("Type TokenStream output is not valid Rust:\n  src: {src}\n  err: {e}");
        }
        Ok(())
    });
}

// ---------------------------------------------------------------------------
// P3: Display ↔ TokenStream semantic consistency
//
// Both paths should parse to equivalent syn ASTs.
// We normalize via `quote::quote!` to compare.
// If either side fails to parse, skip — P1/P2 tests catch those individually.
// ---------------------------------------------------------------------------

#[test]
#[ignore]
fn prop_expr_display_and_tokenstream_are_consistent() {
    arbtest::arbtest(|u| {
        ruast::depth_limiter::set(50);
        let expr = Expr::arbitrary(u)?;
        if should_skip_expr(&expr) {
            return Ok(());
        }
        let display_src = expr.to_string();
        let ts_src = ruast::TokenStream::from(expr).to_string();

        let display_parsed = syn::parse_str::<syn::Expr>(&display_src);
        let ts_parsed = syn::parse_str::<syn::Expr>(&ts_src);

        match (display_parsed, ts_parsed) {
            (Ok(d), Ok(t)) => {
                let d_norm = quote::quote!(#d).to_string();
                let t_norm = quote::quote!(#t).to_string();
                assert_eq!(
                    d_norm, t_norm,
                    "Display and TokenStream diverge:\n  display: {display_src}\n  tokens:  {ts_src}"
                );
            }
            // If either side fails to parse, skip — P1/P2 catch these
            _ => {}
        }
        Ok(())
    });
}

#[test]
#[ignore]
fn prop_type_display_and_tokenstream_are_consistent() {
    arbtest::arbtest(|u| {
        ruast::depth_limiter::set(50);
        let ty = Type::arbitrary(u)?;
        if should_skip_type(&ty) {
            return Ok(());
        }
        let display_src = ty.to_string();
        let ts_src = ruast::TokenStream::from(ty).to_string();

        let display_parsed = syn::parse_str::<syn::Type>(&display_src);
        let ts_parsed = syn::parse_str::<syn::Type>(&ts_src);

        match (display_parsed, ts_parsed) {
            (Ok(d), Ok(t)) => {
                let d_norm = quote::quote!(#d).to_string();
                let t_norm = quote::quote!(#t).to_string();
                assert_eq!(
                    d_norm, t_norm,
                    "Display and TokenStream diverge:\n  display: {display_src}\n  tokens:  {ts_src}"
                );
            }
            _ => {}
        }
        Ok(())
    });
}
