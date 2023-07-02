use std::fmt;

use crate::{impl_display_for_enum, impl_obvious_conversion, Pat, Block, FnDecl};
use crate::token::Token;
use crate::ty::Type;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Attribute(String);

impl fmt::Display for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#[{}]", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Expr {
    pub attrs: Vec<Attribute>,
    pub kind: ExprKind,
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for attr in self.attrs.iter() {
            writeln!(f, "{attr}")?;
        }
        self.kind.fmt(f)
    }
}

impl Expr {
    pub fn new(kind: impl Into<ExprKind>) -> Self {
        Self {
            attrs: Vec::new(),
            kind: kind.into(),
        }
    }

    pub fn add_attr(&mut self, attr: Attribute) {
        self.attrs.push(attr);
    }

    pub fn remove_attr(&mut self, attr: &Attribute) {
        self.attrs.retain(|a| a != attr);
    }

    pub fn is_compound(&self) -> bool {
        matches!(&self.kind, ExprKind::Binary(_) | ExprKind::Unary(_) | ExprKind::Field(_)| ExprKind::Range(_))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Const(Expr);

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Array(Vec<Expr>);

impl fmt::Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        let mut iter = self.0.iter();
        if let Some(expr) = iter.next() {
            write!(f, "{expr}")?;
            for expr in iter {
                write!(f, ", {expr}")?;
            }
        }
        write!(f, "]")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Tuple(Vec<Expr>);

impl fmt::Display for Tuple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        let mut iter = self.0.iter();
        if let Some(expr) = iter.next() {
            write!(f, "{expr}")?;
            for expr in iter {
                write!(f, ", {expr}")?;
            }
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub op: BinOpKind,
    pub rhs: Box<Expr>,
}

impl fmt::Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({lhs} {op} {rhs})", lhs = self.lhs, op = self.op, rhs = self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOpKind {
    Deref,
    Not,
    Neg,
}

impl fmt::Display for UnaryOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Deref => write!(f, "*"),
            Self::Not => write!(f, "!"),
            Self::Neg => write!(f, "-"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Unary {
    pub op: UnaryOpKind,
    pub expr: Box<Expr>,
}

impl fmt::Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({op} {expr})", op = self.op, expr = self.expr)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Let {
    pub pat: Box<Pat>,
    pub expr: Box<Expr>,
}

impl fmt::Display for Let {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {pat} = {expr}", pat = self.pat, expr = self.expr)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct If {
    pub cond: Box<Expr>,
    pub then: Block,
    pub else_: Option<Box<Expr>>,
}

impl fmt::Display for If {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "if {cond} {then}", cond = self.cond, then = self.then)?;
        if let Some(else_) = &self.else_ {
            write!(f, " else {{ {else_} }}", else_ = else_)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct While {
    pub cond: Box<Expr>,
    pub body: Block,
}

impl fmt::Display for While {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "while {cond} {body}", cond = self.cond, body = self.body)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ForLoop {
    pub pat: Box<Pat>,
    pub expr: Box<Expr>,
    pub body: Block,
}

impl fmt::Display for ForLoop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "for {pat} in {expr} {body}", pat = self.pat, expr = self.expr, body = self.body)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Loop {
    pub body: Block,
}

impl fmt::Display for Loop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "loop {body}", body = self.body)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Arm {
    pub attrs: Vec<Attribute>,
    pub pat: Box<Pat>,
    pub guard: Option<Box<Expr>>,
    pub body: Box<Expr>,
}

impl fmt::Display for Arm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for attr in self.attrs.iter() {
            writeln!(f, "{attr}")?;
        }
        write!(f, "{pat}", pat = self.pat)?;
        if let Some(guard) = &self.guard {
            write!(f, " if {guard}", guard = guard)?;
        }
        write!(f, " => {body}", body = self.body)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Match {
    pub expr: Box<Expr>,
    pub arms: Vec<Arm>,
}

impl fmt::Display for Match {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "match {expr} {{", expr = self.expr)?;
        for arm in self.arms.iter() {
            writeln!(f, "{arm},", arm = arm)?;
        }
        write!(f, "}}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Closure {
    pub is_const: bool,
    pub is_async: bool,
    pub is_move: bool,
    pub fn_decl: FnDecl,
    pub body: Box<Expr>,
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "|")?;
        let mut iter = self.fn_decl.inputs.iter();
        if let Some(input) = iter.next() {
            write!(f, "{input}", input = input)?;
            for input in iter {
                write!(f, ", {input}", input = input)?;
            }
        }
        write!(f, "| ")?;
        if let Some(output) = &self.fn_decl.output {
            write!(f, "-> {output} ", output = output)?;
        }
        write!(f, "{{ {} }}", self.body)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    pub expr: Box<Expr>,
    pub ident: String,
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{expr}.{ident}", expr = self.expr, ident = self.ident)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Index {
    pub expr: Box<Expr>,
    pub index: Box<Expr>,
}

impl fmt::Display for Index {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{expr}[{index}]", expr = self.expr, index = self.index)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RangeLimits {
    HalfOpen,
    Closed,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Range {
    pub start: Option<Box<Expr>>,
    pub end: Option<Box<Expr>>,
    pub op: RangeLimits,
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.op {
            RangeLimits::HalfOpen => {
                write!(f, "{start}..{end}", start = self.start.as_ref().map(|e| e.to_string()).unwrap_or_default(), end = self.end.as_ref().map(|e| e.to_string()).unwrap_or_default())
            },
            RangeLimits::Closed => {
                write!(f, "{start}..={end}", start = self.start.as_ref().map(|e| e.to_string()).unwrap_or_default(), end = self.end.as_ref().map(|e| e.to_string()).unwrap_or_default())
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Return {
    pub expr: Option<Box<Expr>>,
}

impl fmt::Display for Return {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(expr) = &self.expr {
            write!(f, "return {expr}", expr = expr)
        } else {
            write!(f, "return")
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Assign {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl fmt::Display for Assign {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{lhs} = {rhs}", lhs = self.lhs, rhs = self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
}

impl fmt::Display for BinOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Rem => write!(f, "%"),
            Self::And => write!(f, "&&"),
            Self::Or => write!(f, "||"),
            Self::BitAnd => write!(f, "&"),
            Self::BitOr => write!(f, "|"),
            Self::BitXor => write!(f, "^"),
            Self::Shl => write!(f, "<<"),
            Self::Shr => write!(f, ">>"),
            Self::Eq => write!(f, "=="),
            Self::Lt => write!(f, "<"),
            Self::Le => write!(f, "<="),
            Self::Ne => write!(f, "!="),
            Self::Ge => write!(f, ">="),
            Self::Gt => write!(f, ">"),
        }
    }
}

impl BinOpKind {
    pub fn as_assign_op(&self) -> &str {
        match self {
            Self::Add => "+=",
            Self::Sub => "-=",
            Self::Mul => "*=",
            Self::Div => "/=",
            Self::Rem => "%=",
            Self::And => "&=",
            Self::Or => "|=",
            Self::BitAnd => "&=",
            Self::BitOr => "|=",
            Self::BitXor => "^=",
            Self::Shl => "<<=",
            Self::Shr => ">>=",
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssignOp {
    pub lhs: Box<Expr>,
    pub op: BinOpKind,
    pub rhs: Box<Expr>,
}

impl fmt::Display for AssignOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{lhs} {op} {rhs}", lhs = self.lhs, op = self.op.as_assign_op(), rhs = self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
    Array(Array),
    Tuple(Tuple),
    Binary(Binary),
    Unary(Unary),
    Lit(Lit),
    Cast(Cast),
    TypeAscription(TypeAscription),
    Let(Let),
    If(If),
    While(While),
    ForLoop(ForLoop),
    Loop(Loop),
    Match(Match),
    Call(Call),
    MethodCall(MethodCall),
    Block(Block),
    Field(Field),
    Index(Index),
    Range(Range),
    Return(Return),
    Assign(Assign),
    AssignOp(AssignOp),
    MacCall(MacCall),
}

impl_display_for_enum!(ExprKind;
    Array,
    Tuple,
    Binary,
    Unary,
    Lit,
    Cast,
    TypeAscription,
    Let,
    If,
    While,
    ForLoop,
    Loop,
    Match,
    Call,
    MethodCall,
    Block,
    Field,
    Index,
    Range,
    Return,
    Assign,
    AssignOp,
    MacCall,
);
impl_obvious_conversion!(ExprKind;
    Array,
    Tuple,
    Binary,
    Unary,
    Lit,
    Cast,
    TypeAscription,
    Let,
    If,
    While,
    ForLoop,
    Loop,
    Match,
    Call,
    MethodCall,
    Block,
    Field,
    Index,
    Range,
    Return,
    Assign,
    AssignOp,
    MacCall,
);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LitKind {
    Bool,
    Byte,
    Char,
    Integer,
    Float,
    Str,
    ByteStr,
    CStr,
    Err,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Lit {
    pub kind: LitKind,
    pub symbol: String,
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.symbol.fmt(f)
    }
}

impl<S: Into<String>> From<S> for Lit {
    fn from(symbol: S) -> Self {
        Self {
            kind: LitKind::Str,
            symbol: format!("\"{}\"", symbol.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Cast {
    pub expr: Box<Expr>,
    pub ty: Type,
}

impl fmt::Display for Cast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({expr} as {ty})", expr = self.expr, ty = self.ty)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeAscription {
    pub expr: Box<Expr>,
    pub ty: Type,
}

impl fmt::Display for TypeAscription {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({expr}: {ty})", expr = self.expr, ty = self.ty)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Call {
    pub func: Box<Expr>,
    pub args: Vec<Expr>,
}

impl fmt::Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.func)?;
        let mut iter = self.args.iter();
        if let Some(arg) = iter.next() {
            write!(f, "{arg}")?;
            for arg in iter {
                write!(f, ", {arg}")?;
            }
        }
        write!(f, ")")
    }
}

impl Call {
    pub fn new(func: impl Into<Expr>, args: Vec<Expr>) -> Self {
        Self {
            func: Box::new(func.into()),
            args,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodCall {
    pub receiver: Box<Expr>,
    pub seg: PathSegment,
    pub args: Vec<Expr>,
}

impl fmt::Display for MethodCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}(", self.receiver, self.seg)?;
        let mut iter = self.args.iter();
        if let Some(arg) = iter.next() {
            write!(f, "{arg}")?;
            for arg in iter {
                write!(f, ", {arg}")?;
            }
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub segments: Vec<PathSegment>,
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.segments.iter();
        if let Some(segment) = iter.next() {
            write!(f, "{segment}")?;
            for segment in iter {
                write!(f, "::{segment}")?;
            }
        }
        Ok(())
    }
}

impl<S: Into<PathSegment>> From<S> for Path {
    fn from(ident: S) -> Self {
        Self::single(ident)
    }
}

impl Path {
    pub fn single(ident: impl Into<PathSegment>) -> Self {
        Self {
            segments: vec![ident.into()],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PathSegment {
    pub ident: String,
    pub args: Option<Vec<GenericArg>>,
}

impl fmt::Display for PathSegment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ident)?;
        if let Some(args) = &self.args {
            write!(f, "::<")?;
            let mut iter = args.iter();
            if let Some(arg) = iter.next() {
                write!(f, "{arg}")?;
                for arg in iter {
                    write!(f, ", {arg}")?;
                }
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl<S: Into<String>> From<S> for PathSegment {
    fn from(ident: S) -> Self {
        Self {
            ident: ident.into(),
            args: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericArg {
    Lifetime(String),
    Type(Type),
    Const(Const),
}

impl fmt::Display for GenericArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lifetime(lifetime) => write!(f, "'{lifetime}"),
            Self::Type(ty) => write!(f, "{ty}"),
            Self::Const(constant) => write!(f, "{constant}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MacDelimiter {
    Parenthesis,
    Bracket,
    Brace,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DelimArgs {
    pub delim: MacDelimiter,
    pub tokens: Vec<Token>,
}

impl fmt::Display for DelimArgs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.delim {
            MacDelimiter::Parenthesis => {
                write!(f, "(")?;
            },
            MacDelimiter::Bracket => {
                write!(f, "[")?;
            },
            MacDelimiter::Brace => {
                write!(f, "{{")?;
            },
        }
        let mut iter = self.tokens.iter();
        if let Some(token) = iter.next() {
            write!(f, "{token}")?;
            for token in iter {
                write!(f, ", {token}")?;
            }
        }
        match self.delim {
            MacDelimiter::Parenthesis => {
                write!(f, ")")
            },
            MacDelimiter::Bracket => {
                write!(f, "]")
            },
            MacDelimiter::Brace => {
                write!(f, "}}")
            },
        }
    }
}

impl Default for DelimArgs {
    fn default() -> Self {
        Self {
            delim: MacDelimiter::Parenthesis,
            tokens: Vec::new(),
        }
    }
}

impl From<Vec<Token>> for DelimArgs {
    fn from(tokens: Vec<Token>) -> Self {
        Self {
            delim: MacDelimiter::Parenthesis,
            tokens,
        }
    }
}

impl DelimArgs {
    pub fn new(delim: MacDelimiter, tokens: Vec<Token>) -> Self {
        Self {
            delim,
            tokens,
        }
    }

    pub fn add_token(&mut self, token: Token) {
        self.tokens.push(token);
    }

    pub fn remove_token(&mut self, token: &Token) {
        self.tokens.retain(|t| t != token);
    }

    pub fn get_token(&self, index: usize) -> Option<&Token> {
        self.tokens.get(index)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MacCall {
    /// `!` not included
    pub path: Path,
    pub args: DelimArgs,
}

impl fmt::Display for MacCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}!{}", self.path, self.args)?;
        Ok(())
    }
}

impl MacCall {
    pub fn new(path: Path, args: impl Into<DelimArgs>) -> Self {
        Self {
            path,
            args: args.into(),
        }
    }
}
