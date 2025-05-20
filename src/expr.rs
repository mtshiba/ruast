use std::fmt;
use std::ops::{Add, Div, Mul, Neg, Sub};

use crate::stmt::{Block, EmptyItem, FnDecl, Pat, Use};
use crate::token::{BinOpToken, Delimiter, KeywordToken, Token, TokenStream};
use crate::ty::Type;
use crate::{impl_display_for_enum, impl_obvious_conversion};

#[cfg(feature = "tokenize")]
macro_rules! impl_to_tokens {
    ($($Ty: ty,)*) => {
        $(
            impl quote::ToTokens for $Ty {
                fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
                    $crate::IntoTokens::into_tokens(self.clone()).to_tokens(tokens);
                }
            }
            impl $Ty {
                pub fn to_token_stream(&self) -> proc_macro2::TokenStream {
                    quote::ToTokens::to_token_stream(&self)
                }
                pub fn into_token_stream(self) -> proc_macro2::TokenStream {
                    quote::ToTokens::into_token_stream(&self)
                }
            }
        )*
    };
}
#[cfg(feature = "tokenize")]
pub(crate) use impl_to_tokens;

#[cfg(feature = "tokenize")]
impl_to_tokens!(
    AttributeItem,
    Expr,
    Const,
    Array,
    Tuple,
    Binary,
    Unary,
    Let,
    If,
    While,
    ForLoop,
    Loop,
    ConstBlock,
    UnsafeBlock,
    Arm,
    Match,
    Closure,
    Async,
    Await,
    TryBlock,
    Field,
    Index,
    Range,
    Underscore,
    Return,
    Yield,
    Assign,
    AssignOp,
    Cast,
    TypeAscription,
    Call,
    MethodCall,
    Path,
    PathSegment,
    AddrOf,
    Break,
    Continue,
    GenericArg,
    DelimArgs,
    MacCall,
    ExprField,
    Struct,
    Repeat,
    Try,
);

pub trait Callable {
    fn call(self, args: Vec<Expr>) -> Call;
    fn call1(self, arg: impl Into<Expr>) -> Call
    where
        Self: Sized,
    {
        self.call(vec![arg.into()])
    }
    fn call2(self, arg1: impl Into<Expr>, args2: impl Into<Expr>) -> Call
    where
        Self: Sized,
    {
        self.call(vec![arg1.into(), args2.into()])
    }
}

impl<E: Into<Expr>> Callable for E {
    fn call(self, args: Vec<Expr>) -> Call {
        Call::new(self, args)
    }
}

pub trait MethodCallable {
    fn method_call(self, seg: impl Into<PathSegment>, args: Vec<Expr>) -> MethodCall;
    fn method_call0(self, seg: impl Into<PathSegment>) -> MethodCall
    where
        Self: Sized,
    {
        self.method_call(seg, vec![])
    }
    fn method_call1(self, seg: impl Into<PathSegment>, arg: impl Into<Expr>) -> MethodCall
    where
        Self: Sized,
    {
        self.method_call(seg, vec![arg.into()])
    }
    fn method_call2(
        self,
        seg: impl Into<PathSegment>,
        arg1: impl Into<Expr>,
        arg2: impl Into<Expr>,
    ) -> MethodCall
    where
        Self: Sized,
    {
        self.method_call(seg, vec![arg1.into(), arg2.into()])
    }
}

impl<E: Into<Expr>> MethodCallable for E {
    fn method_call(self, seg: impl Into<PathSegment>, args: Vec<Expr>) -> MethodCall {
        MethodCall::new(self, seg, args)
    }
}

pub trait Castable {
    fn cast(self, ty: impl Into<Type>) -> Cast;
}

impl<E: Into<Expr>> Castable for E {
    fn cast(self, ty: impl Into<Type>) -> Cast {
        Cast::new(self, ty)
    }
}

pub trait Accessible {
    fn field(self, ident: impl Into<String>) -> Field;
}

impl<E: Into<Expr>> Accessible for E {
    fn field(self, ident: impl Into<String>) -> Field {
        Field::new(self, ident)
    }
}

pub trait Indexable {
    fn index(self, index: impl Into<Expr>) -> Index;
}

impl<E: Into<Expr>> Indexable for E {
    fn index(self, index: impl Into<Expr>) -> Index {
        Index::new(self, index)
    }
}

pub trait Awaitable {
    fn await_(self) -> Await;
}

impl<E: Into<Expr>> Awaitable for E {
    fn await_(self) -> Await {
        Await::new(self)
    }
}

pub trait Tryable {
    fn try_(self) -> Try;
}

impl<E: Into<Expr>> Tryable for E {
    fn try_(self) -> Try {
        Try::new(self)
    }
}

pub trait Assignable {
    fn assign(self, rhs: impl Into<Expr>) -> Assign;
}

impl<E: Into<Expr>> Assignable for E {
    fn assign(self, rhs: impl Into<Expr>) -> Assign {
        Assign::new(self, rhs)
    }
}

pub trait BinaryOperable {
    fn bin_op(self, op: BinOpKind, rhs: impl Into<Expr>) -> Binary;
    fn add(self, rhs: impl Into<Expr>) -> Binary
    where
        Self: Sized,
    {
        self.bin_op(BinOpKind::Add, rhs)
    }
    fn sub(self, rhs: impl Into<Expr>) -> Binary
    where
        Self: Sized,
    {
        self.bin_op(BinOpKind::Sub, rhs)
    }
    fn mul(self, rhs: impl Into<Expr>) -> Binary
    where
        Self: Sized,
    {
        self.bin_op(BinOpKind::Mul, rhs)
    }
    fn div(self, rhs: impl Into<Expr>) -> Binary
    where
        Self: Sized,
    {
        self.bin_op(BinOpKind::Div, rhs)
    }
}

impl<E: Into<Expr>> BinaryOperable for E {
    fn bin_op(self, op: BinOpKind, rhs: impl Into<Expr>) -> Binary {
        Binary::new(self, op, rhs)
    }
}

pub trait UnaryOperable {
    fn unary_op(self, op: UnaryOpKind) -> Unary;
    fn neg(self) -> Unary
    where
        Self: Sized,
    {
        self.unary_op(UnaryOpKind::Neg)
    }
}

impl<E: Into<Expr>> UnaryOperable for E {
    fn unary_op(self, op: UnaryOpKind) -> Unary {
        Unary::new(op, self)
    }
}

pub trait Addressable {
    fn addr_of(self, kind: BorrowKind, mutable: Mutability) -> AddrOf;
    fn ref_immut(self) -> AddrOf
    where
        Self: Sized,
    {
        self.addr_of(BorrowKind::Ref, Mutability::Not)
    }
    fn ref_mut(self) -> AddrOf
    where
        Self: Sized,
    {
        self.addr_of(BorrowKind::Ref, Mutability::Mut)
    }
    fn ptr_immut(self) -> AddrOf
    where
        Self: Sized,
    {
        self.addr_of(BorrowKind::Raw, Mutability::Not)
    }
    fn ptr_mut(self) -> AddrOf
    where
        Self: Sized,
    {
        self.addr_of(BorrowKind::Raw, Mutability::Mut)
    }
}

impl<E: Into<Expr>> Addressable for E {
    fn addr_of(self, kind: BorrowKind, mutable: Mutability) -> AddrOf {
        AddrOf::new(kind, mutable, self)
    }
}

pub trait IntoConst {
    fn into_const(self) -> ConstBlock;
}

impl<E: Into<Expr>> IntoConst for E {
    fn into_const(self) -> ConstBlock {
        ConstBlock::new(Block::single(self))
    }
}

pub trait IntoUnsafe {
    fn into_unsafe(self) -> UnsafeBlock;
}

impl<E: Into<Expr>> IntoUnsafe for E {
    fn into_unsafe(self) -> UnsafeBlock {
        UnsafeBlock::new(Block::single(self))
    }
}

pub trait Returnable {
    fn return_(self) -> Return;
}

impl<E: Into<Expr>> Returnable for E {
    fn return_(self) -> Return {
        Return::new(Some(self))
    }
}

pub trait Yieldable {
    fn yield_(self) -> Yield;
}

impl<E: Into<Expr>> Yieldable for E {
    fn yield_(self) -> Yield {
        Yield::new(Some(self))
    }
}

pub trait IntoTryBlock {
    fn into_try_block(self) -> TryBlock;
}

impl<E: Into<Expr>> IntoTryBlock for E {
    fn into_try_block(self) -> TryBlock {
        TryBlock::new(Block::single(self))
    }
}

pub trait IntoTokens {
    fn into_tokens(self) -> TokenStream;
}

impl<I: Into<TokenStream>> IntoTokens for I {
    fn into_tokens(self) -> TokenStream {
        self.into()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum AttrArgs {
    #[default]
    Empty,
    Delimited(DelimArgs),
    Eq(Expr),
}

impl fmt::Display for AttrArgs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, ""),
            Self::Delimited(delim) => write!(f, "{delim}"),
            Self::Eq(expr) => write!(f, " = {expr}"),
        }
    }
}

impl From<AttrArgs> for TokenStream {
    fn from(attr: AttrArgs) -> Self {
        match attr {
            AttrArgs::Empty => TokenStream::from(vec![]),
            AttrArgs::Delimited(delim) => delim.into(),
            AttrArgs::Eq(expr) => TokenStream::from(expr).and(TokenStream::from(vec![Token::Eq])),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Attribute {
    pub kind: AttrKind,
}

impl fmt::Display for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl From<AttrKind> for Attribute {
    fn from(kind: AttrKind) -> Self {
        Self { kind }
    }
}

impl From<AttributeItem> for Attribute {
    fn from(item: AttributeItem) -> Self {
        Self::new(item)
    }
}

impl From<Attribute> for TokenStream {
    fn from(value: Attribute) -> Self {
        TokenStream::from(value.kind)
    }
}

impl Attribute {
    pub fn new(kind: impl Into<AttrKind>) -> Self {
        Self { kind: kind.into() }
    }

    pub fn doc_comment(comment: impl Into<String>) -> Self {
        Self::new(AttrKind::DocComment(comment.into()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AttrKind {
    Normal(AttributeItem),
    /// This will be displayed but erased when converted to tokens.
    /// To preserve doc comments, use `#[doc = "..."]` instead.
    DocComment(String),
}

impl fmt::Display for AttrKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            AttrKind::Normal(item) => item.fmt(f),
            AttrKind::DocComment(comment) => comment.fmt(f),
        }
    }
}

impl From<AttributeItem> for AttrKind {
    fn from(item: AttributeItem) -> Self {
        Self::Normal(item)
    }
}

impl From<AttrKind> for TokenStream {
    fn from(value: AttrKind) -> Self {
        match value {
            AttrKind::Normal(item) => TokenStream::from(item),
            AttrKind::DocComment(comment) => TokenStream::from(Token::DocComment(comment)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AttributeItem {
    pub path: Path,
    pub args: AttrArgs,
}

impl fmt::Display for AttributeItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#[{}{}]", self.path, self.args)
    }
}

impl From<AttributeItem> for TokenStream {
    fn from(attr: AttributeItem) -> Self {
        let mut ts = TokenStream::from(vec![Token::Pound, Token::OpenDelim(Delimiter::Bracket)]);
        ts.extend(TokenStream::from(attr.path));
        ts.extend(TokenStream::from(attr.args));
        ts.push(Token::CloseDelim(Delimiter::Bracket));
        ts
    }
}

impl AttributeItem {
    pub fn new(path: impl Into<Path>, args: impl Into<AttrArgs>) -> Self {
        Self {
            path: path.into(),
            args: args.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Expr {
    pub attrs: Vec<AttributeItem>,
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

impl<E: Into<ExprKind>> From<E> for Expr {
    fn from(item: E) -> Self {
        Self::new(item)
    }
}

impl From<Expr> for TokenStream {
    fn from(value: Expr) -> Self {
        let mut ts = TokenStream::new();
        for attr in value.attrs.iter() {
            ts.extend(TokenStream::from(attr.clone()));
        }
        ts.extend(TokenStream::from(value.kind));
        ts
    }
}

impl Expr {
    pub fn new(kind: impl Into<ExprKind>) -> Self {
        Self {
            attrs: Vec::new(),
            kind: kind.into(),
        }
    }

    pub fn add_attr(&mut self, attr: AttributeItem) {
        self.attrs.push(attr);
    }

    pub fn remove_attr(&mut self, attr: &AttributeItem) {
        self.attrs.retain(|a| a != attr);
    }

    pub fn is_compound(&self) -> bool {
        matches!(
            &self.kind,
            ExprKind::Binary(_) | ExprKind::Unary(_) | ExprKind::Field(_) | ExprKind::Range(_)
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Const(pub Expr);

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl From<Const> for TokenStream {
    fn from(value: Const) -> Self {
        TokenStream::from(value.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Array(pub Vec<Expr>);

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

impl From<Vec<Expr>> for Array {
    fn from(value: Vec<Expr>) -> Self {
        Self(value)
    }
}

impl From<Array> for TokenStream {
    fn from(value: Array) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::OpenDelim(Delimiter::Bracket));
        for (i, expr) in value.0.into_iter().enumerate() {
            if i > 0 {
                ts.push(Token::Comma);
            }
            ts.extend(TokenStream::from(expr));
        }
        ts.push(Token::CloseDelim(Delimiter::Bracket));
        ts
    }
}

impl Array {
    pub fn new(exprs: Vec<Expr>) -> Self {
        Self(exprs)
    }

    pub fn unit() -> Self {
        Self::new(vec![])
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Tuple(pub Vec<Expr>);

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

impl From<Vec<Expr>> for Tuple {
    fn from(value: Vec<Expr>) -> Self {
        Self(value)
    }
}

impl From<Tuple> for TokenStream {
    fn from(value: Tuple) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::OpenDelim(Delimiter::Parenthesis));
        for (i, expr) in value.0.into_iter().enumerate() {
            if i > 0 {
                ts.push(Token::Comma);
            }
            ts.extend(TokenStream::from(expr));
        }
        ts.push(Token::CloseDelim(Delimiter::Parenthesis));
        ts
    }
}

impl Tuple {
    pub fn new(exprs: Vec<Expr>) -> Self {
        Self(exprs)
    }

    pub fn unit() -> Self {
        Self::new(vec![])
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
        write!(
            f,
            "({lhs} {op} {rhs})",
            lhs = self.lhs,
            op = self.op,
            rhs = self.rhs
        )
    }
}

impl From<Binary> for TokenStream {
    fn from(value: Binary) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::OpenDelim(Delimiter::Parenthesis));
        ts.extend(TokenStream::from(*value.lhs));
        ts.push(Token::from(value.op));
        ts.extend(TokenStream::from(*value.rhs));
        ts.push(Token::CloseDelim(Delimiter::Parenthesis));
        ts
    }
}

impl<E: Into<Expr>> Add<E> for Expr {
    type Output = Binary;

    fn add(self, rhs: E) -> Self::Output {
        Binary::new(self, BinOpKind::Add, rhs)
    }
}
impl<E: Into<Expr>> Sub<E> for Expr {
    type Output = Binary;

    fn sub(self, rhs: E) -> Self::Output {
        Binary::new(self, BinOpKind::Sub, rhs)
    }
}
impl<E: Into<Expr>> Mul<E> for Expr {
    type Output = Binary;

    fn mul(self, rhs: E) -> Self::Output {
        Binary::new(self, BinOpKind::Mul, rhs)
    }
}
impl<E: Into<Expr>> Div<E> for Expr {
    type Output = Binary;

    fn div(self, rhs: E) -> Self::Output {
        Binary::new(self, BinOpKind::Div, rhs)
    }
}

impl Binary {
    pub fn new(lhs: impl Into<Expr>, op: BinOpKind, rhs: impl Into<Expr>) -> Self {
        Self {
            lhs: Box::new(lhs.into()),
            op,
            rhs: Box::new(rhs.into()),
        }
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

impl From<UnaryOpKind> for Token {
    fn from(value: UnaryOpKind) -> Self {
        match value {
            UnaryOpKind::Deref => Token::BinOp(BinOpToken::Star),
            UnaryOpKind::Not => Token::Not,
            UnaryOpKind::Neg => Token::BinOp(BinOpToken::Minus),
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

impl From<Unary> for TokenStream {
    fn from(value: Unary) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::OpenDelim(Delimiter::Parenthesis));
        ts.push(Token::from(value.op));
        ts.extend(TokenStream::from(*value.expr));
        ts.push(Token::CloseDelim(Delimiter::Parenthesis));
        ts
    }
}

impl Neg for Expr {
    type Output = Unary;

    fn neg(self) -> Self::Output {
        Unary::new(UnaryOpKind::Neg, self)
    }
}

impl Unary {
    pub fn new(op: UnaryOpKind, expr: impl Into<Expr>) -> Self {
        Self {
            op,
            expr: Box::new(expr.into()),
        }
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

impl From<Let> for TokenStream {
    fn from(value: Let) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Let));
        ts.extend(TokenStream::from(*value.pat));
        ts.push(Token::Eq);
        ts.extend(TokenStream::from(*value.expr));
        ts
    }
}

impl Let {
    pub fn new(pat: impl Into<Pat>, expr: impl Into<Expr>) -> Self {
        Self {
            pat: Box::new(pat.into()),
            expr: Box::new(expr.into()),
        }
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
            write!(f, " else {else_}")?;
        }
        Ok(())
    }
}

impl From<If> for TokenStream {
    fn from(value: If) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::If));
        ts.extend(TokenStream::from(*value.cond));
        ts.extend(TokenStream::from(value.then));
        if let Some(else_) = value.else_ {
            ts.push(Token::Keyword(KeywordToken::Else));
            ts.push(Token::OpenDelim(Delimiter::Brace));
            ts.extend(TokenStream::from(*else_));
            ts.push(Token::CloseDelim(Delimiter::Brace));
        }
        ts
    }
}

impl If {
    pub fn new(cond: Expr, then: Block, else_: Option<Expr>) -> Self {
        Self {
            cond: Box::new(cond),
            then,
            else_: else_.map(Box::new),
        }
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

impl From<While> for TokenStream {
    fn from(value: While) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::While));
        ts.extend(TokenStream::from(*value.cond));
        ts.extend(TokenStream::from(value.body));
        ts
    }
}

impl While {
    pub fn new(cond: impl Into<Expr>, body: Block) -> Self {
        Self {
            cond: Box::new(cond.into()),
            body,
        }
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
        write!(
            f,
            "for {pat} in {expr} {body}",
            pat = self.pat,
            expr = self.expr,
            body = self.body
        )
    }
}

impl From<ForLoop> for TokenStream {
    fn from(value: ForLoop) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::For));
        ts.extend(TokenStream::from(*value.pat));
        ts.push(Token::Keyword(KeywordToken::In));
        ts.extend(TokenStream::from(*value.expr));
        ts.extend(TokenStream::from(value.body));
        ts
    }
}

impl ForLoop {
    pub fn new(pat: impl Into<Pat>, expr: impl Into<Expr>, body: Block) -> Self {
        Self {
            pat: Box::new(pat.into()),
            expr: Box::new(expr.into()),
            body,
        }
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

impl From<Loop> for TokenStream {
    fn from(value: Loop) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Loop));
        ts.extend(TokenStream::from(value.body));
        ts
    }
}

impl EmptyItem for Loop {
    type Input = ();

    fn empty(_ident: impl Into<()>) -> Self {
        Self::new(Block::empty())
    }
}

impl Loop {
    pub fn new(body: Block) -> Self {
        Self { body }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstBlock {
    pub block: Block,
}

impl fmt::Display for ConstBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "const {block}", block = self.block)
    }
}

impl From<ConstBlock> for TokenStream {
    fn from(value: ConstBlock) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Const));
        ts.extend(TokenStream::from(value.block));
        ts
    }
}

impl EmptyItem for ConstBlock {
    type Input = ();

    fn empty(_ident: impl Into<()>) -> Self {
        Self::new(Block::empty())
    }
}

impl ConstBlock {
    pub fn new(block: Block) -> Self {
        Self { block }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnsafeBlock {
    pub block: Block,
}

impl fmt::Display for UnsafeBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "unsafe {block}", block = self.block)
    }
}

impl From<UnsafeBlock> for TokenStream {
    fn from(value: UnsafeBlock) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Unsafe));
        ts.extend(TokenStream::from(value.block));
        ts
    }
}

impl EmptyItem for UnsafeBlock {
    type Input = ();

    fn empty(_ident: impl Into<()>) -> Self {
        Self::new(Block::empty())
    }
}

impl UnsafeBlock {
    pub fn new(block: Block) -> Self {
        Self { block }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Arm {
    pub attrs: Vec<AttributeItem>,
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

impl From<Arm> for TokenStream {
    fn from(value: Arm) -> Self {
        let mut ts = TokenStream::new();
        for attr in value.attrs.iter() {
            ts.extend(TokenStream::from(attr.clone()));
        }
        ts.extend(TokenStream::from(*value.pat));
        if let Some(guard) = value.guard {
            ts.push(Token::Keyword(KeywordToken::If));
            ts.extend(TokenStream::from(*guard));
        }
        ts.push(Token::FatArrow);
        ts.extend(TokenStream::from(*value.body));
        ts
    }
}

impl Arm {
    pub fn new(pat: impl Into<Pat>, guard: Option<Expr>, body: impl Into<Expr>) -> Self {
        Self {
            attrs: Vec::new(),
            pat: Box::new(pat.into()),
            guard: guard.map(Box::new),
            body: Box::new(body.into()),
        }
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
            writeln!(f, "{arm},")?;
        }
        write!(f, "}}")
    }
}

impl From<Match> for TokenStream {
    fn from(value: Match) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Match));
        ts.extend(TokenStream::from(*value.expr));
        ts.push(Token::OpenDelim(Delimiter::Brace));
        for arm in value.arms {
            ts.extend(TokenStream::from(arm));
            ts.push(Token::Comma);
        }
        ts.push(Token::CloseDelim(Delimiter::Brace));
        ts
    }
}

impl Match {
    pub fn new(expr: impl Into<Expr>, arms: Vec<Arm>) -> Self {
        Self {
            expr: Box::new(expr.into()),
            arms,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Closure {
    pub is_const: bool,
    pub is_static: bool,
    pub is_async: bool,
    pub is_move: bool,
    pub fn_decl: FnDecl,
    pub body: Box<Expr>,
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_const {
            write!(f, "const ")?;
        }
        if self.is_static {
            write!(f, "static ")?;
        }
        if self.is_async {
            write!(f, "async ")?;
        }
        if self.is_move {
            write!(f, "move ")?;
        }
        write!(f, "|")?;
        let mut iter = self.fn_decl.inputs.iter();
        if let Some(input) = iter.next() {
            write!(f, "{input}")?;
            for input in iter {
                write!(f, ", {input}")?;
            }
        }
        write!(f, "| ")?;
        if let Some(output) = &self.fn_decl.output {
            write!(f, "-> {output} ")?;
        }
        write!(f, "{{ {} }}", self.body)
    }
}

impl From<Closure> for TokenStream {
    fn from(value: Closure) -> Self {
        let mut ts = TokenStream::new();
        if value.is_const {
            ts.push(Token::Keyword(KeywordToken::Const));
        }
        if value.is_static {
            ts.push(Token::Keyword(KeywordToken::Static));
        }
        if value.is_async {
            ts.push(Token::Keyword(KeywordToken::Async));
        }
        if value.is_move {
            ts.push(Token::Keyword(KeywordToken::Move));
        }
        ts.push(Token::Or);
        let mut iter = value.fn_decl.inputs.iter();
        if let Some(input) = iter.next() {
            ts.extend(TokenStream::from(input.clone()));
            for input in iter {
                ts.push(Token::Comma);
                ts.extend(TokenStream::from(input.clone()));
            }
        }
        ts.push(Token::Or);
        ts.push(Token::RArrow);
        ts.push(Token::OpenDelim(Delimiter::Brace));
        ts.extend(TokenStream::from(*value.body));
        ts.push(Token::CloseDelim(Delimiter::Brace));
        ts
    }
}

impl Closure {
    pub fn new(
        is_const: bool,
        is_static: bool,
        is_async: bool,
        is_move: bool,
        fn_decl: FnDecl,
        body: impl Into<Expr>,
    ) -> Self {
        Self {
            is_const,
            is_static,
            is_async,
            is_move,
            fn_decl,
            body: Box::new(body.into()),
        }
    }

    pub fn simple(fn_decl: FnDecl, body: impl Into<Expr>) -> Self {
        Self::new(false, false, false, false, fn_decl, body)
    }

    pub fn new_const(fn_decl: FnDecl, body: impl Into<Expr>) -> Self {
        Self::new(true, false, false, false, fn_decl, body)
    }

    pub fn new_static(fn_decl: FnDecl, body: impl Into<Expr>) -> Self {
        Self::new(false, true, false, false, fn_decl, body)
    }

    pub fn new_async(fn_decl: FnDecl, body: impl Into<Expr>) -> Self {
        Self::new(false, false, true, false, fn_decl, body)
    }

    pub fn new_move(fn_decl: FnDecl, body: impl Into<Expr>) -> Self {
        Self::new(false, false, false, true, fn_decl, body)
    }
}

/// `async { ... }`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Async {
    pub block: Block,
}

impl fmt::Display for Async {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "async {block}", block = self.block)
    }
}

impl From<Async> for TokenStream {
    fn from(value: Async) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Async));
        ts.extend(TokenStream::from(value.block));
        ts
    }
}

impl EmptyItem for Async {
    type Input = ();

    fn empty(_ident: impl Into<()>) -> Self {
        Self::new(Block::empty())
    }
}

impl Async {
    pub fn new(block: Block) -> Self {
        Self { block }
    }
}

/// `expr.await`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Await {
    pub expr: Box<Expr>,
}

impl fmt::Display for Await {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{expr}.await", expr = self.expr)
    }
}

impl From<Await> for TokenStream {
    fn from(value: Await) -> Self {
        let mut ts = TokenStream::new();
        ts.extend(TokenStream::from(*value.expr));
        ts.push(Token::Dot);
        ts.push(Token::Keyword(KeywordToken::Await));
        ts
    }
}

impl Await {
    pub fn new(expr: impl Into<Expr>) -> Self {
        Self {
            expr: Box::new(expr.into()),
        }
    }
}

/// `try { ... }`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TryBlock {
    pub block: Block,
}

impl fmt::Display for TryBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "try {block}", block = self.block)
    }
}

impl From<TryBlock> for TokenStream {
    fn from(value: TryBlock) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Try));
        ts.extend(TokenStream::from(value.block));
        ts
    }
}

impl EmptyItem for TryBlock {
    type Input = ();

    fn empty(_ident: impl Into<()>) -> Self {
        Self::new(Block::empty())
    }
}

impl TryBlock {
    pub fn new(block: Block) -> Self {
        Self { block }
    }
}

/// `expr.ident`
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

impl From<Field> for TokenStream {
    fn from(value: Field) -> Self {
        let mut ts = TokenStream::new();
        ts.extend(TokenStream::from(*value.expr));
        ts.push(Token::Dot);
        ts.push(Token::ident(value.ident));
        ts
    }
}

impl Field {
    pub fn new(expr: impl Into<Expr>, ident: impl Into<String>) -> Self {
        Self {
            expr: Box::new(expr.into()),
            ident: ident.into(),
        }
    }
}

/// `expr[index]`
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

impl From<Index> for TokenStream {
    fn from(value: Index) -> Self {
        let mut ts = TokenStream::new();
        ts.extend(TokenStream::from(*value.expr));
        ts.push(Token::OpenDelim(Delimiter::Bracket));
        ts.extend(TokenStream::from(*value.index));
        ts.push(Token::CloseDelim(Delimiter::Bracket));
        ts
    }
}

impl Index {
    pub fn new(expr: impl Into<Expr>, index: impl Into<Expr>) -> Self {
        Self {
            expr: Box::new(expr.into()),
            index: Box::new(index.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RangeLimits {
    HalfOpen,
    Closed,
}

impl fmt::Display for RangeLimits {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::HalfOpen => write!(f, ".."),
            Self::Closed => write!(f, "..="),
        }
    }
}

impl RangeLimits {
    pub fn is_half_open(&self) -> bool {
        matches!(self, Self::HalfOpen)
    }

    pub fn is_closed(&self) -> bool {
        matches!(self, Self::Closed)
    }
}

/// `start..end` or `start..=end`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Range {
    pub start: Option<Box<Expr>>,
    pub end: Option<Box<Expr>>,
    pub limits: RangeLimits,
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{start}{limits}{end}",
            start = self
                .start
                .as_ref()
                .map(|e| e.to_string())
                .unwrap_or_default(),
            limits = self.limits,
            end = self.end.as_ref().map(|e| e.to_string()).unwrap_or_default()
        )
    }
}

impl From<Range> for TokenStream {
    fn from(value: Range) -> Self {
        let mut ts = TokenStream::new();
        if let Some(start) = value.start {
            ts.extend(TokenStream::from(*start));
        }
        match value.limits {
            RangeLimits::HalfOpen => {
                ts.push(Token::DotDot);
            }
            RangeLimits::Closed => {
                ts.push(Token::DotDotEq);
            }
        }
        if let Some(end) = value.end {
            ts.extend(TokenStream::from(*end));
        }
        ts
    }
}

impl Range {
    pub fn new(start: Option<Expr>, end: Option<Expr>, limits: RangeLimits) -> Self {
        Self {
            start: start.map(Box::new),
            end: end.map(Box::new),
            limits,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Underscore {}

impl fmt::Display for Underscore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "_")
    }
}

impl From<Underscore> for TokenStream {
    fn from(_: Underscore) -> Self {
        TokenStream::from(vec![Token::ident("_")])
    }
}

/// `return expr?`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Return {
    pub expr: Option<Box<Expr>>,
}

impl fmt::Display for Return {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(expr) = &self.expr {
            write!(f, "return {expr}")
        } else {
            write!(f, "return")
        }
    }
}

impl From<Return> for TokenStream {
    fn from(value: Return) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Return));
        if let Some(expr) = value.expr {
            ts.extend(TokenStream::from(*expr));
        }
        ts
    }
}

impl Return {
    pub fn new(expr: Option<impl Into<Expr>>) -> Self {
        Self {
            expr: expr.map(|e| Box::new(e.into())),
        }
    }
}

/// `yield expr?`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Yield {
    pub expr: Option<Box<Expr>>,
}

impl fmt::Display for Yield {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(expr) = &self.expr {
            write!(f, "yield {expr}")
        } else {
            write!(f, "yield")
        }
    }
}

impl From<Yield> for TokenStream {
    fn from(value: Yield) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Yield));
        if let Some(expr) = value.expr {
            ts.extend(TokenStream::from(*expr));
        }
        ts
    }
}

impl Yield {
    pub fn new(expr: Option<impl Into<Expr>>) -> Self {
        Self {
            expr: expr.map(|e| Box::new(e.into())),
        }
    }
}

/// `lhs = rhs`
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

impl From<Assign> for TokenStream {
    fn from(value: Assign) -> Self {
        let mut ts = TokenStream::new();
        ts.extend(TokenStream::from(*value.lhs));
        ts.push(Token::Eq);
        ts.extend(TokenStream::from(*value.rhs));
        ts
    }
}

impl Assign {
    pub fn new(lhs: impl Into<Expr>, rhs: impl Into<Expr>) -> Self {
        Self {
            lhs: Box::new(lhs.into()),
            rhs: Box::new(rhs.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Rem,
    /// `&&`
    LazyAnd,
    /// `||`
    LazyOr,
    /// `&`
    BitAnd,
    /// `|`
    BitOr,
    /// `^`
    BitXor,
    /// `<<`
    Shl,
    /// `>>`
    Shr,
    /// `==`
    Eq,
    /// `<`
    Lt,
    /// `<=`
    Le,
    /// `!=`
    Ne,
    /// `>=`
    Ge,
    /// `>`
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
            Self::LazyAnd => write!(f, "&&"),
            Self::LazyOr => write!(f, "||"),
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
            Self::LazyAnd => "&&=",
            Self::LazyOr => "||=",
            Self::BitAnd => "&=",
            Self::BitOr => "|=",
            Self::BitXor => "^=",
            Self::Shl => "<<=",
            Self::Shr => ">>=",
            _ => unreachable!(),
        }
    }
}

impl From<BinOpKind> for Token {
    fn from(value: BinOpKind) -> Self {
        match value {
            BinOpKind::Add => Token::BinOp(BinOpToken::Plus),
            BinOpKind::Sub => Token::BinOp(BinOpToken::Minus),
            BinOpKind::Mul => Token::BinOp(BinOpToken::Star),
            BinOpKind::Div => Token::BinOp(BinOpToken::Slash),
            BinOpKind::Rem => Token::BinOp(BinOpToken::Percent),
            BinOpKind::LazyAnd => Token::BinOp(BinOpToken::LazyAnd),
            BinOpKind::LazyOr => Token::BinOp(BinOpToken::LazyOr),
            BinOpKind::BitAnd => Token::BinOp(BinOpToken::BitAnd),
            BinOpKind::BitOr => Token::BinOp(BinOpToken::BitOr),
            BinOpKind::BitXor => Token::BinOp(BinOpToken::BitXor),
            BinOpKind::Shl => Token::BinOp(BinOpToken::Shl),
            BinOpKind::Shr => Token::BinOp(BinOpToken::Shr),
            BinOpKind::Eq => Token::EqEq,
            BinOpKind::Lt => Token::Lt,
            BinOpKind::Le => Token::Le,
            BinOpKind::Ne => Token::Ne,
            BinOpKind::Ge => Token::Ge,
            BinOpKind::Gt => Token::Gt,
        }
    }
}

impl From<BinOpKind> for TokenStream {
    fn from(value: BinOpKind) -> Self {
        Self::from(vec![Token::from(value)])
    }
}

/// `lhs op= rhs`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssignOp {
    pub lhs: Box<Expr>,
    pub op: BinOpKind,
    pub rhs: Box<Expr>,
}

impl fmt::Display for AssignOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{lhs} {as_op} {rhs}",
            lhs = self.lhs,
            as_op = self.op.as_assign_op(),
            rhs = self.rhs
        )
    }
}

impl From<AssignOp> for TokenStream {
    fn from(value: AssignOp) -> Self {
        let mut ts = TokenStream::new();
        ts.extend(TokenStream::from(*value.lhs));
        ts.push(Token::from(value.op));
        ts.extend(TokenStream::from(*value.rhs));
        ts
    }
}

impl AssignOp {
    pub fn new(lhs: impl Into<Expr>, op: BinOpKind, rhs: impl Into<Expr>) -> Self {
        Self {
            lhs: Box::new(lhs.into()),
            op,
            rhs: Box::new(rhs.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LitKind {
    Bool,
    Byte,
    Char,
    UInteger,
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
        match self.kind {
            LitKind::Str => write!(f, "\"{}\"", self.symbol),
            LitKind::CStr => write!(f, "c\"{}\"", self.symbol),
            LitKind::ByteStr => write!(f, "b\"{}\"", self.symbol),
            _ => self.symbol.fmt(f),
        }
    }
}

impl<S: Into<String>> From<S> for Lit {
    fn from(symbol: S) -> Self {
        Self {
            kind: LitKind::Str,
            symbol: symbol.into(),
        }
    }
}

impl From<Lit> for TokenStream {
    fn from(value: Lit) -> Self {
        TokenStream::from(vec![Token::Lit(value)])
    }
}

impl Lit {
    pub fn new(kind: LitKind, symbol: impl Into<String>) -> Self {
        Self {
            kind,
            symbol: symbol.into(),
        }
    }

    pub fn uint(symbol: impl Into<String>) -> Self {
        Self::new(LitKind::UInteger, symbol)
    }

    pub fn int(symbol: impl Into<String>) -> Self {
        Self::new(LitKind::Integer, symbol)
    }

    pub fn float(symbol: impl Into<String>) -> Self {
        Self::new(LitKind::Float, symbol)
    }

    pub fn str(symbol: impl Into<String>) -> Self {
        Self::new(LitKind::Str, symbol)
    }

    pub fn cstr(symbol: impl Into<String>) -> Self {
        Self::new(LitKind::CStr, symbol)
    }

    pub fn bstr(symbol: impl Into<String>) -> Self {
        Self::new(LitKind::ByteStr, symbol)
    }

    pub fn bool(symbol: impl Into<String>) -> Self {
        Self::new(LitKind::Bool, symbol)
    }
}

/// `expr as ty`
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

impl From<Cast> for TokenStream {
    fn from(value: Cast) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::OpenDelim(Delimiter::Parenthesis));
        ts.extend(TokenStream::from(*value.expr));
        ts.push(Token::Keyword(KeywordToken::As));
        ts.extend(TokenStream::from(value.ty));
        ts.push(Token::CloseDelim(Delimiter::Parenthesis));
        ts
    }
}

impl Cast {
    pub fn new(expr: impl Into<Expr>, ty: impl Into<Type>) -> Self {
        Self {
            expr: Box::new(expr.into()),
            ty: ty.into(),
        }
    }
}

/// `expr: ty`
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

impl From<TypeAscription> for TokenStream {
    fn from(value: TypeAscription) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::OpenDelim(Delimiter::Parenthesis));
        ts.extend(TokenStream::from(*value.expr));
        ts.push(Token::Colon);
        ts.extend(TokenStream::from(value.ty));
        ts.push(Token::CloseDelim(Delimiter::Parenthesis));
        ts
    }
}

impl TypeAscription {
    pub fn new(expr: impl Into<Expr>, ty: impl Into<Type>) -> Self {
        Self {
            expr: Box::new(expr.into()),
            ty: ty.into(),
        }
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
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{arg}")?;
        }
        write!(f, ")")
    }
}

impl From<Call> for TokenStream {
    fn from(value: Call) -> Self {
        let mut ts = TokenStream::new();
        ts.extend(TokenStream::from(*value.func));
        ts.push(Token::OpenDelim(Delimiter::Parenthesis));
        for (i, arg) in value.args.iter().enumerate() {
            if i > 0 {
                ts.push(Token::Comma);
            }
            ts.extend(TokenStream::from(arg.clone()));
        }
        ts.push(Token::CloseDelim(Delimiter::Parenthesis));
        ts
    }
}

impl Call {
    pub fn new(func: impl Into<Expr>, args: Vec<Expr>) -> Self {
        Self {
            func: Box::new(func.into()),
            args,
        }
    }

    pub fn args_to_tokens(exprs: Vec<Expr>) -> TokenStream {
        let mut ts = TokenStream::new();
        for (i, expr) in exprs.into_iter().enumerate() {
            if i > 0 {
                ts.push(Token::Comma);
            }
            ts.extend(TokenStream::from(expr));
        }
        ts
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
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{arg}")?;
        }
        write!(f, ")")
    }
}

impl From<MethodCall> for TokenStream {
    fn from(value: MethodCall) -> Self {
        let mut ts = TokenStream::new();
        ts.extend(TokenStream::from(*value.receiver));
        ts.push(Token::Dot);
        ts.extend(TokenStream::from(value.seg));
        ts.push(Token::OpenDelim(Delimiter::Parenthesis));
        for (i, arg) in value.args.iter().enumerate() {
            if i > 0 {
                ts.push(Token::Comma);
            }
            ts.extend(TokenStream::from(arg.clone()));
        }
        ts.push(Token::CloseDelim(Delimiter::Parenthesis));
        ts
    }
}

impl MethodCall {
    pub fn new(receiver: impl Into<Expr>, seg: impl Into<PathSegment>, args: Vec<Expr>) -> Self {
        Self {
            receiver: Box::new(receiver.into()),
            seg: seg.into(),
            args,
        }
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

impl From<Path> for TokenStream {
    fn from(value: Path) -> Self {
        let mut ts = TokenStream::new();
        let mut iter = value.segments.iter();
        if let Some(segment) = iter.next() {
            ts.extend(TokenStream::from(segment.clone()));
            for segment in iter {
                ts.push(Token::ModSep);
                ts.extend(TokenStream::from(segment.clone()));
            }
        }
        ts
    }
}

impl Path {
    pub const fn new(segments: Vec<PathSegment>) -> Self {
        Self { segments }
    }

    pub fn single(ident: impl Into<PathSegment>) -> Self {
        Self {
            segments: vec![ident.into()],
        }
    }

    pub fn chain(self, ident: impl Into<PathSegment>) -> Self {
        let mut segments = self.segments;
        segments.push(ident.into());
        Self { segments }
    }

    pub fn mac_call(self, args: impl Into<DelimArgs>) -> MacCall {
        MacCall::new(self, args)
    }

    pub fn struct_(self, fields: Vec<ExprField>) -> Struct {
        Struct::new(self, fields)
    }

    pub fn use_(self) -> Use {
        Use::path(self)
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
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{arg}")?;
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

impl From<PathSegment> for TokenStream {
    fn from(value: PathSegment) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::ident(value.ident));
        if let Some(args) = value.args {
            ts.push(Token::ModSep);
            ts.push(Token::Lt);
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    ts.push(Token::Comma);
                }
                ts.extend(TokenStream::from(arg.clone()));
            }
            ts.push(Token::Gt);
        }
        ts
    }
}

impl PathSegment {
    pub fn new(ident: impl Into<String>, args: Option<Vec<GenericArg>>) -> Self {
        Self {
            ident: ident.into(),
            args,
        }
    }

    pub fn simple(ident: impl Into<String>) -> Self {
        Self::new(ident, None)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BorrowKind {
    Ref,
    Raw,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum Mutability {
    #[default]
    Not,
    Mut,
}

impl Mutability {
    pub const fn is_mut(&self) -> bool {
        matches!(self, Self::Mut)
    }
}

impl fmt::Display for Mutability {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Mutability::Mut => write!(f, "mut"),
            Mutability::Not => write!(f, ""),
        }
    }
}

/// `&expr`, `&mut expr`, `&raw const expr`, `&raw mut expr`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AddrOf {
    pub kind: BorrowKind,
    pub mutability: Mutability,
    pub expr: Box<Expr>,
}

impl fmt::Display for AddrOf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "&")?;
        match (self.kind, self.mutability) {
            (BorrowKind::Ref, Mutability::Not) => {}
            (BorrowKind::Ref, Mutability::Mut) => {
                write!(f, "mut ")?;
            }
            (BorrowKind::Raw, Mutability::Not) => {
                write!(f, "raw const ")?;
            }
            (BorrowKind::Raw, Mutability::Mut) => {
                write!(f, "raw mut ")?;
            }
        }
        write!(f, "{expr}", expr = self.expr)
    }
}

impl From<AddrOf> for TokenStream {
    fn from(value: AddrOf) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::And);
        match (value.kind, value.mutability) {
            (BorrowKind::Ref, Mutability::Not) => {}
            (BorrowKind::Ref, Mutability::Mut) => {
                ts.push(Token::Keyword(KeywordToken::Mut));
            }
            (BorrowKind::Raw, Mutability::Not) => {
                ts.push(Token::ident("raw"));
                ts.push(Token::Keyword(KeywordToken::Const));
            }
            (BorrowKind::Raw, Mutability::Mut) => {
                ts.push(Token::ident("raw"));
                ts.push(Token::Keyword(KeywordToken::Mut));
            }
        }
        ts.extend(TokenStream::from(*value.expr));
        ts
    }
}

impl AddrOf {
    pub fn new(kind: BorrowKind, mutability: Mutability, expr: impl Into<Expr>) -> Self {
        Self {
            kind,
            mutability,
            expr: Box::new(expr.into()),
        }
    }

    pub fn ref_immutable(expr: impl Into<Expr>) -> Self {
        Self::new(BorrowKind::Ref, Mutability::Not, expr)
    }

    pub fn ref_mutable(expr: impl Into<Expr>) -> Self {
        Self::new(BorrowKind::Ref, Mutability::Mut, expr)
    }
}

/// `break ('label)? expr`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Break {
    pub label: Option<String>,
    pub expr: Option<Box<Expr>>,
}

impl fmt::Display for Break {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "break")?;
        if let Some(label) = &self.label {
            write!(f, " '{label}")?;
        }
        if let Some(expr) = &self.expr {
            write!(f, " {expr}")?;
        }
        Ok(())
    }
}

impl From<Break> for TokenStream {
    fn from(value: Break) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Break));
        if let Some(label) = value.label {
            ts.push(Token::lifetime(label));
        }
        if let Some(expr) = value.expr {
            ts.extend(TokenStream::from(*expr));
        }
        ts
    }
}

impl Break {
    pub fn new(label: Option<String>, expr: Option<Expr>) -> Self {
        Self {
            label,
            expr: expr.map(Box::new),
        }
    }
}

/// `continue ('label)?`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Continue {
    pub label: Option<String>,
}

impl fmt::Display for Continue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "continue")?;
        if let Some(label) = &self.label {
            write!(f, " '{label}")?;
        }
        Ok(())
    }
}

impl From<Continue> for TokenStream {
    fn from(value: Continue) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Continue));
        if let Some(label) = value.label {
            ts.push(Token::lifetime(label));
        }
        ts
    }
}

impl Continue {
    pub fn new(label: Option<String>) -> Self {
        Self { label }
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

impl From<GenericArg> for TokenStream {
    fn from(value: GenericArg) -> Self {
        match value {
            GenericArg::Lifetime(lifetime) => TokenStream::from(vec![Token::lifetime(lifetime)]),
            GenericArg::Type(ty) => TokenStream::from(ty),
            GenericArg::Const(constant) => TokenStream::from(constant),
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
    pub tokens: TokenStream,
}

impl fmt::Display for DelimArgs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.delim {
            MacDelimiter::Parenthesis => {
                write!(f, "(")?;
            }
            MacDelimiter::Bracket => {
                write!(f, "[")?;
            }
            MacDelimiter::Brace => {
                write!(f, "{{")?;
            }
        }
        write!(f, "{}", self.tokens)?;
        match self.delim {
            MacDelimiter::Parenthesis => {
                write!(f, ")")
            }
            MacDelimiter::Bracket => {
                write!(f, "]")
            }
            MacDelimiter::Brace => {
                write!(f, "}}")
            }
        }
    }
}

impl Default for DelimArgs {
    fn default() -> Self {
        Self {
            delim: MacDelimiter::Parenthesis,
            tokens: TokenStream::new(),
        }
    }
}

impl From<Vec<Token>> for DelimArgs {
    fn from(tokens: Vec<Token>) -> Self {
        Self {
            delim: MacDelimiter::Parenthesis,
            tokens: TokenStream::from(tokens),
        }
    }
}

impl From<DelimArgs> for TokenStream {
    fn from(value: DelimArgs) -> Self {
        let mut ts = TokenStream::new();
        match value.delim {
            MacDelimiter::Parenthesis => {
                ts.push(Token::OpenDelim(Delimiter::Parenthesis));
            }
            MacDelimiter::Bracket => {
                ts.push(Token::OpenDelim(Delimiter::Bracket));
            }
            MacDelimiter::Brace => {
                ts.push(Token::OpenDelim(Delimiter::Brace));
            }
        }
        ts.extend(value.tokens);
        match value.delim {
            MacDelimiter::Parenthesis => {
                ts.push(Token::CloseDelim(Delimiter::Parenthesis));
            }
            MacDelimiter::Bracket => {
                ts.push(Token::CloseDelim(Delimiter::Bracket));
            }
            MacDelimiter::Brace => {
                ts.push(Token::CloseDelim(Delimiter::Brace));
            }
        }
        ts
    }
}

impl DelimArgs {
    pub fn new(delim: MacDelimiter, tokens: TokenStream) -> Self {
        Self { delim, tokens }
    }

    pub fn parenthesis(tokens: TokenStream) -> Self {
        Self::new(MacDelimiter::Parenthesis, tokens)
    }

    pub fn bracket(tokens: TokenStream) -> Self {
        Self::new(MacDelimiter::Bracket, tokens)
    }

    pub fn brace(tokens: TokenStream) -> Self {
        Self::new(MacDelimiter::Brace, tokens)
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

impl From<MacCall> for TokenStream {
    fn from(value: MacCall) -> Self {
        let mut ts = TokenStream::new();
        ts.extend(TokenStream::from(value.path));
        ts.push(Token::Not);
        ts.extend(TokenStream::from(value.args));
        ts
    }
}

impl MacCall {
    pub fn new(path: Path, args: impl Into<DelimArgs>) -> Self {
        Self {
            path,
            args: args.into(),
        }
    }

    pub fn bracket(path: Path, tokens: Vec<impl Into<TokenStream>>) -> Self {
        Self {
            path,
            args: DelimArgs::bracket(TokenStream::aggregate(tokens.into_iter().map(|t| {
                let mut ts = t.into();
                ts.push(Token::Comma);
                ts
            }))),
        }
    }
}

/// `ident: expr`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprField {
    pub ident: String,
    pub expr: Expr,
}

impl fmt::Display for ExprField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{ident}: {expr}", ident = self.ident, expr = self.expr)
    }
}

impl From<ExprField> for TokenStream {
    fn from(value: ExprField) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::ident(value.ident));
        ts.push(Token::Colon);
        ts.extend(TokenStream::from(value.expr));
        ts
    }
}

impl ExprField {
    pub fn new(ident: impl Into<String>, expr: impl Into<Expr>) -> Self {
        Self {
            ident: ident.into(),
            expr: expr.into(),
        }
    }

    pub fn shortened(ident: impl Into<String>) -> Self {
        let ident = ident.into();
        Self {
            ident: ident.clone(),
            expr: Expr::new(ExprKind::Path(Path::single(ident))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
    pub path: Path,
    pub fields: Vec<ExprField>,
}

impl fmt::Display for Struct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {{", self.path)?;
        for (i, field) in self.fields.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            writeln!(f, "{field}")?;
        }
        write!(f, "}}")
    }
}

impl From<Struct> for TokenStream {
    fn from(value: Struct) -> Self {
        let mut ts = TokenStream::new();
        ts.extend(TokenStream::from(value.path));
        ts.push(Token::OpenDelim(Delimiter::Brace));
        for (i, field) in value.fields.iter().enumerate() {
            if i > 0 {
                ts.push(Token::Comma);
            }
            ts.extend(TokenStream::from(field.clone()));
        }
        ts.push(Token::CloseDelim(Delimiter::Brace));
        ts
    }
}

impl Struct {
    pub fn new(path: impl Into<Path>, fields: Vec<ExprField>) -> Self {
        Self {
            path: path.into(),
            fields,
        }
    }
}

/// `[expr; len]`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Repeat {
    pub expr: Box<Expr>,
    pub len: Box<Const>,
}

impl fmt::Display for Repeat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{expr}; {len}]", expr = self.expr, len = self.len)
    }
}

impl From<Repeat> for TokenStream {
    fn from(value: Repeat) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::OpenDelim(Delimiter::Bracket));
        ts.extend(TokenStream::from(*value.expr));
        ts.push(Token::Semi);
        ts.extend(TokenStream::from(*value.len));
        ts.push(Token::CloseDelim(Delimiter::Bracket));
        ts
    }
}

impl Repeat {
    pub fn new(expr: impl Into<Expr>, len: impl Into<Const>) -> Self {
        Self {
            expr: Box::new(expr.into()),
            len: Box::new(len.into()),
        }
    }
}

/// `expr?`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Try {
    pub expr: Box<Expr>,
}

impl fmt::Display for Try {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{expr}?", expr = self.expr)
    }
}

impl From<Try> for TokenStream {
    fn from(value: Try) -> Self {
        let mut ts = TokenStream::new();
        ts.extend(TokenStream::from(*value.expr));
        ts.push(Token::Question);
        ts
    }
}

impl Try {
    pub fn new(expr: impl Into<Expr>) -> Self {
        Self {
            expr: Box::new(expr.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
    Array(Array),
    Call(Call),
    MethodCall(MethodCall),
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
    ConstBlock(ConstBlock),
    UnsafeBlock(UnsafeBlock),
    Match(Match),
    Closure(Closure),
    Block(Block),
    Async(Async),
    Await(Await),
    TryBlock(TryBlock),
    Assign(Assign),
    AssignOp(AssignOp),
    Field(Field),
    Index(Index),
    Range(Range),
    Underscore(Underscore),
    Path(Path),
    AddrOf(AddrOf),
    Break(Break),
    Continue(Continue),
    Return(Return),
    Yield(Yield),
    MacCall(MacCall),
    Struct(Struct),
    Repeat(Repeat),
    Try(Try),
}

impl_display_for_enum!(ExprKind;
    Array,
    Call,
    MethodCall,
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
    ConstBlock,
    UnsafeBlock,
    Match,
    Closure,
    Block,
    Async,
    Await,
    TryBlock,
    Assign,
    AssignOp,
    Field,
    Index,
    Range,
    Underscore,
    Path,
    AddrOf,
    Break,
    Continue,
    Return,
    Yield,
    MacCall,
    Struct,
    Repeat,
    Try,
);
impl_obvious_conversion!(ExprKind;
    Array,
    Call,
    MethodCall,
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
    ConstBlock,
    UnsafeBlock,
    Match,
    Closure,
    Block,
    Async,
    Await,
    TryBlock,
    Assign,
    AssignOp,
    Field,
    Index,
    Range,
    Underscore,
    Path,
    AddrOf,
    Break,
    Continue,
    Return,
    Yield,
    MacCall,
    Struct,
    Repeat,
    Try,
);

impl Expr {
    pub fn call(self, args: Vec<Expr>) -> Self {
        Self::new(ExprKind::Call(Call {
            func: Box::new(self),
            args,
        }))
    }

    pub fn method_call(self, seg: PathSegment, args: Vec<Expr>) -> Self {
        Self::new(ExprKind::MethodCall(MethodCall {
            receiver: Box::new(self),
            seg,
            args,
        }))
    }

    pub fn cast(self, ty: impl Into<Type>) -> Self {
        Self::new(ExprKind::Cast(Cast {
            expr: Box::new(self),
            ty: ty.into(),
        }))
    }

    pub fn field(self, ident: impl Into<String>) -> Self {
        Self::new(ExprKind::Field(Field {
            expr: Box::new(self),
            ident: ident.into(),
        }))
    }

    pub fn index(self, index: impl Into<Expr>) -> Self {
        Self::new(ExprKind::Index(Index {
            expr: Box::new(self),
            index: Box::new(index.into()),
        }))
    }

    pub fn await_(self) -> Self {
        Self::new(ExprKind::Await(Await {
            expr: Box::new(self),
        }))
    }

    pub fn try_(self) -> Self {
        Self::new(ExprKind::Try(Try {
            expr: Box::new(self),
        }))
    }
}
