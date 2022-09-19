use std::error::Error;
use std::fmt::{self, Display};

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Position {
    pub row: u32,
    pub col: u32,
}

impl Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
    }
}

impl Position {
    fn advance(&mut self) {
        self.col += 1;
    }

    fn advance_line(&mut self) {
        self.row += 1;
        self.col = 1;
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct StartEnd {
    pub start: Position,
    pub end: Position,
}

impl Display for StartEnd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

pub struct StringLiteral {
    pub location: StartEnd,
    pub content: String,
}

pub struct Varaible {
    pub name: String,
}

pub enum ExpressionContent {
    Varaible(Varaible),
    Await(Box<Expression>),
    Call(Varaible, Vec<Expression>),
}

pub struct Expression {
    pub location: StartEnd,
    pub content: ExpressionContent,
}

pub enum StatementContent {
    Expression(Expression),
    Assignment(Varaible, Expression),
    Return(Expression),
    Print(StringLiteral),
    PrintLn(StringLiteral),
}

pub struct Statement {
    pub location: StartEnd,
    pub content: StatementContent,
}

pub struct Function {
    pub location: StartEnd,
    pub name: String,
    pub params: Vec<Varaible>,
    pub code: Vec<Statement>,
}

pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(Debug)]
enum ParseErrorContent {
    ExpectedSlashOrAsterisk,
    UnfinishedComment,
    UnfinishedString,
    ExpectedIdentFoundKeyword(String),
    ExpectedAsyncKeyword,
    ExpectedCharacter(char),
    ExpectedFunction,
    ExpectedFunctionName,
    ExpectedExpression,
    ExpectedStatementFoundKeyword(String),
    ExpectedString,
    UnknownEscapeCharacter(char),
}

impl Display for ParseErrorContent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ParseErrorContent::*;
        match self {
            ExpectedSlashOrAsterisk => write!(f, "Expected a slash or an asterisk."),
            UnfinishedComment => write!(f, "Unfinished comment."),
            UnfinishedString => write!(f, "Unfinished string."),
            ExpectedIdentFoundKeyword(s) => {
                write!(f, "Expected an identifier; found keyword '{s}'.")
            }
            ExpectedAsyncKeyword => write!(f, "Expected 'async' keyword."),
            ExpectedCharacter(c) => write!(f, "Expected character {c:?}."),
            ExpectedFunction => write!(f, "Expected a function."),
            ExpectedFunctionName => write!(f, "Expected function name."),
            ExpectedExpression => write!(f, "Expected an expression."),
            ExpectedStatementFoundKeyword(s) => {
                write!(f, "Expected a statement; found keyword '{s}'.")
            }
            ExpectedString => write!(f, "Expected a string literal after 'print' or 'println'."),
            UnknownEscapeCharacter(c) => write!(f, "Unknown escape sequence: '\\{c}'."),
        }
    }
}

#[derive(Debug)]
pub struct ParseError {
    location: StartEnd,
    content: ParseErrorContent,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parse Error at {}: {}", self.location, self.content)
    }
}

impl Error for ParseError {}

macro_rules! error {
    ($this:expr, $start:expr, $content:ident) => {
        Err(ParseError {
            location: StartEnd { start: $start, end: $this.position.clone() },
            content: ParseErrorContent::$content,
        })
    };
    ($this:expr, $start:expr, $content:ident ( $($params:expr),* )) => {
        Err(ParseError {
            location: StartEnd { start: $start, end: $this.position.clone() },
            content: ParseErrorContent::$content($($params),*)
        })
    }
}

type Result<T> = std::result::Result<T, ParseError>;

struct Parser<I: Iterator<Item = char>> {
    iter: std::iter::Peekable<I>,
    position: Position,
}

impl<I: Iterator<Item = char>> Parser<I> {
    fn next_char(&mut self) -> Option<char> {
        let c = self.iter.next();
        match c {
            Some('\n') => self.position.advance_line(),
            Some(_) => self.position.advance(),
            None => {}
        }
        c
    }

    fn peek_char(&mut self) -> Option<char> {
        self.iter.peek().copied()
    }

    fn next_char_if<F: FnOnce(char) -> bool>(&mut self, f: F) -> Option<char> {
        self.peek_char().filter(|&c| f(c)).map(|c| {
            self.next_char();
            c
        })
    }

    fn expect_char(&mut self, c: char) -> Result<()> {
        let pos = self.position.clone();
        if self.next_char_if(|c2| c == c2).is_some() {
            Ok(())
        } else {
            self.next_char();
            error!(self, pos, ExpectedCharacter(c))
        }
    }

    fn line_comment(&mut self) -> Result<()> {
        loop {
            if let None | Some('\n') = self.next_char() {
                return Ok(());
            }
        }
    }

    fn multiline_comment(&mut self, start: Position) -> Result<()> {
        let mut after_asterisk = false;
        loop {
            match self.next_char() {
                None => return error!(self, start, UnfinishedComment),
                Some('*') => after_asterisk = true,
                Some('/') if after_asterisk => return Ok(()),
                Some(_) => after_asterisk = false,
            }
        }
    }

    fn ws(&mut self) -> Result<()> {
        while let Some(c) = self.peek_char() {
            if c.is_whitespace() {
                self.next_char();
                continue;
            }
            if c != '/' {
                break;
            }
            let pos = self.position.clone();
            self.next_char();
            let pos2 = self.position.clone();
            match self.next_char() {
                Some('/') => self.line_comment()?,
                Some('*') => self.multiline_comment(pos)?,
                _ => return error!(self, pos2, ExpectedSlashOrAsterisk),
            }
        }
        Ok(())
    }

    fn parse_word(&mut self) -> Result<Option<String>> {
        let mut s = String::new();
        let c = match self.next_char_if(|c| c.is_alphabetic() || c == '_') {
            Some(c) => c,
            None => return Ok(None),
        };
        s.push(c);
        while let Some(c) = self.next_char_if(|c| c.is_alphanumeric() || c == '_') {
            s.push(c);
        }
        Ok(Some(s))
    }

    const KEYWORDS: &'static [&'static str] = &["async", "await", "return", "print", "println"];

    fn parse_ident(&mut self) -> Result<Option<Varaible>> {
        let pos = self.position.clone();
        match self.parse_word()? {
            None => Ok(None),
            Some(word) if Self::KEYWORDS.contains(&&word[..]) => {
                error!(self, pos, ExpectedIdentFoundKeyword(word))
            }
            Some(name) => Ok(Some(Varaible { name })),
        }
    }

    fn parse_string(&mut self) -> Result<Option<StringLiteral>> {
        let pos = self.position.clone();
        if self.next_char_if(|c| c == '"').is_none() {
            return Ok(None);
        }
        let mut s = String::new();
        loop {
            match self.next_char() {
                None => return error!(self, pos, UnfinishedString),
                Some('"') => {
                    return Ok(Some(StringLiteral {
                        location: StartEnd {
                            start: pos,
                            end: self.position.clone(),
                        },
                        content: s,
                    }))
                }
                Some('\\') => s.push(match self.next_char() {
                    None => return error!(self, pos, UnfinishedString),
                    Some('n') => '\n',
                    Some('"') => '"',
                    Some('\\') => '\\',
                    Some(c) => return error!(self, pos, UnknownEscapeCharacter(c)),
                }),
                Some(c) => s.push(c),
            }
        }
    }

    fn parse_args(&mut self) -> Result<Vec<Expression>> {
        let mut args = vec![];
        while let Some(expr) = self.parse_expression()? {
            args.push(expr);
            self.ws()?;
            if self.next_char_if(|c| c == ',').is_none() {
                break;
            }
            self.ws()?;
        }
        Ok(args)
    }

    fn parse_expression_from_word(&mut self, word: String, pos: Position) -> Result<Expression> {
        if word == "await" {
            self.ws()?;
            let expr = if let Some(expr) = self.parse_expression()? {
                expr
            } else {
                let pos = self.position.clone();
                self.next_char();
                return error!(self, pos, ExpectedExpression);
            };
            return Ok(Expression {
                location: StartEnd {
                    start: pos,
                    end: self.position.clone(),
                },
                content: ExpressionContent::Await(Box::new(expr)),
            });
        }
        if Self::KEYWORDS.contains(&&word[..]) {
            return error!(self, pos, ExpectedIdentFoundKeyword(word));
        }
        let var = Varaible { name: word };
        self.ws()?;
        if self.next_char_if(|c| c == '(').is_some() {
            self.ws()?;
            let args = self.parse_args()?;
            self.expect_char(')')?;
            return Ok(Expression {
                location: StartEnd {
                    start: pos,
                    end: self.position.clone(),
                },
                content: ExpressionContent::Call(var, args),
            });
        }
        Ok(Expression {
            location: StartEnd {
                start: pos,
                end: self.position.clone(),
            },
            content: ExpressionContent::Varaible(var),
        })
    }

    fn parse_expression(&mut self) -> Result<Option<Expression>> {
        let pos = self.position.clone();
        Ok(if self.next_char_if(|c| c == '(').is_some() {
            self.ws()?;
            let expr = if let Some(expr) = self.parse_expression()? {
                expr
            } else {
                let pos = self.position.clone();
                self.next_char();
                return error!(self, pos, ExpectedExpression);
            };
            self.ws()?;
            self.expect_char(')')?;
            Some(expr)
        } else if let Some(word) = self.parse_word()? {
            Some(self.parse_expression_from_word(word, pos)?)
        } else {
            None
        })
    }

    fn parse_statement(&mut self) -> Result<Option<Statement>> {
        let pos = self.position.clone();
        Ok(match self.parse_word()? {
            None if self.peek_char() == Some('(') => {
                let expr = match self.parse_expression()? {
                    Some(expr) => expr,
                    None => return Ok(None),
                };
                Some(Statement {
                    location: expr.location.clone(),
                    content: StatementContent::Expression(expr),
                })
            }
            None => None,
            Some(word) if Self::KEYWORDS.contains(&&word[..]) => Some({
                match &word[..] {
                    "await" => {
                        let expr = self.parse_expression_from_word(word, pos.clone())?;
                        Statement {
                            location: StartEnd {
                                start: pos,
                                end: self.position.clone(),
                            },
                            content: StatementContent::Expression(expr),
                        }
                    }
                    "print" | "println" => {
                        self.ws()?;
                        let s = if let Some(s) = self.parse_string()? {
                            s
                        } else {
                            let pos = self.position.clone();
                            self.next_char();
                            return error!(self, pos, ExpectedString);
                        };
                        Statement {
                            location: StartEnd {
                                start: pos,
                                end: self.position.clone(),
                            },
                            content: match &word[..] {
                                "print" => StatementContent::Print(s),
                                "println" => StatementContent::PrintLn(s),
                                _ => unreachable!(),
                            },
                        }
                    }
                    "return" => {
                        self.ws()?;
                        let expr = if let Some(expr) = self.parse_expression()? {
                            expr
                        } else {
                            return error!(self, pos, ExpectedExpression);
                        };
                        Statement {
                            location: StartEnd {
                                start: pos,
                                end: self.position.clone(),
                            },
                            content: StatementContent::Return(expr),
                        }
                    }
                    _ => return error!(self, pos, ExpectedStatementFoundKeyword(word)),
                }
            }),
            Some(word) => Some({
                self.ws()?;
                if self.next_char_if(|c| c == '=').is_some() {
                    self.ws()?;
                    let expr = if let Some(expr) = self.parse_expression()? {
                        expr
                    } else {
                        return error!(self, pos, ExpectedExpression);
                    };
                    Statement {
                        location: StartEnd {
                            start: pos,
                            end: self.position.clone(),
                        },
                        content: StatementContent::Assignment(Varaible { name: word }, expr),
                    }
                } else {
                    let expr = self.parse_expression_from_word(word, pos.clone())?;
                    Statement {
                        location: StartEnd {
                            start: pos,
                            end: self.position.clone(),
                        },
                        content: StatementContent::Expression(expr),
                    }
                }
            }),
        })
    }

    fn parse_params(&mut self) -> Result<Vec<Varaible>> {
        let mut params = vec![];
        while let Some(ident) = self.parse_ident()? {
            params.push(ident);
            self.ws()?;
            if self.next_char_if(|c| c == ',').is_none() {
                break;
            }
            self.ws()?;
        }
        Ok(params)
    }

    fn parse_function(&mut self) -> Result<Option<Function>> {
        let pos = self.position.clone();
        match self.parse_word()? {
            None => return Ok(None),
            Some(word) if word == "async" => {}
            Some(_) => return error!(self, pos, ExpectedAsyncKeyword),
        }
        self.ws()?;
        let name = if let Some(name) = self.parse_ident()? {
            name.name
        } else {
            let pos = self.position.clone();
            self.next_char();
            return error!(self, pos, ExpectedFunctionName);
        };
        self.ws()?;
        self.expect_char('(')?;
        self.ws()?;
        let params = self.parse_params()?;
        self.expect_char(')')?;
        self.ws()?;
        self.expect_char('{')?;
        self.ws()?;
        let mut code = vec![];
        while let Some(stmt) = self.parse_statement()? {
            code.push(stmt);
            self.ws()?;
        }
        self.expect_char('}')?;
        Ok(Some(Function {
            location: StartEnd {
                start: pos,
                end: self.position.clone(),
            },
            name,
            params,
            code,
        }))
    }

    fn parse(&mut self) -> Result<Program> {
        self.ws()?;
        let mut functions = vec![];
        while let Some(func) = self.parse_function()? {
            functions.push(func);
            self.ws()?;
        }
        let pos = self.position.clone();
        if self.next_char().is_some() {
            return error!(self, pos, ExpectedFunction);
        }
        Ok(Program { functions })
    }
}

pub fn parse<I: Iterator<Item = char>>(iter: I) -> Result<Program> {
    Parser {
        iter: iter.peekable(),
        position: Position { row: 1, col: 1 },
    }
    .parse()
}
