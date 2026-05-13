use crate::numeric::bigint_parse_bytes;
use crate::token::Token;

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    line: usize,
    col: usize,
    current_char: Option<char>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let chars: Vec<char> = input.chars().collect();
        let current_char = chars.first().copied();
        Lexer {
            input: chars,
            position: 0,
            line: 1,
            col: 1,
            current_char,
        }
    }

    fn advance(&mut self) {
        self.position += 1;
        self.current_char = self.input.get(self.position).copied();
        if self.current_char == Some('\n') {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
    }

    fn peek(&self) -> Option<char> {
        self.input.get(self.position + 1).copied()
    }

    fn peek_n(&self, n: usize) -> Option<char> {
        self.input.get(self.position + n).copied()
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current_char {
            if ch.is_whitespace() && ch != '\\' {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) {
        // LSR: 单行注释 (# ...)
        if self.current_char == Some('#')
            && !(self.peek() == Some('#') && self.peek_n(2) == Some('#'))
        {
            while self.current_char.is_some() && self.current_char != Some('\n') {
                self.advance();
            }
            if self.current_char == Some('\n') {
                self.advance();
            }
            return;
        }

        // LSR: 块注释 (### ... ###)
        if self.current_char == Some('#') && self.peek() == Some('#') && self.peek_n(2) == Some('#')
        {
            self.advance(); // #
            self.advance(); // #
            self.advance(); // #

            while self.current_char.is_some() {
                if self.current_char == Some('#')
                    && self.peek() == Some('#')
                    && self.peek_n(2) == Some('#')
                {
                    self.advance(); // #
                    self.advance(); // #
                    self.advance(); // #
                    break;
                }
                self.advance();
            }
            return;
        }

        // 单行注释
        if self.current_char == Some('/') && self.peek() == Some('/') {
            while self.current_char.is_some() && self.current_char != Some('\n') {
                self.advance();
            }
            self.advance(); // 跳过换行符
            return;
        }

        // 块注释
        if self.current_char == Some('/') && self.peek() == Some('*') {
            self.advance(); // 跳过 /
            self.advance(); // 跳过 *

            while self.current_char.is_some() {
                if self.current_char == Some('*') && self.peek() == Some('/') {
                    self.advance(); // 跳过 *
                    self.advance(); // 跳过 /
                    break;
                }
                self.advance();
            }
        }
    }

    fn read_number(&mut self) -> Token {
        let mut num_str = String::new();
        let mut is_float = false;

        while let Some(ch) = self.current_char {
            if ch.is_ascii_digit() {
                num_str.push(ch);
                self.advance();
            } else if ch == '.' && !is_float && self.peek().is_some_and(|c| c.is_ascii_digit()) {
                is_float = true;
                num_str.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        if is_float {
            // Return as Decimal token to preserve precision
            Token::Decimal(num_str)
        } else {
            match num_str.parse::<i64>() {
                Ok(n) => Token::Int(n),
                Err(_) => Token::BigIntLiteral(
                    bigint_parse_bytes(num_str.as_bytes(), 10)
                        .expect("lexer collected only decimal digits"),
                ),
            }
        }
    }

    fn read_string(&mut self) -> Token {
        self.advance(); // 跳过开始的引号
        let mut string = String::new();

        while let Some(ch) = self.current_char {
            if ch == '"' {
                self.advance(); // 跳过结束的引号
                break;
            } else if ch == '\\' {
                self.advance();
                // 处理转义字符
                if let Some(escaped) = self.current_char {
                    match escaped {
                        'n' => string.push('\n'),
                        't' => string.push('\t'),
                        'r' => string.push('\r'),
                        '\\' => string.push('\\'),
                        '"' => string.push('"'),
                        '\'' => string.push('\''),
                        _ => {
                            string.push('\\');
                            string.push(escaped);
                        }
                    }
                    self.advance();
                }
            } else {
                string.push(ch);
                self.advance();
            }
        }

        Token::String(string)
    }
    fn read_single_string(&mut self) -> Token {
        self.advance(); // 跳过开始的引号
        let mut string = String::new();

        while let Some(ch) = self.current_char {
            if ch == '\'' {
                self.advance(); // 跳过结束的引号
                break;
            } else if ch == '\\' {
                self.advance();
                // 处理转义字符
                if let Some(escaped) = self.current_char {
                    match escaped {
                        'n' => string.push('\n'),
                        't' => string.push('\t'),
                        'r' => string.push('\r'),
                        '\\' => string.push('\\'),
                        '"' => string.push('"'),
                        '\'' => string.push('\''),
                        _ => {
                            string.push('\\');
                            string.push(escaped);
                        }
                    }
                    self.advance();
                }
            } else {
                string.push(ch);
                self.advance();
            }
        }

        Token::String(string)
    }

    fn read_identifier(&mut self) -> Token {
        let mut ident = String::new();

        while let Some(ch) = self.current_char {
            if ch.is_alphanumeric() || ch == '_' {
                ident.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        // 检查是否为关键字
        match ident.as_str() {
            "var" => Token::Var,
            "let" => Token::Let,
            "const" => Token::Const,
            "bigint" => Token::BigInt,
            "struct" => Token::Struct,
            "func" => Token::Func,
            "return" => Token::Return,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "for" => Token::For,
            "loop" => Token::Loop,
            "break" => Token::Break,
            "continue" => Token::Continue,
            "include" => Token::Include,
            "import" => Token::Import,
            "use" => Token::Use,
            "as" => Token::As,
            "unit" => Token::Unit,
            "do" => Token::Do,
            "try" => Token::Try,
            "catch" => Token::Catch,
            "match" => Token::Match,
            "module" => Token::Module,
            "table" => Token::Table,
            "vec" => Token::Vec,
            "mat" => Token::Mat,
            "and" => Token::And,
            "or" => Token::Or,
            "not" => Token::Not,
            "in" => Token::In,
            "subset" => Token::Subset,
            "xor" => Token::Xor,
            "true" => Token::True,
            "false" => Token::False,
            "null" => Token::Null,
            // LSR-005: Type keywords
            "num" => Token::TypeNum,
            "int" => Token::TypeInt,
            "float" => Token::TypeFloat,
            "bool" => Token::TypeBool,
            "string" => Token::TypeString,
            "rational" => Token::TypeRational,
            "irrational" => Token::TypeIrrational,
            "complex" => Token::TypeComplex,
            "array" => Token::TypeArray,
            _ => Token::Ident(ident),
        }
    }

    pub fn next_token(&mut self) -> Token {
        loop {
            self.skip_whitespace();

            // 跳过注释
            if (self.current_char == Some('/')
                && (self.peek() == Some('/') || self.peek() == Some('*')))
                || self.current_char == Some('#')
            {
                self.skip_comment();
                continue;
            }

            break;
        }

        match self.current_char {
            None => Token::Eof,
            Some(ch) => match ch {
                '+' => {
                    self.advance();
                    Token::Plus
                }
                '-' => {
                    self.advance();
                    if self.current_char == Some('>') {
                        self.advance();
                        Token::Arrow
                    } else {
                        Token::Minus
                    }
                }
                '*' => {
                    self.advance();
                    Token::Star
                }
                '/' => {
                    self.advance();
                    Token::Slash
                }
                '%' => {
                    self.advance();
                    Token::Percent
                }
                '^' => {
                    self.advance();
                    Token::Caret
                }
                '?' => {
                    self.advance();
                    Token::Question
                }
                '!' => {
                    self.advance();
                    if self.current_char == Some('=') {
                        self.advance();
                        Token::BangEqual
                    } else {
                        Token::Bang
                    }
                }
                '=' => {
                    self.advance();
                    if self.current_char == Some('=') {
                        self.advance();
                        if self.current_char == Some('=') {
                            self.advance();
                            Token::EqualEqualEqual
                        } else {
                            Token::EqualEqual
                        }
                    } else if self.current_char == Some('>') {
                        self.advance();
                        Token::FatArrow
                    } else {
                        Token::Equal
                    }
                }
                '>' => {
                    self.advance();
                    if self.current_char == Some('=') {
                        self.advance();
                        Token::GreaterEqual
                    } else {
                        Token::Greater
                    }
                }
                '<' => {
                    self.advance();
                    if self.current_char == Some('=') {
                        self.advance();
                        Token::LessEqual
                    } else {
                        Token::Less
                    }
                }
                '&' => {
                    self.advance();
                    if self.current_char == Some('&') {
                        self.advance();
                        Token::Ident("&&".to_string())
                    } else {
                        Token::Ampersand
                    }
                }
                '|' => {
                    self.advance();
                    if self.current_char == Some('|') {
                        self.advance();
                        Token::Ident("||".to_string())
                    } else if self.current_char == Some('>') {
                        self.advance();
                        Token::PipeForward
                    } else {
                        Token::Pipe
                    }
                }
                ';' => {
                    self.advance();
                    Token::Semicolon
                }
                ',' => {
                    self.advance();
                    Token::Comma
                }
                '.' => {
                    self.advance();
                    if self.current_char == Some('+') {
                        self.advance();
                        Token::DotPlus
                    } else if self.current_char == Some('=') && self.peek() == Some('=') {
                        self.advance();
                        self.advance();
                        Token::DotEqualEqual
                    } else {
                        Token::Dot
                    }
                }
                ':' => {
                    self.advance();
                    if self.current_char == Some(':') {
                        self.advance();
                        Token::DoubleColon
                    } else {
                        Token::Colon
                    }
                }
                '\\' => {
                    self.advance();
                    Token::Backslash
                }
                '@' => {
                    self.advance();
                    Token::At
                }
                '(' => {
                    self.advance();
                    Token::LParen
                }
                ')' => {
                    self.advance();
                    Token::RParen
                }
                '{' => {
                    self.advance();
                    Token::LBrace
                }
                '}' => {
                    self.advance();
                    Token::RBrace
                }
                '[' => {
                    self.advance();
                    Token::LBracket
                }
                ']' => {
                    self.advance();
                    Token::RBracket
                }
                '\'' if self.position > 0 && self.input.get(self.position - 1) == Some(&'.') => {
                    self.advance();
                    Token::Apostrophe
                }
                '\'' => self.read_single_string(),
                '"' => self.read_string(),
                _ if ch.is_ascii_digit() => self.read_number(),
                _ if ch.is_alphabetic() || ch == '_' => self.read_identifier(),
                _ => {
                    eprintln!(
                        "Lexer error: Unexpected character '{}' (ASCII: {}). at ({},{})",
                        ch, ch as u32, self.line, self.col
                    );
                    eprintln!("Position: {}", self.position);
                    std::process::exit(1);
                }
            },
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();

            // LSR-009: 显式续行，仅处理反斜杠后紧跟换行（\n 或 \r\n）
            if token == Token::Backslash {
                if self.current_char == Some('\r') {
                    self.advance();
                    if self.current_char == Some('\n') {
                        self.advance();
                        continue;
                    }
                } else if self.current_char == Some('\n') {
                    self.advance();
                    continue;
                }
            }

            if token == Token::Eof {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }
        tokens
    }
}

#[cfg(test)]
#[allow(clippy::approx_constant)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_tokens() {
        let mut lexer = Lexer::new("var x = 10;".to_string());
        let tokens = lexer.tokenize();
        assert_eq!(tokens[0], Token::Var);
        assert_eq!(tokens[1], Token::Ident("x".to_string()));
        assert_eq!(tokens[2], Token::Equal);
        assert_eq!(tokens[3], Token::Int(10));
        assert_eq!(tokens[4], Token::Semicolon);
    }

    #[test]
    fn test_float() {
        let mut lexer = Lexer::new("3.14".to_string());
        let tokens = lexer.tokenize();
        assert_eq!(tokens[0], Token::Decimal("3.14".to_string()));
    }

    #[test]
    fn test_string() {
        let mut lexer = Lexer::new(r#""Hello, World!""#.to_string());
        let tokens = lexer.tokenize();
        assert_eq!(tokens[0], Token::String("Hello, World!".to_string()));
    }

    #[test]
    fn test_line_continuation_skips_newline() {
        let source = "var x = 1\\\n + 2;".replace("\\n", "\n");
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0], Token::Var);
        assert_eq!(tokens[1], Token::Ident("x".to_string()));
        assert_eq!(tokens[2], Token::Equal);
        assert_eq!(tokens[3], Token::Int(1));
        assert_eq!(tokens[4], Token::Plus);
        assert_eq!(tokens[5], Token::Int(2));
        assert_eq!(tokens[6], Token::Semicolon);
    }

    #[test]
    fn test_line_continuation_skips_crlf() {
        let mut lexer = Lexer::new("var x = 1\\\r\n + 2;".to_string());
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0], Token::Var);
        assert_eq!(tokens[1], Token::Ident("x".to_string()));
        assert_eq!(tokens[2], Token::Equal);
        assert_eq!(tokens[3], Token::Int(1));
        assert_eq!(tokens[4], Token::Plus);
        assert_eq!(tokens[5], Token::Int(2));
        assert_eq!(tokens[6], Token::Semicolon);
    }
}
