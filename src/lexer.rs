use crate::token::Token;

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    current_char: Option<char>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let chars: Vec<char> = input.chars().collect();
        let current_char = chars.get(0).copied();
        Lexer {
            input: chars,
            position: 0,
            current_char,
        }
    }

    fn advance(&mut self) {
        self.position += 1;
        self.current_char = self.input.get(self.position).copied();
    }

    fn peek(&self) -> Option<char> {
        self.input.get(self.position + 1).copied()
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
            } else if ch == '.' && !is_float && self.peek().map_or(false, |c| c.is_ascii_digit()) {
                is_float = true;
                num_str.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        if is_float {
            Token::Float(num_str.parse().unwrap())
        } else {
            Token::Int(num_str.parse().unwrap())
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
            "do" => Token::Do,
            "true" => Token::True,
            "false" => Token::False,
            "null" => Token::Null,
            // LSR-005: Type keywords
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
            if self.current_char == Some('/')
                && (self.peek() == Some('/') || self.peek() == Some('*'))
            {
                self.skip_comment();
                continue;
            }

            // 跳过续行符（支持 \n 和 \r\n）
            if self.current_char == Some('\\') {
                let next = self.peek();
                if next == Some('\n') || next == Some('\r') {
                    self.advance(); // 跳过 \
                    if self.current_char == Some('\r') {
                        self.advance(); // 跳过 \r
                    }
                    if self.current_char == Some('\n') {
                        self.advance(); // 跳过 \n
                    }
                    continue; // 继续处理，会再次跳过空格
                }
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
                        Token::EqualEqual
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
                        Token::And
                    } else {
                        eprintln!(
                            "Lexer error: Expected '&' after '&', found {:?}",
                            self.current_char
                        );
                        eprintln!("Note: Lamina uses '&&' for logical AND");
                        std::process::exit(1);
                    }
                }
                '|' => {
                    self.advance();
                    if self.current_char == Some('|') {
                        self.advance();
                        Token::Or
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
                    Token::Dot
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
                '"' => self.read_string(),
                _ if ch.is_ascii_digit() => self.read_number(),
                _ if ch.is_alphabetic() || ch == '_' => self.read_identifier(),
                _ => {
                    eprintln!(
                        "Lexer error: Unexpected character '{}' (ASCII: {})",
                        ch, ch as u32
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
        assert_eq!(tokens[0], Token::Float(3.14));
    }

    #[test]
    fn test_string() {
        let mut lexer = Lexer::new(r#""Hello, World!""#.to_string());
        let tokens = lexer.tokenize();
        assert_eq!(tokens[0], Token::String("Hello, World!".to_string()));
    }
}
