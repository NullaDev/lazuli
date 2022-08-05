#[derive(Clone, Copy)]
pub enum ParserErrorKind {
    EmptyInput,
    NoMatched,
    PairError,
    MapError,
    BranchError,
    ContiguousError,
    BackError,
    FrontError,
    SeperatedError,
    BetweenError,
    PluralError,
    EofError,
    ErrorMessage(&'static str),
}

impl ToString for ParserErrorKind {
    fn to_string(&self) -> String {
        match self {
            ParserErrorKind::EmptyInput => String::from("Empty Input"),
            ParserErrorKind::NoMatched => String::from("No Matched"),
            ParserErrorKind::PairError => String::from("Pair Error"),
            ParserErrorKind::MapError => String::from("Map Error"),
            ParserErrorKind::BranchError => String::from("Branch Error"),
            ParserErrorKind::ContiguousError => String::from("Contiguous Error"),
            ParserErrorKind::BackError => String::from("Back Error"),
            ParserErrorKind::FrontError => String::from("Front Error"),
            ParserErrorKind::SeperatedError => String::from("Seperated Error"),
            ParserErrorKind::BetweenError => String::from("Between Error"),
            ParserErrorKind::PluralError => String::from("Plural Error"),
            ParserErrorKind::EofError => String::from("Eof Error"),
            ParserErrorKind::ErrorMessage(msg) => msg.to_string(),
        }
    }
}

#[derive(Clone)]
pub struct ParserError {
    errors: Vec<ParserErrorKind>,
    position: (usize, usize),
    line: String,
}

impl ParserError {
    pub fn new(error: ParserErrorKind, position: (usize, usize), line: String) -> Self {
        Self {
            errors: vec![error],
            position,
            line,
        }
    }

    pub fn append(&mut self, error: ParserErrorKind) {
        self.errors.push(error)
    }

    pub fn concat(&mut self, errors: &[ParserErrorKind]) {
        self.errors.append(&mut errors.to_vec())
    }

    pub fn errors(&self) -> &[ParserErrorKind] {
        self.errors.as_ref()
    }

    pub fn position(&self) -> (usize, usize) {
        self.position
    }

    pub fn line(&self) -> &str {
        self.line.as_ref()
    }
}
