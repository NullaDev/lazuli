use std::collections::HashMap;

#[derive(Clone)]
pub struct Label {
    label: String,
}

impl Label {
    pub fn construct(label: String) -> Self {
        Self { label }
    }

    pub fn from_str(label: &str) -> Self {
        Self {
            label: label.to_string(),
        }
    }

    pub fn label(&self) -> &str {
        self.label.as_ref()
    }
}

#[derive(Clone)]
pub enum CommandArgument {
    Integer(i32),
    Float(f64),
    Str(String),
}

#[derive(Clone)]
pub struct Command {
    name: String,
    args: HashMap<String, CommandArgument>,
}

impl Command {
    pub fn construct(name: &str, args: Vec<(String, CommandArgument)>) -> Self {
        Self {
            name: name.to_string(),
            args: {
                let mut hash_map = HashMap::new();
                for (k, v) in args {
                    hash_map.insert(k, v);
                }
                hash_map
            },
        }
    }
}

#[derive(Clone)]
pub struct ControlSymbol {
    symbol: String,
}

impl ControlSymbol {
    pub fn construct(symbol: &str) -> Self {
        Self {
            symbol: symbol.to_string(),
        }
    }
}

#[derive(Clone)]
pub struct NormalText {
    text: String,
}

impl NormalText {
    pub fn construct(text: &str) -> Self {
        Self {
            text: text.to_string(),
        }
    }
}

#[derive(Clone)]
pub enum ScriptToken {
    Label(Label),
    Command(Command),
    ControlSymbol(ControlSymbol),
    NormalText(NormalText),
}
