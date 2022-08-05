use crate::error::ParserError;

use self::{lexical::TextSpan, semantic::ScriptToken};

mod lexical;
mod semantic;

pub fn parse_script(i: &str) -> Vec<Result<ScriptToken, ParserError>> {
    let text_span = TextSpan::new_str(i);

    match lexical::recognize_blocks(text_span) {
        Ok(v) => {
            let mut result_vec: Vec<Result<ScriptToken, ParserError>> = vec![];
            for b in v {
                match b.block_type() {
                    lexical::BlockType::Label => {
                        result_vec.push(match lexical::parse_label(b.block_content()) {
                            Ok(l) => Ok(ScriptToken::Label(l)),
                            Err(e) => Err(e),
                        })
                    }
                    lexical::BlockType::Command => {
                        result_vec.push(match lexical::parse_command(b.block_content()) {
                            Ok(c) => Ok(ScriptToken::Command(c)),
                            Err(e) => Err(e),
                        })
                    }
                    lexical::BlockType::ControlSymbol => {
                        result_vec.push(match lexical::parse_control_symbol(b.block_content()) {
                            Ok(c) => Ok(ScriptToken::ControlSymbol(c)),
                            Err(e) => Err(e),
                        })
                    }
                    lexical::BlockType::NormalText => {
                        result_vec.push(match lexical::parse_normal_text(b.block_content()) {
                            Ok(n) => Ok(ScriptToken::NormalText(n)),
                            Err(e) => Err(e),
                        })
                    }
                    lexical::BlockType::Whitespace => continue,
                }
            }
            result_vec
        }
        Err(e) => return vec![Err(e)],
    }
}
