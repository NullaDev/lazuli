use crate::error::{ParserError, ParserErrorKind};

use super::semantic::{Command, CommandArgument, ControlSymbol, Label, NormalText};

/// 一段文本及其所在范围。
/// 包含对应的字符切片引用，所在行号和在该行的起始位置。
#[derive(Clone, Copy)]
pub struct TextSpan<'a> {
    line: usize,
    column: usize,
    text: &'a str,
}

impl<'a> TextSpan<'a> {
    /// 构造函数
    pub fn construct(line: usize, offset: usize, text: &'a str) -> Self {
        Self {
            line,
            column: offset,
            text,
        }
    }

    /// 获取该文本所在行号
    pub fn line(&self) -> usize {
        self.line
    }

    /// 获取该文本起始的列号
    pub fn column(&self) -> usize {
        self.column
    }

    /// 获取该段文本的字符切片
    pub fn text(&self) -> &str {
        &self.text
    }

    /// 从一段新的文本构造结构体
    pub fn new_str(i: &'a str) -> Self {
        Self {
            line: 1,
            column: 0,
            text: i,
        }
    }

    /// 获取当前文本位置（即行号，与文本起始处的列号）
    pub fn position(&self) -> (usize, usize) {
        (self.line, self.column)
    }

    /// 当前文本是否为空
    pub fn is_empty(&self) -> bool {
        self.text.is_empty()
    }

    /// 当前文本包含的有效Unicode字符数
    pub fn char_count(&self) -> usize {
        self.text.chars().count()
    }

    /// 当前文本的字节长度
    pub fn len(&self) -> usize {
        self.text.len()
    }

    /// 对应于&str的split_at函数
    pub fn split_at(&self, pos: usize) -> (Self, Self) {
        let (front, back) = self.text.split_at(pos);
        let front_span = Self::construct(self.line, self.column, front);

        let new_line = self.line + front.matches("\n").collect::<Vec<&str>>().len();

        let new_offset = match front.rfind("\n") {
            Some(occurence) => occurence,
            None => self.column + front.len(),
        };

        let back_span = Self::construct(new_line, new_offset, back);

        (front_span, back_span)
    }

    /// 带有一定条件的split_at函数，而不是要求一个固定的usize位置
    pub fn split_on_condition<F>(&self, f: F) -> Result<(Self, Self), ParserError>
    where
        F: Fn(char) -> bool,
    {
        if self.is_empty() {
            Err(ParserError::new(
                ParserErrorKind::NoMatched,
                self.position(),
                self.current_line().to_string(),
            ))
        } else {
            match self.text.find(|c| !f(c)) {
                Some(0) => Err(ParserError::new(
                    ParserErrorKind::NoMatched,
                    self.position(),
                    self.current_line().to_string(),
                )),
                Some(pos) => Ok(self.split_at(pos)),
                None => Ok(self.split_at(self.len())),
            }
        }
    }

    /// 根据要求的字符串，从该字符串的起始位置分割文本
    pub fn split_str(&self, pat: &str) -> Result<(Self, Self), ParserError> {
        if self.is_empty() || pat.is_empty() {
            Err(ParserError::new(
                ParserErrorKind::NoMatched,
                self.position(),
                self.current_line().to_string(),
            ))
        } else if self.text.starts_with(pat) {
            let pos = pat.len();
            Ok(self.split_at(pos))
        } else {
            Err(ParserError::new(
                ParserErrorKind::NoMatched,
                self.position(),
                self.current_line().to_string(),
            ))
        }
    }

    /// 获取文本第一行的内容。
    pub fn current_line(&self) -> &str {
        let text = self.text;
        let (current_line, _) = text.split_at(match self.text.find('\n') {
            Some(pos) => pos,
            None => text.len(),
        });
        current_line
    }
}

/// 附加一个错误信息，不影响匹配
fn err_msg<'a, F, O>(
    mut f: F,
    msg: &'static str,
) -> impl FnMut(TextSpan<'a>) -> Result<(O, TextSpan<'a>), ParserError>
where
    F: FnMut(TextSpan<'a>) -> Result<(O, TextSpan<'a>), ParserError>,
{
    move |i| match f(i) {
        Ok(result) => Ok(result),
        Err(mut e) => {
            e.append(ParserErrorKind::ErrorMessage(msg));
            Err(e)
        }
    }
}

/// 从文本开始处提取满足条件的文本
fn take_while<F>(f: F) -> impl FnMut(TextSpan) -> Result<(TextSpan, TextSpan), ParserError>
where
    F: Fn(char) -> bool,
{
    move |i| i.split_on_condition(|c| f(c))
}

/// 从文本开始处提取文本直到满足条件
fn take_till<F>(f: F) -> impl FnMut(TextSpan) -> Result<(TextSpan, TextSpan), ParserError>
where
    F: Fn(char) -> bool,
{
    move |i| i.split_on_condition(|c| !f(c))
}

/// 从文本开始处提取要求的字符串
fn take_str(
    pat: &'static str,
) -> impl FnMut(TextSpan) -> Result<(TextSpan, TextSpan), ParserError> {
    move |i| i.split_str(pat)
}

/// 将两个Parser按顺序前后组合
fn pair<'a, F, G, O1, O2>(
    mut f: F,
    mut g: G,
) -> impl FnMut(TextSpan<'a>) -> Result<((O1, O2), TextSpan<'a>), ParserError>
where
    F: FnMut(TextSpan<'a>) -> Result<(O1, TextSpan<'a>), ParserError>,
    G: FnMut(TextSpan<'a>) -> Result<(O2, TextSpan<'a>), ParserError>,
{
    move |i| match f(i) {
        Ok((first, left)) => match g(left) {
            Ok((second, left)) => Ok(((first, second), left)),
            Err(mut e) => {
                e.append(ParserErrorKind::PairError);
                Err(e)
            }
        },
        Err(mut e) => {
            e.append(ParserErrorKind::PairError);
            Err(e)
        }
    }
}

/// 按照顺序，先后尝试用所给的两个Parser进行匹配。
/// 若有一个成功就返回该成功的结果。
fn branch<'a, F, G, O>(
    mut f: F,
    mut g: G,
) -> impl FnMut(TextSpan<'a>) -> Result<(O, TextSpan<'a>), ParserError>
where
    F: FnMut(TextSpan<'a>) -> Result<(O, TextSpan<'a>), ParserError>,
    G: FnMut(TextSpan<'a>) -> Result<(O, TextSpan<'a>), ParserError>,
{
    move |i| match f(i) {
        Ok((matched, left)) => Ok((matched, left)),
        Err(e1) => match g(i) {
            Ok((matched, left)) => Ok((matched, left)),
            Err(mut e2) => {
                e2.concat(e1.errors());
                e2.append(ParserErrorKind::BranchError);
                Err(e2)
            }
        },
    }
}

/// 获取所给Parser成功匹配得到的文本
fn contiguous<F, G>(
    mut f: F,
    mut g: G,
) -> impl FnMut(TextSpan) -> Result<(TextSpan, TextSpan), ParserError>
where
    F: FnMut(TextSpan) -> Result<(TextSpan, TextSpan), ParserError>,
    G: FnMut(TextSpan) -> Result<(TextSpan, TextSpan), ParserError>,
{
    move |i| match f(i) {
        Ok((f_matched, left)) => match g(left) {
            Ok((g_matched, _)) => {
                let pos = f_matched.text().len() + g_matched.text().len();
                Ok(i.split_at(pos))
            }
            Err(mut e) => {
                e.append(ParserErrorKind::ContiguousError);
                Err(e)
            }
        },
        Err(mut e) => {
            e.append(ParserErrorKind::ContiguousError);
            Err(e)
        }
    }
}

/// 用两个Parser进行匹配，然后丢弃前一个Parser匹配的内容，返回后一个Parser匹配的内容
fn back<'a, F, G, O1, O2>(
    mut f: F,
    mut g: G,
) -> impl FnMut(TextSpan<'a>) -> Result<(O2, TextSpan<'a>), ParserError>
where
    F: FnMut(TextSpan<'a>) -> Result<(O1, TextSpan<'a>), ParserError>,
    G: FnMut(TextSpan<'a>) -> Result<(O2, TextSpan<'a>), ParserError>,
{
    move |i| match f(i) {
        Ok((_, left)) => match g(left) {
            Ok((matched, left)) => Ok((matched, left)),
            Err(mut e) => {
                e.append(ParserErrorKind::BackError);
                Err(e)
            }
        },
        Err(mut e) => {
            e.append(ParserErrorKind::BackError);
            Err(e)
        }
    }
}

/// 用两个Parser进行匹配，然后丢弃后一个Parser匹配的内容，返回前一个Parser匹配的内容
fn front<'a, F, G, O1, O2>(
    mut f: F,
    mut g: G,
) -> impl FnMut(TextSpan<'a>) -> Result<(O1, TextSpan), ParserError>
where
    F: FnMut(TextSpan<'a>) -> Result<(O1, TextSpan), ParserError>,
    G: FnMut(TextSpan<'a>) -> Result<(O2, TextSpan), ParserError>,
{
    move |i| match f(i) {
        Ok((matched, left)) => match g(left) {
            Ok((_, left)) => Ok((matched, left)),
            Err(mut e) => {
                e.append(ParserErrorKind::FrontError);
                Err(e)
            }
        },
        Err(mut e) => {
            e.append(ParserErrorKind::FrontError);
            Err(e)
        }
    }
}

/// 用三个Parser进行匹配，然后丢弃第二个Parser匹配的内容，返回前一个与后一个Parser匹配的内容
fn seperated<'a, F1, G, F2, O1, O2, O3>(
    mut f1: F1,
    mut g: G,
    mut f2: F2,
) -> impl FnMut(TextSpan<'a>) -> Result<((O1, O3), TextSpan<'a>), ParserError>
where
    F1: FnMut(TextSpan<'a>) -> Result<(O1, TextSpan<'a>), ParserError>,
    G: FnMut(TextSpan<'a>) -> Result<(O2, TextSpan<'a>), ParserError>,
    F2: FnMut(TextSpan<'a>) -> Result<(O3, TextSpan<'a>), ParserError>,
{
    move |i| match f1(i) {
        Ok((f1_matched, left)) => match g(left) {
            Ok((_, left)) => match f2(left) {
                Ok((f2_matched, left)) => Ok(((f1_matched, f2_matched), left)),
                Err(mut e) => {
                    e.append(ParserErrorKind::SeperatedError);
                    Err(e)
                }
            },
            Err(mut e) => {
                e.append(ParserErrorKind::SeperatedError);
                Err(e)
            }
        },
        Err(mut e) => {
            e.append(ParserErrorKind::SeperatedError);
            Err(e)
        }
    }
}

/// 用三个Parser进行匹配，然后丢弃前一个与后一个Parser匹配的内容，返回第二个Parser匹配的内容
fn between<'a, F1, G, F2, O1, O2, O3>(
    mut f1: F1,
    mut g: G,
    mut f2: F2,
) -> impl FnMut(TextSpan<'a>) -> Result<(O2, TextSpan<'a>), ParserError>
where
    F1: FnMut(TextSpan<'a>) -> Result<(O1, TextSpan<'a>), ParserError>,
    G: FnMut(TextSpan<'a>) -> Result<(O2, TextSpan<'a>), ParserError>,
    F2: FnMut(TextSpan<'a>) -> Result<(O3, TextSpan<'a>), ParserError>,
{
    move |i| match f1(i) {
        Ok((_, left)) => match g(left) {
            Ok((g_matched, left)) => match f2(left) {
                Ok((_, left)) => Ok((g_matched, left)),
                Err(mut e) => {
                    e.append(ParserErrorKind::BetweenError);
                    Err(e)
                }
            },
            Err(mut e) => {
                e.append(ParserErrorKind::BetweenError);
                Err(e)
            }
        },
        Err(mut e) => {
            e.append(ParserErrorKind::BetweenError);
            Err(e)
        }
    }
}

/// 对文本重复不断地用所给Parser进行匹配，返回匹配得到的结果向量
fn plural<'a, F, O>(
    mut f: F,
) -> impl FnMut(TextSpan<'a>) -> Result<(Vec<O>, TextSpan<'a>), ParserError>
where
    F: FnMut(TextSpan<'a>) -> Result<(O, TextSpan<'a>), ParserError>,
{
    move |i| match f(i) {
        Err(mut e) => {
            e.append(ParserErrorKind::PluralError);
            Err(e)
        }
        Ok((matched, left)) => {
            let mut res = vec![matched];
            let mut matching = left;
            loop {
                match f(matching) {
                    Ok((matched, left)) => {
                        if matching.len() == left.len() {
                            return Err(ParserError::new(
                                ParserErrorKind::PluralError,
                                matching.position(),
                                matching.current_line().to_string(),
                            ));
                        }

                        res.push(matched);
                        matching = left;
                    }
                    Err(_) => return Ok((res, matching)),
                }
            }
        }
    }
}

/// 要求所给Parser完全匹配文本，若匹配成功后还有剩余文本，则返回失败。
fn complete<'a, F, O>(mut f: F) -> impl FnMut(TextSpan<'a>) -> Result<(O, TextSpan), ParserError>
where
    F: FnMut(TextSpan<'a>) -> Result<(O, TextSpan<'a>), ParserError>,
{
    move |i| match f(i) {
        Ok((matched, left)) => {
            if !left.is_empty() {
                Err(ParserError::new(
                    ParserErrorKind::EofError,
                    i.position(),
                    i.current_line().to_string(),
                ))
            } else {
                Ok((matched, left))
            }
        }
        Err(e) => Err(e),
    }
}

/// 将所给Parser匹配到的文本进行转换。
fn map<'a, F, G, O>(
    mut f: F,
    mut g: G,
) -> impl FnMut(TextSpan<'a>) -> Result<(O, TextSpan<'a>), ParserError>
where
    F: FnMut(TextSpan<'a>) -> Result<(TextSpan<'a>, TextSpan<'a>), ParserError>,
    G: FnMut(TextSpan<'a>) -> O,
{
    move |i| {
        let (m, left) = f(i)?;
        Ok((g(m), left))
    }
}

/// 匹配不在给定的字符串中的字符。
fn not_in(pat: &'static str) -> impl FnMut(TextSpan) -> Result<(TextSpan, TextSpan), ParserError> {
    take_till(|c| pat.contains(c))
}

/// 匹配Unicode中的Alphabetic字符，包括字母，汉字，片假名等
fn alphabetic(i: TextSpan) -> Result<(TextSpan, TextSpan), ParserError> {
    err_msg(
        take_while(|c| c.is_alphabetic()),
        "Match alphabetic characters error!",
    )(i)
}

/// 匹配0～9字符
fn digit(i: TextSpan) -> Result<(TextSpan, TextSpan), ParserError> {
    err_msg(
        take_while(|c| c.is_digit(10)),
        "Match digit characters error!",
    )(i)
}

/// 匹配ASCII空白字符
fn whitespace(i: TextSpan) -> Result<(TextSpan, TextSpan), ParserError> {
    err_msg(
        take_while(|c| c.is_ascii_whitespace()),
        "Match whitespace characters error!",
    )(i)
}

#[derive(Clone, Copy)]
pub enum BlockType {
    Label,
    Command,
    ControlSymbol,
    NormalText,
    Whitespace,
}

impl ToString for BlockType {
    fn to_string(&self) -> String {
        match self {
            BlockType::Label => String::from("Label"),
            BlockType::Command => String::from("Command"),
            BlockType::ControlSymbol => String::from("ControlSymbol"),
            BlockType::NormalText => String::from("NormalText"),
            BlockType::Whitespace => String::from("Whitespace"),
        }
    }
}

pub struct TypedBlock<'a> {
    block_type: BlockType,
    block_content: TextSpan<'a>,
}

impl<'a> TypedBlock<'a> {
    pub fn new(block_type: BlockType, block_content: TextSpan<'a>) -> Self {
        Self {
            block_type,
            block_content,
        }
    }

    pub fn label(block_content: TextSpan<'a>) -> Self {
        Self {
            block_type: BlockType::Label,
            block_content,
        }
    }

    pub fn command(block_content: TextSpan<'a>) -> Self {
        Self {
            block_type: BlockType::Command,
            block_content,
        }
    }

    pub fn control_symbol(block_content: TextSpan<'a>) -> Self {
        Self {
            block_type: BlockType::ControlSymbol,
            block_content,
        }
    }

    pub fn normal_text(block_content: TextSpan<'a>) -> Self {
        Self {
            block_type: BlockType::NormalText,
            block_content,
        }
    }

    pub fn whitespace(block_content: TextSpan<'a>) -> Self {
        Self {
            block_type: BlockType::Whitespace,
            block_content,
        }
    }

    pub fn block_type(&self) -> BlockType {
        self.block_type
    }

    pub fn block_content(&self) -> TextSpan {
        self.block_content
    }
}

fn recognize_label_block(i: TextSpan) -> Result<(TypedBlock, TextSpan), ParserError> {
    map(
        err_msg(
            contiguous(take_str("#"), take_till(|c| c == '\n')),
            "Recognize label block error!",
        ),
        TypedBlock::label,
    )(i)
}

fn recognize_command_block(i: TextSpan) -> Result<(TypedBlock, TextSpan), ParserError> {
    map(
        err_msg(
            contiguous(take_str("@"), take_till(|c| c == '\n')),
            "Recognize command block error!",
        ),
        TypedBlock::command,
    )(i)
}

fn recognize_control_symbol(i: TextSpan) -> Result<(TypedBlock, TextSpan), ParserError> {
    map(
        err_msg(
            contiguous(
                take_str("["),
                contiguous(take_till(|c| c == ']'), take_str("]")),
            ),
            "Recognize control symbol error!",
        ),
        TypedBlock::control_symbol,
    )(i)
}

fn recognize_normal_text_block(i: TextSpan) -> Result<(TypedBlock, TextSpan), ParserError> {
    map(
        err_msg(take_till(|c| c == '['), "Recognize normak text error!"),
        TypedBlock::normal_text,
    )(i)
}

fn recognize_whitespace(i: TextSpan) -> Result<(TypedBlock, TextSpan), ParserError> {
    map(
        err_msg(whitespace, "Recognize whitespace error!"),
        TypedBlock::whitespace,
    )(i)
}

pub fn recognize_blocks(i: TextSpan) -> Result<Vec<TypedBlock>, ParserError> {
    match complete(plural(branch(
        recognize_label_block,
        branch(
            recognize_command_block,
            branch(
                recognize_control_symbol,
                branch(recognize_whitespace, recognize_normal_text_block),
            ),
        ),
    )))(i)
    {
        Ok((v, _)) => Ok(v),
        Err(e) => Err(e),
    }
}

pub fn parse_label(i: TextSpan) -> Result<Label, ParserError> {
    match complete(back(take_str("#"), alphabetic))(i) {
        Ok((matched, _)) => Ok(Label::from_str(matched.text())),
        Err(e) => Err(e),
    }
}

fn parse_command_int_arg(i: TextSpan) -> Result<(CommandArgument, TextSpan), ParserError> {
    match err_msg(digit, "Recognize integer error!")(i) {
        Ok((matched, left)) => match matched.text().parse() {
            Ok(value) => Ok((CommandArgument::Integer(value), left)),
            Err(_) => Err(ParserError::new(
                ParserErrorKind::ErrorMessage(
                    "Parsing integer error! The number is out of the range of i32.",
                ),
                i.position(),
                i.current_line().to_string(),
            )),
        },
        Err(e) => Err(e),
    }
}

fn parse_command_float_arg(i: TextSpan) -> Result<(CommandArgument, TextSpan), ParserError> {
    match err_msg(
        contiguous(digit, contiguous(take_str("."), digit)),
        "Recognize float error!",
    )(i)
    {
        Ok((matched, left)) => match matched.text().parse() {
            Ok(value) => Ok((CommandArgument::Float(value), left)),
            Err(_) => Err(ParserError::new(
                ParserErrorKind::ErrorMessage(
                    "Parsing float number error! The number is out of the range of f64.",
                ),
                i.position(),
                i.current_line().to_string(),
            )),
        },
        Err(e) => Err(e),
    }
}

fn parse_command_str_arg(i: TextSpan) -> Result<(CommandArgument, TextSpan), ParserError> {
    match err_msg(
        between(take_str("\""), take_till(|c| c == '\"'), take_str("\"")),
        "Recognize string error!",
    )(i)
    {
        Ok((matched, left)) => Ok((CommandArgument::Str(matched.text().to_string()), left)),
        Err(e) => Err(e),
    }
}

fn parse_command_arg(i: TextSpan) -> Result<((String, CommandArgument), TextSpan), ParserError> {
    match err_msg(
        back(
            whitespace,
            seperated(
                alphabetic,
                take_str("="),
                branch(
                    parse_command_str_arg,
                    branch(parse_command_float_arg, parse_command_int_arg),
                ),
            ),
        ),
        "Parse command argument error!",
    )(i)
    {
        Ok(((arg_name, arg_value), left)) => Ok(((arg_name.text().to_string(), arg_value), left)),
        Err(e) => Err(e),
    }
}

pub fn parse_command(i: TextSpan) -> Result<Command, ParserError> {
    match err_msg(
        complete(back(
            take_str("@"),
            pair(alphabetic, plural(parse_command_arg)),
        )),
        "Parse command error!",
    )(i)
    {
        Ok(((name, args), _)) => Ok(Command::construct(name.text(), args)),
        Err(e) => Err(e),
    }
}

pub fn parse_control_symbol(i: TextSpan) -> Result<ControlSymbol, ParserError> {
    match err_msg(
        complete(between(take_str("["), alphabetic, take_str("]"))),
        "Parse control symbol error!",
    )(i)
    {
        Ok((matched, _)) => Ok(ControlSymbol::construct(matched.text())),
        Err(e) => Err(e),
    }
}

pub fn parse_normal_text(i: TextSpan) -> Result<NormalText, ParserError> {
    match err_msg(
        complete(take_till(|c| c == '[')),
        "Parse normal text error!",
    )(i)
    {
        Ok((matched, _)) => Ok(NormalText::construct(matched.text())),
        Err(e) => Err(e),
    }
}
