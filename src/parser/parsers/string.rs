use combine::combinator::*;
use combine::{ConsumedResult, ParseError, ParseResult, Parser, Stream};
use std::marker::PhantomData;
use magic;
use num::ToPrimitive;
use std::iter::FromIterator;
use std::str::Chars;
use super::*;

/// Parses a numeric operator.
pub fn numeric_operator<I: Stream<Item = char>>() -> NumericOperator<I> {
    NumericOperator(one_of("=<>!".chars()), PhantomData)
}

pub struct NumericOperator<I>(OneOf<Chars<'static>, I>, PhantomData<fn(I) -> I>)
    where I: Stream<Item = char>;

impl<I: Stream<Item = char>> Parser for NumericOperator<I> {
    type Input = I;
    type Output = magic::NumOp;

    fn parse_lazy(&mut self, input: Self::Input) -> ConsumedResult<Self::Output, Self::Input> {
        self.0.parse_lazy(input).map(|c| {
            match c {
                '=' => magic::NumOp::Equal,
                '<' => magic::NumOp::LessThan,
                '>' => magic::NumOp::GreaterThan,
                '!' => magic::NumOp::NotEqual,
                _ => unreachable!("Invalid numeric operator: {:?}", c),
            }
        })
    }

    fn add_error(&mut self, errors: &mut ParseError<Self::Input>) {
        self.0.add_error(errors)
    }
}

/// Parses a string operator.
pub fn string_operator<I: Stream<Item = char>>() -> StringOperator<I> {
    StringOperator(one_of("=<>".chars()), PhantomData)
}

pub struct StringOperator<I>(OneOf<Chars<'static>, I>, PhantomData<fn(I) -> I>)
    where I: Stream<Item = char>;

impl<I: Stream<Item = char>> Parser for StringOperator<I> {
    type Input = I;
    type Output = magic::StringOp;

    fn parse_lazy(&mut self, input: Self::Input) -> ConsumedResult<Self::Output, Self::Input> {
        self.0.parse_lazy(input).map(|c| {
            match c {
                '=' => magic::StringOp::Equal,
                '<' => magic::StringOp::LexBefore,
                '>' => magic::StringOp::LexAfter,
                _ => unreachable!("Invalid string operator: {:?}", c),
            }
        })
    }

    fn add_error(&mut self, errors: &mut ParseError<Self::Input>) {
        self.0.add_error(errors)
    }
}

pub fn escape_sequence<I>() -> EscapeSequence<I>
    where I: Stream<Item = char>
{
    EscapeSequence {
        named_char: one_of("\\nrt".chars()),
        hex_escape: token('x').with(hex_integer()),
        oct_escape: oct_integer(),
    }
}

pub struct EscapeSequence<I>
    where I: Stream<Item = char>,
{
    named_char: OneOf<Chars<'static>, I>,
    hex_escape: With<Token<I>, HexInteger<I>>,
    oct_escape: OctInteger<I>,
}

impl<I> Parser for EscapeSequence<I>
    where I: Stream<Item = char>
{
    type Input = I;
    type Output = char;

    fn parse_stream(&mut self, input: I) -> ParseResult<Self::Output, Self::Input> {
        let named_parser = self.named_char.by_ref().map(|s| {
            match s {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                _ => unreachable!("Invalid escape sequence: {:?}", s),
            }
        });
        let hex_parser = self.hex_escape.by_ref().map(|n| From::from(n.to_u8().unwrap()));
        let oct_parser = self.oct_escape.by_ref().map(|n| From::from(n.to_u8().unwrap()));
        token('\\').with(named_parser.or(hex_parser).or(oct_parser)).parse_stream(input)
    }
}

pub fn escaped_string<F, I>() -> EscapedString<F, I>
    where I: Stream<Item = char>,
          F: FromIterator<char>,
{
    EscapedString(many::<F, _>(or(
        escape_sequence(),
        none_of(" ".chars())
    )))
}

pub struct EscapedString<F, I>(Many<F, Or<EscapeSequence<I>, NoneOf<Chars<'static>, I>>>)
    where I: Stream<Item = char>,
          F: FromIterator<char>;

impl<F, I> Parser for EscapedString<F, I>
    where I: Stream<Item = char>,
          F: FromIterator<char>
{
    type Input = I;
    type Output = F;

    fn parse_lazy(&mut self, input: Self::Input) -> ConsumedResult<Self::Output, Self::Input> {
        self.0.parse_lazy(input)
    }

    fn add_error(&mut self, errors: &mut ParseError<Self::Input>) {
        self.0.add_error(errors)
    }
}

#[cfg(test)]
mod tests {
    use combine::Parser;

    #[test]
    fn escaped_strings() {
        assert_eq!(Ok((String::from("fmt "), "")), super::escaped_string::<String, _>().parse("fmt\\x20"));
    }

    #[test]
    fn numerical_operators() {
        use magic::NumOp::*;

        assert_eq!(Ok((Equal, "")),       super::numeric_operator().parse("="));
        assert_eq!(Ok((GreaterThan, "")), super::numeric_operator().parse(">"));
        assert_eq!(Ok((LessThan, "")),    super::numeric_operator().parse("<"));
        assert_eq!(Ok((NotEqual, "")),    super::numeric_operator().parse("!"));
    }

    #[test]
    fn string_operators() {
        use magic::StringOp::*;

        assert_eq!(Ok((Equal,     "")), super::string_operator().parse("="));
        assert_eq!(Ok((LexBefore, "")), super::string_operator().parse("<"));
        assert_eq!(Ok((LexAfter,  "")), super::string_operator().parse(">"));
    }

    #[test]
    fn escaped_chars() {
        assert_eq!(Ok(('\n', "")), super::escape_sequence().parse("\\n"));
        assert_eq!(Ok(('\r', "")), super::escape_sequence().parse("\\r"));
        assert_eq!(Ok(('\t', "")), super::escape_sequence().parse("\\t"));
        assert_eq!(Ok(('\\', "")), super::escape_sequence().parse("\\\\"));
        assert_eq!(Ok(('\0', "")), super::escape_sequence().parse("\\0"));
        assert_eq!(Ok(('\x0E', "")), super::escape_sequence().parse("\\016"));
        assert_eq!(Ok((' ', "")), super::escape_sequence().parse("\\x20"));
    }
}
