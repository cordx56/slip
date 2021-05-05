use super::{
    Program,
    Expression,
    List,
    Atom,
    Constant,
};
use super::define;
extern crate nom;

use nom::{
    IResult,
    character::complete::{
        space0,
        space1,
        multispace0,
        multispace1,
        none_of,
        char,
        line_ending,
    },
    bytes::complete::{
        tag,
        is_not,
        escaped_transform,
        take_while_m_n,
    },
    number::complete::{
        double,
    },
    branch::{
        alt,
        permutation,
    },
    combinator::{
        opt,
        map,
        value,
        all_consuming,
    },
    multi::{
        many0,
        many1,
    },
    sequence::delimited,
};

pub fn program(s: &str) -> IResult<&str, Program> {
    map(
        many0(
            delimited(
                multispace0,
                expression,
                multispace0,
            )
        ),
        |expressions| {
            Program { expressions: expressions }
        }
    )(s)
}

pub fn expression(s: &str) -> IResult<&str, Expression> {
    alt((
        map(
            atom,
            |atom| {
                Expression { atom: Some(atom), list: None }
            }
        ),
        map(
            list,
            |list| {
                Expression { atom: None, list: Some(list) }
            }
        ),
    ))(s)
}

pub fn list(s: &str) -> IResult<&str, List> {
    map(
        delimited(
            tag(define::LEFT_PARENTHESIS),
            permutation((
                expression,
                many0(
                    permutation((
                        multispace1,
                        expression,
                    ))
                )
            )),
            tag(define::RIGHT_PARENTHESIS),
        ),
        |(expression, expression_vec)| {
            let mut expressions = vec![expression];
            let expr_vec: Vec<Expression> = expression_vec.into_iter().map(|x| x.1).collect();
            expressions.extend_from_slice(&expr_vec);
            List { expressions: expressions }
        }
    )(s)
}

pub fn atom(s: &str) -> IResult<&str, Atom> {
    alt((
        map(
            identifier,
            |identifier| {
                Atom { identifier: Some(identifier), constant: None }
            }
        ),
        map(
            constant,
            |constant| {
                Atom { identifier: None, constant: Some(constant) }
            }
        ),
    ))(s)
}

pub fn identifier(s: &str) -> IResult<&str, String> {
    map(
        is_not(define::PARSER_NOT_IDENTIFIER),
        |identifier: &str| {
            identifier.to_owned()
        }
    )(s)
}

pub fn constant(s: &str) -> IResult<&str, Constant> {
    alt((
        boolean,
        number,
        string,
    ))(s)
}

pub fn boolean(s: &str) -> IResult<&str, Constant> {
    map(
        alt((
            tag(define::TRUE),
            tag(define::FALSE),
        )),
        |string| {
            if string == define::TRUE {
                Constant { boolean: Some(true), number: None, string: None }
            } else {
                Constant { boolean: Some(false), number: None, string: None }
            }
        }
    )(s)
}

pub fn number(s: &str) -> IResult<&str, Constant> {
    map(
        double,
        |number| {
            Constant { boolean: None, number: Some(number), string: None }
        }
    )(s)
}

pub fn string(s: &str) -> IResult<&str, Constant> {
    map(
        delimited(
            tag(define::QUOTATION_MARK),
            escaped_transform(none_of("\"\\"), '\\', alt((
                value('\\', char('\\')),
                value('\"', char('\"')),
                value('\'', char('\'')),
                value('\r', char('r')),
                value('\n', char('n')),
                value('\t', char('t')),
            ))),
            tag(define::QUOTATION_MARK),
        ),
        |string| {
            Constant { boolean: None, number: None, string: Some(string) }
        }
    )(s)
}
