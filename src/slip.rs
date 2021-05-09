pub mod define;
pub mod parser; 
pub mod compiler;

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub expressions: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expression {
    pub atom: Option<Atom>,
    pub list: Option<List>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct List {
    pub expressions: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Atom {
    pub identifier: Option<String>,
    pub constant: Option<Constant>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Constant {
    pub number: Option<f64>,
    pub string: Option<String>,
}
