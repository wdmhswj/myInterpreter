package ast

import (
	"bytes"
	"monkey/token"
)

type Node interface {
	TokenLiteral() string
	String() string
}

type Statement interface {
	Node
	statementNode()
}

type Expression interface {
	Node
	expressionNode()
}

type Program struct {
	Statements []Statement
}


type LetStatement struct {
	Token token.Token	// token.LET词法单元
	Name *Identifier
	Value Expression
}

type ReturnStatement struct {
	Token token.Token	// 'return' 词法单元
	ReturnValue Expression
}

type ExpressionStatement struct {
	Token token.Token 	// 该表达式中的第一个词法单元
	Expression Expression
}


type Identifier struct {
	Token token.Token	// token.IDENT词法单元
	Value string
}

type IntegerLiteral struct {
	Token token.Token
	Value int64
}

type PrefixExpression struct {
	Token token.Token // 前缀词法单元，如!
	Operator string
	Right Expression
}

type InfixExpression struct {
	Token token.Token // 运算符词法单元，如+
	Left Expression
	Operator string
	Right Expression
}



func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	} else {
		return ""
	}
}

func (p *Program) String() string {
	var out bytes.Buffer
	for _, s := range p.Statements {
		out.WriteString(s.String())
	}
	return out.String()
}



func (es *ExpressionStatement) statementNode () {}

func (es *ExpressionStatement) TokenLiteral() string { return es.Token.Literal }

func (es *ExpressionStatement) String() string {
	if es.Expression != nil {
		return es.Expression.String()
	}
	return ""
}

func (rs *ReturnStatement) statementNode() {}

func (rs *ReturnStatement) TokenLiteral() string { return rs.Token.Literal }

func (rs *ReturnStatement) String() string {
	var out bytes.Buffer

	out.WriteString(rs.TokenLiteral() + " ")
	

	if rs.ReturnValue != nil {
		out.WriteString(rs.ReturnValue.String())
	}

	out.WriteString(";")

	return out.String()
}

func (ls *LetStatement) statementNode() {}

func (ls *LetStatement) TokenLiteral() string {
	return ls.Token.Literal
}

func (ls *LetStatement) String() string {
	var out bytes.Buffer

	out.WriteString(ls.TokenLiteral() + " ")
	out.WriteString(ls.Name.String())
	out.WriteString(" = ")

	if ls.Value != nil {
		out.WriteString(ls.Value.String())
	}

	out.WriteString(";")
	return out.String()
}

// Identifier struct 实现了 Expression interface 接口
func (i *Identifier) expressionNode() {}

func (i *Identifier) TokenLiteral() string {
	return i.Token.Literal
}

func (i *Identifier) String() string {
	return i.Value
}

func (il *IntegerLiteral) expressionNode() {
	
}
func (il *IntegerLiteral) TokenLiteral() string {
	return il.Token.Literal
}

func (il* IntegerLiteral) String() string {
	return il.Token.Literal
}

func (pe *PrefixExpression) expressionNode() {}

func (pe *PrefixExpression) TokenLiteral() string {
	return pe.Token.Literal
}

func (pe *PrefixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(pe.Operator)
	out.WriteString(pe.Right.String())
	out.WriteString(")")
	
	return out.String()
}

func (ie *InfixExpression) expressionNode() {}

func (ie *InfixExpression) TokenLiteral() string {
	return ie.Token.Literal
}

func (ie *InfixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(ie.Left.String())
	out.WriteString(" " + ie.Operator + " ")
	out.WriteString(ie.Right.String())
	out.WriteString(")")
	
	return out.String()
}

