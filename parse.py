from tau import asts, symbols
from tau.tokens import Coord, Span
from tau.tokens import Token
from typing import NoReturn, Iterable, Iterator


class ParseErrorException(Exception):
    msg: str
    token: Token
    expected: set[str]

    def __init__(self, msg: str, current: Token, expected: set[str]):
        self.msg = msg
        self.current = current
        self.expected = expected

    def __str__(self) -> str:
        return f"Parse error {self.msg} at {self.current}:  Expected {self.expected}"


class Parser:
    scanner: Iterator[Token]
    _current: Token

    def __init__(
        self,
        scanner: Iterable[Token],
    ):
        self.scanner: Iterator[Token] = iter(scanner)
        self._current = next(self.scanner)

    def error(self, msg: str, expected: set[str]) -> NoReturn:
        raise ParseErrorException(msg, self._current, expected)

    def match(self, kind: str) -> Token:
        if self.current() == kind:
            prev: Token = self._current
            try:
                self._current = next(self.scanner)
            except StopIteration:
                pass
            return prev
        else:
            self.error("", {kind})

    def current(self) -> str:
        return self._current.kind

    def parse(self) -> asts.Program:
        v = self._Program()
        self.match("EOF")
        return v

    def _Program(self) -> asts.Program:
        # Program -> Function { Function }
        decls = [(self._Function())]
        start = decls[0].span.start
        while self.current() in {"func"}:
            decls.append(self._Function())
        end = decls[-1].span.end
        return asts.Program(Span(start, end), decls)

    def _Function(self) -> asts.FuncDecl:
        # Function -> "func" "ID" "(" Params ")" ":" (Type | "void") CompoundStatement
        start = self.match("func").span.start
        id = self.match("ID")
        self.match("(")
        params = self._Params()
        self.match(")")
        self.match(":")
        if self.current() in {"[", "bool", "int"}:
            ret_type_ast = self._Type()
        elif self.current() in {"void"}:
            voidToken = self.match("void")
            ret_type_ast = asts.VoidType(voidToken.span, voidToken)
        else:
            self.error("syntax error", {"[", "bool", "int", "void"})
        body = self._CompoundStatement()
        end = body.span.end
        return asts.FuncDecl(
            Span(start, end), asts.Id(id.span, id), params, ret_type_ast, body
        )

    def _Params(self) -> list[asts.ParamDecl]:
        # Params -> [ Paramdecl { "," Paramdecl } ]
        ret = []
        if self.current() in {"ID"}:
            ret.append(self._Paramdecl())
            while self.current() in {","}:
                self.match(",")
                ret.append(self._Paramdecl())
        return ret

    def _Paramdecl(self) -> asts.ParamDecl:
        # Paramdecl -> "ID" ":" Type
        id = self.match("ID")
        start = id.span.start
        self.match(":")
        type_ast = self._Type()
        end = type_ast.span.end
        return asts.ParamDecl(Span(start, end), asts.Id(id.span, id), type_ast)

    def _Comparison(self) -> Token:
        # Comparison -> "<=" | "==" | ">=" | "<" | ">" | "!="
        if self.current() in {"<="}:
            return self.match("<=")
        elif self.current() in {"=="}:
            return self.match("==")
        elif self.current() in {">="}:
            return self.match(">=")
        elif self.current() in {"<"}:
            return self.match("<")
        elif self.current() in {">"}:
            return self.match(">")
        elif self.current() in {"!="}:
            return self.match("!=")
        else:
            self.error("syntax error", {"!=", "<", "<=", "==", ">", ">="})

    def _Type(self) -> asts.TypeAST:
        # Type -> { "[" [ "INT" ] "]" } ("bool" | "int")
        arrayCheck = False
        if self.current() in {"["}:
            arrayCheck = True
            start = self.match("[").span.start
            if self.current() in {"INT"}:
                size = self.match("INT")
            else:
                size = Token("INT", "1", Span(start, start))
            self.match("]")
        if self.current() in {"bool"}:
            if arrayCheck:
                token = self.match("bool")
                return asts.ArrayType(
                    Span(start, token.span.end), size, asts.BoolType(token.span, token)
                )
            else:
                token = self.match("bool")
                return asts.BoolType(token.span, token)
        elif self.current() in {"int"}:
            if arrayCheck:
                token = self.match("int")
                return asts.ArrayType(
                    Span(start, token.span.end), size, asts.IntType(token.span, token)
                )
            else:
                token = self.match("int")
                return asts.IntType(token.span, token)
        else:
            self.error("syntax error", {"bool", "int"})

    def _BooleanLiteral(self) -> asts.BoolLiteral:
        # BooleanLiteral -> "true" | "false"
        token = self._current
        if self.current() in {"true"}:
            value = True
            self.match("true")
        elif self.current() in {"false"}:
            value = False
            self.match("false")
        else:
            self.error("syntax error", {"false", "true"})
        return asts.BoolLiteral(token.span, token, value)

    def _FuncOrIdOrArray(self) -> asts.Expr:
        # FuncOrIdOrArray -> "ID" [ "(" ([ Expr ] { ("," Expr) }) ")" | "[" Expr "]" ]
        token = self.match("ID")
        # WARNING: AMBIGUOUS: with lookahead {'"("'}
        if self.current() in {"(", "["}:
            start = token.span.start
            if self.current() in {"("}:
                self.match("(")
                args = []
                if self.current() in {"(", "-", "ID", "INT", "false", "not", "true"}:
                    arg = self._Expr()
                    args.append(asts.Argument(arg.span, arg))
                while self.current() in {","}:
                    self.match(",")
                    arg = self._Expr()
                    args.append(asts.Argument(arg.span, arg))
                end = self.match(")").span.end
                return asts.CallExpr(
                    Span(start, end),
                    asts.IdExpr(token.span, asts.Id(token.span, token)),
                    args,
                )
            elif self.current() in {"["}:
                self.match("[")
                idx = self._Expr()
                end = self.match("]").span.end
                return asts.ArrayCell(
                    Span(start, end),
                    asts.IdExpr(token.span, asts.Id(token.span, token)),
                    idx,
                )
            else:
                self.error("syntax error", {"(", "["})
        return asts.IdExpr(token.span, asts.Id(token.span, token))

    def _CallExpr(self) -> asts.CallExpr:
        # CallExpr -> "ID" "(" ([ Expr ] { ("," Expr) }) ")"
        func = self.match("ID")
        start = func.span.start
        self.match("(")
        args = []
        if self.current() in {"(", "-", "ID", "INT", "false", "not", "true"}:
            arg = self._Expr()
            args.append(asts.Argument(arg.span, arg))
        while self.current() in {","}:
            self.match(",")
            arg = self._Expr()
            args.append(asts.Argument(arg.span, arg))
        end = self.match(")").span.end
        return asts.CallExpr(
            Span(start, end), asts.IdExpr(func.span, asts.Id(func.span, func)), args
        )

    def _Expr(self) -> asts.Expr:
        # Expr -> AndExpr { "or" AndExpr }
        left = self._AndExpr()
        # WARNING: AMBIGUOUS: with lookahead {'"or"'}
        while self.current() in {"or"}:
            op = self.match("or")
            right = self._AndExpr()
            start = left.span.start
            end = right.span.end
            left = asts.BinaryOp(Span(start, end), op, left, right)
        return left

    def _AndExpr(self) -> asts.Expr:
        # AndExpr -> CmprExpr { "and" CmprExpr }
        left = self._CmprExpr()
        # WARNING: AMBIGUOUS: with lookahead {'"and"'}
        while self.current() in {"and"}:
            op = self.match("and")
            right = self._CmprExpr()
            start = left.span.start
            end = right.span.end
            left = asts.BinaryOp(Span(start, end), op, left, right)
        return left

    def _CmprExpr(self) -> asts.Expr:
        # CmprExpr -> AddExpr { Comparison UnaryExpr }
        left = self._AddExpr()
        # WARNING: AMBIGUOUS: with lookahead {'">="', '"!="', '"<"', '"<="', '">"', '"=="'}
        while self.current() in {"!=", "<", "<=", "==", ">", ">="}:
            op = self._Comparison()
            right = self._AddExpr()
            start = left.span.start
            end = right.span.end
            left = asts.BinaryOp(Span(start, end), op, left, right)
        return left

    def _AddExpr(self) -> asts.Expr:
        # AddExpr -> MultExpr { ("+" | "-") MultExpr }
        left = self._MultExpr()
        # WARNING: AMBIGUOUS: with lookahead {'"+"', '"-"'}
        while self.current() in {"+", "-"}:
            if self.current() in {"+"}:
                op = self.match("+")
            elif self.current() in {"-"}:
                op = self.match("-")
            else:
                self.error("syntax error", {"+", "-"})
            right = self._MultExpr()
            start = left.span.start
            end = right.span.end
            left = asts.BinaryOp(Span(start, end), op, left, right)
        return left

    def _MultExpr(self) -> asts.Expr:
        # MultExpr -> UnaryExpr { ("*" | "/") UnaryExpr }
        left = self._UnaryExpr()
        # WARNING: AMBIGUOUS: with lookahead {'"*"', '"/"'}
        while self.current() in {"*", "/"}:
            if self.current() in {"*"}:
                op = self.match("*")
            elif self.current() in {"/"}:
                op = self.match("/")
            else:
                self.error("syntax error", {"*", "/"})
            right = self._UnaryExpr()
            start = left.span.start
            end = right.span.end
            left = asts.BinaryOp(Span(start, end), op, left, right)
        return left

    def _UnaryExpr(self) -> asts.Expr:
        # UnaryExpr -> "-" UnaryExpr | "not" UnaryExpr | BaseExpr
        if self.current() in {"-"}:
            token = self.match("-")
            start = token.span.start
            expr = self._UnaryExpr()
            end = expr.span.end
            return asts.UnaryOp(Span(start, end), token, expr)
        elif self.current() in {"not"}:
            token = self.match("not")
            start = token.span.start
            expr = self._UnaryExpr()
            end = expr.span.end
            return asts.UnaryOp(Span(start, end), token, expr)
        elif self.current() in {"(", "ID", "INT", "false", "true"}:
            return self._BaseExpr()
        else:
            self.error("syntax error", {"(", "-", "ID", "INT", "false", "not", "true"})

    def _BaseExpr(self) -> asts.Expr:
        # BaseExpr -> "INT" | FuncOrIdOrArray | BooleanLiteral | "(" Expr ")"
        if self.current() in {"INT"}:
            token = self.match("INT")
            return asts.IntLiteral(token.span, token)
        elif self.current() in {"ID"}:
            return self._FuncOrIdOrArray()
        elif self.current() in {"false", "true"}:
            return self._BooleanLiteral()
        elif self.current() in {"("}:
            self.match("(")
            ret = self._Expr()
            self.match(")")
            return ret
        else:
            self.error("syntax error", {"(", "ID", "INT", "false", "true"})

    def _VariableDeclaration(self) -> asts.VarDecl:
        # VariableDeclaration -> "var" "ID" ":" Type
        start = self.match("var").span.start
        id = self.match("ID")
        self.match(":")
        type_ast = self._Type()
        end = type_ast.span.end
        return asts.VarDecl(Span(start, end), asts.Id(id.span, id), type_ast)

    def _IfStatement(self) -> asts.IfStmt:
        # IfStatement -> "if" Expr CompoundStatement [ "else" CompoundStatement ]
        start = self.match("if").span.start
        expr = self._Expr()
        thenStmt = self._CompoundStatement()
        if self.current() in {"else"}:
            self.match("else")
            elseStmt = self._CompoundStatement()
            end = elseStmt.span.end
            return asts.IfStmt(Span(start, end), expr, thenStmt, elseStmt)
        else:
            end = thenStmt.span.end
            return asts.IfStmt(Span(start, end), expr, thenStmt, None)

    def _PrintStatement(self) -> asts.PrintStmt:
        # PrintStatement -> "print" Expr
        start = self.match("print").span.start
        expr = self._Expr()
        end = expr.span.end
        return asts.PrintStmt(Span(start, end), expr)

    def _AssignmentStatement(self) -> asts.AssignStmt:
        # AssignmentStatement -> Expr "=" Expr
        lhs = self._Expr()
        self.match("=")
        rhs = self._Expr()
        start = lhs.span.start
        end = rhs.span.end
        return asts.AssignStmt(Span(start, end), lhs, rhs)

    def _WhileStatement(self) -> asts.WhileStmt:
        # WhileStatement -> "while" Expr CompoundStatement
        start = self.match("while").span.start
        expr = self._Expr()
        stmt = self._CompoundStatement()
        end = stmt.span.end
        return asts.WhileStmt(Span(start, end), expr, stmt)

    def _CallStatement(self) -> asts.CallStmt:
        # CallStatement -> "call" CallExpr
        start = self.match("call").span.start
        call = self._CallExpr()
        end = call.span.end
        return asts.CallStmt(Span(start, end), call)

    def _Statement(self) -> asts.Stmt:
        # Statement -> IfStatement | CallStatement | PrintStatement | AssignmentStatement | WhileStatement | CompoundStatement
        if self.current() in {"if"}:
            return self._IfStatement()
        elif self.current() in {"call"}:
            return self._CallStatement()
        elif self.current() in {"print"}:
            return self._PrintStatement()
        elif self.current() in {"ID"}:
            return self._AssignmentStatement()
        elif self.current() in {"while"}:
            return self._WhileStatement()
        elif self.current() in {"{"}:
            return self._CompoundStatement()
        else:
            self.error("syntax error", {"ID", "call", "if", "print", "while", "{"})

    def _CompoundStatement(self) -> asts.CompoundStmt:
        # CompoundStatement -> "{" { VariableDeclaration } { Statement } [ ReturnStatement ] "}"
        start = self.match("{").span.start
        decls = []
        stmts = []
        while self.current() in {"var"}:
            decls.append(self._VariableDeclaration())
        while self.current() in {"ID", "call", "if", "print", "while", "{"}:
            stmts.append(self._Statement())
        if self.current() in {"return"}:
            stmts.append(self._ReturnStatement())
        end = self.match("}").span.end
        return asts.CompoundStmt(Span(start, end), decls, stmts)

    def _ReturnStatement(self) -> asts.ReturnStmt:
        # ReturnStatement -> "return" [ Expr ]
        returnToken = self.match("return")
        if self.current() in {"(", "-", "ID", "INT", "false", "not", "true"}:
            expr = self._Expr()
            return asts.ReturnStmt(Span(returnToken.span.start, expr.span.end), expr)
        return asts.ReturnStmt(returnToken.span, None)
