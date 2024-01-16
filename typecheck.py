from tau import asts, symbols
import tau.error


def process(ast: asts.Program) -> None:
    _Program(ast)


def _Program(ast: asts.Program) -> None:
    for decl in ast.decls:
        _FuncDecl(decl)


def _Argument(ast: asts.Argument) -> symbols.SemanticType:
    ast.semantic_type = _Expr(ast.expr)
    return ast.semantic_type


def _Id(ast: asts.Id) -> symbols.SemanticType:
    # ast.token : Token
    # ast.symbol : Symbol
    if ast.symbol == None:
        raise tau.error.NameError("variable not defined", ast.span)
    ast.semantic_type = ast.symbol.scope.lookup(ast.token.value).get_type()
    return ast.semantic_type


def _FuncDecl(ast: asts.FuncDecl) -> None:
    _Id(ast.id)
    params = []
    for param in ast.params:
        params.append(_ParamDecl(param))
    _TypeAST(ast.ret_type_ast)
    ast.semantic_type = symbols.FuncType(params, ast.ret_type_ast.semantic_type)
    ast.id.semantic_type = ast.semantic_type
    ast.id.symbol.set_type(ast.semantic_type)
    ast.id.symbol.scope.lookup(ast.id.token.value).set_type(ast.semantic_type)
    _CompoundStmt(ast.body)


def _ParamDecl(ast: asts.ParamDecl) -> symbols.SemanticType:
    _Id(ast.id)
    var_type = _TypeAST(ast.type_ast)
    ast.id.symbol.scope.lookup(ast.id.token.value).set_type(var_type)
    ast.semantic_type = var_type
    ast.id.semantic_type = var_type
    return var_type


def _VarDecl(ast: asts.VarDecl) -> symbols.SemanticType:
    _Id(ast.id)
    var_type = _TypeAST(ast.type_ast)
    ast.semantic_type = var_type
    ast.id.symbol.scope.lookup(ast.id.token.value).set_type(var_type)
    ast.id.semantic_type = var_type
    return var_type


def _Expr(ast: asts.Expr) -> symbols.SemanticType:
    match ast:
        case asts.ArrayCell():
            return _ArrayCell(ast)
        case asts.BinaryOp():
            return _BinaryOp(ast)
        case asts.BoolLiteral():
            return _BoolLiteral(ast)
        case asts.CallExpr():
            return _CallExpr(ast)
        case asts.IdExpr():
            return _IdExpr(ast)
        case asts.IntLiteral():
            return _IntLiteral(ast)
        case asts.UnaryOp():
            return _UnaryOp(ast)
        case _:
            raise NotImplementedError(f"Unknown type {ast}")


def _ArrayCell(ast: asts.ArrayCell) -> symbols.SemanticType:
    id = _Expr(ast.arr)
    if not isinstance(id, symbols.ArrayType):
        raise TypeError("not an array name", ast.arr.span)
    var_type = id.element_type
    idx_type = _Expr(ast.idx)
    if not isinstance(idx_type, symbols.IntType):
        raise TypeError("not a valid index", ast.idx.span)
    ast.semantic_type = var_type
    return var_type


def _BinaryOp(ast: asts.BinaryOp) -> symbols.SemanticType:
    int_ops = ["+", "-", "*", "/"]
    bool_ops = ["and", "or"]
    math = ["+", "-", "*", "/"]
    logic = ["<", ">", "<=", ">=", "==", "!=", "and", "or"]
    # ast.op : Token
    left_type = _Expr(ast.left)
    right_type = _Expr(ast.right)
    if (
        type(left_type) is not type(right_type)
        or (ast.op.value in int_ops and not isinstance(left_type, symbols.IntType))
        or (ast.op.value in bool_ops and not isinstance(left_type, symbols.BoolType))
    ):
        raise TypeError("incorrect op types", ast.span)
    elif ast.op.value in math:
        ast.semantic_type = symbols.IntType()
        return symbols.IntType()
    elif ast.op.value in logic:
        ast.semantic_type = symbols.BoolType()
        return symbols.BoolType()
    return symbols.VoidType()  # This should never happen but mypy kept complaining


def _BoolLiteral(ast: asts.BoolLiteral) -> symbols.BoolType:
    # ast.token : Token
    # ast.value : bool
    ast.semantic_type = symbols.BoolType()
    return symbols.BoolType()


def _CallExpr(ast: asts.CallExpr) -> symbols.SemanticType:
    func = _Expr(ast.fn)
    if not isinstance(func, symbols.FuncType):
        raise TypeError("not a function", ast.span)
    ret_type = func.ret
    ast.semantic_type = ret_type
    func_params = func.params
    if len(ast.args) != len(func_params):
        raise tau.error.TypeError("wrong number of parameters", ast.span)
    for num, arg in enumerate(ast.args):
        arg_type = _Argument(arg)
        if type(arg_type) is not type(func_params[num]):
            raise TypeError("wrong parameter type", arg.span)
    return ret_type


def _IdExpr(ast: asts.IdExpr) -> symbols.SemanticType:
    ast.semantic_type = _Id(ast.id)
    return ast.semantic_type


def _IntLiteral(ast: asts.IntLiteral) -> symbols.IntType:
    ast.semantic_type = symbols.IntType()
    return symbols.IntType()


def _UnaryOp(ast: asts.UnaryOp) -> symbols.SemanticType:
    var_type = _Expr(ast.expr)
    if ast.op.value == "-" and isinstance(var_type, symbols.IntType):
        ast.semantic_type = symbols.IntType()
        return symbols.IntType()
    elif ast.op.value == "not" and isinstance(var_type, symbols.BoolType):
        ast.semantic_type = symbols.BoolType()
        return symbols.BoolType()
    else:
        raise TypeError("incorrect op type", ast.span)


def _Stmt(ast: asts.Stmt) -> None:
    match ast:
        case asts.AssignStmt():
            _AssignStmt(ast)
        case asts.CallStmt():
            _CallStmt(ast)
        case asts.CompoundStmt():
            _CompoundStmt(ast)
        case asts.IfStmt():
            _IfStmt(ast)
        case asts.PrintStmt():
            _PrintStmt(ast)
        case asts.ReturnStmt():
            _ReturnStmt(ast)
        case asts.WhileStmt():
            _WhileStmt(ast)
        case _:
            raise NotImplementedError(f"Unknown type {ast}")


def _AssignStmt(ast: asts.AssignStmt) -> None:
    var_type = _Expr(ast.lhs)
    set_type = _Expr(ast.rhs)
    if type(var_type) is not type(set_type):
        raise TypeError("types don't match", ast.span)
    else:
        ast.lhs.semantic_type = var_type
        ast.rhs.semantic_type = var_type


def _CallStmt(ast: asts.CallStmt) -> None:
    _CallExpr(ast.call)


def _CompoundStmt(ast: asts.CompoundStmt) -> None:
    for decl in ast.decls:
        _VarDecl(decl)
    for stmt in ast.stmts:
        _Stmt(stmt)


def _IfStmt(ast: asts.IfStmt) -> None:
    if not isinstance(_Expr(ast.expr), symbols.BoolType):
        raise TypeError("expr isn't a boolean", ast.expr.span)
    _CompoundStmt(ast.thenStmt)
    if ast.elseStmt:
        _CompoundStmt(ast.elseStmt)


def _PrintStmt(ast: asts.PrintStmt) -> None:
    _Expr(ast.expr)


def _ReturnStmt(ast: asts.ReturnStmt) -> None:
    if ast.expr:
        ret_type = _Expr(ast.expr)
        if type(ret_type) is not type(
            ast.enclosing_function.ret_type_ast.semantic_type
        ):
            raise TypeError("incorrect return type", ast.expr.span)
    else:
        if not isinstance(
            ast.enclosing_function.ret_type_ast.semantic_type, symbols.VoidType
        ):
            raise TypeError(
                "can't return nothing unless the function is void", ast.span
            )


def _WhileStmt(ast: asts.WhileStmt) -> None:
    if not isinstance(_Expr(ast.expr), symbols.BoolType):
        raise TypeError("expr isn't a boolean", ast.expr.span)
    _CompoundStmt(ast.stmt)


def _TypeAST(ast: asts.TypeAST) -> symbols.SemanticType:
    match ast:
        case asts.ArrayType():
            ast.semantic_type = _ArrayType(ast)
            return ast.semantic_type
        case asts.BoolType():
            ast.semantic_type = _BoolType(ast)
            return ast.semantic_type
        case asts.IntType():
            ast.semantic_type = _IntType(ast)
            return ast.semantic_type
        case asts.VoidType():
            ast.semantic_type = _VoidType(ast)
            return ast.semantic_type
        case _:
            raise NotImplementedError(f"Unknown type {ast}")


def _ArrayType(ast: asts.ArrayType) -> symbols.ArrayType:
    var_type = _TypeAST(ast.element_type_ast)
    return symbols.ArrayType(var_type, int(ast.size.value))


def _BoolType(ast: asts.BoolType) -> symbols.BoolType:
    # ast.token : Token
    return symbols.BoolType()


def _IntType(ast: asts.IntType) -> symbols.IntType:
    # ast.token : Token
    return symbols.IntType()


def _VoidType(ast: asts.VoidType) -> symbols.VoidType:
    # ast.token : Token
    return symbols.VoidType()
