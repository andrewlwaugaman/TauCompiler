from tau import asts


def process(ast: asts.Program) -> None:
    _Program(ast)


def _Program(ast: asts.Program) -> None:
    current = -2
    for decl in ast.decls:
        _FuncDecl(decl, current)
    return None


def _Decl(ast: asts.Decl, current: int) -> int:
    retval: int
    match ast:
        case asts.FuncDecl():
            retval = _FuncDecl(ast, current)
        case asts.ParamDecl():
            retval = _ParamDecl(ast, current)
        case asts.VarDecl():
            retval = _VarDecl(ast, current)
        case _:
            raise NotImplementedError(f"Unknown type {ast}")
    return retval


def _FuncDecl(ast: asts.FuncDecl, current: int) -> int:
    # ast.id : Id
    # ast.ret_type_ast : TypeAST
    for param in ast.params:
        _ParamDecl(param, current)
        current -= 1
    current = 3
    retval = 2 + _CompoundStmt(ast.body, current)
    ast.size = retval - 2
    return retval


def _ParamDecl(ast: asts.ParamDecl, current: int) -> int:
    # ast.id : Id
    # ast.type_ast : TypeAST
    ast.id.symbol.offset = current
    return 0


def _VarDecl(ast: asts.VarDecl, current: int) -> int:
    # ast.id : Id
    # ast.type_ast : TypeAST
    ast.id.symbol.offset = current
    return 1


def _Stmt(ast: asts.Stmt, current: int) -> int:
    retval: int
    match ast:
        case asts.AssignStmt():
            retval = _AssignStmt(ast, current)
        case asts.CallStmt():
            retval = _CallStmt(ast, current)
        case asts.CompoundStmt():
            retval = _CompoundStmt(ast, current)
        case asts.IfStmt():
            retval = _IfStmt(ast, current)
        case asts.PrintStmt():
            retval = _PrintStmt(ast, current)
        case asts.ReturnStmt():
            retval = _ReturnStmt(ast, current)
        case asts.WhileStmt():
            retval = _WhileStmt(ast, current)
        case _:
            raise NotImplementedError(f"Unknown type {ast}")
    return retval


def _AssignStmt(ast: asts.AssignStmt, current: int) -> int:
    return 0


def _CallStmt(ast: asts.CallStmt, current: int) -> int:
    return 0


def _CompoundStmt(ast: asts.CompoundStmt, current: int) -> int:
    for decl in ast.decls:
        current += _VarDecl(decl, current)
    retval = current
    for stmt in ast.stmts:
        retval = max(retval, _Stmt(stmt, current))
    return retval


def _IfStmt(ast: asts.IfStmt, current: int) -> int:
    _CompoundStmt(ast.thenStmt, current)
    if ast.elseStmt:
        _CompoundStmt(ast.elseStmt, current)
    return 0


def _PrintStmt(ast: asts.PrintStmt, current: int) -> int:
    return 0


def _ReturnStmt(ast: asts.ReturnStmt, current: int) -> int:
    return 1


def _WhileStmt(ast: asts.WhileStmt, current: int) -> int:
    _CompoundStmt(ast.stmt, current)
    return 0
