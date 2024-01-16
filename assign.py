from tau import asts
import tau.error


class Context:
    program: asts.Program
    function: asts.FuncDecl
    cur: int


def process(ast: asts.Program) -> None:
    _Program(ast)


def _Program(ast: asts.Program) -> None:
    # ast.register_pool : list[str]
    ctx = Context()
    ctx.program = ast
    ctx.cur = 1
    mainNum = 0
    for decl in ast.decls:
        _FuncDecl(decl, ctx)
        if decl.id.symbol.name == "main":
            mainNum += 1
    if mainNum == 0:
        raise tau.error.NameError("no main function", ast.span)
    elif mainNum > 1:
        raise tau.error.NameError("too many main functions", ast.span)


def _Decl(ast: asts.Decl, ctx: Context) -> None:
    match ast:
        case asts.FuncDecl():
            _FuncDecl(ast, ctx)
        case asts.ParamDecl():
            _ParamDecl(ast, ctx)
        case asts.VarDecl():
            _VarDecl(ast, ctx)
        case _:
            raise NotImplementedError(f"Unknown type {ast}")


def _FuncDecl(ast: asts.FuncDecl, ctx: Context) -> None:
    # ast.id : Id
    # ast.ret_type_ast : TypeAST
    # ast.register_pool : list[str]
    ctx.function = ast
    for param in ast.params:
        _ParamDecl(param, ctx)
    _CompoundStmt(ast.body, ctx)


def _ParamDecl(ast: asts.ParamDecl, ctx: Context) -> None:
    # ast.id : Id
    # ast.type_ast : TypeAST
    pass


def _VarDecl(ast: asts.VarDecl, ctx: Context) -> None:
    # ast.id : Id
    # ast.type_ast : TypeAST
    pass


def _Stmt(ast: asts.Stmt, ctx: Context) -> None:
    match ast:
        case asts.AssignStmt():
            _AssignStmt(ast, ctx)
        case asts.CallStmt():
            _CallStmt(ast, ctx)
        case asts.CompoundStmt():
            _CompoundStmt(ast, ctx)
        case asts.IfStmt():
            _IfStmt(ast, ctx)
        case asts.PrintStmt():
            _PrintStmt(ast, ctx)
        case asts.ReturnStmt():
            _ReturnStmt(ast, ctx)
        case asts.WhileStmt():
            _WhileStmt(ast, ctx)
        case _:
            raise NotImplementedError(f"Unknown type {ast}")


def _AssignStmt(ast: asts.AssignStmt, ctx: Context) -> None:
    _Expr(ast.lhs, ctx)
    ctx.cur += 1
    _Expr(ast.rhs, ctx)
    ctx.cur -= 1


def _CallStmt(ast: asts.CallStmt, ctx: Context) -> None:
    _CallExpr(ast.call, ctx)


def _CompoundStmt(ast: asts.CompoundStmt, ctx: Context) -> None:
    for decl in ast.decls:
        _VarDecl(decl, ctx)
    for stmt in ast.stmts:
        _Stmt(stmt, ctx)


def _IfStmt(ast: asts.IfStmt, ctx: Context) -> None:
    _Expr(ast.expr, ctx)
    _CompoundStmt(ast.thenStmt, ctx)
    if ast.elseStmt:
        _CompoundStmt(ast.elseStmt, ctx)


def _PrintStmt(ast: asts.PrintStmt, ctx: Context) -> None:
    _Expr(ast.expr, ctx)


def _ReturnStmt(ast: asts.ReturnStmt, ctx: Context) -> None:
    if ast.expr:
        _Expr(ast.expr, ctx)


def _WhileStmt(ast: asts.WhileStmt, ctx: Context) -> None:
    _Expr(ast.expr, ctx)
    _CompoundStmt(ast.stmt, ctx)


def _Expr(ast: asts.Expr, ctx: Context) -> None:
    ast.register = f"r{ctx.cur}"
    if len(ctx.function.register_pool) < ctx.cur:
        ctx.function.register_pool.insert(0, f"r{ctx.cur}")
    match ast:
        case asts.ArrayCell():
            _ArrayCell(ast, ctx)
        case asts.BinaryOp():
            _BinaryOp(ast, ctx)
        case asts.BoolLiteral():
            _BoolLiteral(ast, ctx)
        case asts.CallExpr():
            _CallExpr(ast, ctx)
        case asts.IdExpr():
            _IdExpr(ast, ctx)
        case asts.IntLiteral():
            _IntLiteral(ast, ctx)
        case asts.UnaryOp():
            _UnaryOp(ast, ctx)
        case _:
            raise NotImplementedError(f"Unknown type {ast}")


def _ArrayCell(ast: asts.ArrayCell, ctx: Context) -> None:
    # ast.register : str <--- set this!!
    _Expr(ast.arr, ctx)
    ctx.cur += 1
    _Expr(ast.idx, ctx)
    ctx.cur -= 1


def _BinaryOp(ast: asts.BinaryOp, ctx: Context) -> None:
    # ast.register : str <--- set this!!
    _Expr(ast.left, ctx)
    ctx.cur += 1
    _Expr(ast.right, ctx)
    ctx.cur -= 1


def _BoolLiteral(ast: asts.BoolLiteral, ctx: Context) -> None:
    # ast.register : str <--- set this!!
    pass


def _CallExpr(ast: asts.CallExpr, ctx: Context) -> None:
    # ast.register : str <--- set this!!
    ast.register = f"r{ctx.cur}"
    _Expr(ast.fn, ctx)
    ctx.cur += 1
    for arg in ast.args:
        _Argument(arg, ctx)
    ctx.cur -= 1


def _IdExpr(ast: asts.IdExpr, ctx: Context) -> None:
    # ast.register : str <--- set this!!
    # ast.id : Id
    pass


def _IntLiteral(ast: asts.IntLiteral, ctx: Context) -> None:
    # ast.register : str <--- set this!!
    pass


def _UnaryOp(ast: asts.UnaryOp, ctx: Context) -> None:
    # ast.register : str <--- set this!!
    _Expr(ast.expr, ctx)


def _Argument(ast: asts.Argument, ctx: Context) -> None:
    _Expr(ast.expr, ctx)
