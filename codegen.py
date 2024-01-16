from tau import asts
from vm.vm_insns import *

labelNum = 0


def process(ast: asts.Program) -> list[Insn]:
    return _Program(ast)


def _Program(ast: asts.Program) -> list[Insn]:
    # ast.register_pool : list[str]
    retval = [Call("main"), Halt()]
    for decl in ast.decls:
        retval.extend(_FuncDecl(decl))
    return retval


def _Decl(ast: asts.Decl) -> list[Insn]:
    retval: list[Insn]
    match ast:
        case asts.FuncDecl():
            retval = _FuncDecl(ast)
        case asts.ParamDecl():
            retval = _ParamDecl(ast)
        case asts.VarDecl():
            retval = _VarDecl(ast)
        case _:
            raise NotImplementedError(f"Unknown type {ast}")
    return retval


def _FuncDecl(ast: asts.FuncDecl) -> list[Insn]:
    # ast.semantic_type : SemanticType
    # ast.id : Id
    # ast.ret_type_ast : TypeAST
    # ast.size : int
    # ast.register_pool : list[str]
    retval = [
        Label(ast.id.token.value),
        Store("SP", "RA"),
        AddImmediate("tmpFP", "SP", 1),
        Store("tmpFP", "FP"),
        AddImmediate("tmpSP", "SP", 2),
        Store("tmpSP", "SP"),
        AddImmediate("FP", "SP", 0),
        AddImmediate("SP", "SP", ast.size),
    ]
    for param in ast.params:
        retval.extend(_ParamDecl(param))
    retval.extend(_CompoundStmt(ast.body))
    retval.extend(
        [
            AddImmediate("tmpFP", "FP", 1),
            AddImmediate("tmpSP", "FP", 2),
            Load("FP", "tmpFP"),
            Load("SP", "tmpSP"),
            Load("RA", "SP"),
            JumpIndirect("RA"),
        ]
    )
    return retval


def _ParamDecl(ast: asts.ParamDecl) -> list[Insn]:
    # ast.semantic_type : SemanticType
    # ast.id : Id
    # ast.type_ast : TypeAST
    retval: list[Insn]
    retval = []
    return retval


def _VarDecl(ast: asts.VarDecl) -> list[Insn]:
    # ast.semantic_type : SemanticType
    # ast.id : Id
    # ast.type_ast : TypeAST
    retval: list[Insn]
    return retval


def _Stmt(ast: asts.Stmt) -> list[Insn]:
    retval: list[Insn]
    match ast:
        case asts.AssignStmt():
            retval = _AssignStmt(ast)
        case asts.CallStmt():
            retval = _CallStmt(ast)
        case asts.CompoundStmt():
            retval = _CompoundStmt(ast)
        case asts.IfStmt():
            retval = _IfStmt(ast)
        case asts.PrintStmt():
            retval = _PrintStmt(ast)
        case asts.ReturnStmt():
            retval = _ReturnStmt(ast)
        case asts.WhileStmt():
            retval = _WhileStmt(ast)
        case _:
            raise NotImplementedError(f"Unknown type {ast}")
    return retval


def _AssignStmt(ast: asts.AssignStmt) -> list[Insn]:
    # ast.lhs : Expr
    # ast.rhs : Expr
    retval = _rval_Expr(ast.rhs)
    retval.extend(_lval_Expr(ast.lhs))
    retval.append(Store(ast.lhs.register, ast.rhs.register))
    return retval


def _CallStmt(ast: asts.CallStmt) -> list[Insn]:
    # ast.call : CallExpr
    return _rval_Expr(ast.call)


def _CompoundStmt(ast: asts.CompoundStmt) -> list[Insn]:
    retval = []
    for stmt in ast.stmts:
        retval.extend(_Stmt(stmt))
    return retval


def _IfStmt(ast: asts.IfStmt) -> list[Insn]:
    # ast.expr : Expr
    global labelNum
    tLabel = f"tLabel{labelNum}"
    fLabel = f"fLabel{labelNum}"
    labelNum += 1
    retval: list[Insn]
    retval = flow(ast.expr, fLabel, False)
    retval.extend(_CompoundStmt(ast.thenStmt))
    retval.append(Jump(tLabel))
    retval.append(Label(fLabel))
    if ast.elseStmt:
        retval.extend(_CompoundStmt(ast.elseStmt))
    retval.append(Label(tLabel))
    return retval


def _PrintStmt(ast: asts.PrintStmt) -> list[Insn]:
    # ast.expr : Expr
    retval = _rval_Expr(ast.expr)
    retval.append(Print(ast.expr.register))
    return retval


def _ReturnStmt(ast: asts.ReturnStmt) -> list[Insn]:
    # ast.expr : Optional[Expr]
    # ast.enclosing_function : FuncDecl
    retval = _rval_Expr(ast.expr)
    if ast.expr != None:
        retval.extend([AddImmediate("RV", "FP", -1), Store("RV", ast.expr.register)])
    retval.extend(
        [
            AddImmediate("tmpFP", "FP", 1),
            AddImmediate("tmpSP", "FP", 2),
            Load("FP", "tmpFP"),
            Load("SP", "tmpSP"),
            Load("RA", "SP"),
        ]
    )
    retval.extend([JumpIndirect("RA")])
    return retval


def _WhileStmt(ast: asts.WhileStmt) -> list[Insn]:
    # ast.expr : Expr
    global labelNum
    tLabel = f"top{labelNum}"
    bLabel = f"bottom{labelNum}"
    labelNum += 1
    retval: list[Insn]
    retval = [Label(tLabel)]
    retval.extend(flow(ast.expr, bLabel, False))
    retval.extend(_CompoundStmt(ast.stmt))
    retval.append(Jump(tLabel))
    retval.append(Label(bLabel))
    return retval


def _rval_Expr(ast: asts.Expr) -> list[Insn]:
    retval: list[Insn]
    match ast:
        case asts.ArrayCell():
            retval = _rval_ArrayCell(ast)
        case asts.BinaryOp():
            retval = _rval_BinaryOp(ast)
        case asts.BoolLiteral():
            retval = _rval_BoolLiteral(ast)
        case asts.CallExpr():
            retval = _rval_CallExpr(ast)
        case asts.IdExpr():
            retval = _rval_IdExpr(ast)
        case asts.IntLiteral():
            retval = _rval_IntLiteral(ast)
        case asts.UnaryOp():
            retval = _rval_UnaryOp(ast)
        case _:
            raise NotImplementedError(f"Unknown type {ast}")
    return retval


def _rval_ArrayCell(ast: asts.ArrayCell) -> list[Insn]:
    # ast.semantic_type : SemanticType
    # ast.register : str
    # retval = _lval_ArrayCell(ast)
    # retval.append(Load(ast.register, ast.register))
    return []


def _rval_BinaryOp(ast: asts.BinaryOp) -> list[Insn]:
    # ast.semantic_type : SemanticType
    # ast.register : str
    # ast.op : Token
    if ast.op.value in ["and", "or"]:
        global labelNum
        fLabel = f"False{labelNum}"
        eLabel = f"End{labelNum}"
        labelNum += 1
        retval = flow(ast, fLabel, False)
        retval.extend(
            [
                Immediate(ast.register, 1),
                Jump(eLabel),
                Label(fLabel),
                Immediate(ast.register, 0),
                Label(eLabel),
            ]
        )
    else:
        retval = _rval_Expr(ast.left)
        retval.extend(_rval_Expr(ast.right))
        match ast.op.value:
            case "+":
                retval.append(Add(ast.register, ast.left.register, ast.right.register))
            case "-":
                retval.append(Sub(ast.register, ast.left.register, ast.right.register))
            case "*":
                retval.append(Mul(ast.register, ast.left.register, ast.right.register))
            case "/":
                retval.append(Div(ast.register, ast.left.register, ast.right.register))
            case "<=":
                retval.append(
                    LessThanEqual(ast.register, ast.left.register, ast.right.register)
                )
            case ">=":
                retval.append(
                    GreaterThanEqual(
                        ast.register, ast.left.register, ast.right.register
                    )
                )
            case "<":
                retval.append(
                    LessThan(ast.register, ast.left.register, ast.right.register)
                )
            case ">":
                retval.append(
                    GreaterThan(ast.register, ast.left.register, ast.right.register)
                )
            case "==":
                retval.append(
                    Equal(ast.register, ast.left.register, ast.right.register)
                )
            case "!=":
                retval.append(
                    NotEqual(ast.register, ast.left.register, ast.right.register)
                )
            case _:
                raise NotImplementedError("How")
    return retval


def flow(ast: asts.Expr, label: str, condition: bool) -> list[Insn]:
    global labelNum
    retval = []
    if isinstance(ast, asts.BinaryOp):
        if ast.op.value == "and":
            if condition:
                fall = f"fall{labelNum}"
                labelNum += 1
                retval.extend(flow(ast.left, fall, False))
                retval.extend(flow(ast.right, label, True))
                retval.append(Label(fall))
            else:
                retval.extend(flow(ast.left, label, False))
                retval.extend(flow(ast.right, label, False))
        elif ast.op.value == "or":
            if condition:
                retval.extend(flow(ast.left, label, True))
                retval.extend(flow(ast.right, label, True))
            else:
                fall = f"fall{labelNum}"
                labelNum += 1
                retval.extend(flow(ast.left, fall, True))
                retval.extend(flow(ast.right, label, False))
                retval.append(Label(fall))
        else:
            retval.extend(_rval_Expr(ast))
            if condition:
                retval.append(JumpIfNotZero(ast.register, label))
            else:
                retval.append(JumpIfZero(ast.register, label))
    elif isinstance(ast, asts.UnaryOp):
        retval.extend(flow(ast.expr, label, not condition))
    elif isinstance(ast, asts.BoolLiteral):
        if (ast.value and condition) or (not ast.value and not condition):
            retval.append(Jump(label))
    else:
        retval.extend(_rval_Expr(ast))
        if condition:
            retval.append(JumpIfNotZero(ast.register, label))
        else:
            retval.append(JumpIfZero(ast.register, label))
    return retval


def _rval_BoolLiteral(ast: asts.BoolLiteral) -> list[Insn]:
    # ast.semantic_type : SemanticType
    # ast.register : str
    # ast.token : Token
    # ast.value : bool
    if ast.token.value == "true":
        return [Immediate(ast.register, 1)]
    return [Immediate(ast.register, 0)]


def _rval_CallExpr(ast: asts.CallExpr) -> list[Insn]:
    # ast.semantic_type : SemanticType
    # ast.register : str
    retval: list[Insn]
    retval = []
    for i in range(int(ast.register[1:])):
        retval.append(AddImmediate("tmp", "SP", i))
        retval.append(Store("tmp", f"r{i}"))
    retval.append(AddImmediate("SP", "SP", int(ast.register[1:])))
    retval.append(AddImmediate("SP", "SP", len(ast.args) + 1))
    offset = -2
    for arg in ast.args:
        retval.extend(_rval_Argument(arg, offset))
        offset -= 1
    retval.extend(_lval_CallExpr(ast))
    if not isinstance(ast.semantic_type, asts.VoidType):
        retval.append(Load(ast.register, "RV"))
    retval.append(
        AddImmediate("SP", "SP", len(ast.args) * -1 - 1 - int(ast.register[1:]))
    )
    for i in range(int(ast.register[1:])):
        retval.append(AddImmediate("tmp", "SP", i))
        retval.append(Load(f"r{i}", "tmp"))
    return retval


def _rval_IdExpr(ast: asts.IdExpr) -> list[Insn]:
    # ast.semantic_type : SemanticType
    # ast.register : str
    # ast.id : Id
    retval = _lval_IdExpr(ast)
    retval.append(Load(ast.register, ast.register))
    return retval


def _rval_IntLiteral(ast: asts.IntLiteral) -> list[Insn]:
    # ast.semantic_type : SemanticType
    # ast.register : str
    # ast.token : Token
    return [Immediate(ast.register, int(ast.token.value))]


def _rval_UnaryOp(ast: asts.UnaryOp) -> list[Insn]:
    # ast.semantic_type : SemanticType
    # ast.register : str
    # ast.op : Token
    retval = _rval_Expr(ast.expr)
    if ast.op.value == "-":
        retval.append(Negate(ast.register, ast.expr.register))
    else:
        retval.append(Not(ast.register, ast.expr.register))
    return retval


def _rval_Argument(ast: asts.Argument, offset: int) -> list[Insn]:
    # ast.semantic_type : SemanticType
    retval = _rval_Expr(ast.expr)
    retval.extend(_lval_Argument(ast, offset))
    retval.append(Store("arg", ast.expr.register))
    return retval


def _lval_Expr(ast: asts.Expr) -> list[Insn]:
    retval: list[Insn]
    match ast:
        case asts.ArrayCell():
            retval = _lval_ArrayCell(ast)
        case asts.BinaryOp():
            retval = _lval_BinaryOp(ast)
        case asts.BoolLiteral():
            retval = _lval_BoolLiteral(ast)
        case asts.CallExpr():
            retval = _lval_CallExpr(ast)
        case asts.IdExpr():
            retval = _lval_IdExpr(ast)
        case asts.IntLiteral():
            retval = _lval_IntLiteral(ast)
        case asts.UnaryOp():
            retval = _lval_UnaryOp(ast)
        case _:
            raise NotImplementedError(f"Unknown type {ast}")
    return retval


def _lval_ArrayCell(ast: asts.ArrayCell) -> list[Insn]:
    # ast.semantic_type : SemanticType
    # ast.register : str
    return []


def _lval_BinaryOp(ast: asts.BinaryOp) -> list[Insn]:
    # ast.semantic_type : SemanticType
    # ast.register : str
    # ast.op : Token
    retval: list[Insn]
    _lval_Expr(ast.left)
    _lval_Expr(ast.right)
    return retval


def _lval_BoolLiteral(ast: asts.BoolLiteral) -> list[Insn]:
    # ast.semantic_type : SemanticType
    # ast.register : str
    # ast.token : Token
    # ast.value : bool
    retval: list[Insn]
    return retval


def _lval_CallExpr(ast: asts.CallExpr) -> list[Insn]:
    # ast.semantic_type : SemanticType
    # ast.register : str
    if not isinstance(ast.fn, asts.IdExpr):
        return []
    return [Call(ast.fn.id.token.value)]


def _lval_IdExpr(ast: asts.IdExpr) -> list[Insn]:
    # ast.semantic_type : SemanticType
    # ast.register : str
    # ast.id : Id
    return [AddImmediate(ast.register, "FP", ast.id.symbol.offset)]


def _lval_IntLiteral(ast: asts.IntLiteral) -> list[Insn]:
    # ast.semantic_type : SemanticType
    # ast.register : str
    # ast.token : Token
    retval: list[Insn]
    return retval


def _lval_UnaryOp(ast: asts.UnaryOp) -> list[Insn]:
    # ast.semantic_type : SemanticType
    # ast.register : str
    # ast.op : Token
    retval: list[Insn]
    _lval_Expr(ast.expr)
    return retval


def _lval_Argument(ast: asts.Argument, offset: int) -> list[Insn]:
    # ast.semantic_type : SemanticType
    return [AddImmediate("arg", "SP", offset)]
