#include"front/semantic.h"
#include<cassert>
#include <functional>
#include <unordered_map>
using ir::Instruction;
using ir::Function;
using ir::Operand;
using ir::Operator;
//TODO：引入新空间
using frontend::Analyzer;
using frontend::STE;
using ir::Type;
// ============================
// 通用宏定义
// ============================
#define ASSERT(msg) assert(0 && #msg);
#define GET_CHILD_PTR(node, type, index) auto node = dynamic_cast<type*>(root->children[index]); assert(node);
#define ANALYSIS(type, index, buffer) {auto node = dynamic_cast<type*>(root->children[index]); assert(node); analysis##type(node, buffer);}
#define COPY_EXP_NODE(src, dst) dst->is_computable = src->is_computable; dst->v = src->v; dst->t = src->t;
//TODO新增宏定义
#define COPY_ROOT_NODE(src) {COPY_EXP_NODE(src, root)}
#define SET_ROOT(opd) {root->v = opd.name; root->t = opd.type;}
#define CUR_NODE_IS(nd_type) (root->children[0]->type == frontend::NodeType::nd_type)
#define CUR_TYPE_IS(tk_type) (dynamic_cast<Term*>(root->children[0])->token.type == TokenType::tk_type)
#define IS_LITERAL(n) (n->t == Type::IntLiteral || n->t == Type::FloatLiteral)
#define ADD_INS(op1, op2, des, op) pgm.push_back(new Instruction(op1, op2, des, Operator::op));
#define MERGE_BLOCK(vec) pgm.insert(pgm.end(), vec.begin(), vec.end());
#define CREATE_OPERAND(opd, type) auto opd = Operand("t" + std::to_string(tmp_cnt++) , type);
#define EXPR_TO_OPERAND(node, out) auto out = IS_LITERAL(node) ? Literal2Var(node->t, node->v, pgm) : Operand(node->v, node->t);



map<std::string,ir::Function*>* frontend::get_lib_funcs() {
    static map<std::string,ir::Function*> lib_funcs = {
        {"getint", new Function("getint", Type::Int)},
        {"getch", new Function("getch", Type::Int)},
        {"getfloat", new Function("getfloat", Type::Float)},
        {"getarray", new Function("getarray", {Operand("arr", Type::IntPtr)}, Type::Int)},
        {"getfarray", new Function("getfarray", {Operand("arr", Type::FloatPtr)}, Type::Int)},
        {"putint", new Function("putint", {Operand("i", Type::Int)}, Type::null)},
        {"putch", new Function("putch", {Operand("i", Type::Int)}, Type::null)},
        {"putfloat", new Function("putfloat", {Operand("f", Type::Float)}, Type::null)},
        {"putarray", new Function("putarray", {Operand("n", Type::Int), Operand("arr", Type::IntPtr)}, Type::null)},
        {"putfarray", new Function("putfarray", {Operand("n", Type::Int), Operand("arr", Type::FloatPtr)}, Type::null)},
    };
    return &lib_funcs;
}

// ============================
// 符号条目结构体定义与构造
// ============================
STE::STE(){}
STE::STE(ir::Operand _operand, vector<int> _dimension, int _size, bool _isConst = false):operand(_operand), dimension(_dimension), size(_size), isConst(_isConst), val(string()){}
// ============================
// 符号表管理接口
// ============================
void frontend::SymbolTable::add_scope() {
    int scope_id = scope_stack.size();
    scope_stack.push_back({scope_id, "Scope" + std::to_string(scope_id)});
}
/// @brief 移除当前作用域
void frontend::SymbolTable::exit_scope() {
    scope_stack.pop_back();
}
/// @brief 获取当前作用域下变量的唯一命名标识
string frontend::SymbolTable::get_scoped_name(string id) const {
    return id + '_' + scope_stack.back().name;
}
/// @brief 查找符号表中变量的信息（支持作用域回溯）
frontend::STE frontend::SymbolTable::get_ste(string id) const {
    for (auto scope = scope_stack.rbegin(); scope != scope_stack.rend(); ++scope) {
        auto found = scope->table.find(id);
        if (found != scope->table.end())
            return found->second;
    }
    ASSERT(Symbol not found)
}
// ============================
// 分析器初始化
// ============================

/// @brief 构造函数：初始化分析器状态
Analyzer::Analyzer(): tmp_cnt(0), symbol_table(), cur_func(nullptr) {}
// ============================
// 程序入口：构建中间代码
// ============================

/// @brief 构建 IR 程序入口，包含全局变量和函数的整理
ir::Program Analyzer::get_ir_program(CompUnit* root) {
    ir::Program program;

    // 注册内置函数（用于后续函数调用检查）
    auto libs = get_lib_funcs();
    for(auto lib = libs->begin(); lib != libs->end(); lib++)
    symbol_table.functions[lib->first] = lib->second;

    symbol_table.add_scope();
    
    // 构建所有顶层语法单元
    analysisCompUnit(root);
    
     // 收集全局变量至 IR 全局值表
    for (auto& entry : symbol_table.scope_stack[0].table) {
        auto& ste = entry.second;
        if (!ste.dimension.empty())
            program.globalVal.push_back({ste.operand, ste.size});
        else
            program.globalVal.push_back({ste.operand, 0});
    }
    // 创建 _global 虚拟函数容纳全局初始化 IR
    Function glb("_global", Type::null);// 定义全局函数
    glb.InstVec = g_init_inst;
    glb.addInst(new Instruction({},{},{},Operator::_return));

    program.addFunction(glb);

    // 添加用户定义的函数到程序
    for (auto it = symbol_table.functions.begin(); it != symbol_table.functions.end(); ++it) {
    const std::string& name = it->first;
    ir::Function* func = it->second;

    if (libs->count(name)) continue;
    if (name == "main") {
        func->InstVec.insert(func->InstVec.begin(), new ir::CallInst(Operand("_global", Type::null), {}));
    }
    program.addFunction(*func);
    }

    symbol_table.exit_scope();
    return program;
}
/// @brief 分析顶层语法单元（声明或函数定义）
///        并按深度优先顺序递归处理子 CompUnit
void Analyzer::analysisCompUnit(CompUnit* root)
{
    if (CUR_NODE_IS(DECL)) {
        ANALYSIS(Decl, 0, g_init_inst)
    } else {
        GET_CHILD_PTR(func, FuncDef, 0)
        symbol_table.add_scope();
        analysisFuncDef(func);
        symbol_table.exit_scope();
    }

    if (int(root->children.size()) > 1) {
        GET_CHILD_PTR(next, CompUnit, 1)
        analysisCompUnit(next);
    }
}

/// @brief 分析声明（区分常量定义或变量定义）
void Analyzer::analysisDecl(Decl* root, std::vector<ir::Instruction*>& pgm)
{
    if (CUR_NODE_IS(CONSTDECL)) {
        ANALYSIS(ConstDecl, 0, pgm)
    } else {
        ANALYSIS(VarDecl, 0, pgm)
    }
}

/// @brief 分析常量声明，提取类型和多个常量定义项
void Analyzer::analysisConstDecl(ConstDecl* root, std::vector<ir::Instruction*>& pgm)
{
    GET_CHILD_PTR(btyp, BType, 1)
    analysisBType(btyp);
    root->t = btyp->t;

    for (int idx = 2; idx < int(root->children.size()); idx += 2) {
        GET_CHILD_PTR(cstdef, ConstDef, idx)
        analysisConstDef(cstdef, pgm, root->t);
    }
}

/// @brief 分析类型标识（int 或 float）
void Analyzer::analysisBType(BType* root)
{
    if (CUR_TYPE_IS(INTTK)) {
        root->t = Type::Int;
    } else if (CUR_TYPE_IS(FLOATTK)) {
        root->t = Type::Float;
    } else {
        ASSERT(Invalid BType)
    }
}

/// @brief 分析常量定义项，包括变量和数组常量
void Analyzer::analysisConstDef(ConstDef* root, std::vector<ir::Instruction*>& pgm, ir::Type typ)
{
    GET_CHILD_PTR(idf, Term, 0)
    root->arr_name = symbol_table.get_scoped_name(idf->token.value);

    std::vector<int> dim;
    int size = 1;

    for (int idx = 2; idx < int(root->children.size()); idx += 3)
    {
        if (root->children[idx]->type != frontend::NodeType::CONSTEXP)
            break;

        GET_CHILD_PTR(cstexp, ConstExp, idx)
        analysisConstExp(cstexp);
        assert(cstexp->t == Type::IntLiteral);

        int len = std::stoi(cstexp->v);
        dim.push_back(len);
        size *= len;
    }

    size *= !dim.empty();  // 数组非空时保留 size，否则为 0

    GET_CHILD_PTR(cstval, ConstInitVal, int(root->children.size()) - 1)
    cstval->v = idf->token.value;

    switch (typ)
    {
        case Type::Int:
            if (dim.empty()) {
                // 单值整数常量
                symbol_table.scope_stack.back().table[idf->token.value] =
                    STE(Operand(root->arr_name, Type::Int), dim, size, true);
                cstval->t = Type::Int;
            } else {
                // 整数常量数组
                symbol_table.scope_stack.back().table[idf->token.value] =
                    STE(Operand(root->arr_name, Type::IntPtr), dim, size, true);
                cstval->t = Type::IntLiteral;

                if (int(symbol_table.scope_stack.size()) > 1) {
                    ADD_INS(Operand(std::to_string(size), Type::IntLiteral), {}, Operand(root->arr_name, Type::IntPtr), alloc)
                }
            }
            break;

        case Type::Float:
            if (dim.empty()) {
                // 单值浮点常量
                symbol_table.scope_stack.back().table[idf->token.value] =
                    STE(Operand(root->arr_name, Type::Float), dim, size, true);
                cstval->t = Type::Float;
            } else {
                // 浮点常量数组
                symbol_table.scope_stack.back().table[idf->token.value] =
                    STE(Operand(root->arr_name, Type::FloatPtr), dim, size, true);
                cstval->t = Type::FloatLiteral;

                if (int(symbol_table.scope_stack.size()) > 1) {
                    ADD_INS(Operand(std::to_string(size), Type::IntLiteral), {}, Operand(root->arr_name, Type::FloatPtr), alloc)
                }
            }
            break;

        default:
            ASSERT(Invalid ConstDef)
    }

    analysisConstInitVal(cstval, pgm, size, 0, 0, dim);
}

/// @brief 分析并生成常量初始化对应的中间代码
void Analyzer::analysisConstInitVal(ConstInitVal* root, std::vector<ir::Instruction*>& pgm, int size, int cur, int offset, std::vector<int>& dim) {
    auto name = symbol_table.get_scoped_name(root->v);  // 获取作用域内变量名

    if (size) {  // 处理数组初始化值
        if (dynamic_cast<VarDef*>(root->parent) || size > 1) {
            assert(cur < int(dim.size()));
            size /= dim[cur];  // 当前维度展开长度
            int cnt = 0, total = int(root->children.size()) / 2;

            // 递归初始化每个元素
            for (int idx = 1; idx < int(root->children.size()) - 1; idx += 2) {
                GET_CHILD_PTR(cstval, ConstInitVal, idx);
                cstval->v = root->v;  // 向子节点传递变量名和类型
                cstval->t = root->t;

                // 判断是否进入下一维
                if (total <= dim[cur])
                    analysisConstInitVal(cstval, pgm, size, cur + 1, offset + cnt * size, dim);
                else
                    analysisConstInitVal(cstval, pgm, 1, cur, offset + cnt, dim);
                cnt++;
            }

            // 多余位置填充默认值（0 或 0.0）
            for (int idx = cnt * size; idx < int(dim[cur] * size); ++idx) {
                Type type = (root->t == Type::IntLiteral) ? Type::IntPtr : Type::FloatPtr;
                std::string val = (root->t == Type::IntLiteral) ? "0" : "0.0";
                ADD_INS(Operand(name, type), Operand(std::to_string(idx), Type::IntLiteral), Operand(val, root->t), store)
            }
        } else {  // 单值数组初始化
            assert(CUR_NODE_IS(CONSTEXP));
            GET_CHILD_PTR(cstexp, ConstExp, 0);
            analysisConstExp(cstexp);

            if (root->t == Type::IntLiteral) {
                if (cstexp->t == Type::FloatLiteral) {
                    cstexp->v = std::to_string(int(std::stof(cstexp->v)));
                    cstexp->t = Type::IntLiteral;
                }
                assert(cstexp->t == Type::IntLiteral);
                ADD_INS(Operand(name, Type::IntPtr), Operand(std::to_string(offset), Type::IntLiteral), Operand(cstexp->v, cstexp->t), store)
            } else if (root->t == Type::FloatLiteral) {
                if (cstexp->t == Type::IntLiteral) {
                    cstexp->v = std::to_string(float(std::stoi(cstexp->v)));
                    cstexp->t = Type::FloatLiteral;
                }
                assert(cstexp->t == Type::FloatLiteral);
                ADD_INS(Operand(name, Type::FloatPtr), Operand(std::to_string(offset), Type::IntLiteral), Operand(cstexp->v, cstexp->t), store)
            } else {
                ASSERT(Invalid ConstExp);
            }
        }
    } else {  // 单变量初始化
        assert(CUR_NODE_IS(CONSTEXP));
        GET_CHILD_PTR(cstexp, ConstExp, 0);
        analysisConstExp(cstexp);
        assert(IS_LITERAL(cstexp));

        switch (root->t) {
            case Type::Int:
                if (cstexp->t == Type::FloatLiteral) {
                    cstexp->v = std::to_string(int(std::stof(cstexp->v)));
                    cstexp->t = Type::IntLiteral;
                }
                if (int(symbol_table.scope_stack.size()) > 1)
                    ADD_INS(Operand(cstexp->v, cstexp->t), {}, Operand(name, Type::Int), def)
                else
                    ADD_INS(Operand(cstexp->v, cstexp->t), {}, Operand(name, Type::Int), mov)
                symbol_table.scope_stack.back().table[root->v].val = cstexp->v;
                break;

            case Type::Float:
                if (cstexp->t == Type::IntLiteral) {
                    cstexp->v = std::to_string(float(std::stoi(cstexp->v)));
                    cstexp->t = Type::FloatLiteral;
                }
                if (int(symbol_table.scope_stack.size()) > 1)
                    ADD_INS(Operand(cstexp->v, cstexp->t), {}, Operand(name, Type::Float), fdef)
                else
                    ADD_INS(Operand(cstexp->v, cstexp->t), {}, Operand(name, Type::Float), fmov)
                symbol_table.scope_stack.back().table[root->v].val = cstexp->v;
                break;

            default:
                ASSERT(Invalid ConstExp);
        }
    }
}
/// @brief 分析变量声明，提取类型信息并依次处理每个 VarDef 项
void Analyzer::analysisVarDecl(VarDecl* root, std::vector<ir::Instruction*>& pgm) {
    GET_CHILD_PTR(btyp, BType, 0);
    analysisBType(btyp);
    root->t = btyp->t;

    for (int idx = 1; idx < int(root->children.size()); idx += 2) {
        GET_CHILD_PTR(vardef, VarDef, idx);
        analysisVarDef(vardef, pgm, root->t);
    }
}

/// @brief 分析单个变量定义项，生成初始化或默认定义指令
void Analyzer::analysisVarDef(VarDef* root, std::vector<ir::Instruction*>& pgm, Type typ) {
    GET_CHILD_PTR(idf, Term, 0);
    root->arr_name = symbol_table.get_scoped_name(idf->token.value);

    std::vector<int> dim;
    int size = 1;

    // 收集数组维度并计算总元素数量
    for (int idx = 2; idx < int(root->children.size()); idx += 3) {
        if (root->children[idx]->type != frontend::NodeType::CONSTEXP) break;
        GET_CHILD_PTR(cstexp, ConstExp, idx);
        analysisConstExp(cstexp);
        assert(cstexp->t == Type::IntLiteral);
        int len = std::stoi(cstexp->v);
        dim.push_back(len);
        size *= len;
    }
    size *= !dim.empty();

    // 判断是否包含初始化值
    bool has_init = root->children.back()->type == frontend::NodeType::INITVAL;

    if (has_init) {
        GET_CHILD_PTR(initval, InitVal, int(root->children.size()) - 1);
        initval->v = root->arr_name;

        switch (typ) {
            case Type::Int:
                initval->t = Type::Int;
                symbol_table.scope_stack.back().table[idf->token.value] =
                    STE(Operand(root->arr_name, dim.empty() ? Type::Int : Type::IntPtr), dim, size);
                if (!dim.empty() && symbol_table.scope_stack.size() > 1)
                    ADD_INS(Operand(std::to_string(size), Type::IntLiteral), {}, Operand(root->arr_name, Type::IntPtr), alloc)
                break;

            case Type::Float:
                initval->t = Type::Float;
                symbol_table.scope_stack.back().table[idf->token.value] =
                    STE(Operand(root->arr_name, dim.empty() ? Type::Float : Type::FloatPtr), dim, size);
                if (!dim.empty() && symbol_table.scope_stack.size() > 1)
                    ADD_INS(Operand(std::to_string(size), Type::IntLiteral), {}, Operand(root->arr_name, Type::FloatPtr), alloc)
                break;

            default:
                ASSERT(Invalid Def);
        }

        analysisInitVal(initval, pgm, size, 0, 0, dim);
    } else {
        switch (typ) {
            case Type::Int:
                symbol_table.scope_stack.back().table[idf->token.value] =
                    STE(Operand(root->arr_name, dim.empty() ? Type::Int : Type::IntPtr), dim, size);
                if (dim.empty() && symbol_table.scope_stack.size() > 1)
                    ADD_INS(Operand("114514", Type::IntLiteral), {}, Operand(root->arr_name, Type::Int), def)
                else if (!dim.empty() && symbol_table.scope_stack.size() > 1)
                    ADD_INS(Operand(std::to_string(size), Type::IntLiteral), {}, Operand(root->arr_name, Type::IntPtr), alloc)
                break;

            case Type::Float:
                symbol_table.scope_stack.back().table[idf->token.value] =
                    STE(Operand(root->arr_name, dim.empty() ? Type::Float : Type::FloatPtr), dim, size);
                if (dim.empty() && symbol_table.scope_stack.size() > 1)
                    ADD_INS(Operand("114.514", Type::FloatLiteral), {}, Operand(root->arr_name, Type::Float), fdef)
                else if (!dim.empty() && symbol_table.scope_stack.size() > 1)
                    ADD_INS(Operand(std::to_string(size), Type::IntLiteral), {}, Operand(root->arr_name, Type::FloatPtr), alloc)
                break;

            default:
                ASSERT(Invalid Def);
        }
    }
}
/// @brief 分析变量初始化值，支持数组递归填充或表达式求值赋值
void Analyzer::analysisInitVal(InitVal* root, std::vector<ir::Instruction*>& pgm, int size, int cur, int offset, std::vector<int>& dim) {
    if (size) {  // 数组初始化
        // 判断是否需递归展开数组维度
        if (dynamic_cast<VarDef*>(root->parent) || size > 1) {
            assert(cur < int(dim.size()));
            size /= dim[cur];
            int cnt = 0;
            int total = int(root->children.size()) / 2;

            for (int idx = 1; idx < int(root->children.size()) - 1; idx += 2) {
                GET_CHILD_PTR(sub, InitVal, idx);
                sub->v = root->v;
                sub->t = root->t;
                if (total <= dim[cur])
                    analysisInitVal(sub, pgm, size, cur + 1, offset + cnt * size, dim);
                else
                    analysisInitVal(sub, pgm, 1, cur, offset + cnt, dim);
                cnt++;
            }

            // 对未被赋值位置填充默认值
            for (int idx = cnt * size; idx < int(dim[cur] * size); ++idx) {
                Type ptr_type = (root->t == Type::Int) ? Type::IntPtr : Type::FloatPtr;
                Type val_type = (root->t == Type::Int) ? Type::IntLiteral : Type::FloatLiteral;
                std::string val = (root->t == Type::Int) ? "0" : "0.0";
                ADD_INS(Operand(root->v, ptr_type), Operand(std::to_string(idx), Type::IntLiteral), Operand(val, val_type), store)
            }
        } else {  // 简单表达式初始化（size == 1）
            GET_CHILD_PTR(exp, Exp, 0);
            analysisExp(exp, pgm);
            assert(IS_LITERAL(exp));

            switch (root->t) {
                case Type::Int:
                    if (exp->t == Type::FloatLiteral) {
                        exp->v = std::to_string(int(std::stof(exp->v)));
                        exp->t = Type::IntLiteral;
                    }
                    ADD_INS(Operand(root->v, Type::IntPtr), Operand(std::to_string(offset), Type::IntLiteral), Operand(exp->v, exp->t), store)
                    break;

                case Type::Float:
                    if (exp->t == Type::IntLiteral) {
                        exp->v = std::to_string(float(std::stoi(exp->v)));
                        exp->t = Type::FloatLiteral;
                    }
                    ADD_INS(Operand(root->v, Type::FloatPtr), Operand(std::to_string(offset), Type::IntLiteral), Operand(exp->v, exp->t), store)
                    break;

                default:
                    ASSERT(Invalid Exp);
            }
        }
    } else {  // 非数组变量赋值
        assert(CUR_NODE_IS(EXP));
        GET_CHILD_PTR(exp, Exp, 0);
        analysisExp(exp, pgm);

        // 直接使用字面量赋值（全局或可确定值）
        if (root->t == Type::Int && exp->t == Type::IntLiteral) {
            if (symbol_table.scope_stack.size() > 1)
                ADD_INS(Operand(exp->v, exp->t), {}, Operand(root->v, Type::Int), def)
            else
                ADD_INS(Operand(exp->v, exp->t), {}, Operand(root->v, Type::Int), mov)
        } else if (root->t == Type::Float && exp->t == Type::FloatLiteral) {
            if (symbol_table.scope_stack.size() > 1)
                ADD_INS(Operand(exp->v, exp->t), {}, Operand(root->v, Type::Float), fdef)
            else
                ADD_INS(Operand(exp->v, exp->t), {}, Operand(root->v, Type::Float), fmov)
        } else {
            // 需运行时加载表达式值
            EXPR_TO_OPERAND(exp, expvar);
            switch (root->t) {
                case Type::Int:
                    if (expvar.type == Type::Float) {
                        ADD_INS(expvar, {}, Operand(root->v, Type::Int), cvt_f2i)
                    } else {
                        if (symbol_table.scope_stack.size() > 1)
                            ADD_INS(expvar, {}, Operand(root->v, Type::Int), def)
                        else
                            ADD_INS(expvar, {}, Operand(root->v, Type::Int), mov)
                    }
                    break;

                case Type::Float:
                    if (exp->t == Type::Int) {
                        ADD_INS(expvar, {}, Operand(root->v, Type::Float), cvt_i2f)
                    } else {
                        if (symbol_table.scope_stack.size() > 1)
                            ADD_INS(expvar, {}, Operand(root->v, Type::Float), fdef)
                        else
                            ADD_INS(expvar, {}, Operand(root->v, Type::Float), fmov)
                    }
                    break;

                default:
                    ASSERT(Invalid Exp);
            }
        }
    }
}

/// @brief 分析函数定义，注册函数信息并处理函数体
void Analyzer::analysisFuncDef(FuncDef* root) {
    GET_CHILD_PTR(ftyp, FuncType, 0)
    auto typ = analysisFuncType(ftyp);

    GET_CHILD_PTR(fname, Term, 1)
    auto name = fname->token.value;

    std::vector<Operand> params;
    if (int(root->children.size()) > 5)  // 存在形参列表
        ANALYSIS(FuncFParams, 3, params)

    // 构造函数对象并注册至符号表
    Function* func = new Function(name, params, typ);
    symbol_table.functions[name] = func;
    cur_func = func;

    // 分析函数体块，填充中间代码
    ANALYSIS(Block, int(root->children.size()) - 1, func->InstVec)

    // 针对 main 函数或 void 函数补充 return
    if (name == "main")
        func->addInst(new Instruction(Operand("0", Type::IntLiteral), {}, {}, Operator::_return));
    if (typ == Type::null)
        func->addInst(new Instruction({}, {}, {}, Operator::_return));
}

/// @brief 解析函数返回类型标识
Type Analyzer::analysisFuncType(FuncType* root) {
    if (CUR_TYPE_IS(INTTK)) return Type::Int;
    if (CUR_TYPE_IS(FLOATTK)) return Type::Float;
    if (CUR_TYPE_IS(VOIDTK)) return Type::null;
    ASSERT(Invalid FuncType)
}

/// @brief 分析函数形参列表，收集所有参数信息
void Analyzer::analysisFuncFParams(FuncFParams* root, std::vector<Operand>& params) {
    for (int idx = 0; idx < int(root->children.size()); idx += 2)
        ANALYSIS(FuncFParam, idx, params)
}

/// @brief 分析单个函数参数，支持数组维度参数
void Analyzer::analysisFuncFParam(FuncFParam* root, std::vector<Operand>& params) {
    GET_CHILD_PTR(btyp, BType, 0)
    analysisBType(btyp);

    GET_CHILD_PTR(idf, Term, 1)
    auto name = symbol_table.get_scoped_name(idf->token.value);

    std::vector<int> dim;
    int size = -1;

    if (int(root->children.size()) > 2) {
        // 标记为数组指针类型
        if (btyp->t == Type::Int)
            btyp->t = Type::IntPtr;
        else if (btyp->t == Type::Float)
            btyp->t = Type::FloatPtr;
        else
            ASSERT(Invalid type)

        dim.push_back(-1);  // 第一维未知

        for (int idx = 5; idx < int(root->children.size()); idx += 2) {
            GET_CHILD_PTR(cstexp, ConstExp, idx)
            analysisConstExp(cstexp);
            assert(cstexp->t == Type::IntLiteral);
            int len = std::stoi(cstexp->v);
            dim.push_back(len);
            size *= len;
        }
    }

    size *= !dim.empty();

    // 将参数添加至符号表和参数表
    symbol_table.scope_stack.back().table[idf->token.value] = STE(Operand(name, btyp->t), dim, size);
    params.push_back({name, btyp->t});
}

/// @brief 分析代码块节点，依次处理其中每个 BlockItem 项
void Analyzer::analysisBlock(Block* root, std::vector<ir::Instruction*>& pgm) {
    for (int idx = 1; idx < int(root->children.size()) - 1; ++idx)
        ANALYSIS(BlockItem, idx, pgm)
}

/// @brief 分析块中项，区分声明和语句
void Analyzer::analysisBlockItem(BlockItem* root, std::vector<ir::Instruction*>& pgm) {
    if (CUR_NODE_IS(DECL))
        ANALYSIS(Decl, 0, pgm)
    else
        ANALYSIS(Stmt, 0, pgm)
}
/// @brief 分析语句节点，包括赋值、控制流、return、表达式等
/// @brief 分析语句节点，包括赋值、控制流、return、表达式等
void Analyzer::analysisStmt(Stmt* root, std::vector<ir::Instruction*>& pgm) {
    if (CUR_NODE_IS(LVAL)) {
        // LVal = Exp 类型的赋值语句
        GET_CHILD_PTR(lval, LVal, 0)
        GET_CHILD_PTR(exp, Exp, 2)
        analysisLVal(lval, pgm);
        assert(!IS_LITERAL(lval));
        analysisExp(exp, pgm);

        EXPR_TO_OPERAND(lval, var)
        EXPR_TO_OPERAND(exp, expvar)

        if (lval->t == Type::Int || lval->t == Type::Float) {
            if (var.type == Type::Int && expvar.type == Type::Int)
                ADD_INS(expvar, {}, var, mov)
            else if (var.type == Type::Int && expvar.type == Type::Float) {
                CREATE_OPERAND(tmp, Type::Int)
                ADD_INS(expvar, {}, tmp, cvt_f2i)
                ADD_INS(tmp, {}, var, mov)
            } else if (var.type == Type::Float && expvar.type == Type::Int) {
                CREATE_OPERAND(tmp, Type::Float)
                ADD_INS(expvar, {}, tmp, cvt_i2f)
                ADD_INS(tmp, {}, var, fmov)
            } else {
                ADD_INS(expvar, {}, var, fmov)
            }
        } else if (lval->t == Type::IntPtr || lval->t == Type::FloatPtr) {
            Operand offset = Operand(lval->offset, Type::Int);
            ADD_INS(var, offset, expvar, store)
        } else {
            ASSERT(Invalid LVal)
        }
    } else if (CUR_NODE_IS(BLOCK)) {
        symbol_table.add_scope();
        ANALYSIS(Block, 0, pgm)
        symbol_table.exit_scope();
    } else if (CUR_NODE_IS(TERMINAL)) {
        // 控制结构与跳转语句
        if (CUR_TYPE_IS(IFTK)) {
            GET_CHILD_PTR(cond, Cond, 2)
            analysisCond(cond, pgm);
            std::vector<Instruction*> if_bbl;
            ANALYSIS(Stmt, 4, if_bbl)

            if (IS_LITERAL(cond)) {
                // 编译期可确定条件值，静态判断
                if ((cond->t == Type::IntLiteral && std::stoi(cond->v) != 0) ||
                    (cond->t == Type::FloatLiteral && std::stof(cond->v) != 0)) {
                    MERGE_BLOCK(if_bbl)
                } else if (root->children.size() > 5) {
                    ANALYSIS(Stmt, int(root->children.size()) - 1, pgm)
                }
            } else {
                // 生成运行时判断与跳转
                if (cond->t == Type::Float) {
                    CREATE_OPERAND(tmp, Type::Float)
                    ADD_INS(Operand(cond->v, cond->t), Operand("0.0", Type::FloatLiteral), tmp, fneq)
                    ADD_INS(tmp, {}, Operand("2", Type::IntLiteral), _goto)
                } else {
                    ADD_INS(Operand(cond->v, cond->t), {}, Operand("2", Type::IntLiteral), _goto)
                }

                if (root->children.size() > 5) {
                    ADD_INS({}, {}, Operand(std::to_string(int(if_bbl.size()) + 2), Type::IntLiteral), _goto)
                    MERGE_BLOCK(if_bbl)

                    std::vector<Instruction*> else_bbl;
                    ANALYSIS(Stmt, int(root->children.size()) - 1, else_bbl)
                    ADD_INS({}, {}, Operand(std::to_string(int(else_bbl.size()) + 1), Type::IntLiteral), _goto)
                    MERGE_BLOCK(else_bbl)
                } else {
                    ADD_INS({}, {}, Operand(std::to_string(int(if_bbl.size()) + 1), Type::IntLiteral), _goto)
                    MERGE_BLOCK(if_bbl)
                }
                ADD_INS({}, {}, {}, __unuse__)
            }
        } else if (CUR_TYPE_IS(WHILETK)) {
            int last = pgm.size();
            GET_CHILD_PTR(cond, Cond, 2)
            analysisCond(cond, pgm);
            std::vector<Instruction*> while_bbl;
            ANALYSIS(Stmt, 4, while_bbl)

            if (!IS_LITERAL(cond)) {
                if (cond->t == Type::Float) {
                    CREATE_OPERAND(tmp, Type::Int)
                    ADD_INS(Operand(cond->v, cond->t), {}, tmp, fneq)
                    ADD_INS(tmp, {}, Operand("2", Type::IntLiteral), _goto)
                } else {
                    ADD_INS(Operand(cond->v, cond->t), {}, Operand("2", Type::IntLiteral), _goto)
                }
                ADD_INS({}, {}, Operand(std::to_string(int(while_bbl.size()) + 2), Type::IntLiteral), _goto)
            }

            for (int idx = 0; idx < int(while_bbl.size()); ++idx) {
                auto& ins = while_bbl[idx];
                if (ins->op1.name == "break") {
                    ins = new Instruction({}, {}, Operand(std::to_string(int(while_bbl.size()) - idx + 1), Type::IntLiteral), Operator::_goto);
                } else if (ins->op1.name == "continue") {
                    ins = new Instruction({}, {}, Operand(std::to_string(last - int(pgm.size()) - idx), Type::IntLiteral), Operator::_goto);
                }
            }

            if (IS_LITERAL(cond)) {
                if ((cond->t == Type::IntLiteral && std::stoi(cond->v) != 0) ||
                    (cond->t == Type::FloatLiteral && std::stof(cond->v) != 0)) {
                    MERGE_BLOCK(while_bbl)
                    ADD_INS({}, {}, Operand(std::to_string(-int(while_bbl.size())), Type::IntLiteral), _goto)
                }
            } else {
                MERGE_BLOCK(while_bbl)
                ADD_INS({}, {}, Operand(std::to_string(last - int(pgm.size())), Type::IntLiteral), _goto)
                ADD_INS({}, {}, {}, __unuse__)
            }
        } else if (CUR_TYPE_IS(BREAKTK)) {
            ADD_INS({"break"}, {}, {}, __unuse__)
        } else if (CUR_TYPE_IS(CONTINUETK)) {
            ADD_INS({"continue"}, {}, {}, __unuse__)
        } else if (CUR_TYPE_IS(RETURNTK)) {
            if (int(root->children.size()) > 2) {
                GET_CHILD_PTR(exp, Exp, 1)
                analysisExp(exp, pgm);

                switch (cur_func->returnType) {
                    case Type::Int:
                        if (exp->t == Type::Float) {
                            CREATE_OPERAND(tmp, Type::Int)
                            ADD_INS(Operand(exp->v, exp->t), {}, tmp, cvt_f2i)
                            ADD_INS(tmp, {}, {}, _return)
                        } else {
                            if (exp->t == Type::FloatLiteral) {
                                exp->v = std::to_string(int(std::stof(exp->v)));
                                exp->t = Type::IntLiteral;
                            }
                            ADD_INS(Operand(exp->v, exp->t), {}, {}, _return)
                        }
                        break;
                    case Type::Float:
                        if (exp->t == Type::Int) {
                            CREATE_OPERAND(tmp, Type::Float)
                            ADD_INS(Operand(exp->v, exp->t), {}, tmp, cvt_i2f)
                            ADD_INS(tmp, {}, {}, _return)
                        } else {
                            if (exp->t == Type::IntLiteral) {
                                exp->v = std::to_string(float(std::stoi(exp->v)));
                                exp->t = Type::FloatLiteral;
                            }
                            ADD_INS(Operand(exp->v, exp->t), {}, {}, _return)
                        }
                        break;
                    default:
                        ASSERT(Invalid type)
                }
            } else {
                assert(cur_func->returnType == Type::null);
                ADD_INS({}, {}, {}, _return)
            }
        }
    } else {
        if (int(root->children.size()) > 1)
            ANALYSIS(Exp, 0, pgm)
    }
}

/// @brief 分析常量表达式，要求结果为编译期常量
void Analyzer::analysisConstExp(ConstExp* root) {
    GET_CHILD_PTR(add, AddExp, 0)
    std::vector<Instruction*> bbl;
    analysisAddExp(add, bbl);
    COPY_ROOT_NODE(add)
    assert(IS_LITERAL(add) && bbl.empty());
}

/// @brief 分析表达式 Exp -> AddExp
void Analyzer::analysisExp(Exp* root, std::vector<ir::Instruction*>& pgm) {
    GET_CHILD_PTR(add, AddExp, 0)
    analysisAddExp(add, pgm);
    COPY_ROOT_NODE(add)
}

/// @brief 分析条件表达式 Cond -> LOrExp
void Analyzer::analysisCond(Cond* root, std::vector<ir::Instruction*>& pgm) {
    GET_CHILD_PTR(lor, LOrExp, 0)
    analysisLOrExp(lor, pgm);
    COPY_ROOT_NODE(lor)
}

/// @brief 分析逻辑或表达式，支持编译期折叠与运行时控制跳转生成
void Analyzer::analysisLOrExp(LOrExp* root, std::vector<ir::Instruction*>& pgm) {
    GET_CHILD_PTR(land, LAndExp, 0)
    std::vector<Instruction*> land_bbl;
    analysisLAndExp(land, land_bbl);

    if (root->children.size() > 1) {
        GET_CHILD_PTR(lor, LOrExp, 2)
        std::vector<Instruction*> lor_bbl;
        analysisLOrExp(lor, lor_bbl);

        if (IS_LITERAL(land) && IS_LITERAL(lor)) {
            // 编译期可折叠的布尔表达式
            float lhs = land->t == Type::IntLiteral ? std::stoi(land->v) : std::stof(land->v);
            float rhs = lor->t == Type::IntLiteral ? std::stoi(lor->v) : std::stof(lor->v);
            root->v = std::to_string(int(lhs || rhs));
            root->t = Type::IntLiteral;
        } else if (IS_LITERAL(land)) {
            // 编译期左侧常量，右侧为变量
            bool truthy = (land->t == Type::IntLiteral && std::stoi(land->v) != 0) ||
                          (land->t == Type::FloatLiteral && std::stof(land->v) != 0);
            if (truthy) {
                root->v = "1";
                root->t = Type::IntLiteral;
            } else {
                MERGE_BLOCK(lor_bbl)
                COPY_ROOT_NODE(lor)
            }
        } else {
            // 两边皆为变量，需要生成 IR
            EXPR_TO_OPERAND(land, op1)
            EXPR_TO_OPERAND(lor, op2)
            CREATE_OPERAND(des, Type::Int)
            MERGE_BLOCK(land_bbl)
            ADD_INS(op1, {}, des, mov)
            ADD_INS(des, {}, Operand(std::to_string(int(lor_bbl.size()) + 2), Type::IntLiteral), _goto)
            MERGE_BLOCK(lor_bbl)
            ADD_INS(des, op2, des, _or)
            SET_ROOT(des)
        }
    } else {
        if (!IS_LITERAL(land))
            MERGE_BLOCK(land_bbl)
        COPY_ROOT_NODE(land)
    }
}


/// @brief 分析逻辑与表达式，支持编译期折叠和短路逻辑优化
void Analyzer::analysisLAndExp(LAndExp* root, std::vector<ir::Instruction*>& pgm) {
    GET_CHILD_PTR(eq, EqExp, 0)
    std::vector<Instruction*> eq_bbl;
    analysisEqExp(eq, eq_bbl);

    if (root->children.size() > 1) {
        GET_CHILD_PTR(land, LAndExp, 2)
        std::vector<Instruction*> land_bbl;
        analysisLAndExp(land, land_bbl);

        if (IS_LITERAL(eq) && IS_LITERAL(land)) {
            // 编译期可折叠逻辑表达式
            float lhs = eq->t == Type::IntLiteral ? std::stoi(eq->v) : std::stof(eq->v);
            float rhs = land->t == Type::IntLiteral ? std::stoi(land->v) : std::stof(land->v);
            root->v = std::to_string(int(lhs && rhs));
            root->t = Type::IntLiteral;
        } else if (IS_LITERAL(eq)) {
            // 编译期已知的短路逻辑，左值为假直接返回 false
            bool is_false = (eq->t == Type::IntLiteral && std::stoi(eq->v) == 0) ||
                            (eq->t == Type::Float && std::stof(eq->v) == 0);
            if (is_false) {
                root->v = "0";
                root->t = Type::IntLiteral;
            } else {
                MERGE_BLOCK(land_bbl)
                COPY_ROOT_NODE(land)
            }
        } else {
            // 运行期两个变量都需判断，构造控制流
            EXPR_TO_OPERAND(eq, op1)
            EXPR_TO_OPERAND(land, op2)
            CREATE_OPERAND(des, Type::Int)
            MERGE_BLOCK(eq_bbl)

            CREATE_OPERAND(tmp, Type::Int)
            ADD_INS(op1, {}, des, mov)
            ADD_INS(des, Operand("0", Type::IntLiteral), tmp, eq)
            ADD_INS(tmp, {}, Operand(std::to_string(int(land_bbl.size()) + 2), Type::IntLiteral), _goto)
            MERGE_BLOCK(land_bbl)
            ADD_INS(des, op2, des, _and)
            SET_ROOT(des)
        }
    } else {
        if (!IS_LITERAL(eq))
            MERGE_BLOCK(eq_bbl)
        COPY_ROOT_NODE(eq)
    }
}
/// @brief 加减乘除二元模板函数
template<typename NodeT, typename SubExpT>
void analyzeBinary(Analyzer* self,NodeT* root,
                   std::vector<ir::Instruction*>& pgm,
                   const std::function<void(frontend::Term*)>& check_operator,
                   const std::function<void(SubExpT*, std::vector<ir::Instruction*>&)>& analyze_subexp) {
    auto lhs = dynamic_cast<SubExpT*>(root->children[0]);
    analyze_subexp(lhs, pgm);
    int var_start_idx = -1;

    if (IS_LITERAL(lhs)) {
        for (int i = 2; i < int(root->children.size()); i += 2) {
            std::vector<ir::Instruction*> tmp;
            auto term = dynamic_cast<frontend::Term*>(root->children[i - 1]);
            auto rhs = dynamic_cast<SubExpT*>(root->children[i]);
            analyze_subexp(rhs, tmp);

            if (IS_LITERAL(rhs)) {
                assert(tmp.empty());
                check_operator(term);
                self->BinaryLiteral(lhs->v, lhs->t, rhs->v, rhs->t, term->token.type);
            } else {
                var_start_idx = i;
                break;
            }
        }
    }

    if (IS_LITERAL(lhs) && var_start_idx == -1) {
        COPY_ROOT_NODE(lhs)
        return;
    }

    ir::Operand acc;
    if (var_start_idx == -1) {
        acc = ir::Operand(lhs->v, lhs->t);
        var_start_idx = 2;
    } else {
        acc = self->Literal2Var(lhs->t, lhs->v, pgm);
    }

    if ((acc.type == ir::Type::Int || acc.type == ir::Type::Float) && acc.name.find('_') != std::string::npos) {
        auto tmp = Operand("t" + std::to_string(self->tmp_cnt++) , acc.type);
        ir::Operator mv = acc.type == ir::Type::Int ? ir::Operator::mov : ir::Operator::fmov;
        pgm.push_back(new ir::Instruction(acc, {}, tmp, mv));
        acc = tmp;
    }

    for (int i = var_start_idx; i < int(root->children.size()); i += 2) {
        auto term = dynamic_cast<frontend::Term*>(root->children[i - 1]);
        auto rhs = dynamic_cast<SubExpT*>(root->children[i]);
        analyze_subexp(rhs, pgm);
        check_operator(term);
        auto roperand = (rhs->t == Type::IntLiteral || rhs->t == Type::FloatLiteral) ? self->Literal2Var(rhs->t, rhs->v, pgm) : Operand(rhs->v, rhs->t);
        self->BinaryVar(acc, roperand, term->token.type, pgm);
    }

    SET_ROOT(acc)
}

/// @brief 分析等号表达式 EqExp -> RelExp (==|!= RelExp)*
/// @param root 当前表达式节点
/// @param pgm 当前基本块指令列表
void Analyzer::analysisEqExp(EqExp* root, std::vector<ir::Instruction*>& pgm) {
    analyzeBinary<EqExp, RelExp>(
        this, root, pgm,
        [](Term* term) {
            assert(term->token.type == TokenType::EQL || term->token.type == TokenType::NEQ);
        },
        [this](RelExp* sub, std::vector<ir::Instruction*>& pgm) {
            this->analysisRelExp(sub, pgm);
        }
    );
}

/// @brief 分析关系表达式 RelExp -> AddExp (<|<=|>|>= AddExp)*
/// @param root 当前表达式节点
/// @param pgm 当前基本块指令列表
void Analyzer::analysisRelExp(RelExp* root, std::vector<ir::Instruction*>& pgm) {
    analyzeBinary<RelExp, AddExp>(
        this, root, pgm,
        [](Term* term) {
            assert(term->token.type == TokenType::LSS || term->token.type == TokenType::LEQ ||
                   term->token.type == TokenType::GTR || term->token.type == TokenType::GEQ);
        },
        [this](AddExp* sub, std::vector<ir::Instruction*>& pgm) {
            this->analysisAddExp(sub, pgm);
        }
    );
}

/// @brief 分析加法表达式 AddExp -> MulExp (+|− MulExp)*
///        暂不对非前缀常量合并做优化，避免潜在溢出
/// @param root 当前表达式节点
/// @param pgm 当前基本块指令列表
void Analyzer::analysisAddExp(AddExp* root, std::vector<ir::Instruction*>& pgm) {
    analyzeBinary<AddExp, MulExp>(
        this, root, pgm,
        [](Term* term) {
            assert(term->token.type == TokenType::PLUS || term->token.type == TokenType::MINU);
        },
        [this](MulExp* sub, std::vector<ir::Instruction*>& pgm) {
            this->analysisMulExp(sub, pgm);
        }
    );
}

/// @brief 分析乘除表达式 MulExp -> UnaryExp (*|/|% UnaryExp)*
/// @param root 当前表达式节点
/// @param pgm 当前基本块指令列表
void Analyzer::analysisMulExp(MulExp* root, std::vector<ir::Instruction*>& pgm) {
    analyzeBinary<MulExp, UnaryExp>(
        this, root, pgm,
        [](Term* term) {
            assert(term->token.type == TokenType::MULT || term->token.type == TokenType::DIV ||
                   term->token.type == TokenType::MOD);
        },
        [this](UnaryExp* sub, std::vector<ir::Instruction*>& pgm) {
            this->analysisUnaryExp(sub, pgm);
        }
    );
}


/// @brief 分析一元表达式 UnaryExp
void Analyzer::analysisUnaryExp(UnaryExp* root, std::vector<ir::Instruction*>& pgm) {
    // 处理 PrimaryExp 直接封装的情形
    if (CUR_NODE_IS(PRIMARYEXP)) {
        GET_CHILD_PTR(primary, PrimaryExp, 0)
        analysisPrimaryExp(primary, pgm);
        COPY_ROOT_NODE(primary)
        return;
    }

    // 处理函数调用形式：UnaryExp -> Identifier (FuncRParams?)
    if (CUR_NODE_IS(TERMINAL)) {
        GET_CHILD_PTR(idf, Term, 0)
        auto name = idf->token.value;
        auto func = symbol_table.functions[name];
        std::vector<Operand> args;

        if (root->children.size() > 3) {
            GET_CHILD_PTR(params, FuncRParams, 2)
            analysisFuncRParams(params, func->ParameterList, args, pgm);
        }

        if (func->returnType == Type::null) {
            pgm.push_back(new ir::CallInst(name, args, {}));
            root->t = Type::null;
        } else {
            CREATE_OPERAND(res, func->returnType)
            pgm.push_back(new ir::CallInst(name, args, res));
            SET_ROOT(res)
        }
        return;
    }

    // 处理一元操作符形式：UnaryExp -> UnaryOp UnaryExp
    GET_CHILD_PTR(op_node, UnaryOp, 0)
    GET_CHILD_PTR(operand_exp, UnaryExp, 1)
    analysisUnaryOp(op_node);
    analysisUnaryExp(operand_exp, pgm);

    // 一元正号，直接复制
    if (op_node->op == TokenType::PLUS) {
        COPY_ROOT_NODE(operand_exp)
        return;
    }

    // 可计算常量直接折叠
    if (IS_LITERAL(operand_exp)) {
        float val = (operand_exp->t == Type::IntLiteral) ? stoi(operand_exp->v) : stof(operand_exp->v);
        if (op_node->op == TokenType::MINU)
            val = -val;
        else
            val = !val;
        operand_exp->v = std::to_string(val);
        COPY_ROOT_NODE(operand_exp)
        return;
    }

    // 变量运算需要生成 IR 指令
    EXPR_TO_OPERAND(operand_exp, op)

    if (operand_exp->t == Type::Int) {
        CREATE_OPERAND(tmp, Type::Int)
        Operator opr = (op_node->op == TokenType::MINU) ? Operator::sub : Operator::eq;
        pgm.push_back(new Instruction(Operand("0", Type::IntLiteral), op, tmp, opr));
        SET_ROOT(tmp)
    } else {
        CREATE_OPERAND(tmp, Type::Float)
        Operator opr = (op_node->op == TokenType::MINU) ? Operator::fsub : Operator::feq;
        pgm.push_back(new Instruction(Operand("0.0", Type::FloatLiteral), op, tmp, opr));
        SET_ROOT(tmp)
    }
}

/// @brief 分析 PrimaryExp：括号表达式、变量、字面量
void Analyzer::analysisPrimaryExp(PrimaryExp* root, std::vector<ir::Instruction*>& pgm) {
    if (CUR_NODE_IS(TERMINAL)) {
        GET_CHILD_PTR(exp, Exp, 1)
        analysisExp(exp, pgm);
        COPY_ROOT_NODE(exp)
        return;
    }

    if (CUR_NODE_IS(LVAL)) {
        GET_CHILD_PTR(lval, LVal, 0)
        analysisLVal(lval, pgm);

        if (lval->t == Type::IntPtr || lval->t == Type::FloatPtr) {
            Operand base = Operand(lval->v, lval->t);
            Operand offset = Operand(lval->offset, Type::Int);

            if (lval->ptr) {
                CREATE_OPERAND(tmp, lval->t)
                ADD_INS(base, offset, tmp, getptr)
                SET_ROOT(tmp)
            } else {
                Type resultType = (lval->t == Type::IntPtr) ? Type::Int : Type::Float;
                CREATE_OPERAND(tmp, resultType)
                ADD_INS(base, offset, tmp, load)
                SET_ROOT(tmp)
            }
        } else {
            COPY_ROOT_NODE(lval)
        }
        return;
    }

    // Number
    GET_CHILD_PTR(num, Number, 0)
    analysisNumber(num, pgm);
    COPY_ROOT_NODE(num)
}
/// @brief 分析实参列表，处理类型转换和记录参数
void Analyzer::analysisFuncRParams(FuncRParams* root,
                                   std::vector<ir::Operand>& params,
                                   std::vector<ir::Operand>& args,
                                   std::vector<ir::Instruction*>& pgm) {
    assert(params.size() == (root->children.size() + 1) / 2);

    for (int idx = 0, cnt = 0; idx < int(root->children.size()); idx += 2, ++cnt) {
        GET_CHILD_PTR(exp, Exp, idx)
        analysisExp(exp, pgm);

        Operand arg(exp->v, exp->t);
        const Operand& param = params[cnt];

        switch (param.type) {
            case Type::Int:
                if (arg.type == Type::Float) {
                    CREATE_OPERAND(tmp, Type::Int)
                    ADD_INS(arg, {}, tmp, cvt_f2i)
                    arg = tmp;
                } else if (arg.type == Type::FloatLiteral) {
                    arg.name = std::to_string(int(stof(arg.name)));
                    arg.type = Type::IntLiteral;
                }
                break;

            case Type::Float:
                if (arg.type == Type::Int) {
                    CREATE_OPERAND(tmp, Type::Float)
                    ADD_INS(arg, {}, tmp, cvt_i2f)
                    arg = tmp;
                } else if (arg.type == Type::IntLiteral) {
                    arg.name = std::to_string(float(stoi(arg.name)));
                    arg.type = Type::FloatLiteral;
                }
                break;

            case Type::IntPtr:
            case Type::FloatPtr:
                assert(arg.type == param.type);
                break;

            default:
                ASSERT(Invalid param)
        }

        args.push_back(arg);
    }
}

/// @brief 解析一元运算符，支持 + - !
void Analyzer::analysisUnaryOp(UnaryOp* root) {
    GET_CHILD_PTR(term, Term, 0)
    auto type = term->token.type;
    assert(type == TokenType::PLUS || type == TokenType::MINU || type == TokenType::NOT);
    root->op = type;
}

/// @brief 解析左值 LVal，支持变量、数组（含维度计算）
/// 常量变量在编译期直接替换，数组按维度计算偏移量
void Analyzer::analysisLVal(LVal* root, std::vector<ir::Instruction*> &pgm) {
    GET_CHILD_PTR(idf, Term, 0)
    auto ste = symbol_table.get_ste(idf->token.value);  // 符号表项
    auto opd = ste.operand;
    
    // 如果是常量变量（非数组），直接在编译期替换为字面量
    if (ste.isConst && (opd.type == Type::Int || opd.type == Type::Float)) {
        root->v = ste.val;
        root->t = (opd.type == Type::Int) ? Type::IntLiteral : Type::FloatLiteral;
        root->ptr = false;
        return;
    }

    // 否则按变量或数组处理
    SET_ROOT(opd)
    auto &dim = ste.dimension;
    if (!dim.empty()) {
        Operand offset = Literal2Var(Type::IntLiteral, "0", pgm);
        int cur = 0, size = ste.size;

        for (int idx = 2; idx < int(root->children.size()); idx += 3) {
            size /= dim[cur++];
            Operand scale = Literal2Var(Type::IntLiteral, std::to_string(size), pgm);

            GET_CHILD_PTR(exp, Exp, idx)
            analysisExp(exp, pgm);
            assert(exp->t != Type::Float && exp->t != Type::FloatLiteral);

            EXPR_TO_OPERAND(exp, expvar)
            ADD_INS(scale, expvar, scale, mul)
            ADD_INS(offset, scale, offset, add)
        }

        root->offset = offset.name;
        root->ptr = cur < int(dim.size());  // 是否是数组指针
    } else {
        root->ptr = false;
    }
}

/// @brief 解析字面量 Number，支持十进制、十六进制、八进制、二进制整数与浮点数
void Analyzer::analysisNumber(Number* root, std::vector<ir::Instruction*>&) {
    GET_CHILD_PTR(term, Term, 0)
    const std::string& num = term->token.value;

    if (term->token.type == TokenType::INTLTR) {
        if (num.size() >= 3 && num[0] == '0' && tolower(num[1]) == 'x')
            root->v = std::to_string(stoi(num, nullptr, 16));  // Hex
        else if (num.size() >= 3 && num[0] == '0' && tolower(num[1]) == 'b')
            root->v = std::to_string(stoi(num.substr(2), nullptr, 2));  // Binary
        else if (num.size() >= 2 && num[0] == '0')
            root->v = std::to_string(stoi(num, nullptr, 8));  // Octal
        else
            root->v = num;  // Decimal

        root->t = Type::IntLiteral;
    }
    else if (term->token.type == TokenType::FLOATLTR) {
        root->v = num;
        root->t = Type::FloatLiteral;
    }
    else {
        ASSERT(Invalid Number)
    }
}
/// @brief 立即数转变量
Operand Analyzer::Literal2Var(ir::Type typ, const std::string& val, std::vector<ir::Instruction*>& pgm) {
    Type target_type;
    Operator op;
    
    if (typ == Type::IntLiteral) {
        target_type = Type::Int;
        op = Operator::def;
    } else if (typ == Type::FloatLiteral) {
        target_type = Type::Float;
        op = Operator::fdef;
    } else {
        ASSERT(Not literal)
    }

    Operand result("t" + std::to_string(tmp_cnt++), target_type);
    Operand literal(val, typ);
    pgm.push_back(new ir::Instruction(literal, {}, result, op));
    return result;
}
/// @brief 二元计算模板
std::string Analyzer::calculateBinary(TokenType tk_type, float lhs, float rhs) {
    switch (tk_type) {
        case TokenType::EQL:  return std::to_string(lhs == rhs);
        case TokenType::NEQ:  return std::to_string(lhs != rhs);
        case TokenType::LSS:  return std::to_string(lhs < rhs);
        case TokenType::LEQ:  return std::to_string(lhs <= rhs);
        case TokenType::GTR:  return std::to_string(lhs > rhs);
        case TokenType::GEQ:  return std::to_string(lhs >= rhs);
        case TokenType::PLUS: return std::to_string(lhs + rhs);
        case TokenType::MINU: return std::to_string(lhs - rhs);
        case TokenType::MULT: return std::to_string(lhs * rhs);
        case TokenType::DIV:  return std::to_string(lhs / rhs);
        default: ASSERT(Invalid binary operator)
    }
}

/// @brief 二元计算
void Analyzer::BinaryLiteral(std::string& v1, ir::Type& t1, std::string& v2, ir::Type& t2, TokenType tk_type) {
    if (tk_type == TokenType::MOD) {
        // 模运算只能用于整型字面量
        assert(t1 == Type::IntLiteral && t2 == Type::IntLiteral);
        v1 = std::to_string(std::stoi(v1) % std::stoi(v2));
        return;
    }

    float lhs = (t1 == Type::IntLiteral) ? std::stoi(v1) : std::stof(v1);
    float rhs = (t2 == Type::IntLiteral) ? std::stoi(v2) : std::stof(v2);

    v1 = calculateBinary(tk_type, lhs, rhs);

    // 若有一个为浮点，结果就是浮点
    if (t1 == Type::FloatLiteral || t2 == Type::FloatLiteral)
        t1 = Type::FloatLiteral;
    else
        t1 = Type::IntLiteral;
}
/// @brief 生成两个变量参与的二元运算的中间表示（IR）
void Analyzer::BinaryVar(Operand& op1, Operand& op2, TokenType tk_type, std::vector<ir::Instruction*>& pgm) {
    static const std::unordered_map<TokenType, std::pair<Operator, Operator>> op_map = {
        {TokenType::EQL,  {Operator::eq,   Operator::feq}},
        {TokenType::NEQ,  {Operator::neq,  Operator::fneq}},
        {TokenType::LSS,  {Operator::lss,  Operator::flss}},
        {TokenType::LEQ,  {Operator::leq,  Operator::fleq}},
        {TokenType::GTR,  {Operator::gtr,  Operator::fgtr}},
        {TokenType::GEQ,  {Operator::geq,  Operator::fgeq}},
        {TokenType::PLUS, {Operator::add,  Operator::fadd}},
        {TokenType::MINU, {Operator::sub,  Operator::fsub}},
        {TokenType::MULT, {Operator::mul,  Operator::fmul}},
        {TokenType::DIV,  {Operator::div,  Operator::fdiv}},
        {TokenType::MOD,  {Operator::mod,  Operator::mod}} // only for int
    };

    auto it = op_map.find(tk_type);
    assert(it != op_map.end());
    auto pair = it->second;
    Operator cal = pair.first;
    Operator fcal = pair.second;

    if (tk_type == TokenType::MOD) {
        assert(op1.type == Type::Int && op2.type == Type::Int);
        pgm.push_back(new Instruction(op1, op2, op1, cal));
        return;
    }

    auto ensureFloat = [&](Operand& opd) {
        if (opd.type == Type::Int) {
            CREATE_OPERAND(tmp, Type::Float)
            ADD_INS(opd, {}, tmp, cvt_i2f)
            return tmp;
        }
        return opd;
    };

    if (op1.type == Type::Int && op2.type == Type::Int) {
        pgm.push_back(new Instruction(op1, op2, op1, cal));
    } else {
        Operand lhs = ensureFloat(op1);
        Operand rhs = ensureFloat(op2);
        pgm.push_back(new Instruction(lhs, rhs, lhs, fcal));
        op1 = lhs; // 更新op1为结果
    }
}
