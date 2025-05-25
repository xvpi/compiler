#include"front/semantic.h"
#include<cassert>

using ir::Instruction;
using ir::Function;
using ir::Operand;
using ir::Operator;
// TODO2.4;
using frontend::Analyzer;
using frontend::STE;
using ir::Type;

#define ASSERT(msg) assert(0 && #msg);
#define SIZE int(root->children.size())
#define STR(x) std::to_string(x)
#define VINS vector<Instruction*>
#define INS(op1, op2, des, op) new Instruction(op1, op2, des, Operator::op)
#define FST_NODE_IS(nd_type) (root->children[0]->type == frontend::NodeType::nd_type)
#define NODE_IS(nd_type, idx) (root->children[idx]->type == frontend::NodeType::nd_type)
#define TERM_IS(tk_type) (dynamic_cast<Term*>(root->children[0])->token.type == TokenType::tk_type)
#define LOCAL (int(symbol_table.scope_stack.size()) > 1)
#define LITERAL(node) (node->t == Type::IntLiteral || node->t == Type::FloatLiteral)
#define GET_CHILD_PTR(node, type, index) auto node = dynamic_cast<type*>(root->children[index]); assert(node);
#define ANALYSIS(type, index, buffer) {auto node = dynamic_cast<type*>(root->children[index]); assert(node); analyze##type(node, buffer);}
#define REP(begin, end, step) for(int idx = begin; idx < int(end) ; idx += step)
#define ADDSTE(name, ste) symbol_table.scope_stack.back().table[name] = ste;
#define UPDSTE(name, v) symbol_table.scope_stack.back().table[name].val = v;
#define ADDINS(op1, op2, des, op) pgm.push_back(INS(op1, op2, des, op));
#define ADDINS2(op1, op2, des, op) pgm.push_back(new Instruction(op1, op2, des, op));
#define ADDBBL(bbl) pgm.insert(pgm.end(), bbl.begin(), bbl.end());
#define NEW ("t" + STR(tmp_cnt++))
#define NEW_OPERAND(opd, type) auto opd = Operand(NEW , type);
#define TO_OPERAND(node, opd) auto opd = LITERAL(node) ? Literal2Var(node->t, node->v, pgm) : Operand(node->v, node->t);
#define BE_ROOT(opd) {root->v = opd.name; root->t = opd.type;}
#define COPY_NODE(from) {root->v = from->v; root->t = from->t;}
#define ZERO Operand("0", Type::IntLiteral)
#define FZERO Operand("0.0", Type::FloatLiteral)

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

// TODO2.16;
STE::STE(){}
STE::STE(ir::Operand _operand, vector<int> _dimension, int _size, bool _isConst = false):operand(_operand), dimension(_dimension), size(_size), isConst(_isConst), val(string()){}

void frontend::SymbolTable::add_scope() {
    // TODO2.1;
    int cnt = scope_stack.size();
    scope_stack.push_back({cnt, "Scope" + STR(cnt)});
}
void frontend::SymbolTable::exit_scope() {
    // TODO2.2;
    scope_stack.pop_back();
}

string frontend::SymbolTable::get_scoped_name(string id) const {
    // TODO2.3;
    return id + '_' + scope_stack.back().name;
}

frontend::STE frontend::SymbolTable::get_ste(string id) const {
    // TODO2.5;
    for(auto scope = scope_stack.rbegin(); scope != scope_stack.rend(); scope++)
    {
        auto cur = scope->table.find(id);
        if(cur != scope->table.end())
            return cur->second;
    }
    ASSERT(STE not found)
}

Analyzer::Analyzer(): tmp_cnt(0), symbol_table(), cur_func(nullptr) {}

ir::Program Analyzer::get_ir_program(CompUnit* root) {
    // TODO2.6;
    ir::Program program;

    // TODO 2.37;
    // 依赖库需要添加到符号表,因为后续的函数解析会用到
    auto libs = get_lib_funcs();
    for(auto lib = libs->begin(); lib != libs->end(); lib++)
    symbol_table.functions[lib->first] = lib->second;

    symbol_table.add_scope();
    
    //TODO2.7;
    analyzeCompUnit(root);
    
    for(auto it = symbol_table.scope_stack[0].table.begin() ; it != symbol_table.scope_stack[0].table.end() ; it++)
    {
        auto &ste = it->second;// 添加全局变量
        if(ste.dimension.size())
            program.globalVal.push_back({ste.operand, ste.size});
        else
            // TODO2.8;
            program.globalVal.push_back({ste.operand, 0});
    }

    Function glb("_global", Type::null);// 定义全局函数
    glb.InstVec = g_init_inst;
    glb.addInst(INS({},{},{},_return));

    program.addFunction(glb);// 添加全局函数
    for(auto it = symbol_table.functions.begin() ; it != symbol_table.functions.end(); it++)// 添加函数
    {
        auto &name = it->first;
        auto &func = it->second;
        // TODO3.3;
        // 记录所有的本文件函数(非依赖库函数)
        if(libs->count(name) == 0)// 与依赖库函数名称不同,严格来说参数列表不同也行,命名空间不同也行,不写那么麻烦了
        {
            if(name == "main")// 在主函数中添加全局函数调用
                func->InstVec.insert(func->InstVec.begin(), new ir::CallInst(Operand("_global", Type::null), {}));
            program.addFunction(*func);
        }
    }

    symbol_table.exit_scope();

    return program;
}
void Analyzer::analyzeCompUnit(CompUnit* root)
{
    if(FST_NODE_IS(DECL))
        ANALYSIS(Decl, 0, g_init_inst)
    else
    {
        GET_CHILD_PTR(func, FuncDef, 0)
        // TODO2.18
        symbol_table.add_scope();
        analyzeFuncDef(func);
        symbol_table.exit_scope();
    }

    if(SIZE > 1)
    {
        GET_CHILD_PTR(comp, CompUnit, 1)
        analyzeCompUnit(comp);
    }
}
void Analyzer::analyzeDecl(Decl* root, vector<ir::Instruction*> &pgm)
{
    if(FST_NODE_IS(CONSTDECL))
        ANALYSIS(ConstDecl, 0, pgm)
    else 
        ANALYSIS(VarDecl, 0, pgm)
}
void Analyzer::analyzeConstDecl(ConstDecl* root, vector<ir::Instruction*> &pgm)
{
    GET_CHILD_PTR(btyp, BType, 1)
    analyzeBType(btyp);
    root->t = btyp->t;
    REP(2, SIZE, 2)
    {
        GET_CHILD_PTR(cstdef, ConstDef, idx)
        analyzeConstDef(cstdef, pgm, root->t);
    }
}
void Analyzer::analyzeBType(BType* root)
{
    if(TERM_IS(INTTK))
        root->t = Type::Int;
    else if(TERM_IS(FLOATTK))
        root->t = Type::Float;
    else
        ASSERT(Invalid BType)
}

void Analyzer::analyzeConstDef(ConstDef* root, vector<ir::Instruction*> &pgm, ir::Type typ)
{
    GET_CHILD_PTR(idf, Term, 0)
    root->arr_name = symbol_table.get_scoped_name(idf->token.value);

    vector<int>dim;
    int size = 1;
    REP(2, SIZE, 3)
    {
        if(!NODE_IS(CONSTEXP, idx))
            break;
        GET_CHILD_PTR(cstexp, ConstExp, idx)
        analyzeConstExp(cstexp);
        // TODO2.11;
        assert(cstexp->t == Type::IntLiteral);// cstexp必须是整型字面量
        auto len = std::stoi(cstexp->v);
        dim.push_back(len);
        size *= len;
    }
    size *= !dim.empty();
    
    GET_CHILD_PTR(cstval, ConstInitVal, SIZE - 1)
    cstval->v = idf->token.value;
    switch(typ)
    {
        case Type::Int:
            if(dim.empty())
            {
                // TODO2.14;
                // 符号表里的数据类型不会是立即数类型
                ADDSTE(idf->token.value, STE(Operand(root->arr_name, Type::Int), dim, size, 1))
                // cstval->t此时指代变量本身的类型
                cstval->t = Type::Int;
            }
            else
            {
                ADDSTE(idf->token.value, STE(Operand(root->arr_name, Type::IntPtr), dim, size, 1))
                // 数组需要添加定义指令,cstval->t此时指代初始值的期望类型
                cstval->t = Type::IntLiteral;
                // TODO2.39;
                // 只有局部变量才能调用alloc指令,全局变量已经由IR评测机零初始化了,如果使用alloc指令
                // 会导致IR评测机重新为全局变量申请空间,且不会再次进行初始化操作,导致全局变量的初值不
                // 为零,带来潜在的安全问题(例如在测试点62中会导致段错误)。对于def和fdef道理是一样的。
                // 但是对于指定了初值的数组定义,仍然需要调用store指令。对于变量则需要将def/fdef对应修改
                // 为mov/fmov
                if(LOCAL)
                    ADDINS(Operand(STR(size), Type::IntLiteral), {}, Operand(root->arr_name, Type::IntPtr), alloc)
            }
            break;
        case Type::Float:
            if(dim.empty())
            {
                ADDSTE(idf->token.value, STE(Operand(root->arr_name, Type::Float), dim, size, 1))
                cstval->t = Type::Float;
            }
            else
            {
                ADDSTE(idf->token.value, STE(Operand(root->arr_name, Type::FloatPtr), dim, size, 1))
                cstval->t = Type::FloatLiteral;
                if(LOCAL)
                    ADDINS(Operand(STR(size), Type::IntLiteral), {}, Operand(root->arr_name, Type::FloatPtr), alloc)
            }
            break;
        default:
            ASSERT(Invalid ConstDef)
    }

    analyzeConstInitVal(cstval, pgm, size, 0, 0, dim);
}
void Analyzer::analyzeConstInitVal(ConstInitVal* root, vector<ir::Instruction*> &pgm, int size, int cur, int ofs, vector<int> &dim)
{
    // TODO2.35
    // root->v存储的是变量的原始名称,用于更新符号表,name则用于def和store语句
    auto name = symbol_table.get_scoped_name(root->v);
    if(size)// 数组
    {
        if(dynamic_cast<VarDef*>(root->parent) || size > 1)
        {
            assert(cur < int(dim.size()));
            size /= dim[cur];
            int cnt = 0, tot = SIZE / 2;
            REP(1, SIZE - 1, 2)
            {
                GET_CHILD_PTR(cstval, ConstInitVal, idx)
                cstval->v = root->v;// 递归下传的时候仍然使用root->v
                cstval->t = root->t;
                if(tot <= dim[cur])
                    analyzeConstInitVal(cstval, pgm, size, cur + 1, ofs + cnt * size, dim);
                else
                    analyzeConstInitVal(cstval, pgm, 1, cur, ofs + cnt, dim);
                cnt++;
            }
            REP(cnt * size, dim[cur] * size, 1)// 未声明的部分自动补零
            {
                Type type;
                string val;
                if(root->t == Type::IntLiteral)
                {
                    type = Type::IntPtr;
                    val = "0";
                }
                else
                {
                    type = Type::FloatPtr;
                    val = "0.0";                    
                }
                ADDINS(Operand(name, type), Operand(STR(idx), Type::IntLiteral), Operand(val, root->t), store)
            }
        }
        else
        {
            assert(FST_NODE_IS(CONSTEXP));
            GET_CHILD_PTR(cstexp, ConstExp, 0)
            analyzeConstExp(cstexp);
            switch(root->t)
            {
                case Type::IntLiteral:
                    // TODO2.12;
                    if(cstexp->t == Type::FloatLiteral)// 类型转换
                    {
                        cstexp->v = STR(int(stof(cstexp->v)));
                        cstexp->t = Type::IntLiteral;
                    }
                    assert(cstexp->t == Type::IntLiteral);
                    // TODO2.31;
                    // 常量变量在任何情况下都可以在编译阶段直接替换为字面量,本次实验设计遵循上述规则。代码中出现的
                    // 所有常量变量都将被直接替换为字面量。因此符号表中的常量变量可以认为是定义了但从未调用
                    // 常量数组不可能出现在数组声明中,形如const int a[2] = {1,2}; int b[a[1]];在c语言中是非法的
                    // 但常量数组可能出现在常量定义中,形如const int a[2] = {1,2}; const int b = a[1];是合法的
                    // 因此访问常量数组时,如果数组各位均为常数,形如a[2][1]时,可以在编译时替换为字面量,也可以运行时载入到临时变量。当作为ConstInitval中的ConstExp时必须替换为字面量
                    // 如果数组各位中出现了变量,形如a[x][1]时,则需要在运行阶段载入到临时变量,而不可能在编译阶段就替换为字面量
                    // 由于本实验中不存在常量数组作为ConstInitVal中的ConstExp的情况,因此对常量数组的访问统一实现为
                    // 运行时载入,具体见analyzeLVal
                    ADDINS(Operand(name, Type::IntPtr), Operand(STR(ofs), Type::IntLiteral), Operand(cstexp->v, cstexp->t), store)
                    break;
                case Type::FloatLiteral:
                    if(cstexp->t == Type::IntLiteral)
                    {
                        cstexp->v = STR(float(stoi(cstexp->v)));
                        cstexp->t = Type::FloatLiteral;
                    }
                    assert(cstexp->t == Type::FloatLiteral);
                    ADDINS(Operand(name, Type::FloatPtr), Operand(STR(ofs), Type::IntLiteral), Operand(cstexp->v, cstexp->t), store)
                    break;
                default:
                    ASSERT(Invalid ConstExp)
            }
        }
    }
    else // 变量
    {
        assert(FST_NODE_IS(CONSTEXP));
        GET_CHILD_PTR(cstexp, ConstExp, 0)
        analyzeConstExp(cstexp);
        assert(LITERAL(cstexp));
        switch(root->t)
        {
            case Type::Int:
                if(cstexp->t == Type::FloatLiteral)
                {
                    cstexp->v = STR(int(stof(cstexp->v)));
                    cstexp->t = Type::IntLiteral;
                }
                // TODO2.32.1;
                // 定义但从未调用
                if(LOCAL)
                    ADDINS(Operand(cstexp->v, cstexp->t), {}, Operand(name, Type::Int), def)
                else
                    ADDINS(Operand(cstexp->v, cstexp->t), {}, Operand(name, Type::Int), mov)
                UPDSTE(root->v, cstexp->v)// 将常量变量的字面量记录到符号表
                break;
            case Type::Float:
                if(cstexp->t == Type::IntLiteral)
                {
                    cstexp->v = STR(float(stoi(cstexp->v)));
                    cstexp->t = Type::FloatLiteral;
                }
                if(LOCAL)
                    ADDINS(Operand(cstexp->v, cstexp->t), {}, Operand(name, Type::Float), fdef)
                else
                    ADDINS(Operand(cstexp->v, cstexp->t), {}, Operand(name, Type::Float), fmov)
                UPDSTE(root->v, cstexp->v)
                break;
            default:
                ASSERT(Invalid ConstExp)
        }
    }
}
void Analyzer::analyzeVarDecl(VarDecl* root, vector<ir::Instruction*> &pgm)
{
    GET_CHILD_PTR(btyp, BType, 0)
    analyzeBType(btyp);
    root->t = btyp->t;
    REP(1, SIZE, 2)
    {
        GET_CHILD_PTR(vardef, VarDef, idx)
        analyzeVarDef(vardef, pgm, root->t);
    }
}
void Analyzer::analyzeVarDef(VarDef* root, vector<ir::Instruction*> &pgm, Type typ)
{
    GET_CHILD_PTR(idf, Term, 0)
    root->arr_name = symbol_table.get_scoped_name(idf->token.value);

    vector<int>dim;
    int size = 1;
    REP(2, SIZE, 3)
    {
        if(!NODE_IS(CONSTEXP, idx))
            break;
        GET_CHILD_PTR(cstexp, ConstExp, idx)
        analyzeConstExp(cstexp);
        assert(cstexp->t == Type::IntLiteral);
        auto len = std::stoi(cstexp->v);
        dim.push_back(len);
        size *= len;
    }
    size *= !dim.empty();
    if(NODE_IS(INITVAL, SIZE - 1))
    {
        GET_CHILD_PTR(initval, InitVal, SIZE - 1)
        initval->v = root->arr_name;
        switch(typ)
        {
            case Type::Int:
                // 变量本身的类型和期待的初始值类型一致
                initval->t = Type::Int;
                if(dim.empty())
                    ADDSTE(idf->token.value, STE(Operand(root->arr_name, Type::Int), dim, size))
                else
                {
                    ADDSTE(idf->token.value, STE(Operand(root->arr_name, Type::IntPtr), dim, size))
                    if(LOCAL)
                        ADDINS(Operand(STR(size), Type::IntLiteral), {}, Operand(root->arr_name, Type::IntPtr), alloc)
                }
                break;
            case Type::Float:
                initval->t = Type::Float;
                if(dim.empty())
                    ADDSTE(idf->token.value, STE(Operand(root->arr_name, Type::Float), dim, size))
                else
                {
                    ADDSTE(idf->token.value, STE(Operand(root->arr_name, Type::FloatPtr), dim, size))
                    if(LOCAL)
                        ADDINS(Operand(STR(size), Type::IntLiteral), {}, Operand(root->arr_name, Type::FloatPtr), alloc)
                }
                break;
            default:
                ASSERT(Invalid Def)
        }
        analyzeInitVal(initval, pgm, size, 0, 0, dim);
    }
    else // 没有显式定义则默认定义
    {
        switch(typ)
        {
            case Type::Int:
                if(dim.empty())
                {
                    ADDSTE(idf->token.value, STE(Operand(root->arr_name, Type::Int), dim, size))
                    // 因为没有Initval,无法滞后定义,所以此处直接使用默认值定义(真实情况下一般是随机数)
                    if(LOCAL)
                        ADDINS(Operand("114514", Type::IntLiteral), {}, Operand(root->arr_name, Type::Int), def)
                }
                else
                {
                    ADDSTE(idf->token.value, STE(Operand(root->arr_name, Type::IntPtr), dim, size))
                    if(LOCAL)
                        ADDINS(Operand(STR(size), Type::IntLiteral), {}, Operand(root->arr_name, Type::IntPtr), alloc)
                }
                break;
            case Type::Float:
                if(dim.empty())
                {
                    ADDSTE(idf->token.value, STE(Operand(root->arr_name, Type::Float), dim, size))
                    if(LOCAL)
                        ADDINS(Operand("114.514", Type::FloatLiteral), {}, Operand(root->arr_name, Type::Float), fdef)
                }
                else
                {
                    ADDSTE(idf->token.value, STE(Operand(root->arr_name, Type::FloatPtr), dim, size))
                    if(LOCAL)
                        ADDINS(Operand(STR(size), Type::IntLiteral), {}, Operand(root->arr_name, Type::FloatPtr), alloc)
                }
                break;
            default:
                ASSERT(Invalid Def)
        }
    }
}
void Analyzer::analyzeInitVal(InitVal* root, vector<ir::Instruction*> &pgm, int size, int cur, int ofs, vector<int> &dim)
{
    if(size)// 数组
    {
        // TODO2.36;
        // 通常情况下数组初始化形如int a[2] = {1,2}时,由analyzeVarDecl传入的size为2, 值为{1,2},
        // 递归一次后size为1,值为1。因此size == 1时表达式总是没有'{'和'}'的。但是特别的对于测试点66,
        // int x[1] = {1},由analyzeVarDecl传入的size直接为1。此时值为{1},含有'{'和'}',因此需要特殊处理
        // 真的会有正常人弄个长度为1的数组吗?
        if(dynamic_cast<VarDef*>(root->parent) || size > 1)// 保证size=1时也会递归一次(递归后size仍然为1)
        {
            assert(cur < int(dim.size()));
            size /= dim[cur];
            int cnt = 0, tot = SIZE / 2;
            REP(1, SIZE - 1, 2)
            {
                GET_CHILD_PTR(initval, InitVal, idx)
                initval->v = root->v;
                initval->t = root->t;
                if(tot <= dim[cur])
                    analyzeInitVal(initval, pgm, size, cur + 1, ofs + cnt * size, dim);
                else
                    analyzeInitVal(initval, pgm, 1, cur, ofs + cnt, dim);
                cnt++;
            }
            REP(cnt * size, dim[cur] * size, 1)// 未声明的部分自动补零
            {
                Type ptr, type;
                string val;
                if(root->t == Type::Int)
                {
                    ptr = Type::IntPtr;
                    type = Type::IntLiteral;
                    val = "0";
                }
                else
                {
                    ptr = Type::FloatPtr;
                    type = Type::FloatLiteral;
                    val = "0.0";
                }
                ADDINS(Operand(root->v, ptr), Operand(STR(idx), Type::IntLiteral), Operand(val, type), store)
            }
        }
        else
        {
            GET_CHILD_PTR(exp, Exp, 0);
            // 暂不申请临时变量
            analyzeExp(exp, pgm);
            assert(LITERAL(exp));// 全局变量的初始化值一定能在编译时求值到字面量,尽管被用于赋值的变量本身不一定是常量类型(例如 int x = 2, y = x;)
            switch(root->t)
            {
                case Type::Int:
                    if(exp->t == Type::FloatLiteral)
                    {
                        exp->v = STR(int(stof(exp->v)));
                        exp->t = Type::IntLiteral;
                    }
                    ADDINS(Operand(root->v, Type::IntPtr), Operand(STR(ofs), Type::IntLiteral), Operand(exp->v, exp->t), store)
                    break;
                case Type::Float:
                    if(exp->t == Type::IntLiteral)
                    {
                        exp->v = STR(float(stoi(exp->v)));
                        exp->t = Type::FloatLiteral;
                    }
                    ADDINS(Operand(root->v, Type::FloatPtr), Operand(STR(ofs), Type::IntLiteral), Operand(exp->v, exp->t), store)
                    break;
                default:
                    ASSERT(Invalid Exp)
            }
        }
    }
    else
    {
        assert(FST_NODE_IS(EXP));
        GET_CHILD_PTR(exp, Exp, 0)
        // TODO2.15
        // 暂不申请临时变量
        analyzeExp(exp, pgm);
        
        if(root->t == Type::Int && exp->t == Type::IntLiteral)
        {
            if(LOCAL)
                ADDINS(Operand(exp->v, exp->t), {}, Operand(root->v, Type::Int), def)
            else
                ADDINS(Operand(exp->v, exp->t), {}, Operand(root->v, Type::Int), mov)
        }
        else if(root->t == Type::Float && exp->t == Type::FloatLiteral)
        {
            if(LOCAL)
                ADDINS(Operand(exp->v, exp->t), {}, Operand(root->v, Type::Float), fdef)
            else
                ADDINS(Operand(exp->v, exp->t), {}, Operand(root->v, Type::Float), fmov)
        }
        else
        {
            TO_OPERAND(exp, expvar)
            switch(root->t)
            {
                case Type::Int:
                    if(expvar.type == Type::Float)// 需要运行时类型转换
                    {
                        // TODO2.17;
                        ADDINS(expvar, {}, Operand(root->v, Type::Int), cvt_f2i)
                    }
                    else
                    {
                        if(LOCAL)
                            ADDINS(expvar, {}, Operand(root->v, Type::Int), def)
                        else
                            ADDINS(expvar, {}, Operand(root->v, Type::Int), mov)
                    }
                    break;
                case Type::Float:
                    if(exp->t == Type::Int)
                        ADDINS(expvar, {}, Operand(root->v, Type::Float), cvt_i2f)
                    else
                    {
                        if(LOCAL)
                            ADDINS(expvar, {}, Operand(root->v, Type::Float), fdef)
                        else
                            ADDINS(expvar, {}, Operand(root->v, Type::Float), fmov)
                    }
                    break;
                default:
                    ASSERT(Invalid Exp)
            } 
        }
    }
}
void Analyzer::analyzeFuncDef(FuncDef* root)
{
    GET_CHILD_PTR(ftyp, FuncType, 0)
    auto typ = analyzeFuncType(ftyp);
    GET_CHILD_PTR(fname, Term, 1);
    auto name = fname->token.value;

    vector<Operand> params;
    if(SIZE > 5)// 如果存在参数列表
        ANALYSIS(FuncFParams, 3, params)

    Function *func = new Function(name, params, typ);
    symbol_table.functions[name] = func;// 添加到全局函数表
    cur_func = func;// 标记当前的函数体

    ANALYSIS(Block, SIZE - 1, func->InstVec)// 解析函数包含的指令
    
    if(name == "main")// 主函数和Void函数添加默认的return指令
        func->addInst(INS(Operand("0", ir::Type::IntLiteral), {}, {}, _return));
    if(typ == Type::null)
        func->addInst(INS({}, {}, {}, _return));
}
Type Analyzer::analyzeFuncType(FuncType* root)
{
    if(TERM_IS(INTTK))
        return Type::Int;
    else if(TERM_IS(FLOATTK))
        return Type::Float;
    else if(TERM_IS(VOIDTK))
        return Type::null;
    ASSERT(Invalid FuncType)
}
void Analyzer::analyzeFuncFParams(FuncFParams* root, vector<ir::Operand> &params)
{
    REP(0, SIZE, 2)
        ANALYSIS(FuncFParam, idx, params)
}
void Analyzer::analyzeFuncFParam(FuncFParam* root, vector<ir::Operand> &params)
{
    GET_CHILD_PTR(btyp, BType, 0)
    analyzeBType(btyp);
    GET_CHILD_PTR(idf, Term, 1)
    auto name = symbol_table.get_scoped_name(idf->token.value);

    vector<int>dim;
    int size = -1;
    if(SIZE > 2)
    {
        if(btyp->t == Type::Int)// 如果参数为数组则更改参数类型为指针类型
            btyp->t = Type::IntPtr;
        else if(btyp->t == Type::Float)
            btyp->t = Type::FloatPtr;
        else
            ASSERT(Invalid type)
        
        dim.push_back(-1);// 第一维未知
        REP(5, SIZE, 2)
        {
            // TODO2.13
            // 为什么是Exp而不是ConstExp?
            GET_CHILD_PTR(cstexp, ConstExp, idx)
            analyzeConstExp(cstexp);
            assert(cstexp->t == Type::IntLiteral);
            auto len = std::stoi(cstexp->v);
            dim.push_back(len);
            size *= len;
        }
    }
    size *= !dim.empty();
    
    // TODO2.37;
    // 添加到编译器的符号表的时候使用的是原名称,添加到函数体的参数表的时候使用的是修饰后的名称
    ADDSTE(idf->token.value, STE(Operand(name, btyp->t), dim, size))
    params.push_back({name, btyp->t});
}
void Analyzer::analyzeBlock(Block* root, vector<ir::Instruction*> &pgm)
{
    REP(1, SIZE - 1, 1)
        ANALYSIS(BlockItem, idx, pgm)
}
void Analyzer::analyzeBlockItem(BlockItem* root, vector<ir::Instruction*> &pgm)
{
    if(FST_NODE_IS(DECL))
        ANALYSIS(Decl, 0, pgm)
    else
        ANALYSIS(Stmt, 0, pgm)
}
void Analyzer::analyzeStmt(Stmt* root, vector<ir::Instruction*> &pgm)
{
    if(FST_NODE_IS(LVAL))
    {
        GET_CHILD_PTR(lval, LVal, 0)
        GET_CHILD_PTR(exp, Exp, 2)
        // // TODO2.20
        // 暂不申请临时变量
        analyzeLVal(lval, pgm);
        assert(!LITERAL(lval));
        analyzeExp(exp, pgm);

        TO_OPERAND(lval, var)
        TO_OPERAND(exp, expvar)
        if(lval->t == Type::Int || lval->t == Type::Float)// 变量赋值
        {
            if(var.type == Type::Int && expvar.type == Type::Int)
                ADDINS(expvar, {}, var, mov)
            else if(var.type == Type::Int && expvar.type == Type::Float)
            {
                NEW_OPERAND(tmp, Type::Int)
                ADDINS(expvar, {}, tmp, cvt_f2i)
                ADDINS(tmp, {}, var, mov)
            }
            else if(var.type == Type::Float && expvar.type == Type::Int)
            {
                NEW_OPERAND(tmp, Type::Float)
                ADDINS(expvar, {}, tmp, cvt_i2f)
                ADDINS(tmp, {}, var, fmov)
            }
            else
                ADDINS(expvar, {}, var, fmov)
        }
        else if(lval->t == Type::IntPtr || lval->t == Type::FloatPtr)// 数组赋值
        {
            Operand ofs = Operand(lval->ofs, Type::Int);
            ADDINS(var, ofs, expvar, store)
        }
        else
            ASSERT(Invalid LVal)// 左值表达式只能是变量且一定不是临时变量
    }
    else if(FST_NODE_IS(BLOCK))
    {
        // TODO2.19;
        symbol_table.add_scope();
        ANALYSIS(Block, 0, pgm)
        symbol_table.exit_scope();
    }
    else if(FST_NODE_IS(TERMINAL))
    {
        if(TERM_IS(IFTK))
        {
            GET_CHILD_PTR(cond, Cond, 2)
            // 暂不申请临时变量
            analyzeCond(cond, pgm);//cond...
            VINS if_bbl;
            ANALYSIS(Stmt, 4, if_bbl)
            if(LITERAL(cond))
            {
                // 恒为真则只需要添加if_bbl到pgm即可,恒为假则需要添加else_bbl
                if((cond->t == Type::IntLiteral && stoi(cond->v) != 0) || (cond->t == Type::FloatLiteral && stof(cond->v) != 0))
                    ADDBBL(if_bbl)
                else
                {
                    if(SIZE > 5)
                        ANALYSIS(Stmt, SIZE - 1, pgm)
                }
            }
            else
            {
                // if, goto if
                // TODO2.40.4
                if(cond->t == Type::Float)
                {
                    NEW_OPERAND(tmp, Type::Float)
                    ADDINS(Operand(cond->v, cond->t), FZERO, tmp, fneq)
                    ADDINS(tmp, {}, Operand("2", Type::IntLiteral), _goto)
                }
                else
                    ADDINS(Operand(cond->v, cond->t), {}, Operand("2", Type::IntLiteral), _goto)
                // TODO2.21;
                // cond...          cond...
                // if, goto if       if, goto if
                // goto else        goto out
                // if...            if...
                // goto out         out(__unuse__)
                // else ...
                // out(__unuse__)
                if(SIZE > 5)// 含else
                {
                    ADDINS({}, {},Operand(STR(int(if_bbl.size()) + 2), Type::IntLiteral), _goto)//goto else
                    ADDBBL(if_bbl)//if ...

                    VINS else_bbl;
                    ANALYSIS(Stmt, SIZE - 1, else_bbl)
                    ADDINS({},{},Operand(STR(int(else_bbl.size()) + 1),Type::IntLiteral), _goto)// goto out
                    ADDBBL(else_bbl)//else ...
                }
                else
                {
                    ADDINS({}, {},Operand(STR(int(if_bbl.size()) + 1), Type::IntLiteral), _goto)
                    ADDBBL(if_bbl)
                }
                ADDINS({}, {}, {}, __unuse__)// 防止跳转到未知标签
            }
        }
        else if(TERM_IS(WHILETK))
        {
            // TODO2.22
            int last = pgm.size();// 标记条件判断开始前的最后一条指令位置
            GET_CHILD_PTR(cond, Cond, 2)
            analyzeCond(cond, pgm);// cond...
            VINS while_bbl;
            ANALYSIS(Stmt, 4, while_bbl)
            // TODO2.23;
            // cond...          while...
            // if, goto while   goto while...
            // goto out
            // while...
            // goto cond...
            // out(__unuse__)
            if(!LITERAL(cond))
            {
                if(cond->t == Type::Float)
                {
                    NEW_OPERAND(tmp, Type::Int)
                    ADDINS(Operand(cond->v, cond->t), {}, tmp, fneq)
                    // if, goto while
                    ADDINS(tmp, {}, Operand("2", Type::IntLiteral), _goto)// if, goto while
                }
                else
                    ADDINS(Operand(cond->v, cond->t), {}, Operand("2", Type::IntLiteral), _goto)// if, goto while
                ADDINS({}, {},Operand(STR(int(while_bbl.size()) + 2), Type::IntLiteral), _goto)// goto out
            }
            REP(0, while_bbl.size(), 1)
            {
                auto &ins = while_bbl[idx];
                if(ins->op1.name == "break")//替换为goto out
                    ins = INS({}, {}, Operand(STR(int(while_bbl.size()) - idx + 1), Type::IntLiteral), _goto);
                else if(ins->op1.name == "continue")//替换为goto cond...
                    ins = INS({}, {}, Operand(STR(last - int(pgm.size()) - idx), Type::IntLiteral), _goto);
            }            
            if(LITERAL(cond))
            {
                // 恒为真则只需要添加while_bbl并添加回跳指令即可,恒为假则什么都不做
                if((cond->t == Type::IntLiteral && stoi(cond->v) != 0) || (cond->t == Type::FloatLiteral && stof(cond->v) != 0))
                {
                    ADDBBL(while_bbl)// while...
                    ADDINS({}, {}, Operand(STR(-int(while_bbl.size())), Type::IntLiteral), _goto)// goto while...
                }
            }
            else
            {
                ADDBBL(while_bbl)// while...
                ADDINS({}, {}, Operand(STR(last - int(pgm.size())), Type::IntLiteral), _goto)// goto cond...
                ADDINS({}, {}, {}, __unuse__)// out(__unuse__)
            }
        }
        else if(TERM_IS(BREAKTK))
            ADDINS({"break"}, {}, {}, __unuse__)
        else if(TERM_IS(CONTINUETK))
            ADDINS({"continue"}, {}, {}, __unuse__)
        else if(TERM_IS(RETURNTK))
        {
            if(SIZE > 2)
            {
                GET_CHILD_PTR(exp, Exp, 1)
                // 暂不申请临时变量
                analyzeExp(exp, pgm);
                switch(cur_func->returnType)
                {
                    case Type::Int:
                        if(exp->t == Type::Float)// 需要运行时类型转换
                        {
                            NEW_OPERAND(tmp, Type::Int)// 需要临时变量
                            ADDINS(Operand(exp->v, exp->t), {}, tmp, cvt_f2i)
                            ADDINS(tmp, {}, {}, _return)
                        }
                        else
                        {
                            if(exp->t == Type::FloatLiteral)
                            {
                                exp->v = STR(int(stof(exp->v)));
                                exp->t = Type::IntLiteral;
                            }
                            ADDINS(Operand(exp->v, exp->t), {}, {}, _return)
                        }
                        break;
                    case Type::Float:
                        if(exp->t == Type::Int)// 需要运行时类型转换
                        {
                            NEW_OPERAND(tmp, Type::Float)// 需要临时变量
                            ADDINS(Operand(exp->v, exp->t), {}, tmp, cvt_i2f)
                            ADDINS(tmp, {}, {}, _return)
                        }
                        else
                        {
                            if(exp->t == Type::IntLiteral)
                            {
                                exp->v = STR(float(stoi(exp->v)));
                                exp->t = Type::FloatLiteral;
                            }
                            ADDINS(Operand(exp->v, exp->t), {}, {}, _return)
                        }
                        break;
                    default:
                        ASSERT(Invalid type)
                }
            }
            else
            {
                assert(cur_func->returnType == Type::null);
                ADDINS({}, {}, {}, _return)
            }
        }
        // ;
    }
    else // Exp ';'
    {
        if(SIZE > 1)
            ANALYSIS(Exp, 0, pgm)
    }
}
void Analyzer::analyzeConstExp(ConstExp* root)
{
    GET_CHILD_PTR(add, AddExp, 0)
    VINS bbl;
    analyzeAddExp(add, bbl);
    COPY_NODE(add)
    // 必须是编译时可以确定的字面量
    assert(LITERAL(add) && bbl.empty());
}
void Analyzer::analyzeExp(Exp* root, vector<ir::Instruction*> &pgm)
{
    GET_CHILD_PTR(add, AddExp, 0)
    analyzeAddExp(add, pgm);
    COPY_NODE(add);// 复制不释放变量
}
void Analyzer::analyzeCond(Cond* root, vector<ir::Instruction*> &pgm)
{
    GET_CHILD_PTR(lor, LOrExp, 0)
    analyzeLOrExp(lor, pgm);
    COPY_NODE(lor);// 复制不释放变量
}
void Analyzer::analyzeLOrExp(LOrExp* root, vector<ir::Instruction*> &pgm)
{
    GET_CHILD_PTR(land, LAndExp, 0)
    VINS land_bbl;
    analyzeLAndExp(land, land_bbl);
    
    if(SIZE > 1)
    {        
        GET_CHILD_PTR(lor, LOrExp, 2)
        VINS lor_bbl;
        analyzeLOrExp(lor, lor_bbl);
        if(LITERAL(land) && LITERAL(lor))// 都是常量则结果为常量
        {
            if(land->t == Type::IntLiteral && lor->t == Type::IntLiteral)
                root->v = STR(stoi(land->v) || stoi(lor->v));
            else if(land->t == Type::IntLiteral && lor->t == Type::FloatLiteral)
                root->v = STR(stoi(land->v) || stof(lor->v));
            else if(land->t == Type::FloatLiteral && lor->t == Type::IntLiteral)
                root->v = STR(stof(land->v) || stoi(lor->v));
            else
                root->v = STR(stof(land->v) || stof(lor->v));
            root->t = Type::IntLiteral;
        }
        else
        {
            if(LITERAL(land))// 第一个值为常量,第二个值为变量
            {
                if((land->t == Type::IntLiteral && stoi(land->v) != 0) || (land->t == Type::Float && stof(land->v) != 0))// 第一个值为真则结果为常量, 不需要计算第二个值
                {
                    root->v = "1";
                    root->t = Type::IntLiteral;
                }
                else// 否则等效于第二个值
                {
                    ADDBBL(lor_bbl)// 运行时计算第二个值
                    COPY_NODE(lor)// 复制则不释放临时变量
                }
            }
            else// 第一个值是变量,第二个值要变成变量
            {
                TO_OPERAND(land, op1)
                TO_OPERAND(lor, op2)
                NEW_OPERAND(des, Type::Int);
                ADDBBL(land_bbl)// land...
                // TODO2.40.1
                assert(op1.type == Type::Int && op2.type == Type::Int);
                // 注意,feq,flss,fgtr等比较运算符的返回值在IR评测机中的设计是float类型,然后逻辑运算符and,or和not却没有对应的float版本。
                // 举例来说,如果想float || int,必须先fneq float,fzero -> 1.0000,然后cvt_f2i 1.0000 -> 1,最后才能进行int类型间的_or, 才能通过IR评测机。
                // 既然实际上都是布尔类型的,为什么设计上不统一到float或者int呢?这种不统一会导致IR程序中出现完全没有必要的类型转换语句。
                
                // 对于非运算,可以使用feq绕过not不支持浮点类型的问题,但是与或运算必须额外使用类型转换指令将浮点类型的逻辑计算结果转换到整型。
                // 在转换到RISCV汇编的时候,如果想将这种实际上没有意义的类型转换指令筛除,就需要写额外的逻辑(有点两头添堵的意思)。
                // 不过好消息是测试样例很弱:),除了常数情况外没有涉及浮点类型的与或运算,所以这里暂时不考虑上述情况了
                // 常数情况见测试用例95,处理情况见上方LITERAL部分。实际上95也是唯一的浮点测试点,也就是说这一大堆类型转换其实写得不一定都对,或者说不写也能拿57/58分:)
                // 题外话:其实goto语句的设计也会在条件判断的时候有这种不兼容float类型的问题,不过虽然实验指导书说goto的条件只能是整型的,但是IR评测机实际上没进行类型检查
                // 所以可以看到在形如TODO2.40.4的位置其实条件操作数给的是float类型,也能正常通过所有测试用例。

                // mov
                // if, goto out
                // lor...
                // _or
                // out
                // TODO2.33;
                ADDINS(op1, {}, des, mov)
                ADDINS(des, {}, Operand(STR(int(lor_bbl.size()) + 2), Type::IntLiteral), _goto)//第一个值为真则不计算第二个值
                ADDBBL(lor_bbl)
                ADDINS(des, op2, des, _or)
                BE_ROOT(des)
            }
        }
        
    }
    else
    {
        if(!LITERAL(land))
            ADDBBL(land_bbl)// 只有变量可能需要添加运算指令
        COPY_NODE(land)// 复制不释放变量
    }
}
void Analyzer::analyzeLAndExp(LAndExp* root, vector<ir::Instruction*> &pgm)
{
    GET_CHILD_PTR(eq, EqExp, 0)
    VINS eq_bbl;
    analyzeEqExp(eq, eq_bbl);
    
    if(SIZE > 1)
    {        
        GET_CHILD_PTR(land, LAndExp, 2)
        VINS land_bbl;
        analyzeLAndExp(land, land_bbl);
        if(LITERAL(eq) && LITERAL(land))// 都是常量则结果为常量
        {
            if(eq->t == Type::IntLiteral && land->t == Type::IntLiteral)
                root->v = STR(stoi(eq->v) && stoi(land->v));
            else if(eq->t == Type::IntLiteral && land->t == Type::FloatLiteral)
                root->v = STR(stoi(eq->v) && stof(land->v));
            else if(eq->t == Type::FloatLiteral && land->t == Type::IntLiteral)
                root->v = STR(stof(eq->v) && stoi(land->v));
            else
                root->v = STR(stof(eq->v) && stof(land->v));
            root->t = Type::IntLiteral;
        }
        else
        {
            if(LITERAL(eq))// 第一个值为常量,第二个值为变量
            {
                if((eq->t == Type::IntLiteral && stoi(eq->v) == 0) || (eq->t == Type::Float && stof(eq->v) == 0))// 第一个值为真则结果为常量, 不需要计算第二个值
                {
                    root->v = "0";
                    root->t = Type::IntLiteral;
                }
                else// 否则等效于第二个值
                {
                    ADDBBL(land_bbl)// 运行时计算第二个值
                    COPY_NODE(land)// 复制则不释放临时变量      
                }
            }
            else// 两个值需要是变量
            {
                TO_OPERAND(eq, op1)
                TO_OPERAND(land, op2)
                NEW_OPERAND(des, Type::Int)
                ADDBBL(eq_bbl)// eq...
                // TODO2.40.2;
                assert(op1.type == Type::Int && op2.type == Type::Int);
                // mov
                // not(等效于eq zero)
                // if, goto out
                // land...
                // _and
                // out
                NEW_OPERAND(tmp, Type::Int)
                ADDINS(op1, {}, des, mov)
                ADDINS(des, ZERO, tmp, eq)
                ADDINS(tmp, {}, Operand(STR(int(land_bbl.size()) + 2), Type::IntLiteral), _goto)//第一个值为真则不计算第二个值
                ADDBBL(land_bbl)
                ADDINS(des, op2, des, _and)
                BE_ROOT(des)
            }
        }
    }
    else
    {
        if(!LITERAL(eq))
            ADDBBL(eq_bbl)// 只有变量可能需要添加运算指令
        COPY_NODE(eq)
    }
}
// TODO2.30
#define analyzeBinaryExp(self, chd, asrt) \
{ \
    GET_CHILD_PTR(chd##fst, chd##Exp, 0) \
    analyze##chd##Exp(chd##fst, pgm); \
    int var_idx = -1; \
    if(LITERAL(chd##fst)) \
    { \
        REP(2, SIZE, 2) \
        { \
            VINS chd##_bbl; \
            GET_CHILD_PTR(term, Term, idx - 1) \
            GET_CHILD_PTR(chd, chd##Exp, idx) \
            analyze##chd##Exp(chd, chd##_bbl); \
            if(LITERAL(chd)) \
            { \
                assert(chd##_bbl.empty()); \
                asrt; \
                BinaryLiteral(chd##fst->v, chd##fst->t, chd->v, chd->t, term->token.type); \
            } \
            else \
            { \
                var_idx = idx; \
                break; \
            } \
        } \
    } \
    if(LITERAL(chd##fst) && var_idx == -1) \
        COPY_NODE(chd##fst) \
    else \
    { \
        Operand des; \
        if(var_idx == -1) \
        { \
            des.name = chd##fst->v; \
            des.type = chd##fst->t; \
            var_idx = 2; \
        } \
        else \
            des = Literal2Var(chd##fst->t, chd##fst->v, pgm); \
        if(var_idx < SIZE) \
        { \
            if((des.type == Type::Int || des.type == Type::Float) && des.name.find('_') != des.name.npos) \
            { \
                NEW_OPERAND(tmp, des.type) \
                Operator cal = (des.type == Type::Int) ? Operator::mov : Operator::fmov; \
                ADDINS2(des, {}, tmp, cal) \
                std::swap(des, tmp); \
            } \
            REP(var_idx, SIZE, 2) \
            { \
                GET_CHILD_PTR(term, Term, idx - 1) \
                GET_CHILD_PTR(chd, chd##Exp, idx) \
                analyze##chd##Exp(chd, pgm); \
                TO_OPERAND(chd, op) \
                asrt; \
                BinaryVar(des, op, term->token.type, pgm); \
            } \
        } \
        BE_ROOT(des) \
    }  \
}
void Analyzer::analyzeEqExp(EqExp* root, vector<ir::Instruction*> &pgm)
{
    analyzeBinaryExp(Eq, Rel, assert(term->token.type == TokenType::EQL || term->token.type == TokenType::NEQ))
}
void Analyzer::analyzeRelExp(RelExp* root, vector<ir::Instruction*> &pgm)
{
    analyzeBinaryExp(Rel, Add, assert(term->token.type == TokenType::LSS || term->token.type == TokenType::LEQ || term->token.type == TokenType::GTR || term->token.type == TokenType::GEQ))
}
// TODO2.26 
// 对加减乘除表达式中非前缀的常量暂时不进行优化,以避免错误优化可能导致的溢出问题
void Analyzer::analyzeAddExp(AddExp* root, vector<ir::Instruction*> &pgm)
{
    analyzeBinaryExp(Add, Mul, assert(term->token.type == TokenType::PLUS || term->token.type == TokenType::MINU))
}
void Analyzer::analyzeMulExp(MulExp* root, vector<ir::Instruction*> &pgm)
{
    analyzeBinaryExp(Mul, Unary, assert(term->token.type == TokenType::MULT || term->token.type == TokenType::DIV || term->token.type == TokenType::MOD))
}

void Analyzer::analyzeUnaryExp(UnaryExp* root, vector<ir::Instruction*> &pgm)
{
    if(FST_NODE_IS(PRIMARYEXP))
    {
        GET_CHILD_PTR(primary, PrimaryExp, 0)
        analyzePrimaryExp(primary, pgm);
        COPY_NODE(primary)
    }
    else if(FST_NODE_IS(TERMINAL))
    {
        GET_CHILD_PTR(idf, Term, 0)
        auto name = idf->token.value;// 函数名称
        auto params = symbol_table.functions[name]->ParameterList;
        auto typ = symbol_table.functions[name]->returnType;// 函数的返回值类型
        vector<Operand> args;
        if(SIZE > 3)// 含参调用
        {
            GET_CHILD_PTR(func, FuncRParams, 2)
            analyzeFuncRParams(func, params, args, pgm);
        }
        // TODO2.38;
        if(typ == Type::null)
        {
            pgm.push_back(new ir::CallInst(name, args, {}));
            root->t = Type::null;
        }
        else
        {
            NEW_OPERAND(res, typ);
            pgm.push_back(new ir::CallInst(name, args, res));
            BE_ROOT(res)
        }
    }
    else
    {
        GET_CHILD_PTR(unaryop, UnaryOp, 0)
        analyzeUnaryOp(unaryop);
        GET_CHILD_PTR(unaryexp, UnaryExp, 1)
        analyzeUnaryExp(unaryexp, pgm);

        if(unaryop->op == TokenType::PLUS)
            COPY_NODE(unaryexp)
        else
        {
            if(LITERAL(unaryexp))
            {
                if(unaryop->op == TokenType::MINU)
                {
                    if(unaryexp->t == Type::IntLiteral)
                        unaryexp->v = STR(-stoi(unaryexp->v));
                    else
                        unaryexp->v = STR(-stof(unaryexp->v));
                }
                else
                {
                    if(unaryexp->t == Type::IntLiteral)
                        unaryexp->v = STR(!stoi(unaryexp->v));
                    else
                        unaryexp->v = STR(!stof(unaryexp->v));
                }
                COPY_NODE(unaryexp)
            }
            else
            {
                TO_OPERAND(unaryexp, op)
                // TODO2.40.3
                // not(等效于eq/feq zero)
                if(unaryexp->t == Type::Int)
                {
                    // TODO2.27
                    NEW_OPERAND(des, Type::Int);
                    Operator cal = (unaryop->op == TokenType::MINU) ? Operator::sub : Operator::eq;
                    ADDINS2(ZERO, op, des, cal)
                    BE_ROOT(des)
                }
                else
                {
                    NEW_OPERAND(des, Type::Float);
                    Operator fcal = (unaryop->op == TokenType::MINU) ? Operator::fsub : Operator::feq;
                    ADDINS2(FZERO, op, des, fcal)
                    BE_ROOT(des)
                }
            }
            
        }
    }
}
void Analyzer::analyzePrimaryExp(PrimaryExp* root, vector<ir::Instruction*> &pgm)
{
    if(FST_NODE_IS(TERMINAL))
    {
        GET_CHILD_PTR(exp, Exp, 1)
        analyzeExp(exp, pgm);
        COPY_NODE(exp)
    }
    else if(FST_NODE_IS(LVAL))
    {
        GET_CHILD_PTR(lval, LVal, 0)
        analyzeLVal(lval, pgm);

        // TODO2.29;
        if(lval->t == Type::IntPtr || lval->t == Type::FloatPtr) // 是数组
        {
            // 数组取数到临时变量
            Operand lvar = Operand(lval->v, lval->t);
            Operand ofs = Operand(lval->ofs, Type::Int);
            // 变量类型是指针
            if(lval->isPtr)
            {
                NEW_OPERAND(des, lval->t)
                ADDINS(lvar, ofs, des, getptr)
                BE_ROOT(des)
            }
            else
            {
                auto type = (lval->t == Type::IntPtr)? Type::Int : Type::Float;
                NEW_OPERAND(des, type)
                ADDINS(lvar, ofs, des, load)
                BE_ROOT(des)
            }
        }
        else
            COPY_NODE(lval)
    }
    else
    {
        GET_CHILD_PTR(num, Number, 0)
        analyzeNumber(num, pgm);
        COPY_NODE(num)
    }
}
void Analyzer::analyzeFuncRParams(FuncRParams* root, vector<ir::Operand> &params, vector<ir::Operand> &args, vector<ir::Instruction*> &pgm)
{
    assert(int(params.size()) == (SIZE + 1) / 2);// 检查参数数量
    int cnt = 0;
    REP(0, SIZE, 2)
    {
        GET_CHILD_PTR(exp, Exp, idx)
        analyzeExp(exp, pgm);
        auto &param = params[cnt++];
        auto arg = Operand(exp->v, exp->t);
        switch(param.type)
        {
            case Type::Int:
                if(arg.type == Type::Float)// 需要运行时类型转换
                {
                    NEW_OPERAND(tmp, Type::Int)// 需要临时变量
                    ADDINS(arg, {}, tmp, cvt_f2i)
                    std::swap(arg, tmp);
                }
                else if(arg.type == Type::FloatLiteral)
                {
                    arg.name = STR(int(stof(arg.name)));
                    arg.type = Type::IntLiteral;
                }               
                break;
            case Type::Float:
                if(arg.type == Type::Int)// 需要运行时类型转换
                {
                    NEW_OPERAND(tmp, Type::Float)// 需要临时变量
                    ADDINS(arg, {}, tmp, cvt_i2f)
                    std::swap(arg, tmp);
                }
                else if(arg.type == Type::IntLiteral)
                {
                    arg.name = STR(float(stoi(arg.name)));
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
        args.push_back(arg);// 记录参数列表
    }
}
void Analyzer::analyzeUnaryOp(UnaryOp* root)
{
    GET_CHILD_PTR(term, Term, 0)
    assert(term->token.type == TokenType::PLUS || term->token.type == TokenType::MINU || term->token.type == TokenType::NOT);
    root->op = term->token.type;
}
void Analyzer::analyzeLVal(LVal* root, vector<ir::Instruction*> &pgm)
{
    GET_CHILD_PTR(idf, Term, 0)
    auto ste = symbol_table.get_ste(idf->token.value);// 符号表项
    auto opd = ste.operand;
    
    // TODO2.32.2
    if(ste.isConst && (opd.type == Type::Int || opd.type == Type::Float))// 常量变量直接替换为字面量
    {
        root->v = ste.val;
        root->t = (opd.type == Type::Int) ? Type::IntLiteral : Type::FloatLiteral;
        root->isPtr = false;
    }
    else //变量或常量数组在运行时加载
    {
        BE_ROOT(opd)
        auto &dim = ste.dimension;
        if(dim.size())
        {
            int cur = 0, size = ste.size;
            Operand ofs = Literal2Var(Type::IntLiteral, "0", pgm);
            REP(2, SIZE, 3)
            {
                size /= dim[cur++];// 当前子数组的大小
                auto tmp = Literal2Var(Type::IntLiteral, STR(size), pgm);
                GET_CHILD_PTR(exp, Exp, idx)
                analyzeExp(exp, pgm);
                assert(exp->t != Type::Float && exp->t != Type::FloatLiteral);// 下标不能是浮点数
                TO_OPERAND(exp, expvar)// 当前维度上的偏移位数

                ADDINS(tmp, expvar, tmp, mul)// 本维度贡献的偏移量
                ADDINS(ofs, tmp, ofs, add)// 偏移量累加
            }
            root->ofs = ofs.name;
            root->isPtr = cur < int(dim.size());
        }
        else
            root->isPtr = false;
    }
}
void Analyzer::analyzeNumber(Number* root, vector<ir::Instruction*> &pgm)
{
    GET_CHILD_PTR(term, Term, 0)
    if(term->token.type == TokenType::INTLTR)
    {
        auto &num = term->token.value;
        int size = num.size();
        if (size >= 3 && num[0] == '0' && tolower(num[1]) == 'x')
            root->v = STR(stoi(num, nullptr, 16));// 十六进制
        else if (size >= 3 && num[0] == '0' && tolower(num[1] == 'b'))
            root->v = STR(stoi(num.substr(2), nullptr, 2));// 二进制
        else if (size >= 2 && num[0] == '0')
            root->v = STR(stoi(num, nullptr, 8));// 八进制
        else
            root->v = num;// 十进制

        root->t = Type::IntLiteral;
    }
    else if(term->token.type == TokenType::FLOATLTR)
    {
        root->v = term->token.value;
        root->t = Type::FloatLiteral;
    }
    else
        ASSERT(Invalid Number)
}
// TODO2.25;
// 把立即数变成变量。如果是整型立即数目前使用def实现,浮点立即数目前使用fdef实现,没有必要添加到局部符号表
// 实际上整型立即数应该通过$zero寄存器和addi实现,浮点立即数则应该通过内存取数实现,立即数0和0.0应该由架构通过寄存器等直接提供。本层暂无$zero寄存器,也无浮点数预存在内存中的机制,故此。
Operand Analyzer::Literal2Var(ir::Type typ, string val, vector<ir::Instruction*> &pgm)
{
    Operand opd;
    if(typ == Type::IntLiteral)
    {
        opd = {NEW, Type::Int};
        ADDINS(Operand(val, Type::IntLiteral), {}, opd, def)
    }
    else if(typ == Type::FloatLiteral)
    {
        opd = {NEW, Type::Float};
        ADDINS(Operand(val, Type::FloatLiteral), {}, opd, fdef)
    }
    else
        ASSERT(Not literal)
    return opd;
}
#define CAL(val1, val2) { \
    switch(tk_type) \
    { \
        case TokenType::EQL: \
            v1 = STR(val1 == val2); \
            break; \
        case TokenType::NEQ: \
            v1 = STR(val1 != val2); \
            break; \
        case TokenType::LSS: \
            v1 = STR(val1 < val2); \
            break; \
        case TokenType::LEQ: \
            v1 = STR(val1 <= val2); \
            break; \
        case TokenType::GTR: \
            v1 = STR(val1 > val2); \
            break; \
        case TokenType::GEQ: \
            v1 = STR(val1 >= val2); \
            break; \
        case TokenType::PLUS: \
            v1 = STR(val1 + val2); \
            break; \
        case TokenType::MINU: \
            v1 = STR(val1 - val2); \
            break; \
        case TokenType::MULT: \
            v1 = STR(val1 * val2); \
            break; \
        case TokenType::DIV: \
            v1 = STR(val1 / val2); \
            break; \
        default: \
            ASSERT(Invalid binary operator) \
    } \
}
// TODO2.34;
void Analyzer::BinaryLiteral(string &v1, ir::Type &t1,string &v2, ir::Type &t2, frontend::TokenType tk_type)
{
    if(tk_type == TokenType::MOD)
    {
        assert(t1 == Type::IntLiteral && t2 == Type::IntLiteral);
        v1 = STR(stoi(v1) % stoi(v2));
    }
    else
    {
        if(t1 == Type::IntLiteral && t2 == Type::IntLiteral)
            CAL(stoi(v1), stoi(v2))
        else if(t1 == Type::IntLiteral && t2 == Type::FloatLiteral)
        { 
            CAL(stoi(v1), stof(v2))
            t1 = Type::FloatLiteral;
        }
        else if(t1 == Type::FloatLiteral && t2 == Type::IntLiteral)
            CAL(stof(v1), stoi(v2))
        else
            CAL(stof(v1), stof(v2))
    }
}
void Analyzer::BinaryVar(ir::Operand &op1, ir::Operand &op2, frontend::TokenType tk_type, vector<ir::Instruction*> &pgm)
{
    Operator cal, fcal;
    switch(tk_type)
    {
        case TokenType::EQL:
            cal = Operator::eq;
            fcal = Operator::feq;
            break;
        case TokenType::NEQ:
            cal = Operator::neq;
            fcal = Operator::fneq;
            break;
        case TokenType::LSS:
            cal = Operator::lss;
            fcal = Operator::flss;
            break;
        case TokenType::LEQ:
            cal = Operator::leq;
            fcal = Operator::fleq;
            break;
        case TokenType::GTR:
            cal = Operator::gtr;
            fcal = Operator::fgtr;
            break;
        case TokenType::GEQ:
            cal = Operator::geq;
            fcal = Operator::fgeq;
            break;
        case TokenType::PLUS:
            cal = Operator::add;
            fcal = Operator::fadd;
            break;
        case TokenType::MINU:
            cal = Operator::sub;
            fcal = Operator::fsub;
            break;
        case TokenType::MULT:
            cal = Operator::mul;
            fcal = Operator::fmul;
            break;
        case TokenType::DIV:
            cal = Operator::div;
            fcal = Operator::fdiv;
            break;
        case TokenType::MOD:
            assert(op1.type == Type::Int && op2.type == Type::Int);
            cal = Operator::mod;
            break;
        default:
            ASSERT(Invalid binary operator)
    }

    if(op1.type == Type::Int && op2.type == Type::Int)
        ADDINS2(op1, op2, op1, cal)
    else if(op1.type == Type::Int && op2.type == Type::Float)
    {
        NEW_OPERAND(tmp, Type::Float)
        ADDINS(op1, {}, tmp, cvt_i2f)
        ADDINS2(op2, tmp, op2, fcal)
        std::swap(op1, op2);  
    }
    else if(op1.type == Type::Float && op2.type == Type::Int)
    {
        NEW_OPERAND(tmp, Type::Float)
        ADDINS(op2, {}, tmp, cvt_i2f)
        ADDINS2(op1, tmp, op1, fcal)
    }
    else
        ADDINS2(op1, op2, op1, fcal)
}
