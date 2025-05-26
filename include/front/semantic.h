#ifndef SEMANTIC_H
#define SEMANTIC_H

#include"ir/ir.h"
#include"front/abstract_syntax_tree.h"
#include <functional>
#include<vector>
#include<map>
#include<string>
#include<vector>
using std::map;
using std::string;
using std::vector;

namespace frontend
{

// definition of symbol table entry
struct STE {
    ir::Operand operand;
    vector<int> dimension;
    
    // TODO:新增字符表属性和函数;
    int size;
    bool isConst;
    string val;
    STE();
    STE(ir::Operand, vector<int>, int, bool);
};

using map_str_ste = map<string, STE>;
// definition of scope infomation
struct ScopeInfo {
    int cnt;
    string name;
    map_str_ste table;
};

// surpport lib functions
map<std::string,ir::Function*>* get_lib_funcs();

// definition of symbol table
struct SymbolTable{
    vector<ScopeInfo> scope_stack;
    map<std::string,ir::Function*> functions;

    /**
     * @brief enter a new scope, record the infomation in scope stacks
     */
    void add_scope();


    /**
     * @brief exit a scope, pop out infomations
     */
    void exit_scope();

    /**
     * @brief Get the scoped name, to deal the same name in different scopes, we change origin id to a new one with scope infomation,
     * for example, we have these code:
     * "     
     * int a;
     * {
     *      int a; ....
     * }
     * "
     * in this case, we have two variable both name 'a', after change they will be 'a' and 'a_block'
     * @param id: origin id 
     * @return string: new name with scope infomations
     */
    string get_scoped_name(string id) const;

    /**
     * @brief get the right operand with the input name
     * @param id identifier name
     * @return Operand 
     */
    // ir::Operand get_operand(string id) const;

    /**
     * @brief get the right ste with the input name
     * @param id identifier name
     * @return STE 
     */
    STE get_ste(string id) const;
};


// singleton class
struct Analyzer {
    int tmp_cnt;
    vector<ir::Instruction*> g_init_inst;
    SymbolTable symbol_table;

    // TODO：新增属性;
    ir::Function* cur_func;
    /**
     * @brief constructor
     */
    Analyzer();

    // analysis functions
    ir::Program get_ir_program(CompUnit*);

    // reject copy & assignment
    Analyzer(const Analyzer&) = delete;
    Analyzer& operator=(const Analyzer&) = delete;

    void analysisCompUnit(CompUnit*);
    void analysisDecl(Decl*, vector<ir::Instruction*> &);
    void analysisConstDecl(ConstDecl*, vector<ir::Instruction*> &);
    void analysisBType(BType*);
    void analysisConstDef(ConstDef*, vector<ir::Instruction*> &, ir::Type);
    void analysisConstInitVal(ConstInitVal*, vector<ir::Instruction*> &, int, int, int, vector<int> &);
    void analysisVarDecl(VarDecl*, vector<ir::Instruction*> &);
    void analysisVarDef(VarDef*, vector<ir::Instruction*> &, ir::Type);
    void analysisInitVal(InitVal* root, vector<ir::Instruction*> &, int, int, int, vector<int> &);
    void analysisFuncDef(FuncDef*);
    ir::Type analysisFuncType(FuncType*);
    void analysisFuncFParams(FuncFParams*, vector<ir::Operand> &);
    void analysisFuncFParam(FuncFParam*, vector<ir::Operand> &);
    void analysisBlock(Block*, vector<ir::Instruction*> &);
    void analysisBlockItem(BlockItem*, vector<ir::Instruction*> &);
    void analysisStmt(Stmt*, vector<ir::Instruction*> &);
    void analysisConstExp(ConstExp*);    
    void analysisExp(Exp*, vector<ir::Instruction*> &);    
    void analysisCond(Cond*, vector<ir::Instruction*> &);
    void analysisLOrExp(LOrExp*, vector<ir::Instruction*> &);
    void analysisLAndExp(LAndExp*, vector<ir::Instruction*> &);
    void analysisEqExp(EqExp*, vector<ir::Instruction*> &);
    void analysisRelExp(RelExp*, vector<ir::Instruction*> &);
    void analysisAddExp(AddExp*, vector<ir::Instruction*> &);
    void analysisMulExp(MulExp*, vector<ir::Instruction*> &);
    void analysisUnaryExp(UnaryExp*, vector<ir::Instruction*> &);
    void analysisPrimaryExp(PrimaryExp*, vector<ir::Instruction*> &);
    void analysisFuncRParams(FuncRParams*, vector<ir::Operand> &, vector<ir::Operand> &, vector<ir::Instruction*> &);
    void analysisUnaryOp(UnaryOp*);
    void analysisLVal(LVal*, vector<ir::Instruction*> &);
    void analysisNumber(Number*, vector<ir::Instruction*> &);
    std::string calculateBinary(TokenType tk_type, float lhs, float rhs);    
    ir::Operand Literal2Var(ir::Type typ, const std::string& val, std::vector<ir::Instruction*>& pgm);
    void BinaryLiteral(string &, ir::Type &t1,string &, ir::Type &, frontend::TokenType tk_type);
    void BinaryVar(ir::Operand &, ir::Operand &, frontend::TokenType, vector<ir::Instruction*> &);
};

} // namespace frontend

#endif