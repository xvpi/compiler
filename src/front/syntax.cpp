#include"front/syntax.h"
#include<iostream>
#include<cassert>
using frontend::Parser;
// #define DEBUG_PARSER
#define TODO assert(0 && "todo")
#define CUR_TOKEN_IS(tk_type) (token_stream[index].type == TokenType::tk_type)
#define PARSE_TOKEN(tk_type) root->children.push_back(parseTerm(root, TokenType::tk_type))
#define PARSE(name, type) \
    do { \
        auto name = new type(root); \
        if (!parse##type(name)) { \
            delete name; \
            return false; \
        } \
        root->children.push_back(name); \
    } while(0)
/******************************************/
// 匹配任意一个 TokenType（可变参数宏）
#define NEXT_TOKEN_IS(offset, tk_type) (token_stream[index + offset].type == TokenType::tk_type)
#define CUR_TOKEN_IN(...) token_in({__VA_ARGS__})
#define CUR_TOKEN (token_stream[index])
#define PARSE_TOKEN_VAR(tk_var) root->children.push_back(parseTerm(root, tk_var))

// 判断当前 token 是否在指定集合中
bool Parser::token_in(std::initializer_list<TokenType> types) {
    if (index >= token_stream.size()) return false;  // 越界保护
    for (auto t : types) {
        if (token_stream[index].type == t)
            return true;
    }
    return false;
}
/******************************************/
Parser::Parser(const std::vector<frontend::Token>& tokens): index(0), token_stream(tokens) {}
Parser::~Parser() {}

frontend::CompUnit* Parser::get_abstract_syntax_tree(){
    //TODO： 定义根节点
    /******************************/
    frontend::CompUnit* r = new frontend::CompUnit();
    assert(parseCompUnit(r));
    return r;
    /******************************/
}
void Parser::log(AstNode* node){
    #ifdef DEBUG_PARSER
        std::cout << "in parse" << toString(node->type) << ", cur_token_type::" << toString(token_stream[index].type) << ", token_val::" << token_stream[index].value << '\n';
    #endif
}

//TODO : 编写parser具体内容
/*****************************************************************************/
// 判断辅助函数

// 判断当前 token 是否是 UnaryExp 起始符
bool Parser::in_unaryexp() {
    return CUR_TOKEN_IN(TokenType::PLUS, TokenType::MINU, TokenType::NOT,
                        TokenType::IDENFR, TokenType::LPARENT,
                        TokenType::INTLTR, TokenType::FLOATLTR);
}

// 判断当前 token 是否是语句起始符
bool Parser::in_stmt() {
    return CUR_TOKEN_IN(TokenType::LBRACE, TokenType::IFTK, TokenType::WHILETK,
                        TokenType::BREAKTK, TokenType::CONTINUETK, TokenType::RETURNTK,
                        TokenType::SEMICN) || in_unaryexp();
}
//TODO：挨个词法分析
frontend::Term* Parser::parseTerm(AstNode* root, TokenType expect) {
    if (index >= token_stream.size()) return nullptr;  // 越界检查
    
    if (token_stream[index].type == expect) {
        Term* term = new Term(token_stream[index], root);
        //root->children.push_back(term);
        index++;  // 确保index前进
        return term;
    }
    return nullptr;
}

// 解析 CompUnit: (Decl | FuncDef) [CompUnit]
bool Parser::parseCompUnit(AstNode* root){
    log(root);
    if (CUR_TOKEN_IN(TokenType::INTTK, TokenType::FLOATTK)){
        if (token_stream[index+2].type == TokenType::LBRACK || token_stream[index+2].type == TokenType::ASSIGN || token_stream[index+2].type == TokenType::COMMA || token_stream[index+2].type == TokenType::SEMICN){
            PARSE(decl, Decl);
        }else if (token_stream[index+2].type == TokenType::LPARENT){
            PARSE(funcdef, FuncDef);
        }
    }else if (CUR_TOKEN_IS(CONSTTK)){
        PARSE(decl, Decl);
    }else if (CUR_TOKEN_IS(VOIDTK)){
        PARSE(funcdef, FuncDef);
    }
    if (CUR_TOKEN_IN(TokenType::CONSTTK, TokenType::INTTK, TokenType::VOIDTK, TokenType::FLOATTK)){
        PARSE(compunit, CompUnit);
    }
    return true;
}
// 解析 Decl: ConstDecl | VarDecl
bool Parser::parseDecl(AstNode* root){
    log(root);
    if (CUR_TOKEN_IS(CONSTTK)){
        PARSE(constdecl, ConstDecl);  // 解析常量声明
    } else if (CUR_TOKEN_IN(TokenType::INTTK, TokenType::FLOATTK)) {
        PARSE(vardecl, VarDecl);  // 解析变量声明
    }
    return true;
}

// 解析 ConstDecl: 'const' BType ConstDef { ',' ConstDef } ';'
bool Parser::parseConstDecl(AstNode* root){
    log(root);
    PARSE_TOKEN(CONSTTK);  // 解析 'const'
    PARSE(btype, BType);  // 解析基本类型
    PARSE(constdef, ConstDef);  // 解析常量定义
    while (CUR_TOKEN_IS(COMMA)){
        PARSE_TOKEN(COMMA);  // 解析 ','
        PARSE(constdef, ConstDef);  // 解析另一个常量定义
    }
    PARSE_TOKEN(SEMICN);  // 解析 ';'
    return true;
}

// 解析 BType: 'int' | 'float'
bool Parser::parseBType(AstNode* root){
    log(root);
    switch (CUR_TOKEN.type) {
        case TokenType::INTTK:
            PARSE_TOKEN(INTTK);  // 解析 int 类型
            break;
        case TokenType::FLOATTK:
            PARSE_TOKEN(FLOATTK);  // 解析 float 类型
            break;
        default:
            break;
    }
    return true;
}

// 解析 ConstDef: Ident { '[' ConstExp ']' } '=' ConstInitVal
bool Parser::parseConstDef(AstNode* root){
    log(root);
    PARSE_TOKEN(IDENFR);  // 解析标识符
    while (CUR_TOKEN_IS(LBRACK)){
        PARSE_TOKEN(LBRACK);  // 解析 '['
        PARSE(constexp, ConstExp);  // 解析常量表达式
        PARSE_TOKEN(RBRACK);  // 解析 ']'
    }
    PARSE_TOKEN(ASSIGN);  // 解析 '='
    PARSE(constinitval, ConstInitVal);  // 解析常量初始化值
    return true;
}

// 解析 ConstInitVal: ConstExp | '{' [ ConstInitVal { ',' ConstInitVal } ] '}'
bool Parser::parseConstInitVal(AstNode* root){
    log(root);
    if (CUR_TOKEN_IS(LBRACE)){
        PARSE_TOKEN(LBRACE);  // 解析 '{'
        if (in_unaryexp() || CUR_TOKEN_IS(LBRACE)){
            PARSE(constinitval, ConstInitVal);  // 解析常量初始化值
            while (CUR_TOKEN_IS(COMMA)){
                PARSE_TOKEN(COMMA);  // 解析 ','
                PARSE(constinitval, ConstInitVal);  // 解析另一个常量初始化值
            }
        }
        PARSE_TOKEN(RBRACE);  // 解析 '}'
    } else if (in_unaryexp()){
        PARSE(constexp, ConstExp);  // 解析常量表达式
    }
    return true;
}

// 解析 VarDecl: BType VarDef { ',' VarDef } ';'
bool Parser::parseVarDecl(AstNode* root){
    log(root);
    PARSE(btype, BType);  // 解析基本类型
    PARSE(vardef, VarDef);  // 解析变量定义
    while (CUR_TOKEN_IS(COMMA)){
        PARSE_TOKEN(COMMA);  // 解析 ','
        PARSE(vardef, VarDef);  // 解析另一个变量定义
    }
    PARSE_TOKEN(SEMICN);  // 解析 ';'
    return true;
}

// 解析 VarDef: Ident { '[' ConstExp ']' } [ '=' InitVal ]
bool Parser::parseVarDef(AstNode* root){
    log(root);
    PARSE_TOKEN(IDENFR);  // 解析标识符
    while (CUR_TOKEN_IS(LBRACK)){
        PARSE_TOKEN(LBRACK);  // 解析 '['
        PARSE(constexp, ConstExp);  // 解析常量表达式
        PARSE_TOKEN(RBRACK);  // 解析 ']'
    }
    if (CUR_TOKEN_IS(ASSIGN)){
        PARSE_TOKEN(ASSIGN);  // 解析 '='
        PARSE(initval, InitVal);  // 解析初始化值
    }
    return true;
}

// 解析 InitVal: Exp | '{' [ InitVal { ',' InitVal } ] '}'
bool Parser::parseInitVal(AstNode* root){
    log(root);
    if (in_unaryexp()){
        PARSE(exp, Exp);  // 解析表达式
    } else if (CUR_TOKEN_IS(LBRACE)){
        PARSE_TOKEN(LBRACE);  // 解析 '{'
        if (in_unaryexp() || CUR_TOKEN_IS(LBRACE)){
            PARSE(initval, InitVal);  // 解析初始化值
            while (CUR_TOKEN_IS(COMMA)){
                PARSE_TOKEN(COMMA);  // 解析 ','
                PARSE(initval, InitVal);  // 解析另一个初始化值
            }
        }
        PARSE_TOKEN(RBRACE);  // 解析 '}'
    }
    return true;
}

// 解析 FuncDef: FuncType Ident '(' [FuncFParams] ')' Block
bool Parser::parseFuncDef(AstNode* root){
    log(root);
    PARSE(functype, FuncType);  // 解析函数类型
    PARSE_TOKEN(IDENFR);  // 解析标识符
    PARSE_TOKEN(LPARENT);  // 解析 '('
    if (CUR_TOKEN_IN(TokenType::INTTK, TokenType::FLOATTK)){
        PARSE(funcfparms, FuncFParams);  // 解析函数参数
    }
    PARSE_TOKEN(RPARENT);  // 解析 ')'
    PARSE(block, Block);  // 解析函数体
    return true;
}

// 解析 FuncType: 'void' | 'int' | 'float'
bool Parser::parseFuncType(AstNode* root){
    log(root);
    switch (token_stream[index].type) {
        case TokenType::VOIDTK:
            PARSE_TOKEN(VOIDTK);  // 解析 void 类型
            break;
        case TokenType::INTTK:
            PARSE_TOKEN(INTTK);  // 解析 int 类型
            break;
        case TokenType::FLOATTK:
            PARSE_TOKEN(FLOATTK);  // 解析 float 类型
            break;
        default:
            break;
    }
    return true;
}

// FuncFParams -> FuncFParam { ',' FuncFParam }
bool Parser::parseFuncFParams(AstNode* root) {
    log(root);
    PARSE(funcfparam, FuncFParam);
    while (CUR_TOKEN_IS(COMMA)) {
        PARSE_TOKEN(COMMA);
        PARSE(funcfparam, FuncFParam);
    }
    return true;
}
// 解析 FuncFParam
bool Parser::parseFuncFParam(AstNode* root){
    log(root);
    PARSE(btype, BType);
    PARSE_TOKEN(IDENFR);
    if (CUR_TOKEN_IS(LBRACK)){
        PARSE_TOKEN(LBRACK);
        PARSE_TOKEN(RBRACK);
        while (CUR_TOKEN_IS(LBRACK)){
            PARSE_TOKEN(LBRACK);
            PARSE(exp, Exp);      
            PARSE_TOKEN(RBRACK);
        }
    }
    return true;
}

// Block -> '{' { BlockItem } '}'
bool Parser::parseBlock(AstNode* root) {
    log(root);
    PARSE_TOKEN(LBRACE);  // 只解析一次左大括号
    
    while (index < token_stream.size() && 
          (CUR_TOKEN_IN(TokenType::CONSTTK, TokenType::INTTK, TokenType::FLOATTK) || 
           in_stmt())) {
        PARSE(blockitem, BlockItem);
    }
    
    PARSE_TOKEN(RBRACE);  // 只解析一次右大括号
    return true;
}
// BlockItem -> Decl | Stmt
bool Parser::parseBlockItem(AstNode* root) {
    log(root);
    if (CUR_TOKEN_IN(TokenType::CONSTTK, TokenType::INTTK, TokenType::FLOATTK)) {
        PARSE(decl, Decl);
    } else if (in_stmt()) {
        PARSE(stmt, Stmt);
    }
    return true;
}

// Stmt -> LVal '=' Exp ';' 
//       | Block 
//       | 'if' '(' Cond ')' Stmt [ 'else' Stmt ] 
//       | 'while' '(' Cond ')' Stmt 
//       | 'break' ';' 
//       | 'continue' ';' 
//       | 'return' [Exp] ';' 
//       | [Exp] ';'
bool Parser::parseStmt(AstNode* root) {
    log(root);
    const Token& cur = token_stream[index];

    if (cur.type == TokenType::IDENFR) {
        // 可能是赋值语句或表达式语句
        if (token_stream[index+1].type == TokenType::ASSIGN || 
            token_stream[index+1].type == TokenType::LBRACK) {
            PARSE(lval, LVal);
            PARSE_TOKEN(ASSIGN);
            PARSE(exp, Exp);
            PARSE_TOKEN(SEMICN);
        } else {
            PARSE(exp, Exp);
            PARSE_TOKEN(SEMICN);
        }
    }
    else if (cur.type == TokenType::LBRACE) {
        PARSE(block, Block);
    }
    else if (cur.type == TokenType::IFTK) {
        PARSE_TOKEN(IFTK);
        PARSE_TOKEN(LPARENT);
        PARSE(cond, Cond);
        PARSE_TOKEN(RPARENT);
        PARSE(stmt, Stmt);
        if (CUR_TOKEN_IS(ELSETK)) {
            PARSE_TOKEN(ELSETK);
            PARSE(stmt, Stmt);
        }
    }
    else if (cur.type == TokenType::WHILETK) {
        PARSE_TOKEN(WHILETK);
        PARSE_TOKEN(LPARENT);
        PARSE(cond, Cond);
        PARSE_TOKEN(RPARENT);
        PARSE(stmt, Stmt);
    }
    else if (cur.type == TokenType::BREAKTK) {
        PARSE_TOKEN(BREAKTK);
        PARSE_TOKEN(SEMICN);
    }
    else if (cur.type == TokenType::CONTINUETK) {
        PARSE_TOKEN(CONTINUETK);
        PARSE_TOKEN(SEMICN);
    }
    else if (cur.type == TokenType::RETURNTK) {
        PARSE_TOKEN(RETURNTK);
        if (in_unaryexp()) {
            PARSE(exp, Exp);
        }
        PARSE_TOKEN(SEMICN);
    }
    else if (cur.type == TokenType::SEMICN) {
        PARSE_TOKEN(SEMICN);
    }
    else if (in_unaryexp()) {
        PARSE(exp, Exp);
        PARSE_TOKEN(SEMICN);
    }
    else {
        return false;
    }
    return true;
}

// 加法表达式:Exp -> AddExp
bool Parser::parseExp(AstNode* root){
    log(root);
    PARSE(addexp, AddExp);
    return true;
}
// 条件表达式:Cond -> LOrExp
bool Parser::parseCond(AstNode* root){
    log(root);
    PARSE(lorexp, LOrExp);
    return true;
}
// 左值为标识符，后可接若干维度下标访问
// LVal -> Ident {'[' Exp ']'}
bool Parser::parseLVal(AstNode* root){
    log(root);
    PARSE_TOKEN(IDENFR);
    while (CUR_TOKEN_IS(LBRACK)){
        PARSE_TOKEN(LBRACK);
        PARSE(exp, Exp);
        PARSE_TOKEN(RBRACK);
    }
    return true;
}
// Number -> IntConst | floatConst
bool Parser::parseNumber(AstNode* root) {
    log(root);
    if (CUR_TOKEN_IS(INTLTR)) {
        PARSE_TOKEN(INTLTR);
    }
    else if (CUR_TOKEN_IS(FLOATLTR)) {
        PARSE_TOKEN(FLOATLTR);
    }
    else {
        return false;
    }
    return true;
}
// bool Parser::parseNumber(AstNode* root) {
//     log(root);
//     // 1) 被拆成两段的浮点：03.1415 → INTLTR(03) + FLOATLTR(1415)
//     if (CUR_TOKEN_IS(INTLTR) && NEXT_TOKEN_IS(1, FLOATLTR)) {
//         // 先把整数部分当作一个 Term
//         Term* intPart = parseTerm(root, TokenType::INTLTR);
//         assert(intPart);
//         // 再把小数部分当作一个 Term
//         Term* fracPart = parseTerm(root, TokenType::FLOATLTR);
//         assert(fracPart);
//         // AST 节点里头就放两个子节点，由 semantic 阶段根据它们 Value 拼接 “03.1415”
//     }
//     // 2) 本来就一个带小数点的浮点（词法器支持的 FLOATLTR）
//     else if (CUR_TOKEN_IS(FLOATLTR)) {
//         PARSE_TOKEN(FLOATLTR);
//     }
//     // 3) 普通整数
//     else if (CUR_TOKEN_IS(INTLTR)) {
//         PARSE_TOKEN(INTLTR);
//     }
//     else {
//         return false;
//     }
//     return true;
// }

// PrimaryExp -> '(' Exp ')' | LVal | Number
// 主表达式可以是括号括起来的表达式、一个变量（左值）或一个字面量常数
bool Parser::parsePrimaryExp(AstNode* root) {
    log(root);
    if (CUR_TOKEN_IS(LPARENT)) {
        PARSE_TOKEN(LPARENT);
        PARSE(exp, Exp);
        PARSE_TOKEN(RPARENT);
    } 
    else if (CUR_TOKEN_IS(IDENFR)) {
        PARSE(lval, LVal);
    }
    else if (CUR_TOKEN_IN(TokenType::INTLTR, TokenType::FLOATLTR)) {
        PARSE(number, Number);
    }
    else {
        return false;
    }
    return true;
}
// UnaryExp -> PrimaryExp | Ident '(' [FuncRParams] ')' | UnaryOp UnaryExp
// 一元表达式可能是主表达式、函数调用，或一元运算符作用于另一表达式
bool Parser::parseUnaryExp(AstNode* root) {
    log(root);
    if (CUR_TOKEN_IS(IDENFR) && token_stream[index + 1].type == TokenType::LPARENT) {
        // 函数调用形式
        PARSE_TOKEN(IDENFR);
        PARSE_TOKEN(LPARENT);
        if (in_unaryexp()) {
            PARSE(funcrparams, FuncRParams);
        }
        PARSE_TOKEN(RPARENT);
    } else if (CUR_TOKEN_IN(TokenType::LPARENT, TokenType::IDENFR, TokenType::INTLTR, TokenType::FLOATLTR)) {
        // 普通主表达式
        PARSE(primaryexp, PrimaryExp);
    } else if (CUR_TOKEN_IN(TokenType::PLUS, TokenType::MINU, TokenType::NOT)) {
        // 一元运算符 + 后续表达式
        PARSE(unaryop, UnaryOp);
        PARSE(unaryexp, UnaryExp);
    }
    return true;
}

// 一元运算符为 + - !
// UnaryOp -> '+' | '-' | '!'
bool Parser::parseUnaryOp(AstNode* root){
    log(root);
    if (CUR_TOKEN_IS(PLUS)){
        PARSE_TOKEN(PLUS);
    }else if (CUR_TOKEN_IS(MINU)){
        PARSE_TOKEN(MINU);
    }else if (CUR_TOKEN_IS(NOT)){
        PARSE_TOKEN(NOT);
    }
    return true;
}

// 实参列表：表达式序列，逗号分隔
// FuncRParams -> Exp { ',' Exp }
bool Parser::parseFuncRParams(AstNode* root){
    log(root);
    PARSE(exp, Exp);
    while (CUR_TOKEN_IS(COMMA)){
        PARSE_TOKEN(COMMA);
        PARSE(exp, Exp);
    }
    return true;
}
// MulExp -> UnaryExp { ('*' | '/' | '%') UnaryExp }
// 乘除模表达式
bool Parser::parseMulExp(AstNode* root){
    log(root);
    PARSE(unaryexp, UnaryExp);
    while (CUR_TOKEN_IN(TokenType::MULT, TokenType::DIV, TokenType::MOD)){
        PARSE_TOKEN_VAR(CUR_TOKEN.type);
        PARSE(unaryexp, UnaryExp);
    }
    return true;
}
// AddExp -> MulExp { ('+' | '-') MulExp }
// 加减表达式
bool Parser::parseAddExp(AstNode* root){
    log(root);
    PARSE(mulexp, MulExp);
    while (CUR_TOKEN_IN(TokenType::PLUS, TokenType::MINU)){
        PARSE_TOKEN_VAR(CUR_TOKEN.type);
        PARSE(mulexp, MulExp);
    }
    return true;
}
// RelExp -> AddExp { ('<' | '>' | '<=' | '>=') AddExp }
// 关系表达式
bool Parser::parseRelExp(AstNode* root){
    log(root);
    PARSE(addexp, AddExp);
    while (CUR_TOKEN_IN(TokenType::LSS,TokenType::GTR, TokenType::LEQ, TokenType::GEQ)){
        PARSE_TOKEN_VAR(CUR_TOKEN.type);
        PARSE(addexp, AddExp);
    }
    return true;
}
// EqExp -> RelExp { ('==' | '!=') RelExp }
// 相等性表达式
bool Parser::parseEqExp(AstNode* root){
    log(root);
    PARSE(relexp, RelExp);
    while (CUR_TOKEN_IN(TokenType::EQL, TokenType::NEQ)){
        PARSE_TOKEN_VAR(CUR_TOKEN.type);
        PARSE(relexp, RelExp);
    }
    return true;
}
// LAndExp -> EqExp [ '&&' LAndExp ]
// 逻辑与表达式
bool Parser::parseLAndExp(AstNode* root){
    log(root);
    PARSE(eqexp, EqExp);
    if (CUR_TOKEN_IS(AND)){
        PARSE_TOKEN(AND);
        PARSE(landexp, LAndExp);
    }
    return true;
}
// LOrExp -> LAndExp [ '||' LOrExp ]
// 逻辑或表达式
bool Parser::parseLOrExp(AstNode* root){
    log(root);
    PARSE(landexp, LAndExp);
    if (CUR_TOKEN_IS(OR)){
        PARSE_TOKEN(OR);
        PARSE(lorexp, LOrExp);
    }
    return true;
}
// ConstExp -> AddExp
// 常量表达式直接为加减表达式
bool Parser::parseConstExp(AstNode* root){
    log(root);
    PARSE(addexp, AddExp);
    return true;
}

