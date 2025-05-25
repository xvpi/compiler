#include"front/lexical.h"
#include<map>
#include<cassert>
#include<string>
#include <cctype>
#include <algorithm>
#define TODO assert(0 && "todo")

// #define DEBUG_DFA
// #define DEBUG_SCANNER

std::string frontend::toString(State s) {
    switch (s) {
    case State::Empty: return "Empty";
    case State::Ident: return "Ident";
    case State::IntLiteral: return "IntLiteral";
    case State::FloatLiteral: return "FloatLiteral";
    case State::op: return "op";
    default:
        assert(0 && "invalid State");
    }
    return "";
}

std::set<std::string> frontend::keywords= {
    "const", "int", "float", "if", "else", "while", "continue", "break", "return", "void"
};
// TODO： 仿照keywords定义符号集合
/*******************************************/
std::set<char> frontend::ops = {
    '+', '-', '*', '/', '%', '<', '>', ':', '=', ';', ',', '(', ')', '[', ']', '{', '}', '!', '&', '|'
};

std::set<std::pair<char, char>> frontend::two_ops = {
    {'<', '='},
    {'>', '='},
    {'=', '='},
    {'!', '='},
    {'&', '&'},
    {'|', '|'}
};
namespace frontend {

std::unordered_map<std::string, TokenType> op_type_map = {
    {"+", TokenType::PLUS},
    {"-", TokenType::MINU},
    {"*", TokenType::MULT},
    {"/", TokenType::DIV},
    {"%", TokenType::MOD},
    {"<", TokenType::LSS},
    {">", TokenType::GTR},
    {":", TokenType::COLON},
    {"=", TokenType::ASSIGN},
    {";", TokenType::SEMICN},
    {",", TokenType::COMMA},
    {"(", TokenType::LPARENT},
    {")", TokenType::RPARENT},
    {"[", TokenType::LBRACK},
    {"]", TokenType::RBRACK},
    {"{", TokenType::LBRACE},
    {"}", TokenType::RBRACE},
    {"!", TokenType::NOT},
    {"<=", TokenType::LEQ},
    {">=", TokenType::GEQ},
    {"==", TokenType::EQL},
    {"!=", TokenType::NEQ},
    {"&&", TokenType::AND},
    {"||", TokenType::OR}
};

std::unordered_map<std::string, TokenType> keyword_type_map = {
    {"const", TokenType::CONSTTK},
    {"void", TokenType::VOIDTK},
    {"int", TokenType::INTTK},
    {"float", TokenType::FLOATTK},
    {"if", TokenType::IFTK},
    {"else", TokenType::ELSETK},
    {"while", TokenType::WHILETK},
    {"continue", TokenType::CONTINUETK},
    {"break", TokenType::BREAKTK},
    {"return", TokenType::RETURNTK}
};
} // namespace frontend

//判断操作符
bool  frontend::DFA::isop(char  c)  {
    if (ops.find(c) != ops.end())
        return true;
    else
        return false;
}


//匹配操作符类型
frontend::TokenType frontend::DFA::get_op_type(std::string s) {
    auto it = op_type_map.find(s);
    if (it != op_type_map.end())
        return it->second;
    // 如果无匹配项，可选择返回一个默认值或抛出异常
    throw std::invalid_argument("Unknown operator: " + s);
}

bool frontend::DFA::iskeyword(std::string  s){
    if (keywords.find(s) != keywords.end())
        return true;
    else
        return false;
}

frontend::TokenType frontend::DFA::get_keyword_type(std::string s) {
    auto it = frontend::keyword_type_map.find(s);
    if (it != frontend::keyword_type_map.end()) {
        return it->second;
    }
    return frontend::TokenType::IDENFR;  // 默认不是关键词就是标识符
}


bool frontend::DFA::is_hex(char c) {
    return (('A' <= c && c <= 'F') || ('a' <= c && c <= 'f'));
}



bool frontend::DFA::is_dec(std::string s) {
    if (s.empty()) return false;
    if (s == "0") return true;  // 单个 0 是合法十进制
    if (s[0] != '0') {
        return std::all_of(s.begin(), s.end(), ::isdigit);
    }
    return false;  // 开头是 0 且长度 > 1 不是合法十进制（可能是八进制或非法）
}

bool frontend::DFA::is_num(std::string  s){
    if (s.size() >= 2 && (s[1]!='x' || s[1]!='X'))  return true;
    else if (s.size() == 1 && std::isdigit(s[0]))  return true;
    else return false;
}


bool frontend::DFA::is_two_ops(std::string s, char c) {
    if (s.size() != 1) return false;
    return two_ops.count({s[0], c}) > 0;
}


/*******************************************************************************/


frontend::DFA::DFA(): cur_state(frontend::State::Empty), cur_str() {}

frontend::DFA::~DFA() {}
// 判断是否是空白字符
bool frontend::DFA::is_whitespace(char input) {
    return input == ' ' || input == '\t' || input == '\n' || input == '\r';
}

// reset
void frontend::DFA::reset_state(State new_state, Token& buf) {
    cur_state = new_state;
    buf.value = cur_str;
    cur_str.clear();
}
//next
bool frontend::DFA::next(char input, Token& buf) {
    #ifdef DEBUG_DFA
        std::cout << "in state [" << toString(cur_state) << "], input = \'" << input << "\', str = " << cur_str << "\t";
    #endif

    switch (cur_state) {
        case State::Empty:
            if (is_whitespace(input)) {
                return false;
            } else if (input == '_' || std::isalpha(input)) {
                cur_state = State::Ident;
                cur_str += input;
            } else if (std::isdigit(input)) {
                cur_state = State::IntLiteral;
                cur_str += input;
            } else if (isop(input)) {
                cur_state = State::op;
                cur_str += input;
            } else if (input == '.') {
                cur_state = State::FloatLiteral;
                cur_str += input;
            }
            break;

        case State::Ident:
            if (input == '_' || std::isalpha(input) || std::isdigit(input)) {
                cur_state = State::Ident;
                cur_str += input;
            } else {
                if (iskeyword(cur_str)) {
                    buf.type = get_keyword_type(cur_str);
                } else {
                    buf.type = TokenType::IDENFR;
                }
                buf.value = cur_str;
                
                reset_state(State::Empty, buf);
                if (!is_whitespace(input)) {
                    next(input, buf);
                }
                return true;
            }
            break;

        case State::IntLiteral:
            if (cur_str == "0" && (input == 'x' || input == 'X' || input == 'b' || input == 'B')) {
                cur_str += input;
            } else if ((cur_str[1] == 'x' || cur_str[1] == 'X') && is_hex(input)) {
                cur_str += input;
            } else if (std::isdigit(input)) {
                cur_str += input;
            } else if (input == '.' && is_num(cur_str)) {
                cur_state = State::FloatLiteral;
                cur_str += input;
            } else {
                buf.type = TokenType::INTLTR;
                buf.value = cur_str;

                reset_state(State::Empty, buf);
                if (!is_whitespace(input)) {
                    next(input, buf);
                }
                return true;
            }
            break;

        case State::FloatLiteral:
            if (std::isdigit(input)) {
                cur_str += input;
            } else {
                buf.type = TokenType::FLOATLTR;
                buf.value = cur_str;

                reset_state(State::Empty, buf);
                if (!is_whitespace(input)) {
                    next(input, buf);
                }
                return true;
            }
            break;

        case State::op:
            if (is_two_ops(cur_str, input)) {
                cur_str += input;
            } else {
                buf.type = get_op_type(cur_str);
                buf.value = cur_str;

                reset_state(State::Empty, buf);
                if (!is_whitespace(input)) {
                    next(input, buf);
                }
                return true;
            }
            break;

        default:
            break;
    }

    #ifdef DEBUG_DFA
        std::cout << "next state is [" << toString(cur_state) << "], next str = " << cur_str << std::endl;
    #endif

    return false;
}

void frontend::DFA::reset() {
    cur_state = State::Empty;
    cur_str = "";
}

frontend::Scanner::Scanner(std::string filename): fin(filename) {
    if(!fin.is_open()) {
        assert(0 && "in Scanner constructor, input file cannot open");
    }
}

frontend::Scanner::~Scanner() {
    fin.close();
}
// 处理单行注释，删除注释部分
std::string frontend::Scanner::remove_single_line_comment(std::string& line) {
    size_t pos = line.find("//");
    if (pos != std::string::npos) {
        line = line.substr(0, pos);  // 截取注释前的部分
    }
    return line;
}

// 处理多行注释，删除注释部分
std::string frontend::Scanner::remove_multi_line_comment(std::string &line, bool &inside_comment) {
    size_t pos1 = line.find("/*");
    size_t pos2 = line.find("*/");

    if (inside_comment) {
        if (pos2 != std::string::npos) {
            inside_comment = false;
            line = line.substr(pos2 + 2);
            // 可能同一行中还有代码，需要继续处理
            remove_multi_line_comment(line, inside_comment);
        } else {
            line = "";
        }
    } else if (pos1 != std::string::npos) {
        inside_comment = true;
        std::string before = line.substr(0, pos1);
        std::string after = line.substr(pos1 + 2);
        line = before;
        remove_multi_line_comment(after, inside_comment);
        line += after;
    }
    return line;
}

//TODO：定义运行程序
/*****************************************************/
std::vector<frontend::Token> frontend::Scanner::run() {
    std::vector<frontend::Token> token_stream;  // token串
    std::string str, temp;
    bool inside_comment = false;  // 标记是否在多行注释中

    // 处理文件中的每一行
    while (std::getline(fin, temp)) {
        // 去除单行注释
        temp = remove_single_line_comment(temp);
        
        // 去除多行注释
        temp = remove_multi_line_comment(temp, inside_comment);

        // 只在非注释行中拼接字符串
        if (!temp.empty()) {
            str += temp + "\n";
        }
    }

    // 初始化DFA
    DFA dfa;
    Token tk;

    // 使用DFA处理每个字符并生成Token
    for (size_t i = 0; i < str.size(); i++) {
        if (dfa.next(str[i], tk)) {
            token_stream.push_back(tk);

            #ifdef DEBUG_SCANNER
            #include<iostream>
                std::cout << "token: " << toString(tk.type) << "\t" << tk.value << std::endl;
            #endif
        }
    }

    return token_stream;
}
/*****************************************************/