#include "node.h"
#include <stack>
#include <sstream>
#include <vector>

std::ostream &operator<<(std::ostream &os, ExprType type);

class NBlock;

struct singleDeclaration
{
    public:
        std::string newName, oldName;
        ExprType type;
        NIdentifier *expValue;
};

class CodeGenContext
{
public:
    int indent;
    std::stringstream code;

    std::vector<std::vector<singleDeclaration>> stack;
    
    std::vector<std::string> errorMessage;

    std::vector<std::string> funcDeclaration;

public:
    CodeGenContext()
    {
        indent = -1;
    }
    
    singleDeclaration* findDeclaration(std::string name, int type);
    int findLayerDeclaration(std::string name);

    void generateCode(NBlock &root);
    std::string outputCode();
};