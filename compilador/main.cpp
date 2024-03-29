#include <iostream>
#include "codegen.h"
#include "node.h"

#define YY_STDINIT

using namespace std;

extern int yyparse();
extern NBlock *programBlock;

int main(int argc, char **argv)
{

    if (argc >= 2)
    {
        freopen(argv[1], "r", stdin);
    }
    else
    {
        freopen("in.c", "r", stdin);
    }
    freopen("salida.py", "w", stdout);

    yyparse();
    std::cerr << programBlock << std::endl;

    CodeGenContext context;
    context.generateCode(*programBlock);

    std::cerr << "------\n\n" << std::endl;

    if (context.errorMessage.size() != 0)
    {
        std::cerr << "Error de compilacion ... " << std::endl;
        for (auto msg : context.errorMessage)
        {
            std::cerr << msg << std::endl;
        }
        fclose(stdin);
        fclose(stdout);
        return -1;
    }
    

    std::cerr << "Codigo compilado satisfactoriamente." << std::endl;
    std::string code = context.outputCode();
    std::cout << code << std::endl;

    fclose(stdin);
    fclose(stdout);
    return 0;
}