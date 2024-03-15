#include "cool_tree.h"
#include "value_printer.h"

// custom classes (debugging)
class testField
{
    // TODO: Add operand support
protected:
    // variables
    std::string name;
    op_type *opType = nullptr;
    operand *oper = nullptr;
    ValuePrinter *valPrint = nullptr;

public:
    // constructors
    testField(std::string nm, op_type *optype)
    {
        name = nm;
        opType = optype;
    };
    testField(operand *operandd)
    {
        name = operandd->get_name();
        oper = operandd;
    };
    testField(std::string title)
    {
        name = title;
    }

    // functions
    friend std::ostream &
    operator<<(std::ostream &os, const testField obj)
    {
        if (!obj.oper && !obj.opType)
        {
            os << "\n"
               << "============== " << obj.name << " ==============" << std::endl;
        }
        else
        {
            if (obj.oper)
            {
                os << "Name: " << obj.name << "\t|\t"
                   << "Type: " << obj.oper->get_typename();
            }
            if (obj.opType)
            {
                os << "Name: " << obj.name << "\t|\t"
                   << "IR: " << obj.opType->get_name();
            }
        }
        return os;
    }
};

// my global function definitions
void value_printer_tester(ValuePrinter vp, std::vector<testField> operandsToTest, std::vector<testField> op_typeToTest);

/* Test Code - Use in cgen.cc */

// In lab1 repo - ethansyedUT