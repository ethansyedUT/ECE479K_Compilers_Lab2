/*
 This is the Lab1 skeleton cgen header. As given, it contains only basic
 functionality. You will need to add members to each of the classes
 to get them to perform their desired functions. Document your important
 design decisions below. We should be able to read your documentation and
 get a general overview of how your compiler generates code. For instance,
 how does your compiler generate structures for classes, how is inheritance
 modeled, how do you handle dynamic binding, etc.
*/

// ------------------ INSERT DESIGN DOCUMENTATION HERE --------------------- //

// ----------------------------- END DESIGN DOCS --------------------------- //

#include "cool_tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "value_printer.h"
#include <unordered_set>
#include <map>

// ---------------- My Classes ---------------- //
class method_mine
{
public:
  method_mine() = default;
  method_mine(Symbol nm, op_type rt_type, std::vector<operand> parameters, Expression express)
      : name(nm), returnType(rt_type), params(parameters), expr(express) {}
  Symbol get_name() { return name; }
  op_type get_return_type() { return returnType; }
  std::vector<operand> get_params() { return params; }
  Expression get_expression() { return expr; }
  void set_class_origin(CgenNode *n) { class_origin = n; }
  CgenNode *get_class_origin() { return class_origin; }

private:
  Symbol name;
  op_type returnType;
  std::vector<operand> params;
  Expression expr;

  CgenNode *class_origin;
};

class attribute_mine
{
public:
  attribute_mine() = default;
  attribute_mine(Symbol nm, op_type tp, Expression init)
      : name(nm), type(tp), init_val(init) {}
  Symbol get_name() { return name; }
  op_type get_return_type() { return type; }
  Expression get_init_expr() { return init_val; }
  void set_name(std::string nm)
  {
    Entry *hold = new Entry(nm, name->get_index());
    name = hold;
  }

private:
  Symbol name;
  op_type type;
  Expression init_val;
};

class CgenNode;

// CgenClassTable represents the top level of a Cool program, which is
// basically a list of classes. The class table is used to look up classes
// (CgenNodes) by name, and it also handles global code generation tasks.
// The CgenClassTable constructor is where you'll find the entry point for
// code generation for an entire Cool program.
class CgenClassTable : public cool::SymbolTable<CgenNode>
{
public:
  // CgenClassTable constructor begins and ends the code generation process
  CgenClassTable(Classes, std::ostream &str);

private:
  // The following creates an inheritance graph from a list of classes.
  // The graph is implemented as a tree of `CgenNode', and class names
  // are placed in the base class symbol table.
  void install_basic_classes();
  void install_classes(Classes cs);
  void install_class(CgenNode *nd);
  void install_special_class(CgenNode *nd);
  void build_inheritance_tree();
  void set_relations(CgenNode *nd);
  // Create declarations for C runtime functions we need to generate code
  void setup_external_functions();
  void setup_classes(CgenNode *c, int depth);

  // TODO: implement the following functions.
  std::vector<CgenNode *> getNds() { return nds; }
  // Setup each class in the table and prepare for code generation phase
  void setup();
  // Code generation functions. You need to write these functions.
  void code_module();
#ifdef LAB2
  void code_classes(CgenNode *c);
#endif
  void code_constants();
  void code_main();

  /* Util functions */
#ifndef LAB2
  CgenNode *getMainmain(CgenNode *c);
#endif
  CgenNode *root(); // Get the root of the class Tree, i.e. Object
public:
  int get_num_classes() const { return current_tag; }
  std::string get_const_name(std::string prefix, bool incr)
  {
    std::string suffix = std::to_string(constant_counter);
    constant_counter += incr;
    return prefix + suffix;
  }
  std::vector<CgenNode *> get_classes() { return nds; }
  std::vector<CgenNode *> get_special_classes() { return special_nds; }
  void add_constant(std::string global_name, std::string local_name)
  {
    const_strings.emplace(local_name, global_name);
  }
  std::string *find_const(std::string local_name)
  {
    auto find = const_strings.find(local_name);
    return find == const_strings.end() ? nullptr : &find->second;
  }

private:
  // Class lists and current class tag
  std::vector<CgenNode *> nds, special_nds;
  int current_tag;
  int constant_counter;

  // holds constant strings
  std::unordered_map<std::string, std::string> const_strings;

public:
  // The ostream where we are emitting code
  std::ostream *ct_stream;
};

// Each CgenNode corresponds to a Cool class. As such, it is responsible for
// performing code generation on the class level. This includes laying out
// the class attributes, creating the necessary Types for the class and
// generating code for each of its methods.
class CgenNode : public class__class
{
public:
  enum Basicness
  {
    Basic,
    NotBasic
  };
  CgenNode(Class_ c, Basicness bstatus, CgenClassTable *class_table)
      : class__class((const class__class &)*c), parentnd(0), children(0),
        basic_status(bstatus), class_table(class_table), tag(-1),
        ct_stream(class_table->ct_stream) {}

  // Relationships with other nodes in the tree
  void set_parent(CgenNode *p)
  {
    assert(this->parentnd == nullptr && p != nullptr);
    p->children.push_back(this);
    this->parentnd = p;
  }
  int basic() { return basic_status == Basic; }
  std::vector<CgenNode *> &get_children() { return children; }
  void set_max_child(int mc) { max_child = mc; }
  int get_max_child() const { return max_child; }

  // Accessors for other provided fields
  int get_tag() const { return tag; }
  CgenClassTable *get_classtable() { return class_table; }

#ifdef LAB2
  std::string get_type_name() { return name->get_string(); }
  std::string get_vtable_type_name()
  {
    return "_" + get_type_name() + "_vtable";
  }
  std::string get_vtable_name()
  {
    return "_" + get_type_name() + "_vtable_prototype";
  }
  std::string get_init_function_name() { return get_type_name() + "_new"; }
#endif

  // TODO: Complete the implementations of following functions
  // and add more as necessary
  void add_parent_attributes_methods(std::vector<op_type> &attributes, std::vector<op_type> &methods, std::vector<const_value> &init_values);
  void recurse_attributes_methods(CgenNode *cls, std::vector<op_type> &attributes, std::vector<op_type> &methods, std::vector<const_value> &init_values, std::unordered_set<std::string> &attributesSeen, std::unordered_set<std::string> &methodsSeen);
  std::string strip_method_name(std::string method_name);
  method_mine *find_method_class_init(std::string method_name);
  // Class setup. You need to write the body of this function.
  void setup(int tag, int depth);
#ifdef LAB2
  // Layout the methods and attributes for code generation
  void layout_features();
  // Class codegen. You need to write the body of this function.
  void code_class();
  // Codegen for the init function of every class
  void code_init_function(CgenEnvironment *env);
#endif
  void codeGenMainmain();

  // My Methods
  CgenNode *get_parentNode() { return parentnd; }
  std::ostream *get_ostream() { return ct_stream; }
  void add_method(Symbol nm, op_type rt, std::vector<operand> parameters, Expression expr)
  {
    method_mine *temp = new method_mine(nm, rt, parameters, expr);
    methods_in_class.emplace_back(temp);
  }
  void add_attribute(Symbol nm, op_type type, Expression init)
  {
    attribute_mine *temp = new attribute_mine(nm, type, init);
    attributes_in_class.emplace_back(temp);
  }

  std::vector<method_mine *> get_methods_in_class() { return methods_in_class; }
  std::vector<attribute_mine *> get_attributes_in_class() { return attributes_in_class; }

private:
  CgenNode *parentnd;               // Parent of class
  std::vector<CgenNode *> children; // Children of class
  Basicness basic_status;           // `Basic' or 'NotBasic'
  CgenClassTable *class_table;
  // Class tag. Should be unique for each class in the tree
  int tag, max_child;
  std::ostream *ct_stream;

  // TODO: Add more functions / fields here as necessary.

  // My fields
  std::vector<method_mine *> methods_in_class;
  std::vector<attribute_mine *> attributes_in_class;

  //  // Add DS id_table for each class to ensure non-dual func redef?
};

// CgenEnvironment provides the environment for code generation of a method.
// Its main task is to provide a mapping from Cool names to LLVM Values.
// This mapping needs to be maintained as scopes are entered and exited, new
// variables are declared, and so on. CgenEnvironment is also a good place
// to put non-local information you will need during code generation. Two
// examples are the current CgenNode and the current Function.
class CgenEnvironment
{
public:
  // Class CgenEnvironment should be constructed by a class prior to code
  // generation for each method. You may need to add parameters to this
  // constructor.
  CgenEnvironment(std::ostream &stream, CgenNode *cur_class)
      : var_table(), cur_class(cur_class), block_count(0), tmp_count(0),
        ok_count(0), cur_stream(&stream)
  {
    // var_table.enterscope();
    //  TODO: Walk here for variables???
  }

  // fresh name generation functions
  std::string new_name() { return "tmp." + std::to_string(tmp_count++); }
  std::string new_ok_label() { return "ok." + std::to_string(ok_count++); }
  std::string new_label(const std::string &prefix, bool increment)
  {
    std::string suffix = std::to_string(block_count);
    block_count += increment;
    return prefix + suffix;
  }

  CgenNode *get_class() { return cur_class; }
  void set_class(CgenNode *c) { cur_class = c; }

  // Must return the CgenNode for a class given the symbol of its name
  CgenNode *type_to_class(Symbol t);

  operand *find_in_scopes(Symbol name)
  {
    return var_table.find_in_scopes(name);
  }
  void add_binding(Symbol name, operand *op) { var_table.insert(name, op); }
  void open_scope() { var_table.enterscope(); }
  void close_scope() { var_table.exitscope(); }

  // TODO: Add more functions as necessary.
  void dump_vartable()
  {
    var_table.dump(*cur_stream);
  }
  void insert_scoped(std::string key, operand *value)
  {
    scoped.emplace(key, value);
  }
  operand *find_scoped(std::string key)
  {
    auto it = scoped.find(key);
    if (it != scoped.end())
    {
      // Key found, return the corresponding value
      return it->second;
    }
    else
    {
      // Key not found, return a default-constructed Symbol
      return nullptr;
    }
  }

private:
  cool::SymbolTable<operand>
      var_table; // mapping from variable names to memory locations
  CgenNode *cur_class;
  int block_count, tmp_count, ok_count; // Keep counters for unique name
                                        // generation in the current method

  // personal ds to find self
  std::unordered_map<std::string, operand *> scoped;

public:
  std::ostream *cur_stream;
};

#ifdef LAB2
// TODO: implement these functions (LAB2), and add more functions as necessary

// Utitlity function
// Generate any code necessary to convert from given operand to
// dest_type, assuming it has already been checked to be compatible
operand conform(operand src, op_type dest_type, CgenEnvironment *env);
#endif
