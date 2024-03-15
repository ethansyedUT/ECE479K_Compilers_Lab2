/*********************************************************************
 Intermediate code generator for COOL: SKELETON

 Read the comments carefully and add code to build an LLVM program
*********************************************************************/

#define EXTERN
#include "cgen.h"
#include <fstream>
#include <sstream>
#include <string>

extern int cgen_debug, curr_lineno;

/*********************************************************************
 For convenience, a large number of symbols are predefined here.
 These symbols include the primitive type and method names, as well
 as fixed names used by the runtime system. Feel free to add your
 own definitions as you see fit.
*********************************************************************/
EXTERN Symbol
    // required classes
    Object,
    IO, String, Int, Bool, Main,

    // class methods
    cool_abort, type_name, cool_copy, out_string, out_int, in_string, in_int,
    length, concat, substr,

    // class members
    val,

    // special symbols
    No_class,  // symbol that can't be the name of any user-defined class
    No_type,   // If e : No_type, then no code is generated for e.
    SELF_TYPE, // Special code is generated for new SELF_TYPE.
    self,      // self generates code differently than other references

    // extras
    arg, arg2, newobj, Mainmain, prim_string, prim_int, prim_bool;

// Initializing the predefined symbols.
static void initialize_constants(void)
{
  Object = idtable.add_string("Object");
  IO = idtable.add_string("IO");
  String = idtable.add_string("String");
  Int = idtable.add_string("Int");
  Bool = idtable.add_string("Bool");
  Main = idtable.add_string("Main");

  cool_abort = idtable.add_string("abort");
  type_name = idtable.add_string("type_name");
  cool_copy = idtable.add_string("copy");
  out_string = idtable.add_string("out_string");
  out_int = idtable.add_string("out_int");
  in_string = idtable.add_string("in_string");
  in_int = idtable.add_string("in_int");
  length = idtable.add_string("length");
  concat = idtable.add_string("concat");
  substr = idtable.add_string("substr");

  val = idtable.add_string("val");

  No_class = idtable.add_string("_no_class");
  No_type = idtable.add_string("_no_type");
  SELF_TYPE = idtable.add_string("SELF_TYPE");
  self = idtable.add_string("self");

  arg = idtable.add_string("arg");
  arg2 = idtable.add_string("arg2");
  newobj = idtable.add_string("_newobj");
  Mainmain = idtable.add_string("main");
  prim_string = idtable.add_string("sbyte*");
  prim_int = idtable.add_string("int");
  prim_bool = idtable.add_string("bool");
}

/*********************************************************************

  CgenClassTable methods

*********************************************************************/

// CgenClassTable constructor orchestrates all code generation
CgenClassTable::CgenClassTable(Classes classes, std::ostream &s)
    : nds(0), current_tag(0)
{
  if (cgen_debug)
    std::cerr << "Building CgenClassTable" << std::endl;
  ct_stream = &s;
  // Make sure we have a scope, both for classes and for constants
  enterscope();

  // Create an inheritance tree with one CgenNode per class.
  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();

  // First pass
  setup();

  // Second pass
  code_module();
  // Done with code generation: exit scopes
  exitscope();
}

// Creates AST nodes for the basic classes and installs them in the class list
void CgenClassTable::install_basic_classes()
{
  // The tree package uses these globals to annotate the classes built below.
  curr_lineno = 0;
  Symbol filename = stringtable.add_string("<basic class>");

  //
  // A few special class names are installed in the lookup table but not
  // the class list. Thus, these classes exist, but are not part of the
  // inheritance hierarchy.

  // No_class serves as the parent of Object and the other special classes.
  Class_ noclasscls = class_(No_class, No_class, nil_Features(), filename);
  install_special_class(new CgenNode(noclasscls, CgenNode::Basic, this));
  delete noclasscls;

#ifdef LAB2
  // SELF_TYPE is the self class; it cannot be redefined or inherited.
  Class_ selftypecls = class_(SELF_TYPE, No_class, nil_Features(), filename);
  install_special_class(new CgenNode(selftypecls, CgenNode::Basic, this));
  delete selftypecls;
  //
  // Primitive types masquerading as classes. This is done so we can
  // get the necessary Symbols for the innards of String, Int, and Bool
  //
  Class_ primstringcls =
      class_(prim_string, No_class, nil_Features(), filename);
  install_special_class(new CgenNode(primstringcls, CgenNode::Basic, this));
  delete primstringcls;
#endif
  Class_ primintcls = class_(prim_int, No_class, nil_Features(), filename);
  install_special_class(new CgenNode(primintcls, CgenNode::Basic, this));
  delete primintcls;
  Class_ primboolcls = class_(prim_bool, No_class, nil_Features(), filename);
  install_special_class(new CgenNode(primboolcls, CgenNode::Basic, this));
  delete primboolcls;
  //
  // The Object class has no parent class. Its methods are
  //    cool_abort() : Object    aborts the program
  //    type_name() : Str        returns a string representation of class name
  //    copy() : SELF_TYPE       returns a copy of the object
  //
  // There is no need for method bodies in the basic classes---these
  // are already built in to the runtime system.
  //
  Class_ objcls = class_(
      Object, No_class,
      append_Features(
          append_Features(single_Features(method(cool_abort, nil_Formals(),
                                                 Object, no_expr())),
                          single_Features(method(type_name, nil_Formals(),
                                                 String, no_expr()))),
          single_Features(
              method(cool_copy, nil_Formals(), SELF_TYPE, no_expr()))),
      filename);
  install_class(new CgenNode(objcls, CgenNode::Basic, this));
  delete objcls;

  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer.
  //
  Class_ intcls = class_(
      Int, Object, single_Features(attr(val, prim_int, no_expr())), filename);
  install_class(new CgenNode(intcls, CgenNode::Basic, this));
  delete intcls;

  //
  // Bool also has only the "val" slot.
  //
  Class_ boolcls = class_(
      Bool, Object, single_Features(attr(val, prim_bool, no_expr())), filename);
  install_class(new CgenNode(boolcls, CgenNode::Basic, this));
  delete boolcls;

#ifdef LAB2
  //
  // The class String has a number of slots and operations:
  //       val                                  the string itself
  //       length() : Int                       length of the string
  //       concat(arg: Str) : Str               string concatenation
  //       substr(arg: Int, arg2: Int): Str     substring
  //
  Class_ stringcls =
      class_(String, Object,
             append_Features(
                 append_Features(
                     append_Features(
                         single_Features(attr(val, prim_string, no_expr())),
                         single_Features(
                             method(length, nil_Formals(), Int, no_expr()))),
                     single_Features(method(concat,
                                            single_Formals(formal(arg, String)),
                                            String, no_expr()))),
                 single_Features(
                     method(substr,
                            append_Formals(single_Formals(formal(arg, Int)),
                                           single_Formals(formal(arg2, Int))),
                            String, no_expr()))),
             filename);
  install_class(new CgenNode(stringcls, CgenNode::Basic, this));
  delete stringcls;
#endif

#ifdef LAB2
  //
  // The IO class inherits from Object. Its methods are
  //        out_string(Str) : SELF_TYPE          writes a string to the output
  //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
  //        in_string() : Str                    reads a string from the input
  //        in_int() : Int                         "   an int     "  "     "
  //
  Class_ iocls = class_(
      IO, Object,
      append_Features(
          append_Features(
              append_Features(
                  single_Features(method(out_string,
                                         single_Formals(formal(arg, String)),
                                         SELF_TYPE, no_expr())),
                  single_Features(method(out_int,
                                         single_Formals(formal(arg, Int)),
                                         SELF_TYPE, no_expr()))),
              single_Features(
                  method(in_string, nil_Formals(), String, no_expr()))),
          single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
      filename);
  install_class(new CgenNode(iocls, CgenNode::Basic, this));
  delete iocls;
#endif
}

// install_classes enters a list of classes in the symbol table.
void CgenClassTable::install_classes(Classes cs)
{
  for (auto cls : cs)
  {
    install_class(new CgenNode(cls, CgenNode::NotBasic, this));
  }
}

// Add this CgenNode to the class list and the lookup table
void CgenClassTable::install_class(CgenNode *nd)
{
  Symbol name = nd->get_name();
  if (!this->find(name))
  {
    // The class name is legal, so add it to the list of classes
    // and the symbol table.
    nds.push_back(nd);
    this->insert(name, nd);
  }
}

// Add this CgenNode to the special class list and the lookup table
void CgenClassTable::install_special_class(CgenNode *nd)
{
  Symbol name = nd->get_name();
  if (!this->find(name))
  {
    // The class name is legal, so add it to the list of special classes
    // and the symbol table.
    special_nds.push_back(nd);
    this->insert(name, nd);
  }
}

// CgenClassTable::build_inheritance_tree
void CgenClassTable::build_inheritance_tree()
{
  for (auto node : nds)
    set_relations(node);
}

// CgenClassTable::set_relations
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table. Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNode *nd)
{
  Symbol parent = nd->get_parent();
  auto parent_node = this->find(parent);
  if (!parent_node)
  {
    throw std::runtime_error("Class " + nd->get_name()->get_string() +
                             " inherits from an undefined class " +
                             parent->get_string());
  }
  nd->set_parent(parent_node);
}

// Sets up declarations for extra functions needed for code generation
// You should not need to modify this code for Lab1
void CgenClassTable::setup_external_functions()
{
  ValuePrinter vp(*ct_stream);
  // setup function: external int strcmp(sbyte*, sbyte*)
  op_type i32_type(INT32), i8ptr_type(INT8_PTR), vararg_type(VAR_ARG);
  std::vector<op_type> strcmp_args;
  strcmp_args.push_back(i8ptr_type);
  strcmp_args.push_back(i8ptr_type);
  vp.declare(*ct_stream, i32_type, "strcmp", strcmp_args);

  // setup function: external int printf(sbyte*, ...)
  std::vector<op_type> printf_args;
  printf_args.push_back(i8ptr_type);
  printf_args.push_back(vararg_type);
  vp.declare(*ct_stream, i32_type, "printf", printf_args);

  // setup function: external void abort(void)
  op_type void_type(VOID);
  std::vector<op_type> abort_args;
  vp.declare(*ct_stream, void_type, "abort", abort_args);

  // setup function: external i8* malloc(i32)
  std::vector<op_type> malloc_args;
  malloc_args.push_back(i32_type);
  vp.declare(*ct_stream, i8ptr_type, "malloc", malloc_args);

#ifdef LAB2
  // TODO: add code here
#endif
}

void CgenClassTable::setup_classes(CgenNode *c, int depth)
{
  c->setup(current_tag++, depth);
  for (auto child : c->get_children())
  {
    setup_classes(child, depth + 1);
  }
  c->set_max_child(current_tag - 1);
}

// The code generation first pass. Define these two functions to traverse
// the tree and setup each CgenNode
void CgenClassTable::setup()
{
  setup_external_functions();
  setup_classes(root(), 0);
}

// The code generation second pass. Add code here to traverse the tree and
// emit code for each CgenNode
// TODO: MAIN ENTRY POINT
void CgenClassTable::code_module()
{
  code_constants();

#ifndef LAB2
  // This must be after code_constants() since that emits constants
  // needed by the code() method for expressions
  CgenNode *mainNode = getMainmain(root());
  mainNode->codeGenMainmain();
#endif
  code_main();

#ifdef LAB2
  code_classes(root());
#endif
}

#ifdef LAB2
void CgenClassTable::code_classes(CgenNode *c)
{
  // TODO: add code here
}
#endif

// Create global definitions for constant Cool objects
void CgenClassTable::code_constants()
{
#ifdef LAB2
  // TODO: add code here
#endif
}

// Create LLVM entry point. This function will initiate our Cool program
// by generating the code to execute (new Main).main()
//
void CgenClassTable::code_main()
{
  // TODO: add code here

  // Define a function main that has no parameters and returns an i32
  std::string mainName = "main";

  std::vector<operand> non;
  std::vector<op_type> null;
  int_value zero = int_value(0);
  ValuePrinter vp(*ct_stream);

  global_value cv = global_value(op_arr_ptr_type(INT8, 25), ".str");

  const_value str = const_value(op_arr_type(INT8, 25), "Main.main() returned %d\n", true);
  vp.init_constant(".str", str);
  vp.define(op_type(INT32), "main", non);

  // Define an entry basic block
  vp.begin_block("entry");

  // Call Main_main(). This returns int for phase 1, Object for phase 2
  operand ret = vp.call(null, op_type(INT32), "Main_main", true, non);

#ifdef LAB2
// LAB2
#else
  // Lab1
  // Get the address of the string "Main_main() returned %d\n" using "getelementptr"
  operand arg1 = vp.getelementptr(op_arr_type(INT8, 25), cv, zero, zero, op_type(INT8_PTR));
  // Call printf with the string address of "Main_main() returned %d\n"
  // and the return value of Main_main() as its arguments
  std::vector<op_type> argTypes;
  argTypes.push_back(op_type(INT8_PTR));

  std::vector<operand> args;
  args.push_back(arg1);
  args.push_back(ret);

  vp.call_variadic(argTypes, op_type(INT32), "printf", true, args);

  vp.ret(zero);
  vp.end_define();
#endif
}

// Get the root of the class tree.
CgenNode *CgenClassTable::root()
{
  auto root = this->find(Object);
  if (!root)
  {
    throw std::runtime_error("Class Object is not defined.");
  }
  return root;
}

#ifndef LAB2
// Special-case functions used for the method Int Main::main() for
// Lab1 only.
CgenNode *CgenClassTable::getMainmain(CgenNode *c)
{
  if (c && !c->basic())
    return c; // Found it!
  for (auto child : c->get_children())
  {
    if (CgenNode *foundMain = this->getMainmain(child))
      return foundMain; // Propagate it up the recursive calls
  }
  return 0; // Make the recursion continue
}
#endif

/*********************************************************************

  StrTable / IntTable methods

 Coding string, int, and boolean constants

 Cool has three kinds of constants: strings, ints, and booleans.
 This section defines code generation for each type.

 All string constants are listed in the global "stringtable" and have
 type stringEntry. stringEntry methods are defined both for string
 constant definitions and references.

 All integer constants are listed in the global "inttable" and have
 type IntEntry. IntEntry methods are defined for Int constant references only.

 Since there are only two Bool values, there is no need for a table.
 The two booleans are represented by instances of the class BoolConst,
 which defines the definition and reference methods for Bools.

*********************************************************************/

// Create definitions for all String constants
void StrTable::code_string_table(std::ostream &s, CgenClassTable *ct)
{
  for (auto &[_, entry] : this->_table)
  {
    entry.code_def(s, ct);
  }
}

// generate code to define a global string constant
void StringEntry::code_def(std::ostream &s, CgenClassTable *ct)
{
#ifdef LAB2
  // TODO: add code here
#endif
}

/*********************************************************************

  CgenNode methods

*********************************************************************/

//
// Class setup. You may need to add parameters to this function so that
// the classtable can provide setup information (such as the class tag
// that should be used by this class).
//
// Things that setup should do:
//  - layout the features of the class
//  - create the types for the class and its vtable
//  - create global definitions used by the class such as the class vtable
//
void CgenNode::setup(int tag, int depth)
{
  this->tag = tag;
#ifdef LAB2
  layout_features();

  // TODO: add code here

#endif
}

#ifdef LAB2
// Laying out the features involves creating a Function for each method
// and assigning each attribute a slot in the class structure.
void CgenNode::layout_features()
{
  // TODO: add code here
}

// Class codegen. This should performed after every class has been setup.
// Generate code for each method of the class.
void CgenNode::code_class()
{
  // No code generation for basic classes. The runtime will handle that.
  if (basic())
  {
    return;
  }
  // TODO: add code here
}

void CgenNode::code_init_function(CgenEnvironment *env)
{
  // TODO: add code here
}

#else

// code-gen function main() in class Main
void CgenNode::codeGenMainmain()
{
  // In Phase 1, this can only be class Main. Get method_class for main().
  assert(std::string(this->name->get_string()) == std::string("Main"));
  method_class *mainMethod = (method_class *)features->nth(features->first());

  // TODO: add code here to generate the function `int Mainmain()`.
  // Generally what you need to do are:
  // -- setup or create the environment, env, for translating this method
  // -- invoke mainMethod->code(env) to translate the method
  CgenEnvironment env = CgenEnvironment(*ct_stream, this);

  mainMethod->code(&env);
}

#endif

/*********************************************************************

  CgenEnvironment functions

*********************************************************************/

// Look up a CgenNode given a symbol
CgenNode *CgenEnvironment::type_to_class(Symbol t)
{
  return t == SELF_TYPE ? get_class()
                        : get_class()->get_classtable()->find_in_scopes(t);
}

/*********************************************************************

  APS class methods

    Fill in the following methods to produce code for the
    appropriate expression. You may add or remove parameters
    as you wish, but if you do, remember to change the parameters
    of the declarations in `cool-tree.handcode.h'.

*********************************************************************/

void program_class::cgen(const std::optional<std::string> &outfile)
{
  initialize_constants();
  if (outfile)
  {
    std::ofstream s(*outfile);
    if (!s.good())
    {
      std::cerr << "Cannot open output file " << *outfile << std::endl;
      exit(1);
    }
    class_table = new CgenClassTable(classes, s);
  }
  else
  {
    class_table = new CgenClassTable(classes, std::cout);
  }
}

// Create a method body
void method_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
  {
    std::cerr << "method" << std::endl;
  }

  ValuePrinter vp(*env->cur_stream);

  std::vector<operand> null;
  vp.define(op_type(INT32), "Main_main", null);

  // change in later labs
  //  vp.begin_block(name->get_string());
  vp.begin_block("entry");

  // code() called with method's expression
  operand finalExpression = expr->code(env);
  vp.ret(finalExpression);

  vp.end_define();
}

void code_helper()
{
  // use this for method_class::code() in future labs
  // recurse through the tree here and call based on cgen node type
}

// Codegen for expressions. Note that each expression has a value.

operand assign_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "assign" << std::endl;

  ValuePrinter vp(*env->cur_stream);
  // TODO: add code here and replace `return operand()`
  // gaming time

  // get ptr to name
  // resolve the expression and store in temp
  // store the temp into the ptr to the name
  // return the value of the resolved expr

  // Find symbol within env table
  operand findSym = *env->find_in_scopes(name);
  operand store = vp.getelementptr(findSym.get_type().get_deref_type(), findSym, int_value(0), findSym.get_type());
  operand value = expr->code(env);
  vp.store(value, store);

  return value;
}

operand cond_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
  {
    std::cerr << "cond" << std::endl;
  }

  // resolve predicate
  // if predicate
  // then resolve then_exp
  // else resolve else_exp

  ValuePrinter vp(*env->cur_stream);

  // multiple llvm basic blocks needed

  // allocate stack room for the ret val (check if type int or bool)

  // first resolve the predicate to a operand / virtual reg (predResult)
  // predResult -> vp.branch

  // generate a new label & declare true basic block
  // create the true basic block with then_expression

  // generate a new label & declare false basic block
  // create the false basic block with else_expression

  // generate a end/return block

  // QUESTION: How to use select??

  std::string typeCheck = then_exp->get_type()->get_string();
  operand retVal;
  if (typeCheck == "Int")
    retVal = vp.alloca_mem(op_type(INT32));
  else if (typeCheck == "Bool")
    retVal = vp.alloca_mem(op_type(INT1));

  // Do I need to type check here? (I NEED TO FOR LAB2)
  operand branchOp = pred->code(env);

  // branch names
  std::string btrue = env->new_label("btrue", false);
  std::string bfalse = env->new_label("bfalse", false);
  std::string bEnd = env->new_label("end", true);

  operand bResult;

  vp.branch_cond(branchOp, btrue, bfalse);

  vp.begin_block(btrue);
  bResult = then_exp->code(env);
  vp.store(bResult, retVal);
  vp.branch_uncond(bEnd);

  vp.begin_block(bfalse);
  bResult = else_exp->code(env);
  vp.store(bResult, retVal);
  vp.branch_uncond(bEnd);

  vp.begin_block(bEnd);
  retVal = vp.load(retVal.get_type().get_deref_type(), retVal);

  return retVal;
}

operand loop_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "loop" << std::endl;

  // gaming time

  // resolve the predicate
  // if true ; resolve body
  // else ; terminate
  ValuePrinter vp(*env->cur_stream);
  // more basic blocks

  // should always be true or false
  // QUESTION: Do I have to type check here? (LAB 2 probably)
  // std::cerr << "LOOP IDENTIFIER : " << predResult.get_type().is_bool_object() << std::endl;

  std::string loopName = env->new_label("loop", false);
  std::string bodyName = env->new_label("body", false);
  std::string exit = env->new_label("exit", true);
  // emit IR to move prog to first loop block
  vp.branch_uncond(loopName);

  vp.begin_block(loopName);
  operand predResult = pred->code(env);
  vp.branch_cond(predResult, bodyName, exit);

  vp.begin_block(bodyName);
  body->code(env);
  vp.branch_uncond(loopName);

  vp.begin_block(exit);

  // loop should return int 0 (LAB 1)
  return int_value(0);
}

operand block_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "block" << std::endl;

  // create a block_class operand iterate through Expressions field and call respective code funcs
  // save last expression to serve as return for block
  operand lastExpression;
  for (int i = body->first(); body->more(i); i = body->next(i))
  {
    // std::cerr << body->nth(i)->get_line_number() << std::endl;
    lastExpression = body->nth(i)->code(env);
  }

  return lastExpression;
}

operand let_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "let" << std::endl;

  // TODO: Test more thoroughly / particularly add_bindings may be incorrect

  // resolve all expressions / do assignments
  ValuePrinter vp(*env->cur_stream);

  // open scope
  env->open_scope();

  operand opToStore;
  // check type Int or Bool
  // allocate space for expr
  // might be bypassing call to int_alloca & bool_alloca
  if (type_decl->get_string() == "Int")
  {
    opToStore = vp.alloca_mem(op_type(INT32));
  }
  else if (type_decl->get_string() == "Bool")
  {
    opToStore = vp.alloca_mem(op_type(INT1));
  }

  // very suboptimal
  // ask TA or Dr.Erez how to optimize
  //======================
  // resolve expr
  // if no_expr assign defaults
  operand value = init->code(env);
  if (value.is_empty())
  {
    // if no_expr, assign default vals based on type
    if (type_decl->get_string() == "Int")
      value = int_value(0);
    else if (type_decl->get_string() == "Bool")
      value = bool_value(false, true);
  }

  vp.store(value, opToStore);

  // add binding to symbol table
  // QUESTION: Am I creating bindings correctly??
  env->add_binding(identifier, &opToStore);

  // generate ir for body and store last known result
  operand ret = body->code(env);

  // close scope
  env->close_scope();

  // return result of body expr
  return ret;
}

operand plus_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "plus" << std::endl;

  ValuePrinter vp(*env->cur_stream);
  return vp.add(e1->code(env), e2->code(env));
}

operand sub_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "sub" << std::endl;

  ValuePrinter vp(*env->cur_stream);
  return vp.sub(e1->code(env), e2->code(env));
}

operand mul_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "mul" << std::endl;

  ValuePrinter vp(*env->cur_stream);
  return vp.mul(e1->code(env), e2->code(env));
}

operand divide_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "div" << std::endl;

  std::vector<op_type> empty;
  std::vector<operand> nun;

  ValuePrinter vp(*env->cur_stream);

  // divide by 0 check
  operand divResult = e2->code(env);
  operand retVal;

  if (divResult.get_name() == "0")
  {
    // compile check for constant div by 0
    std::vector<op_type> empty;
    std::vector<operand> nun;
    vp.call(empty, op_type(VOID), "abort", true, nun);
    retVal = int_value(0);
  }
  else
  {
    retVal = vp.div(e1->code(env), divResult);
  }

  // // divide by zero (runtime error) check
  // std::string trueLabel = env->new_label("divby0_", false);
  // std::string falseLabel = env->new_label("cont", true);
  // operand divZero = vp.icmp(EQ, divResult, int_value(0));
  // vp.branch_cond(divZero, trueLabel, falseLabel);

  // vp.begin_block(trueLabel);
  // vp.call(empty, op_type(VOID), "abort", true, nun);

  // vp.begin_block(falseLabel);

  return retVal;
}

operand neg_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "neg" << std::endl;

  // only ever fed INT type (LAB1)
  ValuePrinter vp(*env->cur_stream);
  // resolve to operand-> get value and inverse-> return int const
  // could "simplify"; harder to read
  operand hold = e1->code(env);
  int inv = std::stoi(hold.get_name()) * -1;
  return int_value(inv);
}

operand lt_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "lt" << std::endl;

  ValuePrinter vp(*env->cur_stream);

  return vp.icmp(LT, e1->code(env), e2->code(env));
}

operand eq_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "eq" << std::endl;

  ValuePrinter vp(*env->cur_stream);

  return vp.icmp(EQ, e1->code(env), e2->code(env));
}

operand leq_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "leq" << std::endl;

  ValuePrinter vp(*env->cur_stream);

  return vp.icmp(LE, e1->code(env), e2->code(env));
}

operand comp_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
  {
    std::cerr << "complement" << std::endl;
    std::cerr << e1->code(env).get_name() << " : init bool" << std::endl;
  }

  // Should bools be internal??

  // only ever fed bool (Lab 1)
  operand hold = e1->code(env);
  if (hold.get_name() == "true")
  {
    // if true, return false const
    return bool_value(false, true);
  }

  // if false, return true const
  return bool_value(true, true);
}

operand int_const_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "Integer Constant" << std::endl;

  ValuePrinter vp(*env->cur_stream);

  int_value intConst = int_value(std::stol(token->get_string()));
  return intConst;
}

operand bool_const_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "Boolean Constant" << std::endl;

  ValuePrinter vp(*env->cur_stream);

  bool_value boolConst = bool_value(val, true);
  return boolConst;
}

operand object_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "Object" << std::endl;

  // TODO: Do checks later to see if acesses are found within scope
  // In future add more object checks here (ptrs, strings, custom objs)

  // check label of object
  // look in symbol table
  // load value
  //  return correct operand / value
  ValuePrinter vp(*env->cur_stream);

  operand op = *env->find_in_scopes(name);
  operand ld = vp.load(op.get_type().get_deref_type(), op);

  return ld;
}

operand no_expr_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "No_expr" << std::endl;

  // TODO: add code here and replace `return operand()`
  return operand();
}

//*****************************************************************
// The next few functions are for node types not supported in Phase 1
// but these functions must be defined because they are declared as
// methods via the Expression_SHARED_EXTRAS hack.
//*****************************************************************

operand static_dispatch_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "static dispatch" << std::endl;
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  // TODO: add code here and replace `return operand()`
  return operand();
#endif
}

operand string_const_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "string_const" << std::endl;
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  // TODO: add code here and replace `return operand()`
  return operand();
#endif
}

operand dispatch_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "dispatch" << std::endl;
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  // TODO: add code here and replace `return operand()`
  return operand();
#endif
}

// Handle a Cool case expression (selecting based on the type of an object)
operand typcase_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "typecase::code()" << std::endl;
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  // TODO: add code here and replace `return operand()`
  return operand();
#endif
}

operand new__class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "newClass" << std::endl;
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  // TODO: add code here and replace `return operand()`
  return operand();
#endif
}

operand isvoid_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "isvoid" << std::endl;
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  // TODO: add code here and replace `return operand()`
  return operand();
#endif
}

// Create the LLVM Function corresponding to this method.
void method_class::layout_feature(CgenNode *cls)
{
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  // TODO: add code here
#endif
}

// Handle one branch of a Cool case expression.
// If the source tag is >= the branch tag
// and <= (max child of the branch class) tag,
// then the branch is a superclass of the source.
// See the LAB2 handout for more information about our use of class tags.
operand branch_class::code(operand expr_val, operand tag, op_type join_type,
                           CgenEnvironment *env)
{
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  // TODO: add code here and replace `return operand()`
  return operand();
#endif
}

// Assign this attribute a slot in the class structure
void attr_class::layout_feature(CgenNode *cls)
{
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  // TODO: add code here
#endif
}

void attr_class::code(CgenEnvironment *env)
{
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  // TODO: add code here
#endif
}

/*
 * Definitions of make_alloca
 */
void assign_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "assign" << std::endl;

  // TODO: add code here
}

void cond_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "cond" << std::endl;

  // TODO: add code here
}

void loop_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "loop" << std::endl;

  // TODO: add code here
}

void block_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "block" << std::endl;

  // TODO: add code here
}

void let_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "let" << std::endl;

  // TODO: add code here
}

void plus_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "plus" << std::endl;

  // TODO: add code here
}

void sub_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "sub" << std::endl;

  // TODO: add code here
}

void mul_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "mul" << std::endl;

  // TODO: add code here
}

void divide_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "div" << std::endl;

  // TODO: add code here
}

void neg_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "neg" << std::endl;

  // TODO: add code here
}

void lt_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "lt" << std::endl;

  // TODO: add code here
}

void eq_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "eq" << std::endl;

  // TODO: add code here
}

void leq_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "leq" << std::endl;

  // TODO: add code here
}

void comp_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "complement" << std::endl;

  // TODO: add code here
}

void int_const_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
  {
    std::cerr << "Integer Constant Alloca" << std::endl;
    std::cerr << "Token Name : " << token->get_string() << std::endl;
  }

  // TODO: add code here THIS IS BAD???
  ValuePrinter vp(*env->cur_stream);

  operand hold = vp.alloca_mem(op_type(INT32));
  env->add_binding(token, &hold);
  return;
}

void bool_const_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "Boolean Constant" << std::endl;

  ValuePrinter vp(*env->cur_stream);

  operand hold = vp.alloca_mem(op_type(INT1));
  // env->add_binding(, &hold)
  //  TODO: add code here
}

void object_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "Object" << std::endl;

  // TODO: add code here
}

void no_expr_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "No_expr Alloca" << std::endl;

  // TODO: add code here
}

void static_dispatch_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "static dispatch" << std::endl;
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
    // TODO: add code here
#endif
}

void string_const_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "string_const" << std::endl;
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
    // TODO: add code here
#endif
}

void dispatch_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "dispatch" << std::endl;
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
    // TODO: add code here
#endif
}

void typcase_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "typecase::make_alloca()" << std::endl;
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
    // TODO: add code here
#endif
}

void new__class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "newClass" << std::endl;
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
    // TODO: add code here
#endif
}

void isvoid_class::make_alloca(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "isvoid" << std::endl;
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
    // TODO: add code here
#endif
}

void branch_class::make_alloca(CgenEnvironment *env)
{
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  // TODO: add code here
#endif
}

void method_class::make_alloca(CgenEnvironment *env) { return; }

void attr_class::make_alloca(CgenEnvironment *env)
{
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  // TODO: add code here
#endif
}

#ifdef LAB2
// conform - If necessary, emit a bitcast or boxing/unboxing operations
// to convert an object to a new type. This can assume the object
// is known to be (dynamically) compatible with the target type.
// It should only be called when this condition holds.
// (It's needed by the supplied code for typecase)
operand conform(operand src, op_type type, CgenEnvironment *env)
{
  // TODO: add code here
  return operand();
}
#endif
