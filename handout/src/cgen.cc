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
  // add all coolrt.o funcs here

  /* Object external methods */
  // external %Object *Object_new(void)
  op_type Object_ptr("Object", 1), string("String", 1), object("Object"), string_ptr("String", 1);
  std::vector<op_type> new_object_args;
  vp.declare(*ct_stream, Object_ptr, "Object_new", new_object_args);

  // external %Object *Object_abort(Object *self)
  std::vector<op_type> object_abort_args;
  object_abort_args.push_back(Object_ptr);
  vp.declare(*ct_stream, Object_ptr, "Object_abort", object_abort_args);

  // external %String *Object_type_name(Object *self)
  std::vector<op_type> type_name_args;
  type_name_args.push_back(Object_ptr);
  vp.declare(*ct_stream, string, "Object_type_name", type_name_args);

  // external %Object *Object_copy(Object *self)
  std::vector<op_type> copy_args;
  copy_args.push_back(Object_ptr);
  vp.declare(*ct_stream, Object_ptr, "Object_copy", copy_args);

  /* Int external methods */
  // external %Int *Int_new(void)
  op_type Int_ptr(INT32_PTR);
  std::vector<op_type> new_int_args;
  vp.declare(*ct_stream, i32_type, "Int_new", new_int_args);

  // external void Int_init(Int *self, int i)
  std::vector<op_type> init_int_args;
  init_int_args.push_back(Int_ptr);
  init_int_args.push_back(i32_type);
  vp.declare(*ct_stream, void_type, "Int_init", init_int_args);

  /* Bool external methods */
  // external %Bool *Bool_new(void)
  op_type Bool_ptr("Bool", 1), bool_type(INT1);
  std::vector<op_type> new_bool_args;
  vp.declare(*ct_stream, Bool_ptr, "Bool_new", new_bool_args);

  // external void Bool_init(Bool *self, bool b)
  std::vector<op_type> init_bool_args;
  init_bool_args.push_back(Bool_ptr);
  init_bool_args.push_back(bool_type);
  vp.declare(*ct_stream, void_type, "Bool_init", init_bool_args);

  /* String external methods */
  // external %String *String_new(void)
  std::vector<op_type> new_string_args;
  vp.declare(*ct_stream, string, "String_new", new_string_args);

  // external %int String_length(String *self)
  std::vector<op_type> length_args;
  length_args.push_back(string_ptr);
  vp.declare(*ct_stream, i32_type, "String_length", length_args);

  // external %String *String_concat(String *self, String *s)
  std::vector<op_type> concat_args;
  concat_args.push_back(string_ptr);
  concat_args.push_back(string_ptr);
  vp.declare(*ct_stream, string, "String_concat", concat_args);

  // external %String *String_substr(String *self, int i, int l)
  std::vector<op_type> substr_args;
  substr_args.push_back(string_ptr);
  substr_args.push_back(i32_type);
  substr_args.push_back(i32_type);
  vp.declare(*ct_stream, string, "String_substr", substr_args);

  /* IO external methods */

  // external %IO *IO_out_string(IO *self, String *s)
  op_type IO("IO"), IO_ptr("IO", 1);
  std::vector<op_type> out_string_args;
  out_string_args.push_back(IO_ptr);
  out_string_args.push_back(string_ptr);
  vp.declare(*ct_stream, IO_ptr, "IO_out_string", out_string_args);

  // external %IO *IO_out_int(IO *self, int x)
  std::vector<op_type> out_int_args;
  out_int_args.push_back(IO_ptr);
  out_int_args.push_back(i32_type);
  vp.declare(*ct_stream, IO_ptr, "IO_out_int", out_int_args);

  // external %String *IO_in_string(IO *self)
  std::vector<op_type> in_string_args;
  in_string_args.push_back(IO_ptr);
  vp.declare(*ct_stream, string, "IO_in_string", in_string_args);

  // external %int IO_in_int(IO *self)
  std::vector<op_type> in_int_args;
  in_int_args.push_back(IO_ptr);
  vp.declare(*ct_stream, i32_type, "IO_in_int", in_int_args);

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
  // set up environment
  // iterate through class table

  CgenEnvironment env(*(c->get_ostream()), c);
  for (CgenNode *a : c->get_children())
  {
    a->code_class();

    // for each class
    // call code_classes for each of it's children
  }
}
#endif

// Create global definitions for constant Cool objects
void CgenClassTable::code_constants()
{
#ifdef LAB2
  // TODO: add code here
  stringtable.code_string_table(*ct_stream, this);
#endif
}

// Create LLVM entry point. This function will initiate our Cool program
// by generating the code to execute (new Main).main()
//
void CgenClassTable::code_main()
{
  // TODO: add code here

  // Define a function main that has no parameters and returns an i32
  // std::string mainName = "main";

  // int_value zero = int_value(0);

  // global_value cv = global_value(op_arr_ptr_type(INT8, 25), ".str");

  // const_value str = const_value(op_arr_type(INT8, 25), "Main.main() returned %d\n", true);
  // vp.init_constant(".str", str);

  ValuePrinter vp(*ct_stream);
  std::vector<operand> main_args;
  std::vector<op_type> main_arg_types;

  // Call Main_main(). This returns int for phase 1, Object for phase 2

  // parse through all classes -> find main
  //  parse through all methods in main -> find main
  //  get proper return type & codegen the method

#ifdef LAB2
  // LAB2

  // Function: resolve all methods in the main

  /* --------- Control Flow ---------- //
    1. parse through all methods
    2. call code on the method
      if method is "main"
        define entry *special*

    3. once all methods are resolved end_define
  // -------------------------------- */

  // find main method and get
  // op_type main_method_type;

  vp.define(op_type(INT32), "main", main_args);
  // // Define an entry basic block

  vp.begin_block("entry");
  // // vp.init_constant("mainsudoobj", const_value(op_type("Main"), "mainsudoobj", false));
  operand hold = vp.alloca_mem(op_type("Main"));
  main_args.push_back(hold);
  main_arg_types.push_back(hold.get_type());

  op_type main_type;
  // get main type
  for (CgenNode *n : this->get_classes())
  {
    if (n->get_name()->get_string() == "Main")
    {
      for (method_mine *m : n->get_methods_in_class())
      {
        if (m->get_name()->get_string() == "Main_main")
        {
          main_type = m->get_return_type();
        }
      }
    }
  }

  vp.call(main_arg_types, main_type, "Main_main", true, main_args);
  vp.ret(int_value(0));

  vp.end_define();
#else
  operand ret = vp.call(null, op_type(INT32), "Main_main", true, non);
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
  if (cgen_debug)
  {
  }

  // add values to string table here to be found later

  // TODO: add code here
  ValuePrinter vp(s);

  std::string const_val_name = ct->get_const_name("str", true);
  vp.init_constant(const_val_name, const_value(op_arr_type(INT8, this->str.length() + 1), this->str, false));

  std::string const_struct_name = ct->get_const_name("str", true);
  global_value s_name(op_type("String"), const_struct_name);
  std::vector<op_type> fields;
  std::vector<const_value> init_values;

  // Add Vtable
  fields.emplace_back(op_type("_String_vtable", 1));
  init_values.emplace_back(const_value(op_type("_String_vtable", 1), "@_String_vtable_prototype", false));

  // Add pointer to const
  op_arr_type arr_ptr_type(INT8, this->str.length() + 1);
  const_value string_name(arr_ptr_type, "@" + const_val_name, false);
  fields.emplace_back(op_type(INT8_PTR));
  init_values.emplace_back(string_name);

  vp.init_struct_constant(s_name, fields, init_values);

  // add to string table
  stringtable.add_string(this->get_string());
  // add to classtable constants to get lookup global var
  ct->add_constant(const_struct_name, str);

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
  if (cgen_debug)
  {
    std::cerr << "Class: " << this->name->get_string() << "\n"
              << std::endl;

    std::cerr << "Attributes: " << std::endl;
    for (attribute_mine *m : this->get_attributes_in_class())
    {
      std::cerr << m->get_name()->get_string() << ": " << m->get_return_type().get_name() << std::endl;
    }
    std::cerr << std::endl;

    std::cerr << "Methods: " << std::endl;
    for (method_mine *m : this->get_methods_in_class())
    {
      std::cerr << "Method Name: " << m->get_name()->get_string() << std::endl;
      std::cerr << "Parameters: " << std::endl;
      for (operand n : m->get_params())
      {
        std::cerr << n.get_name() << " : " << n.get_type().get_name() << std::endl;
      }
      std::cerr << std::endl;
    }

    std::cerr << "===============" << std::endl;
  }
  // TODO: add code here

  ValuePrinter vp(*ct_stream);

  // class type define
  std::vector<op_type> class_attributes;

  // vtable type define + const_values for vtable init
  std::vector<const_value> vtable_init_values;
  std::vector<op_type> vtable_methods;

  // method tracker
  std::unordered_set<std::string> attributesSeen;
  std::unordered_set<std::string> methodsSeen;
  recurse_attributes_methods(this, class_attributes, vtable_methods, vtable_init_values, attributesSeen, methodsSeen);

  // add ptr to const char string
  op_arr_type arr_ptr_type(INT8, this->name->get_string().length() + 1);
  const_value class_name(arr_ptr_type, "@" + this->name->get_string() + "_classname", false);
  vp.init_constant(this->name->get_string() + "_classname", const_value(arr_ptr_type, this->name->get_string(), false));
  stringtable.add_string(this->name->get_string());

  vtable_methods.insert(vtable_methods.begin(), op_type(INT8_PTR));
  vtable_init_values.insert(vtable_init_values.begin(), class_name);

  // class_attributes.emplace_back(op_type(this->parent->get_string(), 1));
  class_attributes.insert(class_attributes.begin(), op_type(this->get_vtable_type_name(), 1));

  // add int size of obj (no. bytes)
  vtable_methods.insert(vtable_methods.begin(), op_type(INT32));
  const_value obj_size(op_type(INT32), "ptrtoint (" + op_type(this->get_type_name(), 1).get_name() + " getelementptr (%" + this->get_type_name() + ", " + op_type(this->get_type_name(), 1).get_name() + " null, i32 1) to i32)", true);
  vtable_init_values.insert(vtable_init_values.begin(), obj_size);

  // add tag
  vtable_methods.insert(vtable_methods.begin(), op_type(INT32));
  vtable_init_values.insert(vtable_init_values.begin(), const_value(op_type(INT32), std::to_string(tag), true));

  vp.type_define(this->get_type_name(), class_attributes);
  vp.type_define(this->get_vtable_type_name(), vtable_methods);

  // vtable prototype
  global_value vtab_const_name(op_type(this->get_vtable_type_name()), this->get_vtable_name());
  vp.init_struct_constant(vtab_const_name, vtable_methods, vtable_init_values);

#endif
}

void CgenNode::recurse_attributes_methods(CgenNode *cls, std::vector<op_type> &attributes, std::vector<op_type> &methods, std::vector<const_value> &init_value, std::unordered_set<std::string> &attributesSeen, std::unordered_set<std::string> &methodsSeen)
{
  if (cls->get_name()->get_string() == "_no_class")
  {
    return;
  }
  recurse_attributes_methods(cls->get_parentNode(), attributes, methods, init_value, attributesSeen, methodsSeen);

  for (attribute_mine *attr : cls->get_attributes_in_class())
  {
    unsigned long int sizeLast = attributesSeen.size();
    attributesSeen.insert(attr->get_name()->get_string());
    if (attributesSeen.size() != sizeLast)
    {
      op_type retain = attr->get_return_type();
      if (retain.get_name() == "%int")
        attributes.emplace_back(op_type(INT32));
      else if (retain.get_name() == "%bool")
        attributes.emplace_back(op_type(INT1));
      else if (retain.get_name() == "%sbyte*")
        // TODO: Not Machine agnostic, change later
        attributes.emplace_back(op_type(INT8_PTR));
      else
        attributes.emplace_back(attr->get_return_type());
    }
  }

  // search through the parent types to see
  //  if there are any matching method signatures
  // if matching : skip ; else: add to methods
  for (long unsigned int i = 0; i < cls->get_methods_in_class().size(); i++)
  {
    std::vector<method_mine *> m = cls->get_methods_in_class();
    long unsigned int sizeLast = methodsSeen.size();
    methodsSeen.insert(strip_method_name(m[i]->get_name()->get_string()));
    // if overloaded / virtual function found

    std::vector<op_type> parameters;
    for (operand o : m[i]->get_params())
    {
      parameters.push_back(o.get_type());
    }
    op_func_type func_type(m[i]->get_return_type(), parameters);

    // method not seen
    if (methodsSeen.size() != sizeLast)
    {
      methods.push_back(func_type);
      init_value.emplace_back(const_value(func_type, "@" + m[i]->get_name()->get_string(), true));
    }
    else
    {
      // method seen
      // find the function with mathcing name and insert to replace
      for (long unsigned int j = 0; j < init_value.size() - 1; j++)
      {
        std::string prefix = init_value[j].get_value();
        if (strip_method_name(init_value[j].get_name()) == strip_method_name(m[i]->get_name()->get_string()))
        {
          methods[j] = func_type;
          init_value[j] = (const_value(func_type, "@" + m[i]->get_name()->get_string(), true));
        }
      }
    }
  }
}

// Go through each node until you reach absolute parent
// add methods and attributes along the way
// DEPRECATED
void CgenNode::add_parent_attributes_methods(std::vector<op_type> &attributes, std::vector<op_type> &methods, std::vector<const_value> &init_values)
{
  ValuePrinter vp(*this->ct_stream);
  CgenNode *explore = this;

  std::unordered_set<std::string> attributes_name;
  long unsigned int sizeLasta = 0;
  std::unordered_set<std::string> methods_name;
  long unsigned int sizeLast = 0;

  //  Current checking for function overloading is very primitive
  //  Currently only checking if the name is the same but should check
  //  if entire signature of method is same (parameters mainly)

  // Inefficient parsing (twice through list of all parents)
  // However produces prettier LLVM IR

  while (explore->get_name()->get_string() != "_no_class")
  {
    // add attributes from most local to parents
    for (attribute_mine *attr : explore->get_attributes_in_class())
    {
      attributes_name.insert(attr->get_name()->get_string());
      if (attributes_name.size() != sizeLasta)
      {
        op_type retain = attr->get_return_type();
        if (retain.get_name() == "%int")
          attributes.emplace_back(op_type(INT32));
        else if (retain.get_name() == "%bool")
          attributes.emplace_back(op_type(INT1));
        else if (retain.get_name() == "%sbyte*")
          attributes.emplace_back(op_type(INT8_PTR));
        else
          attributes.emplace_back(attr->get_return_type());
        // explore->add_attribute(attr->get_name(), retain);
        sizeLasta++;
      }
    }

    // search through the parent types to see
    //  if there are any matching method signatures
    // if matching : skip ; else: add to methods
    std::vector<op_type> class_methods;
    std::vector<const_value> class_methods_init;
    for (method_mine *m : explore->get_methods_in_class())
    {
      methods_name.insert(strip_method_name(m->get_name()->get_string()));
      // if overloaded / virtual function found
      //  TODO: add logic to deal with overloaded function
      if (methods_name.size() != sizeLast)
      {
        std::vector<op_type> parameters;
        for (operand o : m->get_params())
        {
          parameters.push_back(o.get_type());
        }
        op_func_type func_type(m->get_return_type(), parameters);
        class_methods.insert(class_methods.begin(), func_type);
        class_methods_init.insert(class_methods_init.begin(), const_value(func_type, "@" + m->get_name()->get_string(), true));
        // explore->add_method(m->get_name(), m->get_return_type(), m->get_params(), m->get_expression());
        sizeLast++;
      }
      else
      {
        std::cerr << "HERE " << m->get_name() << std::endl;
        // find the function with mathcing name and insert to replace
        for (long unsigned int i = 0; i < methods.size() - 1; ++i)
        {
          std::string prefix = explore->get_name()->get_string();
          if (strip_method_name(methods[i].get_name()) == strip_method_name(m->get_name()->get_string()))
          {

            init_values[i].set_name("@" + m->get_name()->get_string());
          }
        }
      }
    }
    for (auto &element : class_methods)
    {
      methods.insert(methods.begin(), element);
    }
    for (auto &element : class_methods_init)
    {
      init_values.insert(init_values.begin(), element);
    }

    explore = explore->get_parentNode();
  }
}

std::string CgenNode::strip_method_name(std::string method_name)
{
  return method_name.substr(method_name.find("_") + 1);
}

#ifdef LAB2
// Laying out the features involves creating a Function for each method
// and assigning each attribute a slot in the class structure.
void CgenNode::layout_features()
{
  if (cgen_debug)
  {
    std::cerr << "Class: " << this->get_type_name() << "\n"
              << std::endl;
  }
  // TODO: add code here
  /* Fields required
  Features features;
  Symbol filename;

  vtable type name
  init function name

  */

  //  Basically create function definitions for methods within each class

  /* --------- Control Flow --------- /
  1. Iterate through feature list of class
  2. For each feature call (polymorphic) layout_feature()
  2a. Layout feature will generate LLVM IR for function definition
  3. End

  / -------------------------------- */
  ValuePrinter vp(*ct_stream);

  int n = 0;
  while (features->more(n))
  {
    features->nth(n)->layout_feature(this);
    n++;
  }

  if (cgen_debug)
  {
    std::cerr << "======================" << std::endl;
  }
}

// Class codegen. This should performed after every class has been setup.
// Generate code for each method of the class.
void CgenNode::code_class()
{
  // No code generation for basic classes. The runtime will handle that.
  // code methods for basic classes' children classes
  CgenEnvironment env(*ct_stream, this);
  if (basic())
  {
    for (CgenNode *n : this->get_children())
    {
      n->code_init_function(&env);
    }
    return;
  }
  this->code_init_function(&env);
  for (CgenNode *n : this->get_children())
  {
    n->code_init_function(&env);
  }
}

void CgenNode::code_init_function(CgenEnvironment *env)
{
  // TODO: add code here
  ValuePrinter vp(*env->cur_stream);
  // Set up class variables here
  std::cerr << "CODE_INIT_FUNCTION \t| Class: " << this->name->get_string() << std::endl;

  // for (attribute_mine *a : this->get_attributes_in_class())
  // {
  //   operand *toStore = new operand(a->get_return_type(), a->get_return_type().get_name());
  //   std::string rtt_name = a->get_return_type().get_name();
  //   env->add_binding(a->get_name(), toStore);
  // }

  // set class in env
  env->set_class(this);
  for (Feature f : features)
  {
    std::cerr << "BEGIN | FEATURE" << std::endl;
    f->code(env);
    std::cerr << "END | FEATURE" << std::endl;
  }
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
    std::cerr << "method " << env->get_class()->get_type_name() << std::endl;
  }
  ValuePrinter vp(*env->cur_stream);

  std::vector<operand> method_parameters;

  // add self
  operand self_ = operand(op_type(env->get_class()->get_name()->get_string(), 1), "self");
  method_parameters.emplace_back(self_);

  Entry *self_name = new Entry("self", SELF_TYPE->get_index());
  std::cerr << self_.get_typename() << " " << self_name->get_string() << " " << self->get_index() << std::endl;
  env->open_scope();
  env->add_binding(Symbol(self_name), &self_);

  for (Formal f : formals)
  {
    method_parameters.push_back(operand(op_type(f->get_type_decl()->get_string()), f->get_name()->get_string()));
  }
  vp.define(op_type(return_type->get_string(), 1), env->get_class()->get_type_name() + "_" + name->get_string(), method_parameters);

  vp.begin_block("entry");
  // operand self_ptr = vp.alloca_mem(self_.get_type());
  // vp.store(self_, self_ptr);

  // env->insert_scoped("self", &self_ptr);
  //  env->dump_vartable();
  //  operand test = *env->find_in_scopes(Symbol(self_name));

  operand finalExpression = expr->code(env);
  if (finalExpression.get_name() == "%self")
  {
    if (finalExpression.get_typename() != return_type->get_string())
    {
      finalExpression = conform(finalExpression, op_type(return_type->get_string(), 1), env);
    }
  }
  vp.ret(finalExpression);
  env->close_scope();
  vp.end_define();
}

// Codegen for expressions. Note that each expression has a value.

operand assign_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "assign" << std::endl;

  ValuePrinter vp(*env->cur_stream);
  // TODO: add code here and replace `return operand()`
  // get ptr to name
  // resolve the expression and store in temp
  // store the temp into the ptr to the name
  // return the value of the resolved expr

  operand findSym;
  operand store;
  operand value = expr->code(env);

  // Find symbol within env table
  std::cerr << name->get_string() << std::endl;
  // TODO: SHOULD NOT DO THIS HERE
  // if (env->find_in_scopes(name) == nullptr)
  // {
  //   // Not found
  //   // Means class definition + malloc needed
  //   std::cerr << value.get_type().get_name() << std::endl;
  //   findSym = vp.malloc_mem(value);
  //   env->add_binding(name, &findSym);
  // }
  // else
  // {
  //   findSym = *env->find_in_scopes(name);
  // }
  if (value.get_typename() == "i32")
  {
    store = vp.alloca_mem(INT32);
  }
  else if (value.get_typename() == "i1")
  {
    store = vp.alloca_mem(INT1);
  }
  else
  {
    // TODO: Do I need alloc here?
    store = vp.alloca_mem(value.get_type());
    op_type elem_type;
    findSym = *env->find_in_scopes(name);
    std::cerr << findSym.get_typename() << std::endl;

    if (findSym.get_type().is_ptr())
    {
      elem_type = findSym.get_type().get_deref_type();
    }
    else
    {
      elem_type = findSym.get_type();
      findSym.set_type(findSym.get_type().get_ptr_type());
    }
    value = vp.getelementptr(elem_type, findSym, int_value(0), findSym.get_type());
    value = vp.load(value.get_type().get_deref_type(), value);
  }

  vp.store(value, store);

  // add to new scope
  operand *yo = new operand(value);
  // env->open_scope();
  env->add_binding(name, yo);

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
    std::cerr << "Object " << name->get_string() << " " << name->get_index() << std::endl;

  // TODO: Do checks later to see if acesses are found within scope
  // In future add more object checks here (ptrs, strings, custom objs)

  // check label of object
  // look in symbol table
  // load value
  //  return correct operand / value
  ValuePrinter vp(*env->cur_stream);
  operand oper;
  std::string obj_name = name->get_string();
  if (obj_name == "self")
  {
    std::cerr << "here???" << std::endl;
    // add self to scope before passing
    // env->dump_vartable();
    // Entry *self_name = new Entry("self", SELF_TYPE->get_index());
    // operand test = *env->find_in_scopes(Symbol(self_name));
    // std::cerr << test.get_name() << " YO I'm tired of this class" << std::endl;
    // oper = *env->find_scoped("self");
    oper = operand(op_type(env->get_class()->get_type_name(), 1), "self");
  }
  else
  {
    oper = *env->find_in_scopes(name);
    if (this->get_type()->get_string() != "Int" && this->get_type()->get_string() != "Bool")
    {
      op_type elem_type;
      // current error
      elem_type = oper.get_type().get_ptr_type();
      int_value zero(0);
      operand temp_hold = vp.getelementptr(oper.get_type(), oper, zero, elem_type);
      oper = vp.load(elem_type, temp_hold);
    }
  }

  return oper;
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
    std::cerr << "static_dispatch " << std::endl;
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  // TODO: add code here and replace `return operand()`
  ValuePrinter vp(*env->cur_stream);
  std::vector<op_type> arg_types;
  std::vector<operand> args;
  // add self
  std::string function_name = name->get_string();

  operand resolve = expr->code(env);

  // Check if the current class has the method
  // Or its parent class
  // return the name of the class where it is found
  // std::cerr << "TEST BEGIN " << env->get_class()->get_name()->get_string() << std::endl;
  std::string find_class;
  /*
  iterate through class table
  if type_name found to match cgennode name return cgennode
  */

  CgenNode *node;
  for (CgenNode *n : env->get_class()->get_classtable()->get_classes())
  {
    if (type_name->get_string().substr(1) == (n->get_name()->get_string() + "*"))
    {
      node = n;
    }
  }

  method_mine *method_ = node->find_method_class_init(name->get_string().substr(name->get_string().find("_") + 1)); // seg fault here

  // add self to method
  // args.pushback(operand(op))
  operand self_ = operand(op_type(env->get_class()->get_name()->get_string(), 1), "self");
  if (self_.get_type().get_name() != type_name->get_string())
  {
    self_ = conform(self_, op_type(method_->get_class_origin()->get_name()->get_string(), 1), env);
  }
  args.emplace_back(self_);
  arg_types.emplace_back(self_.get_type());

  int iter = 0;
  while (actual->more(iter))
  {
    operand hold = actual->nth(iter)->code(env);
    arg_types.emplace_back(hold.get_type());
    args.emplace_back(hold);
    iter++;
  }
  operand call_result = vp.call(arg_types, method_->get_return_type(), method_->get_name()->get_string(), true, args);
  if (!call_result.get_type().is_ptr())
  {
    call_result = conform(call_result, call_result.get_type().get_ptr_type(), env);
  }
  return call_result;
#endif
}

operand string_const_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    std::cerr << "string_const" << std::endl;
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  ValuePrinter vp(*env->cur_stream);
  Symbol found = stringtable.lookup_string(token->get_string());
  std::string *global_name;
  if (env->get_class()->get_classtable()->find_const(token->get_string()) != nullptr)
  {
    global_name = env->get_class()->get_classtable()->find_const(token->get_string());
  }
  else
  {
    throw std::nullopt;
  }
  std::cerr << "String Constant found: " << *global_name << std::endl;

  // init char arr
  global_value ret = global_value(op_type("String", 1), *global_name, const_value(op_type("String", 1), token->get_string(), true));

  return ret;
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

  // expr - body of function
  // name - function to be called name
  // acual - list of parameters

  ValuePrinter vp(*env->cur_stream);

  operand method_self = expr->code(env);
  std::string functionName = name->get_string();

  std::string actual_method_class;

  //"self" is passed to every object
  if (method_self.get_name() == "%self")
  {
    // static dispatch
    method_mine *bark = env->get_class()->find_method_class_init(functionName);
    Entry *speak = new Entry(bark->get_return_type().get_name(), bark->get_return_type().get_id());
    static_dispatch_class yo(expr, Symbol(speak), bark->get_name(), actual);
    return yo.code(env);
  }

  std::vector<op_type> arg_types;
  std::vector<operand> args;

  // make a temp obj
  operand ptr = vp.alloca_mem(method_self.get_type());
  // vp.store(method_self, ptr);

  // add self
  args.push_back(ptr);
  arg_types.emplace_back(ptr.get_type());

  int iter = 0;
  while (actual->more(iter))
  {
    operand hold = actual->nth(iter)->code(env);
    arg_types.emplace_back(hold.get_type());
    args.emplace_back(hold);
    iter++;
  }

  // get vtable ptr
  // operand vtable_ptr = vp.getelementptr(method_self.get_type(), ptr, int_value(0), method_self.get_type().get_ptr_type());
  // vtable_ptr = vp.bitcast(vtable_ptr, op_type("_" + method_self.get_typename().substr(1) + "_vtable", 1));

  // get method return type + index in vtable
  CgenNode *class_to_search;
  // search normal installed classes
  for (CgenNode *n : env->get_class()->get_classtable()->get_classes())
  {
    // std::cerr << "Classname: " << n->get_name()->get_string() << std::endl;
    if (method_self.get_typename().substr(1) == n->get_name()->get_string())
      class_to_search = n;
  }
  // search special installed classes
  for (CgenNode *n : env->get_class()->get_classtable()->get_special_classes())
  {
    // std::cerr << "Special classnames: " << n->get_name()->get_string() << std::endl;
    if (method_self.get_typename().substr(1) == n->get_name()->get_string())
      class_to_search = n;
  }
  method_mine *method_ = class_to_search->find_method_class_init(functionName);
  op_type method_ret_type = method_->get_return_type();
  //  get method from vtable
  // operand method_ptr = vp.getelementptr(vtable_ptr.get_type().get_deref_type(), vtable_ptr, int_value(method_symbol->get_index()), op_func_ptr_type(method_ret_type, arg_types));

  // actual contains parameters of function dispatch
  if (actual_method_class.find("%") == 0)
  {
    actual_method_class = actual_method_class.substr(1);
  }
  operand call_result = vp.call(arg_types, method_ret_type, method_->get_name()->get_string(), true, args);
  operand ret = vp.alloca_mem(method_ret_type);
  vp.store(call_result, ret);

  // vp.load(ret.get_type().get_deref_type(), ret);

  /*  Static Dispatch   */
  // 0. alloc mem for obj ptr of caller class type

  /*  Dynamic Dispatch   */
  // 0. get addr for mem of caller obj

  // 1. get obj
  // 2. get Vtable ptr from obj being called on
  // 3. load Vtable obj
  // 4. get ptr to function from vtable with index
  // 5. load the function ptr to vtmp
  // 6. call vtmp
  return call_result;

#endif
}

// returns the name of the class where the method is defined
method_mine *CgenNode::find_method_class_init(std::string method_name)
{
  method_mine *def;
  CgenNode *explore = this;
  int found = 0;
  while (!found)
  {
    if (explore->get_name()->get_string() == "Object")
      found = 1;
    for (method_mine *m : explore->get_methods_in_class())
    {
      if (method_name == strip_method_name(m->get_name()->get_string()))
      {
        def = m;
        def->set_class_origin(explore);
        found = 1;
      }
    }
    explore = explore->get_parentNode();
  }
  return def;
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
  if (cgen_debug)
  {
    std::cerr << "Testing Methods" << std::endl;
    std::cerr << "Method Name: " << this->name->get_string() << "\t| Return Type:" << this->get_return_type()->get_string() << "\n"
              << std::endl;
  }

  // Get return type
  op_type returnType;
  std::string returnTypeString = this->get_return_type()->get_string();

  if (returnTypeString == "Int")
    returnType = op_type(INT32);
  else if (returnTypeString == "Bool")
    returnType = op_type(INT1);
  else if (returnTypeString == "SELF_TYPE")
    returnType = op_type(cls->get_name()->get_string(), 1);
  else
    returnType = op_type(returnTypeString, 1);

  // Get all params (formals)
  std::vector<operand> parameters;
  std::vector<op_type> params;
  int iter = 0;

  // Add self
  op_type temp = op_type(cls->get_name()->get_string(), 1);
  params.emplace_back(temp);
  parameters.emplace_back(operand(temp, cls->get_name()->get_string()));

  while (formals->more(iter))
  {
    std::string paramName = formals->nth(iter)->get_name()->get_string();
    std::string typeName = formals->nth(iter)->get_type_decl()->get_string();

    op_type resolve_type;
    if (typeName == "Int")
      resolve_type = op_type(INT32);
    else if (typeName == "Bool")
      resolve_type = op_type(INT1);
    else if (typeName == "SELF_TYPE")
    {
      if (cls->get_name()->get_string() == "Int")
        resolve_type = op_type(INT32);
      else if (cls->get_name()->get_string() == "Bool")
        resolve_type = op_type(INT1);
      else
        resolve_type = op_type(cls->get_name()->get_string(), 1);
    }
    else
      resolve_type = op_type(typeName, 1);

    // TODO:
    // SHOULD I CHECK IF PARAM IS SELF_TYPE AND THROW ERR
    params.push_back(resolve_type);
    parameters.emplace_back(operand(resolve_type, paramName));
    iter++;
  }
  // vp.declare(returnType, cls->get_name()->get_string() + "_" + this->name->get_string(), params);

  Entry *temp2 = new Entry(cls->get_name()->get_string() + "_" + name->get_string(), name->get_index());
  Symbol method_with_prefix = Symbol(temp2);
  std::cerr << "LAYOUT " << method_with_prefix->get_string() << std::endl;
  cls->add_method(method_with_prefix, returnType, parameters, this->expr);

  // add method to class's children
  // for (CgenNode *a : cls->get_children())
  // {
  //   a->add_method(cls->get_name()->get_string() + "_" + this->name->get_string(), returnType, parameters, this->expr);
  // }

  // // encode the method
  // CgenEnvironment env(*(cls->get_ostream()), cls);
  // expr->code(&env);

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
  if (cgen_debug)
  {
    std::cerr << "Testing attributes" << std::endl;
    std::cerr << this->type_decl->get_string() << "\n"
              << std::endl;
  }
  std::string typeName = this->type_decl->get_string();
  op_type resolve_type;
  if (typeName == "Int")
    // params.emplace_back(operand((INT32), paramName));
    resolve_type = op_type(INT32);
  else if (typeName == "Bool")
    resolve_type = op_type(INT1);
  else
    resolve_type = op_type(typeName);

  cls->add_attribute(this->name, resolve_type, init);
  // add attribute to class's children
  for (CgenNode *a : cls->get_children())
  {
    a->add_attribute(this->name, resolve_type, init);
  }
#endif
}

void attr_class::code(CgenEnvironment *env)
{
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  // TODO: add code here
  // do alloc here
  ValuePrinter vp(*env->cur_stream);
  std::cerr << name->get_string() << std::endl;
  op_type alloc_type = op_type(type_decl->get_string());
  if (type_decl->get_string() == "Int")
  {
    alloc_type = op_type(INT32);
  }
  else if (type_decl->get_string() == "Bool")
  {
    alloc_type = op_type(INT1);
  }
  env->add_binding(name, new operand(init->code(env)));
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
    std::cerr << "Object alloca" << std::endl;
  ValuePrinter vp(*env->cur_stream);
  operand *check = env->find_in_scopes(name);
  if (check->get_typename() == "i32")
  {
    Entry *hold = new Entry(check->get_name(), name->get_index());
    Symbol temp0 = hold;
    int_const_class temp(temp0);
    temp.make_alloca(env);
  }
  else if (check->get_typename() == "i1")
  {
    Entry *hold = new Entry(check->get_name(), name->get_index());
    Symbol temp0 = hold;
    bool_const_class temp(temp0);
    temp.make_alloca(env);
  }
  else
  {
    vp.alloca_mem(op_type(name->get_string()));
  }
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
  ValuePrinter vp(*env->cur_stream);
  // vp.alloca_mem(type);
  // src.set_type(type);
  operand ret;
  if (src.get_type().is_ptr() && !type.is_ptr())
  {
    operand temp = vp.bitcast(src, type.get_ptr_type());
    ret = vp.load(type, temp);
  }
  else if (!src.get_type().is_ptr() && type.is_ptr())
  {
    ret = vp.getelementptr(src.get_type(), src, int_value(0), type);
  }
  else
  {
    ret = vp.bitcast(src, type);
  }
  return ret;
}
#endif
