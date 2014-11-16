
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;
unsigned long labelCounter=0;            // counter for generating different labels
Symbol live_class;                       // current class for which expressions are being coded
std::vector<Symbol> methodArgs;          // arguments of a method
std::vector<Symbol> letIds;              // let identifiers
//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

// Pop the top of stack and store in reg register
static void emit_pop(char *reg, ostream& str){
	emit_load(reg,1,SP,str);
	emit_addiu(SP,SP,4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD << STRINGNAME << DISPTAB_SUFFIX;


 /***** Add dispatch information for class String ******/

      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD << INTNAME << DISPTAB_SUFFIX; 

 /***** Add dispatch information for class Int ******/

      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD << BOOLNAME << DISPTAB_SUFFIX;

 /***** Add dispatch information for class Bool ******/

      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
   stringclasstag = 4 /* Change to your String class tag here */;
   intclasstag =    2 /* Change to your Int class tag here */;
   boolclasstag =   3 /* Change to your Bool class tag here */;

   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

   for (List<CgenNode> *l = nds; l; l=l->tl())
   		ordered_nodes.push_back(l->hd());
   std::reverse(ordered_nodes.begin(), ordered_nodes.end());

   code();
   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO, 
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
      class_(Str, 
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat, 
				   single_Formals(formal(arg, Str)),
				   Str, 
				   no_expr()))),
	    single_Features(method(substr, 
				   append_Formals(single_Formals(formal(arg, Int)), 
						  single_Formals(formal(arg2, Int))),
				   Str, 
				   no_expr()))),
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(unsigned i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

// function to code class names string indexes in StringTable
void CgenClassTable::code_class_nameTab(){
	str << CLASSNAMETAB << LABEL;
	// iterating over all class names in reverse manner(vector is reversed)
	for (unsigned it = 0; it<ordered_nodes.size(); it++){
		char* s = ordered_nodes[it]->get_name()->get_string();
		StringEntry* entry = stringtable.lookup_string(s);
		str << WORD;
		entry->code_ref(str);
		str << endl;
	}
}

// function to code all class names with init and object suffix
void CgenClassTable::code_class_objTab(){
	str << CLASSOBJTAB << LABEL;
	for (unsigned it=0; it<ordered_nodes.size(); it++){
		str << WORD << ordered_nodes[it]->get_name() << PROTOBJ_SUFFIX << endl;
		str << WORD << ordered_nodes[it]->get_name() << CLASSINIT_SUFFIX << endl;
	}
}


// database containing information about each method and its offset
// map <className, map< methodName, pair< offset, className> > >
std::map< Symbol, std::map< Symbol, std::pair< int, Symbol> > >  methodDB; 

// recursive function to fill method table for class with symbol current
void CgenClassTable::make_methodTable(CgenNodeP node, Symbol current_class){
	if(node->get_name() != Object)		// reach towards the Object Class
		make_methodTable(node->get_parentnd(), current_class);

	for(unsigned i=node->features->first(); node->features->more(i); i = node->features->next(i)){
		method_class* func = dynamic_cast<method_class*>(node->features->nth(i));
		if(func!=NULL){
			/* if method is already there i.e end iterator is not returned by find,
			   just update the class name in the pair, offset will be same  */
			if(methodDB[current_class].find(func->name) != methodDB[current_class].end())
				methodDB[current_class][func->name].second = node->get_name();
			else{		// add new entry if method isn't present
				int offset = methodDB[current_class].size();
				methodDB[current_class][func->name] = std::make_pair(offset, node->get_name());
			}
		}
	}
}

// iterate over all classes and insert their method tables into methodDB
void CgenClassTable::populate_methodDB(){
	for (unsigned i=0; i<ordered_nodes.size(); i++){
		CgenNodeP current_node = ordered_nodes[i];			// get node pointer
		Symbol current_class = current_node->get_name();	// get class name
		std::map< Symbol, std::pair< int, Symbol> > placeHolder;
		methodDB[current_class] = placeHolder;				// insert empty method table for class
		make_methodTable(current_node, current_class);
	}
}

int compare(std::pair< int, Symbol> a, std::pair< int, Symbol> b){
	return (a.first < b.first);
}

// function to emit dispatch tables for all classes
void CgenClassTable::code_class_dispTab(){
	populate_methodDB();
	// iterating over all classes
	for(unsigned i=0; i<ordered_nodes.size(); i++){
		Symbol current_class = ordered_nodes[i]->get_name();

		std::vector< std::pair<int, Symbol> > ordered_methods;		// methods for this class ordered with offsets
		
		// iterating over unordered methods of this class and dumping in vector
		std::map< Symbol, std::pair< int, Symbol> >::iterator it;
		for(it = methodDB[current_class].begin(); it!=methodDB[current_class].end(); it++)
			ordered_methods.push_back(std::make_pair((it->second).first, it->first));

    // sort using their offset values
		std::sort(ordered_methods.begin(), ordered_methods.end(), compare);
		
		str << current_class << DISPTAB_SUFFIX << LABEL; 
		for(unsigned i=0; i<ordered_methods.size(); i++)
			str << WORD << methodDB[current_class][ordered_methods[i].second].second << METHOD_SEP << ordered_methods[i].second << endl;	
	}
}


// database containing information about each attribute and its offset
std::map< Symbol, std::map< Symbol, std::pair<int, Symbol> > >  attribDB;

// recursive function to fill attribute table for class with symbol current
void CgenClassTable::make_attribTable(CgenNodeP node, Symbol current_class){
	if(node->get_name() != Object)
		make_attribTable(node->get_parentnd(), current_class);
	
	for(unsigned i=node->features->first(); node->features->more(i); i = node->features->next(i)){
		attr_class* attrib = dynamic_cast<attr_class*>(node->features->nth(i));
		if(attrib!=NULL){
			int offset = attribDB[current_class].size();
			attribDB[current_class][attrib->name] = std::make_pair(offset, attrib->type_decl);
		}
	}
}

// iterate over all classes and insert their attribute tables into attribDB
void CgenClassTable::populate_attribDB(){
	for (unsigned i=0; i<ordered_nodes.size(); i++){
		CgenNodeP current_node = ordered_nodes[i];			// get node pointer
		Symbol current_class = current_node->get_name();	// get class name
		std::map< Symbol, std::pair<int, Symbol> > placeHolder;
		attribDB[current_class] = placeHolder;				// insert empty attribute table for class
		make_attribTable(current_node, current_class);
	}
}

// function to emit prototype objects for all classes
void CgenClassTable::code_class_protoType(){
	populate_attribDB();

	for(unsigned i=0; i<ordered_nodes.size(); i++){
		Symbol current_class = ordered_nodes[i]->get_name();
		str << WORD << "-1\n" << current_class << PROTOBJ_SUFFIX << LABEL;
		str << WORD << i << endl; 										// class tag
		str << WORD << 3 + attribDB[current_class].size() << endl;		// size of prototype object
		str << WORD << current_class << DISPTAB_SUFFIX << endl;			// dispatch table
		
		std::vector< std::pair<int, Symbol> > ordered_attribs;			// attributes for class ordered according to offsets
		
		// iterating over unordered attributes of this class and dumping in vector
		std::map<Symbol, std::pair<int, Symbol> >::iterator it;
		for(it = attribDB[current_class].begin(); it!=attribDB[current_class].end(); it++)
			ordered_attribs.push_back(std::make_pair( (it->second).first, it->first));

		std::sort(ordered_attribs.begin(), ordered_attribs.end(), compare);		// sort the attributes
		
		// emit code for all the attributes
		for(unsigned j=0; j<ordered_attribs.size(); j++){
			Symbol attr_type = attribDB[current_class][ordered_attribs[j].second].second;
			str << WORD;
			if(attr_type == Int){				    // default is 0
				IntEntry *entry = inttable.lookup_string("0");
				entry->code_ref(str);
			}
			else if (attr_type == Str){			// default is empty string
				StringEntry *entry = stringtable.lookup_string("");
				entry->code_ref(str);
			}
			else if (attr_type == Bool)			// default value is false
				falsebool.code_ref(str);
			else
				str << 0;

			str << endl;
		}
	}
}

// function to emit init function code for all classes
void CgenClassTable::code_class_init(){
	for(unsigned node_num=0; node_num<ordered_nodes.size(); node_num++){
		CgenNodeP node = ordered_nodes[node_num];
		Symbol current_class = node->get_name();
    live_class = current_class;

		str << current_class << CLASSINIT_SUFFIX << LABEL;
    // saving frame pointer, self, return adress in stack, building AR
		emit_push(FP, str);
		emit_push(SELF, str);
		emit_push(RA, str);
		emit_addiu(FP, SP, 4, str);			// $fp = $sp+4 i.e callee has become active now
		emit_move(SELF, ACC, str);			// save accumulater before calling parent

		// call parent if its present
		if(current_class != Object)
			str << JAL << node->get_parentnd()->get_name()->get_string() << CLASSINIT_SUFFIX << endl;

		// evaluate all the initializations
		for(unsigned i=node->features->first(); node->features->more(i); i=node->features->next(i)){
			attr_class* attrib = dynamic_cast<attr_class*>(node->features->nth(i));
			if(attrib!=NULL){
				Expression val = attrib->init;			// init contains the expression on RHS
				if(val->get_type() != NULL){			// if expression has type assigned to it
					
					val->code(str);
					int offset = 3 + attribDB[current_class][attrib->name].first;
					emit_store(ACC, offset, SELF, str);	// put the evaluated expression at right place in object
				}
			}
		}

		emit_move(ACC, SELF, str);			// restore accumultor after parent call
		
		emit_pop(RA, str);
		emit_pop(SELF, str);
		emit_pop(FP, str);

		emit_return(str);					// return to the address in RA
	}
}

// function to emit methods for all classes
void CgenClassTable::code_class_methods(){
	for(unsigned node_num=0; node_num<ordered_nodes.size(); node_num++){
		CgenNodeP node = ordered_nodes[node_num];
		Symbol current_class = node->get_name();
    live_class = current_class;

		if(current_class!=Object && current_class!=Str && current_class!=IO){
			// iterate over all the methods
			for(unsigned i=node->features->first(); node->features->more(i); i=node->features->next(i)){
				method_class *func = dynamic_cast<method_class*>(node->features->nth(i));
				if(func != NULL){
					// calculate number of formals for this method
					int numFormals = 0;
          methodArgs.clear();
          /* caller has pushed all the arguments in the stack. Here at callee side we push all of them
            in a vector so that in expression coding for methods, appropriate offset can be determined. */
					for(unsigned j=func->formals->first(); func->formals->more(j); j=func->formals->next(j)){
						formal_class *curFormal = dynamic_cast<formal_class *> (func->formals->nth(j));
            if(curFormal!=NULL){
              methodArgs.push_back(curFormal->name);
              numFormals++;
            }
          }
          if(cgen_debug) cout<<func->name<<endl;

					str << current_class << METHOD_SEP << func->name << LABEL;
					emit_push(FP, str);
					emit_push(SELF, str);
					emit_push(RA, str);
					emit_addiu(FP, SP, 4, str);			// $fp = $sp+4 i.e callee has become active now
					emit_move(SELF, ACC, str);

					// emit code for function body
					Expression definition = func->expr;
					definition->code(str);

          // pop the values pushed into stack after functione execution
					emit_pop(RA, str);
					emit_pop(SELF, str);
					emit_pop(FP, str);
					emit_addiu(SP, SP, numFormals*4, str);		// shift the stack pointer by number of arguments*4
					emit_return(str);
				}
			}
		}
	}
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

  if (cgen_debug) cout << "coding class name table" << endl;
  code_class_nameTab();

  if (cgen_debug) cout << "coding class prototype objectTable" << endl;
  code_class_objTab();

  if (cgen_debug) cout << "coding each class' dispatch tables" << endl;
  code_class_dispTab();

  if (cgen_debug) cout << "coding prototype objects for each class" << endl;
  code_class_protoType();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

  if (cgen_debug) cout << "coding init code for each class" << endl;
  code_class_init();

  if (cgen_debug) cout << "coding methods for each class" << endl;
  code_class_methods();
//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...

}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s) {
  if(cgen_debug) cout << "assign class executing" << endl;
  // name <- expr     (format of asign expression)
  int offset;
  expr->code(s);             // code the RHS

  /* If assignment is in a let body i.e name is in let
     find LHS in letIds and go to corresponding stack address
  */
  for(int i=letIds.size()-1; i>=0; i--){
    if(name == letIds[i]){
      offset = letIds.size() - i;       // LHS found in let
      emit_store(ACC, offset, SP, s);
      return;
    }
  }

  /* If name is an argument, then retrieve from stack by finding the offset from methodArgs */

  for(int i=0; i<methodArgs.size(); i++){
    if(name == methodArgs[i]){
      offset = methodArgs.size() -1 -i;
      emit_store(ACC, offset+3, FP, s);
      return;
    }
  }

  //  normal attribute of class, get its position in protoObj
  offset = attribDB[live_class][name].first;
  emit_store(ACC, 3+offset, SELF, s);
}


void static_dispatch_class::code(ostream &s) {
  if (cgen_debug) cout << "Static Dispatch_class executing" << endl;
  int numActuals = 0;       // number of arguments passed while dispatch
 
  for(unsigned i=actual->first(); actual->more(i); i=actual->more(i)){
    numActuals++;
    actual->nth(i)->code(s);
    // dispatch can be inside let expression
    letIds.push_back(No_type);
    emit_push(ACC, s);

  }
  expr->code(s);                         // evaluate the dispatch expression
  emit_bne(ACC, ZERO, labelCounter, s); // jump to label if its not void
  // dispatch to void error
  emit_load_address(ACC, "str_const0", s);
  emit_load_imm(T1, 1, s);
  emit_jal("_dispatch_abort", s);

  // valid dispatch
  emit_label_def(labelCounter, s);

  Symbol exprType = type_name;          // type_name contains the type of dispatch
  if(exprType == SELF_TYPE){
    exprType = live_class;
  }
  // getting the address of the method to dispatch
  char* tempAddress = exprType->get_string();
  char finalAddress[500];
  strcpy(finalAddress, tempAddress);
  strcat(finalAddress, "_dispTab");

  emit_load_address(T1, finalAddress, s);
  int offset = methodDB[exprType][name].first;
  emit_load(T1, offset, T1, s);          // go to appropriate offset in dipatch table
  emit_jalr(T1, s);                       // call that method
  // after dispatch is over, pop the dummies from letIds
  for(int i=0; i<numActuals; i++)
    letIds.pop_back();

  labelCounter++;
}

void dispatch_class::code(ostream &s) {
  if (cgen_debug) cout << "Dispatch_class executing" << endl;
	int numActuals = 0;					// number of arguments
	// evaluate and push each argument on stack
	for(int i=actual->first(); actual->more(i); i=actual->next(i)){
		numActuals++;
		actual->nth(i)->code(s);
		emit_push(ACC, s);
    letIds.push_back(No_type);    
	}
	expr->code(s); 			// code for the dispatch expression
	emit_bne(ACC, ZERO, labelCounter, s);	// jump to label if its not void
	// dispatch to void error
	emit_load_address(ACC, "str_const0", s);
	emit_load_imm(T1, 1, s);
	emit_jal("_dispatch_abort", s);

	// valid dispatch
	emit_label_def(labelCounter, s);
	emit_load(T1, 2, ACC, s);		// value of dispatch table for dispatch expression
	Symbol exprType = expr->get_type();
	if(exprType == SELF_TYPE){
		exprType = live_class;
	}
	int offset = methodDB[exprType][name].first;
	emit_load(T1, offset, T1, s);	// go to appropriate offset in dipatch table
	emit_jalr(T1, s);				// call that method
  // pop back dummies from let after call is finished
  for(int i=0; i<numActuals; i++)
    letIds.pop_back();
	labelCounter++;
}

void cond_class::code(ostream &s) {
  // if pred then then_exp else else_exp
  int condThen = labelCounter++;
  int condElse = labelCounter++;

  pred->code(s);                          // code for pre-condition expression
  emit_fetch_int(T1, ACC, s);
  emit_beqz(T1, condElse, s);             // if condition is false 
  then_exp->code(s);
  emit_branch(condThen, s);

  emit_label_def(condElse, s);           // else label
  else_exp->code(s);
  emit_label_def(condThen, s);
}

void loop_class::code(ostream &s) {
  // loop pred body pool
  int condTrue = labelCounter++;
  int condFalse = labelCounter++;

  emit_label_def(condTrue, s);      // if condition is true
  pred->code(s);                    // evaluate condition for loop execution
  emit_fetch_int(T1, ACC, s);
  emit_beqz(T1, condFalse, s);
  body->code(s);                    // evaluate loop body
  emit_branch(condTrue, s);         // unconditional jump

  // return zero in accumulator if condition becomes false
  emit_label_def(condFalse, s);
  emit_move(ACC, ZERO, s);
}

void typcase_class::code(ostream &s) {
}

void block_class::code(ostream &s) {
  // emit code for each line of the block
  for(int i=body->first(); body->more(i); i=body->next(i))
    body->nth(i)->code(s);
}

void let_class::code(ostream &s) {
  // let identifier:type_decl <- init in body
  /* if no initializer is there, assign default value
    Default values:
    Int => 0
    String => ""
    Bool => false
    Object => 0
  */
  if(init->get_type() == NULL){
    if(type_decl==Int)            // for int
      emit_load_int(ACC, inttable.lookup_string("0"), s);
    else if(type_decl==Str)       // for string
      emit_load_string(ACC, stringtable.lookup_string(""), s);
    else if(type_decl==Bool)      // for bool
      emit_load_bool(ACC, falsebool, s);
    else                          // for objects
      emit_load_imm(ACC, 0, s);
  }
  else{
    init->code(s);        // if the identifier has some assignment
  }
  // push the identifier in vector so that offset can be found while body evaluation
  letIds.push_back(identifier);
  emit_push(ACC, s);
  body->code(s);        // evaluate the body of let
  emit_addiu(SP, SP, 4, s);
  letIds.pop_back();    // after body evaluation, identifier is not valid
}

void plus_class::code(ostream &s) {
	e1->code(s);                  // evaluate expression 1
	emit_push(ACC, s);
	letIds.push_back(No_type);    // push dummy value into let arguments
  e2->code(s);
	emit_fetch_int(T2, ACC, s);		// t2 contains actual int value from object
  // create a new object to return the value into
  emit_jal("Object.copy", s);
	emit_pop(T1, s);			      	// t1=e1
	letIds.pop_back();
  emit_fetch_int(T1, T1, s);		// t1 contains actual int 
	emit_add(T1, T1, T2, s);
	emit_store_int(T1, ACC, s);		// store in accumulator
}

void sub_class::code(ostream &s) {
	e1->code(s);
	emit_push(ACC, s);
  letIds.push_back(No_type);
	e2->code(s);
	emit_fetch_int(T2, ACC, s);		// t2 contains actual int value from object
  emit_jal("Object.copy", s);
	emit_pop(T1, s);				// t1=e1
	letIds.pop_back();
  emit_fetch_int(T1, T1, s);		// t1 contains actual int 
	emit_sub(T1, T1, T2, s);
	emit_store_int(T1, ACC, s);		// store in a
}

void mul_class::code(ostream &s) {
	e1->code(s);
	emit_push(ACC, s);
  letIds.push_back(No_type);
	e2->code(s);
	emit_fetch_int(T2, ACC, s);		// t2 contains actual int value from object
  emit_jal("Object.copy", s);   // make a new object in ACC
	emit_pop(T1, s);				      // t1=e1
	letIds.pop_back();
  emit_fetch_int(T1, T1, s);		// t1 contains actual int 
	emit_mul(T1, T1, T2, s);
	emit_store_int(T1, ACC, s);		// store in a
}

void divide_class::code(ostream &s) {
	e1->code(s);
	emit_push(ACC, s);
  letIds.push_back(No_type);
	e2->code(s);
	emit_fetch_int(T2, ACC, s);		// t2 contains actual int value from object
  emit_jal("Object.copy", s);
	emit_pop(T1, s);				// t1=e1
  letIds.pop_back();
  emit_fetch_int(T1, T1, s);		// t1 contains actual int 
	emit_div(T1, T1, T2, s);
	emit_store_int(T1, ACC, s);		// store in a
}

// function to code the negate expressions (~expr)
void neg_class::code(ostream &s) {
	e1->code(s);                    // code the expression
  emit_fetch_int(T2, ACC, s);     // load actual value
	emit_jal("Object.copy", s);
  emit_neg(T2, T2, s);
  emit_store_int(T2, ACC, s);
}

// function to code for the calculation of e1<e2
void lt_class::code(ostream &s) {
	e1->code(s);               // code the expression e1
	emit_push(ACC, s);
  letIds.push_back(No_type);
	e2->code(s);               // code e2
	emit_pop(T1, s);
  letIds.pop_back();
  emit_fetch_int(T1, T1, s);          // T1 contains actual value of e1
	emit_fetch_int(T2, ACC, s);         // T2 contains actual value of e2
	emit_load_bool(ACC, truebool, s);		// load true
	emit_blt(T1, T2, labelCounter, s);    // branch if T1 < T2
	emit_load_bool(ACC, falsebool, s);		// load falsebool
	emit_label_def(labelCounter, s);
	labelCounter++;
}

// function to code for the expression e1==e2
void eq_class::code(ostream &s) {
	e1->code(s);           // code the expression e1
	emit_push(ACC, s);
  letIds.push_back(No_type);
	e2->code(s);           // code the expression e2
	emit_pop(T1, s);		// $t1 = e1
  letIds.pop_back();
	emit_move(T2, ACC, s);	// $t2 = e2
	emit_load_bool(ACC, truebool, s);
	emit_beq(T1, T2, labelCounter, s);       // if t1, t2 has same address then they are equal and jump

  // otherwise we have to return false
  emit_load_bool(ACC, falsebool, s);
	Symbol exprType = e1->get_type();
	if(exprType==Int || exprType==Str || exprType==Bool){
    	emit_load_bool(ACC, truebool, s); 		// load true to $a0
    	emit_load_bool(A1, falsebool, s); 		// load false to $a1
    	emit_jal("equality_test",s);
	}

	emit_label_def(labelCounter, s);
	labelCounter++;
}

// function to code for expression e1<=e2
void leq_class::code(ostream &s) {
	e1->code(s);             // code expression e1
	emit_push(ACC, s);
  letIds.push_back(No_type);
	e2->code(s);             // code expression e2
	emit_pop(T1, s);
  letIds.pop_back();
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, ACC, s);
	emit_load_bool(ACC, truebool, s);		// load true
	emit_bleq(T1, T2, labelCounter, s);
	emit_load_bool(ACC, falsebool, s);		// load false
	emit_label_def(labelCounter, s);
	labelCounter++;
}

// function to code for the complement expression
void comp_class::code(ostream &s) {
	e1->code(s);
  emit_fetch_int(T1, ACC, s);
  emit_load_bool(ACC, truebool, s);   // load true
  emit_beqz(T1, labelCounter, s);     // if T1 is zero then jump as we have to return true
  emit_load_bool(ACC, falsebool, s);
  emit_label_def(labelCounter, s);
  labelCounter++;
}

void int_const_class::code(ostream& s)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s) {
  // new type_name

  if(type_name != SELF_TYPE){
    char *className = type_name->get_string();        // classname for the new
    // construct the prototype string i.e className_protObj for the requested className
    char protoType[200] = "";
    strcpy(protoType, className);
    strcat(protoType, "_protObj");
    emit_load_address(ACC, protoType, s);       // load the address of that prototype into accumulator

    /* construct the init method string i.e className_init for jumping over to
        so that we can initialise the requested object */
    char initMethod[200] = "";
    strcpy(initMethod, className);  
    strcat(initMethod, "_init");
    emit_jal("Object.copy", s);     // create a new object for storing the newly created object
    emit_jal(initMethod, s);        // jump to the init of the class
  }
}

// function to code the isvoid expr
void isvoid_class::code(ostream &s) {
	e1->code(s);                         // evaluate the expression
	emit_load(T1, 0, ACC, s);
	emit_load_bool(ACC, truebool, s);		// load true		
	emit_beqz(T1, labelCounter, s);			// if expression is zero, jump to labelCounter
	emit_load_bool(ACC, falsebool, s); 		// load false
	emit_label_def(labelCounter, s);		
	labelCounter++;
}

void no_expr_class::code(ostream &s) {
}

// function to code the expressions which are not objects
void object_class::code(ostream &s) {
  if(cgen_debug) cout<<"Entered into object_class"<<endl;
  // if its self, it points to same class prototype which is stored in $s0
	if(name == self){
		emit_move(ACC, SELF, s);
		return;
	}

  int offset;
  

  /* If object is in a let expression,
     find it in letIds and go to corresponding stack address
  */
  for(int i=letIds.size()-1; i>=0; i--){
    if(name == letIds[i]){
      offset = letIds.size() - i;
      // 
      emit_load(ACC, offset, SP, s);
      return;
    }
  }

  /* If name is an argument, then retrieve from stack by finding the offset from methodArgs */
  for(int i=0; i<methodArgs.size(); i++){
    if(cgen_debug)cout<<name<<endl;
    if(name == methodArgs[i]){
      offset = methodArgs.size()-i-1;
      /* offset+3 because attributes are after 3 locations in activation record after
         frame pointer, return address, self pointer*/
      emit_load(ACC, offset+3, FP, s);
      return;
    }
  }

  /* otherwise for a simple attribute, search in class prototype objects at 3 places
     after class tag, size, dispatch pointer */
  offset = attribDB[live_class][name].first;
  emit_load(ACC, 3+offset, SELF, s);
}
