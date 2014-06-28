#header
<<
#include <string>
#include <iostream>
#include <map>
using namespace std;

// struct to store information about tokens
typedef struct {
  string kind;
  string text;
} Attrib;

// function to fill token information (predeclaration)
void zzcr_attr(Attrib *attr, int type, char *text);

// fields for AST nodes
#define AST_FIELDS string kind; string text;
#include "ast.h"

// macro to create a new AST node (and function predeclaration)
#define zzcr_ast(as,attr,ttype,textt) as=createASTnode(attr,ttype,textt)
AST* createASTnode(Attrib* attr, int ttype, char *textt);
>>

<<
#include <cstdlib>
#include <cmath>

//global structures
map<string,bool> block;
AST *root;


// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
//  if (type == NUM) {
//    attr->kind = "intconst";
//    attr->text = text;
//  }
//  else {
    attr->kind = text;
    attr->text = "";
//  }
}

// function to create a new AST node
AST* createASTnode(Attrib* attr, int type, char* text) {
  AST* as = new AST;
  as->kind = attr->kind; 
  as->text = attr->text;
  as->right = NULL; 
  as->down = NULL;
  return as;
}


/// create a new "list" AST node with one element
AST* createASTlist(AST *child) {
 AST *as=new AST;
 as->kind="list";
 as->right=NULL;
 as->down=child;
 return as;
}

AST *findASTListDef(string id) {
  AST *n = root->down;
  while (n != NULL and n->down->kind != id) n = n->right;
  return n;
}

/// get nth child of a tree. Count starts at 0.
/// if no such child, returns NULL
AST* child(AST *a,int n) {
AST *c=a->down;
for (int i=0; c!=NULL && i<n; i++) c=c->right;
return c;
}

int applyReduce(AST *a, string op){
  if(child(a,0) == NULL) return 0;
  else{
    int res = 0;
    if(op == "*") res = 1;
    for(AST* aux = child(a,0); aux; aux = aux->right){
      if(aux->kind == "["){
	if(op == "+") res += applyReduce(aux,op);
      	else if(op == "-") res -= applyReduce(aux,op);
      	else if(op == "*") res *= applyReduce(aux,op);
      }
      else{
	if(op == "+") res += atoi(aux->kind.c_str());
      	else if(op == "-") res -= atoi(aux->kind.c_str());
      	else if(op == "*") res *= atoi(aux->kind.c_str());
      }
    }
    return res;
  }
}


int reduce(AST *a){
  return applyReduce(child(findASTListDef(child(a,1)->kind),1), child(a,0)->kind);
}


/// print AST, recursively, with indentation
void ASTPrintIndent(AST *a,string s)
{
  if (a==NULL) return;
  if (a->kind == "lreduce") reduce(a);
  cout<<a->kind;
  if (a->text!="") cout<<"("<<a->text<<")";
  cout<<endl;

  AST *i = a->down;
  while (i!=NULL && i->right!=NULL) {
    cout<<s+"  \\__";
    ASTPrintIndent(i,s+"  |"+string(i->kind.size()+i->text.size(),' '));
    i=i->right;
  }
  
  if (i!=NULL) {
      cout<<s+"  \\__";
      ASTPrintIndent(i,s+"   "+string(i->kind.size()+i->text.size(),' '));
      i=i->right;
  }
}

/// print AST 
void ASTPrint(AST *a)
{
  while (a!=NULL) {
    cout<<" ";
    ASTPrintIndent(a,"");
    a=a->right;
  }
}

int main() {
  root = NULL;
  ANTLR(lists(&root), stdin);
  ASTPrint(root);
}
>>

#lexclass START
#token NUM "[0-9]+"
#token PLUS "\+"
#token MINUS "\-"
#token MULT "\*"
#token GREATER ">"
#token EQUAL "="
#token NEQUAL "!="
#token LESS "<"
#token CONCAT "\#"
#token PRINT "print"
#token LMAP "lmap"
#token LFILTER "lfilter"
#token LREDUCE "lreduce"
#token OPENC "\["
#token CLOSEC "\]"
#token COMA ","
#token ID "[A-z][0-9]*"
#token SPACE "[\ \n]" << zzskip();>>

lists: (list_oper)* <<#0=createASTlist(_sibling);>> ;

list_oper: ( printexp | assigexp)*EOF;

printexp: PRINT^ ID; 

assigexp: ID EQUAL^ subexpr;

subexpr: ( filterexpr | lmapexpr | lredexpr | concexp | listexpr );

filterexpr: LFILTER^ ( eqexpr | neqexpr | grexpr | lesexpr ) ID;

eqexpr:     EQUAL^   NUM;
neqexpr:    NEQUAL^  NUM;
grexpr:     GREATER^ NUM;
lesexpr:    LESS^    NUM;

lmapexpr:   LMAP^    (PLUS|MINUS|MULT) NUM ID;
lredexpr:   LREDUCE^ (PLUS|MINUS|MULT) ID;
concexp:    ID CONCAT^ ID;

listexpr: OPENC^ ( ( listsubexpr (COMA! listsubexpr)* ) | ) CLOSEC!;
listsubexpr: (listexpr | NUM);





