StaticConstructor:
	error / KW_static KW_this opLParen opRParen FunctionBody ;

StaticDestructor:
	error / KW_static opTilde KW_this opLParen opRParen FunctionBody ;

Invariant:
	error / KW_invariant opLParen opRParen BlockStatement ;

UnitTest:
	error / KW_unittest FunctionBody ;

AsmPrimaryExp:
	error / IntegerConstant |
	error / FloatConstant |
	error / PKW___LOCAL_SIZE |
	error / opDollar |
	error / Register |
	error / DotIdentifier ;

DotIdentifier:
	error / Identifier |
	error / Identifier opDot DotIdentifier ;

AsmTypePrefix:
	error / PKW_near PKW_ptr |
	error / PKW_far PKW_ptr |
	error / KW_byte PKW_ptr |
	error / KW_short PKW_ptr |
	error / KW_int PKW_ptr |
	error / PKW_word PKW_ptr |
	error / PKW_dword PKW_ptr |
	error / KW_float PKW_ptr |
	error / KW_double PKW_ptr |
	error / KW_real PKW_ptr ;

AsmUnaExp:
	error / AsmTypePrefix AsmExp |
	error / PKW_offest AsmExp |
	error / PKW_seg AsmExp |
	error / opPlus AsmUnaExp |
	error / opMinus AsmUnaExp |
	error / opBang AsmUnaExp |
	error / opTilde AsmUnaExp |
	error / AsmPrimaryExp ;

AsmRelExp:
	error / AsmShiftExp |
	error / AsmShiftExp opLessThan AsmShiftExp |
	error / AsmShiftExp opLessEqual AsmShiftExp |
	error / AsmShiftExp opGreaterThan AsmShiftExp |
	error / AsmShiftExp opGreaterEqual AsmShiftExp ;

AsmShiftExp:
	error / AsmAddExp |
	error / AsmAddExp opLShift AsmAddExp |
	error / AsmAddExp opRShift AsmAddExp |
	error / AsmAddExp op3RShift AsmAddExp ;

AsmAddExp:
	error / AsmMulExp |
	error / AsmMulExp opPlus AsmMulExp |
	error / AsmMulExp opMinus AsmMulExp ;

AsmMulExp:
	error / AsmBrExp |
	error / AsmBrExp opStar AsmBrExp |
	error / AsmBrExp opDivision AsmBrExp |
	error / AsmBrExp opModulo AsmBrExp ;

AsmBrExp:
	error / AsmUnaExp |
	error / AsmBrExp opLBrace AsmExp opRBrace ;

AsmLogAndExp:
	error / AsmOrExp |
	error / AsmOrExp opLogicAnd AsmOrExp ;

AsmOrExp:
	error / AsmXorExp |
	error / AsmXorExp opPipe AsmXorExp ;

AsmXorExp:
	error / AsmAndExp |
	error / AsmAndExp opCarrot AsmAndExp ;

AsmAndExp:
	error / AsmEqualExp |
	error / AsmEqualExp opAmpersand AsmEqualExp ;

AsmEqualExp:
	error / AsmRelExp |
	error / AsmRelExp opEqual AsmRelExp |
	error / AsmRelExp opNotEqual AsmRelExp ;
AsmInstruction:
	error / Identifier opCollin AsmInstruction |
	error / KW_align IntegerExpression |
	error / PKW_even |
	error / PKW_naked |
	error / PKW_db Operands |
	error / PKW_ds Operands |
	error / PKW_di Operands |
	error / PKW_dl Operands |
	error / PKW_df Operands |
	error / PKW_dd Operands |
	error / PKW_de Operands |
	error / Opcode |
	error / Opcode Operands ;

Operands:
	error / Operand |
	error / Operand opComma Operands ;


Operand:
	error / AsmExp ;

AsmExp:
	error / AsmLogOrExp |
	error / AsmLogOrExp opQuestion AsmExp opCollin AsmExp ;

AsmLogOrExp:
	error / AsmLogAndExp |
	error / AsmLogAndExp opLogicOr AsmLogAndExp ;AttributeSpecifier:
	error / Attribute opCollin |
	error / Attribute DeclarationBlock ;

Attribute:
	error / LinkageAttribute |
	error / AlignAttribute |
	error / Pragma |
	error / KW_deprecated |
	error / KW_private |
	error / KW_package |
	error / KW_protected |
	error / KW_public |
	error / KW_export |
	error / KW_static |
	error / KW_final |
	error / KW_override |
	error / KW_abstract |
	error / KW_const |
	error / KW_auto |
	error / KW_scope ;

LinkageAttribute:
	error / KW_extern |
	error / KW_extern opLParen LinkageType opRParen ;

LinkageType:
	error / PKW_C |
	error / PKW_C opPlusPlus |
	error / PKW_D |
	error / PKW_Windows |
	error / PKW_Pascal ;

AlignAttribute:
	error / KW_align |
	error / KW_align opLParen Integer opRParen ;

BaseClassList_Comma:
	error / opCollin SuperClass |
	error / opCollin SuperClass opComma InterfaceClasses_Comma ;

InterfaceClasses_Comma:
	error / InterfaceClass |
	error / InterfaceClass opComma  InterfaceClasses_Comma ;

BaseClassList:
	error / opCollin SuperClass |
	error / opCollin SuperClass InterfaceClasses |
	error / opCollin InterfaceClass ;

SuperClass:
	error / Identifier |
	error / Protection Identifier ;

InterfaceClasses:
	error / InterfaceClass |
	error / InterfaceClass InterfaceClasses ;

InterfaceClass:
	error / Identifier |
	error / Protection Identifier ;

Protection:
	error / KW_private |
	error / KW_package |
	error / KW_public |
	error / KW_export ;
ClassDeclaration:
	error / KW_class Identifier BaseClassList? ClassBody ;

ClassBody:
	error / opLBracket opRBracket |
	error / opLBracket ClassBodyDeclarations opRBracket ;

ClassBodyDeclarations:
	error / ClassBodyDeclaration |
	error / ClassBodyDeclaration ClassBodyDeclarations ;

ClassBodyDeclaration:
	error / Declaration |
	error / Constructor |
	error / Destructor |
	error / StaticConstructor |
	error / StaticDestructor |
	error / Invariant |
	error / UnitTest |
	error / ClassAllocator |
	error / ClassDeallocator ;

Constructor:
	error / KW_this Parameters FunctionBody ;

Destructor:
	error / opTilde KW_this opLParen opRParen FunctionBody ;

PostfixExpression :
	pass1 / PrimaryExpression |
	error / PostfixExpression opDot Identifier |
	error / PostfixExpression opDot NewExpression |
	error / PostfixExpression opPlusPlus |
	error / PostfixExpression opMinusMinus |
	error / PostfixExpression opLParen opRParen |
	error / PostfixExpression opLParen ArgumentList opRParen |
	pass1 / IndexExpression |
	pass1 / SliceExpression ;

PrimaryExpression:
	pass1 / Identifier |
	error / opDot Identifier |
	pass1 / KW_this |
	pass1 / KW_super |
	pass1 / KW_null |
	pass1 / KW_true |
	pass1 / KW_false |
	pass1 / opDollar |
	pass1 / NumericLiteral |
	pass1 / CharacterLiteral |
	pass1 / StringLiterals |
	pass1 / ArrayLiteral |
	pass1 / AssocArrayLiteral |
	pass1 / FunctionLiteral |
	pass1 / AssertExpression |
	pass1 / MixinExpression |
	pass1 / ImportExpression |
	error / BasicType opDot Identifier |
	error / KW_typeid opLParen Type opRParen |
	pass1 / IsExpression |
	error / opLParen Expression opRParen ;

IsExpression:
	error / KW_is opLParen Type opRParen |
	error / KW_is opLParen Type opCollin TypeSpecialization opRParen |
	error / KW_is opLParen Type opEqual TypeSpecialization opRParen |
	error / KW_is opLParen Type Identifier opRParen |
	error / KW_is opLParen Type Identifier opCollin TypeSpecialization opRParen |
	error / KW_is opLParen Type Identifier opEqual TypeSpecialization opRParen ;

TypeSpecialization:
	pass1 / Type |
	pass1 / KW_typedef |
	pass1 / KW_struct |
	pass1 / KW_union |
	pass1 / KW_class |
	pass1 / KW_interface |
	pass1 / KW_enum |
	pass1 / KW_function |
	pass1 / KW_delegate |
	pass1 / KW_super ;
EnumDeclaration:
	error / KW_enum Identifier EnumBody |
	error / KW_enum EnumBody |
	error / KW_enum Identifier opCollin EnumBaseType EnumBody |
	error / KW_enum opCollin EnumBaseType EnumBody ;

EnumBaseType:
	error / Type ;

EnumBody:
	error / opSemicolon |
	error / opLBracket EnumMembers opRBracket ;

EnumMembers:
	error / EnumMember |
	error / EnumMember opComma |
	error / EnumMember opComma EnumMembers ;

EnumMember:
	error / Identifier |
	error / Identifier opAssignment AssignExpression ;
Module:
	error / ModuleDeclaration DeclDefs |
	error / DeclDefs ;


DeclDef:
	pass1 / AttributeSpecifier |
	pass1 / ImportDeclaration |
	pass1 / EnumDeclaration |
	pass1 / ClassDeclaration |
	pass1 / InterfaceDeclaration |
	pass1 / AggregateDeclaration |
	pass1 / Declaration |
	pass1 / Constructor |
	pass1 / Destructor |
	pass1 / Invariant |
	pass1 / UnitTest |
	pass1 / StaticConstructor |
	pass1 / StaticDestructor |
	pass1 / DebugSpecification |
	pass1 / VersionSpecification |
	pass1 / MixinDeclaration |
	error / opSemicolon ;

DeclDefs:
	pass1 / DeclDef |
	error / DeclDef DeclDefs ;

ModuleDeclaration:
	error / KW_module ModuleName opSemicolon ;

ModuleName:
	error / Identifier |
	error / ModuleName opDot Identifier ;

MixinDeclaration:
	error / KW_mixin opLParen AssignExpression opRParen opSemicolon ;

Declaration:
	error / KW_typedef Decl |
	error / KW_alias Decl |
	error / Decl ;

Decl:
	error / StorageClasses Decl |
	error / BasicType Declarators opSemicolon |
	error / BasicType Declarator FunctionBody |
	error / AutoDeclaration ;

Declarators:
	error / DeclaratorInitializer |
	error / DeclaratorInitializer opComma DeclaratorIdentifierList ;

DeclaratorInitializer:
	error / Declarator |
	error / Declarator opAssignment Initializer ;

DeclaratorIdentifierList:
	error / DeclaratorIdentifier |
	error / DeclaratorIdentifier opComma DeclaratorIdentifierList ;

DeclaratorIdentifier:
	error / Identifier |
	error / Identifier opAssignment Initializer ;

Declarator:
	error / BasicType2 Declarator |
	error / Identifier |
	error / opLParen opRParen Declarator |
	error / Identifier DeclaratorSuffixes |
	error / opLParen opRParen Declarator DeclaratorSuffixes ;

DeclaratorSuffixes:
	error / DeclaratorSuffix |
	error / DeclaratorSuffix DeclaratorSuffixes ;

DeclaratorSuffix:
	error / opLBrace opRBrace |
	error / opLBrace Expression opRBrace |
	error / opLBrace Type opRBrace |
	error / Parameters ;

Typeof:
	error / KW_typeof opLParen Expression opRParen ;

StorageClasses:
	error / StorageClass |
	error / StorageClass StorageClasses ;

StorageClass:
	error / KW_abstract |
	error / KW_auto |
	error / KW_const |
	error / KW_deprecated |
	error / KW_extern |
	error / KW_final |
	error / KW_invariant |
	error / KW_override |
	error / KW_scope |
	error / KW_static |
	error / KW_synchronized ;

Declarator2:
	error / BasicType2 Declarator2 |
	error / opLParen Declarator2 opRParen |
	error / opLParen Declarator2 opRParen DeclaratorSuffixes ;

Parameters:
	error / opLParen ParameterList opRParen |
	error / opLParen opRParen ;

ParameterList:
	error / Parameter |
	error / Parameter opComma ParameterList |
	error / Parameter opEllipsis |
	error / opEllipsis ;

Parameter:
	error / Declarator |
	error / Declarator opAssignment AssignExpression |
	error / InOut Declarator |
	error / InOut Declarator opAssignment AssignExpression ;

InOut:
	error / KW_in |
	error / KW_out |
	error / KW_ref |
	error / KW_lazy ;

AutoDeclaration:
	error / StorageClasses Identifier opAssignment AssignExpression opSemicolon ;

DeclarationBlock2:
	error / DeclDef |
	error / opLBracket opRBracket |
	error / opLBracket DeclDefs opRBracket ;

Pragma:
	error / KW_pragma opLParen Identifier opRParen |
	error / KW_pragma opLParen Identifier opComma ExpressionList opRParen ;

Statement:
	error / opSemicolon |
	error / NonEmptyStatement |
	error / ScopeBlockStatement ;

NoScopeNonEmptyStatement:
	error / NonEmptyStatement |
	error / BlockStatement ;

NoScopeStatement:
	error / opSemicolon |
	error / NonEmptyStatement |
	error / BlockStatement ;

NonEmptyOrScopeBlockStatement:
	error / NonEmptyStatement |
	error / ScopeBlockStatement ;

NonEmptyStatement:
	error / LabeledStatement |
	error / ExpressionStatement |
	error / DeclarationStatement |
	error / IfStatement |
	error / ConditionalStatement |
	error / WhileStatement |
	error / DoStatement |
	error / ForStatement |
	error / ForeachStatement |
	error / SwitchStatement |
	error / CaseStatement |
	error / DefaultStatement |
	error / ContinueStatement |
	error / BreakStatement |
	error / ReturnStatement |
	error / GotoStatement |
	error / WithStatement |
	error / SynchronizedStatement |
	error / TryStatement |
	error / ScopeGuardStatement |
	error / ThrowStatement |
	error / VolatileStatement |
	error / AsmStatement |
	error / PragmaStatement |
	error / MixinStatement ;

ScopeStatement:
	error / NonEmptyStatement |
	error / BlockStatement ;

ScopeBlockStatement:
	error / BlockStatement ;

LabeledStatement:
	error / Identifier opCollin NoScopeStatement ;

BlockStatement:
	error / opLBracket opRBracket |
	error / opLBracket StatementList opRBracket ;

StatementList:
	error / Statement |
	error / Statement StatementList ;

ExpressionStatement:
	error / Expression opSemicolon ;

DeclarationStatement:
	error / Declaration ;

IfStatement:
	error / KW_if opLParen IfCondition opRParen ThenStatement |
	error / KW_if opLParen IfCondition opRParen ThenStatement KW_else ElseStatement ;

IfCondition:
	error / Expression |
	error / KW_auto Identifier opAssignment Expression |
	error / Declarator opAssignment Expression ;

ThenStatement:
	error / ScopeStatement ;

ElseStatement:
	error / ScopeStatement ;

WhileStatement:
	error / KW_while opLParen Expression opRParen ScopeStatement ;

DoStatement:
	error / KW_do ScopeStatement KW_while opLParen Expression opRParen ;

ForStatement:
	error / KW_for opLParen Initialize Test opSemicolon Increment opRParen ScopeStatement ;

Initialize:
	error / opSemicolon |
	error / NoScopeNonEmptyStatement ;

Test:
	error / empty |
	error / Expression ;

Increment:
	error / empty |
	error / Expression ;

ForeachStatement:
	error / Foreach opLParen ForeachTypeList opSemicolon Aggregate opRParen ScopeStatement ;

Foreach:
	error / KW_foreach |
	error / KW_foreach_reverse ;

ForeachTypeList:
	error / ForeachType |
	error / ForeachType opComma ForeachTypeList ;

ForeachType:
	error / KW_ref Type Identifier |
	error / Type Identifier |
	error / KW_ref Identifier |
	error / Identifier ;

Aggregate:
	error / Expression |
	error / Tuple ;

SwitchStatement:
	error / KW_switch opLParen Expression opRParen ScopeStatement ;

CaseStatement:
	error / KW_case ExpressionList opCollin Statement ;

DefaultStatement:
	error / KW_default opCollin Statement ;

ContinueStatement:
	error / KW_continue opSemicolon |
	error / KW_continue Identifier opSemicolon ;

BreakStatement:
	error / KW_break opSemicolon |
	error / KW_break Identifier opSemicolon ;

ReturnStatement:
	error / KW_return opSemicolon |
	error / KW_return Expression opSemicolon ;

GotoStatement:
	error / KW_goto Identifier opSemicolon |
	error / KW_goto KW_default opSemicolon |
	error / KW_goto KW_case opSemicolon |
	error / KW_goto KW_case Expression opSemicolon ;

WithStatement:
	error / KW_with opLParen Expression opRParen ScopeStatement |
	error / KW_with opLParen Symbol opRParen ScopeStatement |
	error / KW_with opLParen TemplateInstance opRParen ScopeStatement ;

SynchronizedStatement:
	error / KW_synchronized ScopeStatement |
	error / KW_synchronized opLParen Expression opRParen ScopeStatement ;

TryStatement:
	error / KW_try ScopeStatement Catches |
	error / KW_try ScopeStatement Catches FinallyStatement |
	error / KW_try ScopeStatement FinallyStatement ;

Catches:
	error / LastCatch |
	error / Catch |
	error / Catch Catches ;

LastCatch:
	error / KW_catch NoScopeNonEmptyStatement ;

Catch:
	error / KW_catch opLParen CatchParameter opRParen NoScopeNonEmptyStatement ;

FinallyStatement:
	error / KW_finally NoScopeNonEmptyStatement ;

ThrowStatement:
	error / KW_throw Expression opSemicolon ;

ScopeGuardStatement:
	error / KW_scope opLParen PKW_exit opRParen NonEmptyOrScopeBlockStatement |
	error / KW_scope opLParen PKW_success opRParen NonEmptyOrScopeBlockStatement |
	error / KW_scope opLParen PKW_failure opRParen NonEmptyOrScopeBlockStatement ;

VolatileStatement:
	error / KW_volatile Statement |
	error / KW_volatile opSemicolon ;

AsmStatement:
	error / KW_asm opLBracket opRBracket |
	error / KW_asm opLBracket AsmInstructionList opRBracket ;

AsmInstructionList:
	error / AsmInstruction opSemicolon |
	error / AsmInstruction opSemicolon AsmInstructionList ;

PragmaStatement:
	error / Pragma NoScopeStatement ;

MixinStatement:
	error / KW_mixin opLParen AssignExpression opRParen opSemicolon ;

InterfaceDeclaration:
	error / KW_interface Identifier InterfaceBody |
	error / KW_interface Identifier opCollin SuperInterfaces InterfaceBody ;

SuperInterfaces:
	error / Identifier |
	error / Identifier opComma SuperInterfaces ;

InterfaceBody:
	error / opLBracket DeclDefs opRBracket ;

TemplateDeclaration:
	error / KW_template TemplateIdentifier opLParen TemplateParameterList opRParen opLBracket DeclDefs opRBracket ;

TemplateIdentifier:
	error / Identifier ;

TemplateInstance:
	error / TemplateIdentifer opBang opLParen TemplateArgumentList opRParen ;

TemplateArgumentList:
	error / TemplateArgument |
	error / TemplateArgument opComma TemplateArgumentList ;

TemplateArgument:
	error / Type |
	error / AssignExpression |
	error / Symbol ;

ClassTemplateDeclaration:
	error / KW_class Identifier opLParen TemplateParameterList opRParen BaseClassList_Comma ClassBody ;

FunctionTemplateDeclaration:
	error / Type Identifier opLParen TemplateParameterList opRParen opLParen FunctionParameterList opRParen FunctionBody ;

TemplateMixin:
	error / KW_mixin TemplateIdentifier opSemicolon |
	error / KW_mixin TemplateIdentifier MixinIdentifier opSemicolon |
	error / KW_mixin TemplateIdentifier opBang opLParen TemplateArgumentList opRParen opSemicolon |
	error / KW_mixin TemplateIdentifier opBang opLParen TemplateArgumentList opRParen MixinIdentifier opSemicolon ;

MixinIdentifier:
	error / Identifier ;

ConditionalDeclaration:
	error / Condition DeclarationBlock |
	error / Condition DeclarationBlock KW_else DeclarationBlock |
	error / Condition opCollin Declarations ;

DeclarationBlock:
	error / Declaration |
	error / opLBracket Declarations opRBracket |
	error / opLBracket opRBracket ;

Declarations:
	error / Declaration |
	error / Declaration Declarations ;

ConditionalStatement:
	error / Condition NoScopeNonEmptyStatement |
	error / Condition NoScopeNonEmptyStatement KW_else NoScopeNonEmptyStatement ;

Condition:
	error / VersionCondition |
	error / DebugCondition |
	error / StaticIfCondition ;

VersionCondition:
	error / KW_version opLParen Integer opRParen |
	error / KW_version opLParen Identifier opRParen ;

VersionSpecification:
	error / KW_version opAssignment Identifier opSemicolon |
	error / KW_version opAssignment Integer opSemicolon ;

DebugCondition:
	error / KW_debug |
	error / KW_debug opLParen Integer opRParen |
	error / KW_debug opLParen Identifier opRParen ;

DebugSpecification:
	error / KW_debug opAssignment Identifier opSemicolon |
	error / KW_debug opAssignment Integer opSemicolon ;

StaticIfCondition:
	error / KW_static KW_if opLParen AssignExpression opRParen ;

StaticAssert:
	error / KW_static KW_assert opLParen AssignExpression opRParen opSemicolon |
	error / KW_static KW_assert opLParen AssignExpression opComma AssignExpression opRParen opSemicolon ;

ClassAllocator:
	error / KW_new Parameters FunctionBody ;

ClassDeallocator:
	error / KW_delete Parameters FunctionBody ;

NewAnonClassExpression:
	error / KW_new Parameters? KW_class Parameters? SuperClass? InterfaceClasses? ClassBody ;

CatchParameter:
	error / Type Identifier ;

DeleteExpression:
	error / KW_delete UnaryExpression ;

ExpressionList:
	error / ExpressionList opComma Expression ;

FunctionParameterList:
	error / Declarators ;

TemplateIdentifer:
	error / Identifier ;

IntegerExpression:
	error / Expression ;
FunctionBody:
	error / BlockStatement |
	error / BodyStatement |
	error / InStatement BodyStatement |
	error / OutStatement BodyStatement |
	error / InStatement OutStatement BodyStatement |
	error / OutStatement InStatement BodyStatement ;

InStatement:
	error / KW_in BlockStatement ;

OutStatement:
	error / KW_out BlockStatement |
	error / KW_out opLParen Identifier opRParen BlockStatement ;

BodyStatement:
	error / KW_body BlockStatement ;
ImportDeclaration:
	error / KW_import ImportList opSemicolon |
	error / KW_static KW_import ImportList opSemicolon ;

ImportList:
	error / Import |
	error / ImportBindings |
	error / Import opComma ImportList ;

Import:
	error / ModuleName |
	error / ModuleAliasIdentifier opAssignment ModuleName ;

ImportBindings:
	error / Import opCollin ImportBindList ;

ImportBindList:
	error / ImportBind |
	error / ImportBind opComma ImportBindList ;

ImportBind:
	error / Identifier |
	error / Identifier opAssignment ;

ModuleAliasIdentifier:
	error / Identifier ;

Initializer:
	error / KW_void |
	error / NonVoidInitializer ;

NonVoidInitializer:
	error / AssignExpression |
	error / ArrayInitializer |
	error / StructInitializer ;

ArrayInitializer:
	error / opLBrace opRBrace |
	error / opLBrace ArrayMemberInitializations opRBrace ;

ArrayMemberInitializations:
	error / ArrayMemberInitialization |
	error / ArrayMemberInitialization opComma |
	error / ArrayMemberInitialization opComma ArrayMemberInitializations ;

ArrayMemberInitialization:
	error / NonVoidInitializer |
	error / AssignExpression opCollin NonVoidInitializer ;

StructInitializer:
	error / opLBracket opRBracket |
	error / opLBracket StructMemberInitializers opRBracket ;

StructMemberInitializers:
	error / StructMemberInitializer |
	error / StructMemberInitializer opComma |
	error / StructMemberInitializer opComma StructMemberInitializers ;

StructMemberInitializer:
	error / NonVoidInitializer |
	error / Identifier opCollin NonVoidInitializer ;
CmpExpression:
	pass1 / EqualExpression |
	pass1 / IdentityExpression |
	pass1 / RelExpression |
	pass1 / InExpression ;

EqualExpression:
	pass1 / ShiftExpression |
	error / ShiftExpression opEqual ShiftExpression |
	error / ShiftExpression opNotEqual ShiftExpression |
	error / ShiftExpression KW_is ShiftExpression |
	error / ShiftExpression opBang KW_is ShiftExpression ;

RelExpression:
	pass1 / ShiftExpression |
	error / ShiftExpression opLessThan ShiftExpression |
	error / ShiftExpression opLessEqual ShiftExpression |
	error / ShiftExpression opGreaterThan ShiftExpression |
	error / ShiftExpression opGreaterEqual ShiftExpression |
	error / ShiftExpression opNotLessGraterEqual ShiftExpression |
	error / ShiftExpression opNotLessGrater ShiftExpression |
	error / ShiftExpression opLessGreater ShiftExpression |
	error / ShiftExpression opLessGraterEqual ShiftExpression |
	error / ShiftExpression opNotGrater ShiftExpression |
	error / ShiftExpression opNotGraterEqual ShiftExpression |
	error / ShiftExpression opNotLess ShiftExpression |
	error / ShiftExpression opNotLessEqual ShiftExpression ;

InExpression:
	error / ShiftExpression KW_in ShiftExpression ;

ShiftExpression:
	pass1 / AddExpression |
	error / ShiftExpression opLShift AddExpression |
	error / ShiftExpression opRShift AddExpression |
	error / ShiftExpression op3RShift AddExpression ;

IdentityExpression:
	error / ShiftExpression KW_is ShiftExpression |
	error / ShiftExpression opBang KW_is ShiftExpression ;
AddExpression:
	pass1 / MulExpression |
	error / AddExpression opPlus MulExpression |
	error / AddExpression opMinus MulExpression |
	pass1 / CatExpression ;

CatExpression:
	error / AddExpression opTilde MulExpression ;

MulExpression:
	pass1 / UnaryExpression |
	error / MulExpression opStar UnaryExpression |
	error / MulExpression opDivision UnaryExpression |
	error / MulExpression opModulo UnaryExpression ;

UnaryExpression:
	pass1 / PostfixExpression |
	error / opAmpersand UnaryExpression |
	error / opPlusPlus UnaryExpression |
	error / opMinusMinus UnaryExpression |
	error / opStar UnaryExpression |
	error / opMinus UnaryExpression |
	error / opPlus UnaryExpression |
	error / opBang UnaryExpression |
	error / opTilde UnaryExpression |
	error / opLParen Type opRParen opDot Identifier |
	pass1 / NewExpression |
	pass1 / DeleteExpression |
	pass1 / CastExpression |
	pass1 / NewAnonClassExpression ;

CastExpression:
	error / KW_cast opLParen Type opRParen UnaryExpression ;
Expression :
	pass1 / AssignExpression |
	error / AssignExpression opComma Expression ;

AssignExpression :
	pass1 / ConditionalExpression |
	error / ConditionalExpression opAssignment      AssignExpression |
	error / ConditionalExpression opPlusAssign      AssignExpression |
	error / ConditionalExpression opMinusAssign     AssignExpression |
	error / ConditionalExpression opStarAssign      AssignExpression |
	error / ConditionalExpression opDivideAssign    AssignExpression |
	error / ConditionalExpression opModuloAssign    AssignExpression |
	error / ConditionalExpression opAmpersandAssign AssignExpression |
	error / ConditionalExpression opPipeAssign      AssignExpression |
	error / ConditionalExpression opCarrotAssign    AssignExpression |
	error / ConditionalExpression opTildeAssign     AssignExpression |
	error / ConditionalExpression opLShiftAssign    AssignExpression |
	error / ConditionalExpression opRShiftAssign    AssignExpression |
	error / ConditionalExpression op3RShiftAssign   AssignExpression ;

ConditionalExpression:
	pass1 / OrOrExpression |
	error / OrOrExpression opQuestion Expression opCollin ConditionalExpression ;

OrOrExpression:
	pass1 / AndAndExpression |
	error / OrOrExpression opLogicOr AndAndExpression ;

AndAndExpression:
	pass1 / OrExpression |
	error / AndAndExpression opLogicAnd OrExpression ;

OrExpression:
	pass1 / XorExpression |
	error / OrExpression opPipe XorExpression ;

XorExpression:
	pass1 / AndExpression |
	error / XorExpression opCarrot AndExpression ;

AndExpression :
	pass1 / CmpExpression |
	error / AndExpression opAmpersand CmpExpression ;
AggregateDeclaration:
	error / Tag opLBracket DeclDefs opRBracket |
	error / Tag Identifier StructBody |
	error / Tag Identifier opSemicolon ;

Tag:
	error / KW_struct |
	error / KW_union ;

StructBody:
	error / opLBracket opRBracket |
	error / opLBracket StructBodyDeclarations opRBracket ;

StructBodyDeclarations:
	error / StructBodyDeclaration |
	error / StructBodyDeclaration StructBodyDeclarations ;

StructBodyDeclaration:
	error / Declaration |
	error / StaticConstructor |
	error / StaticDestructor |
	error / Invariant |
	error / UnitTest |
	error / StructAllocator |
	error / StructDeallocator ;

StructAllocator:
	error / ClassAllocator ;

StructDeallocator:
	error / ClassDeallocator ;

TemplateParameterList:
	error / TemplateParameter |
	error / TemplateParameter opComma TemplateParameterList ;

TemplateParameter:
	error / TemplateTypeParameter |
	error / TemplateValueParameter |
	error / TemplateAliasParameter |
	error / TemplateTupleParameter ;

TemplateTypeParameter:
	error / Identifier |
	error / Identifier TemplateTypeParameterSpecialization |
	error / Identifier TemplateTypeParameterDefault |
	error / Identifier TemplateTypeParameterSpecialization TemplateTypeParameterDefault ;

TemplateTypeParameterSpecialization:
	error / opCollin Type ;

TemplateTypeParameterDefault:
	error / opAssignment Type ;

TemplateAliasParameter:
	error / KW_alias Identifier |
	error / KW_alias Identifier TemplateAliasParameterSpecialization |
	error / KW_alias Identifier TemplateAliasParameterDefault |
	error / KW_alias Identifier TemplateAliasParameterSpecialization TemplateAliasParameterDefault ;

TemplateAliasParameterSpecialization:
	error / opCollin Type ;

TemplateAliasParameterDefault:
	error / opAssignment Type ;

TemplateTupleParameter:
	error / Identifier opEllipsis ;

TemplateValueParameter:
	error / Declaration |
	error / Declaration TemplateValueParameterSpecialization |
	error / Declaration TemplateValueParameterDefault |
	error / Declaration TemplateValueParameterSpecialization TemplateValueParameterDefault ;

TemplateValueParameterSpecialization:
	error / opCollin ConditionalExpression ;

TemplateValueParameterDefault:
	error / opAssignment ConditionalExpression ;

NewExpression:
	error / NewArguments Type opLBrace AssignExpression opRBrace |
	error / NewArguments Type opLParen ArgumentList opRParen |
	error / NewArguments Type |
	error / NewArguments ClassArguments BaseClassList? opLBracket DeclDefs opRBracket ;

NewArguments:
	error / KW_new opLParen ArgumentList opRParen |
	error / KW_new opLParen opRParen |
	pass1 / KW_new ;

ClassArguments:
	error / KW_class opLParen ArgumentList opRParen |
	error / KW_class opLParen opRParen |
	pass1 / KW_class ;

ArgumentList:
	pass1 / AssignExpression |
	error / AssignExpression opComma ArgumentList ;

IndexExpression :
	error / PostfixExpression opLBrace ArgumentList opRBrace ;

SliceExpression:
	error / PostfixExpression opLBrace opRBrace |
	error / PostfixExpression opLBrace AssignExpression opDoubleDot AssignExpression opRBrace ;


Type:
	pass1 / BasicType |
	error / BasicType Declarator2 ;

BasicType:
	pass1 / KW_bool |
	pass1 / KW_byte |
	pass1 / KW_ubyte |
	pass1 / KW_short |
	pass1 / KW_ushort |
	pass1 / KW_int |
	pass1 / KW_uint |
	pass1 / KW_long |
	pass1 / KW_ulong |
	pass1 / KW_char |
	pass1 / KW_wchar |
	pass1 / KW_dchar |
	pass1 / KW_float |
	pass1 / KW_double |
	pass1 / KW_real |
	pass1 / KW_ifloat |
	pass1 / KW_idouble |
	pass1 / KW_ireal |
	pass1 / KW_cfloat |
	pass1 / KW_cdouble |
	pass1 / KW_creal |
	pass1 / KW_void |
	error / opDot IdentifierList |
	pass1 / IdentifierList |
	pass1 / Typeof |
	error / Typeof opDot IdentifierList ;

BasicType2:
	pass1 / opStar |
	error/ opLBrace opRBrace |
	error/ opLBrace Expression opRBrace |
	error/ opLBrace Type opRBrace |
	error/ KW_delegate Parameters |
	error/ KW_function Parameters ;

IdentifierList:
	pass1 / Identifier |
	error/ Identifier opDot IdentifierList |
	pass1 / TemplateInstance |
	error/ TemplateInstance opDot IdentifierList ;

