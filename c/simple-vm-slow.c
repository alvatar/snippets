/*
 * Simple Virtual Machine: simvm source
 *	By Benjamin Kittridge. Copyright (C) 2010, All rights reserved.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

////////////////////////////////////////////////////////////////////////////////
// Class:       simvm
// Description: Simple Virtual Machine

////////////////////////////////////////////////////////////////////////////////
// Section:     Enums

enum {
	OP_NOP=0,	OP_LDI,		OP_LDR,
	OP_STO,		OP_ADD,		OP_SUB,
	OP_MUL,		OP_DIV,		OP_MOD,
	OP_ORR,		OP_XOR,		OP_AND,
	OP_SHL,		OP_SHR,		OP_NOT,
	OP_NEG,		OP_CMP,		OP_BEQ,
	OP_BNE,		OP_BGT,		OP_BLT,
	OP_BGE,		OP_BLE,		OP_CAL,
	OP_JMP,		OP_RET,		OP_EOF
};

////////////////////////////////////////////////////////////////////////////////
// Section:     Structs

typedef struct inst {
	int32_t op, arg;
	void *jmp;
} inst, *inst_p;

////////////////////////////////////////////////////////////////////////////////
// Section:     Virtual Machine

int vm(inst *i) {
	static void *optable[]={
		[OP_NOP]=&&op_nop,	[OP_LDI]=&&op_ldi,	[OP_LDR]=&&op_ldr,
		[OP_STO]=&&op_sto,	[OP_ADD]=&&op_add,	[OP_SUB]=&&op_sub,
		[OP_MUL]=&&op_mul,	[OP_DIV]=&&op_div,	[OP_MOD]=&&op_mod,
		[OP_ORR]=&&op_orr,	[OP_XOR]=&&op_xor,	[OP_AND]=&&op_and,
		[OP_SHL]=&&op_shl,	[OP_SHR]=&&op_shr,	[OP_NOT]=&&op_not,
		[OP_NEG]=&&op_neg,	[OP_CMP]=&&op_cmp,	[OP_BEQ]=&&op_beq,
		[OP_BNE]=&&op_bne,	[OP_BGT]=&&op_bgt,	[OP_BLT]=&&op_blt,
		[OP_BGE]=&&op_bge,	[OP_BLE]=&&op_ble,	[OP_CAL]=&&op_cal,
		[OP_JMP]=&&op_jmp,	[OP_RET]=&&op_ret,	[OP_EOF]=&&op_eof
	};
	int r[4], f, s[32], *sp=s;
	inst *ip;
	
	for (ip=i;;ip++) {
		ip->jmp=optable[ip->op];
		if (ip->op == OP_EOF)
			break;
	}

	goto *(ip=i)->jmp;
	
	#define NEXT()	__asm__("jmp *%0"::"r"((++ip)->jmp)); goto *ip->jmp
	
	op_nop:				NEXT();
	op_ldi:	*sp++=ip->arg;		NEXT();
	op_ldr:	*sp++=r[ip->arg];	NEXT();
	op_sto:	r[ip->arg]=*--sp;	NEXT();
	op_add:	sp--, sp[-1]+=*sp;	NEXT();
	op_sub:	sp--, sp[-1]-=*sp;	NEXT();
	op_mul:	sp--, sp[-1]*=*sp;	NEXT();
	op_div:	sp--, sp[-1]/=*sp;	NEXT();
	op_mod:	sp--, sp[-1]%=*sp;	NEXT();
	op_orr:	sp--, sp[-1]|=*sp;	NEXT();
	op_xor:	sp--, sp[-1]^=*sp;	NEXT();
	op_and:	sp--, sp[-1]&=*sp;	NEXT();
	op_shl:	sp--, sp[-1]<<=*sp;	NEXT();
	op_shr:	sp--, sp[-1]>>=*sp;	NEXT();
	op_not:	sp[-1]=!sp[-1];		NEXT();
	op_neg: sp[-1]=-sp[-1];		NEXT();
	op_cmp:	sp-=2, f=*sp-sp[1];	NEXT();
	op_beq:	if(!f)  ip=i+ip->arg-1;	NEXT();
	op_bne:	if(f)   ip=i+ip->arg-1;	NEXT();
	op_bgt:	if(f>0) ip=i+ip->arg-1;	NEXT();
	op_blt:	if(f<0) ip=i+ip->arg-1;	NEXT();
	op_bge:	if(f>=0)ip=i+ip->arg-1;	NEXT();
	op_ble:	if(f<=0)ip=i+ip->arg-1;	NEXT();
	op_cal:	*sp++=ip-i;
	op_jmp:	ip=i+ip->arg-1;		NEXT();
	op_ret:	ip=(*--sp)+i;		NEXT();
	op_eof:	printf("%d\n",r[0]);	return;
}

////////////////////////////////////////////////////////////////////////////////
// Section:     Main

int main(int argc, char **argv) {
	inst program[]={
		{ OP_LDI,     0 },	//  0: load 0
		{ OP_STO,     0 },	//  1: store r0
		{ OP_LDI,     0 },	//  2: load 0
		{ OP_STO,     1 },	//  3: store r1
		{ OP_JMP,    26 },	//  4: jmp 26
		{ OP_LDI,     0 },	//  5: load 0
		{ OP_STO,     2 },	//  6: store r2
		{ OP_JMP,    18 },	//  7: jmp 18
		{ OP_LDR,     0 },	//  8: load r0
		{ OP_LDR,     1 },	//  9: load r1
		{ OP_LDR,     2 },	// 10: load r2
		{ OP_MUL        }, 	// 11: mul
		{ OP_ADD        },	// 12: add
		{ OP_STO,     0 },	// 13: store r0
		{ OP_LDR,     2 },	// 14: load r2
		{ OP_LDI,     1 },	// 15: load 1
		{ OP_ADD        },	// 16: add
		{ OP_STO,     2 },	// 17: store r2
		{ OP_LDR,     2 },	// 18: load r2
		{ OP_LDI, 10000 },	// 19: load 10000
		{ OP_CMP        },	// 20: cmp
		{ OP_BLT,     8 },	// 21: blt 8
		{ OP_LDR,     1 },	// 22: load r1
		{ OP_LDI,     1 },	// 23: load 1
		{ OP_ADD        },	// 24: add
		{ OP_STO,     1 },	// 25: store r1
		{ OP_LDR,     1 },	// 26: load r1
		{ OP_LDI, 10000 },	// 27: load 10000
		{ OP_CMP        },	// 28: cmp
		{ OP_BLT,     5 },	// 29: blt 5
		{ OP_EOF        }, 	// 30: eof
	};
	
	vm(program);
	return 0;
}

