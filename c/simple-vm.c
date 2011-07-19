/*
 * Simple Virtual Machine: simvm source
 *	By Benjamin Kittridge. Copyright (C) 2010, All rights reserved.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>

////////////////////////////////////////////////////////////////////////////////
// Class:       simvm
// Description: Simple Virtual Machine

////////////////////////////////////////////////////////////////////////////////
// Section:     Enums

enum {
	op_nop=0,	op_ldi,		op_ld0,
	op_ld1,		op_ld2,		op_ld3,
	op_st0,		op_st1,		op_st2,
	op_st3,		op_add,		op_sub,
	op_mul,		op_div,		op_mod,
	op_orr,		op_xor,		op_and,
	op_neg,		op_cmp,		op_beq,
	op_bne,		op_bgt,		op_blt,
	op_bge,		op_ble,		op_cal,
	op_jmp,		op_ret,		op_eof
};

////////////////////////////////////////////////////////////////////////////////
// Section:     Structs

typedef struct inst {
	int32_t op, arg, len, pos;
} inst, *inst_p;

////////////////////////////////////////////////////////////////////////////////
// Section:     Virtual Machine

#if __SIZEOF_LONG__ == 4
	#define LONG
#elif __SIZEOF_LONG__ == 8
	#define LONG 0x48,
#else
	#error "Unsupported bit size"
#endif

#define INS(op,pad,arg,jmp,bytes...) \
	[op]={ sizeof((char[]){ bytes })+pad, arg, jmp, { bytes } }

int vm(inst *i) {
	struct {
		int32_t size, arg, jmp;
		char data[16];
	} *op, optable[]={
		INS(op_nop, 0, 0, 0, 0x90),
		INS(op_ldi, 4, 1, 0, 0x68),
		INS(op_ld0, 0, 0, 0, 0x53),
		INS(op_ld1, 0, 0, 0, 0x56),
		INS(op_ld2, 0, 0, 0, 0x57),
		INS(op_ld3, 0, 0, 0, 0x51),
		INS(op_st0, 0, 0, 0, 0x5b),
		INS(op_st1, 0, 0, 0, 0x5e),
		INS(op_st2, 0, 0, 0, 0x5f),
		INS(op_st3, 0, 0, 0, 0x59),
		INS(op_add, 0, 0, 0, 0x58, LONG 0x01, 0x04, 0x24),
		INS(op_sub, 0, 0, 0, 0x58, LONG 0x29, 0x04, 0x24),
		INS(op_mul, 0, 0, 0, 0x5a, LONG 0x8b, 0x04, 0x24, LONG 0x0f,
			0xaf, 0xc2, LONG 0x89, 0x04, 0x24),
		INS(op_div, 0, 0, 0, 0x5a, LONG 0x89, 0xd0, LONG 0xc1, 0xfa,
			0x1f, LONG 0xf7, 0x3c, 0x24, LONG 0x89, 0x04, 0x24),
		INS(op_mod, 0, 0, 0, 0x5a, LONG 0x89, 0xd0, LONG 0xc1, 0xfa,
			0x1f, LONG 0xf7, 0x3c, 0x24, LONG 0x89, 0x14, 0x24),
		INS(op_orr, 0, 0, 0, 0x58, LONG 0x09, 0x04, 0x24),
		INS(op_xor, 0, 0, 0, 0x58, LONG 0x31, 0x04, 0x24),
		INS(op_and, 0, 0, 0, 0x58, LONG 0x21, 0x04, 0x24),
		INS(op_neg, 0, 0, 0, 0x58, LONG 0xf7, 0xd8, 0x50),
		INS(op_cmp, 0, 0, 0, 0x58, 0x5a, LONG 0x39, 0xc2),
		INS(op_beq, 4, 0, 2, 0x0f, 0x84),
		INS(op_bne, 4, 0, 2, 0x0f, 0x85),
		INS(op_bgt, 4, 0, 2, 0x0f, 0x8f),
		INS(op_blt, 4, 0, 2, 0x0f, 0x8c),
		INS(op_bge, 4, 0, 2, 0x0f, 0x8d),
		INS(op_ble, 4, 0, 2, 0x0f, 0x8e),
		INS(op_cal, 4, 0, 1, 0xe8),
		INS(op_jmp, 4, 0, 1, 0xe9),
		INS(op_ret, 0, 0, 0, 0xc3),
		INS(op_eof, 0, 0, 0, 0xc3),
	};
	register int r0 asm ("ebx");
	int m, *g;
	char *pn, *pr;
	inst *ip;
	
	for (m=0,ip=i;;ip++) {
		ip->len=optable[ip->op].size;
		ip->pos=m;
		m+=ip->len;
		if (ip->op == op_eof)
			break;
	}
	
	if (!(pn=mmap(0,m,PROT_READ|PROT_WRITE|PROT_EXEC,MAP_PRIVATE|MAP_ANON,-1,0)))
		return 0;
	
	for (pr=pn,ip=i;;ip++) {
		op=&optable[ip->op];
		memcpy(pr,op->data,ip->len);
		if (op->arg) {
			g=(int*)(pr+op->arg);
			*g=ip->arg;
		}
		if (op->jmp) {
			g=(int*)(pr+op->jmp);
			*g=(i[ip->arg].pos-ip->pos)-op->size;
		}
		pr+=ip->len;
		if (ip->op == op_eof)
			break;
	}
	
	((void(*)())pn)();
	printf("%d\n",r0);
	return 0;
}

////////////////////////////////////////////////////////////////////////////////
// Section:     Main

int main(int argc, char **argv) {
	/*
	 * This example program is compiled from the following:
	 * 
	 * 	int s, i, j;
	 *	for (s=0,i=0;i<10000;i++) {
	 *		for (j=0;j<10000;j++)
	 *			s+=i*j;
	 *	}
	 *	printf("%d\n",s);
	 * 
	 */
	inst program[]={
		{ op_ldi,     0 },	//  0: load 0
		{ op_st0        },	//  1: store r0
		{ op_ldi,     0 },	//  2: load 0
		{ op_st1        },	//  3: store r1
		{ op_jmp,    26 },	//  4: jmp 26
		{ op_ldi,     0 },	//  5: load 0
		{ op_st2        },	//  6: store r2
		{ op_jmp,    18 },	//  7: jmp 18
		{ op_ld0        },	//  8: load r0
		{ op_ld1        },	//  9: load r1
		{ op_ld2        },	// 10: load r2
		{ op_mul        }, 	// 11: mul
		{ op_add        },	// 12: add
		{ op_st0        },	// 13: store r0
		{ op_ld2        },	// 14: load r2
		{ op_ldi,     1 },	// 15: load 1
		{ op_add        },	// 16: add
		{ op_st2        },	// 17: store r2
		{ op_ld2        },	// 18: load r2
		{ op_ldi, 10000 },	// 19: load 10000
		{ op_cmp        },	// 20: cmp
		{ op_blt,     8 },	// 21: blt 8
		{ op_ld1        },	// 22: load r1
		{ op_ldi,     1 },	// 23: load 1
		{ op_add        },	// 24: add
		{ op_st1        },	// 25: store r1
		{ op_ld1        },	// 26: load r1
		{ op_ldi, 10000 },	// 27: load 10000
		{ op_cmp        },	// 28: cmp
		{ op_blt,     5 },	// 29: blt 5
		{ op_eof        }, 	// 30: eof
	};
	
	vm(program);
	return 0;
}
