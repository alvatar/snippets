/*******************************************************************************
        copyright:      Copyright (c) 2007  (yidabu  g m a i l at com) All rights reserved

        license:        BSD style: $(LICENSE)

        version:        Initial release: 2007

        author:         modified by yidabu ( D China : http://www.d-programming-language-china.org/ )
        original post:  http://www.fenglin.info/gen_ex/53/Algorithm/323345.html

*******************************************************************************/

/**
    TODO:
        1. very low  efficiency of opDiv now.
    Examples:
    ---
        auto a = new Bignumber("12345678901234567890123456789012345678901234567890");
        auto b = new Bignumber("12345678901234567890123456789012345678901234567890");
        auto c = a - b;
        Trace("a - b = ")( c.toUtf8() ).newline;
        
        c = a + b -b;
        Trace("a + b - b = ")( c.toUtf8() ).newline;
        
        c = a * b / b;      
        Trace("a * b / b = ")( c.toUtf8() ).newline;
        
        c = a / b * b;
        Trace("a / b * b = ")( c.toUtf8() ).newline;    
    ---
*/
// module dwin.lab.Bignumber;
module Bignumber.Bignumber; // it's in the Bignumber folder, it has the Bignumber package. Easy as pie.

version(Tango) {
    import tango.text.convert.Integer;
    debug( UnitTest ) import tango.util.log.Trace;  
} else {
    import std.math, std.string, std.stdio;
    struct _Trace { _Trace opCall(T)(T t) { writef(t); return *this; } void newline() { writefln(); } }
    struct Trace { static _Trace opCall(T)(T t) { writef(t); _Trace bogus; return bogus; } }
}

class Bignumber 
{ 
public: 
    enum : uint {BASELEN = 3U,BASE = 1_000_000U}; 
    enum : uint {MAX = 100U};      //i'm thinking change this to dynamic array if  possible.... 
    uint data[MAX];   //initialize as 0 
    uint len;         //initialize as 0  
    int  sign;        //initialize as 0   ; 0: positive; 1: negative 
	
    this (Bignumber b){ 
        Assign(b); 
    } 
    void Assign(Bignumber b) 
    { 
        len = b.len; 
        sign = b.sign; 
        data[]=b.data[]; 
    } 
    this(int num = 0){                 
        if(num == 0){len = 1;return;} 
        if(num < 0){ 
            sign = 1; 
            num = -num; 
        }        
        while(num > 0){            
            data[len++] = num % BASE; 
            num /= BASE; 
        } 
    } 
     
    this(char[] num){        
        if(num.length == 0) {len = 1;return;} 
        int beg = 0;       
        switch(num[0]){ 
			case '-': {sign = 1;}; 
			case '+': {++beg;} ;
			default : break; 
        } 
        int i = num.length - BASELEN; 
        for(; i >= beg; i -= BASELEN){     
            char[] tmp = num[i..i+BASELEN]; 
            data[len++] = atoi(tmp); 
            //data[len++] = std.conv.toUint(tmp); 
        } 
        i += BASELEN; 
        if(i>beg){ 
            char[] tmp = num[0..i]; 
            data[len++] = atoi(tmp); 
            //data[len++] = std.conv.toUint(tmp); 
        }        
    } 
     
    Bignumber opNeg() 
    { 
        Bignumber ret = new Bignumber(this); 
        ret.sign ^= 1; 
        return ret; 
    } 
         
    int opEquals(Bignumber b){ 
        if(sign != b.sign || len != b.len) return 0; 
        return data == b.data;         
    } 
     
    int absCmp(Bignumber b, Bignumber r){ 
        if(b.len>r.len) return 1; 
        else if(b.len<r.len) return 0;       
         
        for(int i = b.len-1;i >= 0; i--){ 
            if(b.data[i] > r.data[i]) 
                return 1; 
            else if(b.data[i] < r.data[i]) return 0; 
        } 
        return 0;        
    } 
     
    int opCmp(Bignumber b) 
    { 
        if(sign < b.sign) return 1; 
        else if(sign > b.sign) return 0;       
        if(sign == 0) return absCmp(this,b); 
        else return 1&absCmp(b,this);                
    } 
         
     // +
    Bignumber opAdd(Bignumber b)
    {
        if(sign!=b.sign) {
            b.sign ^= 1;
            Bignumber ret = this-b;
            b.sign ^= 1;
            return ret;
        }
        Bignumber ret = new Bignumber();
        ret.sign = sign;
        uint tmp = 0;
        uint i;
        for(i = 0; i<b.len && i<len; i++) {
            ret.data[i]= (data[i] + b.data[i] + tmp) % BASE;
            tmp = (data[i] + b.data[i] + tmp) / BASE;
        }
        void residual(uint len,uint data[]) {
            for(;i < len; i++) {
                ret.data[i] = (tmp + data[i]) % BASE;
                tmp = (tmp + data[i]) / BASE;
            } 
        } 
        if(i < len) 
            residual(len, data);                              
        else if(i < b.len) 
            residual(b.len, b.data); 
        ret.len = i;       
        if(tmp) ret.data[ret.len++] = tmp;         
        return ret;      
    } 
         
	// -
    Bignumber opSub(Bignumber b) 
    {                                 
        if(sign != b.sign){ 
            b.sign ^= 1; 
            Bignumber ret = this + b;    
            b.sign ^= 1; 
            return ret; 
        } 
        Bignumber big,small; 
        Bignumber ret = new Bignumber(); 
        Bignumber absSub(Bignumber big,Bignumber small){  //assume big > small           
            int tmp = 0; 
            uint i;          
            ret.len = big.len; 
            for(i = 0;i<small.len&&i<big.len;i++){  
           
                uint t = small.data[i]+tmp; 
                tmp = (t>big.data[i]); 
                ret.data[i]=(BASE&(-tmp))+big.data[i]-t; 
            }                    
            for(;i<big.len;i++){ 
                uint t = tmp; 
                tmp = (t>big.data[i]); 
				ret.data[i]=(BASE&(-tmp))+big.data[i]-t;                    
     
            } 
            while(ret.data[ret.len-1] == 0&&ret.len>1) 
				ret.len--;      
            return ret; 
        }                    
        if(absCmp(this,b)){          
            big = this;small = b; 
            ret.sign = sign; 
        }else{           
            small = this;big = b; 
            ret.sign = sign^1; 
        }                                                
        return absSub(big,small); 
    } 
         
	// *
    Bignumber opMul(Bignumber b){ 
        Bignumber ret = new Bignumber; 
        ret.sign = (b.sign^sign); 
        ret.len = (len+b.len-1); 
        uint tmp = 0; 
         
        for(uint i = 0;i<len;i++){ 
            for(int j = 0;j<b.len;j++){ 
                int c;               
                c = (ret.data[i+j]+data[i]*b.data[j]+tmp)/BASE; 
                //writefln(data[i],",",b.data[j],"; ",data[i]*b.data[j]); 
                 
				ret.data[i+j]=(ret.data[i+j]+data[i]*b.data[j]+tmp)%BASE;       
         
                tmp = c; 
            }            
        }                
        if(tmp) 
            ret.data[ret.len++]=tmp; 
        while(ret.data[ret.len-1] == 0&&ret.len>1) ret.
			len--;  
        return ret; 
    } 
         
	// /
    Bignumber opDiv(Bignumber b){ 
        Bignumber ret = new Bignumber; 
        if( len < b.len) return ret;        
        ret.sign = (b.sign ^ sign);        
        ret.len = len-b.len+1; 
        Bignumber tmp = new Bignumber(this);       
        tmp >>= (len-b.len+1);         
        int i = len-b.len; 
        for(; i >= 0; i--){ 
            tmp <<= 1;             
            tmp = tmp + new Bignumber(data[i]); 
            Bignumber c2 = new Bignumber(b); 
            int j = 1; 
            for(; c2 <= tmp && j < BASE; j++, c2 = c2+b){};
			//very low  efficiency,use binary search will be better 
			j--; 
            ret.data[i]=j;                       
            tmp = (tmp-c2+b);          
        } 
        while(ret.data[ret.len-1] ==0 && ret.len > 1) ret.len--;  
        return ret; 
    } 
     
     
    Bignumber opAddAssign(Bignumber b) 
    { 
        Bignumber tmp = this + b; 
        Assign(tmp); 
        return this; 
    } 
     
    Bignumber opSubAssign(Bignumber b) 
    { 
        Bignumber tmp = this - b; 
        Assign(tmp); 
        return this; 
    } 
     
    Bignumber opMulAssign(Bignumber b) 
    { 
        Bignumber tmp = this * b; 
        Assign(tmp); 
        return this; 
    } 
    Bignumber opDivAssign(Bignumber b) 
    { 
        Bignumber tmp = this / b; 
        Assign(tmp); 
        return this; 
    } 
    Bignumber opShr(uint s) 
    { 
        Bignumber ret = new Bignumber(this); 
        ret>>=s; 
        return ret; 
    } 
     
    Bignumber opShrAssign(uint s){ 
        uint i; 
        for(i = 0;i<(len-s);i++) 
            data[i]=data[i+s];       
        for(;i<len;i++)data[i]=0; 
        len-=s; 
        return this; 
    } 
         
    Bignumber opShlAssign(uint s){ 
        assert(len+s<MAX); 
        int i;                       
        for(i = len-1;i>=0;i--)        
            data[i+s]=data[i];       
        for(i = 0;i<s;i++) data[i]=0; 
        len+=s; 
        return this; 
    } 
     
    Bignumber opShl(uint s) 
    { 
        Bignumber ret = new Bignumber(this); 
        ret<<=s; 
        return ret; 
    } 
     
    //char[] toString(){ 
    char[] toUtf8(){ 
        char[64] tmp;
        char[] ret; 
        if(sign) ret~="-"; 
        else     ret~="+"; 
        for(int i = len-1; i >= 0; i--){ 
            ret~= .toString(data[i]) ~ ","; 
        } 
        //ret ~= "BASE:"~ .toString(BASE); 
        //ret ~= "BASE:"~ itoa(tmp, BASE); 
        return ret;      
    } 
} 

Bignumber sqrt(Bignumber b) {
    Bignumber res;
    auto one = new Bignumber("1"), two = new Bignumber("2"); // sorry
    res = (b+one) / two; // init
    // writefln((b+one).toUtf8, " / ", two.toUtf8, " = ", res.toUtf8); // hey look! opDiv is broken!
    auto r = new Bignumber("");
    while (res>r) { res -= r; r += one; }
    delete r;
    return res;
}

debug( UnitTest ) unittest
{ 
    auto a = new Bignumber("12345678901234567890123456789012345678901234567890");
    auto b = new Bignumber("12345678901234567890123456789012345678901234567890");
    auto c = a - b;
    Trace("a - b = ")( c.toUtf8() ).newline;
    
    c = a + b -b;
    Trace("a + b - b = ")( c.toUtf8() ).newline;
    
    c = a * b / b;      
    Trace("a * b / b = ")( c.toUtf8() ).newline;
    
    c = a / b * b;
    Trace("a / b * b = ")( c.toUtf8() ).newline;
    
    Trace(sqrt(new Bignumber("65536")).toUtf8).newline;
} 

void main() { }
