module xf.utils.String;
import tango.text.Util;
import tango.text.Ascii;
import tango.io.Stdout;

int count(char[] s, char[] sub){
    int i;
    int j;
    int count = 0;

    for (i = 0; i < s.length; i += j + sub.length){
    	
		j = locatePattern(s[i .. s.length], sub);
		if (j == s[i .. s.length].length)   break;
		count++;
    }
    
    return count;
}

final bool isNumeric(in char[] s, in bool bAllowSep = false)
{
    int    iLen = s.length;
    bool   bDecimalPoint = false;
    bool   bExponent = false;
    bool   bComplex = false;
    char[] sx = toLower(s); 
    int    j  = 0;
    char   c;

    //writefln("isNumeric(char[], bool = false) called!");
    // Empty string, return false
    if (iLen == 0)
        return false;
    
    // Check for NaN (Not a Number)
    if (sx == "nan" || sx == "nani" || sx == "nan+nani")
        return true;
        
    // Check for Infinity
    if (sx == "inf" || sx == "-inf")
        return true;
     
    // A sign is allowed only in the 1st character   
    if (sx[0] == '-' || sx[0] == '+')
        j++;
            
    for (int i = j; i < iLen; i++)
    {
        c = sx[i];
    
        // Digits are good, continue checking 
        // with the next character... ;)
        if (c >= '0' && c <= '9') 
            continue;

        // Check for the complex type, and if found 
        // reset the flags for checking the 2nd number.  
        else if (c == '+')
            if (i > 0) 
            {
                bDecimalPoint = false;
                bExponent = false;
                bComplex = true;
                continue;
            }
            else
                return false;
                
        // Allow only one exponent per number   
        else if (c == 'e')  
        {
            // A 2nd exponent found, return not a number
            if (bExponent)
                return false;
                
            if (i + 1 < iLen)
            {
                // Look forward for the sign, and if 
                // missing then this is not a number.
                if (sx[i + 1] != '-' && sx[i + 1] != '+')
                    return false;
                else
                {
                    bExponent = true;
                    i++;    
                }    
            }        
            else
                // Ending in "E", return not a number
                return false;        
        }  
        // Allow only one decimal point per number to be used
        else if (c == '.' )
        {
            // A 2nd decimal point found, return not a number
            if (bDecimalPoint)
                return false;
            
            bDecimalPoint = true;
            continue;
        }   
        // Check for ending literal characters: "f,u,l,i,ul,fi,li",
        // and wheater they're being used with the correct datatype.
        else if (i == iLen - 2)
        {
            // Integer Whole Number
            if (sx[i..iLen] == "ul" && 
               (!bDecimalPoint && !bExponent && !bComplex))
                return true;
            // Floating-Point Number
            else if ((sx[i..iLen] == "fi" || sx[i..iLen] == "li") &&
                     (bDecimalPoint || bExponent || bComplex))
                return true;
            else if (sx[i..iLen] == "ul" && 
                    (bDecimalPoint || bExponent || bComplex))
                return false;    
            // Could be a Integer or a Float, thus
            // all these suffixes are valid for both  
            else if (sx[i..iLen] == "ul" || 
                     sx[i..iLen] == "fi" || 
                     sx[i..iLen] == "li")
                return true;
            else    
                return false;
        }
        else if (i == iLen - 1)
        {
            // Integer Whole Number
            if ((c == 'u' || c == 'l') && 
                (!bDecimalPoint && !bExponent && !bComplex))
                return true;
            // Check to see if the last character in the string 
            // is the required 'i' character
            else if (bComplex)
                if (c == 'i')
                    return true;
                else 
                    return false;        
            // Floating-Point Number
            else if ((c == 'l' || c == 'f' || c == 'i') &&
                     (bDecimalPoint || bExponent))
                return true;
            // Could be a Integer or a Float, thus  
            // all these suffixes are valid for both 
            else if (c == 'l' || c == 'f' || c == 'i')
                return true;
            else
                return false;
        }
        else
            // Check if separators are allow  
            // to be in the numeric string
            if (bAllowSep == true && (c == '_' || c == ','))
                continue;
            else    
                return false;       
    }     
    
    return true;
}


bool inPattern(char c, char[] pattern)
{
    bool result = false;
    int range = 0;
    dchar lastc;

    foreach (int i, char p; pattern)
    {
	if (p == '^' && i == 0)
	{   result = true;
	    if (i + 1 == pattern.length)
		return (c == p);	// or should this be an error?
	}
	else if (range)
	{
	    range = 0;
	    if (lastc <= c && c <= p || c == p)
		return !result;
	}
	else if (p == '-' && i > result && i + 1 < pattern.length)
	{
	    range = 1;
	    continue;
	}
	else if (c == p)
	    return !result;
	lastc = p;
    }
    return result;
}

char[] removechar(char[] s, char[] pattern){
    assert(pattern.length == 1);
    return substitute(s,pattern,"");
}
