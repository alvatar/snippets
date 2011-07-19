// Copyright 2008 Sergey "SnakE" Gromov
// snake.scaly@gmail.com

// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#include <assert.h>
#include <ctype.h>
#include <stddef.h>
#include <string.h>
#include <stdlib.h>

#include <memory>
#include <string>
#include <functional>

#include "Accessor.h"
#include "PropSet.h"
#include "KeyWords.h"
#include "SciLexer.h"
#include "Scintilla.h"

namespace dlex
{

static const int eol = -2;      // end of line
static const int range = -3;    // range operator, ".."

class Folder
{
public:

    Folder() : nest(0), opened(0), compact(false), level(0)
    {
    }

    void init(int nest, bool compact)
    {
        this->nest = nest;
        this->compact = compact;
        opened = 0;
        level = nest;
    }

    void newLine()
    {
        opened = 0;
        level = nest;
    }

    void open()
    {
        if (level > nest)
            level = nest;
        ++nest;
        ++opened;
    }

    void close()
    {
        if (compact && level > nest)
            level = nest;
        if (nest > 0)
            --nest;
        if (!compact && level > nest)
            level = nest;
        if (opened > 0)
            --opened;
    }

    int getNesting() const
    {
        return nest;
    }

    bool isFoldingPoint() const
    {
        return opened > 0;
    }

    int getLevel() const
    {
        return level;
    }

private:

    int nest;  ///< current nesting level
    int opened; ///< number of blocks opened and not closed on this line
    bool compact;   ///< compact mode folds up to the next folding point of the same nest
    int level;
};

static Folder folder;
static bool foldComments = false;   // enable folding comments

/**
    \brief Tests whether str matches one of the words in the given range.
    \return Returns that word if matched.
*/
template <class InStr, class WordIter>
static WordIter IsKeyword(InStr const & str, WordIter firstWord, WordIter lastWord)
{
    for (; firstWord != lastWord; ++firstWord)
        if (str.compare(*firstWord) == 0)
            break;
    return firstWord;
}

#include <stdarg.h>

static void log(char const * format...)
{
    FILE * f = fopen("c:\\home\\snake\\src\\scintilla\\work\\lexd.log", "a");
    if (f)
    {
        va_list ap;
        va_start(ap, format);
        vfprintf(f, format, ap);
        va_end(ap);
        fputc('\n', f);
        fclose(f);
    }
}

class Colourizer
{
public:

    Colourizer() {}

    Colourizer(Accessor & styler, WordList * * words, unsigned cur, unsigned next)
        : m_styler(&styler)
        , m_words(words)
        , m_cur(cur)
        , m_next(next)
    {
    }

    // colourize all preceding characters, but not the current
    void colourExcl(int style)
    {
        if (m_cur > 0)
            m_styler->ColourTo(m_cur-1, style);
    }

    // colourize all characters including current
    void colourIncl(int style)
    {
        if (m_next > 0)
            m_styler->ColourTo(m_next-1, style);
    }

    // checks whether the given string matches a keyword
    bool isKeyword(std::string const & s, int group)
    {
        char * * end = m_words[group]->words + m_words[group]->len;
        return IsKeyword(s, m_words[group]->words, end) != end;
    }

private:
    Accessor * m_styler;
    WordList * * m_words;
    unsigned m_cur, m_next;
};

struct State;
typedef std::auto_ptr<State> StatePtr;
struct State
{
    // returns a style associated with this state
    virtual int getStyle() const = 0;
    // sub-style helps distinguish between various flavours of a single style
    virtual int getSubStyle() const {return 0;}
    // can either return this or a new instance of another state
    virtual StatePtr onChar(int ch, Colourizer colourizer) = 0;
    // to be able to delete inctances
    virtual ~State() {}
};

StatePtr NewDefaultState();

struct NullState : State
{
    virtual int getStyle() const {return 0;}
    virtual int getSubStyle() const {return 1;}
    StatePtr onChar(int ch, Colourizer colourizer)
    {
        return StatePtr(new NullState);
    }
};

//
// Numbers
//

struct NumberPart
{
    enum type
    {
        integer,    // integer number, or an integer part of a floating point
        fractional, // fractional part of a floating point
        exponent    // exponent part of a floating point
    };
};

// checks whether a char is a decimal digit
// if *first* is true, only 1-9 are valid digits
static bool IsDecimal(int ch, bool first)
{
    return ch >= '1' && ch <= '9' || !first && ch == '0';
}

static StatePtr NewDecimalNumber(NumberPart::type t);

struct Exponent : State
{
    virtual int getStyle() const {return SCE_D_NUMBER;}
    StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == '+' || ch == '-')
            return NewDecimalNumber(NumberPart::exponent);
        else
            return NewDecimalNumber(NumberPart::exponent)->onChar(ch, colourizer);
    }
};

struct IntegerSuffix : State
{
    IntegerSuffix() : first(-1) {}
    virtual int getStyle() const {return SCE_D_NUMBER;}
    StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (first == -1 && strchr("LuUfFi", ch))
        {
            first = ch;
            return StatePtr(new IntegerSuffix(*this));
        }
        if (
            first == 'L' && (ch == 'u' || ch == 'U' || ch == 'i')
            || (first == 'u' || first == 'U') && ch == 'L'
            || (first == 'f' || first == 'F') && ch == 'i'
            )
        {
            colourizer.colourIncl(SCE_D_NUMBER);
            return NewDefaultState();
        }
        colourizer.colourExcl(SCE_D_NUMBER);
        return NewDefaultState()->onChar(ch, colourizer);
    }
private:
    int first;
};

struct FloatSuffix : State
{
    FloatSuffix() : first(-1) {}
    virtual int getStyle() const {return SCE_D_NUMBER;}
    StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (first == -1 && strchr("fFLi", ch))
        {
            first = ch;
            return StatePtr(new FloatSuffix(*this));
        }
        if ((first == 'f' || first == 'F' || first == 'L') && ch == 'i')
        {
            colourizer.colourIncl(SCE_D_NUMBER);
            return NewDefaultState();
        }
        colourizer.colourExcl(SCE_D_NUMBER);
        return NewDefaultState()->onChar(ch, colourizer);
    }
private:
    int first;
};

template <class DigitTestPred>
class Number : public State
{
    typedef Number<DigitTestPred> this_type;
public:
    Number(DigitTestPred test, char const * exponent, NumberPart::type part) : test(test), exponent(exponent), part(part) {}
    virtual int getStyle() const {return SCE_D_NUMBER;}
    StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (part == NumberPart::integer && ch == '.')
            return StatePtr(new this_type(test, exponent, NumberPart::fractional));

        if (part == NumberPart::fractional && strchr(exponent, ch))
            return StatePtr(new Exponent);

        if (!(test(ch) || ch == '_'))
        {
            colourizer.colourExcl(SCE_D_NUMBER);
            // HACK: Hex floats *must* have exponent
            if (part == NumberPart::fractional && strchr(exponent, 'p'))
            {
                colourizer.colourIncl(SCE_D_NUMBER_ERROR);
                return NewDefaultState();
            }
            else
            {
                StatePtr suffix;
                if (part == NumberPart::integer)
                    suffix = StatePtr(new IntegerSuffix);
                else
                    suffix = StatePtr(new FloatSuffix);
                return suffix->onChar(ch, colourizer);
            }
        }

        return StatePtr(new this_type(*this));
    }
private:
    DigitTestPred test;
    char const * exponent;
    NumberPart::type part;
};

template <class DigitTestPred>
inline StatePtr NewNumber(DigitTestPred p, char const * e, NumberPart::type t)
{
    return StatePtr(new Number<DigitTestPred>(p, e, t));
}

static StatePtr NewDecimalNumber(NumberPart::type t)
{
    return NewNumber(std::bind2nd(std::ptr_fun(IsDecimal), false), "eE", t);
}

static StatePtr NewHexNumber(NumberPart::type t)
{
    return NewNumber(isxdigit, "pP", t);
}

struct RadixDigits : State
{
    RadixDigits(unsigned radix) : radix(radix), error(false) {}
    virtual int getStyle() const {return error ? SCE_D_NUMBER_ERROR : SCE_D_NUMBER;}
    StatePtr onChar(int ch, Colourizer colourizer)
    {
        unsigned c = tolower(ch);
        if (c == '_')
            ;
        else if (c >= '0' && c <= '9')
        {
            if (!error && c - '0' >= radix)
            {
                error = true;
                colourizer.colourExcl(SCE_D_NUMBER);
            }
        }
        else if (!(c >= 'a' && c <= 'z' && c - 'a' + 10 < radix))
        {
            colourizer.colourExcl(getStyle());
            return StatePtr(new IntegerSuffix)->onChar(ch, colourizer);
        }
        return StatePtr(new RadixDigits(*this));
    }
private:
    unsigned radix;
    bool error;
};

StatePtr NewMaybeKeyword(std::string beg);

struct MaybeFloat : State
{
    MaybeFloat(Colourizer dot) : dot(dot) {}
    virtual int getStyle() const {return SCE_D_OPERATOR;}
    StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (IsDecimal(ch, false))
            return NewDecimalNumber(NumberPart::fractional);    // phew, it was a number after all.
        if (ch == '_')
        {
            // it still may be an identifier after a dot
            underscore += ch;
            return StatePtr(new MaybeFloat(*this));
        }
        // dot was a dot, and the rest is either an ident or a keyword
        dot.colourIncl(SCE_D_OPERATOR);
        return NewMaybeKeyword(underscore)->onChar(ch, colourizer);
    }
private:
    Colourizer dot;
    std::string underscore;
};

struct NumberOpen : State
{
    virtual int getStyle() const {return SCE_D_NUMBER;}
    StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == 'x' || ch == 'X')
            return NewHexNumber(NumberPart::integer);
        if (ch == 'b' || ch == 'B')
            return StatePtr(new RadixDigits(2));
        if (ch == '.')
            return NewDecimalNumber(NumberPart::fractional);
        return StatePtr(new RadixDigits(8))->onChar(ch, colourizer);
    }
};

//
// Strings
//

// For strings, sub-style is a string type:
// LSB is the stringt type;
// next-to-LSB meaning is different for different strings

struct StringType
{
    enum type
    {
        // regular string; next-to-LSB byte is not used
        regular,

        // wysiwyg string; next-to-LSB byte contains a string termination character
        wysiwyg,

        // hexadecimal string; next-to-LSB byte specifies whether a string contained an odd number of hex digits till yet
        hex,

        // delimited string; next-to-LSB byte contains number of skipped double-quotes for back-tracking
        delimited
    };
};

StatePtr NewDoubleQuotedString();

struct HexEscape : State
{
    HexEscape(int expect, StatePtr next) : numDigits(expect), nextState(next) {}
    virtual int getStyle() const {return SCE_D_STRING_ERROR;}
    virtual int getSubStyle() const {return StringType::regular;}
    virtual StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (!isxdigit(ch))
        {
            colourizer.colourExcl(SCE_D_STRING_ERROR);
            return nextState->onChar(ch, colourizer);
        }
        if (--numDigits == 0)
        {
            colourizer.colourIncl(SCE_D_ESCAPE);
            return nextState;
        }
        return StatePtr(new HexEscape(*this));
    }
private:
    int numDigits;
    StatePtr nextState;
};

struct OctEscape : State
{
    OctEscape(StatePtr next) : numDigits(1), nextState(next) {}
    virtual int getStyle() const {return SCE_D_ESCAPE;}
    virtual int getSubStyle() const {return StringType::regular;}
    virtual StatePtr onChar(int ch, Colourizer colourizer)
    {
    	if (!(ch >= '0' && ch <= '7'))
        {
            colourizer.colourExcl(SCE_D_ESCAPE);
    	    return nextState->onChar(ch, colourizer);
        }
    	if (++numDigits == 3)
        {
            colourizer.colourIncl(SCE_D_ESCAPE);
    	    return nextState;
        }
    	return StatePtr(new OctEscape(*this));
    }
private:
    int numDigits;
    StatePtr nextState;
};

struct NamedEntityEscape : State
{
    NamedEntityEscape(StatePtr next) : nextState(next) {}
    virtual int getStyle() const {return SCE_D_STRING_ERROR;}
    virtual int getSubStyle() const {return StringType::regular;}
    virtual StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == ';')
        {
            colourizer.colourIncl(SCE_D_ESCAPE);
            return nextState;
        }
        if (!isalnum(ch))
        {
            colourizer.colourExcl(SCE_D_STRING_ERROR);
            return nextState->onChar(ch, colourizer);
        }
        return StatePtr(new NamedEntityEscape(*this));
    }
private:
    StatePtr nextState;
};

struct EscapeString : State
{
    EscapeString(StatePtr next) : nextState(next) {}
    virtual int getStyle() const {return SCE_D_ESCAPE_ERROR;}
    virtual StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == 'x')
            return StatePtr(new HexEscape(2, nextState));
        else if (ch == 'u')
            return StatePtr(new HexEscape(4, nextState));
        else if (ch == 'U')
            return StatePtr(new HexEscape(8, nextState));
        else if (ch >= '0' && ch <= '7')
            return StatePtr(new OctEscape(nextState));
        else if (ch == '&')
            return StatePtr(new NamedEntityEscape(nextState));
        else if (strchr("'\"?\\abfnrtv", ch))
            colourizer.colourIncl(SCE_D_ESCAPE);
        else
            colourizer.colourIncl(SCE_D_ESCAPE_ERROR);
        return nextState;
    }
private:
    StatePtr nextState;
};

struct StringPostfix : State
{
    virtual int getStyle() const {return SCE_D_IDENT;}
    virtual StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == 'c' || ch == 'w' || ch == 'd')
        {
            colourizer.colourIncl(SCE_D_STRING);
            return NewDefaultState();
        }
        return NewDefaultState()->onChar(ch, colourizer);
    }
};

struct DoubleQuotedString : State
{
    virtual int getStyle() const {return SCE_D_STRING;}
    virtual int getSubStyle() const {return StringType::regular;}
    virtual StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == '"')
        {
            colourizer.colourIncl(SCE_D_STRING);
            return StatePtr(new StringPostfix);
        }
        if (ch == '\\')
        {
            colourizer.colourExcl(SCE_D_STRING);
            return StatePtr(new EscapeString(StatePtr(new DoubleQuotedString)));
        }
        return StatePtr(new DoubleQuotedString);
    }
};

StatePtr NewDoubleQuotedString()
{
    return StatePtr(new DoubleQuotedString);
}

struct WysiwygString : State
{
    WysiwygString(int ch) : m_end(ch) {}
    virtual int getStyle() const {return SCE_D_STRING;}
    virtual int getSubStyle() const {return StringType::wysiwyg | (m_end << 8);}
    virtual StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch != m_end)
            return StatePtr(new WysiwygString(*this));

        colourizer.colourIncl(getStyle());
        return StatePtr(new StringPostfix);
    }
private:
    int m_end;
};

struct HexString : State
{
    HexString(bool odd = false) : oddDigit(odd) {}
    virtual int getStyle() const {return SCE_D_STRING;}
    virtual int getSubStyle() const {return StringType::hex | (oddDigit << 8);}
    virtual StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == '"')
        {
            colourizer.colourExcl(SCE_D_STRING_ERROR);  // odd number of hex digits, if any
            colourizer.colourIncl(SCE_D_STRING);
            return StatePtr(new StringPostfix);
        }
        if (isxdigit(ch) || isspace(ch) || ch == eol)
        {
            if (isxdigit(ch))
                oddDigit = !oddDigit;
            if (!oddDigit)
                colourizer.colourIncl(SCE_D_STRING);    // this byte is complete, with any space after it
        }
        else
        {
            // bad char in hex string
            colourizer.colourExcl(SCE_D_STRING);
            colourizer.colourIncl(SCE_D_STRING_ERROR);
        }
        return StatePtr(new HexString(*this));
    }
private:
    bool oddDigit;
};

struct DelimitedStringNesting : State
{
    DelimitedStringNesting(char open, char close) : openChar(open), closeChar(close), level(1) {}
    virtual int getStyle() const {return SCE_D_STRING;}
    virtual StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == openChar)
            ++level;
        else if (ch == closeChar)
            --level;
        if (level == 0)
        {
            colourizer.colourIncl(SCE_D_STRING);
            return StatePtr(new NullState);
        }
        return StatePtr(new DelimitedStringNesting(*this));
    }
private:
    char openChar, closeChar;
    int level;
};

struct DelimitedStringSimple : State
{
    DelimitedStringSimple(char end) : endChar(end) {}
    virtual int getStyle() const {return SCE_D_STRING;}
    virtual StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == endChar)
        {
            colourizer.colourIncl(SCE_D_STRING);
            return StatePtr(new NullState);
        }
        return StatePtr(new DelimitedStringSimple(*this));
    }
private:
    char endChar;
};

struct DelimitedStringHeredoc : State
{
    DelimitedStringHeredoc(std::string end) : endIdent(end) {}
    virtual int getStyle() const {return SCE_D_STRING;}
    virtual StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == eol)
            currentIdent.clear();
        else if (currentIdent.size() < endIdent.size())
            currentIdent += ch;

        if (currentIdent.size() == endIdent.size())
        {
            if (currentIdent.compare(endIdent) == 0)
            {
                colourizer.colourIncl(SCE_D_STRING);
                return StatePtr(new NullState);
            }
            currentIdent += '$';    // never match until the next EOL
        }

        return StatePtr(new DelimitedStringHeredoc(*this));
    }
private:
    std::string endIdent;
    std::string currentIdent;
};

struct DelimitedStringBegin : State
{
    DelimitedStringBegin(std::string id) : ident(id) {}
    virtual int getStyle() const {return SCE_D_STRING_ERROR;}
    virtual StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch != eol)
            return StatePtr(new DelimitedStringBegin(*this));
        colourizer.colourExcl(SCE_D_STRING_ERROR);
        return StatePtr(new DelimitedStringHeredoc(ident));
    }
private:
    std::string ident;
};

struct DelimitedStringIdent : State
{
    virtual int getStyle() const {return SCE_D_STRING;}
    virtual StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (isalnum(ch) || ch == '_')
        {
            ident += ch;
            return StatePtr(new DelimitedStringIdent(*this));
        }
        colourizer.colourExcl(SCE_D_STRING);
        return StatePtr(new DelimitedStringBegin(ident))->onChar(ch, colourizer);
    }
private:
    std::string ident;
};

struct DelimitedStringOpen : State
{
    virtual int getStyle() const {return SCE_D_STRING;}
    virtual StatePtr onChar(int ch, Colourizer colourizer)
    {
        static char const open[]   = "([{<";
        static char const close[]  = ")]}>";
        static char const single[] = "`-=~!@#$%^&*+\\|;:\'\",./?";

        char const * c = strchr(open, ch);

        if (c)
            return StatePtr(new DelimitedStringNesting(ch, close[c-open]));
        else if (strchr(single, ch))
            return StatePtr(new DelimitedStringSimple(ch));
        else
            return StatePtr(new DelimitedStringIdent)->onChar(ch, colourizer);
    }
};

struct DelimitedStringEnd : State
{
    virtual int getStyle() const {return SCE_D_STRING_ERROR;}
    virtual StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == '"')
        {
            colourizer.colourExcl(SCE_D_STRING_ERROR);
            colourizer.colourIncl(SCE_D_STRING);
            return StatePtr(new StringPostfix);
        }
        return StatePtr(new DelimitedStringEnd);
    }
};

struct DelimitedString : State
{
    DelimitedString() : state(new DelimitedStringOpen), skipQuotes(1) {}
    virtual int getStyle() const {return state->getStyle();}
    virtual int getSubStyle() const {return StringType::delimited | (skipQuotes << 8);}
    virtual StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == '"')
            ++skipQuotes;

        state = state->onChar(ch, colourizer);

        if (!state->getSubStyle())
            return StatePtr(new DelimitedString(*this));
        else
            return StatePtr(new DelimitedStringEnd)->onChar(ch, colourizer);
    }
private:
    StatePtr state;
    unsigned skipQuotes;
};

struct MaybeKeyword : State
{
    MaybeKeyword(std::string beg = std::string()) : keyword(beg) {}
    virtual int getStyle() const {return SCE_D_IDENT;}
    virtual StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (isalnum(ch) || ch == '_' || (!keyword.empty() && isdigit(ch)))
        {
            keyword += (char)ch;
            return StatePtr(new MaybeKeyword(*this));
        }

        // end of a word, check whether it's a keyword

        if (ch == '"')
        {
            if (keyword.compare("r") == 0)
                return StatePtr(new WysiwygString(ch));

            if (keyword.compare("x") == 0)
                return StatePtr(new HexString);

            if (keyword.compare("q") == 0)
                return StatePtr(new DelimitedString);
        }

        colourizer.colourExcl(colourizer.isKeyword(keyword, 0) ? SCE_D_KEYWORD : SCE_D_IDENT);
        return NewDefaultState()->onChar(ch, colourizer);
    }
private:
    std::string keyword;
};

StatePtr NewMaybeKeyword(std::string beg)
{
    return StatePtr(new MaybeKeyword(beg));
}
//
// Chars
//

struct CharacterEnd : State
{
    virtual int getStyle() const {return SCE_D_CHAR_ERROR;}
    virtual StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == '\'')
        {
            colourizer.colourExcl(SCE_D_CHAR_ERROR);
            colourizer.colourIncl(SCE_D_CHAR);
            return NewDefaultState();
        }
        else if (ch == eol)
        {
            colourizer.colourIncl(SCE_D_CHAR_ERROR);
            return NewDefaultState();
        }
        return StatePtr(new CharacterEnd);
    }
};

struct Character : State
{
    virtual int getStyle() const {return SCE_D_CHAR_ERROR;}
    virtual StatePtr onChar(int ch, Colourizer colourizer)
    {
        colourizer.colourExcl(SCE_D_CHAR);
        if (ch == '\\')
            return StatePtr(new EscapeString(StatePtr(new CharacterEnd)));
        if (ch == eol)
        {
            colourizer.colourIncl(SCE_D_CHAR_ERROR);
            return NewDefaultState();
        }
        if (ch == range || ch == '\'')
            colourizer.colourIncl(SCE_D_CHAR_ERROR);
        else
            colourizer.colourIncl(SCE_D_CHAR);
        return StatePtr(new CharacterEnd);
    }
};

//
// Comments
//

// For stream comments, sub-style is a comment type:
// LSB is the comment type, '*' or '+';
// next-to-LSB is a number of '*' or '+' to skip to back-track to the comment start

static StatePtr NewDdocText();

struct DdocMacroBody : State
{
    DdocMacroBody() : nest(1) {}
    virtual int getStyle() const {return SCE_D_DDOC_MACRO;}
    StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == '(')
            ++nest;
        else if (ch == ')' && --nest == 0)
        {
            colourizer.colourIncl(SCE_D_DDOC_MACRO);
            return NewDdocText();
        }
        return StatePtr(new DdocMacroBody(*this));
    }
private:
    int nest;
};

struct DdocMacroOpen : State
{
    virtual int getStyle() const {return SCE_D_DDOC;}
    StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == '(')
            return StatePtr(new DdocMacroBody);
        return NewDdocText();
    }
};

struct DdocText : State
{
    virtual int getStyle() const {return SCE_D_DDOC;}
    StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == '$')
        {
            colourizer.colourExcl(SCE_D_DDOC);
            return StatePtr(new DdocMacroOpen);
        }
        return StatePtr(new DdocText);
    }
};

static StatePtr NewDdocText()
{
    return StatePtr(new DdocText);
}

struct DdocSection : State
{
    virtual int getStyle() const {return SCE_D_DDOC;}
    StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == ':')
        {
            colourizer.colourIncl(SCE_D_DDOC_SECTION);
            return StatePtr(new DdocText);
        }
        if (!(isalnum(ch) || ch == '_'))
            return StatePtr(new DdocText);
        return StatePtr(new DdocSection);
    }
};

struct DdocWaitSection : State
{
    DdocWaitSection(int skip = 0) : skipChar(skip) {}
    virtual int getStyle() const {return SCE_D_DDOC;}
    StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (isalpha(ch) || ch == '_')
        {
            colourizer.colourExcl(SCE_D_DDOC);
            return StatePtr(new DdocSection);
        }

        if (ch == skipChar)
            skipChar = 0;   // skip once
        else if (isspace(ch))
            ;
        else
            return StatePtr(new DdocText)->onChar(ch, colourizer);

        return StatePtr(new DdocWaitSection(*this));
    }
private:
    int skipChar;   // for skipping leading stars and pluses in multiline comments
};

struct Comment : State
{
    virtual int getStyle() const {return SCE_D_COMMENT;}
    StatePtr onChar(int ch, Colourizer colourizer)
    {
        return StatePtr(new Comment);
    }
};

struct EolMaybeDdoc : State
{
    virtual int getStyle() const {return SCE_D_COMMENT;}
    StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == '/')
            return StatePtr(new DdocWaitSection);
        else
            return StatePtr(new Comment);
    }
};

struct EolComment : State
{
    EolComment() : state(new EolMaybeDdoc) {}
    virtual int getStyle() const {return state->getStyle();}
    StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == eol)
        {
            colourizer.colourExcl(getStyle());
            if (getStyle() == SCE_D_DDOC_MACRO)     // macro not closed
                colourizer.colourIncl(SCE_D_DDOC_MACRO_ERROR);
            return NewDefaultState();
        }
        state = state->onChar(ch, colourizer);
        return StatePtr(new EolComment(*this));
    }
private:
    StatePtr state;
};

struct StreamMaybeDdoc : State
{
    StreamMaybeDdoc(int type) : type(type) {}
    virtual int getStyle() const {return SCE_D_COMMENT;}
    StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == type)
        {
            colourizer.colourIncl(SCE_D_DDOC);
            return StatePtr(new DdocWaitSection(type));
        }
        else
            return StatePtr(new Comment);
    }
private:
    int type;
};

struct StreamComment : State
{
    StreamComment(int type) : state(new StreamMaybeDdoc(type)), type(type), level(1), backtrackCount(1), levelUp(false), levelDown(false) {}
    virtual int getStyle() const {return state->getStyle();}
    virtual int getSubStyle() const {return type | (backtrackCount << 8);}
    StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == type)
            ++backtrackCount;

        if (levelUp)
        {
            levelUp = false;
            if (ch == type)
                ++level;
        }
        if (type == '+' && ch == '/')
            levelUp = true;     // can be a nesting comment opening sequence

        if (levelDown)
        {
            levelDown = false;
            if (ch == '/' && --level == 0)
            {
                // end of stream comment
                levelDownColourizer.colourExcl(getStyle());
                colourizer.colourIncl(
                    getStyle() == SCE_D_COMMENT ?
                        SCE_D_COMMENT :
                        getStyle() == SCE_D_DDOC_MACRO ?
                            SCE_D_DDOC_MACRO_ERROR :
                            SCE_D_DDOC
                    );
                if (foldComments)
                    folder.close();
                return NewDefaultState()->onChar(ch, colourizer);
            }
        }
        if (ch == type)
        {
            levelDownColourizer = colourizer;
            levelDown = true;   // can be a comment closing sequence
        }

        state = state->onChar(ch, colourizer);

        if (ch == eol && state->getStyle() == SCE_D_DDOC)
            state = StatePtr(new DdocWaitSection(type));

        return StatePtr(new StreamComment(*this));
    }

private:

    StatePtr state;
    int type;   // either '*' or '+'
    int level;  // level for nesting comments
    int backtrackCount; // number of '*' or '+' from the start of this comment
    bool levelUp;   // if found '/' and type == '+'
    bool levelDown; // if found 'type'
    Colourizer levelDownColourizer;     // to close comment correctly if there is a macro error
};

struct MaybeComment : State
{
    int getStyle() const {return SCE_D_OPERATOR;}
    StatePtr onChar(int ch, Colourizer colourizer)
    {
        if (ch == '/')
            return StatePtr(new EolComment);
        if (ch == '*' || ch == '+')
        {
            if (foldComments)
                folder.open();
            return StatePtr(new StreamComment(ch));
        }

        // otherwise it's just a division
        colourizer.colourExcl(SCE_D_OPERATOR);

        // process the ch in default state
        return NewDefaultState()->onChar(ch, colourizer);
    }
};

//
// Main state
//

struct DefaultState : State
{
    virtual int getStyle() const {return SCE_D_DEFAULT;}
    virtual StatePtr onChar(int ch, Colourizer colourizer)
    {
        colourizer.colourExcl(SCE_D_DEFAULT);

        if (isalpha(ch) || ch == '_')
            return StatePtr(new MaybeKeyword)->onChar(ch, colourizer);

        if (IsDecimal(ch, true))
            return NewDecimalNumber(NumberPart::integer);
        if (ch == '0')
            return StatePtr(new NumberOpen);
        if (ch == '.')
            return StatePtr(new MaybeFloat(colourizer));

        if (ch == '\'')
            return StatePtr(new Character);
        if (ch == '"')
            return StatePtr(new DoubleQuotedString);
        if (ch == '`')
            return StatePtr(new WysiwygString(ch));
        if (ch == '\\')
            return StatePtr(new EscapeString(StatePtr(new DefaultState)));
        if (ch == '/')
            return StatePtr(new MaybeComment);

        if (strchr("~!$%^&*=-+\\|,.?;:()[]{}<>", ch) || ch == range)
            colourizer.colourIncl(SCE_D_OPERATOR);

        if (ch == '{')
            folder.open();
        if (ch == '}')
            folder.close();

        return StatePtr(new DefaultState);
    }
};

StatePtr NewDefaultState()
{
    return StatePtr(new DefaultState);
}

// returns the previous char and adjusts position to point to that char
static int PreviousChar(unsigned & pos, Accessor & styler)
{
    if (pos == 0)
        return 0;

    --pos;
    int ch = (unsigned char) styler.SafeGetCharAt(pos);

    if (pos > 0)
    {
        int prevCh = (unsigned char) styler.SafeGetCharAt(pos-1);
        if (styler.IsLeadByte((char)prevCh))
        {
            --pos;
            ch |= prevCh << 8;
        }
        else if (ch == '\n' && prevCh == '\r')
            --pos;
    }

    if (ch == '\r' || ch == '\n')
        ch = eol;

    return ch;
}

static void ColouriseDDoc(
    unsigned startPos,
    int length,
    int initStyle,
    WordList * keywordlists[],
    Accessor & styler
    )
{
    bool fold = styler.GetPropertyInt("fold");
    foldComments = styler.GetPropertyInt("fold.comment") != 0;
    bool foldCompact = styler.GetPropertyInt("fold.compact", 1) != 0;

    unsigned endPos = startPos + length;
    StatePtr state(new DefaultState);

    // reconstruct the initial state
    int lineState = styler.GetLineState(styler.GetLine(startPos)) & 0xFFFFF;
    switch (initStyle)
    {
        case SCE_D_STRING:
        case SCE_D_STRING2:
            if ((lineState & 0xFF) == StringType::regular)
                state = StatePtr(new DoubleQuotedString);
            else if ((lineState & 0xFF) == StringType::wysiwyg)
                state = StatePtr(new WysiwygString(lineState >> 8));
            else if ((lineState & 0xFF) == StringType::hex)
                state = StatePtr(new HexString(lineState >> 8));
            else if ((lineState & 0xFF) == StringType::delimited)
            {
                // back-track to the start of the string
                int firstQuote = (lineState >> 8);
                while (startPos > 0)
                {
                    int ch = PreviousChar(startPos, styler);
                    if (ch == '"' && --firstQuote == 0)
                        break;
                }
                PreviousChar(startPos, styler);
            }
            break;
        case SCE_D_STRING_ERROR:
            // string error can span beyond EOL only at the end of a delimited string
            state = StatePtr(new DelimitedStringEnd);
            break;
        case SCE_D_COMMENT:
        case SCE_D_DDOC:
        case SCE_D_DDOC_MACRO:
        case SCE_D_DDOC_MACRO_ERROR:
        {
            // back-track to the start of the comment
            int type = lineState & 0xFF;
            int toSkip = lineState >> 8;
            while (startPos > 0)
            {
                int ch = PreviousChar(startPos, styler);
                if (ch == type && --toSkip == 0)
                    break;
            }
            PreviousChar(startPos, styler);
            break;
        }
        default:
            state = StatePtr(new DefaultState);
    }

    styler.StartAt(startPos);
    styler.StartSegment(startPos);

    int line = styler.GetLine(startPos);

    folder.init(styler.GetLineState(line) >> 20, foldCompact);
    bool emptyLine = true;

    while (startPos < endPos)
    {
        unsigned nextPos = startPos;

        int ch = (unsigned char) styler.SafeGetCharAt(nextPos++);

        // test for EOL
        if (ch == '\r' || ch == '\n')
        {
            if (ch == '\r' && styler.SafeGetCharAt(nextPos) == '\n')
            	++nextPos;
            ch = eol;
        }

        // test for range ("..")
        else if (ch == '.' && styler.SafeGetCharAt(nextPos) == '.')
        {
            ++nextPos;
            ch = range;
        }

        // test for shift-encodings
        else if (styler.IsLeadByte((char)ch))
            ch = (unsigned char) styler.SafeGetCharAt(nextPos++) | (ch << 8);

        state = state->onChar(ch, Colourizer(styler, keywordlists, startPos, nextPos));

        if (!(isspace(ch) || ch == eol))
            emptyLine = false;

        startPos = nextPos;

        // treat EOF like EOL
        if (startPos >= endPos)
        {
            ch = eol;
            state = state->onChar(ch, Colourizer(styler, keywordlists, endPos, endPos));
        }

        if (ch == eol)
        {
            if (fold)
                styler.SetLevel(line, folder.getLevel() | SC_FOLDLEVELHEADERFLAG * folder.isFoldingPoint() | SC_FOLDLEVELWHITEFLAG * emptyLine);
            folder.newLine();
            emptyLine = true;
            ++line;
            styler.SetLineState(line, (state->getSubStyle() & 0xFFFFF) | (folder.getNesting() << 20));
        }
    }

    // if there's an unfinished delimited string, make sure the segment ends with
    // a different style than it was so that the string is re-coloured till its end.
    int style = state->getStyle();
    if (
        style == SCE_D_STRING
        && (
            (state->getSubStyle() & 0xFF) == StringType::delimited
            || (state->getSubStyle() & 0xFF) == StringType::hex
            )
        && styler.StyleAt(endPos-1) == SCE_D_STRING
        )
    {
        style = SCE_D_STRING2;
    }
    styler.ColourTo(endPos-1, style);
}

static void FoldDDoc(
    unsigned startPos,
    int length,
    int initStyle,
    WordList * keywordlists[],
    Accessor & styler
    )
{
}

static char const * const keywordListsDesc[] =
{
    "Keywords",
    0
};

}

LexerModule lmD(SCLEX_D, &dlex::ColouriseDDoc, "d", &dlex::FoldDDoc, dlex::keywordListsDesc);
