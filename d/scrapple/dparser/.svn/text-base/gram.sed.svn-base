# limit range
/bnf/,/\/pre/ s/\(.\)/\1/
t go
d
: go

s/\r//

# extract ops
/opt/ { s/<sub>opt<\/sub>/ @/g }

# dmp HTML
s/<[^>]*>//g
s/&lt;/</g
s/&gt;/>/g
s/&amp;/\&/g

# normalize WS
s/        /\t/g
s/    /\t/g

#adjust stuff

s/\([()]\)/ \1 /g

# mark PKW

s/\bdb\b/PKW_db/g
s/\bdd\b/PKW_dd/g
s/\bde\b/PKW_de/g
s/\bdf\b/PKW_df/g
s/\bdi\b/PKW_di/g
s/\bdl\b/PKW_dl/g
s/\bds\b/PKW_ds/g
s/\bdword\b/PKW_dword/g
s/\beven\b/PKW_even/g
s/\bfar\b/PKW_far/g
s/\bnaked\b/PKW_naked/g
s/\bnear\b/PKW_near/g
s/\boffest\b/PKW_offest/g
s/\bptr\b/PKW_ptr/g
s/\bseg\b/PKW_seg/g
s/\bword\b/PKW_word/g
s/\bC\b/PKW_C/g
s/\bD\b/PKW_D/g
s/\bPascal\b/PKW_Pascal/g
s/\bWindows\b/PKW_Windows/g
s/\bSystem\b/PKW_System/g
s/\bfailure\b/PKW_failure/g
s/\bexit\b/PKW_exit/g
s/\bsuccess\b/PKW_success/
s/\b__LOCAL_SIZE\b/PKW___LOCAL_SIZE/g

s/^\([^ \t]*\):/\1@@/g


s/>>>=/ op3RShiftAssign /g
s/!<>=/ opNotLessGraterEqual /g
s/<<=/ opLShiftAssign /g
s/>>=/ opRShiftAssign /g
s/<>=/ opLessGraterEqual /g
s/!<>/ opNotLessGrater /g
s/!>=/ opNotGraterEqual /g
s/!<=/ opNotLessEqual /g
s/>>>/ op3RShift /g
s/\.\.\./ opEllipsis /g
s/%=/ opModuloAssign /g
s/\/=/ opDivideAssign /g
s/\*=/ opStarAssign /g
s/&=/ opAmpersandAssign /g
s/+=/ opPlusAssign /g
s/\^=/ opCarrotAssign /g
s/|=/ opPipeAssign /g
s/-=/ opMinusAssign /g
s/||/ opLogicOr /g
s/&&/ opLogicAnd /g
s/!=/ opNotEqual /g
s/==/ opEqual /g
s/<=/ opLessEqual /g
s/>=/ opGreaterEqual /g
s/<>/ opLessGreater /g
s/!>/ opNotGrater /g
s/!</ opNotLess /g
s/<</ opLShift /g
s/>>/ opRShift /g
s/++/ opPlusPlus /g
s/--/ opMinusMinus /g
s/\.\./ opDoubleDot /g
s/%/ opModulo /g
s/&/ opAmpersand /g
s/\*/ opStar /g
s/+/ opPlus /g
s/-/ opMinus /g
s/~/ opTilde /g
s/,/ opComma /g
s/=/ opAssignment /g
s/?/ opQuestion /g
s/:/ opCollin /g
s/|/ opPipe /g
s/\^/ opCarrot /g
s/</ opLessThan /g
s/>/ opGreaterThan /g
s/\// opDivision /g
s/!/ opBang /g
s/\./ opDot /g
s/;/ opSemicolon /g
s/(/ opLParen /g
s/)/ opRParen /g
s/\[/ opLBrace /g
s/\]/ opRBrace /g
s/{/ opLBracket /g
s/}/ opRBracket /g
s/\$/ opDollar /g

s/@@/:/g
s/ *@/?/g

# mark KW as sutch
s/\([\t ]\)\([a-z]\)/\1KW_\2/g
s/KW_op/op/g

# fix typos
s/^const//
s/^override//
s/^static//
s/^auto//
s/^scope//

# s/^\(.*\):/;\n\1:/

s/^[\t ]*$//

s/^\t\(.*\)$/\tdummy \/ \1 |/

#done
p