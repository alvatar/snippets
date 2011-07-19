// match.c
//
// Copyright (c) 2006, Mike Acton <macton@cellperformance.com>
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated 
// documentation files (the "Software"), to deal in the Software without restriction, including without
// limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
// the Software, and to permit persons to whom the Software is furnished to do so, subject to the following
// conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial
// portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
// LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO
// EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
// AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
// OR OTHER DEALINGS IN THE SOFTWARE.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define MAX_LINE_WIDTH 4096 /* TODO: At some point replace with BSD fgetln() */

char source_line [ MAX_LINE_WIDTH ];
char uniq_line   [ MAX_LINE_WIDTH ];

void
usage( char* path )
{
    printf("Usage: %s [-h] <source_file> <uniq_file>\n", path);
    printf("For each line in <source_file> print the index to the first matching line in <uniq_file>.\n");
    printf("[-h] Print results in 32 bit hexidecimal (default is decimal)\n");
    printf("Note: The max line width supported is %d characters.\n",MAX_LINE_WIDTH-1);
    printf("Note: Maximum number of lines supported is (2^32)\n");
}

int
main( int argc, char** argv )
{
    if ( argc < 3 )
    {
        usage( argv[0] );
        return (-1);
    }    

    int   output_hex       = 0;
    char* source_file_path = argv[1];
    char* uniq_file_path   = argv[2];

    if ( argc == 4 )
    {
        if ( strncmp( argv[1], "-h", 2 ) == 0 )
        {
            output_hex       = 1;
            source_file_path = argv[2];
            uniq_file_path   = argv[3];
        }
        else
        {
            usage( argv[0] );
            return (-1);
        }
    }
    
    int   ret_code         = 0;
    FILE* source_file      = NULL;
    FILE* uniq_file        = NULL;

    source_file = fopen( source_file_path, "rt" );

    if ( source_file == NULL )
    {
        perror( "Could not open source file." );
        ret_code = -1;
        goto EXIT;
    }

    uniq_file = fopen( uniq_file_path, "rt" );

    if ( uniq_file == NULL )
    {
        perror( "Could not open uniq file." );
        ret_code = -1;
        goto EXIT;
    }

    for (;;)
    {
        uint32_t match_line_ndx = 0;
        int      found_match    = 0;

        char* source_result = fgets( source_line, MAX_LINE_WIDTH, source_file );
        if ( source_result == NULL )
        {
            break;
        }

        for (;;)
        {
            char* uniq_result = fgets( uniq_line, MAX_LINE_WIDTH, uniq_file );
            if ( uniq_result == NULL )
            {
                break;
            }

            if ( strncmp( source_line, uniq_line, MAX_LINE_WIDTH ) == 0 )
            {
                if ( output_hex )
                {
                    printf("0x%08x\n",match_line_ndx);
                }
                else
                {
                    printf("%d\n",match_line_ndx);
                }

                found_match = 1;
                break;
            }
          
            match_line_ndx++;
        }

        if (!found_match)
        {
            printf("NO MATCH\n");
        }

        fseek( uniq_file, 0, SEEK_SET );
    }

    EXIT:
    if ( source_file )
    {
        fclose( source_file );
    }

    if ( uniq_file )
    {
        fclose( uniq_file );
    }

    return (ret_code);
}
