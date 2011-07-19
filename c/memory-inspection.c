#include <stdio.h>
#include <stdlib.h>

const char test1[] = "Hello World!!!\n";
const char *test2 = "This is a test!!!";
const long longValue = -123456L;


//A function to view he contence of a block of memory.
void displayMemory(char *address, int length);

//Macros's to assist
#define DISPLAY_VARMEM(var) displayMemory((char*) &( var ), (sizeof( var )));
#define DISPLAY_PTRMEM(ptr) displayMemory((char*) ( ptr ), (sizeof( *ptr )));

int main() {
 //First I will create an array with all 256 values of a char. 
 unsigned char chars[256];
 chars[0] = 255; while(*chars > 0) { chars[*chars]=*chars; (*chars)--;}
 //Display all 256 ASCII chars
 DISPLAY_VARMEM(chars);
 puts("");
 //Display the contence of string test1.
 DISPLAY_VARMEM(test1);
 puts("");
 //Display the contence longValue.
 DISPLAY_VARMEM(longValue);
 puts("");
 //Display the contence of test2 (note use of _PTRMEM since test2 is a pointer.
 DISPLAY_PTRMEM(test2); //Only displays 1 byte since sizeof(char*) = 1 (on most systems).
 puts("");
 displayMemory((char*)main, 100); //display 100 bytes of code from main().

 return 0;

}


/****
 * This function will print out a memory dump of the current address for the specified
 * length. Note that if the current program does not have access to the block of memory
 * from address to address + length then an access violation will occur. So the user
 * must be careful not try to display memory that the program does not have access to.
 * <br>
 * The following is the format for the output:
 * <bR>
 * ADDRESS  | 16 CHARACTERS OF DATA IN HEX                    | ASCII CHARS
 * 0040103C | 48 65 6C 6C 6F 20 57 6F 72 6C 64 21 21 21 00 __ | Hello.World!!!.
 * <br>
 * Note that '__' is place holder if the length is not a multiple of 16 (one complete
 * line.) This helps keep the report consistent and easy to scan with eyes.
 */
void displayMemory(char *address, int length) {
	int i = 0; //used to keep track of line lengths
	char *line = (char*)address; //used to print char version of data
	unsigned char ch; // also used to print char version of data
	printf("%08X | ", (int)address); //Print the address we are pulling from
	while (length-- > 0) {
		printf("%02X ", (unsigned char)*address++); //Print each char
		if (!(++i % 16) || (length == 0 && i % 16)) { //If we come to the end of a line...
			//If this is the last line, print some fillers.
			if (length == 0) { while (i++ % 16) { printf("__ "); } }
			printf("| ");
			while (line < address) {  // Print the character version
				ch = *line++;
				printf("%c", (ch < 33 || ch == 255) ? 0x2E : ch);
			}
			// If we are not on the last line, prefix the next line with the address.
			if (length > 0) { printf("\n%08X | ", (int)address); }
		}
	}
	puts("");
}


