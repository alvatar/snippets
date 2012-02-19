/* (c) Chris O'Hara 2011 - MIT License */

#include <stdio.h>
#include <stdlib.h>
#include "prototype.h"

#ifndef NDEBUG
#define debug(fmt, ...) printf(fmt, ##__VA_ARGS__)
#else
#define debug(fmt, ...)
#endif

void constructor(void *, char *);
void destructor(void *);
void greet(void *);
void trollGreet(void *);

INTERFACE(
    char *name;
    void (*construct)(void *self, char *name);
    void (*destruct)(void *self);
    void (*greet)(void *self);
);

PROTOTYPE(Person,
    /* no member vars */
  , .construct = constructor
  , .destruct = destructor
  , .greet = greet
);

PROTOTYPE_INHERIT(Troll, Person,
    int trollFactor;
  , .greet = trollGreet
);

int main(int argc, char **argv) {
    Person *chris = CONSTRUCT(Person, "Chris");
    Troll *troll = CONSTRUCT(Troll, "Bob");

    troll->trollFactor = 20;

    CALL(chris, greet);
    CALL(troll, greet);

    DESTRUCT(chris);
    DESTRUCT(troll);

    return 0;
}

void constructor(void *self, char *new) {
    debug("(Constructing %s)\n", new);
    SET(self, name, new);
}

void destructor(void *self) {
    debug("(Destructing %s)\n", GET(self, name));
}

void greet(void *self) {
    debug("(Greeting %s)\n", GET(self, name));
    printf("%s: hello\n", GET(self, name));
}

void trollGreet(void *self) {
    SUPER(self, greet);

    int trollFactor = ((Troll *)self)->trollFactor;

    printf("%s: also, F", GET(self, name));
    while (trollFactor--) putchar('U');
    putchar('\n');
}
