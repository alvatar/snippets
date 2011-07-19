DC = gdmd
DFLAGS = -ItesteD

all: libtesteD.a

test: libtesteD.a main.o
	$(CC) main.o -ltesteD -L. -lm -lpthread -lgphobos -o test

libtesteD.a: testeD/api.o testeD/unit.o testeD/suite.o
	$(AR) r $@ $?

%.o:%.d
	$(DC) -c $(DFLAGS) $< -of$@

clean:
	rm -f *.o *.a */*.o test