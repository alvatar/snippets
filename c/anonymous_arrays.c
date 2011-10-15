/* C99 offers some really cool stuff using anonymous arrays: */

/* Removing pointless variables */

{
    int yes=1;
    setsockopt(yourSocket, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int));
}

/* becomes */

setsockopt(yourSocket, SOL_SOCKET, SO_REUSEADDR, (int[]){1}, sizeof(int));

/* Passing a Variable Amount of Arguments */

void func(type* values) {
    while(*values) {
        x = *values++;
        /* do whatever with x */
    }
}

func((type[]){val1,val2,val3,val4,0});

/* Static linked lists */

int main() {
    struct llist { int a; struct llist* next;};
    #define cons(x,y) (struct llist[]){{x,y}}
    struct llist *list=cons(1, cons(2, cons(3, cons(4, NULL))));
    struct llist *p = list;
    while(p != 0) {
        printf("%d\n", p->a);
        p = p->next;
    }
}
